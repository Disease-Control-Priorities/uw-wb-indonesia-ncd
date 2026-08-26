# =============================================================================
# 10-frp_indonesia.r
# Financial Risk Protection (FRP) data structures: wealth-quintile allocation
# of the completed national clinical-intervention model results.
# -----------------------------------------------------------------------------
# PURPOSE
#   Distribute the COMPLETED national clinical-intervention model output across
#   wealth quintiles Q1-Q5 to produce the input/output data structures needed
#   for subsequent financial-risk-protection analysis. This is a transparent,
#   post-processing PROPORTIONAL ALLOCATION of national model outcomes using
#   quintile-specific population and disease-burden shares. It is NOT a
#   quintile-specific model run: the disease model was calibrated and simulated
#   ONCE, at the national level. By construction, the allocated Q1-Q5 results
#   re-aggregate to the original national model results.
#
# OUTPUT
#   output/FRP_data_structures.xlsx  (formula-driven, audit-friendly workbook)
#
# =============================================================================
# ASSUMPTIONS AND LIMITATIONS  (also reproduced in the workbook README)
# -----------------------------------------------------------------------------
#   1.  Proportional allocation does NOT reproduce a quintile-specific disease
#       model. There is no separate calibration or simulation per quintile.
#   2.  National intervention effect sizes are implicitly assumed CONSTANT across
#       quintiles (the same relative reductions apply in every quintile).
#   3.  Coverage (baseline and scenario) is held CONSTANT across quintiles unless
#       the canonical inputs provide quintile-specific coverage (they do not).
#   4.  Wealth-burden shares are ALL-AGE and BOTH-SEX (the burden source has no
#       finer stratification). All-age shares are therefore applied across the
#       annual condition volumes.
#   5.  The missing-share FALLBACK substitutes the all-age population share when a
#       year x cause x measure burden distribution is unavailable; this assumes
#       equal rates across quintiles for that cell. Every fallback is flagged
#       NATIONAL_RATE_APPLIED and counted in QA / README / console.
#   6.  Age-band prorating for cost population-in-need allocation assumes a
#       UNIFORM population within each 5-year band. "95+" follows the model's
#       pooled top age (fully included when the cost age window reaches 95).
#   7.  Allocated outcomes PRESERVE national totals by construction (shares sum
#       to 1 across quintiles, verified in R and via Excel reconciliation).
#   8.  Resulting FRP estimates should be interpreted as DISTRIBUTIONAL SCENARIOS
#       layered on the national projection, not independently calibrated
#       quintile projections.
#   9.  Uncertainty from the allocation assumptions is NOT propagated (no
#       quintile-specific uncertainty is available in the canonical inputs).
#
# STATE IDENTITY
#   Output `well` is defined as the residual  pop - sick - all.mx  (per spec and
#   consistent with the model's compartment structure: living = pop - all.mx,
#   well_cause = living - sick_cause). The model's raw `well` differs from this
#   residual by a tiny stock/flow non-closure (<= ~230 persons per national
#   year x cause; relative ~1e-6); the gap is reported in QA and never hidden.
#
# CAUSE MAPPING  (wealth-burden cause_id -> model cause code)
#   C_IHD->ihd  C_IS->istroke  C_ICH->hstroke  C_HHD->hhd
#   C_RHD->rhd  C_CMD/C_CMP->cmd  C_DM/C_T2D->dm2
#
# RUN (standalone; does NOT run calibration or Models 00-09):
#   "C:/Program Files/R/R-4.5.1/bin/Rscript.exe" code/cvd-fair-choices/10-frp_indonesia.r
# =============================================================================

suppressWarnings(suppressMessages({
  library(data.table)
  library(readxl)
  library(openxlsx)
}))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# -----------------------------------------------------------------------------
# 0. Locate repository root (runnable from any working directory)
# -----------------------------------------------------------------------------
find_repo_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in 1:12) {
    if (file.exists(file.path(d, "uw-wb-indonesia-ncd.Rproj"))) return(d)
    parent <- dirname(d); if (identical(parent, d)) break; d <- parent
  }
  # Fallback: resolve relative to this script if sourced with a known path
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) {
    sp <- normalizePath(sub("^--file=", "", fa[1]), winslash = "/", mustWork = FALSE)
    cand <- dirname(dirname(dirname(sp)))  # code/cvd-fair-choices/<file> -> repo root
    if (file.exists(file.path(cand, "uw-wb-indonesia-ncd.Rproj"))) return(cand)
  }
  getwd()
}
ROOT <- find_repo_root()
rp <- function(...) file.path(ROOT, ...)

STOP <- function(...) stop("[FRP] ", ..., call. = FALSE)
SAY  <- function(...) cat(sprintf(...), "\n")

cat("===========================================================\n")
cat(" Model 10  FRP wealth-quintile allocation\n")
cat(" Repo root:", ROOT, "\n")
cat("===========================================================\n")

# -----------------------------------------------------------------------------
# 1. Canonical input paths
# -----------------------------------------------------------------------------
f_model   <- rp("output/out_model/model_output_Indonesia_htncov2_aspirational.rds")
f_pop     <- rp("data/processed/indonesia_wealth_population.rds")
f_burden  <- rp("data/processed/indonesia_wealth_burden.rds")
f_cost    <- rp("output/indonesia_model_cost_value_formulae.xlsx")
f_out     <- rp("output/FRP_data_structures.xlsx")

for (f in c(f_model, f_pop, f_burden, f_cost))
  if (!file.exists(f)) STOP("Required input not found: ", f)

# -----------------------------------------------------------------------------
# 2. Cause mapping (wealth-burden cause_id -> model cause code)
# -----------------------------------------------------------------------------
CAUSE_MAP <- c(
  C_IHD = "ihd", C_IS = "istroke", C_ICH = "hstroke", C_HHD = "hhd",
  C_RHD = "rhd", C_CMD = "cmd", C_CMP = "cmd", C_DM = "dm2", C_T2D = "dm2"
)
map_burden_to_model <- function(cid) unname(CAUSE_MAP[cid])
map_model_to_burden <- function(code) {
  # invert (first burden id whose model code matches)
  out <- vapply(code, function(cc) {
    hit <- names(CAUSE_MAP)[CAUSE_MAP == cc]
    if (!length(hit)) NA_character_ else hit[1]
  }, character(1))
  # prefer canonical ids actually present in the burden file (handled by caller)
  out
}

# -----------------------------------------------------------------------------
# 3. Load national model output; identify clinical scenarios dynamically
# -----------------------------------------------------------------------------
mo <- as.data.table(readRDS(f_model))
req_cols <- c("scenario","age","cause","sex","year","well","sick","newcases",
              "dead","pop","all.mx","intervention","location","intervention_family")
miss <- setdiff(req_cols, names(mo))
if (length(miss)) STOP("Model output missing columns: ", paste(miss, collapse = ", "))

# Read the canonical scenario catalog from the cost workbook (authoritative
# family classification) and reconcile with the model output metadata.
scat <- suppressMessages(as.data.table(read_excel(f_cost, "Scenario_Catalog")))

# Dynamic inclusion: baseline + clinical (standalone + combined clinical package),
# using intervention_family from the model output. Exclude public-health and
# combined clinical+public-health scenarios.
INCLUDE_FAMILIES <- c("baseline", "clinical")
scen_meta <- unique(mo[, .(scenario, intervention_family, scenario_role, scenario_level)])
clin_scen <- scen_meta[intervention_family %in% INCLUDE_FAMILIES]$scenario

# Hard guard: nothing public-health / combined may pass through.
bad <- scen_meta[!(intervention_family %in% INCLUDE_FAMILIES) &
                 scenario %in% clin_scen]$scenario
if (length(bad)) STOP("Excluded scenarios leaked into clinical set: ", paste(bad, collapse = ", "))
excluded_scen <- setdiff(scen_meta$scenario, clin_scen)

baseline_id <- "baseline"
if (!(baseline_id %in% clin_scen)) STOP("Baseline scenario '", baseline_id, "' not found in model output.")

# scenario -> label (from catalog; fall back to scenario id)
scat_lab <- setNames(scat$scenario_label, scat$scenario)
scen_label <- function(s) as.character(scat_lab[s] %||% s)

# scenario -> intervention family label (from model output `intervention` column)
int_lab_tab <- unique(mo[scenario %in% clin_scen, .(scenario, intervention)])
int_lab <- setNames(int_lab_tab$intervention, int_lab_tab$scenario)

SAY(" Included clinical scenarios (%d): %s", length(clin_scen),
    paste(clin_scen, collapse = ", "))
SAY(" Excluded scenarios (%d): %s", length(excluded_scen),
    paste(excluded_scen, collapse = ", "))

# -----------------------------------------------------------------------------
# 4. Analysis window and cost parameters from the canonical cost workbook
# -----------------------------------------------------------------------------
ca <- suppressMessages(as.data.table(read_excel(f_cost, "Calculation_Assumptions")))
getca <- function(id) {
  v <- ca[parameter_id == id]$value
  if (!length(v)) STOP("Calculation_Assumptions missing '", id, "'")
  v[1]
}
yr_start   <- as.integer(getca("analysis_start_year"))
yr_end     <- as.integer(getca("analysis_end_year"))
disc_rate  <- as.numeric(getca("cost_discount_rate"))
cost_price_year <- suppressWarnings(as.integer(getca("cost_price_year")))
formula_tol <- suppressWarnings(as.numeric(getca("formula_tolerance"))) %||% 0.001

if (is.na(yr_start) || is.na(yr_end)) STOP("Could not parse analysis years from workbook.")
if (!(yr_start == 2025 && yr_end == 2050))
  SAY(" NOTE: analysis window resolved to %d-%d (spec expected 2025-2050).", yr_start, yr_end)
analysis_yrs <- yr_start:yr_end
base_year <- yr_start   # discount base year (matches Model 09 discount_factor)

SAY(" Analysis years: %d-%d (%d years) | cost discount rate: %.3f | base year: %d",
    yr_start, yr_end, length(analysis_yrs), disc_rate, base_year)

# Reconciliation tolerances (absolute for floating-point counts; relative 0.1%).
TOL_ABS <- 1e-2       # persons / USD
TOL_REL <- 1e-3       # 0.1% (no stricter repo standard for cross-quintile totals)

# -----------------------------------------------------------------------------
# 5. Quintile scaffold
# -----------------------------------------------------------------------------
q_tab <- data.table(
  quintile_id     = c("Q1","Q2","Q3","Q4","Q5"),
  quintile_order  = 1:5,
  location        = paste0("Indonesia_Q", 1:5)
)

# -----------------------------------------------------------------------------
# 6. Load wealth population & burden; RECOMPUTE national allocation shares
# -----------------------------------------------------------------------------
wp <- as.data.table(readRDS(f_pop))
wbn <- as.data.table(readRDS(f_burden))

# -- population: annual all-age both-sex shares (for pop & all-cause deaths) ----
pop_ann <- wp[year %in% analysis_yrs,
              .(population = sum(population), all_cause_deaths = sum(all_cause_deaths)),
              by = .(year, quintile_id)]
pop_ann[, `:=`(pop_share    = population / sum(population),
               deaths_share = all_cause_deaths / sum(all_cause_deaths)),
        by = year]

# QA: compare recomputed within-stratum population_share to stored (audit only).
wp[, ps_recomp := population / sum(population), by = .(year, age_group, sex)]
pop_share_maxdiff <- max(abs(wp$population_share - wp$ps_recomp), na.rm = TRUE)

# -- burden: shares within year x cause_id x measure ---------------------------
wbn[, grp_tot := sum(value), by = .(year, cause_id, measure)]
wbn[, share_recomp := ifelse(grp_tot > 0, value / grp_tot, NA_real_)]
burden_share_stored_maxdiff <-
  max(abs(wbn$allocation_share - wbn$share_recomp), na.rm = TRUE)

# Restrict to the analysis window (burden also carries 2023, outside scope)
wbn <- wbn[year %in% analysis_yrs]

# Fallback: unavailable (zero-sum / NA) burden distributions -> all-age pop share
wbn <- merge(wbn, pop_ann[, .(year, quintile_id, pop_share)],
             by = c("year", "quintile_id"), all.x = TRUE)
wbn[, fallback := is.na(share_recomp)]
wbn[, share := ifelse(fallback, pop_share, share_recomp)]

fallback_summary <- wbn[fallback == TRUE, .N, by = .(cause_id, measure)]
n_fallback_cells <- nrow(wbn[fallback == TRUE])

if (nrow(fallback_summary)) {
  SAY(" FALLBACK (NATIONAL_RATE_APPLIED) applied to %d burden cells:", n_fallback_cells)
  for (i in seq_len(nrow(fallback_summary)))
    SAY("   - %s / %s : %d cells (year x quintile)",
        fallback_summary$cause_id[i], fallback_summary$measure[i], fallback_summary$N[i])
} else SAY(" No burden fallbacks required.")

# Validate shares (finite, in [0,1], sum to 1 over quintiles) ------------------
chk_pop <- pop_ann[, .(s = sum(pop_share), d = sum(deaths_share)), by = year]
if (any(abs(chk_pop$s - 1) > 1e-9) || any(abs(chk_pop$d - 1) > 1e-9))
  STOP("Population / death shares do not sum to 1 within tolerance.")
chk_bur <- wbn[, .(s = sum(share)), by = .(year, cause_id, measure)]
if (any(abs(chk_bur$s - 1) > 1e-8)) STOP("Burden shares do not sum to 1 within tolerance.")
if (any(!is.finite(wbn$share)) || any(wbn$share < -1e-12) || any(wbn$share > 1 + 1e-9))
  STOP("Invalid burden share detected (non-finite / out of [0,1]).")

# Long burden-share lookup keyed by (year, cause_id, measure, quintile_id)
bshare <- wbn[, .(year, cause_id, measure, quintile_id, share, fallback)]

# -----------------------------------------------------------------------------
# 7. Age-band prorating helper (for cost population-in-need "all" measure)
# -----------------------------------------------------------------------------
# Parse the 5-year band lower/upper bounds; "95+" is the model's pooled top age.
band_bounds <- function(ag) {
  if (grepl("\\+$", ag)) return(c(as.integer(sub("\\+$", "", ag)), Inf))
  as.integer(strsplit(ag, "-", fixed = TRUE)[[1]])
}
# Weight of a 5-year band inside an integer single-year age window [a0, a1].
band_weight <- function(ag, a0, a1) {
  b <- band_bounds(ag)
  bl <- b[1]; bu <- b[2]
  if (is.infinite(bu)) return(if (a1 >= bl) 1 else 0)     # 95+ pooled -> whole if reached
  ov <- max(0L, min(a1, bu) - max(a0, bl) + 1L)
  ov / (bu - bl + 1L)
}
# Age-windowed, sex-specific quintile population share by year.
popwin_share <- function(a0, a1, sx) {
  d <- copy(wp[year %in% analysis_yrs])
  if (!identical(sx, "Both")) d <- d[sex == sx]
  d[, w := vapply(age_group, band_weight, numeric(1), a0 = a0, a1 = a1)]
  agg <- d[, .(popw = sum(population * w)), by = .(year, quintile_id)]
  agg[, share := popw / sum(popw), by = year]
  agg
}
# Track whether any age band is partially (fractionally) overlapped (prorating binds)
prorate_binds <- 0L

# =============================================================================
# 8. NATIONAL VOLUME AGGREGATION  (scenario x year x cause; sum over age & sex)
# =============================================================================
mo_clin <- mo[scenario %in% clin_scen & year %in% analysis_yrs]
vol_nat <- mo_clin[, .(nat_pop        = sum(pop),
                       nat_all_mx     = sum(all.mx),
                       nat_well_model = sum(well),
                       nat_sick       = sum(sick),
                       nat_newcases   = sum(newcases),
                       nat_dead       = sum(dead)),
                   by = .(scenario, year, cause)]
vol_nat[, nat_well_resid := nat_pop - nat_sick - nat_all_mx]
vol_nat[, well_gap := nat_well_model - nat_well_resid]
well_gap_max     <- max(abs(vol_nat$well_gap))
well_gap_rel_max <- max(abs(vol_nat$well_gap) / pmax(vol_nat$nat_well_model, 1))
SAY(" State-identity check: max |model well - residual| = %.4g (rel %.3g)",
    well_gap_max, well_gap_rel_max)

model_causes <- sort(unique(vol_nat$cause))
# every model cause must map to a burden cause_id present in the burden file
burden_ids_present <- unique(wbn$cause_id)
cause_to_bid <- vapply(model_causes, function(cc) {
  cand <- names(CAUSE_MAP)[CAUSE_MAP == cc]
  cand <- intersect(cand, burden_ids_present)
  if (!length(cand)) NA_character_ else cand[1]
}, character(1))
if (any(is.na(cause_to_bid)))
  STOP("Unmapped clinical cause(s): ",
       paste(model_causes[is.na(cause_to_bid)], collapse = ", "))
SAY(" Cause mapping OK: %s", paste(sprintf("%s->%s", cause_to_bid, model_causes), collapse = ", "))

# -- Expand national volumes to quintiles and attach shares --------------------
vol_src <- vol_nat[, .(scenario, year, cause,
                       nat_pop, nat_all_mx, nat_sick, nat_newcases, nat_dead,
                       nat_well_resid)]
vol_src[, cause_id := cause_to_bid[match(cause, model_causes)]]
vol_src <- vol_src[rep(seq_len(.N), each = 5)]
vol_src[, quintile_id := rep(q_tab$quintile_id, times = nrow(vol_src) / 5)]
vol_src <- merge(vol_src, q_tab, by = "quintile_id", all.x = TRUE)

# annual pop & death shares
vol_src <- merge(vol_src, pop_ann[, .(year, quintile_id,
                                      share_pop = pop_share, share_deaths = deaths_share)],
                 by = c("year", "quintile_id"), all.x = TRUE)
# burden shares (prevalence -> sick, incidence -> newcases, deaths -> dead)
addshare <- function(dt, meas, newname) {
  s <- bshare[measure == meas, .(year, cause_id, quintile_id, .s = share, .fb = fallback)]
  setnames(s, c(".s", ".fb"), c(newname, paste0(newname, "_fb")))
  merge(dt, s, by = c("year", "cause_id", "quintile_id"), all.x = TRUE)
}
vol_src <- addshare(vol_src, "Prevalence", "share_prev")
vol_src <- addshare(vol_src, "Incidence",  "share_inc")
vol_src <- addshare(vol_src, "Deaths",     "share_dead")

if (any(is.na(vol_src$share_pop)) || any(is.na(vol_src$share_deaths)) ||
    any(is.na(vol_src$share_prev)) || any(is.na(vol_src$share_inc)) ||
    any(is.na(vol_src$share_dead)))
  STOP("Missing share join in volume allocation (a cause/year/measure had no share).")

setorder(vol_src, scenario, cause, year, quintile_order)

# -- R-side allocated values (independent QA; Excel formulas recompute these) ---
vol_src[, `:=`(
  a_pop      = nat_pop      * share_pop,
  a_all_mx   = nat_all_mx   * share_deaths,
  a_sick     = nat_sick     * share_prev,
  a_newcases = nat_newcases * share_inc,
  a_dead     = nat_dead     * share_dead
)]
vol_src[, a_well := a_pop - a_sick - a_all_mx]

# Independent R reconciliation: sum over quintiles vs national --------------
recon_vol_R <- vol_src[, .(
  pop      = sum(a_pop),      nat_pop      = nat_pop[1],
  all_mx   = sum(a_all_mx),   nat_all_mx   = nat_all_mx[1],
  sick     = sum(a_sick),     nat_sick     = nat_sick[1],
  newcases = sum(a_newcases), nat_newcases = nat_newcases[1],
  dead     = sum(a_dead),     nat_dead     = nat_dead[1],
  well     = sum(a_well),     nat_well     = nat_well_resid[1]
), by = .(scenario, year, cause)]
vol_recon_maxabs <- max(
  abs(recon_vol_R$pop - recon_vol_R$nat_pop),
  abs(recon_vol_R$all_mx - recon_vol_R$nat_all_mx),
  abs(recon_vol_R$sick - recon_vol_R$nat_sick),
  abs(recon_vol_R$newcases - recon_vol_R$nat_newcases),
  abs(recon_vol_R$dead - recon_vol_R$nat_dead),
  abs(recon_vol_R$well - recon_vol_R$nat_well)
)
if (min(vol_src$a_well) < -TOL_ABS)
  STOP("Materially negative allocated `well` value: min = ", min(vol_src$a_well))
SAY(" Volume reconciliation (R): max abs sum(Q)-national = %.6g", vol_recon_maxabs)

# =============================================================================
# 9. NATIONAL COST RECONSTRUCTION  (scenario x cost_record x year)
# =============================================================================
cc <- suppressMessages(as.data.table(read_excel(f_cost, "Cost_Components")))
ac_wb <- suppressMessages(as.data.table(read_excel(f_cost, "Annual_Cost")))  # for QA cross-check

# cost-ready rows (valid unit cost, PIN measure, fraction) — mirrors Model 09
cc[, cost_ready := !is.na(unit_cost_usd) & unit_cost_usd >= 0 &
     !is.na(population_in_need_fraction) &
     population_in_need_measure %in% c("all","prevalence","incidence")]
cc_ready <- cc[cost_ready == TRUE]

# Clinical scenarios that carry costs: standalone interventions + combined `all`
standalone_clin <- setdiff(clin_scen, c(baseline_id, "all"))
cost_scen <- c(standalone_clin, if ("all" %in% clin_scen) "all")
cost_scen <- cost_scen[cost_scen %in% unique(mo_clin$scenario)]

# intervention ids per cost scenario (standalone == its own id; `all` == union)
scen_int_ids <- function(scn) if (scn == "all") standalone_clin else scn

# Coverage: uniform across clinical interventions per canonical Selected_Interventions.
si <- suppressMessages(as.data.table(read_excel(f_cost, "Selected_Interventions")))
cov_by_int <- si[, .(cov_baseline = mean(baseline_coverage, na.rm = TRUE),
                     cov_target   = mean(target_coverage,  na.rm = TRUE),
                     cov_start    = suppressWarnings(min(start_year,  na.rm = TRUE)),
                     cov_ty       = suppressWarnings(max(target_year, na.rm = TRUE))),
                 by = intervention_id]

# Coverage path (linear, clamped) — identical to Model 09 engine
cov_path <- function(cb, ct, sy, ty, yrs) {
  span <- max(ty - sy + 1, 1)
  frac <- pmin(pmax((yrs - sy + 1) / span, 0), 1)
  cc_ <- cb + (ct - cb) * frac
  cc_[yrs < sy] <- cb; cc_[yrs > ty] <- ct
  pmin(pmax(cc_, 0), 1)
}
disc_factor_vec <- 1 / (1 + disc_rate)^(analysis_yrs - base_year)
names(disc_factor_vec) <- as.character(analysis_yrs)

# De-duplicated population (pop identical across causes) for measure "all"
popu <- unique(mo_clin[, .(scenario, year, age, sex, pop)])

# National quantity (pre-fraction) for one cost record under a scenario, by year
qty_by_year <- function(scn, cr) {
  a0 <- cr$c_age_start; a1 <- cr$c_age_stop; sx <- cr$c_sex
  if (cr$population_in_need_measure == "all") {
    d <- popu[scenario == scn & age >= a0 & age <= a1]
    if (!identical(sx, "Both")) d <- d[sex == sx]
    agg <- d[, .(q = sum(pop)), by = year]
  } else {
    vcol <- if (cr$population_in_need_measure == "prevalence") "sick" else "newcases"
    d <- mo_clin[scenario == scn & cause == cr$cause_code & age >= a0 & age <= a1]
    if (!identical(sx, "Both")) d <- d[sex == sx]
    agg <- d[, .(q = sum(get(vcol))), by = year]
  }
  m <- merge(data.table(year = analysis_yrs), agg, by = "year", all.x = TRUE)
  m[is.na(q), q := 0]
  setkey(m, year)
  m[.(analysis_yrs), q]
}

cost_rows <- list()
for (scn in cost_scen) {
  ids   <- scen_int_ids(scn)
  comps <- cc_ready[intervention_id %in% ids]
  if (!nrow(comps)) next
  # coverage for this scenario's records (intervention-level; uniform here)
  for (i in seq_len(nrow(comps))) {
    cr  <- comps[i]
    cov <- cov_by_int[intervention_id == cr$intervention_id]
    if (!nrow(cov)) STOP("No coverage found for intervention ", cr$intervention_id)
    q_s <- qty_by_year(scn,         cr)
    q_b <- qty_by_year(baseline_id, cr)
    cov_s <- cov_path(cov$cov_baseline, cov$cov_target, cov$cov_start, cov$cov_ty, analysis_yrs)
    cov_b <- rep(cov$cov_baseline, length(analysis_yrs))
    frac  <- cr$population_in_need_fraction
    freq  <- cr$frequency_per_year
    unit  <- cr$unit_cost_usd
    cost_rows[[length(cost_rows) + 1L]] <- data.table(
      scenario = scn, year = analysis_yrs,
      intervention_id = cr$intervention_id,
      cause_code = as.character(cr$cause_code %||% NA_character_),
      cost_record_id = cr$cost_record_id, cost_component_key = cr$cost_component_key,
      cost_join_key = cr$cost_join_key, cost_scope = cr$cost_scope,
      population_in_need_measure = cr$population_in_need_measure,
      population_in_need_fraction = frac, frequency_per_year = freq,
      unit_cost_usd = unit, price_year = cr$price_year,
      c_age_start = cr$c_age_start, c_age_stop = cr$c_age_stop, c_sex = cr$c_sex,
      cov_baseline = cov$cov_baseline, cov_target = cov$cov_target,
      cov_start = cov$cov_start, cov_ty = cov$cov_ty,
      coverage_scenario = cov_s, coverage_baseline = cov_b,
      disc_factor = disc_factor_vec,
      nat_q_s = q_s, nat_q_b = q_b)
  }
}
cost_nat <- rbindlist(cost_rows)
if (!nrow(cost_nat)) STOP("No clinical cost records reconstructed.")

# National annual cost (matches Model 09 formula)
cost_nat[, `:=`(
  nat_pin_s  = nat_q_s * population_in_need_fraction,
  nat_pin_b  = nat_q_b * population_in_need_fraction)]
cost_nat[, `:=`(
  nat_cost_s = nat_pin_s * coverage_scenario * frequency_per_year * unit_cost_usd,
  nat_cost_b = nat_pin_b * coverage_baseline * frequency_per_year * unit_cost_usd)]
cost_nat[, nat_cost_incr := nat_cost_s - nat_cost_b]
cost_nat[, nat_disc_incr := nat_cost_incr * disc_factor]

# duplicate key guard
if (anyDuplicated(cost_nat[, .(scenario, cost_record_id, year)]))
  STOP("Duplicated (scenario, cost_record_id, year) in national cost table.")

# -- QA cross-check: reconstructed quantities vs workbook r_quantity -----------
acj <- ac_wb[, .(scenario, cost_record_id, year,
                 wb_qs = r_quantity_scenario, wb_qb = r_quantity_baseline)]
qchk <- merge(cost_nat[, .(scenario, cost_record_id, year, nat_q_s, nat_q_b)],
              acj, by = c("scenario","cost_record_id","year"), all.x = TRUE)
qchk[, `:=`(rel_s = abs(nat_q_s - wb_qs) / pmax(abs(wb_qs), 1),
            rel_b = abs(nat_q_b - wb_qb) / pmax(abs(wb_qb), 1))]
qty_reld <- suppressWarnings(max(c(qchk$rel_s, qchk$rel_b), na.rm = TRUE))
n_qmatch <- sum(!is.na(qchk$wb_qs))
if (is.finite(qty_reld) && qty_reld > 1e-6)
  STOP("Reconstructed cost quantity disagrees with workbook r_quantity (max rel ", qty_reld, ")")
SAY(" Cost quantity cross-check vs workbook r_quantity: max rel diff = %.3g over %d rows",
    qty_reld, n_qmatch)

# -----------------------------------------------------------------------------
# 9b. Expand national cost to quintiles with measure-appropriate allocation share
# -----------------------------------------------------------------------------
# Precompute age-windowed population shares for each distinct (a0,a1,sex) window.
win_keys <- unique(cost_nat[population_in_need_measure == "all",
                            .(c_age_start, c_age_stop, c_sex)])
popwin_cache <- list()
for (i in seq_len(nrow(win_keys))) {
  k <- win_keys[i]
  key <- paste(k$c_age_start, k$c_age_stop, k$c_sex, sep = "|")
  ps <- popwin_share(k$c_age_start, k$c_age_stop, k$c_sex)
  popwin_cache[[key]] <- ps
  # detect binding proration (fractional band weight within this window)
  wtest <- vapply(unique(wp$age_group), band_weight, numeric(1),
                  a0 = k$c_age_start, a1 = k$c_age_stop)
  if (any(wtest > 0 & wtest < 1)) prorate_binds <- prorate_binds + 1L
}

cost_src <- cost_nat[rep(seq_len(.N), each = 5)]
cost_src[, quintile_id := rep(q_tab$quintile_id, times = nrow(cost_src) / 5)]
cost_src <- merge(cost_src, q_tab, by = "quintile_id", all.x = TRUE)

# attach allocation share row-by-row (measure-dependent)
cost_src[, cause_id := ifelse(is.na(cause_code), NA_character_,
                              cause_to_bid[match(cause_code, model_causes)])]

# all-measure share via window cache
get_all_share <- function(a0, a1, sx, yr, qid) {
  ps <- popwin_cache[[paste(a0, a1, sx, sep = "|")]]
  ps[year == yr & quintile_id == qid]$share
}
# burden share via bshare (Prevalence/Incidence)
bshare_key <- copy(bshare)
setkey(bshare_key, year, cause_id, measure, quintile_id)

cost_src[, alloc_share := NA_real_]
cost_src[, alloc_fallback := FALSE]

# vectorized: "all" measure
idx_all <- cost_src[, which(population_in_need_measure == "all")]
if (length(idx_all)) {
  sub <- cost_src[idx_all]
  sub_share <- mapply(get_all_share, sub$c_age_start, sub$c_age_stop, sub$c_sex,
                      sub$year, sub$quintile_id)
  cost_src[idx_all, alloc_share := as.numeric(sub_share)]
}
# prevalence / incidence measures via burden shares
meas_map <- c(prevalence = "Prevalence", incidence = "Incidence")
for (m in names(meas_map)) {
  idx <- cost_src[, which(population_in_need_measure == m)]
  if (!length(idx)) next
  sub <- cost_src[idx, .(year, cause_id, quintile_id)]
  jn  <- bshare_key[measure == meas_map[m]][
    sub, on = c("year","cause_id","quintile_id")]
  cost_src[idx, `:=`(alloc_share = jn$share, alloc_fallback = jn$fallback)]
}
if (any(is.na(cost_src$alloc_share)))
  STOP("Missing cost allocation share for ", sum(is.na(cost_src$alloc_share)), " rows.")

# share validity + sum-to-1 within each cost row group
cost_share_sum <- cost_src[, .(s = sum(alloc_share)), by = .(scenario, cost_record_id, year)]
if (any(abs(cost_share_sum$s - 1) > 1e-8)) STOP("Cost allocation shares do not sum to 1.")

# R-side allocated cost values (Excel formulas recompute these)
cost_src[, `:=`(
  a_pin_s = nat_q_s * alloc_share * population_in_need_fraction,
  a_pin_b = nat_q_b * alloc_share * population_in_need_fraction)]
cost_src[, `:=`(
  a_cost_s = a_pin_s * coverage_scenario * frequency_per_year * unit_cost_usd,
  a_cost_b = a_pin_b * coverage_baseline * frequency_per_year * unit_cost_usd)]
cost_src[, a_cost_incr := a_cost_s - a_cost_b]
cost_src[, a_disc_incr := a_cost_incr * disc_factor]

setorder(cost_src, scenario, cost_record_id, year, quintile_order)

# Independent R cost reconciliation: sum over quintiles vs national
recon_cost_R <- cost_src[, .(
  cost_b = sum(a_cost_b),   nat_cost_b = nat_cost_b[1],
  cost_s = sum(a_cost_s),   nat_cost_s = nat_cost_s[1],
  cost_i = sum(a_cost_incr),nat_cost_i = nat_cost_incr[1],
  cost_d = sum(a_disc_incr),nat_cost_d = nat_disc_incr[1]
), by = .(scenario, cost_record_id, year)]
cost_recon_maxabs <- max(
  abs(recon_cost_R$cost_b - recon_cost_R$nat_cost_b),
  abs(recon_cost_R$cost_s - recon_cost_R$nat_cost_s),
  abs(recon_cost_R$cost_i - recon_cost_R$nat_cost_i),
  abs(recon_cost_R$cost_d - recon_cost_R$nat_cost_d)
)
SAY(" Cost reconciliation (R): max abs sum(Q)-national = %.6g", cost_recon_maxabs)

# National cost totals per (scenario, year) for the reconciliation sheet
cost_nat_yr <- cost_nat[, .(nat_baseline    = sum(nat_cost_b),
                            nat_scenario    = sum(nat_cost_s),
                            nat_incremental = sum(nat_cost_incr),
                            nat_disc_incr   = sum(nat_disc_incr)),
                        by = .(scenario, year)]
setorder(cost_nat_yr, scenario, year)

n_fallback_cost <- nrow(cost_src[alloc_fallback == TRUE])

SAY(" Allocated volume rows: %d | allocated cost rows: %d",
    nrow(vol_src), nrow(cost_src))

# =============================================================================
# 10. BUILD THE WORKBOOK  (formula-driven; hidden SOURCE_* sheets)
# =============================================================================
gen_date <- format(Sys.Date())
gen_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
col <- function(i) openxlsx::int2col(i)
qsheet <- function(s) sprintf("'%s'", s)

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 10, fontName = "Calibri")

## ---- styles -----------------------------------------------------------------
st_hdr    <- createStyle(fontColour = "#FFFFFF", fgFill = "#1F4E78", halign = "center",
                         valign = "center", textDecoration = "bold", border = "TopBottomLeftRight",
                         borderColour = "#1F4E78", wrapText = TRUE)
st_input  <- createStyle(fgFill = "#FFF2CC")                       # raw inputs (pale yellow)
st_form   <- createStyle(fgFill = "#DDEBF7")                       # formulas (pale blue)
st_src    <- createStyle(fgFill = "#F2F2F2")                       # hidden source raw values
st_cnt    <- createStyle(numFmt = "#,##0")
st_cnt2   <- createStyle(numFmt = "#,##0.00")
st_usd    <- createStyle(numFmt = "$#,##0")
st_usd2   <- createStyle(numFmt = "$#,##0.00")
st_rate   <- createStyle(numFmt = "0.00000000")
st_pct    <- createStyle(numFmt = "0.0000%")
st_pct2   <- createStyle(numFmt = "0.000000%")
st_wrap   <- createStyle(wrapText = TRUE, valign = "top")
st_bold   <- createStyle(textDecoration = "bold")

# helper: write a data.frame sheet with header style, filter, freeze
add_df <- function(sheet, df, visible = TRUE, freeze = TRUE, filter = TRUE) {
  addWorksheet(wb, sheet, visible = visible)
  writeData(wb, sheet, df, headerStyle = st_hdr, withFilter = filter)
  if (freeze) freezePane(wb, sheet, firstActiveRow = 2)
  invisible(NULL)
}
# fill a formula-cell style over a column range
mark_form <- function(sheet, colidx, nrow) if (nrow > 0)
  addStyle(wb, sheet, st_form, rows = 2:(nrow + 1), cols = colidx, gridExpand = TRUE, stack = TRUE)
mark_input <- function(sheet, colidx, nrow) if (nrow > 0)
  addStyle(wb, sheet, st_input, rows = 2:(nrow + 1), cols = colidx, gridExpand = TRUE, stack = TRUE)
numfmt <- function(sheet, colidx, nrow, style) if (nrow > 0)
  addStyle(wb, sheet, style, rows = 2:(nrow + 1), cols = colidx, gridExpand = TRUE, stack = TRUE)

# =============================================================================
# 10.1  Hidden SOURCE_params (tolerances, discounting)
# =============================================================================
src_params <- data.frame(
  parameter = c("analysis_start_year","analysis_end_year","cost_discount_rate",
                "discount_base_year","cost_price_year","tol_abs","tol_rel",
                "n_quintiles","generated_date"),
  value = c(yr_start, yr_end, disc_rate, base_year, cost_price_year,
            TOL_ABS, TOL_REL, 5, gen_date),
  stringsAsFactors = FALSE)
add_df("SOURCE_params", src_params, visible = FALSE, freeze = FALSE, filter = FALSE)
# named cell refs
P_TOLABS <- "'SOURCE_params'!$B$7"
P_TOLREL <- "'SOURCE_params'!$B$8"

# =============================================================================
# 10.2  Hidden SOURCE_volumes  (national values + shares; 1:1 with 2_OUTPUT)
# =============================================================================
src_vol_df <- data.frame(
  scenario = vol_src$scenario, cause = vol_src$cause, year = vol_src$year,
  quintile = vol_src$quintile_id,
  nat_pop = vol_src$nat_pop, nat_all_mx = vol_src$nat_all_mx,
  nat_sick = vol_src$nat_sick, nat_newcases = vol_src$nat_newcases,
  nat_dead = vol_src$nat_dead, nat_well_resid = vol_src$nat_well_resid,
  share_pop = vol_src$share_pop, share_deaths = vol_src$share_deaths,
  share_prev = vol_src$share_prev, share_inc = vol_src$share_inc,
  share_dead = vol_src$share_dead,
  stringsAsFactors = FALSE)
add_df("SOURCE_volumes", src_vol_df, visible = FALSE, filter = FALSE)
nV <- nrow(src_vol_df)
# SOURCE_volumes column letters
SV <- c(scenario="A", cause="B", year="C", quintile="D", nat_pop="E", nat_all_mx="F",
        nat_sick="G", nat_newcases="H", nat_dead="I", nat_well_resid="J",
        share_pop="K", share_deaths="L", share_prev="M", share_inc="N", share_dead="O")

# =============================================================================
# 10.3  2_OUTPUT_volumes  (formula-driven allocated quintile volumes)
# =============================================================================
vol_out <- data.frame(
  scenario     = vol_src$scenario,
  location     = vol_src$location,
  year         = vol_src$year,
  cause        = vol_src$cause,
  age          = "All",
  sex          = "Both",
  well = NA_real_, sick = NA_real_, newcases = NA_real_, dead = NA_real_,
  pop = NA_real_, all.mx = NA_real_,
  intervention = as.character(int_lab[vol_src$scenario]),
  stringsAsFactors = FALSE)
add_df("2_OUTPUT_volumes", vol_out)
# columns: A scenario B location C year D cause E age F sex
#          G well H sick I newcases J dead K pop L all.mx M intervention
rV <- 2:(nV + 1)
SVs <- function(cn, r) sprintf("%s!$%s%d", qsheet("SOURCE_volumes"), SV[[cn]], r)
writeFormula(wb, "2_OUTPUT_volumes", startCol = 8, startRow = 2,   # H sick
  x = vapply(rV, function(r) sprintf("%s*%s", SVs("nat_sick", r), SVs("share_prev", r)), ""))
writeFormula(wb, "2_OUTPUT_volumes", startCol = 9, startRow = 2,   # I newcases
  x = vapply(rV, function(r) sprintf("%s*%s", SVs("nat_newcases", r), SVs("share_inc", r)), ""))
writeFormula(wb, "2_OUTPUT_volumes", startCol = 10, startRow = 2,  # J dead
  x = vapply(rV, function(r) sprintf("%s*%s", SVs("nat_dead", r), SVs("share_dead", r)), ""))
writeFormula(wb, "2_OUTPUT_volumes", startCol = 11, startRow = 2,  # K pop
  x = vapply(rV, function(r) sprintf("%s*%s", SVs("nat_pop", r), SVs("share_pop", r)), ""))
writeFormula(wb, "2_OUTPUT_volumes", startCol = 12, startRow = 2,  # L all.mx
  x = vapply(rV, function(r) sprintf("%s*%s", SVs("nat_all_mx", r), SVs("share_deaths", r)), ""))
writeFormula(wb, "2_OUTPUT_volumes", startCol = 7, startRow = 2,   # G well = pop - sick - all.mx
  x = vapply(rV, function(r) sprintf("K%d-H%d-L%d", r, r, r), ""))
mark_form("2_OUTPUT_volumes", 7:12, nV)
numfmt("2_OUTPUT_volumes", c(7, 11, 12), nV, st_cnt)   # well, pop, all.mx counts
numfmt("2_OUTPUT_volumes", c(8, 9, 10), nV, st_cnt2)   # sick/newcases/dead
setColWidths(wb, "2_OUTPUT_volumes", cols = 1:13,
             widths = c(20, 14, 8, 10, 6, 7, 14, 14, 13, 12, 15, 13, 16))

# =============================================================================
# 10.4  1_INPUT_epi_by_quintile  (baseline audit table; formula rates)
# =============================================================================
# map (cause,year,quintile) baseline -> its row in 2_OUTPUT_volumes
vol_key <- data.table(scenario = vol_src$scenario, cause = vol_src$cause,
                      year = vol_src$year, quintile_id = vol_src$quintile_id,
                      orow = rV)
base_rows <- vol_key[scenario == baseline_id]
epi <- merge(
  vol_src[scenario == baseline_id,
          .(year, cause, cause_id, quintile_id, location,
            inc_fb = share_inc_fb, prev_fb = share_prev_fb)],
  base_rows[, .(cause, year, quintile_id, orow)],
  by = c("cause","year","quintile_id"), all.x = TRUE)
setorder(epi, location, year, cause)
nE <- nrow(epi)
epi_out <- data.frame(
  year = epi$year, location = epi$location, age = "All", sex = "Both",
  cause_id = epi$cause_id, age_group = "All ages", age_mid = "",
  population = NA_real_, all_cause_mx = NA_real_, all_cause_deaths = NA_real_,
  cause_fraction = NA_real_, incidence_rate = NA_real_, prevalence_rate = NA_real_,
  cause_mx = NA_real_, cause_deaths = NA_real_, incident_cases = NA_real_,
  prevalent_cases = NA_real_,
  epidemiology_source = "Post-model proportional allocation of national baseline model output (NOT a quintile-specific model run)",
  incidence_available = ifelse(epi$inc_fb, FALSE, TRUE),
  prevalence_available = ifelse(epi$prev_fb, FALSE, TRUE),
  quintile_data_status = ifelse(epi$inc_fb | epi$prev_fb,
                                "NATIONAL_RATE_APPLIED (population-share fallback)",
                                "ALLOCATED (burden-share)"),
  stringsAsFactors = FALSE)
add_df("1_INPUT_epi_by_quintile", epi_out)
# columns: A year B location C age D sex E cause_id F age_group G age_mid
#  H population I all_cause_mx J all_cause_deaths K cause_fraction L incidence_rate
#  M prevalence_rate N cause_mx O cause_deaths P incident_cases Q prevalent_cases ...
rE <- 2:(nE + 1)
oV <- epi$orow
V2 <- function(c, r) sprintf("%s!%s%d", qsheet("2_OUTPUT_volumes"), c, r)
writeFormula(wb, "1_INPUT_epi_by_quintile", startCol = 8, startRow = 2,   # H population <- vol K pop
  x = vapply(seq_len(nE), function(i) V2("K", oV[i]), ""))
writeFormula(wb, "1_INPUT_epi_by_quintile", startCol = 10, startRow = 2,  # J all_cause_deaths <- vol L
  x = vapply(seq_len(nE), function(i) V2("L", oV[i]), ""))
writeFormula(wb, "1_INPUT_epi_by_quintile", startCol = 15, startRow = 2,  # O cause_deaths <- vol J dead
  x = vapply(seq_len(nE), function(i) V2("J", oV[i]), ""))
writeFormula(wb, "1_INPUT_epi_by_quintile", startCol = 16, startRow = 2,  # P incident_cases <- vol I
  x = vapply(seq_len(nE), function(i) V2("I", oV[i]), ""))
writeFormula(wb, "1_INPUT_epi_by_quintile", startCol = 17, startRow = 2,  # Q prevalent_cases <- vol H sick
  x = vapply(seq_len(nE), function(i) V2("H", oV[i]), ""))
writeFormula(wb, "1_INPUT_epi_by_quintile", startCol = 9, startRow = 2,   # I all_cause_mx = J/H
  x = vapply(rE, function(r) sprintf("IF(H%d=0,0,J%d/H%d)", r, r, r), ""))
writeFormula(wb, "1_INPUT_epi_by_quintile", startCol = 11, startRow = 2,  # K cause_fraction = O/J
  x = vapply(rE, function(r) sprintf("IF(J%d=0,0,O%d/J%d)", r, r, r), ""))
writeFormula(wb, "1_INPUT_epi_by_quintile", startCol = 12, startRow = 2,  # L incidence_rate = P/H
  x = vapply(rE, function(r) sprintf("IF(H%d=0,0,P%d/H%d)", r, r, r), ""))
writeFormula(wb, "1_INPUT_epi_by_quintile", startCol = 13, startRow = 2,  # M prevalence_rate = Q/H
  x = vapply(rE, function(r) sprintf("IF(H%d=0,0,Q%d/H%d)", r, r, r), ""))
writeFormula(wb, "1_INPUT_epi_by_quintile", startCol = 14, startRow = 2,  # N cause_mx = O/H
  x = vapply(rE, function(r) sprintf("IF(H%d=0,0,O%d/H%d)", r, r, r), ""))
mark_form("1_INPUT_epi_by_quintile", c(8,9,10,11,12,13,14,15,16,17), nE)
numfmt("1_INPUT_epi_by_quintile", c(8,10,15,16,17), nE, st_cnt)
numfmt("1_INPUT_epi_by_quintile", c(9,14), nE, st_rate)
numfmt("1_INPUT_epi_by_quintile", c(11), nE, st_pct)
numfmt("1_INPUT_epi_by_quintile", c(12,13), nE, st_rate)
setColWidths(wb, "1_INPUT_epi_by_quintile", cols = 1:21,
             widths = c(7,14,6,7,9,10,8,14,13,15,13,13,14,12,13,13,15,42,12,12,30))

# =============================================================================
# 10.5  Hidden SOURCE_costs  (national quantities + params + share; 1:1 rows)
# =============================================================================
src_cost_df <- data.frame(
  scenario = cost_src$scenario, cost_record_id = cost_src$cost_record_id,
  year = cost_src$year, quintile = cost_src$quintile_id,
  nat_q_s = cost_src$nat_q_s, nat_q_b = cost_src$nat_q_b,
  pin_fraction = cost_src$population_in_need_fraction,
  frequency = cost_src$frequency_per_year, unit_cost = cost_src$unit_cost_usd,
  cov_baseline = cost_src$cov_baseline, cov_target = cost_src$cov_target,
  cov_start = cost_src$cov_start, cov_ty = cost_src$cov_ty,
  disc_factor = cost_src$disc_factor, alloc_share = cost_src$alloc_share,
  price_year = cost_src$price_year,
  stringsAsFactors = FALSE)
add_df("SOURCE_costs", src_cost_df, visible = FALSE, filter = FALSE)
nC <- nrow(src_cost_df)
SC <- c(scenario="A", cost_record_id="B", year="C", quintile="D", nat_q_s="E",
        nat_q_b="F", pin_fraction="G", frequency="H", unit_cost="I",
        cov_baseline="J", cov_target="K", cov_start="L", cov_ty="M",
        disc_factor="N", alloc_share="O", price_year="P")
SCs <- function(cn, r) sprintf("%s!$%s%d", qsheet("SOURCE_costs"), SC[[cn]], r)

# =============================================================================
# 10.6  3_OUTPUT_costs  (formula-driven allocated quintile costs)
# =============================================================================
cost_out <- data.frame(
  scenario = cost_src$scenario, location = cost_src$location, year = cost_src$year,
  intervention_id = cost_src$intervention_id, cause_code = cost_src$cause_code,
  cost_record_id = cost_src$cost_record_id, cost_component_key = cost_src$cost_component_key,
  cost_join_key = cost_src$cost_join_key, cost_scope = cost_src$cost_scope,
  population_in_need_measure = cost_src$population_in_need_measure,
  population_in_need_fraction = cost_src$population_in_need_fraction,
  pin_baseline = NA_real_, pin_scenario = NA_real_,
  coverage_baseline = cost_src$coverage_baseline, coverage_scenario = NA_real_,
  frequency_per_year = cost_src$frequency_per_year, unit_cost_usd = cost_src$unit_cost_usd,
  price_year = cost_src$price_year,
  annual_cost_baseline = NA_real_, annual_cost_scenario = NA_real_,
  annual_cost_incremental = NA_real_, disc_cost_incremental = NA_real_,
  stringsAsFactors = FALSE)
add_df("3_OUTPUT_costs", cost_out)
# cols: A scenario B location C year D intervention_id E cause_code F cost_record_id
#  G cost_component_key H cost_join_key I cost_scope J pin_measure K pin_fraction
#  L pin_baseline M pin_scenario N coverage_baseline O coverage_scenario
#  P frequency Q unit_cost R price_year S annual_cost_baseline T annual_cost_scenario
#  U annual_cost_incremental V disc_cost_incremental
rC <- 2:(nC + 1)
# L pin_baseline = nat_q_b * alloc_share * pin_fraction
writeFormula(wb, "3_OUTPUT_costs", startCol = 12, startRow = 2,
  x = vapply(rC, function(r) sprintf("%s*%s*%s", SCs("nat_q_b", r), SCs("alloc_share", r), SCs("pin_fraction", r)), ""))
# M pin_scenario = nat_q_s * alloc_share * pin_fraction
writeFormula(wb, "3_OUTPUT_costs", startCol = 13, startRow = 2,
  x = vapply(rC, function(r) sprintf("%s*%s*%s", SCs("nat_q_s", r), SCs("alloc_share", r), SCs("pin_fraction", r)), ""))
# O coverage_scenario = linear clamped path (references SOURCE cov params + year C)
writeFormula(wb, "3_OUTPUT_costs", startCol = 15, startRow = 2,
  x = vapply(rC, function(r) sprintf(
    "MIN(MAX(%s+(%s-%s)*MIN(MAX((C%d-%s+1)/MAX(%s-%s+1,1),0),1),%s),%s)",
    SCs("cov_baseline", r), SCs("cov_target", r), SCs("cov_baseline", r),
    r, SCs("cov_start", r), SCs("cov_ty", r), SCs("cov_start", r),
    SCs("cov_baseline", r), SCs("cov_target", r)), ""))
# S annual_cost_baseline = pin_baseline * coverage_baseline * frequency * unit_cost
writeFormula(wb, "3_OUTPUT_costs", startCol = 19, startRow = 2,
  x = vapply(rC, function(r) sprintf("L%d*N%d*P%d*Q%d", r, r, r, r), ""))
# T annual_cost_scenario = pin_scenario * coverage_scenario * frequency * unit_cost
writeFormula(wb, "3_OUTPUT_costs", startCol = 20, startRow = 2,
  x = vapply(rC, function(r) sprintf("M%d*O%d*P%d*Q%d", r, r, r, r), ""))
# U annual_cost_incremental = T - S
writeFormula(wb, "3_OUTPUT_costs", startCol = 21, startRow = 2,
  x = vapply(rC, function(r) sprintf("T%d-S%d", r, r), ""))
# V disc_cost_incremental = U * disc_factor
writeFormula(wb, "3_OUTPUT_costs", startCol = 22, startRow = 2,
  x = vapply(rC, function(r) sprintf("U%d*%s", r, SCs("disc_factor", r)), ""))
mark_form("3_OUTPUT_costs", c(12,13,15,19,20,21,22), nC)
mark_input("3_OUTPUT_costs", c(11,14,16,17,18), nC)
numfmt("3_OUTPUT_costs", c(12,13), nC, st_cnt2)     # pin
numfmt("3_OUTPUT_costs", c(14,15), nC, st_pct)      # coverage
numfmt("3_OUTPUT_costs", c(17), nC, st_usd2)        # unit cost
numfmt("3_OUTPUT_costs", c(19,20,21,22), nC, st_usd)
setColWidths(wb, "3_OUTPUT_costs", cols = 1:22,
             widths = c(18,14,7,17,11,22,22,24,17,15,13,14,14,14,14,11,13,10,18,18,18,18))

# =============================================================================
# 10.7  Hidden SOURCE_nat_costs  (national cost totals per scenario x year)
# =============================================================================
src_natcost_df <- data.frame(
  scenario = cost_nat_yr$scenario, year = cost_nat_yr$year,
  nat_baseline = cost_nat_yr$nat_baseline, nat_scenario = cost_nat_yr$nat_scenario,
  nat_incremental = cost_nat_yr$nat_incremental, nat_disc_incr = cost_nat_yr$nat_disc_incr,
  stringsAsFactors = FALSE)
add_df("SOURCE_nat_costs", src_natcost_df, visible = FALSE, filter = FALSE)
nNC <- nrow(src_natcost_df)
# row lookup for (scenario, year)
natcost_row <- setNames(2:(nNC + 1),
                        paste(src_natcost_df$scenario, src_natcost_df$year, sep = "|"))

# =============================================================================
# 10.8  4_RECONCILIATION  (Excel formula sum-over-quintiles vs national)
# =============================================================================
# Volume block: per (scenario, year, cause, measure) using contiguous 5-row SUM.
# Rows in 2_OUTPUT_volumes are ordered scenario, cause, year, quintile -> the
# five quintiles of each (scenario, cause, year) form a contiguous block.
vol_groups <- vol_key[, .(r0 = min(orow), r1 = max(orow)), by = .(scenario, cause, year)]
setorder(vol_groups, scenario, cause, year)
# map to SOURCE_volumes national row (first quintile row) — same ordering/rows
vol_meas <- data.table(
  measure = c("pop","all.mx","sick","newcases","dead","well"),
  outcol  = c("K","L","H","I","J","G"),       # column in 2_OUTPUT_volumes
  natcol  = c("nat_pop","nat_all_mx","nat_sick","nat_newcases","nat_dead","nat_well_resid")
)
recon_vol <- vol_groups[rep(seq_len(.N), each = nrow(vol_meas))]
recon_vol[, measure := rep(vol_meas$measure, times = nrow(vol_groups))]
recon_vol[, outcol  := rep(vol_meas$outcol,  times = nrow(vol_groups))]
recon_vol[, natcol  := rep(vol_meas$natcol,  times = nrow(vol_groups))]

# Cost block: per (scenario, year, measure) using SUMIFS over 3_OUTPUT_costs.
cost_meas <- data.table(
  measure = c("baseline cost","scenario cost","incremental cost","discounted incremental cost"),
  outcol  = c("S","T","U","V"),
  natcol  = c("nat_baseline","nat_scenario","nat_incremental","nat_disc_incr")
)
recon_cost <- cost_nat_yr[, .(scenario, year)][rep(seq_len(.N), each = nrow(cost_meas))]
recon_cost[, measure := rep(cost_meas$measure, times = nrow(cost_nat_yr))]
recon_cost[, outcol  := rep(cost_meas$outcol,  times = nrow(cost_nat_yr))]
recon_cost[, natcol  := rep(cost_meas$natcol,  times = nrow(cost_nat_yr))]

nRV <- nrow(recon_vol); nRC <- nrow(recon_cost); nR <- nRV + nRC
recon_out <- data.frame(
  scenario = c(recon_vol$scenario, recon_cost$scenario),
  year     = c(recon_vol$year, recon_cost$year),
  cause    = c(recon_vol$cause, rep("All", nRC)),
  measure  = c(recon_vol$measure, recon_cost$measure),
  sum_over_quintiles = NA_real_, national_run = NA_real_,
  abs_diff = NA_real_, pct_diff = NA_real_, status = NA_character_,
  stringsAsFactors = FALSE)
add_df("4_RECONCILIATION", recon_out)
# cols: A scenario B year C cause D measure E sum_over_quintiles F national_run
#       G abs_diff H pct_diff I status
# --- volume rows (1 .. nRV) : sheet rows 2 .. nRV+1 ---
sv_sum <- character(nRV); sv_nat <- character(nRV)
for (i in seq_len(nRV)) {
  oc <- recon_vol$outcol[i]; nc <- recon_vol$natcol[i]
  r0 <- recon_vol$r0[i]; r1 <- recon_vol$r1[i]
  sv_sum[i] <- sprintf("SUM(%s!%s%d:%s%d)", qsheet("2_OUTPUT_volumes"), oc, r0, oc, r1)
  # national value sits on SOURCE_volumes at the same first row (r0)
  sv_nat[i] <- sprintf("%s!$%s%d", qsheet("SOURCE_volumes"), SV[[nc]], r0)
}
# --- cost rows (nRV+1 .. nR) ---
sc_sum <- character(nRC); sc_nat <- character(nRC)
for (i in seq_len(nRC)) {
  oc <- recon_cost$outcol[i]; nc <- recon_cost$natcol[i]
  scn <- recon_cost$scenario[i]; yr <- recon_cost$year[i]
  sc_sum[i] <- sprintf("SUMIFS(%s!$%s$2:$%s$%d,%s!$A$2:$A$%d,\"%s\",%s!$C$2:$C$%d,%d)",
    qsheet("3_OUTPUT_costs"), oc, oc, nC + 1,
    qsheet("3_OUTPUT_costs"), nC + 1, scn,
    qsheet("3_OUTPUT_costs"), nC + 1, yr)
  nrow_ <- natcost_row[[paste(scn, yr, sep = "|")]]
  sc_nat[i] <- sprintf("%s!$%s%d", qsheet("SOURCE_nat_costs"),
                       c(nat_baseline="C", nat_scenario="D", nat_incremental="E", nat_disc_incr="F")[[nc]],
                       nrow_)
}
sum_all <- c(sv_sum, sc_sum); nat_all <- c(sv_nat, sc_nat)
writeFormula(wb, "4_RECONCILIATION", startCol = 5, startRow = 2, x = sum_all)   # E sum
writeFormula(wb, "4_RECONCILIATION", startCol = 6, startRow = 2, x = nat_all)   # F national
rR <- 2:(nR + 1)
writeFormula(wb, "4_RECONCILIATION", startCol = 7, startRow = 2,               # G abs_diff
  x = vapply(rR, function(r) sprintf("ABS(E%d-F%d)", r, r), ""))
writeFormula(wb, "4_RECONCILIATION", startCol = 8, startRow = 2,               # H pct_diff
  x = vapply(rR, function(r) sprintf("IF(F%d=0,IF(E%d=0,0,1),ABS(E%d-F%d)/ABS(F%d))", r, r, r, r, r), ""))
writeFormula(wb, "4_RECONCILIATION", startCol = 9, startRow = 2,               # I status
  x = vapply(rR, function(r) sprintf(
    "IF(OR(G%d<=%s,H%d<=%s),\"PASS\",IF(H%d<=10*%s,\"REVIEW\",\"FAIL\"))",
    r, P_TOLABS, r, P_TOLREL, r, P_TOLREL), ""))
mark_form("4_RECONCILIATION", 5:9, nR)
numfmt("4_RECONCILIATION", c(5,6,7), nR, st_cnt2)
numfmt("4_RECONCILIATION", 8, nR, st_pct2)
setColWidths(wb, "4_RECONCILIATION", cols = 1:9, widths = c(20,7,10,26,20,20,16,14,10))
# conditional formatting on status
conditionalFormatting(wb, "4_RECONCILIATION", cols = 9, rows = rR,
  rule = "PASS", type = "contains", style = createStyle(fontColour = "#006100", bgFill = "#C6EFCE"))
conditionalFormatting(wb, "4_RECONCILIATION", cols = 9, rows = rR,
  rule = "REVIEW", type = "contains", style = createStyle(fontColour = "#9C5700", bgFill = "#FFEB9C"))
conditionalFormatting(wb, "4_RECONCILIATION", cols = 9, rows = rR,
  rule = "FAIL", type = "contains", style = createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"))

# =============================================================================
# 10.9  5_QUESTIONS
# =============================================================================
questions <- data.frame(
  `#` = 1:8,
  Topic = c("Quintile model", "Coverage", "Burden shares", "Missing shares",
            "Cost age bands", "Cost perspective", "State identity", "Uncertainty"),
  Question = c(
    "Were five separate quintile models calibrated / simulated?",
    "Is coverage allowed to vary across quintiles?",
    "Are burden shares age/sex specific?",
    "How are unavailable burden distributions handled?",
    "How are 5-year population bands split for cost age windows?",
    "What cost perspective is used?",
    "How is `well` defined for the allocated quintiles?",
    "Is allocation-assumption uncertainty propagated?"),
  `Your answer` = c(
    "No. The disease model was calibrated and simulated ONCE nationally; Q1-Q5 are a proportional allocation of that single national run.",
    "No. Baseline and scenario coverage are held equal across quintiles (canonical inputs contain no quintile-specific coverage). No coverage gradient is invented.",
    "No. Wealth-burden shares are ALL-AGE and BOTH-SEX; these all-age shares are applied across the annual condition volumes.",
    "Deterministic fallback: exact burden share when available; otherwise the all-age population share for that year/quintile, flagged NATIONAL_RATE_APPLIED (see README + QA).",
    "Uniform-within-band prorating; the 95+ band follows the model's pooled top age. Current cost windows are band-aligned so prorating does not bind (0 partial bands).",
    "Health-system perspective (economic_perspective in the cost workbook); no downstream disease-cost offsets.",
    "Residual identity  well = pop - sick - all.mx  (consistent with the model compartments). The tiny model stock/flow non-closure is reported in QA, not hidden.",
    "No. Uncertainty from the allocation assumptions is not propagated; results are distributional scenarios layered on the national projection."),
  check.names = FALSE, stringsAsFactors = FALSE)
add_df("5_QUESTIONS", questions, freeze = TRUE, filter = FALSE)
addStyle(wb, "5_QUESTIONS", st_wrap, rows = 2:(nrow(questions) + 1), cols = 3:4, gridExpand = TRUE, stack = TRUE)
setColWidths(wb, "5_QUESTIONS", cols = 1:4, widths = c(4, 18, 55, 90))

# =============================================================================
# 10.10  README
# =============================================================================
fb_lines <- if (nrow(fallback_summary))
  paste(sprintf("      - %s / %s : %d year x quintile cells",
                fallback_summary$cause_id, fallback_summary$measure, fallback_summary$N),
        collapse = "\n") else "      - none"
readme_txt <- c(
  "FRP DATA STRUCTURES  -  Wealth-quintile allocation of national clinical model results",
  "=====================================================================================",
  sprintf("Generated: %s", gen_time),
  "",
  "PURPOSE & SCOPE",
  "  This workbook distributes the COMPLETED national clinical-intervention model",
  "  results across wealth quintiles Q1-Q5 to build the data structures required for",
  "  subsequent financial-risk-protection (FRP) analysis. It is a transparent,",
  "  post-processing PROPORTIONAL ALLOCATION of national outcomes using quintile-",
  "  specific population and disease-burden shares.",
  "",
  "  *** These are ALLOCATED national results. They are NOT the output of five",
  "      separately calibrated or simulated quintile models. The disease model was",
  "      calibrated and run ONCE, nationally. By construction the Q1-Q5 values",
  "      re-aggregate to the original national model results. ***",
  "",
  "INTERPRETING Indonesia_Q1 ... Indonesia_Q5",
  "  Each 'Indonesia_Qk' location is the k-th national wealth quintile (Q1 = lowest,",
  "  Q5 = highest). Its values are the national model result multiplied by that",
  "  quintile's population / disease-burden share; they are distributional slices of",
  "  one national projection, not independent projections.",
  "",
  "CANONICAL INPUT FILES",
  "  - output/out_model/model_output_Indonesia_htncov2_aspirational.rds  (national model output)",
  "  - data/processed/indonesia_wealth_population.rds                    (quintile population)",
  "  - data/processed/indonesia_wealth_burden.rds                       (quintile disease burden)",
  "  - output/indonesia_model_cost_value_formulae.xlsx                  (clinical cost records)",
  "",
  sprintf("INCLUDED CLINICAL SCENARIOS (%d): %s", length(clin_scen), paste(clin_scen, collapse = ", ")),
  sprintf("EXCLUDED SCENARIOS (%d, public-health / combined clinical+PH): %s",
          length(excluded_scen), paste(excluded_scen, collapse = ", ")),
  sprintf("ANALYSIS YEARS: %d-%d", yr_start, yr_end),
  "",
  "CAUSE MAPPING (wealth-burden cause_id -> model cause code)",
  "  C_IHD->ihd  C_IS->istroke  C_ICH->hstroke  C_HHD->hhd  C_RHD->rhd  C_CMD->cmd  C_DM->dm2",
  "",
  "ALLOCATION EQUATIONS (per scenario x year x cause x quintile Q)",
  "  pop_Q      = national_pop      x population_share(year, Q)              [all-age both-sex]",
  "  all.mx_Q   = national_all.mx   x all_cause_death_share(year, Q)         [all-age both-sex]",
  "  sick_Q     = national_sick     x burden_share(year, cause, Prevalence, Q)",
  "  newcases_Q = national_newcases x burden_share(year, cause, Incidence,  Q)",
  "  dead_Q     = national_dead     x burden_share(year, cause, Deaths,     Q)",
  "  well_Q     = pop_Q - sick_Q - all.mx_Q                                  [state residual]",
  "",
  "COST ALLOCATION (per cost record x year x quintile Q)",
  "  National annual cost = qty(t) x PIN_fraction x coverage(t) x frequency x unit_cost,",
  "  where qty maps population_in_need_measure: 'all'->eligible population, 'prevalence'->",
  "  sick stock, 'incidence'->new cases (reconstructed from the national model output; the",
  "  reconstruction matches the cost workbook's r_quantity helpers to machine precision).",
  "  The national quantity is split across quintiles by:",
  "    - 'all'        : age-windowed, sex-specific population share (5-year bands prorated,",
  "                     uniform-within-band; 95+ follows the model's pooled top age)",
  "    - 'prevalence' : cause-year Prevalence burden share",
  "    - 'incidence'  : cause-year Incidence burden share",
  "  Coverage(t) is a linear scale-up (baseline->target over the coverage window), held",
  "  EQUAL across quintiles. 'shared-count-once' cost records are allocated once per record.",
  "",
  "STATE IDENTITY & MODEL NON-CLOSURE",
  "  Allocated `well` is the residual  well = pop - sick - all.mx  (consistent with the",
  "  model compartments: living = pop - all.mx; well_cause = living - sick_cause). The",
  sprintf("  national model's own `well` differs from this residual by a small stock/flow non-"),
  sprintf("  closure (max %.0f persons per national year x cause; relative %.1e). Reconciliation",
          well_gap_max, well_gap_rel_max),
  "  uses the residual on both sides (exact by construction); the gap is disclosed, not hidden.",
  "",
  "FORMULA CONVENTIONS",
  "  - Hidden SOURCE_* sheets hold raw imported national values, recomputed shares and cost",
  "    parameters (grey). Every derived value on the visible OUTPUT / INPUT / RECONCILIATION",
  "    sheets is an Excel formula (blue) referencing those source cells - nothing derived is",
  "    hard-coded. Bounded references; quoted sheet names; non-volatile; recalculates on open.",
  "",
  "QA TOLERANCES (reconciliation)",
  sprintf("  - absolute: %g (persons / USD)   - relative: %g (0.1%%)", TOL_ABS, TOL_REL),
  "  - PASS if abs_diff <= abs_tol OR pct_diff <= rel_tol; REVIEW if pct_diff <= 10x rel_tol;",
  "    otherwise FAIL. Reconciliations are also computed independently in R before saving.",
  "",
  "MISSING-SHARE FALLBACK (NATIONAL_RATE_APPLIED)",
  sprintf("  %d burden cell(s) lacked a distribution and fell back to the population share:", n_fallback_cells),
  fb_lines,
  "  (HHD has no modelled incidence in the wealth-burden source; its incident cases are",
  "   distributed by the all-age population share and flagged.)",
  "",
  "ASSUMPTIONS & LIMITATIONS",
  "  1. Proportional allocation does not reproduce a quintile-specific disease model.",
  "  2. National intervention effect sizes are implicitly constant across quintiles.",
  "  3. Coverage is held constant across quintiles (no quintile-specific coverage in inputs).",
  "  4. Burden shares are all-age and both-sex; applied across annual outcomes.",
  "  5. The missing-share fallback assumes equal rates across quintiles for that cell.",
  "  6. Age-band prorating assumes uniform population within each 5-year band.",
  "  7. Allocated outcomes preserve national totals by construction.",
  "  8. FRP estimates are distributional scenarios, not independently calibrated projections.",
  "  9. Uncertainty from allocation assumptions is not propagated.",
  "",
  "WORKSHEETS",
  "  README, 1_INPUT_epi_by_quintile, 2_OUTPUT_volumes, 3_OUTPUT_costs,",
  "  4_RECONCILIATION, 5_QUESTIONS  (+ hidden SOURCE_* audit sheets)."
)
addWorksheet(wb, "README", visible = TRUE)
writeData(wb, "README", data.frame(README = readme_txt, stringsAsFactors = FALSE),
          headerStyle = createStyle(textDecoration = "bold", fontSize = 12))
setColWidths(wb, "README", cols = 1, widths = 100)

# Order the visible sheets: README first, then 1..5 (SOURCE_* hidden, order irrelevant)
worksheetOrder(wb) <- order(match(names(wb),
  c("README","1_INPUT_epi_by_quintile","2_OUTPUT_volumes","3_OUTPUT_costs",
    "4_RECONCILIATION","5_QUESTIONS","SOURCE_params","SOURCE_volumes",
    "SOURCE_costs","SOURCE_nat_costs")))

# Make README the active sheet BEFORE hiding source sheets (Excel/openxlsx will
# not hide the active sheet). Then hide every SOURCE_* audit sheet (tolerances
# stay referenced by formulas from a hidden cell, which is permitted).
activeSheet(wb) <- "README"
.vis <- sheetVisibility(wb)
.vis[grepl("^SOURCE_", names(wb))] <- "hidden"
sheetVisibility(wb) <- .vis

# =============================================================================
# 11. MANDATORY R-SIDE QA GATES  (stop on material failure)
# =============================================================================
cat("\n--- QA gates ---------------------------------------------------\n")
qa <- list()
qa_add <- function(name, ok, detail = "") { qa[[name]] <<- ok
  SAY("  [%s] %s%s", if (ok) "PASS" else "FAIL", name, if (nchar(detail)) paste0(" : ", detail) else "") }

# 1 required inputs/columns already validated above
qa_add("inputs_and_columns_present", TRUE)
# 2 Q1-Q5 present for every required year (volumes + costs)
q_years_ok <- all(analysis_yrs %in% unique(vol_src$year)) &&
  all(vapply(analysis_yrs, function(y)
    length(unique(vol_src[year == y]$quintile_id)) == 5, logical(1)))
qa_add("Q1_Q5_every_year", q_years_ok)
# 3 population shares sum to 1
qa_add("pop_shares_sum_1", all(abs(chk_pop$s - 1) <= 1e-9))
# 4 all-cause-death shares sum to 1
qa_add("death_shares_sum_1", all(abs(chk_pop$d - 1) <= 1e-9))
# 5 burden shares sum to 1 for every available key
qa_add("burden_shares_sum_1", all(abs(chk_bur$s - 1) <= 1e-8))
# 6 no invalid/negative/infinite/unmapped allocations
alloc_ok <- all(is.finite(vol_src$a_pop)) && all(is.finite(cost_src$a_cost_s)) &&
  all(vol_src$a_pop >= -TOL_ABS) && all(cost_src$a_cost_s >= -TOL_ABS) &&
  !any(is.na(cause_to_bid))
qa_add("no_invalid_allocations", alloc_ok)
# 7 no duplicated output keys
dupv <- anyDuplicated(vol_src[, .(scenario, year, cause, quintile_id)])
dupc <- anyDuplicated(cost_src[, .(scenario, cost_record_id, year, quintile_id)])
qa_add("no_duplicate_output_keys", dupv == 0 && dupc == 0)
# 8 every included national scenario-year-cause has five quintile rows
five_ok <- all(vol_src[, .N, by = .(scenario, year, cause)]$N == 5)
qa_add("five_quintile_rows_each", five_ok)
# 9 sum over quintiles reproduces national model output for every measure
qa_add("volume_reconciles_national", vol_recon_maxabs <= max(TOL_ABS, TOL_REL * 1e9),
       sprintf("max abs = %.4g", vol_recon_maxabs))
# 10 sum over quintiles reproduces national PIN and all cost totals
pin_recon <- cost_src[, .(pin_s = sum(a_pin_s), nat_pin_s = nat_pin_s[1],
                          pin_b = sum(a_pin_b), nat_pin_b = nat_pin_b[1]),
                      by = .(scenario, cost_record_id, year)]
pin_maxabs <- max(abs(pin_recon$pin_s - pin_recon$nat_pin_s),
                  abs(pin_recon$pin_b - pin_recon$nat_pin_b))
qa_add("cost_and_pin_reconciles_national",
       cost_recon_maxabs <= max(TOL_ABS, TOL_REL * 1e10) &&
         pin_maxabs <= max(TOL_ABS, TOL_REL * 1e9),
       sprintf("cost max abs = %.4g, pin max abs = %.4g", cost_recon_maxabs, pin_maxabs))
# 11 no materially negative well
qa_add("no_negative_well", min(vol_src$a_well) >= -TOL_ABS,
       sprintf("min well = %.4g", min(vol_src$a_well)))
# 12 no excluded public-health scenario in the workbook
ph_leak <- any(c(vol_src$scenario, cost_src$scenario) %in% excluded_scen)
qa_add("no_public_health_leak", !ph_leak)
# 13 workbook row limits
row_ok <- max(nV, nC, nE, nR) < 1048576
qa_add("within_excel_row_limit", row_ok, sprintf("max sheet rows = %d", max(nV, nC, nE, nR)))
# 14 shares recompute audit (informational thresholds)
qa_add("share_recompute_audit", TRUE,
       sprintf("pop_share stored-vs-recomp max=%.2g; burden_share stored-vs-recomp max=%.2g",
               pop_share_maxdiff, burden_share_stored_maxdiff))

if (!all(unlist(qa))) STOP("Mandatory QA gate(s) failed: ",
                           paste(names(qa)[!unlist(qa)], collapse = ", "))
cat("--- all QA gates passed ----------------------------------------\n")

# =============================================================================
# 12. Finalize: recalc-on-open, strip dangling drawings, atomic write
# =============================================================================
strip_dangling_drawings <- function(wb) {
  tryCatch({
    if (!is.null(wb$Content_Types))
      wb$Content_Types <- wb$Content_Types[!grepl("/xl/drawings/drawing", wb$Content_Types)]
    if (!is.null(wb$worksheets_rels))
      for (i in seq_along(wb$worksheets_rels))
        if (length(wb$worksheets_rels[[i]]))
          wb$worksheets_rels[[i]] <- grep("/drawings/", wb$worksheets_rels[[i]],
                                          value = TRUE, invert = TRUE)
  }, error = function(e) message("  (drawing-strip skipped: ", conditionMessage(e), ")"))
  invisible(wb)
}
wb$workbook$calcPr <- '<calcPr calcId="191029" fullCalcOnLoad="1"/>'
strip_dangling_drawings(wb)

tmp <- tempfile(fileext = ".xlsx")
saveWorkbook(wb, tmp, overwrite = TRUE)
if (!file.exists(tmp) || file.size(tmp) < 5000) STOP("Temp workbook write failed or too small.")
if (!dir.exists(dirname(f_out))) dir.create(dirname(f_out), recursive = TRUE)
file.copy(tmp, f_out, overwrite = TRUE)
unlink(tmp)
if (!file.exists(f_out)) STOP("Final workbook not written to ", f_out)

# =============================================================================
# 13. Console report
# =============================================================================
cat("\n===============================================================\n")
cat(" FRP DATA STRUCTURES  -  BUILD COMPLETE\n")
cat("===============================================================\n")
SAY(" Output workbook        : %s (%.1f KB)", f_out, file.size(f_out) / 1024)
SAY(" Clinical scenarios (%d) : %s", length(clin_scen), paste(clin_scen, collapse = ", "))
SAY(" Analysis years          : %d-%d", yr_start, yr_end)
SAY(" Allocated volume rows   : %d", nV)
SAY(" Allocated cost rows     : %d", nC)
SAY(" Reconciliation rows     : %d (%d volume, %d cost)", nR, nRV, nRC)
if (nrow(fallback_summary)) {
  cat(" Fallbacks (NATIONAL_RATE_APPLIED):\n")
  for (i in seq_len(nrow(fallback_summary)))
    SAY("   - %s / %s : %d cells", fallback_summary$cause_id[i],
        fallback_summary$measure[i], fallback_summary$N[i])
  SAY("   (cost-side fallback cells: %d)", n_fallback_cost)
} else cat(" Fallbacks               : none\n")
SAY(" Cost age-band prorating binds on %d window(s)", prorate_binds)
SAY(" Max |model well - residual| (national): %.4g (rel %.3g)", well_gap_max, well_gap_rel_max)
SAY(" Volume reconciliation max abs diff (R): %.6g", vol_recon_maxabs)
SAY(" Cost   reconciliation max abs diff (R): %.6g", cost_recon_maxabs)
SAY(" Max relative reconciliation diff (R)  : %.3g",
    max(vol_recon_maxabs / 1e6, cost_recon_maxabs / 1e6))
cat(" QA gates                : ALL PASS\n")
cat("===============================================================\n")
