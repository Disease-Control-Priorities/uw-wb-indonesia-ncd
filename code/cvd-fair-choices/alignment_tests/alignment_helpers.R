#===============================================================================
# alignment_helpers.R
#-------------------------------------------------------------------------------
# TESTABLE HELPER FUNCTIONS for the Indonesia epidemiology-alignment pipeline.
#
# Sourced by 03_calibration_indonesia_alignment.R (Phase 2 = build the alignment
# transition-probability input) and by the unit tests in
# alignment_tests/test_alignment_prep.R. Defining ONLY functions here (no
# top-level side effects) keeps the prep logic unit-testable in isolation.
#
# This file NEVER modifies a production artifact. It reads production
# adjusted_searo_part*.rds (as the pre-2023 history + missing-cause / missing-
# measure fallback) and the external projection RDS, and returns in-memory
# objects. Writing is done by the calling alignment scripts, only under
# data/processed/alignment/ and output/out_model_alignment/.
#
# Design decisions (verified against the real RDS + the production code):
#   * adjusted_searo schema (03 output) = 13 cols, in this order:
#       age, sex, location, year, cause, BG.mx.all, ALL.mx, BG.mx,
#       PREVt0, DIS.mx.t0, Nx, IR, CF
#     age/year = integer; sex/location/cause = character; the rest numeric;
#     `cause` = LONG GBD names (Model 05 maps long->short). Ages 0-95, 95=95+.
#   * Verified identities in production adjusted_searo:
#       BG.mx     == ALL.mx - DIS.mx.t0                      (cause-specific)
#       BG.mx.all == ALL.mx - sum_over_causes(DIS.mx.t0)     (cause-INVARIANT)
#       ALL.mx    cause-invariant (all-cause envelope)
#   * Projection provides internally-consistent single-age (0-100) counts:
#       cause_deaths    == population * cause_mx
#       prevalent_cases == population * prevalence_rate
#     We pool ages >= 95 into the model terminal age 95 (sum counts, re-derive
#     rates from pooled counts) so the 95+ stock reconciles exactly.
#   * cmd + dm2 are ABSENT from the projection -> retained from production
#     (per-cause fallback). HHD incidence is UNAVAILABLE -> IR retained from
#     production (per-measure fallback); HHD prevalence/CF/BG.mx come from the
#     projection.
#===============================================================================

if (!requireNamespace("data.table", quietly = TRUE))
  stop("alignment_helpers.R requires the 'data.table' package.")
suppressWarnings(suppressMessages(library(data.table)))

#-------------------------------------------------------------------------------
# 0. Bootstrap Model-00 configuration WITHOUT running Models 02-10.
#
# Model 00 sources everything 01..10 sequentially; Model 02 is expensive and
# depends on an out-of-repo path, and it only regenerates on-disk intermediates
# that already exist. We therefore evaluate Model 00's PREFIX up to (but not
# including) `source("02_load_inputs_indonesia.R")`. That runs the libraries,
# the path/flag config, the central cause_map / age grid + validation, and
# `source("01_utils_indonesia.R")` (pure function defs) -- exactly the common
# configuration Models 03/04/05/06 rely on -- and NOTHING heavy. Reusing Model
# 00's own text (rather than copying its config) guarantees no drift.
#
# The leading `rm(list=ls())` in Model 00 is stripped so the bootstrap never
# wipes alignment state a caller has already set.
#-------------------------------------------------------------------------------
align_bootstrap_config <- function(
    wd_root = "C:/Users/wrgar/OneDrive - UW/02Work/WorldBank-Indonesia/uw-wb-indonesia-ncd/",
    model00 = NULL) {

  if (exists("cause_map", envir = globalenv()) &&
      exists("wd_data",  envir = globalenv()) &&
      exists("gbd_band_label", envir = globalenv())) {
    message("align_bootstrap_config: config already present; skipping re-init.")
    return(invisible(TRUE))
  }

  wd_code <- file.path(wd_root, "code", "cvd-fair-choices")
  if (is.null(model00)) model00 <- file.path(wd_code, "00_run_model_cvd_fair.R")
  if (!file.exists(model00))
    stop("align_bootstrap_config: cannot find Model 00 at ", model00)

  lines <- readLines(model00, warn = FALSE)
  cut   <- grep('source\\("02_load_inputs_indonesia\\.R"\\)', lines)
  if (length(cut) == 0L)
    stop("align_bootstrap_config: could not locate the source(\"02_...\") line ",
         "in Model 00; bootstrap cut point is ambiguous -- aborting rather than ",
         "risk running Model 02.")
  prefix <- lines[seq_len(cut[1] - 1L)]
  # Drop the destructive workspace clear so the bootstrap is non-destructive.
  prefix <- prefix[!grepl("^\\s*rm\\(list\\s*=\\s*ls\\(\\)\\)", prefix)]

  # Model 00 uses relative source() calls after setwd(wd_code); make sure that
  # works regardless of the caller's cwd by evaluating in globalenv.
  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
  eval(parse(text = paste(prefix, collapse = "\n")), envir = globalenv())

  # sanity: the pieces downstream scripts need must now exist
  need <- c("cause_map", "model_cause_codes", "min_model_age", "max_model_age",
            "age_single", "wd", "wd_data", "wd_raw", "wd_outp", "wd_code",
            "gbd_band_label", "gbd_age_bands")
  miss <- need[!vapply(need, exists, logical(1), envir = globalenv())]
  if (length(miss))
    stop("align_bootstrap_config: bootstrap did not create: ",
         paste(miss, collapse = ", "))
  invisible(TRUE)
}

#-------------------------------------------------------------------------------
# 1. Required on-disk inputs (fail early with an informative error).
#-------------------------------------------------------------------------------
align_required_inputs <- function(wd_data) {
  c(projection      = file.path(wd_data, "indonesia_epidemiology_baseline_alignment.rds"),
    adjusted_searo1 = file.path(wd_data, "adjusted_searo_part1.rds"))
}

align_check_inputs <- function(wd_data, extra = character(0)) {
  req <- c(align_required_inputs(wd_data), extra)
  miss <- req[!file.exists(req)]
  if (length(miss))
    stop("Missing required alignment input(s):\n  ",
         paste(sprintf("%s -> %s", names(miss), miss), collapse = "\n  "),
         "\nAlignment cannot proceed without these. (Do NOT fabricate data.)")
  invisible(req)
}

#-------------------------------------------------------------------------------
# 2. Load the bound production adjusted_searo (explicit path list -- never a
#    broad "adjusted" glob that could also match alignment files).
#-------------------------------------------------------------------------------
align_load_production_adjusted <- function(wd_data) {
  f <- list.files(wd_data, pattern = "^adjusted_searo_part[0-9]+\\.rds$",
                  full.names = TRUE)
  if (!length(f))
    stop("No production adjusted_searo_part*.rds found in ", wd_data)
  dt <- rbindlist(lapply(f, function(x){ d <- readRDS(x); setDT(d); d }),
                  use.names = TRUE, fill = TRUE)
  dt[]
}

#-------------------------------------------------------------------------------
# 3. Cause crosswalk, derived from the central cause_map + the projection's own
#    gbd_cause_name column (NOT invented). Returns one row per projection cause.
#-------------------------------------------------------------------------------
align_build_crosswalk <- function(projection, cause_map) {
  setDT(projection)
  # short code <- long GBD name via the central cause_map
  long2short <- setNames(names(cause_map), unname(cause_map))
  xw <- unique(projection[, .(cause_id, gbd_cause_name)])
  xw[, cause_long  := gbd_cause_name]
  xw[, cause_short := long2short[gbd_cause_name]]
  if (anyNA(xw$cause_short))
    stop("align_build_crosswalk: projection cause(s) not in cause_map: ",
         paste(unique(xw[is.na(cause_short), gbd_cause_name]), collapse = ", "),
         ". Do not invent a mapping -- reconcile cause_map first.")
  setcolorder(xw, c("cause_id", "gbd_cause_name", "cause_long", "cause_short"))
  xw[]
}

#-------------------------------------------------------------------------------
# 4. Pool ages >= max_age into the open-ended terminal stock.
#    Counts are SUMMED; rates are RE-DERIVED from the pooled counts so the 95+
#    group reconciles exactly. Single ages < max_age are passed through
#    unchanged. Done independently per (year, sex, cause_id).
#-------------------------------------------------------------------------------
align_pool_terminal <- function(projection, max_age = 95L) {
  dt <- copy(projection); setDT(dt)
  below <- dt[age <  max_age]
  above <- dt[age >= max_age]
  if (nrow(above)) {
    pooled <- above[, .(
      age              = max_age,
      age_group        = "95+",
      age_mid          = 97,
      population       = sum(population),
      all_cause_deaths = sum(all_cause_deaths),
      cause_deaths     = sum(cause_deaths),
      incident_cases   = sum(incident_cases),          # NA-> stays NA if all NA
      prevalent_cases  = sum(prevalent_cases),
      incidence_available  = all(incidence_available),
      prevalence_available = all(prevalence_available)
    ), by = .(year, sex, cause_id, gbd_cause_name, cause_name)]
    # rates re-derived from pooled counts (population always > 0 here)
    pooled[, all_cause_mx    := all_cause_deaths / population]
    pooled[, cause_mx        := cause_deaths     / population]
    pooled[, prevalence_rate := prevalent_cases  / population]
    pooled[, incidence_rate  := incident_cases   / population]  # NA if inc NA
    out <- rbindlist(list(
      below[, .(year, age, sex, cause_id, gbd_cause_name, cause_name, age_group,
                age_mid, population, all_cause_mx, all_cause_deaths,
                incidence_rate, prevalence_rate, cause_mx, cause_deaths,
                incident_cases, prevalent_cases, incidence_available,
                prevalence_available)],
      pooled[, .(year, age, sex, cause_id, gbd_cause_name, cause_name, age_group,
                 age_mid, population, all_cause_mx, all_cause_deaths,
                 incidence_rate, prevalence_rate, cause_mx, cause_deaths,
                 incident_cases, prevalent_cases, incidence_available,
                 prevalence_available)]
    ), use.names = TRUE)
  } else {
    out <- below
  }
  setorder(out, cause_id, sex, year, age)
  out[]
}

#-------------------------------------------------------------------------------
# 4b. Fill missing PROJECTION years by linear interpolation between the nearest
#     present years (the projection is a smooth damped-logit trajectory). The
#     supplied RDS omits 2024 (jumps 2023 -> 2025); the cohort recursion needs a
#     gap-free 2023-2050 sequence, so interior gaps are filled here.
#     Interpolation is on the RATE scale + population; counts are RE-DERIVED from
#     the interpolated rates so internal consistency (count = pop*rate) holds.
#     An interior gap is interpolated (tagged interp_year=TRUE); a gap with no
#     bracketing year on one side is carried from the nearest present year.
#     Done independently per (age, sex, cause_id).
#-------------------------------------------------------------------------------
align_fill_missing_years <- function(proj_pooled, y_start = 2023L, y_end = 2050L) {
  dt <- copy(proj_pooled); setDT(dt)
  if (!("interp_year" %in% names(dt))) dt[, interp_year := FALSE]
  present <- sort(unique(dt$year))
  missing <- setdiff(y_start:y_end, present)
  if (!length(missing)) { setorder(dt, cause_id, sex, year, age); return(dt[]) }

  keycols   <- c("age", "sex", "cause_id", "gbd_cause_name", "cause_name")
  # fields interpolated on their own scale (rates + population + metadata)
  interp_cols <- intersect(c("age_mid", "population", "all_cause_mx",
                             "prevalence_rate", "incidence_rate", "cause_mx",
                             "cause_fraction", "cause_fraction_raw",
                             "selected_fraction_sum_raw",
                             "fraction_adjustment_factor"), names(dt))
  new_rows <- vector("list", length(missing))
  for (k in seq_along(missing)) {
    y  <- missing[k]
    bl <- present[present < y]; bu <- present[present > y]
    if (!length(bl) || !length(bu)) {                 # edge gap -> carry nearest
      src <- if (!length(bl)) min(bu) else max(bl)
      row <- copy(dt[year == src]); row[, `:=`(year = y, interp_year = TRUE)]
      new_rows[[k]] <- row; next
    }
    yl <- max(bl); yu <- min(bu); w <- (y - yl) / (yu - yl)
    lo <- copy(dt[year == yl]); hi <- copy(dt[year == yu])
    setorderv(lo, keycols); setorderv(hi, keycols)
    if (!identical(lo[, ..keycols], hi[, ..keycols]))
      stop("align_fill_missing_years: bracket years ", yl, "/", yu,
           " have mismatched (age,sex,cause) grids; cannot interpolate.")
    row <- copy(lo)
    for (rc in interp_cols)
      set(row, j = rc, value = (1 - w) * lo[[rc]] + w * hi[[rc]])
    row[, `:=`(year = y, interp_year = TRUE)]
    # re-derive counts from interpolated rates (NA incidence stays NA)
    row[, all_cause_deaths := population * all_cause_mx]
    row[, cause_deaths     := population * cause_mx]
    row[, prevalent_cases  := population * prevalence_rate]
    row[, incident_cases   := population * incidence_rate]
    new_rows[[k]] <- row
  }
  out <- rbindlist(c(list(dt), new_rows), use.names = TRUE, fill = TRUE)
  setorder(out, cause_id, sex, year, age)
  out[]
}

#-------------------------------------------------------------------------------
# 5. Projection-style coarse 5-year age-group label (matches the projection's
#    OWN age_group column: "0-4","5-9",...,"90-94","95+"). Used ONLY to define
#    the calibration target grain, so model output and projection targets
#    aggregate onto identical bins. (Deliberately NOT gbd_band_label(), whose
#    young bands are <1 / 12-23mo / 2-4 and would not match the projection.)
#-------------------------------------------------------------------------------
align_coarse_age_group <- function(age, max_age = 95L) {
  lo <- (age %/% 5L) * 5L
  lab <- paste0(lo, "-", lo + 4L)
  lab[age >= max_age] <- "95+"
  lab
}

align_make_age_match <- function(min_age = 0L, max_age = 95L) {
  data.table(age = min_age:max_age,
             age.group = align_coarse_age_group(min_age:max_age, max_age))
}

#-------------------------------------------------------------------------------
# 6. Probability / row constraints (COPIED from 03_calibration_indonesia_
#    nelder_mead.R:enforce_tp_constraints, generalised to optionally include
#    covid.mx in the headroom). Preference order:
#      (1) NA->0, clamp IR,CF,BG.mx,(covid) into [0,1];
#      (2) PRIMARY: preserve BG.mx(+covid), cap the disease TP so
#          IR + BG.mx + covid <= 1 and CF + BG.mx + covid <= 1;
#      (3) FALLBACK (only when the envelope alone leaves no room): proportional
#          renormalisation of the disease TP AND BG.mx (share-shrink), flag
#          bg_modified.
#    At the 03 stage covid.mx is absent (added in Model 05); the covid-inclusive
#    constraint is additionally enforced in alignment Model 05 (mirroring
#    production 05 lines 148-167). If a covid.mx column is present it is honoured
#    here too.
#-------------------------------------------------------------------------------
enforce_tp_constraints <- function(dt, tp_eps = 0.005) {
  has_covid <- "covid.mx" %in% names(dt)
  dt[is.na(IR),    IR := 0]
  dt[is.na(CF),    CF := 0]
  dt[is.na(BG.mx), BG.mx := 0]
  if (has_covid) dt[is.na(covid.mx), covid.mx := 0]
  dt[IR < 0, IR := 0]; dt[IR > 1, IR := 1]
  dt[CF < 0, CF := 0]; dt[CF > 1, CF := 1]
  dt[BG.mx < 0, BG.mx := 0]
  if (has_covid) { dt[covid.mx < 0, covid.mx := 0]; dt[covid.mx > 1, covid.mx := 1] }

  if (!("bg_modified" %in% names(dt))) dt[, bg_modified := 0L]

  cov <- if (has_covid) dt$covid.mx else 0
  dt[, headroom := 1 - BG.mx - cov - tp_eps]

  ## (2) PRIMARY: cap IR/CF into the headroom, leaving BG.mx untouched
  dt[headroom >= 0 & IR > headroom, IR := headroom]
  dt[headroom >= 0 & CF > headroom, CF := headroom]

  ## (3) FALLBACK: BG.mx(+covid) alone leaves no room -> proportional renorm.
  dt[headroom < 0 & (IR + BG.mx) > 1, `:=`(
        IR_new   = IR    / (IR + BG.mx) - tp_eps,
        BGmx_new = BG.mx / (IR + BG.mx) - tp_eps,
        bg_modified = 1L)]
  dt[!is.na(IR_new), `:=`(IR = pmax(IR_new, 0), BG.mx = pmax(BGmx_new, 0))]
  dt[, c("IR_new", "BGmx_new") := NULL]
  dt[(CF + BG.mx) > 1, `:=`(
        CF_new   = CF    / (CF + BG.mx) - tp_eps,
        BGmx_new = BG.mx / (CF + BG.mx) - tp_eps,
        bg_modified = 1L)]
  dt[!is.na(CF_new), `:=`(CF = pmax(CF_new, 0), BG.mx = pmax(BGmx_new, 0))]
  dt[, c("CF_new", "BGmx_new") := NULL]

  dt[, headroom := NULL]
  dt[]
}

#-------------------------------------------------------------------------------
# 7. Core Phase-2 builder: turn the pooled projection into the 13-col
#    adjusted_searo-schema pre-calibration TP table for the projection years
#    (2023-2050) and the projection causes (5), with a per-field provenance
#    audit and a constraint-adjustment log.
#
# Field derivation (see header + verified identities):
#   Nx        = population                                        (alignment)
#   ALL.mx    = all_cause_mx                                      (alignment)
#   PREVt0    = prevalence_rate                                   (alignment)
#   DIS.mx.t0 = cause_mx                                          (alignment)
#   BG.mx     = ALL.mx - DIS.mx.t0    (production identity)       (derived)
#   IR        = incidence_rate / (1 - prevalence_rate)  [well denom] (alignment)
#               HHD: incidence unavailable -> production IR (2019 carry) (fallback)
#   CF        = cause_deaths / prevalent_cases  (denom>0 else 0)  (alignment)
#   BG.mx.all = ALL.mx - sum_over_ALL7causes(DIS.mx.t0)           (derived)
#               = ALL.mx - [sum_5_projection cause_mx + prod cmd+dm2 mort (2019 carry)]
#
# `prod_adj` is the bound production adjusted_searo (long cause names, 2000-2019).
# Returns list(tps, provenance, adj_log).
#-------------------------------------------------------------------------------
align_derive_projection_tps <- function(proj_pooled, xwalk, prod_adj,
                                        cause_map, location = "Indonesia",
                                        max_age = 95L,
                                        y_start = 2023L, y_end = 2050L,
                                        tp_eps = 0.005) {
  setDT(proj_pooled); setDT(prod_adj)
  proj <- merge(proj_pooled, xwalk[, .(cause_id, cause_long = cause_long,
                                       cause_short = cause_short)],
                by = "cause_id", all.x = TRUE)
  proj <- proj[year >= y_start & year <= y_end & age <= max_age]

  ## ---- production 2019 carry-forward lookups (fallbacks) --------------------
  prod2019 <- prod_adj[year == 2019L, .(age, sex, cause, IR_prod = IR,
                                        DIS_prod = DIS.mx.t0)]
  # cmd + dm2 mortality (long names) at 2019, for the BG.mx.all envelope
  cmd_long <- unname(cause_map[["cmd"]]); dm2_long <- unname(cause_map[["dm2"]])
  extra_mort <- prod_adj[year == 2019L & cause %in% c(cmd_long, dm2_long),
                         .(extra_dis = sum(DIS.mx.t0)), by = .(age, sex)]

  ## ---- direct alignment fields ---------------------------------------------
  proj[, `:=`(
    location  = location,
    cause     = cause_long,
    Nx        = population,
    ALL.mx    = all_cause_mx,
    PREVt0    = prevalence_rate,
    DIS.mx.t0 = cause_mx
  )]
  proj[, BG.mx := ALL.mx - DIS.mx.t0]                        # production identity

  ## ---- CF = cause_deaths / prevalent_cases (denom>0) -----------------------
  proj[, CF := fifelse(prevalent_cases > 0, cause_deaths / prevalent_cases, 0)]
  proj[, cf_source := fifelse(prevalent_cases > 0, "alignment", "derived_zero_denom")]

  ## ---- IR: alignment where incidence available, else production fallback ---
  proj[, IR := fifelse(incidence_available & !is.na(incidence_rate) &
                         prevalence_rate < 1,
                       incidence_rate / (1 - prevalence_rate),
                       NA_real_)]
  proj[, ir_source := fifelse(!is.na(IR), "alignment", "production_fallback")]
  # fill production fallback IR (2019 carry) where alignment IR is NA (e.g. HHD)
  proj <- merge(proj, prod2019[, .(age, sex, cause, IR_prod)],
                by = c("age", "sex", "cause"), all.x = TRUE)
  proj[is.na(IR), IR := IR_prod]
  proj[is.na(IR), IR := 0]                                    # last-resort guard
  proj[, IR_prod := NULL]

  ## ---- BG.mx.all = ALL.mx - sum_7(DIS.mx.t0) -------------------------------
  ## cause-invariant per (year,age,sex): sum the 5 projection DIS.mx.t0 then add
  ## the production cmd+dm2 2019-carry mortality.
  sum5 <- proj[, .(sum5 = sum(DIS.mx.t0)), by = .(year, age, sex)]
  bg_all <- merge(sum5, extra_mort, by = c("age", "sex"), all.x = TRUE)
  bg_all[is.na(extra_dis), extra_dis := 0]
  bg_all <- merge(bg_all, unique(proj[, .(year, age, sex, ALL.mx)]),
                  by = c("year", "age", "sex"), all.x = TRUE)
  bg_all[, BG.mx.all := ALL.mx - sum5 - extra_dis]
  n_bgall_neg <- bg_all[BG.mx.all < 0, .N]
  bg_all[BG.mx.all < 0, BG.mx.all := 0]
  proj <- merge(proj, bg_all[, .(year, age, sex, BG.mx.all)],
                by = c("year", "age", "sex"), all.x = TRUE)

  ## ---- defensive clamps (as production 03 lines 242-245) + constraints -----
  pre <- proj[, .(IR_raw = IR, CF_raw = CF)]
  proj[CF >= 1, CF := 0.99]; proj[IR >= 1, IR := 0.99]
  proj[CF < 0, CF := 0];     proj[IR < 0, IR := 0]
  enforce_tp_constraints(proj, tp_eps = tp_eps)
  bg_modified_rows <- sum(proj$bg_modified)
  # log material constraint adjustments (never silently clip)
  adj_log <- data.table(
    metric = c("BG.mx.all_neg_clamped_to_0", "bg_modified_rows_renorm",
               "IR_clamped_ge1_to_0.99", "CF_clamped_ge1_to_0.99"),
    n = c(n_bgall_neg, bg_modified_rows,
          sum(pre$IR_raw >= 1, na.rm = TRUE),
          sum(pre$CF_raw >= 1, na.rm = TRUE)))

  ## ---- assemble the 13-col adjusted_searo schema (exact order/classes) -----
  proj[, age := as.integer(age)]
  proj[, year := as.integer(year)]
  tps <- proj[, .(age, sex, location, year, cause,
                  BG.mx.all, ALL.mx, BG.mx, PREVt0, DIS.mx.t0, Nx, IR, CF)]

  ## ---- provenance (per field, tagged) --------------------------------------
  ## Rows from an interpolated year (e.g. 2024) are tagged "interpolation" for
  ## the alignment/derived fields; a production_fallback (e.g. HHD IR) stays so.
  if (!("interp_year" %in% names(proj))) proj[, interp_year := FALSE]
  prov <- proj[, .(location, year, age, sex, cause,
     Nx_src        = fifelse(interp_year, "interpolation", "alignment"),
     ALL.mx_src    = fifelse(interp_year, "interpolation", "alignment"),
     PREVt0_src    = fifelse(interp_year, "interpolation", "alignment"),
     DIS.mx.t0_src = fifelse(interp_year, "interpolation", "alignment"),
     BG.mx_src     = fifelse(interp_year, "interpolation", "derived"),
     BG.mx.all_src = fifelse(interp_year, "interpolation", "derived"),
     IR_src = fifelse(interp_year & ir_source == "alignment", "interpolation", ir_source),
     CF_src = fifelse(interp_year & cf_source == "alignment", "interpolation", cf_source))]

  list(tps = tps[], provenance = prov[], adj_log = adj_log[])
}

#-------------------------------------------------------------------------------
# 8. Assemble the full pre-calibration adjusted_searo-schema table:
#      production 2000-2019 (ALL 7 causes, verbatim)  +  projection 2023-2050
#      (5 projection causes, derived).
#    Years 2020-2022 and cmd/dm2 for 2023-2050 are intentionally ABSENT here --
#    they are produced by alignment Model 05's carry-forward, exactly as
#    production Model 05 carries 2019 forward for every cause. Fails on any
#    duplicate key.
#-------------------------------------------------------------------------------
align_assemble_precalib <- function(prod_adj, proj_tps, y_hist_end = 2019L) {
  setDT(prod_adj); setDT(proj_tps)
  hist <- prod_adj[year <= y_hist_end]
  cols <- names(prod_adj)
  proj_tps <- proj_tps[, ..cols]                              # identical schema
  out <- rbindlist(list(hist, proj_tps), use.names = TRUE)
  key_cols <- c("age", "sex", "cause", "year", "location")
  dup <- out[, .N, by = key_cols][N > 1L]
  if (nrow(dup))
    stop("align_assemble_precalib: duplicate keys after bind (", nrow(dup),
         " combos). Examples:\n",
         paste(capture.output(print(head(dup, 5))), collapse = "\n"))
  setorderv(out, key_cols)
  out[]
}

#-------------------------------------------------------------------------------
# 9. FLOW-INVERSION (forward-solve) calibration for ONE (cause,sex) combo.
#
# Inverts the well-sick-dead cohort recursion to recover the IR(yr,s)/CF(yr,s)
# that reproduce the projection's prevalent_cases + cause_deaths under the SAME
# mechanics Model 06 / project_combo use. Key indexing (verified against
# 06_run_scenarios_indonesia_fair.R:2427-2467 and project_combo): the rate stored
# at b_rates(year=yr, age=s) governs the (yr-1, s) -> (yr, s+1) transition
#   sick(yr,s+1) = sick(yr-1,s)*(1-(CF(yr,s)+BG.mx(yr,s))) + well(yr-1,s)*IR(yr,s)
#   dead(yr,s+1) = sick(yr-1,s)*CF(yr,s)
#   pop(yr,s+1)  = pop(yr-1,s) - all.mx(yr-1,s)
#   all.mx(yr,s+1)= dead(yr,s+1) + pop(yr,s+1)*BG.mx.all(yr,s)      (covid=0 for 2023+)
#   well(yr,s+1) = pop(yr,s+1) - all.mx(yr,s+1) - sick(yr,s+1)
# Terminal age 95 pools source ages 94 & 95 (age2>95 -> 95); a single shared
# terminal rate reproduces the pooled 95+ stock. Age 0 is reseeded from the
# projection each year (newborn entry), exactly as the recursion does.
#
# For each target cell we SOLVE:
#   CF(yr,s) = dead_target(yr,s+1) / sick_actual(yr-1,s)
#   IR(yr,s) = [prev_target(yr,s+1) - sick_actual(yr-1,s)*(1-CF-BG.mx(yr,s))]
#              / well_actual(yr-1,s)
# then CLAMP CF,IR into [0, 1-BG.mx-tp_eps] (envelope-preserving, flagged when the
# raw solve was infeasible) and propagate the ACTUAL (post-clamp) state forward,
# so the baked rates reproduce the projection where flow-feasible and deviate
# minimally (never silently) where not.
#
# Seeds at y_start (2023) from the projection state; year y_start rates are NOT
# produced here (that transition is 2022->2023, outside the projection window) --
# the caller keeps the projection-naive IR/CF for year y_start.
#
# `cd` : one combo's projection rows with columns age(0:max_age),
#        year(y_start:y_end), Nx, PREVt0, DIS.mx.t0, BG.mx, BG.mx.all, ALL.mx.
# Returns list(rates, fit):
#   rates: data.table(age=source s, year in (y_start+1):y_end, IR, CF,
#          ir_infeasible, cf_infeasible)
#   fit  : data.table(age=target a, year, prev_target, prev_model,
#          death_target, death_model)   [year y_start included, reproduced exactly]
#-------------------------------------------------------------------------------
align_forward_solve <- function(cd, y_start = 2023L, y_end = 2050L,
                                max_age = 95L, tp_eps = 0.005) {
  setDT(cd)
  A <- 0:max_age; nA <- length(A); yrs <- y_start:y_end; nY <- length(yrs)
  Mk <- function(col) {
    m <- matrix(0, nA, nY, dimnames = list(A, as.character(yrs)))
    m[cbind(match(cd$age, A), match(cd$year, yrs))] <- cd[[col]]; m
  }
  Nx <- Mk("Nx"); PREVr <- Mk("PREVt0"); DISr <- Mk("DIS.mx.t0")
  BGm <- Mk("BG.mx"); BGa <- Mk("BG.mx.all"); ALLr <- Mk("ALL.mx")
  tS <- Nx * PREVr           # prevalent-case target by (age,year)
  tD <- Nx * DISr            # cause-death target
  # actual state + rate + flag matrices
  S <- W <- P <- AM <- matrix(0, nA, nY, dimnames = dimnames(Nx))
  IR <- CF <- matrix(NA_real_, nA, nY, dimnames = dimnames(Nx))
  irf <- cff <- matrix(FALSE, nA, nY, dimnames = dimnames(Nx))
  fit <- vector("list", nY)

  # seed year 1 (y_start): reproduce the projection state exactly
  S[, 1] <- tS[, 1]; P[, 1] <- Nx[, 1]; AM[, 1] <- Nx[, 1] * ALLr[, 1]
  W[, 1] <- pmax(Nx[, 1] - AM[, 1] - S[, 1], 0)
  fit[[1]] <- data.table(age = A, year = yrs[1],
                         prev_target = tS[, 1], prev_model = S[, 1],
                         death_target = tD[, 1], death_model = tD[, 1])

  clampfun <- function(x, bg) pmin(pmax(x, 0), pmax(1 - bg - tp_eps, 0))

  for (ti in 2:nY) {
    Sp <- S[, ti - 1]; Wp <- W[, ti - 1]; Pp <- P[, ti - 1]; AMp <- AM[, ti - 1]

    ## non-terminal: source s = 0..(max_age-2) -> target a = s+1 = 1..(max_age-1)
    s_ix <- 1:(nA - 2)          # source indices (ages 0..93)
    a_ix <- 2:(nA - 1)          # target indices (ages 1..94)
    S0 <- Sp[s_ix]; W0 <- Wp[s_ix]; P0 <- Pp[s_ix]; AM0 <- AMp[s_ix]
    bg <- BGm[s_ix, ti]; bga <- BGa[s_ix, ti]
    tS_a <- tS[a_ix, ti]; tD_a <- tD[a_ix, ti]
    cf_raw <- ifelse(S0 > 0, tD_a / S0, 0)
    ir_raw <- ifelse(W0 > 0, (tS_a - S0 * (1 - cf_raw - bg)) / W0, 0)
    cf_c <- clampfun(cf_raw, bg); ir_c <- clampfun(ir_raw, bg)
    CF[s_ix, ti] <- cf_c; IR[s_ix, ti] <- ir_c
    cff[s_ix, ti] <- !is.finite(cf_raw) | cf_raw < 0 | cf_raw > 1 - bg - tp_eps
    irf[s_ix, ti] <- !is.finite(ir_raw) | ir_raw < 0 | ir_raw > 1 - bg - tp_eps
    P1 <- pmax(P0 - AM0, 0)
    actS <- pmax(S0 * (1 - cf_c - bg) + W0 * ir_c, 0)
    actD <- S0 * cf_c
    AM1 <- pmax(actD + P1 * bga, 0)
    S[a_ix, ti] <- actS; P[a_ix, ti] <- P1; AM[a_ix, ti] <- AM1
    W[a_ix, ti] <- pmax(P1 - AM1 - actS, 0)

    ## terminal age max_age (index nA): sources = ages max_age-1 & max_age
    it1 <- nA - 1; it2 <- nA        # indices for source ages 94 & 95
    S0t <- Sp[it1] + Sp[it2]; W0t <- Wp[it1] + Wp[it2]
    P0t <- Pp[it1] + Pp[it2]; AM0t <- AMp[it1] + AMp[it2]
    bgt <- BGm[it2, ti]; bgat <- BGa[it2, ti]
    tS_t <- tS[nA, ti]; tD_t <- tD[nA, ti]
    cft_raw <- ifelse(S0t > 0, tD_t / S0t, 0)
    irt_raw <- ifelse(W0t > 0, (tS_t - S0t * (1 - cft_raw - bgt)) / W0t, 0)
    cft <- clampfun(cft_raw, bgt); irt <- clampfun(irt_raw, bgt)
    # shared terminal rate stored at BOTH source ages 94 & 95
    CF[c(it1, it2), ti] <- cft; IR[c(it1, it2), ti] <- irt
    cff[c(it1, it2), ti] <- !is.finite(cft_raw) | cft_raw < 0 | cft_raw > 1 - bgt - tp_eps
    irf[c(it1, it2), ti] <- !is.finite(irt_raw) | irt_raw < 0 | irt_raw > 1 - bgt - tp_eps
    P1t <- pmax(P0t - AM0t, 0)
    actSt <- pmax(S0t * (1 - cft - bgt) + W0t * irt, 0)
    actDt <- S0t * cft
    AM1t <- pmax(actDt + P1t * bgat, 0)
    S[nA, ti] <- actSt; P[nA, ti] <- P1t; AM[nA, ti] <- AM1t
    W[nA, ti] <- pmax(P1t - AM1t - actSt, 0)

    ## age 0 (index 1): newborn reseed from the projection
    S[1, ti] <- tS[1, ti]; P[1, ti] <- Nx[1, ti]; AM[1, ti] <- Nx[1, ti] * ALLr[1, ti]
    W[1, ti] <- pmax(Nx[1, ti] - AM[1, ti] - S[1, ti], 0)

    ## achieved fit at TARGET ages this year
    dmod <- numeric(nA)
    dmod[a_ix] <- actD; dmod[nA] <- actDt; dmod[1] <- tD[1, ti]  # age0 death = seed
    fit[[ti]] <- data.table(age = A, year = yrs[ti],
                            prev_target = tS[, ti], prev_model = S[, ti],
                            death_target = tD[, ti], death_model = dmod)
  }

  rates <- data.table(
    age = rep(A, nY), year = rep(yrs, each = nA),
    IR = as.vector(IR), CF = as.vector(CF),
    ir_infeasible = as.vector(irf), cf_infeasible = as.vector(cff)
  )[year > y_start]           # year y_start rates kept naive by the caller
  list(rates = rates[], fit = rbindlist(fit))
}
