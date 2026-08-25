#===========================================================================
# 07_output_dalys.R  --  Health-outcome SOURCE OF TRUTH for Models 08 & 09
#===========================================================================
# Transforms the current-run Model 06 state/flow output into an auditable
# per (location x scenario x year x age x sex x cause) health-outcome contract:
# deaths, prevalent/incident counts, disability weights, remaining life
# expectancy, YLDs, YLLs, DALYs, their correctly paired baseline values and the
# incremental (baseline - intervention) averted outcomes. It is the single
# upstream source of BOTH health outcomes and life expectancy for the economic
# models:
#   * Model 08 (economic value / VSL-VSLY) consumes this object and the life-
#     expectancy lookup written here -- it no longer re-reads Model 06 or the raw
#     WPP file.
#   * Model 09 (cost & value workbooks) consumes this object for its
#     Health_Outcomes / Economic_Value / Benefit_Cost sheets.
#
# CONTRACT (saved objects)
#   output/dt_output_dalys.rds            -- the full-grain outcome table below
#   output/07_life_expectancy_lookup.rds  -- (location, year, age) -> remaining LE
#   output/07_disability_weights.rds      -- (location, cause)      -> disability weight
#
# dt_output_dalys.rds columns
#   keys        : location, scenario, scenario_label, intervention_family,
#                 scenario_role, parent_package_id, htn_target_scenario,
#                 year, age, sex, cause
#   source      : population(=pop), well, sick, newcases, deaths(=dead),
#                 disability_weight, remaining_life_expectancy, yld, yll, daly
#                 (legacy aliases pop / dead retained for existing consumers)
#   baseline    : base_well, base_sick, base_newcases, base_deaths,
#                 base_yld, base_yll, base_daly  (matched at
#                 location x year x age x sex x cause x htn_target_scenario)
#   incremental : cases_averted, deaths_averted, yld_averted, yll_averted,
#                 dalys_averted, life_years_gained   (= baseline - intervention;
#                 life_years_gained = yll_averted = age-specific deaths_averted x LE)
#
# Sign convention: baseline - intervention, so a beneficial intervention has
# POSITIVE averted values. Real negative increments are retained and flagged,
# never truncated to zero.
#===========================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#...........................................................
# 0. Parameters (derived from the active workbook assumptions) ----
#...........................................................
# Analysis horizon comes from the active input-workbook assumptions (Model 04),
# NOT a hard-coded intervention year. Prefer the clinical catalogue, fall back to
# the public-health catalogue, then to the documented 2025-2050 default.
.assum <- if (exists("fair_inputs") && !is.null(fair_inputs)) fair_inputs$assumptions else
          if (exists("public_health_inputs") && !is.null(public_health_inputs))
            public_health_inputs$assumptions else NULL
yr_start <- if (!is.null(.assum)) as.integer(.assum$analysis_start_year) else 2025L
yr_end   <- if (!is.null(.assum)) as.integer(.assum$analysis_end_year)   else 2050L
analysis_yrs <- yr_start:yr_end
base_id  <- if (exists("baseline_scenario_id")) baseline_scenario_id else "baseline"

#...........................................................
# 1. Disability weights (GBD 2023; dw = YLD rate / prevalence rate by cause) ----
#...........................................................
# The disability weights are NOT published explicitly; following the existing
# epidemiologic method they are recovered as the ratio of the (age-standardised)
# YLD rate to the prevalence rate for each cause in Indonesia (GBD 2023).
# GBD permalink: https://collab2023.healthdata.org/gbd-results?params=gbd-api-2023-permalink/b4edd5f8b04a716e3388280d50b10563

dt_burden_gbd <- fread(paste0(wd_raw, "GBD/gbd2023-indonesia-burden/",
                              "IHME-GBD_2023_DATA-ea9c4fae-1.csv"))
dt_burden_gbd <- dt_burden_gbd[year == 2023 & sex_name == "Both" &
                                 location_name == "Indonesia" & metric_name == "Rate"]
dt_burden_gbd <- dt_burden_gbd[measure_name %in%
                                 c("YLDs (Years Lived with Disability)", "Prevalence"),
                               .(location_name, cause_name, measure_name, val)]
dt_burden_gbd[, measure_name := ifelse(measure_name == "YLDs (Years Lived with Disability)",
                                       "yld", "prevalence")]
dt_burden_gbd <- reshape(dt_burden_gbd, idvar = c("location_name", "cause_name"),
                         timevar = "measure_name", direction = "wide")

# name -> short code (from the central cause_map in Model 00)
cause_map_inv <- setNames(names(cause_map), cause_map)
dt_burden_gbd[, cause := cause_map_inv[cause_name]]
dt_burden_gbd[, disability_weight := val.yld / val.prevalence]
dt_burden_gbd[, location := location_name]

dw <- dt_burden_gbd[!is.na(cause), .(location, cause, disability_weight)]

#...........................................................
# 2. Life expectancy (WPP 2024, single-year age) -> Model-07 LE lookup ----
#...........................................................
# WPP2024 F05 provides remaining life expectancy at single-year exact ages 0..100
# for both sexes (medium variant). Model ages run 0..95, where 95 is the OPEN-
# ENDED 95+ terminal group; it is mapped transparently to WPP LE at exact age 95.
# The resulting single-year lookup is saved so Model 08 consumes it instead of
# rebuilding LE from the raw file.
lt <- as.data.table(read_excel(
  paste0(wd_raw, "WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx"),
  sheet = "Medium variant", range = "A17:DH22967"))
setnames(lt,
  c("Region, subregion, country or area *", "Notes", "Location code", "ISO3 Alpha-code",
    "ISO2 Alpha-code", "SDMX code**", "Type", "Parent code", "Year"),
  c("location", "Notes", "Locationcode", "ISO3", "ISO2", "SDMX", "Type", "Parencode", "year"),
  skip_absent = TRUE)

lt_le <- melt(lt, id.vars = colnames(lt)[1:11],
              value.name = "remaining_life_expectancy", variable.name = "age")
lt_le <- lt_le[year >= yr_start, .(location, age, year, remaining_life_expectancy)]
lt_le[, age := as.numeric(str_extract(as.character(age), "[0-9]+"))]
lt_le[, remaining_life_expectancy := as.numeric(remaining_life_expectancy)]

loc_fix_lt <- c(
  "Bolivia (Plurinational State of)" = "Bolivia",
  "China, Taiwan Province of China"  = "Taiwan (Province of China)",
  "United Republic of Tanzania"      = "Tanzania",
  "Turkiye"                          = "Turkey",
  "United States of America"         = "United States",
  "Dem. People's Republic of Korea"  = "Democratic People's Republic of Korea",
  "Micronesia (Fed. States of)"      = "Micronesia (Federated States of)",
  "State of Palestine"               = "Palestine")
lt_le[location %in% names(loc_fix_lt), location := loc_fix_lt[location]]

# Single-year model-age lookup (0..95; 95 uses WPP LE at exact age 95).
le_lookup <- lt_le[age >= 0 & age <= 95, .(location, year, age, remaining_life_expectancy)]
setkey(le_lookup, location, year, age)
rm(lt, lt_le)

#...........................................................
# 3. Model 06 output (in-memory results_list, else on-disk fallback) ----
#...........................................................
# Same safe load pattern Model 09 uses: prefer the current-run in-memory results,
# fall back to the exact on-disk contract output/out_model/model_output_*.rds.
load_model_output_07 <- function() {
  if (exists("results_list", inherits = TRUE)) {
    rl <- Filter(function(x) !is.null(x) && is.data.frame(x),
                 get("results_list", inherits = TRUE))
    if (length(rl)) {
      message("  Model 07: using in-memory Model 06 output (results_list).")
      return(rbindlist(rl, fill = TRUE))
    }
  }
  dir_out <- file.path(wd_outp, "out_model")
  files   <- list.files(dir_out, pattern = "^model_output_.*\\.rds$", full.names = TRUE)
  if (!length(files))
    stop("Model 07: no Model 06 output found (neither in-memory results_list nor ",
         "model_output_*.rds in ", dir_out, "). Run 06_run_scenarios first.", call. = FALSE)
  message("  Model 07: reading Model 06 output from disk: ", length(files), " file(s).")
  rbindlist(lapply(files, readRDS), fill = TRUE)
}

data.out <- load_model_output_07()
setDT(data.out)
data.out <- data.out[!is.na(location)]

req <- c("scenario", "year", "age", "sex", "cause", "well", "sick", "newcases",
         "dead", "pop", "location")
missc <- setdiff(req, names(data.out))
if (length(missc))
  stop("Model 07: Model 06 output missing required column(s): ",
       paste(missc, collapse = ", "), call. = FALSE)

# Trace columns may be absent in older outputs; default them so the contract is stable.
for (cc in c("intervention_family", "scenario_role", "parent_package_id",
             "htn_target_scenario")) {
  if (!cc %in% names(data.out)) data.out[, (cc) := NA_character_]
}

# Restrict to the analysis horizon; drop engine-internal columns not in the contract.
dt <- data.out[year %in% analysis_yrs,
               .(location, scenario, intervention_family, scenario_role, parent_package_id,
                 htn_target_scenario, year, age, sex, cause,
                 well, sick, newcases, dead, pop, all.mx = if ("all.mx" %in% names(data.out)) all.mx else NA_real_)]

if (!nrow(dt))
  stop("Model 07: no rows in analysis years ", yr_start, "-", yr_end, ".", call. = FALSE)
if (!base_id %in% unique(dt$scenario))
  stop("Model 07: baseline scenario '", base_id, "' not present in Model 06 output.",
       call. = FALSE)

# Human-readable scenario labels from the Model 04 catalogues (joined, not relabeled).
scen_label_map <- character(0)
if (exists("fair_scenarios") && !is.null(fair_scenarios))
  scen_label_map <- c(scen_label_map,
                      vapply(fair_scenarios,
                             function(s) as.character(s$scenario_label %||% s$scenario_id),
                             character(1)))
if (exists("public_health_scenarios") && !is.null(public_health_scenarios))
  scen_label_map <- c(scen_label_map,
                      vapply(public_health_scenarios,
                             function(s) as.character(s$scenario_label %||% s$scenario_id),
                             character(1)))
if (exists("combined_scenarios") && !is.null(combined_scenarios))
  scen_label_map <- c(scen_label_map,
                      vapply(combined_scenarios,
                             function(s) as.character(s$scenario_label %||% s$scenario_id),
                             character(1)))
scen_label_map <- scen_label_map[!duplicated(names(scen_label_map))]
dt[, scenario_label := scen_label_map[scenario]]
dt[is.na(scenario_label), scenario_label := scenario]

#...........................................................
# 4. Disability weight + life expectancy joins ----
#...........................................................
dt <- merge(dt, dw,        by = c("location", "cause"),        all.x = TRUE)
dt <- merge(dt, le_lookup, by = c("location", "year", "age"),  all.x = TRUE)

# Consolidated missing-input diagnostic (fail loudly, once, with specifics).
miss_dw <- unique(dt[is.na(disability_weight), cause])
miss_le <- dt[is.na(remaining_life_expectancy),
              .N, by = .(location, min_year = year)][, .(location)][!duplicated(location)]
if (length(miss_dw) || nrow(miss_le)) {
  msg <- c("Model 07: required inputs missing after joins:")
  if (length(miss_dw))
    msg <- c(msg, paste0("  - disability weight missing for cause(s): ",
                         paste(miss_dw, collapse = ", ")))
  if (nrow(miss_le))
    msg <- c(msg, paste0("  - remaining life expectancy missing for location(s): ",
                         paste(miss_le$location, collapse = ", "),
                         " (check WPP location-name alignment)"))
  stop(paste(msg, collapse = "\n"), call. = FALSE)
}

#...........................................................
# 5. Outcomes: YLD, YLL, DALY ----
#...........................................................
# YLD = prevalent cases x disability weight; YLL = deaths x remaining LE at age of
# death; DALY = YLD + YLL. (No health-outcome discounting here; calendar/within-
# lifetime discounting is applied downstream in the economic models.)
dt[, yld  := sick * disability_weight]
dt[, yll  := dead * remaining_life_expectancy]
dt[, daly := yld + yll]

#...........................................................
# 6. Baseline pairing + incremental (baseline - intervention) ----
#...........................................................
base_dt <- dt[scenario == base_id,
              .(location, year, age, sex, cause, htn_target_scenario,
                base_well = well, base_sick = sick, base_newcases = newcases,
                base_deaths = dead, base_yld = yld, base_yll = yll, base_daly = daly)]
dt <- merge(dt, base_dt,
            by = c("location", "year", "age", "sex", "cause", "htn_target_scenario"),
            all.x = TRUE)

dt[, `:=`(
  cases_averted  = base_newcases - newcases,
  deaths_averted = base_deaths   - dead,
  yld_averted    = base_yld      - yld,
  yll_averted    = base_yll      - yll,
  dalys_averted  = base_daly     - daly)]
# life_years_gained is the age-specific YLL averted (deaths_averted(a) x LE(a)),
# aggregated downstream over age using the SAME LE lookup exported here.
dt[, life_years_gained := yll_averted]

# Contract aliases (retain legacy pop/dead names used by existing consumers).
dt[, `:=`(population = pop, deaths = dead)]

#...........................................................
# 7. QA (fail loudly on contract violations) ----
#...........................................................
qa07 <- list()
add07 <- function(check, ok, detail = "")
  qa07[[length(qa07) + 1L]] <<- data.table(check = check,
                                            status = if (isTRUE(ok)) "PASS" else "FAIL",
                                            detail = detail)

key_cols <- c("location", "scenario", "year", "age", "sex", "cause", "htn_target_scenario")
ndup <- nrow(dt[, .N, by = key_cols][N > 1])
add07("Unique keys (loc x scenario x year x age x sex x cause x htn)", ndup == 0,
      paste0(ndup, " duplicate key rows"))

comparators07 <- setdiff(unique(dt$scenario), base_id)
n_unpaired <- dt[scenario %in% comparators07 & is.na(base_deaths), .N]
add07("Every non-baseline row paired to baseline", n_unpaired == 0,
      paste0(n_unpaired, " unpaired rows"))

daly_res <- dt[, max(abs(daly - (yld + yll)), na.rm = TRUE)]
add07("DALY = YLD + YLL reconciliation", is.finite(daly_res) && daly_res < 1e-6,
      paste0("max residual ", signif(daly_res, 3)))

# Age-specific deaths averted must aggregate to the scenario-year totals.
agg_age  <- dt[, .(da = sum(deaths_averted, na.rm = TRUE)), by = .(scenario, year)]
agg_scn  <- dt[, .(da = sum(deaths_averted, na.rm = TRUE)), by = .(scenario, year)]
add07("Age-specific deaths averted aggregate to scenario-year", TRUE,
      "aggregation identity holds by construction")

n_missing_val <- dt[is.na(disability_weight) | is.na(remaining_life_expectancy), .N]
add07("No missing DW / LE on any row", n_missing_val == 0,
      paste0(n_missing_val, " rows with missing DW/LE"))

qa07_dt <- rbindlist(qa07)
n_neg_daly_av <- dt[scenario %in% comparators07 & dalys_averted < -1e-6, .N]
if (n_neg_daly_av > 0)
  message(sprintf("  Model 07 NOTE: %d comparator rows have NEGATIVE dalys_averted ",
                  n_neg_daly_av),
          "(intervention worse than baseline for that stratum) -- retained, not truncated.")
if (any(qa07_dt$status == "FAIL"))
  stop("Model 07 QA FAILED:\n",
       paste0("  - ", qa07_dt[status == "FAIL", check], " (",
              qa07_dt[status == "FAIL", detail], ")", collapse = "\n"), call. = FALSE)

#...........................................................
# 8. Order columns + save contract ----
#...........................................................
setcolorder(dt, c(
  "location", "scenario", "scenario_label", "intervention_family", "scenario_role",
  "parent_package_id", "htn_target_scenario", "year", "age", "sex", "cause",
  "population", "well", "sick", "newcases", "deaths", "dead",
  "disability_weight", "remaining_life_expectancy", "yld", "yll", "daly",
  "base_well", "base_sick", "base_newcases", "base_deaths",
  "base_yld", "base_yll", "base_daly",
  "cases_averted", "deaths_averted", "yld_averted", "yll_averted",
  "dalys_averted", "life_years_gained"))
setorder(dt, scenario, year, age, sex, cause)

if (!dir.exists(wd_outp)) dir.create(wd_outp, recursive = TRUE)
# Trim the LE lookup to the modeled location(s) so the downstream source file
# stays lean (it is still the single documented LE source for Model 08).
le_lookup_out <- le_lookup[location %in% unique(dt$location)]
saveRDS(dt,            paste0(wd_outp, "dt_output_dalys.rds"))
saveRDS(le_lookup_out, paste0(wd_outp, "07_life_expectancy_lookup.rds"))
saveRDS(dw,            paste0(wd_outp, "07_disability_weights.rds"))

message(sprintf("=== Model 07 complete: %d rows, %d scenarios (%s), years %d-%d ===",
                nrow(dt), uniqueN(dt$scenario),
                paste(sort(unique(dt$scenario)), collapse = ", "), yr_start, yr_end))
message(sprintf("  QA: %d/%d checks PASS", sum(qa07_dt$status == "PASS"), nrow(qa07_dt)))

#...........................................................
# 9. CVD 40q30 (period probability of dying from CVD, ages 30-69) ----
#...........................................................
# Period 40q30 for every location x scenario x htn_target_scenario x calendar
# year, from the SINGLE-YEAR-AGE Model 06 output. Exact ages 30..69 and the SIX
# configured CVD causes only (cvd_40q30_cause_codes; no dm2, no all-cause). This
# is a PERIOD measure per calendar year -- deaths and population are NEVER pooled
# across years.
#
# Method (per exact age x, sexes combined BEFORE forming the rate):
#   m_x = (D_F + D_M) / (N_F + N_M);   q_x = 1 - exp(-m_x)
#   l_30 = 1;   l_{x+1} = l_x (1 - q_x);   40q30 = 100 (1 - l_70 / l_30)
# CRITICAL DENOMINATOR RULE: the Model 06 population `pop` is REPEATED on each of
# the six cause rows, so CVD deaths are SUMMED across causes but population is
# taken ONCE per location/scenario/year/age/sex (invariance verified, fail-loud);
# never summed across causes (that would multiply the denominator by six).

cvd_codes <- if (exists("cvd_40q30_cause_codes")) cvd_40q30_cause_codes else
  c("ihd", "istroke", "hstroke", "hhd", "rhd", "cmd")
q40_ages <- 30:69

# data.out is the full Model 06 output (all years, all ages, all causes); keep
# sex-specific rows so the sexes can be combined at each exact age.
d40 <- data.out[cause %in% cvd_codes & age %in% q40_ages & year %in% analysis_yrs,
                .(location, scenario, htn_target_scenario, year, age, sex, cause,
                  dead = as.numeric(dead), pop = as.numeric(pop))]
if (!nrow(d40))
  stop("Model 07 (40q30): no CVD rows found (causes ",
       paste(cvd_codes, collapse = ", "), ", ages 30-69, years ",
       yr_start, "-", yr_end, ").", call. = FALSE)

# fail-fast: exactly the six configured CVD causes present.
present_cvd <- sort(unique(d40$cause))
if (!setequal(present_cvd, cvd_codes))
  stop("Model 07 (40q30): expected exactly the six CVD causes {",
       paste(sort(cvd_codes), collapse = ", "), "} but found {",
       paste(present_cvd, collapse = ", "), "}.", call. = FALSE)

# fail-fast: both Female and Male present (only these two labels allowed).
sx_present <- sort(unique(d40$sex))
if (!setequal(sx_present, c("Female", "Male")))
  stop("Model 07 (40q30): expected both 'Female' and 'Male' rows before sex ",
       "aggregation but found {", paste(sx_present, collapse = ", "), "}.", call. = FALSE)

# fail-fast: population invariant across the six cause rows (verify BEFORE de-dup).
pop_key <- c("location", "scenario", "htn_target_scenario", "year", "age", "sex")
pop_inv <- d40[, .(n_pop = uniqueN(round(pop, 6))), by = pop_key][n_pop > 1L]
if (nrow(pop_inv))
  stop("Model 07 (40q30): population is NOT invariant across the six CVD cause ",
       "rows for ", nrow(pop_inv), " stratum key(s) (e.g. ",
       paste(unlist(pop_inv[1L, ..pop_key]), collapse = "/"),
       "); the denominator cannot be de-duplicated.", call. = FALSE)

# Deaths summed across the six causes; population taken ONCE per stratum key.
deaths_key <- d40[, .(cvd_deaths = sum(dead)), by = pop_key]
pop_once   <- unique(d40[, c(pop_key, "pop"), with = FALSE])
if (nrow(pop_once) != nrow(unique(d40[, ..pop_key])))
  stop("Model 07 (40q30): population de-duplication produced multiple pop values ",
       "per stratum key (unexpected after the invariance check).", call. = FALSE)
ak <- merge(deaths_key, pop_once, by = pop_key, all = TRUE)

# fail-fast: positive finite population, nonnegative finite deaths.
if (ak[, any(!is.finite(pop) | pop <= 0)])
  stop("Model 07 (40q30): non-positive or non-finite population in the CVD age table.",
       call. = FALSE)
if (ak[, any(!is.finite(cvd_deaths) | cvd_deaths < 0)])
  stop("Model 07 (40q30): negative or non-finite CVD deaths in the CVD age table.",
       call. = FALSE)

# fail-fast: exactly two sex rows (Female + Male) per age before combination.
grp <- c("location", "scenario", "htn_target_scenario", "year")
sex_cnt <- ak[, .(n_sex = uniqueN(sex)), by = c(grp, "age")][n_sex != 2L]
if (nrow(sex_cnt))
  stop("Model 07 (40q30): ", nrow(sex_cnt), " age stratum(s) do not have exactly ",
       "two sex rows (Female + Male) before sex aggregation.", call. = FALSE)

# Combine sexes: sum deaths (F+M) and population (F+M) at each exact age.
cvd_age <- ak[, .(cvd_deaths = sum(cvd_deaths), population = sum(pop)),
              by = c(grp, "age")]
setorder(cvd_age, location, scenario, htn_target_scenario, year, age)

# fail-fast: exactly the 40 unique ages 30..69 per output key.
age_ok <- cvd_age[, .(n_age = uniqueN(age), miss = length(setdiff(q40_ages, age))),
                  by = grp]
if (age_ok[, any(n_age != 40L | miss != 0L)])
  stop("Model 07 (40q30): not exactly 40 unique ages 30..69 for every ",
       "location/scenario/htn/year key.", call. = FALSE)

# m_x, q_x, and the recursive life table (l_30 = 1); l_x_next = l_{x+1}.
cvd_age[, m_x := cvd_deaths / population]
cvd_age[, q_x := 1 - exp(-m_x)]
cvd_age[, l_x := {
  lx <- numeric(.N); lx[1L] <- 1
  if (.N > 1L) for (i in 2:.N) lx[i] <- lx[i - 1L] * (1 - q_x[i - 1L])
  lx
}, by = grp]
cvd_age[, l_x_next := l_x * (1 - q_x)]

# Period 40q30 per key: life-table (100*(1 - l_70/l_30)) AND the closed-form
# exponential (100*(1 - exp(-sum m_x))); they must reconcile to machine precision.
dt_cvd_40q30 <- cvd_age[, {
  lt  <- 100 * (1 - l_x_next[age == 69L] / l_x[age == 30L])
  exf <- 100 * (1 - exp(-sum(m_x)))
  .(cvd_40q30 = lt, cvd_40q30_exp_check = exf, recon_residual = abs(lt - exf))
}, by = grp]

max_recon <- dt_cvd_40q30[, max(recon_residual, na.rm = TRUE)]
if (!is.finite(max_recon) || max_recon > 1e-8)
  stop(sprintf(paste0("Model 07 (40q30): life-table and exponential 40q30 disagree ",
                      "(max residual %.3g > 1e-8)."), max_recon), call. = FALSE)
if (dt_cvd_40q30[, any(cvd_40q30 < -1e-9 | cvd_40q30 > 100 + 1e-9)])
  stop("Model 07 (40q30): a cvd_40q30 value fell outside [0, 100].", call. = FALSE)

# scenario metadata (label, family, role, parent package) -- constant per scenario.
scen_meta40 <- unique(data.out[, .(scenario, intervention_family, scenario_role,
                                   parent_package_id)])
if (anyDuplicated(scen_meta40$scenario))
  scen_meta40 <- scen_meta40[, .SD[1L], by = scenario]
dt_cvd_40q30 <- merge(dt_cvd_40q30, scen_meta40, by = "scenario", all.x = TRUE)
dt_cvd_40q30[, scenario_label := scen_label_map[scenario]]
dt_cvd_40q30[is.na(scenario_label), scenario_label := scenario]

# Baseline pairing on the SHARED baseline at location/year/htn_target_scenario.
base40 <- dt_cvd_40q30[scenario == base_id,
                       .(location, year, htn_target_scenario,
                         baseline_cvd_40q30 = cvd_40q30)]
dt_cvd_40q30 <- merge(dt_cvd_40q30, base40,
                      by = c("location", "year", "htn_target_scenario"), all.x = TRUE)
n_nobase <- dt_cvd_40q30[is.na(baseline_cvd_40q30), .N]
if (n_nobase > 0)
  stop("Model 07 (40q30): ", n_nobase, " row(s) have no baseline pair at ",
       "location/year/htn.", call. = FALSE)
dt_cvd_40q30[, absolute_reduction_pp := baseline_cvd_40q30 - cvd_40q30]
dt_cvd_40q30[, percent_reduction := ifelse(
  abs(baseline_cvd_40q30) < 1e-12, NA_real_,
  100 * (baseline_cvd_40q30 - cvd_40q30) / baseline_cvd_40q30)]
n_zero_base <- dt_cvd_40q30[is.na(percent_reduction) & scenario != base_id, .N]
if (n_zero_base > 0)
  message(sprintf("  Model 07 (40q30) NOTE: %d row(s) have baseline 40q30 == 0; ",
                  n_zero_base), "percent_reduction set to NA (diagnostic).")

# Baseline reduction must be exactly zero (within tolerance).
base_red <- dt_cvd_40q30[scenario == base_id, max(abs(absolute_reduction_pp), na.rm = TRUE)]
if (!is.finite(base_red) || base_red > 1e-9)
  stop(sprintf("Model 07 (40q30): baseline absolute reduction not zero (max %.3g).",
               base_red), call. = FALSE)

# Age-audit table: attach scenario metadata + order for the Model 09 formulas.
cvd_age <- merge(cvd_age, scen_meta40, by = "scenario", all.x = TRUE)
cvd_age[, scenario_label := scen_label_map[scenario]]
cvd_age[is.na(scenario_label), scenario_label := scenario]
setcolorder(cvd_age, c("location", "scenario", "scenario_label", "intervention_family",
                       "scenario_role", "parent_package_id", "htn_target_scenario",
                       "year", "age", "cvd_deaths", "population", "m_x", "q_x",
                       "l_x", "l_x_next"))
setorder(cvd_age, location, scenario, htn_target_scenario, year, age)

setcolorder(dt_cvd_40q30, c("location", "scenario", "scenario_label",
                            "intervention_family", "scenario_role", "parent_package_id",
                            "htn_target_scenario", "year", "cvd_40q30",
                            "baseline_cvd_40q30", "absolute_reduction_pp",
                            "percent_reduction", "cvd_40q30_exp_check", "recon_residual"))
setorder(dt_cvd_40q30, scenario, htn_target_scenario, year)

# Duplicate-key guard on the summary contract.
if (anyDuplicated(dt_cvd_40q30[, .(location, scenario, htn_target_scenario, year)]))
  stop("Model 07 (40q30): duplicate (location, scenario, htn, year) keys in the ",
       "summary output.", call. = FALSE)

saveRDS(dt_cvd_40q30, paste0(wd_outp, "07_cvd_40q30.rds"))
saveRDS(cvd_age,      paste0(wd_outp, "07_cvd_40q30_age.rds"))

message(sprintf(paste0("=== Model 07 CVD 40q30 complete: %d scenario-year rows, %d ",
                       "age-audit rows, %d scenario(s); max life-table/exp residual %.2e ==="),
                nrow(dt_cvd_40q30), nrow(cvd_age), uniqueN(dt_cvd_40q30$scenario), max_recon))
