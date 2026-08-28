#===========================================================================
# 08_economic_value_calculation.R
#   VSL / VSLY monetary value of deaths averted, on the 2019 Reference Case
#   Guidelines for Benefit-Cost Analysis in Global Health and Development
#   (Robinson LA, Hammitt JK, Cecchini M, et al. 2019; CHDS/Harvard).
#===========================================================================
# PIPELINE CONTRACT (this file is now a pure consumer of Model 07)
#   Reads:
#     * output/dt_output_dalys.rds           (Model 07 age-level health outcomes)
#     * output/07_life_expectancy_lookup.rds (Model 07 single-year LE by loc/year/age)
#     * data/raw/API_NY.GNP.PCAP.PP.KD_*.csv (World Bank GNI per capita, PPP)
#     * data/raw/..._ssp_basic_drivers_release_3.1_full.xlsx (IIASA SSP 3.1 growth)
#     * data/processed/PopulationsSingleAge0050.rds (UN WPP national population)
#     * data/processed/Country_groupings_extended.csv ; data/raw/who-regions.csv
#     * the input workbook Assumptions sheets (VSL/discount/currency parameters)
#   Does NOT re-read Model 06 output and does NOT rebuild life expectancy from the
#   raw WPP file (raw rebuild survives only as an explicit, warned fallback).
#   Writes: output/08_vsl_results.rds/.csv  (+ the existing summary tables, and
#           output/08_bca_parameters.rds -- the resolved BCA parameter set that
#           Model 09 anchors its Excel formulas to).
#
# ── VALUATION CASES ──────────────────────────────────────────────────────────
# REFERENCE CASE (preferred LMIC default, Robinson et al. 2019):
#   VSL = MAX( vsl_us_gni_ratio * GNIpc_US * (GNIpc_IDN/GNIpc_US)^elasticity,
#              vsl_floor_gni_multiple * GNIpc_IDN )
#   with elasticity = vsl_income_elasticity_preferred (1.5) and the floor multiple
#   (20). Standardized sensitivities: VSL = 100*GNIpc_IDN and VSL = 160*GNIpc_IDN.
#   All numbers come from the workbook Assumptions rows, NOT hard-coded here.
#
# LEGACY differential-elasticity case (0.8 at/above US income, 1.2 below), coded
#   e1_2 (with e1_0 = uniform 1.0 and e1_5 = uniform 1.5), is RETAINED only as a
#   labelled legacy/additional sensitivity for the Aim-1 slide pipeline and the
#   existing report. It is NOT the reference case and is NOT used for the headline
#   benefit-cost ratio. (Note: e1_5 == the reference case by construction, since
#   the preferred case is elasticity 1.5 + the 20x floor.)
#
# VSLY (age/LE sensitivity, not a co-primary welfare estimate):
#   VSLY = VSL / undiscounted remaining LE at the average age of the adult
#   (working-age) population [vsly_adult_min_age .. vsly_adult_max_age].
#   VSLY value = VSLY * Sum_a [ deaths_averted(a) * LE(a) ] using Model 07's
#   age-specific life-years gained (so it is not an algebraic identity with VSL).
#
# DISCOUNTING: undiscounted annual streams are reported; calendar-year discount
#   factors bring streams to bca_base_year at the primary rate, with the
#   Guidelines' standardized sensitivities (3% and 2x near-term real GDP-pc
#   growth). Within-lifetime treatment (LE) is kept separate from calendar-year
#   discounting (no double discounting).
#
# MONETARY BASIS: VSL benefits are in PPP international dollars (transfer uses PPP
#   GNI per capita). Costs (Model 09) are market-exchange-rate USD; the workbook
#   cost_to_bca_currency_factor converts them to the benefit basis in Model 09 --
#   the factor is never silently assumed to be 1.
#===========================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(countrycode)
  library(stringr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# 0) Paths ----
# Output root. Honor `wd_outp` when Model 00 (or an isolated runner such as the
# 70-30-30 -> 70-70-70 cascade) supplies it, so every Model 08 read (Model 07
# health outcomes) and write (VSL results, BCA parameters, summaries) lands in
# the SAME directory the rest of that run uses. Default is byte-for-byte the old
# behavior when `wd_outp` is absent.
DIR_OUT      <- if (exists("wd_outp")) sub("/+$", "", wd_outp) else file.path(wd, "output")
GNI_FILE     <- file.path(wd, "data", "raw",
                          "API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_7203.csv")
SSP_FILE     <- file.path(wd, "data", "raw",
                          "1721734326790-ssp_basic_drivers_release_3.1_full.xlsx")
LT_FILE      <- file.path(wd, "data", "raw",
                          "WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx")
COUNTRY_FILE <- file.path(wd, "data", "processed",
                          "Country_groupings_extended.csv")
DALYS_FILE   <- file.path(DIR_OUT, "dt_output_dalys.rds")
LE_LOOKUP    <- file.path(DIR_OUT, "07_life_expectancy_lookup.rds")

OUT_FILE     <- file.path(DIR_OUT, "08_vsl_results.rds")
OUT_CSV      <- file.path(DIR_OUT, "08_vsl_results.csv")
OUT_BCA      <- file.path(DIR_OUT, "08_bca_parameters.rds")
OUT_SUMM_VSL       <- file.path(DIR_OUT, "08_vsl_summary_table.rds")
OUT_SUMM_VSLY      <- file.path(DIR_OUT, "08_vsly_summary_table.rds")
OUT_SUMM_APP       <- file.path(DIR_OUT, "08_vsl_vsly_summary_table_appended.rds")
OUT_SUMM_VSL_PRIM  <- file.path(DIR_OUT, "08_vsl_summary_table_e1_2_primary.rds")
OUT_SUMM_VSLY_PRIM <- file.path(DIR_OUT, "08_vsly_summary_table_e1_2_primary.rds")

# 1) Resolve BCA parameters from the workbook Assumptions sheet(s) ----
#
# Analytic BCA/VSL parameters live ONLY in the input workbooks. Model 08 reads
# them (reconciling the two families when both run) and never hard-codes them.

read_assum <- function(path) {
  a <- as.data.table(read_excel(path, sheet = "Assumptions"))
  setNames(as.character(a$value), as.character(a$parameter_id))
}

# Which workbook(s) are active this run?
.rci  <- if (exists("run_clinical_interventions"))      isTRUE(run_clinical_interventions)      else TRUE
.rphi <- if (exists("run_public_health_interventions")) isTRUE(run_public_health_interventions) else FALSE
.paths <- character(0)
if (.rci  && exists("model_inputs_file"))         .paths <- c(.paths, clinical = model_inputs_file)
if (.rphi && exists("public_health_inputs_file")) .paths <- c(.paths, public_health = public_health_inputs_file)
if (!length(.paths)) {   # standalone fallback: whatever workbook path is defined
  if (exists("model_inputs_file"))              .paths <- c(clinical = model_inputs_file)
  else if (exists("public_health_inputs_file")) .paths <- c(public_health = public_health_inputs_file)
  else stop("Model 08: no input-workbook path (model_inputs_file / public_health_inputs_file).")
}
.assum_list <- lapply(.paths, read_assum)

# Parameters that MUST be identical across active workbooks (BCA transfer basis).
BCA_PARAM_IDS <- c(
  "bca_base_year", "bca_discount_rate_primary", "bca_discount_rate_sensitivity_3pct",
  "bca_discount_rate_sensitivity_2x_gdp_pc_growth", "vsl_us_gni_ratio",
  "vsl_income_elasticity_preferred", "vsl_floor_gni_multiple",
  "vsl_sensitivity_gni_multiple_100", "vsl_sensitivity_gni_multiple_160",
  "vsly_adult_min_age", "vsly_adult_max_age", "bca_currency_basis",
  "bca_price_year", "cost_to_bca_currency_factor", "bca_standing", "bca_scope")

# Reconcile: fail loudly (consolidated) if the active workbooks disagree, or if a
# required parameter is absent from every active workbook.
.conflicts <- character(0); .missing <- character(0); BCA <- list()
for (pid in BCA_PARAM_IDS) {
  vals <- lapply(.assum_list, function(A) A[[pid]])
  present <- vals[!vapply(vals, function(v) is.null(v) || is.na(v), logical(1))]
  if (!length(present)) { .missing <- c(.missing, pid); next }
  uu <- unique(unlist(present))
  if (length(uu) > 1)
    .conflicts <- c(.conflicts,
                    sprintf("%s: {%s}", pid,
                            paste(sprintf("%s=%s", names(.paths)[seq_along(present)], unlist(present)),
                                  collapse = ", ")))
  BCA[[pid]] <- present[[1]]
}
if (length(.missing))
  stop("Model 08: required BCA parameter(s) missing from the input workbook Assumptions ",
       "sheet(s): ", paste(.missing, collapse = ", "),
       "\n  Add them (parameter_id/value/...) to: ", paste(.paths, collapse = "; "),
       call. = FALSE)
if (length(.conflicts))
  stop("Model 08: the active workbooks carry CONFLICTING BCA assumptions in a ",
       "both-families run:\n  ", paste(.conflicts, collapse = "\n  "),
       "\n  Make these identical in both workbooks before running.", call. = FALSE)

# Typed accessors
.num <- function(id) as.numeric(BCA[[id]])
.chr <- function(id) as.character(BCA[[id]])

US_VSL_RATIO   <- .num("vsl_us_gni_ratio")                 # 160 (reference-case transfer ratio)
VSL_ELAST_PREF <- .num("vsl_income_elasticity_preferred")  # 1.5 (preferred)
VSL_RATIO_FLOOR<- .num("vsl_floor_gni_multiple")           # 20  (floor multiple)
VSL_MULT_100   <- .num("vsl_sensitivity_gni_multiple_100") # 100 (standardized sensitivity 1)
VSL_MULT_160   <- .num("vsl_sensitivity_gni_multiple_160") # 160 (standardized sensitivity 2)
ADULT_MIN_AGE  <- as.integer(.num("vsly_adult_min_age"))   # 15  (working-age lower bound)
ADULT_MAX_AGE  <- as.integer(.num("vsly_adult_max_age"))   # 64  (working-age upper bound)
BASE_YEAR      <- as.integer(.num("bca_base_year"))        # 2025 (single BCA base year)
R_PRIMARY      <- .num("bca_discount_rate_primary")        # 0.03
R_SENS_3PCT    <- .num("bca_discount_rate_sensitivity_3pct")             # 0.03
R_SENS_2XG_WB  <- .num("bca_discount_rate_sensitivity_2x_gdp_pc_growth") # 0.08 (workbook default)
CURRENCY_BASIS <- .chr("bca_currency_basis")
PRICE_YEAR     <- as.integer(.num("bca_price_year"))
COST_TO_BCA    <- .num("cost_to_bca_currency_factor")
BCA_STANDING   <- .chr("bca_standing")
BCA_SCOPE      <- .chr("bca_scope")
MAX_MODEL_AGE  <- 95L

# LEGACY (differential-elasticity) sensitivity constants -- retained columns only,
# NOT the reference case. e1_2 = 0.8 at/above US income, 1.2 below; e1_0 = 1.0.
VSL_ELAST_HIC  <- 0.8; VSL_ELAST_LMIC <- 1.2; VSL_ELAST_LOW <- 1.0; VSL_ELAST_HIGH <- 1.5

# Closed-form within-lifetime annuity (kept for the auxiliary discounted LY column).
R_VSLY <- R_PRIMARY
disc_life_years <- function(le, r) {
  le <- as.numeric(le)
  if (r == 0) fifelse(is.na(le) | le <= 0, NA_real_, le)
  else        fifelse(is.na(le) | le <= 0, NA_real_, (1 - (1 + r)^(-le)) / r)
}

# 2) Load Model 07 health outcomes (age level) + the Model 07 LE lookup ----
if (!file.exists(DALYS_FILE))
  stop("Model 08: Model 07 output not found (", DALYS_FILE, "). Run 07_output_dalys.R first.",
       call. = FALSE)
dt_h <- as.data.table(readRDS(DALYS_FILE))
req_h <- c("location", "scenario", "scenario_label", "intervention_family",
           "htn_target_scenario", "year", "age", "sex", "cause",
           "deaths", "deaths_averted", "remaining_life_expectancy", "life_years_gained")
miss_h <- setdiff(req_h, names(dt_h))
if (length(miss_h))
  stop("Model 08: Model 07 output missing required column(s): ",
       paste(miss_h, collapse = ", "), "\n  Re-run Model 07 (its contract changed).", call. = FALSE)

model_years  <- sort(unique(dt_h$year))
analysis_yrs <- model_years

# Model 07 is the single source of life expectancy; only rebuild from raw WPP as
# an explicit, warned fallback (should not happen in the normal pipeline).
if (file.exists(LE_LOOKUP)) {
  le_lookup <- as.data.table(readRDS(LE_LOOKUP))
} else {
  warning("Model 08: Model 07 LE lookup (", LE_LOOKUP, ") not found; ",
          "rebuilding life expectancy from the raw WPP file as a FALLBACK.")
  lt <- as.data.table(read_excel(LT_FILE, sheet = "Medium variant", range = "A17:DH22967"))
  setnames(lt, c("Region, subregion, country or area *", "Year"),
           c("location", "year"), skip_absent = TRUE)
  le_lookup <- melt(lt, id.vars = colnames(lt)[1:11],
                    variable.name = "age", value.name = "remaining_life_expectancy")
  le_lookup <- le_lookup[year >= min(model_years), .(location, age, year, remaining_life_expectancy)]
  le_lookup[, age := as.numeric(str_extract(as.character(age), "[0-9]+"))]
  le_lookup[, remaining_life_expectancy := as.numeric(remaining_life_expectancy)]
  le_lookup <- le_lookup[age >= 0 & age <= MAX_MODEL_AGE]
}

# 3) Scenario-year health aggregates from Model 07 (deaths averted, LY gained) ----
# All aggregation is over age/sex/cause at the scenario x year x htn grain. Deaths
# averted and life-years gained already carry Model 07's baseline pairing.
dt_saved <- dt_h[, .(
  deaths_baseline      = sum(base_deaths,       na.rm = TRUE),
  deaths_intervention  = sum(deaths,            na.rm = TRUE),
  deaths_averted       = sum(deaths_averted,    na.rm = TRUE),
  life_years_gained_undisc = sum(life_years_gained, na.rm = TRUE),
  # within-lifetime discounted LY: age-specific deaths averted x annuitised LE
  life_years_gained_disc   = sum(deaths_averted * disc_life_years(remaining_life_expectancy, R_VSLY),
                                 na.rm = TRUE),
  avg_age_of_death_averted = fifelse(sum(deaths_averted, na.rm = TRUE) != 0,
                                     sum(age * deaths_averted, na.rm = TRUE) /
                                       sum(deaths_averted, na.rm = TRUE), NA_real_)),
  by = .(location, year, scenario, scenario_label, intervention_family, htn_target_scenario)]

# Valuation applies to comparator scenarios only (baseline has zero averted).
base_id  <- if (exists("baseline_scenario_id")) baseline_scenario_id else "baseline"
dt_compare <- dt_saved[scenario != base_id]
if (!nrow(dt_compare))
  stop("Model 08: no comparator scenarios found in Model 07 output.", call. = FALSE)

# 4) Country mapping (iso3, WHO region) ----
country_grp <- fread(COUNTRY_FILE)
required_country_cols <- c("iso3", "location", "region")
missing_country_cols  <- setdiff(required_country_cols, names(country_grp))
if (length(missing_country_cols) > 0)
  stop("Country mapping file missing required columns: ",
       paste(missing_country_cols, collapse = ", "))
dt_compare <- country_grp[dt_compare, on = .(location)]
missing_iso3 <- is.na(dt_compare$iso3)
if (any(missing_iso3))
  dt_compare[missing_iso3, iso3 := countrycode(location, "country.name", "iso3c", warn = FALSE)]
setnames(dt_compare, "region", "who_region")

# 6) GNI per capita (PPP) with SSP2 forward projection ---------------------------
#    (unchanged World Bank + IIASA SSP 3.1 machinery). Observed World Bank GNI pc
#    PPP is kept; years beyond the last observed year are projected with SSP2
#    annual GDP-per-capita growth. NOTE: SSP provides GDP not GNI (see header of
#    prior versions); tracked closely for Indonesia.
gni_raw   <- fread(GNI_FILE, skip = 4, header = TRUE)
year_cols <- grep("^[0-9]{4}$", names(gni_raw), value = TRUE)
gni <- melt(gni_raw, id.vars = "Country Code", measure.vars = year_cols,
            variable.name = "year", value.name = "gni_pc_ppp")
setnames(gni, "Country Code", "iso3")
gni[, year := as.integer(as.character(year))]
gni <- gni[!is.na(gni_pc_ppp) & year >= 2000 & year <= max(model_years)]

if (!exists("ssp_gdp")) ssp_gdp <- as.data.table(read_excel(SSP_FILE, sheet = "data"))
ssp_pc <- ssp_gdp[Scenario == "SSP2" & Variable == "GDP|PPP [per capita]"]
if (nrow(ssp_pc) == 0)
  stop("SSP data filtered to 0 rows. Check Scenario='SSP2' and Variable='GDP|PPP [per capita]'.")
ssp_yr_cols <- grep("^[0-9]{4}$", names(ssp_pc), value = TRUE)
ssp_pc_long <- melt(ssp_pc, id.vars = "Region", measure.vars = ssp_yr_cols,
                    variable.name = "year", value.name = "ssp_gdp_pc")
setnames(ssp_pc_long, "Region", "location")
ssp_pc_long[, year := as.integer(as.character(year))]
ssp_pc_long[, iso3 := countrycode(location, "country.name", "iso3c")]
# Drop SSP placeholder nodes (this release stores 1.0 for pre-projection years,
# e.g. 2020); a log-linear interp between a 1.0 placeholder and a real 2025 value
# would fabricate enormous growth. Keeping only real nodes makes approx(rule=2)
# hold the first real (2025) value flat for earlier years.
ssp_pc_long <- ssp_pc_long[!is.na(iso3) & !is.na(ssp_gdp_pc) & ssp_gdp_pc > 100]

iso3_list <- c("IDN", "USA")   # target country + US reference

# Contiguous projection years: from the last observed GNI year (so the forward
# recursion has an anchor and no gap) through the last analysis year. The model
# horizon may start (2025) AFTER the last observed World Bank GNI year (~2023);
# projecting over a contiguous span avoids breaking the year-on-year chain.
gni_last <- gni[iso3 %in% iso3_list, {
  idx <- which.max(year); .(last_year = year[idx], gni_last = gni_pc_ppp[idx])
}, by = iso3]
proj_years <- seq(min(gni_last$last_year, na.rm = TRUE), max(model_years))

# SSP2 GDP-pc interpolated (log-linear) to the contiguous projection years.
ssp_annual <- ssp_pc_long[iso3 %in% iso3_list, {
  ord <- order(year)
  log_interp <- approx(x = year[ord], y = log(ssp_gdp_pc[ord]), xout = proj_years, rule = 2)$y
  data.table(year = proj_years, ssp_interp = exp(log_interp))
}, by = iso3]
setorder(ssp_annual, iso3, year)
ssp_annual[, ssp_growth := ssp_interp / shift(ssp_interp) - 1, by = iso3]

# 2x near-term (2026-2030) real GDP-pc growth sensitivity, recomputed from SSP2.
.g_near <- mean(ssp_annual[iso3 == "IDN" & year %in% 2026:2030, ssp_growth], na.rm = TRUE)
R_SENS_2XG <- if (is.finite(.g_near) && .g_near > 0) 2 * .g_near else R_SENS_2XG_WB
BCA[["bca_discount_rate_sensitivity_2x_gdp_pc_growth_computed"]] <- R_SENS_2XG

# Project over the contiguous proj_years so the recursion is anchored at the
# last observed GNI year and never hits a gap, then subset to the model horizon.
gni_grid <- CJ(iso3 = iso3_list, year = proj_years)
gni_grid <- gni[gni_grid, on = .(iso3, year)]
gni_grid <- ssp_annual[gni_grid, on = .(iso3, year)]
gni_grid <- gni_last[gni_grid, on = .(iso3)]
setorder(gni_grid, iso3, year)
gni_grid[, gni_pc_proj := {
  out <- gni_pc_ppp
  if (!is.na(last_year[1]) && !is.na(gni_last[1])) {
    future_idx <- which(year > last_year[1])
    for (j in future_idx) {
      if (year[j] == last_year[1] + 1) { if (!is.na(ssp_growth[j])) out[j] <- gni_last[1] * (1 + ssp_growth[j]) }
      else if (!is.na(out[j - 1]) && !is.na(ssp_growth[j])) out[j] <- out[j - 1] * (1 + ssp_growth[j])
    }
  }
  out
}, by = iso3]
gni_grid[, gni_pc_ppp_final := fifelse(!is.na(gni_pc_ppp), gni_pc_ppp, gni_pc_proj)]
gni_grid <- gni_grid[year %in% model_years, .(iso3, year, gni_pc_ppp = gni_pc_ppp_final)]
if (sum(is.na(gni_grid$gni_pc_ppp)) > 0)
  warning(sum(is.na(gni_grid$gni_pc_ppp)),
          " (iso3, year) rows still missing GNI after SSP projection.")
dt_compare <- gni_grid[dt_compare, on = .(iso3, year)]
us_gni <- gni_grid[iso3 == "USA", .(year, gni_pc_usa = gni_pc_ppp)]
if (nrow(us_gni) == 0) stop("No USA GNI values found (iso3='USA' in the GNI file).")
dt_compare <- us_gni[dt_compare, on = .(year)]

# 7) UN WPP national population + average adult (working-age) age --------------
#    Population and the average-adult-age denominator use externally sourced UN
#    WPP counts (stable, scenario-invariant), not the competing-risks per-cause
#    model population. Average adult age uses the workbook working-age band.
dt_pop_unwpp <- as.data.table(readRDS(paste0(wd_data, "PopulationsSingleAge0050.rds")))
setnames(dt_pop_unwpp, "year_id", "year", skip_absent = TRUE)
# Modeled location(s) and analysis horizon only (avoids out-of-horizon LE misses).
dt_pop_unwpp <- dt_pop_unwpp[location %in% unique(dt_compare$location) & year %in% model_years]
dt_pop_unwpp[age >= MAX_MODEL_AGE, age := MAX_MODEL_AGE]
dt_pop_unwpp <- dt_pop_unwpp[, .(Nx = sum(Nx)), by = .(location, year, age)]

# national population (income denominator)
pop_total <- dt_pop_unwpp[, .(population = sum(Nx)), by = .(location, year)]
# average adult age over the working-age band [ADULT_MIN_AGE .. ADULT_MAX_AGE]
adult <- dt_pop_unwpp[age >= ADULT_MIN_AGE & age <= ADULT_MAX_AGE,
                      .(adult_population = sum(Nx),
                        avg_adult_age   = sum(Nx * age) / sum(Nx)), by = .(location, year)]
adult[, age_ref_5y := as.integer(pmin(MAX_MODEL_AGE, round(avg_adult_age)))]  # single-year LE age
# remaining LE at the average adult age (Model 07 single-year lookup)
adult <- merge(adult, le_lookup[, .(location, year, age, le_avg_adult = remaining_life_expectancy)],
               by.x = c("location", "year", "age_ref_5y"),
               by.y = c("location", "year", "age"), all.x = TRUE)
if (sum(is.na(adult$le_avg_adult)) > 0)
  warning(sum(is.na(adult$le_avg_adult)), " (location, year) rows missing LE at average adult age.")

dt_compare <- merge(dt_compare, pop_total, by = c("location", "year"), all.x = TRUE)
dt_compare <- merge(dt_compare, adult,     by = c("location", "year"), all.x = TRUE)
dt_compare[, le_avg_adult_disc := disc_life_years(le_avg_adult, R_VSLY)]
dt_compare[, total_national_gni := population * gni_pc_ppp]

# 8) VSL transfer -- REFERENCE CASE + standardized sensitivities + legacy -------
# Reference case (preferred): elasticity 1.5 transfer, floored at 20x GNIpc.
dt_compare[, vsl_preferred := pmax(
  US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_PREF,
  VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
# Also expose the transfer BEFORE the floor and the floor itself (for the workbook).
dt_compare[, vsl_transfer_prefloor := US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_PREF]
dt_compare[, vsl_floor := VSL_RATIO_FLOOR * gni_pc_ppp]
# Standardized sensitivities: constant multiples of target GNI per capita.
dt_compare[, vsl_gni100 := VSL_MULT_100 * gni_pc_ppp]
dt_compare[, vsl_gni160 := VSL_MULT_160 * gni_pc_ppp]

# LEGACY differential-elasticity columns (retained; NOT the reference case).
dt_compare[, vsl_e1_0 := US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_LOW]
dt_compare[, vsl_e1_5 := US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_HIGH]
dt_compare[, vsl_e1_2 := fifelse(gni_pc_ppp >= gni_pc_usa,
  US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_HIC,
  US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_LMIC)]
dt_compare[, vsl_e1_0 := pmax(vsl_e1_0, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
dt_compare[, vsl_e1_2 := pmax(vsl_e1_2, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
dt_compare[, vsl_e1_5 := pmax(vsl_e1_5, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
# VSL / GNIpc ratio for the reference case (audit).
dt_compare[, vsl_preferred_gni_ratio := vsl_preferred / gni_pc_ppp]

# 9) VSLY (VSL / undiscounted LE at average adult age) --------------------------
dt_compare[le_avg_adult > 0, `:=`(
  vsly_preferred = vsl_preferred / le_avg_adult,
  vsly_gni100    = vsl_gni100    / le_avg_adult,
  vsly_gni160    = vsl_gni160    / le_avg_adult,
  vsly_e1_0      = vsl_e1_0      / le_avg_adult,
  vsly_e1_2      = vsl_e1_2      / le_avg_adult,
  vsly_e1_5      = vsl_e1_5      / le_avg_adult)]

# Monetary value of deaths averted -- VSL (undiscounted annual streams)
dt_compare[, `:=`(
  economic_value_preferred = vsl_preferred * deaths_averted,
  economic_value_gni100    = vsl_gni100    * deaths_averted,
  economic_value_gni160    = vsl_gni160    * deaths_averted,
  economic_value_e1_0      = vsl_e1_0      * deaths_averted,
  economic_value_e1_2      = vsl_e1_2      * deaths_averted,
  economic_value_e1_5      = vsl_e1_5      * deaths_averted)]
# Monetary value -- VSLY (undiscounted age-specific life-years gained)
dt_compare[, `:=`(
  vsly_value_preferred = vsly_preferred * life_years_gained_undisc,
  vsly_value_gni100    = vsly_gni100    * life_years_gained_undisc,
  vsly_value_gni160    = vsly_gni160    * life_years_gained_undisc,
  vsly_value_e1_0      = vsly_e1_0      * life_years_gained_undisc,
  vsly_value_e1_2      = vsly_e1_2      * life_years_gained_undisc,
  vsly_value_e1_5      = vsly_e1_5      * life_years_gained_undisc)]

# 10) Calendar-year discounting to the single BCA base year --------------------
# Primary rate + standardized sensitivities (3% and 2x near-term GDP-pc growth).
# Legacy disc_r1/r3/r5 retained (disc_r3 is consumed downstream); disc_r3 equals
# the primary factor because the primary rate is 3%.
dt_compare[, `:=`(
  disc_bca_primary = 1 / (1 + R_PRIMARY)^(year - BASE_YEAR),
  disc_bca_3pct    = 1 / (1 + R_SENS_3PCT)^(year - BASE_YEAR),
  disc_bca_2xg     = 1 / (1 + R_SENS_2XG)^(year - BASE_YEAR),
  disc_r1 = 1 / (1 + 0.01)^(year - BASE_YEAR),
  disc_r3 = 1 / (1 + R_PRIMARY)^(year - BASE_YEAR),
  disc_r5 = 1 / (1 + 0.05)^(year - BASE_YEAR))]
dt_compare[, gni_pc_disc_r3 := gni_pc_ppp * disc_r3]

# PV benefit columns (primary rate) -- reference case + legacy sensitivities kept.
dt_compare[, `:=`(
  economic_value_preferred_disc = economic_value_preferred * disc_bca_primary,
  vsly_value_preferred_disc     = vsly_value_preferred     * disc_bca_primary,
  economic_value_e1_2_disc_r3   = economic_value_e1_2      * disc_r3,
  vsly_value_e1_2_disc_r3       = vsly_value_e1_2          * disc_r3,
  economic_value_e1_5_disc_r3   = economic_value_e1_5      * disc_r3,
  vsly_value_e1_5_disc_r3       = vsly_value_e1_5          * disc_r3)]

# BCA basis metadata (constants carried onto every row so Model 09 can anchor to them)
dt_compare[, `:=`(bca_base_year = BASE_YEAR, bca_currency_basis = CURRENCY_BASIS,
                  bca_price_year = PRICE_YEAR, bca_standing = BCA_STANDING,
                  bca_scope = BCA_SCOPE, cost_to_bca_currency_factor = COST_TO_BCA)]

# 11) Final dataset (superset: reference case + legacy compat columns) ----------
setorder(dt_compare, location, year, scenario)
dt_final <- copy(dt_compare)

# WHO region relabelling (retain existing who_region encoding for the summaries)
country_who <- fread(file.path(wd, "data", "raw", "who-regions.csv"))
setnames(country_who, old = c("Entity", "Code", "World regions according to WHO"),
         new = c("location", "iso3_who", "region_who"), skip_absent = TRUE)
if ("region_who" %in% names(country_who)) {
  country_who[, region_who := gsub("\\s*\\(WHO\\)", "", region_who)]
  if ("Year" %in% names(country_who)) country_who[, Year := NULL]
  rw <- unique(country_who[, .(location, region_who)])
  dt_final <- rw[dt_final, on = .(location)]
  dt_final[, region_who := fcase(
    region_who == "AFR", "Africa", region_who == "EMR", "Eastern Mediterranean",
    region_who == "EUR", "Europe", region_who == "AMR", "Americas",
    region_who == "SEAR", "South-East Asia", region_who == "WPR", "Western Pacific",
    default = region_who)]
  dt_final[!is.na(region_who), who_region := region_who]
  dt_final[, region_who := NULL]
}
dt_final[is.na(who_region), who_region := "South-East Asia"]   # Indonesia default

if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)
saveRDS(dt_final, OUT_FILE)
fwrite(dt_final, OUT_CSV)

# Resolved BCA parameter set (Model 09 anchors its Excel formulas to this).
bca_params <- data.table(
  parameter_id = c(BCA_PARAM_IDS, "bca_discount_rate_sensitivity_2x_gdp_pc_growth_computed"),
  value = c(vapply(BCA_PARAM_IDS, function(i) as.character(BCA[[i]]), character(1)),
            as.character(R_SENS_2XG)))
saveRDS(bca_params, OUT_BCA)

cat("Saved:", OUT_FILE, "\n")
cat("Rows:", nrow(dt_final), "| Columns:", ncol(dt_final), "\n")
cat("Scenarios:", paste(sort(unique(dt_final$scenario)), collapse = ", "), "\n")
cat("Families:", paste(sort(unique(dt_final$intervention_family)), collapse = ", "), "\n")
cat(sprintf("Reference-case VSL (IDN, %d): %.3e int$ | VSL/GNIpc = %.1f | floor active in %d row(s)\n",
            BASE_YEAR,
            dt_final[year == BASE_YEAR, mean(vsl_preferred, na.rm = TRUE)],
            dt_final[year == BASE_YEAR, mean(vsl_preferred_gni_ratio, na.rm = TRUE)],
            dt_final[vsl_preferred > vsl_transfer_prefloor + 1e-6, .N]))
cat(sprintf("Discount rates: primary=%.3f | 3%%=%.3f | 2x-growth(computed)=%.3f\n",
            R_PRIMARY, R_SENS_3PCT, R_SENS_2XG))

# 13) Summary reporting tables (unchanged schema; legacy e1_2 primary / e1_5) ---
# NOTE: these tables retain the historical e1_2/e1_5 elasticity_case labels used by
# scenarios/cvd/aim1_report.Rmd. e1_2 here is the LEGACY differential-elasticity
# case, retained for backward compatibility -- not the 2019 reference case.
make_summary_table <- function(dt, value_col, value_prefix) {
  stopifnot(value_col %in% names(dt), "gni_pc_disc_r3" %in% names(dt), "who_region" %in% names(dt))
  slice_region <- function(yr) {
    sub <- dt[year == yr, .(metric_value = sum(get(value_col), na.rm = TRUE),
                            total_income = sum(population * gni_pc_disc_r3, na.rm = TRUE)),
              by = .(who_region, scenario)]
    sub[, share_income := metric_value / total_income][, total_income := NULL]
    setnames(sub, c("metric_value", "share_income"),
             c(paste0(value_prefix, "_", yr), paste0("share_", yr))); sub
  }
  slice_world <- function(yr) {
    sub <- dt[year == yr, .(metric_value = sum(get(value_col), na.rm = TRUE),
                            total_income = sum(population * gni_pc_disc_r3, na.rm = TRUE)),
              by = .(scenario)]
    sub[, `:=`(who_region = "World", share_income = metric_value / total_income)][, total_income := NULL]
    setnames(sub, c("metric_value", "share_income"),
             c(paste0(value_prefix, "_", yr), paste0("share_", yr))); sub
  }
  merge_slices <- function(lst) Reduce(function(a, b)
    merge(a, b, by = c("who_region", "scenario"), all = TRUE), lst)
  total_region <- function() {
    sub <- dt[year >= BASE_YEAR & year <= max(model_years),
              .(metric_total = sum(get(value_col), na.rm = TRUE),
                income_total = sum(population * gni_pc_disc_r3, na.rm = TRUE)),
              by = .(who_region, scenario)]
    sub[, share_total := metric_total / income_total][, income_total := NULL]
    setnames(sub, "metric_total", paste0(value_prefix, "_total")); sub
  }
  total_world <- function() {
    sub <- dt[year >= BASE_YEAR & year <= max(model_years),
              .(metric_total = sum(get(value_col), na.rm = TRUE),
                income_total = sum(population * gni_pc_disc_r3, na.rm = TRUE)), by = .(scenario)]
    sub[, `:=`(who_region = "World", share_total = metric_total / income_total)][, income_total := NULL]
    setnames(sub, "metric_total", paste0(value_prefix, "_total")); sub
  }
  SUMMARY_YEARS <- intersect(c(2026L, 2030L, 2040L, 2050L), model_years)
  reg_ann   <- merge_slices(lapply(SUMMARY_YEARS, slice_region))
  world_ann <- merge_slices(lapply(SUMMARY_YEARS, slice_world))
  s_region  <- merge(reg_ann,   total_region(), by = c("who_region", "scenario"), all = TRUE)
  s_world   <- merge(world_ann, total_world(),  by = c("who_region", "scenario"), all = TRUE)
  out <- rbind(s_region, s_world, fill = TRUE); setorder(out, scenario, who_region); out
}

dt_summary_vsl_e1_2  <- make_summary_table(dt_final, "economic_value_e1_2_disc_r3", "vsl")
dt_summary_vsly_e1_2 <- make_summary_table(dt_final, "vsly_value_e1_2_disc_r3",     "vsly")
dt_summary_vsl_e1_5  <- make_summary_table(dt_final, "economic_value_e1_5_disc_r3", "vsl")
dt_summary_vsly_e1_5 <- make_summary_table(dt_final, "vsly_value_e1_5_disc_r3",     "vsly")

rename_to_metric <- function(dt, old_prefix) {
  old_val <- grep(paste0("^", old_prefix, "_"), names(dt), value = TRUE)
  setnames(dt, old_val, sub(paste0("^", old_prefix, "_"), "metric_", old_val)); dt
}
dt_summary_vsl_e1_2  <- rename_to_metric(dt_summary_vsl_e1_2,  "vsl")
dt_summary_vsly_e1_2 <- rename_to_metric(dt_summary_vsly_e1_2, "vsly")
dt_summary_vsl_e1_5  <- rename_to_metric(dt_summary_vsl_e1_5,  "vsl")
dt_summary_vsly_e1_5 <- rename_to_metric(dt_summary_vsly_e1_5, "vsly")

dt_summary_appended <- rbindlist(list(
  copy(dt_summary_vsl_e1_2) [, `:=`(valuation_type = "VSL",  elasticity_case = "e1_2_primary")],
  copy(dt_summary_vsly_e1_2)[, `:=`(valuation_type = "VSLY", elasticity_case = "e1_2_primary")],
  copy(dt_summary_vsl_e1_5) [, `:=`(valuation_type = "VSL",  elasticity_case = "e1_5_sensitivity")],
  copy(dt_summary_vsly_e1_5)[, `:=`(valuation_type = "VSLY", elasticity_case = "e1_5_sensitivity")]),
  fill = TRUE, use.names = TRUE)
setcolorder(dt_summary_appended,
            c("valuation_type", "elasticity_case", "who_region", "scenario",
              setdiff(names(dt_summary_appended),
                      c("valuation_type", "elasticity_case", "who_region", "scenario"))))
setorder(dt_summary_appended, valuation_type, elasticity_case, scenario, who_region)

# 15) Save summary tables (unchanged filenames) --------------------------------
saveRDS(dt_summary_vsl_e1_2,  OUT_SUMM_VSL)
saveRDS(dt_summary_vsly_e1_2, OUT_SUMM_VSLY)
saveRDS(dt_summary_appended,  OUT_SUMM_APP)
saveRDS(dt_summary_vsl_e1_2,  OUT_SUMM_VSL_PRIM)
saveRDS(dt_summary_vsly_e1_2, OUT_SUMM_VSLY_PRIM)
fwrite(dt_summary_vsl_e1_2,  file.path(DIR_OUT, "08_vsl_summary_table_e1_2_primary.csv"))
fwrite(dt_summary_vsly_e1_2, file.path(DIR_OUT, "08_vsly_summary_table_e1_2_primary.csv"))
fwrite(dt_summary_vsl_e1_5,  file.path(DIR_OUT, "08_vsl_summary_table_e1_5_sensitivity.csv"))
fwrite(dt_summary_vsly_e1_5, file.path(DIR_OUT, "08_vsly_summary_table_e1_5_sensitivity.csv"))
fwrite(dt_summary_appended,  file.path(DIR_OUT, "08_vsl_vsly_summary_table_appended.csv"))

message(sprintf("=== Model 08 complete: %d rows, %d comparator scenarios ===",
                nrow(dt_final), uniqueN(dt_final$scenario)))
