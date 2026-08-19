rm(list=ls()) 

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)   
library(countrycode)
library(stringr)
library(parallel)
library(doParallel)
library(foreach)
library(gmodels)

# For forecasting mortality
library(forecast)

wd <- "C:/Users/wrgar/OneDrive - UW/02Work/WorldBank-Indonesia/uw-wb-indonesia-ncd/"

wd_code <- paste0(wd,"code/cvd-fair-choices/")

# Raw data not available on GitHub
wd_raw <- paste0(wd,"data/raw/")

# Processed data (from base rates and tps)
wd_data <- paste0(wd,"data/processed/")
wd_outp <- paste0(wd,"output/")

# Create a temporary directory for the processing data change to wd in final version
wd_temp <- paste0("C:/Users/wrgar/OneDrive - UW/02Work/WorldBank-Indonesia/","temp/")
if (!dir.exists(wd_temp)) {
  dir.create(wd_temp, recursive = TRUE)
}

setwd(paste0(wd_code))

#...........................................................
# 0. Functions and parameters-----
#...........................................................

source("01_utils_indonesia.R")

run_calibration_par <- TRUE # set to TRUE to run parallel calibration

run_adjustment_model <- FALSE # set to TRUE to run adjustment model

run_aod_par <- FALSE # set to TRUE to run model with dementia

run_adjustments_inputs <- FALSE

run_bgmx_trend <- TRUE

run_CF_trend   <- TRUE

# Baseline scenario 80% of secular trend. 20% historically explained by
# HTN control improvements

run_CF_trend_80   <- TRUE

run_CF_trend_ihme  <- FALSE

#===========================================================================
# COST / VALUE (Model 09) EXECUTION CONFIG -- execution metaparameters only ----
#---------------------------------------------------------------------------
# Single source of truth for execution-level settings shared by Models 04/06/09.
# ANALYTIC assumptions (selected intervention-cause links, effects, affected
# fractions, coverage, cost components, cost discount rate, analysis years,
# price year, economic perspective) live ONLY in the input workbook below --
# never duplicate them here.
#===========================================================================
# Path to the user-editable analytic input workbook (authoritative contract).
model_inputs_file             <- paste0(wd, "data/indonesia_model_inputs.xlsx")
# Final cost/value workbook written by Model 09.
cost_value_output_file        <- paste0(wd_outp, "indonesia_model_cost_value.xlsx")
# Whether to run Model 09 (costing / budget impact / cost-effectiveness).
run_cost_value                <- TRUE
# Scenario id treated as the no-new-intervention comparator.
baseline_scenario_id          <- "baseline"
# TRUE  = STOP the run if any SELECTED workbook link/cost has a FAIL-level issue.
# FALSE = drop the invalid links/scenarios (with a consolidated diagnostic) and
#         run only the valid scenarios; blockers are reported in Model 09.
strict_model_input_validation <- FALSE

#===========================================================================
# INTERVENTION-FAMILY EXECUTION SWITCHES  (execution-level only) ----
#---------------------------------------------------------------------------
# Choose which intervention FAMILIES this run analyses. Both are user-editable
# and independently honoured by Models 04 (catalogue), 06 (scenarios) and 09
# (cost/value workbooks). The baseline (no-new-intervention) comparator ALWAYS
# runs regardless of these switches -- it is required for valid deaths-averted
# and incremental-cost comparisons.
#   TRUE / TRUE   -> both clinical (FAIR Choices) and public-health families
#   TRUE / FALSE  -> public-health interventions only
#   FALSE / TRUE  -> clinical interventions only
#   FALSE / FALSE -> nothing to analyse: the run stops early with an error.
# NOTE: analytic assumptions (exposures, effects, lags, costs, coverage, years,
# discounting) stay in the Excel input workbooks -- NEVER duplicate them here.
run_public_health_interventions <- TRUE
run_clinical_interventions      <- TRUE
if (!isTRUE(run_public_health_interventions) && !isTRUE(run_clinical_interventions))
  stop("No intervention family selected: set run_public_health_interventions ",
       "and/or run_clinical_interventions to TRUE. With both FALSE there is no ",
       "intervention analysis to run.", call. = FALSE)

# Public-health analytic input workbook (authoritative PH contract) and the
# public-health cost/value formula workbook Model 09 writes. Names are kept
# deliberately distinct from the clinical `model_inputs_file` /
# `cost_value_output_file` above so the two families can never be confused.
# The repository file name contains a space ("indonesia_model_inputs_ public_health.xlsx");
# use the ACTUAL file, falling back to the no-space variant if it is ever renamed.
public_health_inputs_file <- paste0(wd, "data/indonesia_model_inputs_ public_health.xlsx")
if (!file.exists(public_health_inputs_file)) {
  .ph_alt <- paste0(wd, "data/indonesia_model_inputs_public_health.xlsx")
  if (file.exists(.ph_alt)) public_health_inputs_file <- .ph_alt
  rm(.ph_alt)
}
public_health_cost_value_formulae_file <- paste0(
  wd_outp, "indonesia_cost_value_public_health_formulae.xlsx")

#===========================================================================
# CENTRAL MODEL CONFIGURATION  (SINGLE SOURCE OF TRUTH) ----
#---------------------------------------------------------------------------
# Ages and causes are declared ONLY here. Every downstream script (021-06 and
# the calibration) derives its age grid, GBD band mapping, cause filtering,
# loops, joins, validation and outputs from these objects. To add or remove a
# modeled cause, edit `cause_map` / `dx_include` below ONLY (provided the
# required GBD input fields exist for that cause -- see README). Do NOT
# re-declare the cause vector, the cause map, or the age grid anywhere else.
#===========================================================================

## --- Causes ----------------------------------------------------------------
# `cause_map`: stable short code = long GBD cause name. Keep "all" (All causes)
# LAST -- it is NOT a disease-state model; it is used only to derive background
# mortality as (all-cause minus the sum of modeled causes).
cause_map <- c(
  ihd      = "Ischemic heart disease",
  istroke  = "Ischemic stroke",
  hstroke  = "Intracerebral hemorrhage",
  hhd      = "Hypertensive heart disease",
  rhd     = "Rheumatic heart disease",
  cmd     = "Cardiomyopathy and myocarditis",
  dm2     = "Diabetes mellitus type 2",
  all      = "All causes"
)

# Short code used for the all-cause envelope (background mortality only).
all_cause_code <- "all"
all_cause_name <- unname(cause_map[[all_cause_code]])

# Modeled disease causes = everything except the all-cause envelope.
model_cause_codes <- setdiff(names(cause_map), all_cause_code)  # short codes
model_cause_names <- unname(cause_map[model_cause_codes])       # long GBD names

# `dx_include`: the long GBD names to KEEP when reading raw GBD extracts.
# Derived from cause_map so it can never drift from it.
dx_include <- unname(cause_map)

# Short codes (kept for backward compatibility with downstream references).
cause_cols <- names(cause_map)

## --- Ages ------------------------------------------------------------------
# Single-year model ages. Numeric age 95 represents the OPEN-ENDED GBD
# "95+ years" group: in the projection all survivors aged 95 and older are
# pooled and retained in this terminal stock (see 05/06 recursion).
min_model_age <- 0L
max_model_age <- 95L                 # 95 == open-ended 95+ terminal group
age_single    <- min_model_age:max_model_age

## --- Configuration validation (fail early) --------------------------------
# Guards against duplicate codes/names, a missing all-cause entry, invalid age
# bounds, and missing required fields. `gbd_age_bands()` (01_utils) provides the
# authoritative single-age <-> GBD-band mapping used everywhere downstream.
local({
  if (anyDuplicated(names(cause_map)))
    stop("cause_map has duplicate short codes: ",
         paste(names(cause_map)[duplicated(names(cause_map))], collapse = ", "))
  if (anyDuplicated(unname(cause_map)))
    stop("cause_map has duplicate long names: ",
         paste(unname(cause_map)[duplicated(unname(cause_map))], collapse = ", "))
  if (!all(nzchar(names(cause_map))) || any(is.na(cause_map)))
    stop("cause_map has empty/NA codes or names.")
  if (!(all_cause_code %in% names(cause_map)))
    stop("cause_map is missing the all-cause envelope code '", all_cause_code,
         "' required to derive background mortality.")
  if (length(model_cause_codes) < 1L)
    stop("cause_map defines no modeled disease causes (only the all-cause envelope).")
  if (!is.numeric(min_model_age) || !is.numeric(max_model_age) ||
      min_model_age < 0 || max_model_age <= min_model_age)
    stop("Invalid age bounds: need 0 <= min_model_age < max_model_age.")
  # gbd_age_bands() must cover every single-year model age exactly once.
  bands <- gbd_age_bands(min_model_age, max_model_age)
  covered <- unlist(Map(seq, bands$age_lo, pmin(bands$age_hi, max_model_age)))
  if (!setequal(covered, age_single))
    stop("gbd_age_bands() does not cover age_single exactly once; check 01_utils.")
  cat(sprintf("Config OK: %d modeled causes (%s) + all-cause envelope; ages %d-%d (%d==%s).\n",
              length(model_cause_codes), paste(model_cause_codes, collapse = ", "),
              min_model_age, max_model_age, max_model_age, all_cause_name))
})

#...........................................................
# 02. Load inputs-----
#...........................................................

source("02_load_inputs_indonesia.R")

#...........................................................
# 03. Clean and process inputs-----
#...........................................................

source("03_calibration_indonesia_nelder_mead.R")

#...........................................................
# 04. define interventions ----
#...........................................................

source("04_define_interventions_indonesia.R")

#...........................................................
# 05. build baseline ----
#...........................................................

# Run CVD multiple interventions
setwd(wd_code)
source("05_build_baseline_indonesia.R")

#...........................................................
# 06. Run model ----
#...........................................................

# Run CVD multiple interventions
setwd(wd_code)
source("06_run_scenarios_indonesia_fair.R")

#...........................................................
# 07. Run Burden of Disease ----
#...........................................................

setwd(wd_code)
source("07_output_dalys.R")

#...........................................................
# 08. Run Economic Value ----
#...........................................................
setwd(wd_code)
source("08_economic_value_calculation.R")

#...........................................................
# 09. Cost & Value (costing, budget impact, cost-effectiveness) ----
#...........................................................
setwd(wd_code)
if (run_cost_value) source("09_cost_value.R")

#...........................................................
# 10. Run Validation ----
#...........................................................
setwd(wd_code)
source("10_validation_indonesia.R")
