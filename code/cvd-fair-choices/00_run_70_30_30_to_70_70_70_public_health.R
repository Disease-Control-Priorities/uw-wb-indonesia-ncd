#===========================================================================
# 00_run_70_30_30_to_70_70_70_public_health.R
#   ISOLATED runner for the 70-30-30 -> 70-70-70 treatment cascade RUN TOGETHER
#   WITH the public-health intervention family (the "join run").
#---------------------------------------------------------------------------
# Feeds the dedicated cascade input workbook
#   data/indonesia_70_30_30_to_70_70_70_inputs.xlsx
# AND the authoritative public-health workbook through Models 01/04-09, producing
# ONE new cost/value workbook that is a DIRECT ANALOGUE of the standard combined
# workbook indonesia_model_cost_value_clinical_public_health_formulae.xlsx -- same
# sheets, styles, columns, live formulas, summary tables, ordering and
# reconciliation -- with the cascade substituted for the clinical arm and a genuine
# JOINT model run (all_cascade_public_health):
#   output/indonesia_model_cost_value_70_30_30_to_70_70_70_public_health_formulae.xlsx
#
# Design (differs from 00_run_70_30_30_to_70_70_70.R):
#   * BOTH families on: the cascade rides the clinical (fair_wb) machinery and the
#     public-health (ph_wb) family runs too. run_clinical_interventions is FALSE
#     so the FAIR *clinical* catalogue is NOT built -- Model 04 routes to the
#     cascade catalogue on the cascade flag alone, and the cascade carries the
#     "clinical" arm. The derived predicate run_cascade_public_health_join (cascade
#     flag AND public-health family) arms the join-only branches in Models 04/06/09.
#   * Model 04 additionally builds the joint scenario `all_cascade_public_health`
#     (cascade effect rows + public-health effect rows applied ONCE EACH in a single
#     Model 06 projection -> effects compound multiplicatively on the rate scale).
#   * Models 02 (load inputs) and 03 (Nelder-Mead calibration) are SKIPPED: Model 05
#     loads the already-calibrated baseline table directly and no downstream model
#     needs an in-memory object from 02/03. A guard fails loudly if any persisted
#     calibration/prep artifact is missing.
#   * EVERY intermediate output lands under output/70_30_30_to_70_70_70_public_health/
#     (isolated). The ONE deliverable written to the canonical output/ directory is
#     the roll-up workbook above. The two SOURCE workbooks it reads -- the ordinary
#     run's public-health workbook and the cascade runner's cascade workbook -- are
#     treated as READ-ONLY (asserted unchanged after the run).
#   * Model 10 (clinical validation) is NOT sourced.
#
# Standalone: run in a fresh R session (starts with rm(list=ls())).
#   source("code/cvd-fair-choices/00_run_70_30_30_to_70_70_70_public_health.R")
#===========================================================================
rm(list = ls())

# libraries (identical to 00_run_model_cvd_fair.R)
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
library(forecast)

wd <- "C:/Users/wrgar/OneDrive - UW/02Work/WorldBank-Indonesia/uw-wb-indonesia-ncd/"

wd_code <- paste0(wd, "code/cvd-fair-choices/")
wd_raw  <- paste0(wd, "data/raw/")
wd_data <- paste0(wd, "data/processed/")

wd_temp <- paste0("C:/Users/wrgar/OneDrive - UW/02Work/WorldBank-Indonesia/", "temp/")
if (!dir.exists(wd_temp)) dir.create(wd_temp, recursive = TRUE)

setwd(wd_code)

#===========================================================================
# JOIN CONFIG  (the only substantive differences vs Model 00) ----
#===========================================================================
# Cascade flag routes Model 04 to the cascade catalogue and (together with the PH
# family) arms the join-only branches in Models 04/06/09.
run_cascade_70_30_30_to_70_70_70 <- TRUE

# Cascade namespace.
cascade_scenario_id <- "S_70_30_30_TO_70_70_70"
cascade_family      <- "cascade_70_30_30_to_70_70_70"

# Isolated cascade INPUT workbook (bespoke cascade format).
model_inputs_file <- file.path(wd, "data", "indonesia_70_30_30_to_70_70_70_inputs.xlsx")
if (!file.exists(model_inputs_file))
  stop("Join runner: cascade input workbook not found: ", model_inputs_file, call. = FALSE)

# Intervention-family switches. BOTH families on. run_clinical_interventions is
# FALSE (the cascade carries the clinical arm; do NOT also build the FAIR clinical
# catalogue). run_public_health_interventions is TRUE -- this is the join run.
run_public_health_interventions  <- TRUE   # PH family ON -- this is the join run
run_clinical_interventions       <- FALSE  # cascade is the "clinical" arm; FAIR catalogue OFF
run_cost_value                   <- TRUE
baseline_scenario_id             <- "baseline"
strict_model_input_validation    <- FALSE

# Public-health analytic workbook: resolve the SAME authoritative contract the
# ordinary runner uses (prefer the mortality-updated edition). Reuses the ordinary
# runner's .ph_candidates fallback logic.
.ph_candidates <- paste0(wd, c(
  "data/indonesia_model_inputs_public_health_updated_mortality.xlsx",  # authoritative (mortality)
  "data/indonesia_model_inputs_public_health_updated.xlsx",            # previous updated
  "data/indonesia_model_inputs_public_health.xlsx"))                   # original
public_health_inputs_file <- .ph_candidates[file.exists(.ph_candidates)][1]
if (is.na(public_health_inputs_file))
  stop("Join runner: no public-health input workbook found; expected one of:\n  ",
       paste(.ph_candidates, collapse = "\n  "), call. = FALSE)
rm(.ph_candidates)
tobacco_timing_analysis <- "base"

# Derived predicate (must equal what Models 04/06/09 recompute from the two flags).
run_cascade_public_health_join <-
  isTRUE(run_cascade_70_30_30_to_70_70_70) && isTRUE(run_public_health_interventions)
if (!isTRUE(run_cascade_public_health_join))
  stop("Join runner: run_cascade_public_health_join must be TRUE here.", call. = FALSE)

#===========================================================================
# OUTPUT ISOLATION  ----
#---------------------------------------------------------------------------
# Every Model 06-09 intermediate is redirected to an OWN directory. The ONE
# deliverable written to canonical output/ is the roll-up workbook.
#===========================================================================
join_output_dir <- paste0(wd, "output/70_30_30_to_70_70_70_public_health")
dir.create(join_output_dir,                     recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(join_output_dir, "out_model"), recursive = TRUE, showWarnings = FALSE)
wd_outp <- paste0(join_output_dir, "/")

# Isolated Model 09 workbook paths (all under wd_outp). run_clinical = FALSE means
# the R-value + clinical/combined workbooks are NOT written; the PH workbook IS.
cost_value_output_file                 <- paste0(wd_outp, "indonesia_model_cost_value.xlsx")
cost_value_formulae_file               <- paste0(wd_outp, "indonesia_model_cost_value_formulae.xlsx")
public_health_cost_value_formulae_file <- paste0(wd_outp, "indonesia_cost_value_public_health_formulae.xlsx")
combined_cost_value_formulae_file      <- paste0(wd_outp, "UNUSED_combined_formulae.xlsx")

# THE deliverable -- canonical output/ path (never under wd_outp).
combined_cost_value_70_30_30_public_health_formulae_file <- paste0(
  wd, "output/indonesia_model_cost_value_70_30_30_to_70_70_70_public_health_formulae.xlsx")

#===========================================================================
# FAIL-FAST GUARD (a): SKIP OF MODELS 02 / 03 ----
#---------------------------------------------------------------------------
# Models 02/03 are NOT sourced. Model 05 loads the already-calibrated baseline
# table directly and reads several 02/03-produced artifacts from data/processed/
# whose CONTENT it then discards (b_rates is fully replaced by the reconciled
# table). No downstream model references an in-memory 02/03 object. Assert every
# such persisted artifact exists; if any is missing, STOP and tell the user to run
# the calibration once (via the ordinary or cascade runner, which source 02/03).
#===========================================================================
.req_artifacts <- c(
  paste0(wd_data, "wpp.adj.Rda"),
  paste0(wd_data, "PopulationsSingleAge0050.rds"),
  paste0(wd_data, "PopulationsAge20_2050.csv"),
  paste0(wd_data, "bp_data6.csv"),
  paste0(wd_data, "covfxn2.csv"),
  paste0(wd_data, "tps_bgmx_forecasted.rds"),
  paste0(wd_data, "tps_bgmx_all_forecasted.rds"),
  paste0(wd_data, "tps_bgmx_cvd_forecasted.rds"),
  paste0(wd_data, "b_rates_full_period_reconciled_2017_2050_national_current38.rds"))
.missing_art <- .req_artifacts[!file.exists(.req_artifacts)]
if (!length(list.files(wd_data, pattern = "adjusted")))
  .missing_art <- c(.missing_art, paste0(wd_data, "adjusted_searo_part*.rds (calibration output)"))
if (length(.missing_art))
  stop("Join runner: Models 02/03 are skipped but these prerequisite artifacts are ",
       "missing from data/processed/:\n  - ", paste(.missing_art, collapse = "\n  - "),
       "\nRun the calibration once first (e.g. source the ordinary runner ",
       "00_run_model_cvd_fair.R or the cascade runner 00_run_70_30_30_to_70_70_70.R, ",
       "which source Models 02 and 03), then re-run this join runner.", call. = FALSE)
cat("Join runner: 02/03-skip prerequisite artifacts OK (all present in data/processed/).\n")
rm(.req_artifacts, .missing_art)

# NOTE: this join run is SELF-CONTAINED. Model 09's cascade+public-health combined
# workbook (Section 15) is a direct analogue of the standard combined workbook,
# built by source_combined_cost_value() from THIS run's own Models 04-08 -- it does
# NOT read the ordinary run's or the cascade runner's output workbooks. Those
# canonical workbooks are still asserted UNTOUCHED by the collision guard below.
.casc_src_wb <- paste0(wd, "output/70_30_30_to_70_70_70/indonesia_70_30_30_to_70_70_70_cost_value_formulae.xlsx")
.casc_cv_wb  <- paste0(wd, "output/70_30_30_to_70_70_70/indonesia_70_30_30_to_70_70_70_cost_value.xlsx")

#===========================================================================
# FAIL-FAST OUTPUT-COLLISION CHECK  (before any model runs) ----
#---------------------------------------------------------------------------
# Assert every resolved join intermediate is UNDER join_output_dir, that the one
# canonical deliverable is exactly the expected output/ file, and snapshot the
# ordinary + cascade outputs so we can prove afterward they were never touched.
#===========================================================================
.norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
join_root <- paste0(.norm(join_output_dir), "/")

join_intermediates <- c(
  public_health_cost_value_formulae_file,
  paste0(wd_outp, "dt_output_dalys.rds"),
  paste0(wd_outp, "07_cvd_40q30.rds"),
  paste0(wd_outp, "08_vsl_results.rds"),
  paste0(wd_outp, "08_bca_parameters.rds"),
  paste0(wd_outp, "09_deck_results.rds"),
  file.path(join_output_dir, "out_model"))
for (f in join_intermediates)
  if (!startsWith(.norm(f), join_root))
    stop("Join collision guard: resolved intermediate '", f, "' is NOT under ",
         join_output_dir, ".", call. = FALSE)

# The deliverable must be the canonical output/ path and NOT under join_output_dir.
.deliverable_expected <- .norm(paste0(wd,
  "output/indonesia_model_cost_value_70_30_30_to_70_70_70_public_health_formulae.xlsx"))
if (.norm(combined_cost_value_70_30_30_public_health_formulae_file) != .deliverable_expected)
  stop("Join collision guard: deliverable path mis-resolved.", call. = FALSE)
if (startsWith(.deliverable_expected, join_root))
  stop("Join collision guard: deliverable must live in output/, not under the join dir.",
       call. = FALSE)

# Ordinary + cascade outputs that must remain UNTOUCHED (mtime snapshot).
protected_outputs <- c(
  paste0(wd, "output/out_model"),
  paste0(wd, "output/dt_output_dalys.rds"),
  paste0(wd, "output/08_vsl_results.rds"),
  paste0(wd, "output/indonesia_model_cost_value.xlsx"),
  paste0(wd, "output/indonesia_model_cost_value_formulae.xlsx"),
  paste0(wd, "output/indonesia_cost_value_public_health_formulae.xlsx"),
  paste0(wd, "output/indonesia_model_cost_value_clinical_public_health_formulae.xlsx"),
  .casc_src_wb, .casc_cv_wb)
.protected_mtimes_before <- setNames(file.mtime(protected_outputs), protected_outputs)
cat("Join collision guard: OK. Intermediates resolve under\n  ", join_output_dir,
    "\n  Deliverable: ", combined_cost_value_70_30_30_public_health_formulae_file, "\n", sep = "")

#...........................................................
# 0. Functions and parameters (identical to Model 00) -----
#...........................................................
source("01_utils_indonesia.R")

run_calibration_par  <- TRUE
run_adjustment_model <- FALSE
run_aod_par          <- FALSE
run_adjustments_inputs <- FALSE
run_bgmx_trend <- TRUE
run_CF_trend   <- TRUE
run_CF_trend_80   <- TRUE
run_CF_trend_ihme <- FALSE

## --- Causes (identical to Model 00) ---------------------------------------
cause_map <- c(
  ihd      = "Ischemic heart disease",
  istroke  = "Ischemic stroke",
  hstroke  = "Intracerebral hemorrhage",
  hhd      = "Hypertensive heart disease",
  rhd      = "Rheumatic heart disease",
  cmd      = "Cardiomyopathy and myocarditis",
  dm2      = "Diabetes mellitus type 2",
  all      = "All causes"
)
all_cause_code   <- "all"
all_cause_name   <- unname(cause_map[[all_cause_code]])
model_cause_codes <- setdiff(names(cause_map), all_cause_code)
model_cause_names <- unname(cause_map[model_cause_codes])
dx_include <- unname(cause_map)
cause_cols <- names(cause_map)

cvd_40q30_cause_codes <- c("ihd", "istroke", "hstroke", "hhd", "rhd", "cmd")
local({
  if (length(cvd_40q30_cause_codes) != 6L)
    stop("cvd_40q30_cause_codes must contain exactly six CVD cause codes.")
  miss <- setdiff(cvd_40q30_cause_codes, names(cause_map))
  if (length(miss)) stop("cvd_40q30_cause_codes references code(s) absent from cause_map: ",
                         paste(miss, collapse = ", "))
})

min_model_age <- 0L
max_model_age <- 95L
age_single    <- min_model_age:max_model_age

local({
  if (anyDuplicated(names(cause_map))) stop("cause_map has duplicate short codes.")
  if (!(all_cause_code %in% names(cause_map))) stop("cause_map missing all-cause envelope.")
  bands   <- gbd_age_bands(min_model_age, max_model_age)
  covered <- unlist(Map(seq, bands$age_lo, pmin(bands$age_hi, max_model_age)))
  if (!setequal(covered, age_single)) stop("gbd_age_bands() does not cover age_single once.")
  cat(sprintf("Config OK: %d modeled causes (%s) + all-cause; ages %d-%d.\n",
              length(model_cause_codes), paste(model_cause_codes, collapse = ", "),
              min_model_age, max_model_age))
})

#...........................................................
# 04-09: reuse the EXISTING model scripts (behavior guarded by the flags) ----
#   NOTE: Models 02 (load inputs) and 03 (calibration) are DELIBERATELY SKIPPED
#   (see FAIL-FAST GUARD (a) above). Model 05 loads the reconciled baseline table.
#...........................................................

# Model 04 -> cascade catalogue + public-health catalogue + joint all_cascade_public_health.
setwd(wd_code); source("04_define_interventions_indonesia.R")

# After Model 04: assert the join-run scope (relaxed vs the plain cascade runner).
stopifnot(exists("fair_scenarios"))
.catalog_ids <- names(fair_scenarios)
if (!setequal(.catalog_ids, c(baseline_scenario_id, cascade_scenario_id)))
  stop("Join runner: after Model 04 fair_scenarios must be exactly {",
       baseline_scenario_id, ", ", cascade_scenario_id, "}; got {",
       paste(.catalog_ids, collapse = ", "), "}.", call. = FALSE)
if (is.null(get0("public_health_scenarios")))
  stop("Join runner: public_health_scenarios must be non-NULL in a join run.", call. = FALSE)
if (is.null(get0("combined_scenarios")) ||
    !all(c("all_cascade_public_health", "S_70_30_30_TO_70_70_70_I_PH_TOBACCO_TAX") %in% names(combined_scenarios)))
  stop("Join runner: combined_scenarios must contain {all_cascade_public_health, ",
       "S_70_30_30_TO_70_70_70_I_PH_TOBACCO_TAX}; got {",
       paste(names(get0("combined_scenarios")), collapse = ", "), "}.", call. = FALSE)
cat(sprintf("Join Model 04 scope OK: fair {%s} | %d PH scenario(s) | joint {%s}\n",
            paste(.catalog_ids, collapse = ", "), length(public_health_scenarios),
            paste(names(combined_scenarios), collapse = ", ")))
rm(.catalog_ids)

setwd(wd_code); source("05_build_baseline_indonesia.R")
setwd(wd_code); source("06_run_scenarios_indonesia_fair.R")
setwd(wd_code); source("07_output_dalys.R")
setwd(wd_code); source("08_economic_value_calculation.R")
setwd(wd_code); if (run_cost_value) source("09_cost_value.R")

# NOTE: Model 10 (10_validation_indonesia.R) is intentionally NOT sourced.

#===========================================================================
# POST-RUN VALIDATION  (isolation, scope, collision, deliverable) ----
#===========================================================================
setwd(wd_code)
cat("\n=========================================================\n")
cat("CASCADE + PUBLIC-HEALTH JOIN POST-RUN VALIDATION\n")
cat("=========================================================\n")

# (1) Every intermediate file written is under the join dir.
join_files <- list.files(join_output_dir, recursive = TRUE, full.names = TRUE)
cat(sprintf("\n[V-isolation] %d file(s) under %s:\n", length(join_files), join_output_dir))
for (f in sort(join_files)) cat("   ", sub(paste0(.norm(wd), "/"), "", .norm(f)), "\n")

# (2) The deliverable exists at the canonical output/ path.
.deliverable <- combined_cost_value_70_30_30_public_health_formulae_file
cat(sprintf("[V-deliverable] %s (%s)\n", .deliverable,
            if (file.exists(.deliverable)) "written" else "MISSING"))
if (!file.exists(.deliverable))
  stop("Join runner: the deliverable roll-up workbook was not written.", call. = FALSE)

# (3) Protected ordinary + cascade outputs untouched (mtime comparison).
.protected_mtimes_after <- setNames(file.mtime(protected_outputs), protected_outputs)
touched_any <- FALSE
for (p in protected_outputs) {
  b <- .protected_mtimes_before[[p]]; a <- .protected_mtimes_after[[p]]
  changed <- !( (is.na(b) && is.na(a)) || (!is.na(b) && !is.na(a) && b == a) )
  if (changed) { touched_any <- TRUE; cat("   [!!] PROTECTED OUTPUT CHANGED:", p, "\n") }
}
cat(sprintf("[V-collision] protected ordinary/cascade outputs changed: %s\n",
            if (touched_any) "YES (PROBLEM)" else "NO (OK)"))
if (touched_any) stop("Join runner: a protected output was modified -- isolation breached.",
                      call. = FALSE)

cat("\nCASCADE + PUBLIC-HEALTH JOIN RUN COMPLETE.\n")
