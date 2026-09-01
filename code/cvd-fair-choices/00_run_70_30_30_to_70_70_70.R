#===========================================================================
# 00_run_70_30_30_to_70_70_70.R
#   ISOLATED runner for the 70-30-30 -> 70-70-70 hypertension/cholesterol +
#   diabetes treatment cascade.
#---------------------------------------------------------------------------
# Feeds the dedicated cascade input workbook
#   data/indonesia_70_30_30_to_70_70_70_inputs.xlsx
# through the SAME Indonesia NCD pipeline (Models 01-09) as the ordinary FAIR
# Choices run, but:
#   * every output lands under output/70_30_30_to_70_70_70/ (nothing overlaps,
#     appends to, or overwrites any ordinary clinical / public-health / combined
#     output);
#   * Model 04 builds a dedicated CASCADE catalogue (baseline + the single
#     scenario S_70_30_30_TO_70_70_70) instead of the clinical FAIR catalogue;
#   * the public-health family is OFF; the clinical machinery carries the cascade.
#
# This runner is standalone: run it in a fresh R session (it starts with
# rm(list=ls())). It NEVER changes how 00_run_model_cvd_fair.R behaves -- that
# runner does not set run_cascade_70_30_30_to_70_70_70, so the flag-guarded
# branches added to Models 04/06/08/09 are inert there.
#
#   source("code/cvd-fair-choices/00_run_70_30_30_to_70_70_70.R")
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
# CASCADE ISOLATION CONFIG  (the only substantive differences vs Model 00) ----
#===========================================================================
# Opt-in cascade flag. This -- and ONLY this -- routes Model 04 to the cascade
# catalogue builder and arms the additive cascade branches in Models 06/09. The
# ordinary runner never sets it, so its behavior is byte-for-byte unchanged.
run_cascade_70_30_30_to_70_70_70 <- TRUE

# Cascade namespace.
cascade_scenario_id <- "S_70_30_30_TO_70_70_70"
cascade_family      <- "cascade_70_30_30_to_70_70_70"

# Isolated INPUT workbook (bespoke cascade format).
model_inputs_file <- file.path(wd, "data", "indonesia_70_30_30_to_70_70_70_inputs.xlsx")
if (!file.exists(model_inputs_file))
  stop("Cascade runner: input workbook not found: ", model_inputs_file, call. = FALSE)

# Isolated OUTPUT root. EVERY Model 06-09 output is redirected here via wd_outp.
cascade_output_dir <- paste0(wd, "output/70_30_30_to_70_70_70")
dir.create(cascade_output_dir,                     recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(cascade_output_dir, "out_model"), recursive = TRUE, showWarnings = FALSE)
wd_outp <- paste0(cascade_output_dir, "/")

# Cascade cost/value workbook base name; Model 09 derives the _formulae edition.
cost_value_output_file <- paste0(wd_outp, "indonesia_70_30_30_to_70_70_70_cost_value.xlsx")

# Intervention-family switches: cascade rides the clinical machinery; PH is OFF.
run_clinical_interventions      <- TRUE
run_public_health_interventions <- FALSE
run_cost_value                  <- TRUE
baseline_scenario_id            <- "baseline"
strict_model_input_validation   <- FALSE

# Public-health paths are never read (PH off) but are referenced by Model 04's
# fallback defaults; point them somewhere harmless so nothing outside the cascade
# directory can ever be created. (These files are NOT written in a PH-off run.)
public_health_inputs_file              <- paste0(wd, "data/indonesia_model_inputs_public_health.xlsx")
public_health_cost_value_formulae_file <- paste0(wd_outp, "UNUSED_public_health_formulae.xlsx")
combined_cost_value_formulae_file      <- paste0(wd_outp, "UNUSED_combined_formulae.xlsx")
tobacco_timing_analysis                <- "base"

#===========================================================================
# FAIL-FAST OUTPUT-COLLISION CHECK  (before any model runs) ----
#---------------------------------------------------------------------------
# Assert every resolved cascade output path is UNDER cascade_output_dir and does
# not equal any ordinary clinical / public-health / combined output path. Also
# snapshot the mtimes of the ordinary outputs so we can prove afterward that the
# cascade run never touched them.
#===========================================================================
.norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
cost_value_formulae_file <- sub("\\.xlsx$", "_formulae.xlsx", cost_value_output_file)

cascade_outputs <- c(
  cost_value_output_file, cost_value_formulae_file,
  paste0(wd_outp, "dt_output_dalys.rds"),
  paste0(wd_outp, "07_life_expectancy_lookup.rds"),
  paste0(wd_outp, "07_disability_weights.rds"),
  paste0(wd_outp, "07_cvd_40q30.rds"),
  paste0(wd_outp, "07_cvd_40q30_age.rds"),
  paste0(wd_outp, "08_vsl_results.rds"),
  paste0(wd_outp, "08_bca_parameters.rds"),
  file.path(cascade_output_dir, "out_model"))

ordinary_outputs <- c(
  paste0(wd, "output/out_model"),
  paste0(wd, "output/dt_output_dalys.rds"),
  paste0(wd, "output/08_vsl_results.rds"),
  paste0(wd, "output/08_bca_parameters.rds"),
  paste0(wd, "output/indonesia_model_cost_value.xlsx"),
  paste0(wd, "output/indonesia_model_cost_value_formulae.xlsx"),
  paste0(wd, "output/indonesia_cost_value_public_health_formulae.xlsx"),
  paste0(wd, "output/indonesia_model_cost_value_clinical_public_health_formulae.xlsx"))

.casc_root <- .norm(cascade_output_dir)
for (f in cascade_outputs) {
  if (!startsWith(.norm(f), .casc_root))
    stop("Cascade collision guard: resolved output '", f,
         "' is NOT under ", cascade_output_dir, ".", call. = FALSE)
  if (.norm(f) %in% .norm(ordinary_outputs))
    stop("Cascade collision guard: resolved output '", f,
         "' collides with an ordinary pipeline output.", call. = FALSE)
}
# Snapshot ordinary-output mtimes (NA if absent) for the after-run comparison.
.ordinary_mtimes_before <- setNames(file.mtime(ordinary_outputs), ordinary_outputs)
cat("Cascade collision guard: OK. All cascade outputs resolve under\n  ",
    cascade_output_dir, "\n", sep = "")

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
# 02-09: reuse the EXISTING model scripts (unchanged behavior) ----
#...........................................................
#source("02_load_inputs_indonesia.R")

#setwd(wd_code); source("03_calibration_indonesia_nelder_mead.R")

# Model 04 -> cascade catalogue (run_cascade flag routes .build_cascade_catalogue)
setwd(wd_code); source("04_define_interventions_indonesia.R")

# After Model 04: assert exactly baseline + the cascade scenario, nothing else.
stopifnot(exists("fair_scenarios"))
.catalog_ids <- names(fair_scenarios)
if (!setequal(.catalog_ids, c(baseline_scenario_id, cascade_scenario_id)))
  stop("Cascade runner: after Model 04 the catalogue must be exactly {",
       baseline_scenario_id, ", ", cascade_scenario_id, "}; got {",
       paste(.catalog_ids, collapse = ", "), "}.", call. = FALSE)
if (!is.null(get0("public_health_scenarios")))
  stop("Cascade runner: public_health_scenarios must be NULL in a cascade run.", call. = FALSE)
if (!is.null(get0("combined_scenarios")))
  stop("Cascade runner: combined_scenarios must be NULL in a cascade run.", call. = FALSE)
cat(sprintf("Cascade Model 04 scope OK: {%s}\n", paste(.catalog_ids, collapse = ", ")))

setwd(wd_code); source("05_build_baseline_indonesia.R")
setwd(wd_code); source("06_run_scenarios_indonesia_fair.R")
setwd(wd_code); source("07_output_dalys.R")
setwd(wd_code); source("08_economic_value_calculation.R")
setwd(wd_code); if (run_cost_value) source("09_cost_value.R")

# NOTE: Model 10 (10_validation_indonesia.R) is a clinical-run validation and is
# intentionally NOT sourced here.

#===========================================================================
# POST-RUN VALIDATION  (isolation, scope, coverage, collision) ----
#===========================================================================
setwd(wd_code)
cat("\n=========================================================\n")
cat("CASCADE POST-RUN VALIDATION\n")
cat("=========================================================\n")

# (1) Every file written is under output/70_30_30_to_70_70_70/.
cascade_files <- list.files(cascade_output_dir, recursive = TRUE, full.names = TRUE)
cat(sprintf("\n[V-isolation] %d file(s) under %s:\n", length(cascade_files), cascade_output_dir))
for (f in sort(cascade_files)) cat("   ", sub(paste0(.norm(wd), "/"), "", .norm(f)), "\n")

# (2) Ordinary outputs untouched (mtime comparison).
.ordinary_mtimes_after <- setNames(file.mtime(ordinary_outputs), ordinary_outputs)
touched_any <- FALSE
for (p in ordinary_outputs) {
  b <- .ordinary_mtimes_before[[p]]; a <- .ordinary_mtimes_after[[p]]
  changed <- !( (is.na(b) && is.na(a)) || (!is.na(b) && !is.na(a) && b == a) )
  if (changed) { touched_any <- TRUE; cat("   [!!] ORDINARY OUTPUT CHANGED:", p, "\n") }
}
cat(sprintf("[V-collision] ordinary clinical/PH/combined outputs changed: %s\n",
            if (touched_any) "YES (PROBLEM)" else "NO (OK)"))
if (touched_any) stop("Cascade runner: an ordinary output was modified -- isolation breached.", call. = FALSE)

# (4) unrounded coverage milestones from the formula workbook. Read the R-source
# `scenario_effective_coverage` column (a real cached number) rather than the
# `model_effective_coverage_used` FORMULA column (which readxl reads as NA until
# Excel recalculates); the two are equal by construction.
if (file.exists(cost_value_formulae_file)) {
  .ct <- tryCatch(as.data.table(readxl::read_excel(cost_value_formulae_file,
                                                   sheet = "Cascade_Trajectory")),
                  error = function(e) NULL)
  if (!is.null(.ct) && all(c("year", "scenario_effective_coverage") %in% names(.ct))) {
    .m30 <- sort(unique(round(.ct[year == 2030, scenario_effective_coverage], 12)))
    .m40 <- sort(unique(round(.ct[year == 2040, scenario_effective_coverage], 12)))
    cat("[V-coverage] 2030 modeled effective coverage (distinct): ",
        paste(sprintf("%.12g", .m30), collapse = ", "), "\n", sep = "")
    cat("[V-coverage] 2040 modeled effective coverage (distinct): ",
        paste(sprintf("%.12g", .m40), collapse = ", "), "\n", sep = "")
    if (!any(abs(.m30 - 0.1365) < 1e-9))
      cat("   [!!] 2030 coverage does not hit 0.1365 exactly\n")
    if (!any(abs(.m40 - 0.4165) < 1e-9))
      cat("   [!!] 2040 coverage does not hit 0.4165 exactly\n")
  }
}
cat(sprintf("[V-workbook] cascade formula workbook: %s (%s)\n",
            cost_value_formulae_file,
            if (file.exists(cost_value_formulae_file)) "written" else "MISSING"))

cat("\nCASCADE RUN COMPLETE.\n")
