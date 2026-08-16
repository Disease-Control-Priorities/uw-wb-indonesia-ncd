################################################################################
# INDONESIA INTEGRATED NCD MODEL — MASTER ORCHESTRATOR
# scripts/00_run_all.R
# ─────────────────────────────────────────────────────────────────────────────
# Sources all pipeline scripts in order. Each script can also be run
# independently after its prerequisites have been completed.
#
# PIPELINE ORDER:
#   01  prepare_gbd_inputs     Raw GBD CSVs → clean disease + PAF tables
#   01b prepare_sbp_rr         Build GBD SBP relative-risk lookup table
#   02  build_demography        wpp2024 package → WPP demographic backbone
#   03  build_cause_fractions   GBD anchor years → projected annual fractions
#   04  calibrate_modules       Fit TPMs for IHD, ischemic stroke, ICH, HHD,
#                                and cervical cancer to GBD 2023 targets
#   05  run_baseline            Project 2025–2100 under no-intervention scenario
#   06  run_scenarios           Project 2025–2100 under 12 intervention scenarios
#   07  make_outputs            Compile summary tables and figures for reporting
#
# STEPS 01–03 are one-time data preparation steps. Re-run only when:
#   - GBD data is updated (new download from vizhub.healthdata.org/gbd-results)
#   - WPP version changes (requires wpp2024 package update)
#
# STEP 04 re-runs whenever the calibration data or methodology changes.
# STEPS 05–07 are run for every model update.
################################################################################

rm(list = ls())

# Libraries
library(here)

# Paths
root_dir <- here::here()
code_dir <- here::here("code")

source(file.path(code_dir, "R", "packages.R"))

message("Project root: ", root_dir)
message("Code dir: ", code_dir)



if (!requireNamespace("here", quietly = TRUE))
  stop("Package 'here' is required. Install with: install.packages('here')", call. = FALSE)
source(here::here("code", "R", "packages.R"))
library(here)   # attach so here() is available without :: throughout

message("════════════════════════════════════════════════════════════════════════")
message("  INDONESIA INTEGRATED NCD MODEL — V1 PIPELINE")
message("════════════════════════════════════════════════════════════════════════")

# ── DATA PREPARATION (run once) ───────────────────────────────────────────────
# Comment these out once data/gbd/ and data/wpp/ outputs exist.
source(file.path(code_dir, "scripts", "01_prepare_gbd_inputs.R"))
source(file.path(code_dir, "scripts", "02_build_demography.R"))
source(file.path(code_dir, "scripts", "03_build_cause_fractions.R"))

# ── CALIBRATION ───────────────────────────────────────────────────────────────
source(file.path(code_dir, "scripts", "04_calibrate_modules.R"))

# ── PROJECTION AND OUTPUTS ────────────────────────────────────────────────────
source(file.path(code_dir, "scripts", "05_run_baseline.R"))
source(file.path(code_dir, "scripts", "06_run_scenarios.R"))
source(file.path(code_dir, "scripts", "07_make_outputs.R"))

message("\n════════════════════════════════════════════════════════════════════════")
message("  Pipeline complete.")
message("  Key outputs:")
message("    data/model/baseline/    — baseline mx, q4030, population objects")
message("    data/model/scenarios/   — scenario mx, deaths averted, q4030")
message("    outputs/tables/         — summary tables for reporting")
message("    outputs/figures/        — charts for slides")
message("════════════════════════════════════════════════════════════════════════")
