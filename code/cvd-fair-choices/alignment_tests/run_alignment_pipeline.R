#===============================================================================
# run_alignment_pipeline.R
#-------------------------------------------------------------------------------
# Reproducible ENTRY POINT + test harness for the isolated Indonesia epidemiology
# alignment pipeline. Runs the whole flow in ONE R session (Models 06 & 09 share
# in-memory objects) WITHOUT triggering the production Models 03/05/06/07-10.
#
#   Rscript code/cvd-fair-choices/alignment_tests/run_alignment_pipeline.R
#
# Stages (each sourced with setwd(wd_code) as Model 00 does):
#   0. bootstrap Model-00 config (+ source 01) via align_bootstrap_config()  [no Model 02]
#   1. 03_calibration_indonesia_alignment.R   (Phase 2 build + flow-inversion)  [skippable]
#   2. 04_define_interventions_indonesia.R    (UNCHANGED production catalogues)
#   3. 05_build_baseline_indonesia_alignment.R
#   4. 06_run_scenarios_indonesia_fair_alignment.R  (-> output_alignment/out_model/)
#   5. validate_baseline_vs_projection.R      (aligned baseline vs projection)
#   6. test_downstream_compat.R               (07/08/09 read the alignment output)
#
# Flags (define before sourcing, or edit here):
#   SKIP_CALIB     TRUE reuses existing adjusted_searo_alignment_*.rds (skip 03)
#   ALIGN_SCENARIOS "smoke" (baseline + 1 clinical + 1 PH; default) | "all"
#
# NON-DESTRUCTIVE: writes only under data/processed/alignment/ and output_alignment/.
# The downstream test redirects Models 07/09 outputs into the alignment tree, so
# production output/ and adjusted_searo_part*.rds are never modified.
#===============================================================================
WD_ROOT <- "C:/Users/wrgar/OneDrive - UW/02Work/WorldBank-Indonesia/uw-wb-indonesia-ncd/"
wd_code_align <- file.path(WD_ROOT, "code", "cvd-fair-choices")
setwd(wd_code_align)
source(file.path(wd_code_align, "alignment_tests", "alignment_helpers.R"))
align_bootstrap_config(WD_ROOT)

if (!exists("SKIP_CALIB"))      SKIP_CALIB      <- FALSE
if (!exists("ALIGN_SCENARIOS")) ALIGN_SCENARIOS <- "smoke"
.t <- function() Sys.time()
.el <- function(t0) round(as.numeric(difftime(.t(), t0, units = "mins")), 2)

cat(sprintf("\n##### ALIGNMENT PIPELINE (SKIP_CALIB=%s, ALIGN_SCENARIOS=%s) #####\n",
            SKIP_CALIB, ALIGN_SCENARIOS))

## 1. Calibration (flow-inversion) --------------------------------------------
adj1 <- file.path(wd_data, "alignment", "adjusted_searo_alignment_part1.rds")
if (isTRUE(SKIP_CALIB) && file.exists(adj1)) {
  cat("\n[1/6] SKIP_CALIB: reusing existing adjusted_searo_alignment_*.rds\n")
} else {
  cat("\n[1/6] 03_calibration_indonesia_alignment.R (Phase 2 + flow-inversion)\n")
  setwd(wd_code_align); t0 <- .t(); source("03_calibration_indonesia_alignment.R")
  cat(sprintf("[1/6] done in %s min\n", .el(t0)))
}

## 2. Interventions (unchanged production) ------------------------------------
cat("\n[2/6] 04_define_interventions_indonesia.R (unchanged)\n")
setwd(wd_code_align); t0 <- .t(); source("04_define_interventions_indonesia.R")
cat(sprintf("[2/6] done in %s min\n", .el(t0)))

## 3. Baseline builder --------------------------------------------------------
cat("\n[3/6] 05_build_baseline_indonesia_alignment.R\n")
setwd(wd_code_align); t0 <- .t(); source("05_build_baseline_indonesia_alignment.R")
cat(sprintf("[3/6] done in %s min\n", .el(t0)))

## 4. Scenario runner ---------------------------------------------------------
cat("\n[4/6] 06_run_scenarios_indonesia_fair_alignment.R\n")
setwd(wd_code_align); t0 <- .t(); source("06_run_scenarios_indonesia_fair_alignment.R")
cat(sprintf("[4/6] done in %s min\n", .el(t0)))

## 5. Baseline vs projection --------------------------------------------------
cat("\n[5/6] validate_baseline_vs_projection.R\n")
setwd(wd_code_align); source("alignment_tests/validate_baseline_vs_projection.R")

## 6. Downstream 07/08/09 compatibility ---------------------------------------
cat("\n[6/6] test_downstream_compat.R (07/08/09 read the alignment output)\n")
setwd(wd_code_align); source("alignment_tests/test_downstream_compat.R")

cat("\n##### ALIGNMENT PIPELINE COMPLETE #####\n")
