#===========================================================================
# 00_run_70_30_30_to_70_70_70_subnational.R
#   STAND-ALONE, ISOLATED runner for the SUBNATIONAL (38-province) 70-30-30 ->
#   70-70-70 hypertension/cholesterol + type-2-diabetes treatment cascade.
#---------------------------------------------------------------------------
# WHAT THIS IS
#   A completely self-contained province-level analysis. It reuses the analytic
#   LOGIC of the national FAIR-Choices cascade (Models 04-09) but as ADAPTED
#   COPIES living entirely in this directory. It NEVER sources any production
#   `code/cvd-fair-choices/00-09*.R` script at runtime, and it NEVER runs Model
#   01 (utils), Model 02 (load inputs), or Model 03 (calibration).
#
# DELIBERATE NUMBERING GAP (no 01/02/03) -- see README.md
#   The reconciled province rate table
#     data/processed/b_rates_full_period_reconciled_2017_2050_national_current38.rds
#   is the FINAL, already-prepared and already-calibrated model-rate input. It
#   incorporates the data preparation that would otherwise have happened in
#   Models 01/02 and the calibration that would otherwise have happened in Model
#   03. Therefore this runner loads it DIRECTLY and begins the analytical
#   pipeline at the adapted Model 04 stage. Re-running 01/02/03 here is neither
#   required nor permitted.
#
# SCENARIOS (per province)
#   baseline  and  S_70_30_30_TO_70_70_70  (a single COMBINED cascade scenario
#   carrying the two component interventions I_CVD_PRIMARY + I_T2D_TREATMENT --
#   never emitted as separate policy scenarios).
#
# ISOLATION
#   * Everything is written ONLY under output/70_30_30_to_70_70_70_subnational/.
#   * A fail-fast collision guard (below) asserts every resolved output path is
#     under that directory and equals no ordinary production output, and snapshots
#     the ordinary outputs' mtimes so the post-run validation proves none was
#     touched.
#   * run_public_health_interventions = FALSE (clinical machinery carries the
#     cascade; no PH / combined workbook is built).
#
#   Run from a fresh R session, from the repository root:
#     source("code/cvd-fair-choices/70_30_30_to_70_70_70_subnational/00_run_70_30_30_to_70_70_70_subnational.R")
#   or:
#     Rscript code/cvd-fair-choices/70_30_30_to_70_70_70_subnational/00_run_70_30_30_to_70_70_70_subnational.R
#===========================================================================
rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(openxlsx)
})

#---------------------------------------------------------------------------
# 0. Repository-root detection (NO hard-coded Windows/OneDrive paths) --------
#---------------------------------------------------------------------------
# Walk up from a set of candidate starting points until the RStudio project
# marker `uw-wb-indonesia-ncd.Rproj` is found. Works whether the runner is
# `source()`d from the repo root or launched via `Rscript <path>` from anywhere.
.find_repo_root <- function() {
  marker <- "uw-wb-indonesia-ncd.Rproj"
  cands <- character(0)
  # (a) explicit override
  env <- Sys.getenv("UW_WB_INDONESIA_NCD_ROOT", unset = NA_character_)
  if (!is.na(env) && nzchar(env)) cands <- c(cands, env)
  # (b) current working directory
  cands <- c(cands, tryCatch(getwd(), error = function(e) NULL))
  # (c) this script's own location (when run via Rscript)
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) cands <- c(cands, dirname(normalizePath(f[1], winslash = "/", mustWork = FALSE)))
  # (d) the sourced file path, if available
  if (!is.null(sys.frames()) && length(sys.calls())) {
    sf <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
                   error = function(e) NULL)
    if (!is.null(sf) && nzchar(sf)) cands <- c(cands, dirname(sf))
  }
  walk_up <- function(start) {
    d <- normalizePath(start, winslash = "/", mustWork = FALSE)
    for (i in 1:12) {
      if (file.exists(file.path(d, marker))) return(d)
      p <- dirname(d); if (identical(p, d)) break; d <- p
    }
    NA_character_
  }
  for (c0 in cands) {
    if (is.null(c0) || !nzchar(c0)) next
    r <- walk_up(c0)
    if (!is.na(r)) return(r)
  }
  stop("Subnational runner: could not locate the repository root (marker '", marker,
       "'). Run from inside the repo, or set env var UW_WB_INDONESIA_NCD_ROOT.",
       call. = FALSE)
}

repo_root <- .find_repo_root()
wd      <- paste0(repo_root, "/")
wd_sub  <- paste0(wd, "code/cvd-fair-choices/70_30_30_to_70_70_70_subnational/")  # NEW scripts
wd_raw  <- paste0(wd, "data/raw/")
wd_data <- paste0(wd, "data/processed/")
wd_temp <- paste0(dirname(repo_root), "/temp/")
if (!dir.exists(wd_temp)) dir.create(wd_temp, recursive = TRUE, showWarnings = FALSE)
cat("Subnational runner: repository root detected at\n  ", repo_root, "\n", sep = "")

#---------------------------------------------------------------------------
# 1. Cascade + subnational isolation config ---------------------------------
#---------------------------------------------------------------------------
# Opt-in cascade flag (routes the adapted Model 04 to the cascade catalogue and
# arms the cascade branches in the adapted Models 06/09). NEW subnational flag
# distinguishes province logic from the national cascade.
run_cascade_70_30_30_to_70_70_70 <- TRUE
run_subnational                  <- TRUE

cascade_scenario_id <- "S_70_30_30_TO_70_70_70"
cascade_family      <- "cascade_70_30_30_to_70_70_70_subnational"
baseline_scenario_id <- "baseline"

# Isolated INPUT workbook (province-extended bespoke cascade format).
model_inputs_file <- file.path(wd, "data", "indonesia_70_30_30_to_70_70_70_inputs_subnational.xlsx")
if (!file.exists(model_inputs_file))
  stop("Subnational runner: input workbook not found: ", model_inputs_file, call. = FALSE)

# The authoritative reconciled province rate table (final, prepared+calibrated).
b_rates_file <- file.path(wd, "data", "processed",
                          "b_rates_full_period_reconciled_2017_2050_national_current38.rds")

# Isolated OUTPUT root. EVERY adapted Model 06-09 output is redirected here.
subnational_output_dir <- paste0(wd, "output/70_30_30_to_70_70_70_subnational")
dir.create(subnational_output_dir,                       recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(subnational_output_dir, "out_model"), recursive = TRUE, showWarnings = FALSE)
wd_outp <- paste0(subnational_output_dir, "/")

# Cost/value workbook: the base (R-value) name and the REQUIRED formulae name.
cost_value_output_file   <- paste0(wd_outp, "indonesia_70_30_30_to_70_70_70_cost_value_subnational.xlsx")
# Deliverable filename is fixed by the task (note _formulae_subnational, not _subnational_formulae).
cost_value_formulae_file <- paste0(wd_outp,
  "indonesia_70_30_30_to_70_70_70_cost_value_formulae_subnational.xlsx")

# Intervention-family switches: cascade rides the clinical machinery; PH is OFF.
run_clinical_interventions      <- TRUE
run_public_health_interventions <- FALSE
run_cost_value                  <- TRUE
strict_model_input_validation   <- FALSE

# Model 06 parallel cores (province jobs fan out across these).
n_cores <- max(1L, min(6L, parallel::detectCores() - 1L))

#---------------------------------------------------------------------------
# 2. FAIL-FAST OUTPUT-COLLISION GUARD (before any model runs) ----------------
#---------------------------------------------------------------------------
.norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

subnational_outputs <- c(
  cost_value_output_file, cost_value_formulae_file,
  paste0(wd_outp, "dt_output_dalys.rds"),
  paste0(wd_outp, "07_life_expectancy_lookup.rds"),
  paste0(wd_outp, "07_disability_weights.rds"),
  paste0(wd_outp, "07_cvd_40q30.rds"),
  paste0(wd_outp, "07_cvd_40q30_age.rds"),
  paste0(wd_outp, "08_vsl_results.rds"),
  paste0(wd_outp, "08_bca_parameters.rds"),
  file.path(subnational_output_dir, "out_model"))

# Ordinary production outputs that MUST remain untouched (clinical, PH, combined,
# AND the national cascade). We snapshot mtimes+sizes before the run.
ordinary_outputs <- c(
  paste0(wd, "output/out_model"),
  paste0(wd, "output/dt_output_dalys.rds"),
  paste0(wd, "output/08_vsl_results.rds"),
  paste0(wd, "output/08_bca_parameters.rds"),
  paste0(wd, "output/indonesia_model_cost_value.xlsx"),
  paste0(wd, "output/indonesia_model_cost_value_formulae.xlsx"),
  paste0(wd, "output/indonesia_cost_value_public_health_formulae.xlsx"),
  paste0(wd, "output/indonesia_model_cost_value_clinical_public_health_formulae.xlsx"),
  paste0(wd, "output/70_30_30_to_70_70_70"))   # the NATIONAL cascade output tree

.sub_root  <- .norm(subnational_output_dir)
for (f in subnational_outputs) {
  if (!startsWith(.norm(f), .sub_root))
    stop("Subnational collision guard: resolved output '", f,
         "' is NOT under ", subnational_output_dir, ".", call. = FALSE)
  if (.norm(f) %in% .norm(ordinary_outputs))
    stop("Subnational collision guard: resolved output '", f,
         "' collides with an ordinary pipeline output.", call. = FALSE)
}
# The subnational dir must not sit inside any ordinary output path either.
for (o in ordinary_outputs) {
  if (o != paste0(wd, "output/70_30_30_to_70_70_70") &&
      startsWith(.sub_root, .norm(o)) && .norm(o) != .norm(paste0(wd, "output")))
    stop("Subnational collision guard: output dir is inside ordinary output '", o, "'.", call. = FALSE)
}
.snapshot <- function(paths) {
  out <- list()
  for (p in paths) {
    if (dir.exists(p)) {
      ff <- list.files(p, recursive = TRUE, full.names = TRUE)
      out[[p]] <- data.frame(file = ff, mtime = as.numeric(file.mtime(ff)),
                             size = file.size(ff), stringsAsFactors = FALSE)
    } else if (file.exists(p)) {
      out[[p]] <- data.frame(file = p, mtime = as.numeric(file.mtime(p)),
                             size = file.size(p), stringsAsFactors = FALSE)
    } else out[[p]] <- data.frame(file = character(0), mtime = numeric(0),
                                  size = numeric(0), stringsAsFactors = FALSE)
  }
  out
}
.ordinary_before <- .snapshot(ordinary_outputs)
cat("Subnational collision guard: OK. All outputs resolve under\n  ",
    subnational_output_dir, "\n", sep = "")

#---------------------------------------------------------------------------
# 3. Model config (causes / ages) -- copied from the FAIR runner -------------
#   (config constants only; NOT the Model 01 utility stage)
#---------------------------------------------------------------------------
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
all_cause_code    <- "all"
all_cause_name    <- unname(cause_map[[all_cause_code]])
model_cause_codes <- setdiff(names(cause_map), all_cause_code)
model_cause_names <- unname(cause_map[model_cause_codes])
cause_cols        <- names(cause_map)

cvd_40q30_cause_codes <- c("ihd", "istroke", "hstroke", "hhd", "rhd", "cmd")

min_model_age <- 0L
max_model_age <- 95L
age_single    <- min_model_age:max_model_age

# Minimal inlined age-band helper (from 01_utils_indonesia.R) -- used ONLY by the
# config self-check below. Copied verbatim; NOT a re-created Model 01 stage.
gbd_age_bands <- function(min_age = 0L, max_age = 95L) {
  bands <- data.table::data.table(
    label  = c("<1 year", "12-23 months", "2-4 years",
               "5-9 years", "10-14 years", "15-19 years",
               "20-24 years","25-29 years","30-34 years","35-39 years",
               "40-44 years","45-49 years","50-54 years","55-59 years",
               "60-64 years","65-69 years","70-74 years","75-79 years",
               "80-84 years","85-89 years","90-94 years","95+ years"),
    age_lo = c(0L, 1L, 2L, 5L, 10L, 15L, seq(20L, 90L, by = 5L), 95L),
    age_hi = c(0L, 1L, 4L, 9L, 14L, 19L, seq(24L, 94L, by = 5L), Inf),
    midpt  = c(0, 1, 3, 7, 12, 17, seq(22, 92, by = 5), 95))
  bands <- bands[age_hi >= min_age & age_lo <= max_age]
  bands[age_hi > max_age, age_hi := max_age]
  bands[age_lo < min_age, age_lo := min_age]
  bands[]
}
local({
  if (anyDuplicated(names(cause_map))) stop("cause_map has duplicate short codes.")
  if (!(all_cause_code %in% names(cause_map))) stop("cause_map missing all-cause envelope.")
  if (length(cvd_40q30_cause_codes) != 6L) stop("cvd_40q30_cause_codes must be six CVD codes.")
  bands   <- gbd_age_bands(min_model_age, max_model_age)
  covered <- unlist(Map(seq, bands$age_lo, pmin(bands$age_hi, max_model_age)))
  if (!setequal(covered, age_single)) stop("gbd_age_bands() does not cover age_single once.")
  cat(sprintf("Config OK: %d modeled causes (%s) + all-cause; ages %d-%d.\n",
              length(model_cause_codes), paste(model_cause_codes, collapse = ", "),
              min_model_age, max_model_age))
})

#---------------------------------------------------------------------------
# 4. Load the FINAL prepared/calibrated province rate table DIRECTLY ----------
#   (this replaces Models 01/02/03; no calibration is run)
#---------------------------------------------------------------------------
if (!file.exists(b_rates_file))
  stop("Subnational runner: reconciled province b_rates file not found:\n  ", b_rates_file,
       "\n  This exact prepared/calibrated file is required; do NOT substitute a national-only ",
       "file or invoke calibration.", call. = FALSE)

# Safe, explicit loader. The task path uses a `.R` extension but the serialized
# artifact is an RDS (same basename); load robustly and inspect, rather than
# trusting the extension.
.load_b_rates <- function(path) {
  obj <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(obj, "error"))
    stop("Subnational runner: b_rates file exists but is not a readable RDS: ",
         conditionMessage(obj), call. = FALSE)
  if (is.list(obj) && !is.data.frame(obj)) {
    # If a list, take the first data-frame-like element.
    dfi <- which(vapply(obj, is.data.frame, logical(1)))
    if (!length(dfi)) stop("Subnational runner: b_rates RDS is a list with no data.frame element.")
    obj <- obj[[dfi[1]]]
  }
  as.data.table(obj)
}
b_rates <- .load_b_rates(b_rates_file)

# Fail-fast province-data contract on the loaded table.
.req_cols <- c("location", "year", "age", "sex", "cause", "Nx", "IR", "CF", "BG.mx")
.miss <- setdiff(.req_cols, names(b_rates))
if (length(.miss))
  stop("Subnational runner: b_rates missing required column(s): ", paste(.miss, collapse = ", "),
       ".\n  This file does not carry the model dimensions required by the baseline engine.",
       call. = FALSE)
.locs_all <- sort(unique(as.character(b_rates$location)))
.provs_in_rates <- setdiff(.locs_all, "Indonesia")
if (length(.provs_in_rates) < 2L)
  stop("Subnational runner: b_rates contains no usable province data (locations found: ",
       paste(.locs_all, collapse = ", "), "). Refusing to proceed on a national-only file.",
       call. = FALSE)
cat(sprintf("Loaded b_rates: %s rows, %d locations (%d provinces + %s), causes {%s}, years %d-%d.\n",
            format(nrow(b_rates), big.mark = ","), length(.locs_all), length(.provs_in_rates),
            if ("Indonesia" %in% .locs_all) "Indonesia" else "no national row",
            paste(sort(unique(b_rates$cause)), collapse = ", "),
            min(b_rates$year), max(b_rates$year)))

#---------------------------------------------------------------------------
# 5. Source ONLY the new subnational 04-09 scripts (NEVER production) ---------
#---------------------------------------------------------------------------
# Explicit absolute paths; no setwd() dependency. There is deliberately NO
# subnational 01/02/03 to source.
source(file.path(wd_sub, "04_define_interventions_70_30_30_to_70_70_70_subnational.R"))

# After Model 04: the catalogue must be exactly {baseline, cascade}.
stopifnot(exists("fair_scenarios"))
.catalog_ids <- names(fair_scenarios)
if (!setequal(.catalog_ids, c(baseline_scenario_id, cascade_scenario_id)))
  stop("Subnational runner: after Model 04 the catalogue must be exactly {",
       baseline_scenario_id, ", ", cascade_scenario_id, "}; got {",
       paste(.catalog_ids, collapse = ", "), "}.", call. = FALSE)
if (!is.null(get0("public_health_scenarios")))
  stop("Subnational runner: public_health_scenarios must be NULL.", call. = FALSE)
cat(sprintf("Subnational Model 04 scope OK: {%s}\n", paste(.catalog_ids, collapse = ", ")))

source(file.path(wd_sub, "05_build_baseline_70_30_30_to_70_70_70_subnational.R"))
source(file.path(wd_sub, "06_run_scenarios_70_30_30_to_70_70_70_subnational.R"))
source(file.path(wd_sub, "07_output_dalys_70_30_30_to_70_70_70_subnational.R"))
source(file.path(wd_sub, "08_economic_value_70_30_30_to_70_70_70_subnational.R"))
if (run_cost_value)
  source(file.path(wd_sub, "09_cost_value_70_30_30_to_70_70_70_subnational.R"))

#---------------------------------------------------------------------------
# 6. POST-RUN VALIDATION (isolation, scope, collision) -----------------------
#---------------------------------------------------------------------------
cat("\n=========================================================\n")
cat("SUBNATIONAL CASCADE POST-RUN VALIDATION\n")
cat("=========================================================\n")

# (1) Every file written is under output/70_30_30_to_70_70_70_subnational/.
sub_files <- list.files(subnational_output_dir, recursive = TRUE, full.names = TRUE)
cat(sprintf("\n[V-isolation] %d file(s) under %s\n", length(sub_files), subnational_output_dir))

# (2) Ordinary outputs untouched (mtime+size comparison against the snapshot).
.ordinary_after <- .snapshot(ordinary_outputs)
touched_any <- FALSE
for (p in ordinary_outputs) {
  b <- .ordinary_before[[p]]; a <- .ordinary_after[[p]]
  merged <- merge(b, a, by = "file", all = TRUE, suffixes = c(".b", ".a"))
  chg <- merged[ which(
    is.na(merged$mtime.b) != is.na(merged$mtime.a) |
      (!is.na(merged$mtime.b) & !is.na(merged$mtime.a) &
         (merged$mtime.b != merged$mtime.a | merged$size.b != merged$size.a)) ), , drop = FALSE]
  if (nrow(chg) > 0) { touched_any <- TRUE
    for (fp in chg$file) cat("   [!!] ORDINARY OUTPUT CHANGED:", fp, "\n") }
}
cat(sprintf("[V-collision] ordinary clinical/PH/combined/national-cascade outputs changed: %s\n",
            if (touched_any) "YES (PROBLEM)" else "NO (OK)"))
if (touched_any)
  stop("Subnational runner: an ordinary output was modified -- isolation breached.", call. = FALSE)

# (3) The deliverable formula workbook exists.
cat(sprintf("[V-workbook] subnational formula workbook: %s (%s)\n",
            cost_value_formulae_file,
            if (file.exists(cost_value_formulae_file)) "written" else "MISSING"))
if (!file.exists(cost_value_formulae_file))
  stop("Subnational runner: the required formula workbook was not written.", call. = FALSE)

cat("\nSUBNATIONAL CASCADE RUN COMPLETE.\n")
