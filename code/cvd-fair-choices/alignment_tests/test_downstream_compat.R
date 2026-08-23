#===============================================================================
# test_downstream_compat.R -- prove Models 07/08/09 consume the alignment Model 06
# output WITHOUT modification, non-destructively (production output/ untouched).
#
# Prerequisites in memory (the harness / a runner provides them):
#   * Model-00 config (wd, wd_data, wd_raw, wd_outp, wd_code, cause_map, ...)
#   * Model 04 catalogues (fair_inputs, fair_scenarios, public_health_*), which
#     Model 09 requires -- source 04 before this.
#   * The alignment Model 06 output on disk at <wd>/output_alignment/out_model/.
# Reads production output/out_model/ only to compare schemas (never writes there).
#===============================================================================
suppressWarnings(suppressMessages(library(data.table)))
stopifnot(exists("wd"), exists("wd_outp"), exists("wd_code"))

PASS <- 0L; FAILS <- character(0)
ok <- function(cond, msg) {
  if (isTRUE(cond)) { PASS <<- PASS + 1L; cat(sprintf("  PASS  %s\n", msg)) }
  else { FAILS <<- c(FAILS, msg); cat(sprintf("  FAIL  %s\n", msg)) }
}
align_out <- file.path(wd, "output_alignment")
align_mo_dir <- file.path(align_out, "out_model")
align_files <- list.files(align_mo_dir, pattern = "^model_output_.*\\.rds$", full.names = TRUE)
prod_files  <- list.files(file.path(wd, "output", "out_model"),
                          pattern = "^model_output_.*\\.rds$", full.names = TRUE)

#-------------------------------------------------------------------------------
cat("\n== 1. SCHEMA IDENTITY vs production Model 06 output ==\n")
#-------------------------------------------------------------------------------
ok(length(align_files) >= 1, "alignment model_output_*.rds present")
amo <- readRDS(align_files[1]); setDT(amo)
CONTRACT <- c("scenario","age","cause","sex","year","well","sick","newcases","dead",
              "pop","all.mx","intervention","location","eff_ir","eff_cf",
              "intervention_family","scenario_role","parent_package_id","htn_target_scenario")
ok(identical(names(amo), CONTRACT), "alignment output columns == documented 19-col contract (order incl.)")
if (length(prod_files)) {
  pmo <- readRDS(prod_files[1]); setDT(pmo)
  ok(identical(names(amo), names(pmo)), "names identical to production model_output")
  ok(identical(sapply(amo, function(x) class(x)[1]), sapply(pmo, function(x) class(x)[1])),
     "per-column classes identical to production model_output")
} else cat("  (no production model_output on disk to compare classes; using contract)\n")
kc <- c("scenario","age","cause","sex","year","htn_target_scenario")
ok(nrow(amo) == nrow(unique(amo[, ..kc])), "output key (scenario,age,cause,sex,year,htn) is unique")
ok("baseline" %in% amo$scenario, "baseline scenario present")
ok(all(c("scenario","intervention") %in% names(amo)) &&
     uniqueN(amo$intervention) >= 1, "both scenario + intervention columns present (07 keys on intervention; 08/09 on scenario)")
ok(!anyNA(amo[, .(well, sick, dead, pop, newcases)]) &&
     amo[, all(well >= -1e-6 & sick >= -1e-6 & dead >= -1e-6 & pop >= -1e-6 & newcases >= -1e-6)],
   "no NA / negative in well/sick/dead/pop/newcases")

#-------------------------------------------------------------------------------
cat("\n== 2. Model 07 (DALYs) reads alignment output (wd_outp redirect) ==\n")
#-------------------------------------------------------------------------------
res07 <- tryCatch({
  .orig <- wd_outp
  assign("wd_outp", paste0(align_out, "/"), envir = globalenv())
  setwd(wd_code); source(file.path(wd_code, "07_output_dalys.R"), local = FALSE)
  assign("wd_outp", .orig, envir = globalenv())
  "ok"
}, error = function(e) { assign("wd_outp", .orig, envir = globalenv()); conditionMessage(e) })
ok(identical(res07, "ok"), paste0("07_output_dalys.R runs on alignment output",
     if (!identical(res07, "ok")) paste0(" [", res07, "]") else ""))
ok(file.exists(file.path(align_out, "dt_output_dalys.rds")),
   "07 wrote dt_output_dalys.rds into the alignment tree (production output untouched)")

#-------------------------------------------------------------------------------
cat("\n== 3. Model 08 loader + required-column contract on alignment output ==\n")
#-------------------------------------------------------------------------------
# 08 locates output via file.path(wd,'output','out_model') and requires a fixed
# column set (08_economic_value_calculation.R:158-178). We exercise that exact
# read/init contract against the alignment files (its heavy VSL computation is
# out of scope for a schema/naming/path compatibility check).
m08 <- list.files(align_mo_dir, pattern = "^model_output_.*\\.rds$", full.names = TRUE)
ok(length(m08) >= 1, "08 filename pattern ^model_output_.*\\.rds$ matches alignment files")
dt08 <- rbindlist(lapply(m08, readRDS), fill = TRUE)
req08 <- c("location","year","scenario","htn_target_scenario","age","sex","dead","pop")
ok(all(req08 %in% names(dt08)), "08 required columns all present in alignment output")
ok(dt08[scenario == "baseline", .N] > 0 && dt08[scenario != "baseline", .N] > 0,
   "08 baseline + comparator scenarios both present")

#-------------------------------------------------------------------------------
cat("\n== 4. Model 09 (cost/value) reads alignment output from disk ==\n")
#-------------------------------------------------------------------------------
# 09 prefers an in-memory results_list; in this fresh session there is none, so
# it reads the DISK path (wd_outp/out_model). It requires the Model 04 catalogues
# (fair_inputs/fair_scenarios) in memory. Redirect wd_outp so 09 reads + writes
# only in the alignment tree.
ok(exists("fair_inputs") && exists("fair_scenarios"),
   "Model 04 catalogues present in memory for 09")
if (exists("results_list")) rm(list = "results_list", envir = globalenv())  # force disk path
# 09's workbook OUTPUT paths are computed at Model-00 bootstrap from the ORIGINAL
# wd_outp, so redirecting wd_outp alone would still write the clinical workbooks
# to production output/. Redirect those output-file variables into the alignment
# tree too (and restore them after) so production artifacts are never written.
res09 <- tryCatch({
  .orig <- wd_outp
  .ovars <- c("cost_value_output_file", "cost_value_formulae_file",
              "public_health_cost_value_formulae_file")
  .osave <- lapply(.ovars, function(v) if (exists(v, envir = globalenv())) get(v, envir = globalenv()) else NULL)
  assign("wd_outp", paste0(align_out, "/"), envir = globalenv())
  assign("cost_value_output_file",
         file.path(align_out, "indonesia_model_cost_value.xlsx"), envir = globalenv())
  assign("cost_value_formulae_file",
         file.path(align_out, "indonesia_model_cost_value_formulae.xlsx"), envir = globalenv())
  assign("public_health_cost_value_formulae_file",
         file.path(align_out, "indonesia_cost_value_public_health_formulae.xlsx"), envir = globalenv())
  setwd(wd_code); source(file.path(wd_code, "09_cost_value.R"), local = FALSE)
  assign("wd_outp", .orig, envir = globalenv())
  for (k in seq_along(.ovars)) if (!is.null(.osave[[k]])) assign(.ovars[k], .osave[[k]], envir = globalenv())
  "ok"
}, error = function(e) { assign("wd_outp", .orig, envir = globalenv()); conditionMessage(e) })
ok(identical(res09, "ok"), paste0("09_cost_value.R runs on alignment output (disk path)",
     if (!identical(res09, "ok")) paste0(" [", substr(res09,1,120), "]") else ""))
ok(file.exists(file.path(align_out, "indonesia_model_cost_value.xlsx")),
   "09 clinical workbook written to the ALIGNMENT tree (production output/ untouched)")

cat(sprintf("\n==== downstream compatibility: %d PASS / %d FAIL ====\n", PASS, length(FAILS)))
if (length(FAILS)) { cat("FAILURES:\n"); cat(paste0("  - ", FAILS, collapse = "\n"), "\n") }
