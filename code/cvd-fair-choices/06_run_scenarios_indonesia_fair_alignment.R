# #############################################################################
# 06_run_scenarios_indonesia_fair_alignment.R
#
# ALIGNMENT-ONLY counterpart of 06_run_scenarios_indonesia_fair.R.
#
# WHY A THIN WRAPPER (not a 3,000-line copy): the production Model 06 output
# contract (row grain, column names, classes, scenario naming, semantics) is
# "sacred" and must stay byte-identical so the unchanged Models 07/08/09 read it.
# The most faithful way to guarantee that is to RUN THE PRODUCTION ENGINE ITSELF
# on the alignment `b_rates` (already in memory from
# 05_build_baseline_indonesia_alignment.R), changing ONLY:
#   (1) the OUTPUT LOCATION -- redirected to an isolated alignment tree
#       (<wd>/output_alignment/out_model/) so production output/out_model/ is
#       never touched. Production 06 builds every output/log path from `wd_outp`,
#       so we temporarily point `wd_outp` at the alignment tree, source the
#       unmodified production 06, then restore `wd_outp`.
#   (2) (optional) the SCENARIO SET -- for a smoke test we subset the workbook
#       catalogues (fair_scenarios / public_health_scenarios) to
#       {baseline, one clinical, one public-health} BEFORE sourcing 06, which
#       rebuilds its `scenarios` list from those catalogues. Set
#       ALIGN_SCENARIOS <- "all" to run the full production scenario set.
#
# Nothing about the intervention engines, effect sizes, coverage curves,
# scenario ids, intervention-family / hierarchy fields or execution switches is
# altered -- they come verbatim from production Model 06.
#
# Prerequisites in memory: Model-00 config; Model 04 catalogues (fair_scenarios,
# public_health_scenarios, fair_inputs, public_health_inputs); alignment Model 05
# objects (b_rates, data.in, inc, repYear). Source AFTER 04 and alignment 05.
# #############################################################################

stopifnot("alignment 06: b_rates not found (run alignment Model 05 first)" = exists("b_rates"),
          "alignment 06: fair_scenarios not found (source Model 04 first)"  =
            exists("fair_scenarios"))

baseline_id_align <- if (exists("baseline_scenario_id")) baseline_scenario_id else "baseline"

#-------------------------------------------------------------------------------
# (2) Optional scenario subset for the smoke test (baseline + 1 clinical + 1 PH).
#     ALIGN_SCENARIOS: "smoke" (default) | "all".
#-------------------------------------------------------------------------------
if (!exists("ALIGN_SCENARIOS")) ALIGN_SCENARIOS <- "smoke"
if (identical(ALIGN_SCENARIOS, "smoke")) {
  # clinical: baseline + the combined "all" package (falls back to first lever)
  clin_keep <- intersect(c(baseline_id_align, "all"), names(fair_scenarios))
  if (length(clin_keep) < 2)
    clin_keep <- unique(c(baseline_id_align, setdiff(names(fair_scenarios), baseline_id_align)[1]))
  fair_scenarios <- fair_scenarios[clin_keep]
  # public health: baseline + the combined "all_public_health" (fallback: first)
  if (exists("public_health_scenarios") && !is.null(public_health_scenarios)) {
    ph_keep <- intersect(c(baseline_id_align, "all_public_health"),
                         names(public_health_scenarios))
    if (!length(ph_keep) || (length(ph_keep) == 1 && ph_keep[1] == baseline_id_align))
      ph_keep <- unique(c(baseline_id_align,
                          setdiff(names(public_health_scenarios), baseline_id_align)[1]))
    public_health_scenarios <- public_health_scenarios[ph_keep]
  }
  cat(sprintf("[alignment 06] SMOKE scenario subset: clinical={%s}",
              paste(names(fair_scenarios), collapse = ", ")))
  if (exists("public_health_scenarios") && !is.null(public_health_scenarios))
    cat(sprintf(" | public_health={%s}", paste(names(public_health_scenarios), collapse = ", ")))
  cat("\n(set ALIGN_SCENARIOS <- \"all\" before sourcing for the full scenario set)\n")
} else {
  cat("[alignment 06] running the FULL production scenario set.\n")
}

#-------------------------------------------------------------------------------
# (1) Redirect output to the isolated alignment tree, then run production 06.
#-------------------------------------------------------------------------------
.orig_wd_outp <- wd_outp
align_out_root <- file.path(wd, "output_alignment")
if (!grepl("/$", align_out_root)) align_out_root <- paste0(align_out_root, "/")
dir.create(file.path(align_out_root, "out_model"), recursive = TRUE, showWarnings = FALSE)
wd_outp <- align_out_root                                   # production 06 writes here

# Shim so production 06's final `rm(is, bpcats, locs, i, time1, time2)` cannot
# error in this isolated context (those names may be absent here).
for (.v in c("is", "time1", "time2"))
  if (!exists(.v)) assign(.v, NULL)

setwd(wd_code)
message("[alignment 06] sourcing production 06 engine (output -> ", wd_outp, "out_model/)")
source(file.path(wd_code, "06_run_scenarios_indonesia_fair.R"))

# restore production wd_outp for any later step; results_list stays in globalenv
wd_outp <- .orig_wd_outp
assign("wd_outp", .orig_wd_outp, envir = globalenv())

cat(sprintf("\n[alignment 06] scenarios written to %sout_model/ (production output untouched).\n",
            align_out_root))
cat("[alignment 06] in-memory `results_list` is available for Model 09.\n")
