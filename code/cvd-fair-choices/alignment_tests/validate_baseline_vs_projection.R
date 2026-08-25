#===============================================================================
# validate_baseline_vs_projection.R -- how closely does the ALIGNMENT Model 06
# BASELINE (full recursion, seeded 2017) reproduce the projection's prevalence +
# cause-deaths for 2023-2050? This is the END-TO-END alignment quality and
# includes the 2017-seed transient (the forward-solve seeds at 2023).
#
# Prereqs in memory: cause_map, wd, wd_data. Reads the alignment Model 06 output
# + the projection RDS. Writes alignment/alignment_baseline_vs_projection.csv.
#===============================================================================
suppressWarnings(suppressMessages(library(data.table)))
stopifnot(exists("cause_map"), exists("wd"), exists("wd_data"))

.align_mo_dir <- file.path(wd, "output_alignment", "out_model")
.mo_f <- list.files(.align_mo_dir, pattern = "^model_output_.*\\.rds$", full.names = TRUE)
if (!length(.mo_f)) stop("validate_baseline: no alignment model_output in ", .align_mo_dir)

mo <- readRDS(.mo_f[1]); setDT(mo)
base <- mo[scenario == "baseline" & year >= 2023 & year <= 2050]
mod  <- base[, .(prev_model = sum(sick), death_model = sum(dead)), by = .(cause, year)]

proj <- readRDS(file.path(wd_data, "indonesia_epidemiology_baseline_alignment.rds")); setDT(proj)
proj[, cause := setNames(names(cause_map), cause_map)[gbd_cause_name]]
tgt <- proj[, .(prev_target = sum(prevalent_cases), death_target = sum(cause_deaths)),
            by = .(cause, year)]

cmp <- merge(mod, tgt, by = c("cause", "year"))
fwrite(cmp, file.path(wd_data, "alignment", "alignment_baseline_vs_projection.csv"))

ov_p <- cmp[, sum(abs(prev_model - prev_target)) / sum(prev_target)]
ov_d <- cmp[, sum(abs(death_model - death_target)) / sum(death_target)]
cat("\n", strrep("=", 70), "\nALIGNMENT Model 06 BASELINE vs PROJECTION (2023-2050)\n",
    strrep("=", 70), "\n", sep = "")
cat(sprintf("Overall relative error: prevalence %.2f%% | deaths %.2f%%\n",
            100 * ov_p, 100 * ov_d))
cat("\nBy cause (summed over years):\n")
print(cmp[, .(prev_target = round(sum(prev_target)), prev_model = round(sum(prev_model)),
              prev_relerr = round(100 * sum(abs(prev_model - prev_target)) / sum(prev_target), 2),
              death_relerr = round(100 * sum(abs(death_model - death_target)) / pmax(sum(death_target),1), 2)),
          by = .(cause = setNames(names(cause_map), cause_map)[cause])][order(cause)])
cat("\nBy year (rel err %, over 5 projection causes):\n")
print(cmp[, .(prev = round(100*sum(abs(prev_model-prev_target))/sum(prev_target),2),
              death = round(100*sum(abs(death_model-death_target))/sum(death_target),2)),
          by = year][year %% 3 == 2 | year %in% c(2023,2050)][order(year)])
cat("\nNOTE: the 2017-seed transient (Model 06 seeds at 2017; the forward-solve\n",
    "seeds at 2023) accounts for most of this gap; the forward-solve's own\n",
    "2023-seeded fit is ~1.4% prevalence / 0.25% deaths (see alignment_fit_by_year.csv).\n", sep = "")
