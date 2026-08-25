#===============================================================================
# 03_calibration_indonesia_alignment.R
#-------------------------------------------------------------------------------
# ALIGNMENT calibration: turn the externally-projected Indonesia epidemiology
# (data/processed/indonesia_epidemiology_baseline_alignment.rds, 2023-2050) into
# model-ready transition probabilities that make the well-sick-dead Markov
# baseline REPRODUCE the projection's prevalent_cases + cause_deaths for
# 2023-2050.
#
# CALIBRATION METHOD = FLOW-INVERSION (forward-solve), chosen after a feasibility
# diagnostic showed that time-invariant Nelder-Mead multipliers cannot follow the
# projection's 2023-2050 trajectory (~39% prevalence undershoot), whereas the
# projection is largely flow-consistent with the 3-state model (IHD & HHD fully;
# ischemic/hemorrhagic stroke & RHD ~81-90% of cells). The forward-solve inverts
# Model 06's EXACT source-age-indexed recursion (align_forward_solve) to recover
# the IR(yr,s)/CF(yr,s) that reproduce the projection where flow-feasible, and
# clamps + REPORTS the infeasible tail (never distorts the data silently).
#
# ISOLATED / NON-DESTRUCTIVE. Reads the projection RDS + production
# adjusted_searo_part*.rds (read-only); writes ONLY under data/processed/alignment/.
# Does NOT modify 00/02/04/05/06 or any production artifact.
#
# The ONLY substantive differences from the production calibration, as scoped:
#   (a) TARGET SOURCE = the projection's prevalent_cases + cause_deaths;
#   (b) YEAR WINDOW   = 2023-2050;
#   (c) PATHS         = alignment inputs/outputs under data/processed/alignment/;
#   (d) METHOD        = flow-inversion (user-approved) in place of Nelder-Mead,
#                       because it "reproduces the projection as closely as
#                       possible" (the stated goal) which multipliers cannot.
#
# SEED-YEAR NOTE (reported as a limitation): the forward-solve seeds at 2023 from
# the projection state; Model 06's baseline seeds at 2017 and evolves to 2023, so
# its 2023 state differs slightly. The alignment 05/06 + validation quantify how
# closely Model 06's baseline reproduces the projection.
#
# IMPORTANT: run_adjustment_model <- FALSE so alignment Model 05 does not re-apply
# 032's adjustments2023_age.csv on top of the calibrated rates.
#
# EXECUTION: see alignment_tests/run_alignment_pipeline.R. In brief:
#   source("alignment_tests/alignment_helpers.R"); align_bootstrap_config()
#   source("03_calibration_indonesia_alignment.R")
#===============================================================================

suppressWarnings(suppressMessages(library(data.table)))

#===============================================================================
# 0. PARAMETERS (all alignment settings centralised here)
#===============================================================================
CAL_YEAR_START <- 2023L            # projection window / forward-solve seed year
CAL_YEAR_END   <- 2050L
HIST_YEAR_END  <- 2019L            # production adjusted_searo history end (2000-2019)
TP_EPS         <- 0.005            # probability head-room buffer (as production 032/03)
N_OUT_CHUNKS   <- 10               # adjusted_searo_alignment_part{1..10}.rds (= production)
LOCATION       <- "Indonesia"
PROJ_SHORT     <- c("ihd", "istroke", "hstroke", "hhd", "rhd")  # projection causes

run_adjustment_model <- FALSE      # avoid double-calibration in alignment Model 05

#===============================================================================
# 1. BOOTSTRAP CONFIG + PATHS (no production Models 02-10 are triggered)
#===============================================================================
if (!exists("align_bootstrap_config")) {
  .here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
  hp <- if (!is.null(.here)) file.path(.here, "alignment_tests", "alignment_helpers.R")
        else "code/cvd-fair-choices/alignment_tests/alignment_helpers.R"
  source(hp)
}
align_bootstrap_config()
align_dir <- file.path(wd_data, "alignment")
if (!dir.exists(align_dir)) dir.create(align_dir, recursive = TRUE)
align_check_inputs(wd_data)

cat(sprintf("\n[alignment 03] flow-inversion calibration %d-%d\n", CAL_YEAR_START, CAL_YEAR_END))

#===============================================================================
# 2. PHASE 2 -- BUILD THE ALIGNMENT TRANSITION-PROBABILITY INPUT
#    (projection -> pooled 95+ -> fill 2024 -> derive 13-col adjusted_searo schema)
#===============================================================================
projection <- readRDS(file.path(wd_data, "indonesia_epidemiology_baseline_alignment.rds"))
setDT(projection)
prod_adj <- align_load_production_adjusted(wd_data)          # long names, 2000-2019

xwalk <- align_build_crosswalk(projection, cause_map)
fwrite(xwalk, file.path(align_dir, "alignment_cause_mapping.csv"))

pooled <- align_pool_terminal(projection, max_age = max_model_age)
filled <- align_fill_missing_years(pooled, CAL_YEAR_START, CAL_YEAR_END)
built  <- align_derive_projection_tps(filled, xwalk, prod_adj, cause_map,
                                      location = LOCATION, max_age = max_model_age,
                                      y_start = CAL_YEAR_START, y_end = CAL_YEAR_END,
                                      tp_eps = TP_EPS)
proj_tps <- built$tps                                         # naive IR/CF, 13 cols
fwrite(built$provenance, file.path(align_dir, "alignment_source_provenance.csv"))
fwrite(built$adj_log,    file.path(align_dir, "alignment_constraint_adjustments.csv"))

precalib <- align_assemble_precalib(prod_adj, proj_tps, y_hist_end = HIST_YEAR_END)
saveRDS(precalib, file.path(align_dir, "transition_probabilities_alignment_precalibration.rds"))

cat("[Phase 2] Phase-2 constraint-adjustment log (report, never silently clip):\n")
print(built$adj_log)

#===============================================================================
# 3. FLOW-INVERSION over the 10 location-sex-cause combos (5 causes x 2 sexes)
#    Fast + deterministic (no optimiser); run sequentially.
#===============================================================================
proj_tps[, cause_short := fcoalesce(setNames(names(cause_map), cause_map)[cause], cause)]
combos <- unique(proj_tps[, .(sex, cause, cause_short)])
cat(sprintf("[alignment 03] forward-solving %d combos...\n", nrow(combos)))

rates_list <- vector("list", nrow(combos)); fit_list <- vector("list", nrow(combos))
for (ci in seq_len(nrow(combos))) {
  sx <- combos$sex[ci]; cl <- combos$cause[ci]
  cd <- proj_tps[sex == sx & cause == cl,
                 .(age, year, Nx, PREVt0, DIS.mx.t0, BG.mx, BG.mx.all, ALL.mx)]
  fs <- align_forward_solve(cd, CAL_YEAR_START, CAL_YEAR_END, max_model_age, TP_EPS)
  fs$rates[, `:=`(sex = sx, cause = cl, location = LOCATION)]
  fs$fit[,   `:=`(sex = sx, cause = cl, cause_short = combos$cause_short[ci])]
  rates_list[[ci]] <- fs$rates; fit_list[[ci]] <- fs$fit
}
solved_rates <- rbindlist(rates_list)
fit_all      <- rbindlist(fit_list)

#===============================================================================
# 4. BUILD CALIBRATED TP TABLE: override IR/CF for 2024-2050 with the solved
#    rates; year 2023 keeps the projection-naive IR/CF (its transition is
#    2022->2023, outside the projection window).
#===============================================================================
calibrated <- copy(proj_tps)
calibrated[solved_rates, on = .(location, sex, cause, age, year),
           `:=`(IR = i.IR, CF = i.CF)]
# defensive clamps + covid-free envelope constraints (as production 03)
calibrated[CF >= 1, CF := 0.99]; calibrated[IR >= 1, IR := 0.99]
calibrated[CF < 0, CF := 0];     calibrated[IR < 0, IR := 0]
enforce_tp_constraints(calibrated)
calibrated[, c("bg_modified", "cause_short") := NULL]
setcolorder(calibrated, names(proj_tps)[names(proj_tps) != "cause_short"])

#===============================================================================
# 5. ASSEMBLE + WRITE adjusted_searo_alignment_part{1..10}.rds
#    = production 2000-2019 (ALL 7 causes) + calibrated 2023-2050 (5 causes).
#    (cmd/dm2 and 2020-2022 carried forward by alignment Model 05, as production.)
#===============================================================================
adj_align <- align_assemble_precalib(prod_adj, calibrated, y_hist_end = HIST_YEAR_END)
setcolorder(adj_align, names(prod_adj))
n <- nrow(adj_align); chunk <- ceiling(n / N_OUT_CHUNKS)
for (i in 1:N_OUT_CHUNKS) {
  s <- (i - 1) * chunk + 1; e <- min(i * chunk, n)
  out_i <- if (s > n) adj_align[0] else adj_align[s:e]
  saveRDS(out_i, file.path(align_dir, paste0("adjusted_searo_alignment_part", i, ".rds")))
}

#===============================================================================
# 6. DIAGNOSTICS: achieved-vs-target fit + feasibility (what the flow-inversion
#    could and could not reproduce). All full tables saved to CSV.
#===============================================================================
## per-cause feasibility (fraction of solved 2024-2050 cells that were clamped)
feas <- solved_rates[, .(
  n_cells = .N,
  ir_infeasible_frac = mean(ir_infeasible),
  cf_infeasible_frac = mean(cf_infeasible)), by = .(cause, sex)]
feas <- merge(feas, unique(combos[, .(cause, cause_short)]), by = "cause")
fwrite(solved_rates, file.path(align_dir, "alignment_forward_solve_rates.csv"))

## achieved fit by year (totals + relative error) and by cause
relerr <- function(m, t) ifelse(t > 0, abs(m - t) / t, NA_real_)
by_year <- fit_all[, .(
  prev_target = sum(prev_target), prev_model = sum(prev_model),
  death_target = sum(death_target), death_model = sum(death_model),
  prev_relerr  = sum(abs(prev_model - prev_target)) / sum(prev_target),
  death_relerr = sum(abs(death_model - death_target)) / pmax(sum(death_target), 1)),
  by = year][order(year)]
by_cause <- fit_all[, .(
  prev_target = sum(prev_target), prev_model = sum(prev_model),
  death_target = sum(death_target), death_model = sum(death_model),
  MAE_prev = mean(abs(prev_model - prev_target)),
  MAE_death = mean(abs(death_model - death_target)),
  maxabs_prev = max(abs(prev_model - prev_target)),
  maxabs_death = max(abs(death_model - death_target))),
  by = .(cause_short, sex)][order(cause_short, sex)]
fwrite(fit_all,  file.path(align_dir, "alignment_target_vs_modeled.csv"))
fwrite(by_year,  file.path(align_dir, "alignment_fit_by_year.csv"))
fwrite(by_cause, file.path(align_dir, "calibration_diagnostics_alignment.csv"))
fwrite(feas,     file.path(align_dir, "alignment_feasibility_by_cause.csv"))

#===============================================================================
# 7. VALIDATION (assert constraints, schema, unchanged history)
#===============================================================================
stopifnot(
  "IR NA"         = !anyNA(calibrated$IR),
  "CF NA"         = !anyNA(calibrated$CF),
  "BG.mx NA"      = !anyNA(calibrated$BG.mx),
  "IR outside 01" = calibrated[, all(IR >= 0 & IR <= 1)],
  "CF outside 01" = calibrated[, all(CF >= 0 & CF <= 1)],
  "IR+BG.mx>1"    = calibrated[, all(IR + BG.mx <= 1 + 1e-9)],
  "CF+BG.mx>1"    = calibrated[, all(CF + BG.mx <= 1 + 1e-9)],
  "schema drift"  = identical(names(adj_align), names(prod_adj)),
  "hist changed"  = nrow(adj_align[year <= HIST_YEAR_END]) == nrow(prod_adj),
  "dup keys"      = nrow(adj_align) == nrow(unique(adj_align[, .(age, sex, cause, year, location)])))

#===============================================================================
# 8. SUMMARY
#===============================================================================
cat("\n", strrep("=", 70), "\n[alignment 03] FLOW-INVERSION SUMMARY\n", strrep("=", 70), "\n", sep = "")
cat("\nFeasibility (fraction of 2024-2050 solved cells clamped as infeasible):\n")
print(feas[order(cause_short, sex),
           .(cause = cause_short, sex, ir_infeasible = round(ir_infeasible_frac, 3),
             cf_infeasible = round(cf_infeasible_frac, 3))])
cat("\nAchieved fit by year (prevalence + deaths totals, model vs target):\n")
print(by_year[year %in% c(2023, 2024, 2030, 2040, 2050),
      .(year, prev_target = round(prev_target), prev_model = round(prev_model),
        prev_relerr = round(prev_relerr, 4),
        death_target = round(death_target), death_model = round(death_model),
        death_relerr = round(death_relerr, 4))])
cat("\nPer-cause achieved fit (summed over years, model vs target):\n")
print(by_cause[, .(cause_short, sex, prev_target = round(prev_target),
      prev_model = round(prev_model), death_target = round(death_target),
      death_model = round(death_model))])
ov_prev <- fit_all[, sum(abs(prev_model - prev_target)) / sum(prev_target)]
ov_death <- fit_all[, sum(abs(death_model - death_target)) / sum(death_target)]
cat(sprintf("\nOVERALL relative error (all years/causes): prevalence %.2f%% | deaths %.2f%%\n",
            100 * ov_prev, 100 * ov_death))
cat(sprintf("Cells clamped infeasible: IR %.1f%% | CF %.1f%% (reported, not distorted).\n",
            100 * mean(solved_rates$ir_infeasible), 100 * mean(solved_rates$cf_infeasible)))
cat(sprintf("\nWrote alignment artifacts to %s\n", align_dir))
cat("  adjusted_searo_alignment_part{1..10}.rds (production 2000-2019 + calibrated 2023-2050)\n")
cat("  transition_probabilities_alignment_precalibration.rds, alignment_cause_mapping.csv\n")
cat("  alignment_source_provenance.csv, alignment_constraint_adjustments.csv\n")
cat("  alignment_forward_solve_rates.csv, alignment_feasibility_by_cause.csv\n")
cat("  alignment_target_vs_modeled.csv, alignment_fit_by_year.csv, calibration_diagnostics_alignment.csv\n")
cat("\nrun_adjustment_model is FALSE -> alignment Model 05 will not re-apply 032 factors.\n")
