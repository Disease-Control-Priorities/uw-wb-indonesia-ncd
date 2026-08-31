#===========================================================================
# 06_run_scenarios_70_30_30_to_70_70_70_subnational.R
#   SUBNATIONAL scenario / Markov engine (well/sick/dead), run PROVINCE BY
#   PROVINCE.
#---------------------------------------------------------------------------
# Adapted from code/cvd-fair-choices/06_run_scenarios_indonesia_fair.R. The
# production engine is fully location-parameterized internally; the ONLY thing
# pinning it to Indonesia is the line `locs <- c("Indonesia")`. It also
# unconditionally reads risk-factor tables (data.in / inc / dt_hbp_control /
# dt_statin_scenarios) built by Models 01/02, which this stand-alone run neither
# has nor is permitted to build. Because the cascade scenario uses ONLY the
# "fair_wb" intervention (and baseline uses none), this adaptation STRIPS
# project.all() down to exactly what the cascade needs: the b_rates slice, the
# workbook-effect application, and the Markov recurrence. The recurrence, the
# seeding, calculate_fair_workbook_impact(), and the output schema are copied
# verbatim from production; the only functional addition is the per-province
# `location` filter on the effect rows.
#
# OUTPUT (per location, saved to out_model/model_output_<location>.rds and kept
# in-memory as results_list): one row per age x cause x sex x year x scenario
# with well/sick/newcases/dead/pop/all.mx/eff_ir/eff_cf + location + scenario +
# family/role/parent/level + htn_target_scenario.
#===========================================================================

suppressPackageStartupMessages({
  library(data.table); library(foreach); library(doParallel); library(parallel)
})

if (!exists("b_rates")) stop("Model 06 (subnational): b_rates not found (run Model 05 first).")
if (!exists("fair_scenarios")) stop("Model 06 (subnational): fair_scenarios not found (run Model 04 first).")

# Global clamp on the reconciled probabilities (matches production line 2659-2662).
b_rates[CF >= 1, CF := 0.99]; b_rates[IR >= 1, IR := 0.99]
b_rates[CF < 0,  CF := 0];    b_rates[IR < 0,  IR := 0]

#---------------------------------------------------------------------------
# FAIR coverage-adjusted effect (verbatim from production 06, ~L539-552) ------
#---------------------------------------------------------------------------
apply_coverage_adjustment <- function(effect_size, coverage_t, coverage_0) {
  if (is.na(coverage_0) || coverage_0 == 0) effect_size * coverage_t
  else effect_size * (coverage_t - coverage_0) / (1 - effect_size * coverage_0)
}

#---------------------------------------------------------------------------
# calculate_fair_workbook_impact() (verbatim from production 06 L1722-1836,
#   plus a per-province `location` filter on the effect rows) -----------------
#---------------------------------------------------------------------------
calculate_fair_workbook_impact <- function(intervention_rates, Country, effect_rows) {
  cat("  - Calculating FAIR-Choices (workbook) package impact for", Country, "\n")
  if (is.null(effect_rows) || nrow(effect_rows) == 0) {
    cat("    (no effect rows supplied; rates returned unchanged)\n")
    return(intervention_rates[])
  }
  dt <- copy(intervention_rates)
  er <- as.data.table(copy(effect_rows))

  # SUBNATIONAL addition: apply only this province's effect rows.
  if ("location" %in% names(er)) er <- er[location == Country]
  if (nrow(er) == 0) {
    cat("    (no effect rows for", Country, "; rates returned unchanged)\n")
    return(intervention_rates[])
  }

  present <- unique(dt$cause)
  miss    <- setdiff(unique(er$cause_code), present)
  if (length(miss) > 0)
    cat("    FAIR wb: mapped cause(s) not present in rates, skipped:", paste(miss, collapse = ", "), "\n")
  er <- er[cause_code %in% present]

  dt[, `:=`(fair_surv_ir = 1, fair_surv_cf = 1)]

  for (i in seq_len(nrow(er))) {
    r <- er[i]
    base_cov <- r$baseline_coverage
    tgt_cov  <- r$target_coverage
    sy       <- r$start_year
    ty       <- r$target_year

    span  <- max(ty - sy + 1, 1)
    frac  <- pmin(pmax((dt$year - sy + 1) / span, 0), 1)
    cov_t <- base_cov + (tgt_cov - base_cov) * frac
    cov_t[dt$year <  sy] <- base_cov
    cov_t[dt$year >  ty] <- tgt_cov
    cov_t <- pmin(pmax(cov_t, 0), 1)

    # Exact per-year coverage path (the cascade trajectory) replaces the ramp.
    if ("coverage_path" %in% names(er)) {
      cp <- er[["coverage_path"]][[i]]
      if (!is.null(cp) && NROW(cp) > 0L) {
        cp <- as.data.table(cp)
        cov_lookup <- setNames(as.numeric(cp$coverage_t), as.character(cp$year))
        got <- cov_lookup[as.character(dt$year)]
        if (anyNA(got)) {
          yrs <- as.integer(cp$year)
          got[is.na(got) & dt$year < min(yrs)] <- cov_lookup[[as.character(min(yrs))]]
          got[is.na(got) & dt$year > max(yrs)] <- cov_lookup[[as.character(max(yrs))]]
        }
        cov_t <- pmin(pmax(as.numeric(got), 0), 1)
      }
    }

    e_adj <- apply_coverage_adjustment(effect_size = r$effect_value,
                                       coverage_t  = cov_t, coverage_0 = base_cov)
    es <- r$affected_fraction * e_adj
    es[!is.finite(es)] <- 0
    es <- pmin(pmax(es, 0), 1)

    gender_ok <- if (identical(r$sex, "Both")) rep(TRUE, nrow(dt)) else dt$sex == r$sex
    mask <- dt$cause == r$cause_code & dt$age >= r$age_start & dt$age <= r$age_stop &
            dt$year >= sy & gender_ok
    mask[is.na(mask)] <- FALSE
    es[!mask] <- 0

    if (identical(r$model_transition, "incidence")) {
      dt[, fair_surv_ir := fair_surv_ir * (1 - es)]
    } else if (identical(r$model_transition, "case_fatality")) {
      dt[, fair_surv_cf := fair_surv_cf * (1 - es)]
    } else {
      stop("FAIR wb: unmapped model_transition '", r$model_transition,
           "' for link ", r$intervention_cause_key, call. = FALSE)
    }
  }

  dt[, `:=`(CF = CF * fair_surv_cf, IR = IR * fair_surv_ir,
            eff_cf = eff_cf * fair_surv_cf, eff_ir = eff_ir * fair_surv_ir)]
  dt[CF > 0.99, CF := 0.99]; dt[IR > 0.99, IR := 0.99]
  dt[CF < 0, CF := 0];       dt[IR < 0, IR := 0]
  dt[, c("fair_surv_ir", "fair_surv_cf") := NULL]

  if (dt[, any(is.na(CF))] || dt[, any(is.na(IR))])
    stop("FAIR workbook computation produced NA in CF or IR.", call. = FALSE)
  setorder(dt, year, sex, location, cause, age)
  cat("    Applied", nrow(er), "workbook effect row(s) for", Country, "across cause(s):",
      paste(sort(unique(er$cause_code)), collapse = ", "), "\n")
  dt[]
}

#---------------------------------------------------------------------------
# project.all() -- STRIPPED to b_rates slice + fair_wb + Markov recurrence ----
#   (recurrence + seeding verbatim from production 06 L2569-2650) -------------
#---------------------------------------------------------------------------
project.all <- function(Country, interventions = character(0), fair_effect_rows = NULL) {
  cat("\n======================================\n")
  cat("PROJECTION:", Country, " | interventions:",
      if (length(interventions)) paste(interventions, collapse = ", ") else "(baseline)", "\n")

  base_rates <- b_rates[location == Country & year >= 2017]
  if (!nrow(base_rates))
    stop("Model 06 (subnational): no b_rates rows for location '", Country, "'.", call. = FALSE)

  intervention_rates <- copy(base_rates)
  intervention_rates[, `:=`(eff_ir = 1, eff_cf = 1, intervention = "baseline")]

  applied <- character(0)
  if ("fair_wb" %in% interventions) {
    intervention_rates <- calculate_fair_workbook_impact(intervention_rates, Country, fair_effect_rows)
    applied <- c(applied, "FAIR")
  }
  intervention_rates[, intervention := if (length(applied)) paste(applied, collapse = " + ") else "baseline"]

  ## Initial states: seed year 2017 (all ages) and age-0 newborns (all years).
  intervention_rates[year == 2017 | age == min_model_age, `:=`(
    sick   = Nx * PREVt0,
    dead   = Nx * DIS.mx.t0,
    well   = Nx * (1 - (PREVt0 + BG.mx)),
    pop    = Nx,
    all.mx = Nx * DIS.mx.t0 + Nx * BG.mx)]
  intervention_rates[CF > 0.99, CF := 0.99]; intervention_rates[IR > 0.99, IR := 0.99]
  setorder(intervention_rates, sex, location, cause, intervention, age)

  a_lo <- min_model_age; a_hi <- max_model_age

  for (i in 1:41) {
    b2 <- intervention_rates[year <= 2017 + i & year >= 2017 + i - 1]
    setorder(b2, sex, location, cause, intervention, age, year)
    b2[, age2 := age + 1]
    b2[, newcases2 := shift(well) * IR, by = .(sex, location, cause, age, intervention)]
    b2[, sick2 := shift(sick) * (1 - (CF + BG.mx + covid.mx)) + shift(well) * IR,
       by = .(sex, location, cause, age, intervention)]
    b2[sick2 < 0, sick2 := 0]
    b2[, dead2 := shift(sick) * CF, by = .(sex, location, cause, age, intervention)]
    b2[dead2 < 0, dead2 := 0]
    b2[, pop2 := shift(pop) - shift(all.mx), by = .(sex, location, cause, age, intervention)]
    b2[pop2 < 0, pop2 := 0]
    b2[, all.mx2 := sum(dead2), by = .(sex, location, year, age, intervention)]
    b2[, all.mx2 := all.mx2 + (pop2 * BG.mx.all) + (pop2 * covid.mx)]
    b2[all.mx2 < 0, all.mx2 := 0]
    b2[, well2 := pop2 - all.mx2 - sick2]
    b2[well2 < 0, well2 := 0]

    upd <- b2[year == 2017 + i & age2 > a_lo]
    upd[age2 > a_hi, age2 := a_hi]
    upd <- upd[, .(newcases = sum(newcases2), sick = sum(sick2), dead = sum(dead2),
                   well = sum(well2), pop = sum(pop2), all.mx = sum(all.mx2)),
               by = .(age = age2, year, sex, location, cause, intervention)]
    intervention_rates[upd, on = .(year, age, sex, location, cause, intervention), `:=`(
      newcases = i.newcases, sick = i.sick, dead = i.dead,
      well = i.well, pop = i.pop, all.mx = i.all.mx)]
  }

  intervention_rates[, .(age, cause, sex, year, well, sick, newcases,
                         dead, pop, all.mx, intervention, location, eff_ir, eff_cf)]
}

#---------------------------------------------------------------------------
# run_multiple_scenarios() -- loop a scenario_list for one location ----------
#   (mirrors production 06 L2713-2828) ---------------------------------------
#---------------------------------------------------------------------------
run_multiple_scenarios <- function(Country, scenario_list) {
  results <- list()
  for (scenario_name in names(scenario_list)) {
    entry <- scenario_list[[scenario_name]]
    ints_arg <- if (!is.null(entry$interventions)) entry$interventions else character(0)
    fer_arg  <- entry$fair_effect_rows
    res <- project.all(Country = Country, interventions = ints_arg, fair_effect_rows = fer_arg)
    res[, `:=`(intervention_family = if (!is.null(entry$family)) entry$family else NA_character_,
               scenario_role       = if (!is.null(entry$scenario_role)) entry$scenario_role else NA_character_,
               parent_package_id   = if (!is.null(entry$parent_package_id)) entry$parent_package_id else NA_character_,
               scenario_level      = if (!is.null(entry$scenario_level)) entry$scenario_level else NA_character_)]
    results[[scenario_name]] <- res
  }
  rbindlist(results, idcol = "scenario", fill = TRUE)
}

#---------------------------------------------------------------------------
# Execution: build the per-location job list (province set + national) --------
#---------------------------------------------------------------------------
# `subnational_locations` (optional) lets a smoke test restrict the province set.
if (exists("subnational_locations") && length(subnational_locations)) {
  run_provinces <- intersect(province_locations, subnational_locations)
  if (!length(run_provinces))
    stop("Model 06 (subnational): `subnational_locations` matched no valid provinces.", call. = FALSE)
} else run_provinces <- province_locations

# Also run the national row (baseline only) for the province-to-national check,
# unless the smoke test restricted the set to a subset of provinces.
run_national <- (national_location %in% b_rates$location) &&
  !(exists("subnational_locations") && length(subnational_locations))

casc_scen <- fair_scenarios[c(baseline_scenario_id, cascade_scenario_id)]
base_only <- fair_scenarios[baseline_scenario_id]

jobs <- c(
  lapply(run_provinces, function(p) list(location = p, scenarios = casc_scen)),
  if (run_national) list(list(location = national_location, scenarios = base_only)) else NULL)
cat(sprintf("\nModel 06 (subnational): %d location-job(s) (%d provinces x {baseline,cascade}%s).\n",
            length(jobs), length(run_provinces),
            if (run_national) " + Indonesia x {baseline}" else ""))

htn_tag <- "cascade_subnational"

run_one_job <- function(job) {
  res <- run_multiple_scenarios(job$location, job$scenarios)
  res[, htn_target_scenario := htn_tag]
  out_file <- file.path(wd_outp, "out_model",
                        paste0("model_output_", gsub("[^A-Za-z0-9]+", "_", job$location), ".rds"))
  saveRDS(res, out_file)
  list(location = job$location, rows = nrow(res), file = out_file, data = res)
}

use_par <- exists("n_cores") && n_cores > 1L && length(jobs) > 1L
t0 <- proc.time()[["elapsed"]]
if (use_par) {
  cat(sprintf("Model 06 (subnational): running %d jobs on %d cores...\n", length(jobs), n_cores))
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  clusterExport(cl, c("b_rates", "project.all", "run_multiple_scenarios",
                      "calculate_fair_workbook_impact", "apply_coverage_adjustment",
                      "min_model_age", "max_model_age", "wd_outp", "htn_tag"),
                envir = environment())
  clusterEvalQ(cl, { suppressPackageStartupMessages(library(data.table)); NULL })
  job_out <- foreach(j = seq_along(jobs), .packages = "data.table",
                     .errorhandling = "pass") %dopar% run_one_job(jobs[[j]])
  stopCluster(cl)
} else {
  job_out <- lapply(jobs, run_one_job)
}
cat(sprintf("Model 06 (subnational): projection finished in %.1f min.\n",
            (proc.time()[["elapsed"]] - t0) / 60))

# Surface any worker error rather than silently continuing.
errs <- Filter(function(x) inherits(x, "error") || inherits(x, "condition"), job_out)
if (length(errs))
  stop("Model 06 (subnational): ", length(errs), " job(s) errored; first: ",
       conditionMessage(errs[[1]]), call. = FALSE)

results_list <- lapply(job_out, function(x) x$data)
names(results_list) <- vapply(job_out, function(x) x$location, character(1))

#---------------------------------------------------------------------------
# Cascade scope guard -- every row carries a permitted scenario id -----------
#---------------------------------------------------------------------------
all_scn <- sort(unique(unlist(lapply(results_list, function(x) as.character(unique(x$scenario))))))
ok_scn  <- c(baseline_scenario_id, cascade_scenario_id)
bad_scn <- setdiff(all_scn, ok_scn)
if (length(bad_scn))
  stop("Model 06 (subnational): unexpected scenario id(s): ", paste(bad_scn, collapse = ", "),
       " (permitted: ", paste(ok_scn, collapse = ", "), ").", call. = FALSE)

# Every simulated province must carry BOTH baseline and the cascade scenario.
prov_out <- results_list[names(results_list) %in% run_provinces]
for (p in names(prov_out)) {
  sc <- sort(unique(as.character(prov_out[[p]]$scenario)))
  if (!setequal(sc, ok_scn))
    stop("Model 06 (subnational): province '", p, "' is missing a scenario; has {",
         paste(sc, collapse = ", "), "}.", call. = FALSE)
}
cat(sprintf("Model 06 (subnational) scope OK: scenarios {%s}; %d province result set(s)%s.\n",
            paste(all_scn, collapse = ", "), length(prov_out),
            if (run_national) " + national baseline" else ""))
