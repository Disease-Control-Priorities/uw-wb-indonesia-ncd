#===========================================================================
# 09_cost_value.R  --  FAIR Choices costing, budget impact & cost-effectiveness
#===========================================================================
# Transforms the current-run model outputs into a transparent state/flow trace
# and one user-friendly workbook (`indonesia_model_cost_value.xlsx`) containing:
#   * run metadata & the validated intervention / cost catalogue (from Model 04)
#   * a compact auditable model trace (+ de-duplicated background mortality)
#   * annual mortality (baseline vs scenario, deaths averted)
#   * component-level costing, budget impact (UNDISCOUNTED), and a mortality-
#     based cost-effectiveness table (USD per death averted)
#   * Model 08 economic value (reused, when reconcilable) and QA / methods.
#
# CONTRACT / SCOPE
#   * Consumes: fair_inputs & fair_scenarios (Model 04, in memory); the Model 06
#     state/flow output (in-memory `results_list` if present, else the exact
#     current on-disk contract output/out_model/model_output_*.rds); Model 08
#     economic value (output/08_vsl_results.rds) when available & reconcilable.
#   * Does NOT compute or report DALYs/YLL/YLD/disability weights/life-expectancy
#     (deferred). Does NOT depend on Model 07.
#   * Writes exactly one file (`cost_value_output_file`); no new .rds/.csv.
#   * FAIR effect, costing and discounting rules follow FairChoices_Methods and
#     the input workbook (see Methods_and_Sources sheet).
#===========================================================================

suppressWarnings(suppressMessages({
  library(data.table); library(openxlsx)
}))

message("\n=== Model 09: cost & value ===")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

## --- 0. Resolve execution metaparameters (single source of truth: Model 00) --
if (!exists("wd_outp"))
  stop("Model 09: `wd_outp` not set (run from Model 00 or set output path).")
if (!exists("cost_value_output_file"))
  cost_value_output_file <- paste0(wd_outp, "indonesia_model_cost_value.xlsx")
if (!exists("baseline_scenario_id")) baseline_scenario_id <- "baseline"
if (!exists("fair_inputs") || !exists("fair_scenarios"))
  stop("Model 09: `fair_inputs`/`fair_scenarios` not found. Source Model 04 ",
       "(04_define_interventions_indonesia.R) before Model 09.")

A            <- fair_inputs$assumptions
yr_start     <- A$analysis_start_year
yr_end       <- A$analysis_end_year
analysis_yrs <- yr_start:yr_end
disc_rate    <- A$cost_discount_rate
base_id      <- fair_inputs$baseline_scenario_id %||% baseline_scenario_id

## --- 1. Load the Model 06 state/flow output --------------------------------
load_model_output <- function() {
  if (exists("results_list", inherits = TRUE)) {
    rl <- get("results_list", inherits = TRUE)
    rl <- Filter(function(x) !is.null(x) && is.data.frame(x), rl)
    if (length(rl)) {
      message("  Using in-memory Model 06 output (results_list).")
      return(rbindlist(rl, fill = TRUE))
    }
  }
  dir_out <- file.path(wd_outp, "out_model")
  files   <- list.files(dir_out, pattern = "^model_output_.*\\.rds$", full.names = TRUE)
  if (!length(files))
    stop("Model 09: no Model 06 output found (neither in-memory results_list ",
         "nor model_output_*.rds in ", dir_out, ").")
  message("  Reading Model 06 output from disk: ", length(files), " file(s).")
  rbindlist(lapply(files, readRDS), fill = TRUE)
}

mo_all <- load_model_output()
setDT(mo_all)
req <- c("scenario", "year", "age", "sex", "cause", "well", "sick",
         "newcases", "dead", "pop", "all.mx", "location")
missc <- setdiff(req, names(mo_all))
if (length(missc))
  stop("Model 09: Model 06 output missing required column(s): ",
       paste(missc, collapse = ", "))

loc_run <- unique(mo_all$location)[1]
# Keep only scenarios that Model 04 declared AND that were actually produced.
declared     <- names(fair_scenarios)
produced     <- intersect(declared, unique(mo_all$scenario))
scen_missing <- setdiff(declared, produced)
if (length(scen_missing))
  message("  NOTE: declared scenario(s) not present in Model 06 output: ",
          paste(scen_missing, collapse = ", "))
comparators  <- setdiff(produced, base_id)   # scenarios to cost / compare

mo <- mo_all[scenario %in% produced & year %in% analysis_yrs]

## --- 2. State / flow trace --------------------------------------------------
# Cause-grained trace at location x scenario x year x age x cause. It is summed
# over SEX for compactness: no selected intervention/cost row is sex-specific
# (all are sex = "Both"), so the both-sex totals reproduce every PIN quantity
# and death count used here. All internal computations still use the full
# single-sex Model 06 output (`mo`). `population` is the total stratum population
# (identical across causes in the source; summed over sex here).
trace <- mo[, .(well = sum(well), sick = sum(sick), new_cases = sum(newcases),
                cause_deaths = sum(dead), population = sum(pop)),
            by = .(scenario, location, year, age, cause)]
setorder(trace, scenario, year, age, cause)

# Background / all-cause mortality de-duplicated to its proper stratum: all.mx
# (all-cause deaths) is constant across cause, so it is taken ONCE per
# (scenario, year, age, sex) and then summed over sex here -- never summed
# across the modeled causes.
bg_stratum <- unique(mo[, .(scenario, location, year, age, sex,
                            all_cause_deaths = all.mx, pop)])
bg <- bg_stratum[, .(all_cause_deaths = sum(all_cause_deaths), population = sum(pop)),
                 by = .(scenario, location, year, age)]
modeled <- mo[, .(modeled_cause_deaths = sum(dead)),
              by = .(scenario, location, year, age)]
bg <- merge(bg, modeled, by = c("scenario", "location", "year", "age"))
bg[, background_deaths := all_cause_deaths - modeled_cause_deaths]
setorder(bg, scenario, year, age)

## --- 3. Annual mortality: baseline vs scenario, deaths averted -------------
mort <- mo[, .(cases = sum(newcases), cause_deaths = sum(dead)),
           by = .(scenario, year, cause)]
base_mort <- mo[scenario == base_id, .(base_deaths = sum(dead), base_cases = sum(newcases)),
                by = .(year, cause)]
mort <- merge(mort, base_mort, by = c("year", "cause"), all.x = TRUE)
mort[, deaths_averted := base_deaths - cause_deaths]
mort[, cases_averted  := base_cases  - cases]
mort <- merge(mort,
              data.table(scenario = names(fair_scenarios),
                         scenario_label = vapply(fair_scenarios,
                                                 function(s) s$scenario_label, character(1))),
              by = "scenario", all.x = TRUE)
setcolorder(mort, c("scenario", "scenario_label", "year", "cause",
                    "cases", "cause_deaths", "base_deaths", "deaths_averted",
                    "base_cases", "cases_averted"))
setorder(mort, scenario, year, cause)

## --- 4. Component costing ---------------------------------------------------
costs <- copy(fair_inputs$costs)
# Only cost records belonging to scenarios we actually run, and only "ready"
# rows (valid unit cost, coverage and PIN). Unready rows are reported, not used.
costs[, cost_ready := !is.na(unit_cost_usd) & unit_cost_usd >= 0 &
        !is.na(cov_baseline) & !is.na(population_in_need_fraction) &
        population_in_need_measure %in% c("all", "prevalence", "incidence")]

# De-duplicated population table (pop identical across causes).
popu <- unique(mo[, .(scenario, year, age, sex, pop)])

# Model quantity for one cost record under a given scenario, by analysis year.
qty_by_year <- function(scn, cr) {
  a0 <- cr$c_age_start; a1 <- cr$c_age_stop; sx <- cr$c_sex
  if (cr$population_in_need_measure == "all") {
    d <- popu[scenario == scn & age >= a0 & age <= a1]
    if (!identical(sx, "Both")) d <- d[sex == sx]
    agg <- d[, .(q = sum(pop)), by = year]
  } else {
    vcol <- if (cr$population_in_need_measure == "prevalence") "sick" else "newcases"
    d <- mo[scenario == scn & cause == cr$cause_code & age >= a0 & age <= a1]
    if (!identical(sx, "Both")) d <- d[sex == sx]
    agg <- d[, .(q = sum(get(vcol))), by = year]
  }
  m <- merge(data.table(year = analysis_yrs), agg, by = "year", all.x = TRUE)
  m[is.na(q), q := 0]; m$q
}

# Absolute coverage path baseline -> target (linear; matches Model 06 engine).
cov_path <- function(cb, ct, sy, ty, yrs) {
  span <- max(ty - sy + 1, 1)
  frac <- pmin(pmax((yrs - sy + 1) / span, 0), 1)
  cc <- cb + (ct - cb) * frac
  cc[yrs < sy] <- cb; cc[yrs > ty] <- ct
  pmin(pmax(cc, 0), 1)
}

disc_factor <- 1 / (1 + disc_rate)^(analysis_yrs - yr_start)

cost_rows <- list()
for (scn in comparators) {
  ids   <- fair_scenarios[[scn]]$intervention_ids
  comps <- costs[intervention_id %in% ids & cost_ready == TRUE]
  if (nrow(comps) == 0) next
  for (i in seq_len(nrow(comps))) {
    cr    <- comps[i]
    q_s   <- qty_by_year(scn,     cr)
    q_b   <- qty_by_year(base_id, cr)
    cov_s <- cov_path(cr$cov_baseline, cr$cov_target, cr$cov_start_year, cr$cov_target_year, analysis_yrs)
    cov_b <- rep(cr$cov_baseline, length(analysis_yrs))
    pin_s <- q_s * cr$population_in_need_fraction
    pin_b <- q_b * cr$population_in_need_fraction
    cost_s <- pin_s * cov_s * cr$frequency_per_year * cr$unit_cost_usd
    cost_b <- pin_b * cov_b * cr$frequency_per_year * cr$unit_cost_usd
    cost_rows[[length(cost_rows) + 1L]] <- data.table(
      scenario = scn, year = analysis_yrs,
      cost_record_id = cr$cost_record_id, cost_component_key = cr$cost_component_key,
      cost_join_key = cr$cost_join_key, cost_scope = cr$cost_scope,
      intervention_id = cr$intervention_id, cause_code = cr$cause_code %||% NA_character_,
      population_in_need_measure = cr$population_in_need_measure,
      population_in_need_fraction = cr$population_in_need_fraction,
      coverage_scenario = cov_s, coverage_baseline = cov_b,
      frequency_per_year = cr$frequency_per_year, unit_cost_usd = cr$unit_cost_usd,
      pin_scenario = pin_s, pin_baseline = pin_b,
      annual_cost_baseline = cost_b, annual_cost_scenario = cost_s,
      annual_cost_incremental = cost_s - cost_b,
      indonesia_adjusted_flag = cr$indonesia_adjusted_flag, price_year = cr$price_year,
      discount_factor = disc_factor,
      disc_cost_baseline = cost_b * disc_factor,
      disc_cost_scenario = cost_s * disc_factor,
      disc_cost_incremental = (cost_s - cost_b) * disc_factor)
  }
}
annual_cost <- if (length(cost_rows)) rbindlist(cost_rows) else
  data.table()  # (no runnable cost components)
if (nrow(annual_cost))
  setcolorder(annual_cost, c("scenario", "year", "intervention_id", "cause_code",
                             "cost_record_id", "cost_component_key", "cost_join_key",
                             "cost_scope", "population_in_need_measure"))

## --- 5. Budget impact (UNDISCOUNTED headline; discounted kept separate) -----
if (nrow(annual_cost)) {
  bi <- annual_cost[, .(baseline_cost   = sum(annual_cost_baseline),
                        scenario_cost   = sum(annual_cost_scenario),
                        incremental_cost = sum(annual_cost_incremental),
                        disc_incremental_cost = sum(disc_cost_incremental)),
                    by = .(scenario, year)]
  setorder(bi, scenario, year)
  bi[, cumulative_incremental_cost := cumsum(incremental_cost), by = scenario]
  bi[, cumulative_disc_incremental_cost := cumsum(disc_incremental_cost), by = scenario]
} else bi <- data.table()

## --- 6. Cost-effectiveness: USD per death averted --------------------------
da_by_scn <- mort[scenario %in% comparators,
                  .(deaths_averted = sum(deaths_averted, na.rm = TRUE),
                    cases_averted  = sum(cases_averted,  na.rm = TRUE)),
                  by = scenario]
ic_by_scn <- if (nrow(bi))
  bi[, .(incremental_cost = sum(incremental_cost),
         disc_incremental_cost = sum(disc_incremental_cost)), by = scenario] else
  data.table(scenario = comparators, incremental_cost = 0, disc_incremental_cost = 0)

cea <- merge(da_by_scn, ic_by_scn, by = "scenario", all.x = TRUE)
cea[is.na(incremental_cost), incremental_cost := 0]
cea[is.na(disc_incremental_cost), disc_incremental_cost := 0]
cea <- merge(cea,
             data.table(scenario = names(fair_scenarios),
                        scenario_label = vapply(fair_scenarios,
                                                function(s) s$scenario_label, character(1))),
             by = "scenario", all.x = TRUE)
# Discount costs only; deaths averted counted UNDISCOUNTED.
cea[, cost_per_death_averted := NA_real_]
cea[deaths_averted > 0, cost_per_death_averted := disc_incremental_cost / deaths_averted]
cea[, dominance := "USD per death averted"]
cea[deaths_averted > 0 & disc_incremental_cost < 0, dominance := "Dominant (more health, lower cost)"]
cea[deaths_averted <= 0 & disc_incremental_cost > 0, dominance := "Dominated (less/no health, higher cost)"]
cea[deaths_averted <= 0 & disc_incremental_cost <= 0,
    dominance := "No deaths averted; ratio not defined"]
cea[deaths_averted <= 0, cost_per_death_averted := NA_real_]
setcolorder(cea, c("scenario", "scenario_label", "deaths_averted", "cases_averted",
                   "incremental_cost", "disc_incremental_cost",
                   "cost_per_death_averted", "dominance"))
setorder(cea, -deaths_averted)

## --- 7. Economic value (reuse Model 08 VSL/VSLY when reconcilable) ----------
# We sum ONLY aggregate monetary-value columns (economic_value_* = VSL-based,
# vsly_value_* = VSLY-based) over the analysis horizon by scenario -- never the
# per-capita VSL/VSLY *rates*. A clearly-labelled SUPPLEMENTARY benefit-cost
# ratio and net benefit are added against the (undiscounted) incremental cost.
# This is a benefit-cost view, NOT the cost-effectiveness result.
econ_value <- NULL; econ_note <- ""
vsl_file <- file.path(wd_outp, "08_vsl_results.rds")
econ_value <- tryCatch({
  if (!file.exists(vsl_file)) {
    econ_note <- "Model 08 output (08_vsl_results.rds) not found; economic value omitted."; NULL
  } else {
    v <- as.data.table(readRDS(vsl_file))
    if (!("scenario" %in% names(v))) {
      econ_note <- "Model 08 output has no `scenario` column; not reconcilable."; NULL
    } else {
      ov <- intersect(unique(v$scenario), comparators)
      if (!length(ov)) {
        econ_note <- paste0("Model 08 scenarios (",
          paste(head(unique(v$scenario), 6), collapse = ", "),
          ") do not match current run scenarios; economic value omitted. ",
          "Re-run Model 08 on the current scenarios to populate this sheet."); NULL
      } else {
        val_cols <- names(v)[grepl("^economic_value_|^vsly_value_", names(v)) &
                               vapply(v, is.numeric, logical(1))]
        if (!length(val_cols)) {
          econ_note <- "Model 08 output has no aggregate economic_value_/vsly_value_ column."; NULL
        } else {
          ev <- v[scenario %in% ov, c(lapply(.SD, sum, na.rm = TRUE),
                                      list(model08_deaths_averted =
                                             if ("deaths_averted" %in% names(v))
                                               sum(deaths_averted, na.rm = TRUE) else NA_real_)),
                  .SDcols = val_cols, by = scenario]
          # central VSL benefit (elasticity 1.2) if available for BCR / net benefit
          cen <- if ("economic_value_e1_2" %in% names(ev)) "economic_value_e1_2" else val_cols[1]
          ev <- merge(ev, cea[, .(scenario, incremental_cost, disc_incremental_cost,
                                  deaths_averted)], by = "scenario", all.x = TRUE)
          ev[, benefit_cost_ratio_supp := get(cen) / incremental_cost]
          ev[, net_benefit_supp_usd   := get(cen) - incremental_cost]
          econ_note <- paste0("Reused from Model 08 (VSL/VSLY). Supplementary benefit-cost ",
                              "columns use ", cen, " vs undiscounted incremental cost. ",
                              "This is NOT cost-effectiveness.")
          ev[]
        }
      }
    }
  }
}, error = function(e) { econ_note <<- paste0("Model 08 reuse failed: ", conditionMessage(e)); NULL })

## --- 8. R-side reconciliation / QA -----------------------------------------
qa <- list()
add_qa <- function(check, expected, actual, status, note = "")
  qa[[length(qa) + 1L]] <<- data.table(check = check, expected = as.character(expected),
                                       actual = as.character(actual), status = status, note = note)
tol <- 1e-6

# (1) key uniqueness in the validated catalogue
dupk <- fair_inputs$links[, .N, by = intervention_cause_key][N > 1]
add_qa("Intervention-cause key uniqueness", 0, nrow(dupk),
       if (nrow(dupk) == 0) "PASS" else "FAIL", "Each selected link key appears once")
# (2) input readiness
nfail <- sum(fair_inputs$validation$severity == "FAIL")
nrev  <- sum(fair_inputs$validation$severity == "REVIEW")
add_qa("Workbook FAIL-level issues", 0, nfail, if (nfail == 0) "PASS" else "REVIEW",
       "Blocked links/scenarios excluded (see Selected_Interventions / diagnostic)")
add_qa("Workbook REVIEW-level issues", 0, nrev, if (nrev == 0) "PASS" else "REVIEW",
       "Flagged but usable (e.g. cost not Indonesia-adjusted, missing optional component)")
# (3) baseline pairing
paired <- all(comparators %in% unique(mort$scenario)) &&
  all(!is.na(mort[scenario %in% comparators]$base_deaths))
add_qa("Every scenario paired to baseline", TRUE, paired, if (paired) "PASS" else "FAIL",
       "Deaths averted = baseline - scenario at matched location/year/cause")
# (4) stock/flow non-negativity
negc <- mo[, sum(well < -tol | sick < -tol | newcases < -tol | dead < -tol | pop < -tol)]
add_qa("No impossible negative states", 0, negc, if (negc == 0) "PASS" else "FAIL",
       "well/sick/new_cases/cause_deaths/population >= 0")
# (5) stock/flow identity: per cause row, pop = well_c + sick_c + all-cause deaths
maxres <- mo[, max(abs(pop - (well + sick + all.mx)))]
add_qa("Stock/flow identity pop = well + sick + all-cause deaths", "~0",
       round(maxres, 2), if (maxres < 1e3 || maxres / mo[, max(pop)] < 1e-3) "PASS" else "REVIEW",
       "Per cause row; small residual from 95+ pooling / rounding")
# (6) background mortality not duplicated across causes
ndist <- mo[, .(n = uniqueN(round(all.mx, 6))), by = .(scenario, year, age, sex)][, max(n)]
add_qa("Background mortality constant across cause (not duplicated)", 1, ndist,
       if (ndist == 1) "PASS" else "FAIL", "all.mx taken once per stratum in Background sheet")
# (7) cost reconciliation: component rows sum to budget-impact totals
if (nrow(annual_cost) && nrow(bi)) {
  chk <- merge(annual_cost[, .(c_scn = sum(annual_cost_scenario),
                               c_base = sum(annual_cost_baseline)), by = .(scenario, year)],
               bi[, .(scenario, year, scenario_cost, baseline_cost)],
               by = c("scenario", "year"))
  d1 <- chk[, max(abs(c_scn - scenario_cost) + abs(c_base - baseline_cost))]
  add_qa("Cost reconciliation (components -> budget impact)", "0", signif(d1, 3),
         if (d1 < 1e-3) "PASS" else "FAIL", "Component rows sum exactly to annual totals")
} else add_qa("Cost reconciliation (components -> budget impact)", "0", "n/a", "REVIEW",
              "No runnable cost components")
# (8) shared-cost counted once per stratum/year
if (nrow(annual_cost)) {
  shdup <- annual_cost[cost_scope == "shared-count-once",
                       .N, by = .(scenario, year, cost_record_id)][N > 1]
  add_qa("Shared cost counted once per stratum/year", 0, nrow(shdup),
         if (nrow(shdup) == 0) "PASS" else "FAIL",
         "shared-count-once components appear once per scenario-year")
} else add_qa("Shared cost counted once per stratum/year", 0, "n/a", "REVIEW", "No cost components")
# (9) CEA reconciliation: scenario disc incremental cost & deaths averted -> ratio
if (nrow(cea)) {
  rec_ok <- TRUE; rec_note <- "OK"
  for (s in cea$scenario) {
    dc <- if (nrow(bi)) bi[scenario == s, sum(disc_incremental_cost)] else 0
    da <- mort[scenario == s, sum(deaths_averted, na.rm = TRUE)]
    row <- cea[scenario == s]
    if (abs(dc - row$disc_incremental_cost) > max(1, abs(dc) * 1e-6)) { rec_ok <- FALSE; rec_note <- paste0("disc cost mismatch ", s) }
    if (abs(da - row$deaths_averted) > 1e-3) { rec_ok <- FALSE; rec_note <- paste0("deaths averted mismatch ", s) }
    if (!is.na(row$cost_per_death_averted) &&
        abs(row$cost_per_death_averted - dc / da) > max(1, abs(dc/da) * 1e-6)) {
      rec_ok <- FALSE; rec_note <- paste0("ratio mismatch ", s) }
  }
  add_qa("CEA reconciliation (detail -> summary ratio)", "consistent",
         if (rec_ok) "consistent" else "mismatch", if (rec_ok) "PASS" else "FAIL", rec_note)
}
qa_dt <- rbindlist(qa)

## --- 9. Assemble supporting / metadata tables ------------------------------
`%f%` <- function(x, d = 4) ifelse(is.na(x), NA_real_, round(x, d))
sel <- copy(fair_inputs$valid_links)
sel[, adjusted_effect_at_target :=
      affected_fraction * (effect_value * (target_coverage - baseline_coverage) /
                             (1 - effect_value * baseline_coverage))]
sel_out <- sel[, .(intervention_id, intervention_cause_key, intervention_name,
                   cause_id, cause_code, model_name,
                   transition_from, transition_to, model_transition,
                   effect_value, affected_fraction,
                   baseline_coverage, target_coverage, start_year, target_year,
                   adjusted_effect_at_target = `%f%`(adjusted_effect_at_target),
                   cost_join_key, cost_scope, effect_review, coverage_review)]
setorder(sel_out, intervention_id, cause_code)

blocked_out <- fair_inputs$blocked_links[, .(intervention_id, intervention_cause_key,
                                             cause_id, transition_from, transition_to,
                                             effect_value, affected_fraction,
                                             baseline_coverage, target_coverage, problem)]

cost_out <- costs[, .(cost_record_id, cost_component_key, cost_option,
                      intervention_id, cause_id, cause_code, cost_join_key,
                      cost_scope, cost_component, population_in_need_measure,
                      population_in_need_fraction, frequency_per_year,
                      c_age_start, c_age_stop, c_sex, unit_cost_usd, price_year,
                      indonesia_adjusted_flag, cov_baseline, cov_target,
                      cov_start_year, cov_target_year, cost_review, cost_ready)]
setorder(cost_out, intervention_id, cost_component_key)

diag_out <- fair_inputs$validation

meta <- data.table(item = c(
  "Workbook title", "Run date", "Model / pipeline", "Input workbook",
  "Model output source", "Location", "Analysis years", "Baseline scenario",
  "Scenarios costed", "Cost discount rate", "Cost price year", "Currency",
  "Economic perspective", "Coverage scale-up shape", "Downstream cost offsets",
  "Health outcomes (DALYs/YLL/YLD)", "R version", "openxlsx / data.table"),
  value = c(
  "Indonesia NCD FAIR Choices - cost & value",
  as.character(Sys.Date()),
  "CVD FAIR Choices (Models 00-06 -> 09)",
  fair_inputs$inputs_path,
  "output/out_model/model_output_*.rds (Model 06)",
  loc_run,
  paste0(yr_start, "-", yr_end),
  base_id,
  paste(comparators, collapse = ", "),
  sprintf("%.1f%%", 100 * disc_rate),
  as.character(A$cost_price_year),
  A$currency,
  A$economic_perspective,
  A$scale_up_shape,
  as.character(A$downstream_cost_offsets),
  "Out of scope in this stage (deferred)",
  R.version.string,
  paste0(as.character(packageVersion("openxlsx")), " / ",
         as.character(packageVersion("data.table")))))

readme <- data.table(section = c(
  "Purpose",
  "How to read",
  "Scenarios",
  "Baseline pairing",
  "Model trace grain",
  "Background mortality",
  "Costing",
  "Shared costs",
  "Budget impact",
  "Cost-effectiveness",
  "Economic value",
  "Colour legend",
  "Deferred"),
  detail = c(
  "Costing, budget impact and mortality-based cost-effectiveness for the FAIR Choices CVD interventions selected in indonesia_model_inputs.xlsx.",
  "Each sheet is a flat, filterable table (frozen header row). Totals were computed and reconciled in R (see QA_Checks) before writing.",
  "Baseline + one scenario per selected, valid intervention + a combined 'all' scenario. Membership derives only from workbook selections (Model 04).",
  "Deaths averted = baseline deaths - scenario deaths, matched at location x year x age x sex x cause.",
  "Model_State_Trace: location x scenario x year x age x cause, summed over sex (no selection is sex-specific). Population is the total stratum population.",
  "All-cause deaths are constant across cause, so they are taken ONCE per (scenario, year, age, sex) and reported in Background_Mortality (never summed per modeled cause).",
  "annual_cost = population_in_need x coverage(t) x frequency x unit_cost. PIN measure maps 'all'->eligible population, 'prevalence'->sick stock, 'incidence'->new cases.",
  "Components flagged 'shared-count-once' (cost_join_key ...__C_SHARED) are counted once at intervention level, never once per affected cause.",
  "Budget impact reports UNDISCOUNTED baseline, scenario, incremental and cumulative incremental cost. Discounted costs are separate columns.",
  "USD per death averted = cumulative discounted incremental cost / cumulative (undiscounted) deaths averted over the horizon. Not a DALY-based ICER.",
  "Value of statistical life (VSL/VSLY) is reused from Model 08 only when its scenarios reconcile with this run; otherwise it is omitted (see Economic_Value).",
  "Header dark-blue; derived light-blue; unresolved/flagged pale-yellow; PASS green; FAIL/REVIEW red/orange.",
  "DALYs, YLL, YLD, disability weights and life-expectancy outcomes are deferred to later work and are NOT in this workbook."))

methods <- data.table(
  method_id = c("M01","M02","M03","M04","M05","M06","M07","M08","M09","M10","M11"),
  concept = c("Incremental coverage", "FAIR adjusted effect", "Affected fraction",
              "Adjusted transition", "HF/severe mapping", "Annual component cost",
              "Shared cost", "Discounting", "Budget impact", "Cost-effectiveness",
              "FAIR unit-cost markups"),
  formula_or_rule = c(
    "delta_cov(t) = coverage(t) - baseline_coverage",
    "e_adj(t) = effect_value * delta_cov(t) / (1 - effect_value * baseline_coverage)",
    "transition_effect(t) = e_adj(t) * affected_fraction",
    "p_scenario(t) = p_baseline(t) * (1 - transition_effect(t)); prevention->incidence (IR/eff_ir), management->case fatality (CF/eff_cf)",
    "workbook sick_hf / sick_severe collapse onto the single 'sick' state via affected_fraction; NO new Markov states",
    "annual_cost = population_in_need * coverage(t) * frequency_per_year * unit_cost_usd",
    "shared-count-once components counted once per intervention & eligible stratum, not once per affected cause",
    "discount_factor(t) = 1 / (1 + cost_discount_rate)^(t - analysis_start_year); costs discounted, death counts undiscounted",
    "incremental = scenario_cost - baseline_cost (undiscounted headline; discounted reported separately)",
    "USD per death averted = cumulative discounted incremental cost / cumulative deaths averted",
    "not re-applied where indonesia_adjusted_flag = 1 (supplied adjusted costs already include them)"),
  source = c(rep("FairChoices_Methods sheet + https://fairchoices.w.uib.no/documentation/fairchoices-methods/", 4),
             "Model 04 translation table (Indonesia Markov adaptation)",
             rep("FairChoices_Methods sheet / input workbook", 5),
             "FairChoices_Methods M08"))

## --- 10. Write the workbook -------------------------------------------------
message("  Building workbook: ", cost_value_output_file)
wb <- createWorkbook()
modifyBaseFont(wb, fontName = "Carlito", fontSize = 10)

C_HDR <- "#1F4E78"; C_DERIVED <- "#DDEBF7"; C_INPUT <- "#FFF2CC"
C_PASS <- "#C6EFCE"; C_FAIL <- "#FFC7CE"; C_REVIEW <- "#FFEB9C"
st_hdr   <- createStyle(fontName = "Carlito", fontColour = "#FFFFFF", fgFill = C_HDR,
                        textDecoration = "bold", halign = "center", valign = "center",
                        border = "TopBottomLeftRight", borderColour = "#8EA9C1", wrapText = TRUE)
st_title <- createStyle(fontName = "Carlito", fontColour = "#FFFFFF", fgFill = C_HDR,
                        textDecoration = "bold", fontSize = 13)
st_wrap  <- createStyle(fontName = "Carlito", valign = "top", wrapText = TRUE)
st_pass  <- createStyle(fgFill = C_PASS)
st_fail  <- createStyle(fgFill = C_FAIL)
st_rev   <- createStyle(fgFill = C_REVIEW)
st_flag  <- createStyle(fgFill = C_INPUT)

fmt_of <- function(col) {
  cl <- tolower(col)
  if (grepl("^year$|_year$|price_year|^age|c_age", cl))             return(NA_character_)
  if (grepl("discount_factor", cl))                                 return("0.000")
  if (grepl("effect_value|affected_fraction|adjusted_effect", cl))  return("0.000")
  if (grepl("coverage|fraction|^cov_", cl))                         return("0.0%")
  if (grepl("unit_cost", cl))                                       return("#,##0.00")
  if (grepl("per_death", cl))                                       return("#,##0")
  if (grepl("cost|value|benefit|^pin_", cl))                        return("#,##0")
  if (grepl("death|case|population|averted|^well$|^sick$|dead|new_cases", cl)) return("#,##0")
  NA_character_
}

add_sheet <- function(name, df, big = FALSE, round_num = 2) {
  df <- as.data.frame(df)
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = st_hdr, withFilter = !big)
  freezePane(wb, name, firstActiveRow = 2, firstActiveCol = 1)
  nc <- ncol(df); nr <- nrow(df)
  w <- pmin(pmax(nchar(names(df)) + 2, 10), 42)
  setColWidths(wb, name, cols = seq_len(nc), widths = w)
  if (!big && nr > 0) {
    for (j in seq_len(nc)) {
      f <- fmt_of(names(df)[j])
      if (!is.na(f))
        addStyle(wb, name, createStyle(numFmt = f), rows = 2:(nr + 1), cols = j,
                 gridExpand = TRUE, stack = TRUE)
    }
  }
  invisible(NULL)
}

# README (two-column narrative)
addWorksheet(wb, "README")
writeData(wb, "README", "Indonesia NCD FAIR Choices - cost & value workbook", startRow = 1)
addStyle(wb, "README", st_title, rows = 1, cols = 1)
writeData(wb, "README", readme, startRow = 3, headerStyle = st_hdr)
setColWidths(wb, "README", cols = 1:2, widths = c(22, 120))
addStyle(wb, "README", st_wrap, rows = 4:(nrow(readme) + 3), cols = 2, gridExpand = TRUE, stack = TRUE)

add_sheet("Run_Metadata", meta); setColWidths(wb, "Run_Metadata", cols = 1:2, widths = c(30, 70))
add_sheet("Selected_Interventions", sel_out)
if (nrow(blocked_out)) add_sheet("Blocked_Links", blocked_out)
add_sheet("Cost_Components", cost_out)
add_sheet("Annual_Mortality", mort)
if (nrow(annual_cost)) add_sheet("Annual_Cost", annual_cost)
if (nrow(bi))          add_sheet("Budget_Impact", bi)
add_sheet("Cost_Effectiveness", cea)
if (!is.null(econ_value)) {
  addWorksheet(wb, "Economic_Value")
  writeData(wb, "Economic_Value",
            paste0("Reused from Model 08 (VSL/VSLY). ", econ_note),
            startRow = 1)
  addStyle(wb, "Economic_Value", st_wrap, rows = 1, cols = 1)
  writeData(wb, "Economic_Value", as.data.frame(econ_value), startRow = 3, headerStyle = st_hdr)
  freezePane(wb, "Economic_Value", firstActiveRow = 4)
  setColWidths(wb, "Economic_Value", cols = 1:ncol(econ_value),
               widths = pmin(pmax(nchar(names(econ_value)) + 2, 12), 30))
} else {
  addWorksheet(wb, "Economic_Value")
  writeData(wb, "Economic_Value", data.frame(note = econ_note), headerStyle = st_hdr)
  setColWidths(wb, "Economic_Value", cols = 1, widths = 110)
}
add_sheet("QA_Checks", qa_dt)
if (nrow(diag_out)) add_sheet("Input_Diagnostic", diag_out)
add_sheet("Methods_and_Sources", methods)
setColWidths(wb, "Methods_and_Sources", cols = 1:4, widths = c(10, 26, 90, 60))
addStyle(wb, "Methods_and_Sources", st_wrap, rows = 2:(nrow(methods) + 1), cols = 3:4,
         gridExpand = TRUE, stack = TRUE)

# Large trace sheets last (header-only styling for speed; values rounded)
trace_w <- copy(trace)
num_c <- c("well", "sick", "new_cases", "cause_deaths", "population")
trace_w[, (num_c) := lapply(.SD, function(x) round(x, 2)), .SDcols = num_c]
add_sheet("Model_State_Trace", trace_w, big = TRUE)
bg_w <- copy(bg)
num_b <- c("all_cause_deaths", "modeled_cause_deaths", "background_deaths", "population")
bg_w[, (num_b) := lapply(.SD, function(x) round(x, 2)), .SDcols = num_b]
add_sheet("Background_Mortality", bg_w, big = TRUE)

# Conditional colouring on QA status
qa_r <- which(names(as.data.frame(qa_dt)) == "status")
if (length(qa_r)) {
  sc <- as.data.frame(qa_dt)$status
  for (i in seq_along(sc)) {
    stl <- if (sc[i] == "PASS") st_pass else if (sc[i] == "FAIL") st_fail else st_rev
    addStyle(wb, "QA_Checks", stl, rows = i + 1, cols = qa_r, stack = TRUE)
  }
}
# Flag unresolved/blocked cells
if (nrow(blocked_out)) addStyle(wb, "Blocked_Links", st_flag,
                                rows = 2:(nrow(blocked_out) + 1),
                                cols = which(names(blocked_out) == "problem"),
                                gridExpand = TRUE, stack = TRUE)
if (nrow(diag_out)) {
  sev <- diag_out$severity
  cj <- which(names(diag_out) == "severity")
  for (i in seq_along(sev))
    addStyle(wb, "Input_Diagnostic", if (sev[i] == "FAIL") st_fail else st_rev,
             rows = i + 1, cols = cj, stack = TRUE)
}

if (!dir.exists(dirname(cost_value_output_file)))
  dir.create(dirname(cost_value_output_file), recursive = TRUE)
saveWorkbook(wb, cost_value_output_file, overwrite = TRUE)

message("  Wrote: ", cost_value_output_file)
message(sprintf("  Scenarios: %s", paste(produced, collapse = ", ")))
message(sprintf("  QA: %d PASS / %d REVIEW / %d FAIL",
                sum(qa_dt$status == "PASS"), sum(qa_dt$status == "REVIEW"),
                sum(qa_dt$status == "FAIL")))
message("=== Model 09 complete ===")
