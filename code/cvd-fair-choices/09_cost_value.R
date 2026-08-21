#===========================================================================
# 09_cost_value.R  --  FAIR Choices costing, budget impact & cost-effectiveness
#===========================================================================
# Transforms the current-run model outputs into a transparent state/flow trace
# and TWO complementary, user-friendly workbooks:
#   (A) indonesia_model_cost_value.xlsx          -- R-value workbook (Sections
#       1-10): full model trace, background mortality and all decision tables as
#       computed, formatted values.  Contains:
#   * run metadata & the validated intervention / cost catalogue (from Model 04)
#   * a compact auditable model trace (+ de-duplicated background mortality)
#   * annual mortality (baseline vs scenario, deaths averted)
#   * component-level costing, budget impact (UNDISCOUNTED), and a mortality-
#     based cost-effectiveness table (USD per death averted)
#   * Model 08 economic value (reused, when reconcilable) and QA / methods.
#   (B) indonesia_model_cost_value_formulae.xlsx -- formula edition (Section 11):
#       the same decision tables driven by LIVE cross-sheet Excel formulas
#       anchored to an editable Calculation_Assumptions sheet, so a user can edit
#       an exposed assumption and watch every dependent result recompute. Colours
#       distinguish header / formula / R-source / editable-input cells; QA
#       formulas reconcile the Excel results against the R engine values.
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

# The installed openxlsx build registers a drawing/vmlDrawing part reference on
# every worksheet but never writes the drawing content, leaving dangling parts
# that Excel tolerates but stricter readers (openpyxl / LibreOffice / Sheets)
# reject. Strip them so both emitted workbooks are well-formed everywhere. Safe:
# no sheet references a drawing (no images / charts / comments are added). It is
# a guarded no-op if a future openxlsx changes this internal layout.
strip_dangling_drawings <- function(wb) {
  tryCatch({
    if (!is.null(wb$Content_Types))
      wb$Content_Types <- wb$Content_Types[!grepl("/xl/drawings/drawing", wb$Content_Types)]
    if (!is.null(wb$worksheets_rels))
      for (i in seq_along(wb$worksheets_rels))
        if (length(wb$worksheets_rels[[i]]))
          wb$worksheets_rels[[i]] <- grep("/drawings/", wb$worksheets_rels[[i]],
                                          value = TRUE, invert = TRUE)
  }, error = function(e) message("  (drawing-strip skipped: ", conditionMessage(e), ")"))
  invisible(wb)
}

## --- 0. Resolve execution metaparameters (single source of truth: Model 00) --
if (!exists("wd_outp"))
  stop("Model 09: `wd_outp` not set (run from Model 00 or set output path).")
if (!exists("cost_value_output_file"))
  cost_value_output_file <- paste0(wd_outp, "indonesia_model_cost_value.xlsx")
# Companion formula-driven workbook (Section 11); defaults to the R-value file
# name with a `_formulae` suffix in the same directory.
if (!exists("cost_value_formulae_file"))
  cost_value_formulae_file <- sub("\\.xlsx$", "_formulae.xlsx", cost_value_output_file)
if (!exists("baseline_scenario_id")) baseline_scenario_id <- "baseline"
# Public-health formula workbook path (Section 12); default alongside the others.
if (!exists("public_health_cost_value_formulae_file"))
  public_health_cost_value_formulae_file <-
    paste0(wd_outp, "indonesia_cost_value_public_health_formulae.xlsx")

# Intervention-family switches (single source of truth: Model 00). Each family's
# workbook is written only when its switch is TRUE; the shared Model 06 output
# (baseline + whichever families ran) is loaded once below and filtered per family.
if (!exists("run_clinical_interventions"))      run_clinical_interventions      <- TRUE
if (!exists("run_public_health_interventions")) run_public_health_interventions <- FALSE
if (!isTRUE(run_clinical_interventions) && !isTRUE(run_public_health_interventions))
  stop("Model 09: both intervention-family switches are FALSE; nothing to report.")
if (isTRUE(run_clinical_interventions) && (!exists("fair_inputs") || !exists("fair_scenarios")))
  stop("Model 09: run_clinical_interventions = TRUE but `fair_inputs`/`fair_scenarios` not ",
       "found. Source Model 04 (04_define_interventions_indonesia.R) before Model 09.")
if (isTRUE(run_public_health_interventions) && (!exists("public_health_inputs") ||
    is.null(public_health_inputs) || !exists("public_health_scenarios") ||
    is.null(public_health_scenarios)))
  stop("Model 09: run_public_health_interventions = TRUE but `public_health_inputs`/",
       "`public_health_scenarios` not found. Source Model 04 before Model 09.")

# Clinical costing metaparameters (used by the clinical workbook body). Present
# whenever the clinical catalogue exists; the public-health body derives its own.
if (exists("fair_inputs") && !is.null(fair_inputs)) {
  A            <- fair_inputs$assumptions
  yr_start     <- A$analysis_start_year
  yr_end       <- A$analysis_end_year
  analysis_yrs <- yr_start:yr_end
  disc_rate    <- A$cost_discount_rate
  base_id      <- fair_inputs$baseline_scenario_id %||% baseline_scenario_id
} else {
  base_id      <- baseline_scenario_id
}

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

# ==========================================================================
# CLINICAL (FAIR Choices) cost/value workbooks -- written only when clinical
# interventions are enabled. All existing behaviour and both existing output
# files are preserved unchanged inside this block. (Braces do not create a new
# scope in R, so every object below remains available exactly as before.)
# ==========================================================================
if (isTRUE(run_clinical_interventions)) {

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
strip_dangling_drawings(wb)
saveWorkbook(wb, cost_value_output_file, overwrite = TRUE)

message("  Wrote: ", cost_value_output_file)

## ===========================================================================
## 11. Formula-driven decision workbook (live cross-sheet Excel formulas) ------
## ===========================================================================
# Emits a SECOND, compact workbook whose decision tables (adjusted effects,
# costing, budget impact, cost-effectiveness, supplementary economic value and
# QA) are LIVE EXCEL FORMULAS instead of static R values. Editing an exposed
# assumption in Excel recomputes every dependent result. The large model traces
# stay in the R-value workbook above; this book carries only the annual health
# aggregates and the full-precision cost quantities the formulas need.
#
#   Cell colour legend (also stated on the README sheet):
#     * header      dark-blue  (#1F4E78, white bold)
#     * formula     light-blue (#DDEBF7)  -- Excel-calculated
#     * R source    grey       (#F2F2F2)  -- R-generated value the formulas read
#     * input       pale-yellow(#FFF2CC)  -- user-editable assumption/control
#     * QA status   green/red/orange via conditional formatting (PASS/FAIL/REVIEW)
#
# Design notes
#   * Coverage in Cost_Components is pulled (INDEX/MATCH on cost_join_key) from
#     Selected_Interventions so a single coverage assumption is never duplicated.
#   * Annual_Cost exposes the full-precision R quantity (q_scenario/q_baseline)
#     BEFORE the PIN fraction; all costing arithmetic is then Excel formula, so
#     results reconcile with R despite the display rounding used elsewhere.
#   * QA_Checks recomputes each invariant in Excel AND reconciles the Excel CEA
#     headline against the R engine values embedded in Calculation_Assumptions.
# ---------------------------------------------------------------------------

if (!exists("cost_value_formulae_file"))
  cost_value_formulae_file <- sub("\\.xlsx$", "_formulae.xlsx", cost_value_output_file)

message("  Building formula workbook: ", cost_value_formulae_file)

# ---- 11.1 small helpers ---------------------------------------------------
int2col <- openxlsx::int2col

# range-end sheet rows (header = row 1; data rows 2 .. r_<sheet>)
n_si <- nrow(sel_out);      r_si <- n_si + 1L        # Selected_Interventions
n_cc <- nrow(cost_out);     r_cc <- n_cc + 1L        # Cost_Components
n_am <- nrow(mort);         r_am <- n_am + 1L        # Annual_Mortality
n_ac <- nrow(annual_cost);  r_ac <- max(n_ac + 1L, 2L)   # Annual_Cost
n_bi <- nrow(bi);           r_bi <- max(n_bi + 1L, 2L)   # Budget_Impact
n_ce <- nrow(cea);          r_ce <- n_ce + 1L        # Cost_Effectiveness
n_id <- nrow(diag_out);     r_id <- max(n_id + 1L, 2L)   # Input_Diagnostic

# per-column number format from column name (sensible units: people, %, USD,
# rates, ratios, years).
fmt_of2 <- function(col) {
  cl <- tolower(col)
  if (grepl("frequency", cl))                                   return("0.00")
  if (grepl("adjusted_effect", cl))                             return("0.0000")
  if (grepl("effect_value|affected_fraction", cl))              return("0.000")
  if (grepl("discount_factor", cl))                             return("0.000")
  if (grepl("benefit_cost_ratio|_ratio$|^ratio$", cl))          return("0.00")
  if (grepl("^year$|_year$|price_year|age_start|age_stop|^c_age", cl)) return("0")
  if (grepl("coverage|_fraction$|^fraction$|^cov_base|^cov_targ$|coverage_", cl)) return("0.0%")
  if (grepl("^cov_baseline$|^cov_target$", cl))                 return("0.0%")
  if (grepl("unit_cost|r_quantity", cl))                        return("#,##0.00")
  if (grepl("per_death", cl))                                   return("#,##0")
  if (grepl("cost|value|benefit|^pin_|net_benefit|budget", cl)) return("#,##0")
  if (grepl("death|case|population|averted|duplicate_count|key_count|distinct|residual|negative|_count$|^count$", cl)) return("#,##0")
  NA_character_
}

# vector of per-row formulas: fn(r) -> formula string (no leading "=")
frows <- function(fn, rows) vapply(rows, fn, character(1))

# INDEX/MATCH one Cost_Components column (target letter) by cost_record_id (E)
idx_cc <- function(tgt, r)
  sprintf("IFERROR(INDEX('Cost_Components'!$%s$2:$%s$%d,MATCH(E%d,'Cost_Components'!$A$2:$A$%d,0)),\"\")",
          tgt, tgt, r_cc, r, r_cc)
# INDEX/MATCH one Selected_Interventions column (target letter) by cost_join_key (G)
idx_si <- function(tgt, r)
  sprintf("IFERROR(INDEX('Selected_Interventions'!$%s$2:$%s$%d,MATCH(G%d,'Selected_Interventions'!$Q$2:$Q$%d,0)),\"\")",
          tgt, tgt, r_si, r, r_si)
# INDEX/MATCH one Cost_Effectiveness column (target letter) by scenario (A)
idx_ce <- function(tgt, r)
  sprintf("IFERROR(INDEX('Cost_Effectiveness'!$%s$2:$%s$%d,MATCH(A%d,'Cost_Effectiveness'!$A$2:$A$%d,0)),\"\")",
          tgt, tgt, r_ce, r, r_ce)

# (strip_dangling_drawings() is defined once in Section 0 and reused here.)

# ---- 11.2 styles ----------------------------------------------------------
C_HDR <- "#1F4E78"; C_FORMULA <- "#DDEBF7"; C_RSRC <- "#F2F2F2"; C_INPUT <- "#FFF2CC"
st_hdr     <- createStyle(fontColour = "#FFFFFF", fgFill = C_HDR, textDecoration = "bold",
                          halign = "center", valign = "center", wrapText = TRUE,
                          border = "TopBottomLeftRight", borderColour = "#8EA9C1")
st_title   <- createStyle(fontColour = "#FFFFFF", fgFill = C_HDR, textDecoration = "bold",
                          fontSize = 13, valign = "center")
st_formula <- createStyle(fgFill = C_FORMULA)
st_rsrc    <- createStyle(fgFill = C_RSRC)
st_input   <- createStyle(fgFill = C_INPUT)
st_wrap    <- createStyle(valign = "top", wrapText = TRUE)
# conditional-format styles (use bgFill)
cf_pass <- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")
cf_fail <- createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")
cf_rev  <- createStyle(bgFill = "#FFEB9C", fontColour = "#9C6500")

wb <- createWorkbook()
modifyBaseFont(wb, fontName = "Carlito", fontSize = 11)

# style a freshly-written tabular sheet: header, fills, number formats, freeze,
# filter, column widths. formula_cols / rsource_cols / input_cols are 1-based
# column indices; nm is the vector of column names (drives number formats/width).
style_sheet <- function(sheet, nm, nrow_data,
                        formula_cols = integer(0), rsource_cols = integer(0),
                        input_cols = integer(0), header_row = 1L,
                        wrap_cols = integer(0), filter = TRUE, min_w = 11, max_w = 46) {
  ncol <- length(nm)
  addStyle(wb, sheet, st_hdr, rows = header_row, cols = seq_len(ncol), gridExpand = TRUE)
  if (nrow_data > 0) {
    dr <- (header_row + 1L):(header_row + nrow_data)
    for (j in formula_cols) addStyle(wb, sheet, st_formula, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    for (j in rsource_cols) addStyle(wb, sheet, st_rsrc,    rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    for (j in input_cols)   addStyle(wb, sheet, st_input,   rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    for (j in seq_len(ncol)) {
      f <- fmt_of2(nm[j])
      if (!is.na(f)) addStyle(wb, sheet, createStyle(numFmt = f), rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    }
    for (j in wrap_cols) addStyle(wb, sheet, st_wrap, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
  }
  freezePane(wb, sheet, firstActiveRow = header_row + 1L, firstActiveCol = 1L)
  if (filter) addFilter(wb, sheet, rows = header_row, cols = seq_len(ncol))
  w <- pmin(pmax(nchar(nm) + 2L, min_w), max_w)
  setColWidths(wb, sheet, cols = seq_len(ncol), widths = w)
  setRowHeights(wb, sheet, rows = header_row, heights = 28)
  invisible(NULL)
}

# =========================================================================
# 11.3 Calculation_Assumptions  (single source of truth for formula controls)
# =========================================================================
anchor_scn <- if ("all" %in% cea$scenario) "all" else cea$scenario[1]
ar <- cea[scenario == anchor_scn]
r_da_anchor  <- if (nrow(ar)) ar$deaths_averted[1]        else NA_real_
r_dic_anchor <- if (nrow(ar)) ar$disc_incremental_cost[1] else NA_real_
r_cpd_anchor <- if (nrow(ar)) ar$cost_per_death_averted[1] else NA_real_

ca <- data.table(
  parameter_id = c("analysis_start_year","analysis_end_year","baseline_scenario_id",
                   "cost_discount_rate","cost_price_year","currency",
                   "economic_perspective","scale_up_shape","downstream_cost_offsets",
                   "formula_tolerance","stock_flow_residual_limit","trace_precision",
                   "r_stock_flow_max_residual","r_background_distinct_count",
                   "r_negative_state_count","r_deaths_averted_anchor",
                   "r_disc_incremental_cost_anchor","r_cost_per_death_anchor",
                   "qa_anchor_scenario"),
  value = list(as.integer(yr_start), as.integer(yr_end), base_id,
               disc_rate, as.integer(A$cost_price_year), A$currency,
               A$economic_perspective, A$scale_up_shape, as.integer(A$downstream_cost_offsets),
               0.001, 1000L, 2L,
               round(as.numeric(maxres), 2), as.integer(ndist),
               as.integer(negc), as.numeric(r_da_anchor),
               as.numeric(r_dic_anchor), as.numeric(r_cpd_anchor),
               anchor_scn),
  unit = c("year","year","scenario id","proportion/year","USD year","currency",
           "text","text","0/1 flag","USD/count","persons","decimal places",
           "persons","count","count","deaths","USD","USD/death","scenario id"),
  role = c("formula control","formula control","formula control","formula control",
           "metadata","metadata","metadata","formula control","scope","QA control",
           "QA control","audit note","R QA source","R QA source","R QA source",
           "R reconciliation source","R reconciliation source","R reconciliation source",
           "R reconciliation source"),
  description = c("First model and discount year","Last model year",
                  "Comparator used for health and cost calculations",
                  "Annual discount rate applied to costs only","Reporting price year",
                  "Workbook reporting currency","Economic evaluation perspective",
                  "Coverage increases linearly from start to target year",
                  "Downstream disease-cost offsets excluded at this stage",
                  "Absolute reconciliation tolerance (internal Excel checks)",
                  "Review threshold used by the R stock/flow check",
                  "Exported model traces are rounded; R quantity helpers keep full precision",
                  "Maximum stock/flow residual computed in R before export",
                  "Max distinct all-cause mortality values across causes (R)",
                  "Count of impossible negative state/flow values (R)",
                  "R engine deaths averted for the anchor scenario",
                  "R engine discounted incremental cost for the anchor scenario",
                  "R engine USD per death averted for the anchor scenario",
                  "Scenario used for the Excel-vs-R reconciliation checks"),
  source = c(rep("indonesia_model_inputs.xlsx / Model 09", 2), "Model 04 / Model 09",
             "indonesia_model_inputs.xlsx / Model 09", rep("indonesia_model_inputs.xlsx", 4),
             "indonesia_model_inputs.xlsx", "Workbook QA rule", "Model 09",
             "Model 09 export rule", rep("Model 09 current run", 3),
             rep("Model 09 current run (R CEA)", 4)))

addWorksheet(wb, "Calculation_Assumptions")
writeData(wb, "Calculation_Assumptions",
          data.frame(parameter_id = "parameter_id", value = "value", unit = "unit",
                     role = "role", description = "description", source = "source"),
          colNames = FALSE, startRow = 1)
writeData(wb, "Calculation_Assumptions", ca$parameter_id, startCol = 1, startRow = 2, colNames = FALSE)
writeData(wb, "Calculation_Assumptions",
          as.data.frame(ca[, .(unit, role, description, source)]),
          startCol = 3, startRow = 2, colNames = FALSE)
for (i in seq_len(nrow(ca)))
  writeData(wb, "Calculation_Assumptions", ca$value[[i]], startCol = 2, startRow = 1 + i, colNames = FALSE)
# fills: editable inputs (B2:B13) yellow; R-source/reconciliation (B14:B20) grey
addStyle(wb, "Calculation_Assumptions", st_hdr, rows = 1, cols = 1:6, gridExpand = TRUE)
addStyle(wb, "Calculation_Assumptions", st_input, rows = 2:13, cols = 2, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Calculation_Assumptions", st_rsrc,  rows = 14:20, cols = 2, gridExpand = TRUE, stack = TRUE)
# per-value number formats
ca_fmt <- c("0","0",NA,"0.0%","0",NA,NA,NA,"0","0.000","#,##0","0",
            "#,##0.0","0","0","#,##0","#,##0","#,##0.00",NA)
for (i in seq_along(ca_fmt)) if (!is.na(ca_fmt[i]))
  addStyle(wb, "Calculation_Assumptions", createStyle(numFmt = ca_fmt[i]),
           rows = 1 + i, cols = 2, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Calculation_Assumptions", st_wrap, rows = 2:(nrow(ca)+1), cols = 5, gridExpand = TRUE, stack = TRUE)
freezePane(wb, "Calculation_Assumptions", firstActiveRow = 2)
addFilter(wb, "Calculation_Assumptions", rows = 1, cols = 1:6)
setColWidths(wb, "Calculation_Assumptions", cols = 1:6, widths = c(30, 16, 14, 24, 62, 40))
setRowHeights(wb, "Calculation_Assumptions", rows = 1, heights = 28)

# =========================================================================
# 11.4 README (narrative + colour legend)
# =========================================================================
readme_f <- data.table(
  section = c("Purpose","How to read","Scenarios","Baseline pairing",
              "Model aggregates","Costing","Shared costs","Budget impact",
              "Cost-effectiveness","Economic value","QA & reconciliation",
              "Colour legend","Companion workbook","Deferred"),
  detail = c(
    "Costing, budget impact and mortality-based cost-effectiveness for the FAIR Choices CVD interventions selected in indonesia_model_inputs.xlsx.",
    "Grey cells are R-generated source values; light-blue cells are LIVE Excel formulas; pale-yellow cells on Calculation_Assumptions are editable controls. Change a yellow control and the blue results recompute. Calculation_Map lists every dependency.",
    "Baseline + one scenario per selected valid intervention + a combined 'all' scenario. Membership derives only from the workbook selections (Model 04).",
    "Deaths averted = baseline deaths - scenario deaths, matched at location x year x age x sex x cause (aggregated to year x cause here).",
    "Annual_Mortality carries the R health aggregates (cases, deaths, baseline); Annual_Cost carries the full-precision R population quantity before the PIN fraction. The 157k-row state trace stays in the companion R workbook.",
    "annual_cost = population_in_need x coverage(t) x frequency x unit_cost. PIN measure maps 'all'->eligible population, 'prevalence'->sick stock, 'incidence'->new cases.",
    "Components flagged 'shared-count-once' (cost_join_key ...__C_SHARED) are counted once at intervention level, never once per affected cause (see Annual_Cost shared_duplicate_count and QA).",
    "Budget impact reports UNDISCOUNTED baseline, scenario, incremental and cumulative incremental cost. Discounted costs are separate columns.",
    "USD per death averted = cumulative discounted incremental cost / cumulative (undiscounted) deaths averted over the horizon. Not a DALY-based ICER.",
    "Value of statistical life (VSL/VSLY) is reused from Model 08 as R source values; only the cost link, supplementary benefit-cost ratio and net benefit are recalculated here.",
    "QA_Checks recomputes each invariant in Excel and reconciles the Excel cost-effectiveness headline against the R engine values stored on Calculation_Assumptions. PASS/FAIL/REVIEW are conditionally formatted.",
    "Header dark-blue; formula-derived light-blue; R source/helper grey; editable controls pale-yellow; PASS green; FAIL/REVIEW red/orange.",
    "The full R-value workbook (indonesia_model_cost_value.xlsx) keeps the detailed Model_State_Trace and Background_Mortality tables for independent review.",
    "DALYs, YLL, YLD, disability weights and life-expectancy outcomes are deferred to later work and are NOT in this workbook."))
addWorksheet(wb, "README")
writeData(wb, "README", "Indonesia NCD FAIR Choices - cost & value workbook (formula edition)", startRow = 1)
addStyle(wb, "README", st_title, rows = 1, cols = 1)
writeData(wb, "README", readme_f, startRow = 3, headerStyle = st_hdr)
setColWidths(wb, "README", cols = 1:2, widths = c(22, 118))
addStyle(wb, "README", st_wrap, rows = 4:(nrow(readme_f) + 3), cols = 2, gridExpand = TRUE, stack = TRUE)
setRowHeights(wb, "README", rows = 1, heights = 22)

# =========================================================================
# 11.5 Run_Metadata (values; several pulled from Calculation_Assumptions)
# =========================================================================
meta_f <- copy(meta)   # same 18-row item/value table used by the R workbook
addWorksheet(wb, "Run_Metadata")
writeData(wb, "Run_Metadata", meta_f, headerStyle = st_hdr)
# formula cells (sheet rows: data row k -> sheet row k+1)
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 8,
             x = "'Calculation_Assumptions'!B2&\"-\"&'Calculation_Assumptions'!B3")   # Analysis years
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 9,  x = "'Calculation_Assumptions'!B4")   # Baseline scenario
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 11, x = "'Calculation_Assumptions'!B5")   # Cost discount rate
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 12, x = "'Calculation_Assumptions'!B6")   # Cost price year
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 13, x = "'Calculation_Assumptions'!B7")   # Currency
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 14, x = "'Calculation_Assumptions'!B8")   # Economic perspective
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 15, x = "'Calculation_Assumptions'!B9")   # Coverage scale-up shape
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 16, x = "'Calculation_Assumptions'!B10")  # Downstream cost offsets
addStyle(wb, "Run_Metadata", st_formula, rows = c(8,9,11,12,13,14,15,16), cols = 2, gridExpand = FALSE, stack = TRUE)
addStyle(wb, "Run_Metadata", createStyle(numFmt = "0.0%"), rows = 11, cols = 2, stack = TRUE)
setColWidths(wb, "Run_Metadata", cols = 1:2, widths = c(30, 74))
freezePane(wb, "Run_Metadata", firstActiveRow = 2)
setRowHeights(wb, "Run_Metadata", rows = 1, heights = 28)

# =========================================================================
# 11.6 Selected_Interventions  (P adjusted effect + U/V key check = formulas)
# =========================================================================
si <- as.data.frame(sel_out)
si$adjusted_effect_at_target <- NA_real_       # -> formula P
si$key_count <- NA_real_                        # -> formula U
si$formula_status <- NA_character_              # -> formula V
addWorksheet(wb, "Selected_Interventions")
writeData(wb, "Selected_Interventions", si, headerStyle = st_hdr)
writeFormula(wb, "Selected_Interventions", startCol = 16, startRow = 2,
             x = frows(function(r) sprintf(
               "IF(OR(J%d=\"\",K%d=\"\",L%d=\"\",M%d=\"\"),\"\",ROUND(K%d*(J%d*(M%d-L%d)/(1-J%d*L%d)),4))",
               r,r,r,r, r,r,r,r,r,r), 2:r_si))
writeFormula(wb, "Selected_Interventions", startCol = 21, startRow = 2,
             x = frows(function(r) sprintf("COUNTIF($B$2:$B$%d,B%d)", r_si, r), 2:r_si))
writeFormula(wb, "Selected_Interventions", startCol = 22, startRow = 2,
             x = frows(function(r) sprintf("IF(U%d=1,\"OK\",\"DUPLICATE KEY\")", r), 2:r_si))
style_sheet("Selected_Interventions", names(si), n_si,
            formula_cols = c(16, 21, 22))

# =========================================================================
# 11.7 Blocked_Links  (R values; problem highlighted)
# =========================================================================
if (nrow(blocked_out)) {
  addWorksheet(wb, "Blocked_Links")
  writeData(wb, "Blocked_Links", as.data.frame(blocked_out), headerStyle = st_hdr)
  style_sheet("Blocked_Links", names(blocked_out), nrow(blocked_out))
  addStyle(wb, "Blocked_Links", st_input, rows = 2:(nrow(blocked_out) + 1),
           cols = which(names(blocked_out) == "problem"), gridExpand = TRUE, stack = TRUE)
}

# =========================================================================
# 11.8 Cost_Components  (S:V coverage pull + X cost_ready = formulas)
# =========================================================================
cc <- copy(cost_out)
cc[, `:=`(cov_baseline = NA_real_, cov_target = NA_real_,
          cov_start_year = NA_real_, cov_target_year = NA_real_, cost_ready = NA_real_)]
cc <- as.data.frame(cc)
addWorksheet(wb, "Cost_Components")
writeData(wb, "Cost_Components", cc, headerStyle = st_hdr)
writeFormula(wb, "Cost_Components", startCol = 19, startRow = 2, x = frows(function(r) idx_si("L", r), 2:r_cc)) # cov_baseline
writeFormula(wb, "Cost_Components", startCol = 20, startRow = 2, x = frows(function(r) idx_si("M", r), 2:r_cc)) # cov_target
writeFormula(wb, "Cost_Components", startCol = 21, startRow = 2, x = frows(function(r) idx_si("N", r), 2:r_cc)) # cov_start_year
writeFormula(wb, "Cost_Components", startCol = 22, startRow = 2, x = frows(function(r) idx_si("O", r), 2:r_cc)) # cov_target_year
writeFormula(wb, "Cost_Components", startCol = 24, startRow = 2,
             x = frows(function(r) sprintf(
               "IF(AND(P%d<>\"\",P%d>=0,S%d<>\"\",K%d<>\"\",K%d>=0,K%d<=1,OR(J%d=\"all\",J%d=\"prevalence\",J%d=\"incidence\")),1,0)",
               r,r,r,r,r,r,r,r,r), 2:r_cc))  # cost_ready
style_sheet("Cost_Components", names(cc), n_cc, formula_cols = c(19,20,21,22,24),
            wrap_cols = which(names(cc) == "cost_component"))

# =========================================================================
# 11.9 Annual_Mortality  (H deaths_averted, J cases_averted = formulas)
# =========================================================================
am <- copy(mort)
am[, `:=`(deaths_averted = NA_real_, cases_averted = NA_real_)]
am <- as.data.frame(am)
addWorksheet(wb, "Annual_Mortality")
writeData(wb, "Annual_Mortality", am, headerStyle = st_hdr)
writeFormula(wb, "Annual_Mortality", startCol = 8, startRow = 2,
             x = frows(function(r) sprintf("G%d-F%d", r, r), 2:r_am))
writeFormula(wb, "Annual_Mortality", startCol = 10, startRow = 2,
             x = frows(function(r) sprintf("I%d-E%d", r, r), 2:r_am))
style_sheet("Annual_Mortality", names(am), n_am,
            formula_cols = c(8, 10), rsource_cols = c(7, 9))   # G, I are R base values

# =========================================================================
# 11.10 Annual_Cost  (purpose-built formula layout; AF/AG = R quantities)
# =========================================================================
ac_cols <- c("scenario","year","intervention_id","cause_code","cost_record_id",
             "cost_component_key","cost_join_key","cost_scope","population_in_need_measure",
             "population_in_need_fraction","coverage_scenario","coverage_baseline",
             "frequency_per_year","unit_cost_usd","pin_scenario","pin_baseline",
             "annual_cost_baseline","annual_cost_scenario","annual_cost_incremental",
             "indonesia_adjusted_flag","price_year","discount_factor","disc_cost_baseline",
             "disc_cost_scenario","disc_cost_incremental","cov_target","cov_start_year",
             "cov_target_year","c_age_start","c_age_stop","c_sex","r_quantity_scenario",
             "r_quantity_baseline","shared_duplicate_count")
if (n_ac > 0) {
  q_s <- ifelse(annual_cost$population_in_need_fraction > 0,
                annual_cost$pin_scenario / annual_cost$population_in_need_fraction, 0)
  q_b <- ifelse(annual_cost$population_in_need_fraction > 0,
                annual_cost$pin_baseline / annual_cost$population_in_need_fraction, 0)
  ac <- data.frame(
    scenario = annual_cost$scenario, year = annual_cost$year,
    intervention_id = annual_cost$intervention_id, cause_code = annual_cost$cause_code,
    cost_record_id = annual_cost$cost_record_id, cost_component_key = annual_cost$cost_component_key,
    cost_join_key = annual_cost$cost_join_key, cost_scope = annual_cost$cost_scope,
    population_in_need_measure = annual_cost$population_in_need_measure,
    stringsAsFactors = FALSE)
  for (cn in ac_cols[10:31]) ac[[cn]] <- NA_real_        # J..AE formula placeholders
  ac$c_sex <- NA_character_                              # AE is text
  ac$r_quantity_scenario <- q_s                          # AF
  ac$r_quantity_baseline  <- q_b                         # AG
  ac$shared_duplicate_count <- NA_real_                  # AH formula
  ac <- ac[, ac_cols]
} else {
  ac <- as.data.frame(setNames(replicate(length(ac_cols), logical(0), simplify = FALSE), ac_cols))
}
addWorksheet(wb, "Annual_Cost")
writeData(wb, "Annual_Cost", ac, headerStyle = st_hdr)
if (n_ac > 0) {
  R <- 2:r_ac
  wf <- function(col, fn) writeFormula(wb, "Annual_Cost", startCol = col, startRow = 2, x = frows(fn, R))
  wf(10, function(r) idx_cc("K", r))                                        # J population_in_need_fraction
  wf(11, function(r) sprintf(                                               # K coverage_scenario (linear path)
    "IF(OR(L%d=\"\",Z%d=\"\",AA%d=\"\",AB%d=\"\"),\"\",IF(B%d<AA%d,L%d,IF(B%d>AB%d,Z%d,L%d+(Z%d-L%d)*MIN(MAX((B%d-AA%d+1)/MAX(AB%d-AA%d+1,1),0),1))))",
    r,r,r,r, r,r,r, r,r,r, r,r,r, r,r,r,r))
  wf(12, function(r) idx_cc("S", r))                                        # L coverage_baseline
  wf(13, function(r) idx_cc("L", r))                                        # M frequency_per_year
  wf(14, function(r) idx_cc("P", r))                                        # N unit_cost_usd
  wf(15, function(r) sprintf("AF%d*J%d", r, r))                             # O pin_scenario
  wf(16, function(r) sprintf("AG%d*J%d", r, r))                             # P pin_baseline
  wf(17, function(r) sprintf("P%d*L%d*M%d*N%d", r, r, r, r))                # Q annual_cost_baseline
  wf(18, function(r) sprintf("O%d*K%d*M%d*N%d", r, r, r, r))                # R annual_cost_scenario
  wf(19, function(r) sprintf("R%d-Q%d", r, r))                              # S annual_cost_incremental
  wf(20, function(r) idx_cc("R", r))                                        # T indonesia_adjusted_flag
  wf(21, function(r) idx_cc("Q", r))                                        # U price_year
  wf(22, function(r) sprintf("1/(1+'Calculation_Assumptions'!$B$5)^(B%d-'Calculation_Assumptions'!$B$2)", r)) # V discount_factor
  wf(23, function(r) sprintf("Q%d*V%d", r, r))                             # W disc_cost_baseline
  wf(24, function(r) sprintf("R%d*V%d", r, r))                             # X disc_cost_scenario
  wf(25, function(r) sprintf("S%d*V%d", r, r))                             # Y disc_cost_incremental
  wf(26, function(r) idx_cc("T", r))                                        # Z cov_target
  wf(27, function(r) idx_cc("U", r))                                        # AA cov_start_year
  wf(28, function(r) idx_cc("V", r))                                        # AB cov_target_year
  wf(29, function(r) idx_cc("M", r))                                        # AC c_age_start
  wf(30, function(r) idx_cc("N", r))                                        # AD c_age_stop
  wf(31, function(r) idx_cc("O", r))                                        # AE c_sex
  wf(34, function(r) sprintf(                                               # AH shared_duplicate_count
    "IF(H%d=\"shared-count-once\",COUNTIFS($A$2:$A$%d,A%d,$B$2:$B$%d,B%d,$E$2:$E$%d,E%d),1)",
    r, r_ac, r, r_ac, r, r_ac, r))
}
style_sheet("Annual_Cost", ac_cols, n_ac,
            formula_cols = c(10:31, 34), rsource_cols = c(32, 33))

# =========================================================================
# 11.11 Budget_Impact  (C:H = formulas over Annual_Cost)
# =========================================================================
bud_cols <- c("scenario","year","baseline_cost","scenario_cost","incremental_cost",
              "disc_incremental_cost","cumulative_incremental_cost","cumulative_disc_incremental_cost")
if (n_bi > 0) {
  bud <- data.frame(scenario = bi$scenario, year = bi$year, stringsAsFactors = FALSE)
  for (cn in bud_cols[3:8]) bud[[cn]] <- NA_real_
} else bud <- as.data.frame(setNames(replicate(8, logical(0), simplify = FALSE), bud_cols))
addWorksheet(wb, "Budget_Impact")
writeData(wb, "Budget_Impact", bud, headerStyle = st_hdr)
if (n_bi > 0) {
  R <- 2:r_bi
  sumif_ac <- function(tgt, r) sprintf(
    "SUMIFS('Annual_Cost'!$%s$2:$%s$%d,'Annual_Cost'!$A$2:$A$%d,A%d,'Annual_Cost'!$B$2:$B$%d,B%d)",
    tgt, tgt, r_ac, r_ac, r, r_ac, r)
  writeFormula(wb, "Budget_Impact", startCol = 3, startRow = 2, x = frows(function(r) sumif_ac("Q", r), R)) # baseline_cost
  writeFormula(wb, "Budget_Impact", startCol = 4, startRow = 2, x = frows(function(r) sumif_ac("R", r), R)) # scenario_cost
  writeFormula(wb, "Budget_Impact", startCol = 5, startRow = 2, x = frows(function(r) sumif_ac("S", r), R)) # incremental_cost
  writeFormula(wb, "Budget_Impact", startCol = 6, startRow = 2, x = frows(function(r) sumif_ac("Y", r), R)) # disc_incremental_cost
  writeFormula(wb, "Budget_Impact", startCol = 7, startRow = 2,
               x = frows(function(r) sprintf("SUMIFS($E$2:E%d,$A$2:A%d,A%d)", r, r, r), R))                 # cumulative
  writeFormula(wb, "Budget_Impact", startCol = 8, startRow = 2,
               x = frows(function(r) sprintf("SUMIFS($F$2:F%d,$A$2:A%d,A%d)", r, r, r), R))                 # cumulative disc
}
style_sheet("Budget_Impact", bud_cols, n_bi, formula_cols = 3:8)

# =========================================================================
# 11.12 Cost_Effectiveness  (C:I = formulas)
# =========================================================================
ce_cols <- c("scenario","scenario_label","deaths_averted","cases_averted",
             "incremental_cost","disc_incremental_cost","cost_per_death_averted",
             "dominance","reconciliation_status")
ce <- data.frame(scenario = cea$scenario, scenario_label = cea$scenario_label, stringsAsFactors = FALSE)
for (cn in ce_cols[3:7]) ce[[cn]] <- NA_real_
ce$dominance <- NA_character_; ce$reconciliation_status <- NA_character_
addWorksheet(wb, "Cost_Effectiveness")
writeData(wb, "Cost_Effectiveness", ce, headerStyle = st_hdr)
R <- 2:r_ce
writeFormula(wb, "Cost_Effectiveness", startCol = 3, startRow = 2,
             x = frows(function(r) sprintf("SUMIFS('Annual_Mortality'!$H$2:$H$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", r_am, r_am, r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 4, startRow = 2,
             x = frows(function(r) sprintf("SUMIFS('Annual_Mortality'!$J$2:$J$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", r_am, r_am, r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 5, startRow = 2,
             x = frows(function(r) sprintf("SUMIFS('Budget_Impact'!$E$2:$E$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 6, startRow = 2,
             x = frows(function(r) sprintf("SUMIFS('Budget_Impact'!$F$2:$F$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 7, startRow = 2,
             x = frows(function(r) sprintf("IF(C%d>0,F%d/C%d,\"\")", r, r, r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 8, startRow = 2,
             x = frows(function(r) sprintf(
               "IF(AND(C%d>0,F%d<0),\"Dominant (more health, lower cost)\",IF(AND(C%d<=0,F%d>0),\"Dominated (less/no health, higher cost)\",IF(AND(C%d<=0,F%d<=0),\"No deaths averted; ratio not defined\",\"USD per death averted\")))",
               r,r, r,r, r,r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 9, startRow = 2,
             x = frows(function(r) sprintf(
               "IF(AND(ABS(F%d-SUMIFS('Budget_Impact'!$F$2:$F$%d,'Budget_Impact'!$A$2:$A$%d,A%d))<='Calculation_Assumptions'!$B$11,ABS(C%d-SUMIFS('Annual_Mortality'!$H$2:$H$%d,'Annual_Mortality'!$A$2:$A$%d,A%d))<='Calculation_Assumptions'!$B$11),\"consistent\",\"mismatch\")",
               r, r_bi, r_bi, r, r, r_am, r_am, r), R))
style_sheet("Cost_Effectiveness", ce_cols, n_ce, formula_cols = 3:9,
            wrap_cols = c(2, 8))
# CEA reconciliation_status conditional format
conditionalFormatting(wb, "Cost_Effectiveness", cols = 9, rows = 2:r_ce, rule = "consistent", type = "contains", style = cf_pass)
conditionalFormatting(wb, "Cost_Effectiveness", cols = 9, rows = 2:r_ce, rule = "mismatch",   type = "contains", style = cf_fail)
conditionalFormatting(wb, "Cost_Effectiveness", cols = 8, rows = 2:r_ce, rule = "Dominated",  type = "contains", style = cf_rev)

# =========================================================================
# 11.13 Economic_Value  (R VSL/VSLY source B:L; M:Q = formulas) -------------
# =========================================================================
addWorksheet(wb, "Economic_Value")
if (!is.null(econ_value)) {
  ev <- as.data.frame(econ_value)
  ev_names <- names(ev)
  val_cols <- ev_names[grepl("^economic_value_|^vsly_value_", ev_names)]
  n_val <- length(val_cols)
  col_ic  <- 2L + n_val                      # incremental_cost column index (after model08_deaths_averted)
  # locate the central VSL benefit column (elasticity 1.2) used for BCR / net benefit
  cen <- if ("economic_value_e1_2" %in% val_cols) "economic_value_e1_2" else val_cols[1]
  cen_idx <- match(cen, ev_names)            # column index of cen (economic_value_e1_2) in the written frame
  # blank the five formula columns before writing
  ev$incremental_cost <- NA_real_; ev$disc_incremental_cost <- NA_real_
  ev$deaths_averted <- NA_real_; ev$benefit_cost_ratio_supp <- NA_real_
  ev$net_benefit_supp_usd <- NA_real_
  writeData(wb, "Economic_Value",
            paste0("Supplementary benefit-cost view (NOT cost-effectiveness). ", econ_note),
            startRow = 1)
  addStyle(wb, "Economic_Value", st_wrap, rows = 1, cols = 1)
  writeData(wb, "Economic_Value", ev, startRow = 3, headerStyle = st_hdr)
  nrev <- nrow(ev); r0 <- 4L; rN <- 3L + nrev
  Lic <- int2col(col_ic + 1L); Ldc <- int2col(col_ic + 2L); Lda <- int2col(col_ic + 3L)
  Lcen <- int2col(cen_idx)
  Rrows <- r0:rN
  writeFormula(wb, "Economic_Value", startCol = col_ic + 1L, startRow = r0,
               x = frows(function(r) idx_ce("E", r), Rrows))                    # incremental_cost
  writeFormula(wb, "Economic_Value", startCol = col_ic + 2L, startRow = r0,
               x = frows(function(r) idx_ce("F", r), Rrows))                    # disc_incremental_cost
  writeFormula(wb, "Economic_Value", startCol = col_ic + 3L, startRow = r0,
               x = frows(function(r) idx_ce("C", r), Rrows))                    # deaths_averted
  writeFormula(wb, "Economic_Value", startCol = col_ic + 4L, startRow = r0,
               x = frows(function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", Lic, r, Lcen, r, Lic, r), Rrows))  # BCR
  writeFormula(wb, "Economic_Value", startCol = col_ic + 5L, startRow = r0,
               x = frows(function(r) sprintf("%s%d-%s%d", Lcen, r, Lic, r), Rrows))                          # net benefit
  # styling: header row 3, grey R-source B:col_ic, formula col_ic+1 : col_ic+5
  ncE <- ncol(ev)
  addStyle(wb, "Economic_Value", st_hdr, rows = 3, cols = 1:ncE, gridExpand = TRUE)
  addStyle(wb, "Economic_Value", st_rsrc, rows = r0:rN, cols = 2:col_ic, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Economic_Value", st_formula, rows = r0:rN, cols = (col_ic + 1L):(col_ic + 5L), gridExpand = TRUE, stack = TRUE)
  for (j in seq_len(ncE)) {
    f <- fmt_of2(names(ev)[j]); if (!is.na(f))
      addStyle(wb, "Economic_Value", createStyle(numFmt = f), rows = r0:rN, cols = j, gridExpand = TRUE, stack = TRUE)
  }
  freezePane(wb, "Economic_Value", firstActiveRow = 4)
  addFilter(wb, "Economic_Value", rows = 3, cols = 1:ncE)
  setColWidths(wb, "Economic_Value", cols = 1:ncE, widths = pmin(pmax(nchar(names(ev)) + 2, 12), 30))
} else {
  writeData(wb, "Economic_Value", data.frame(note = econ_note), headerStyle = st_hdr)
  setColWidths(wb, "Economic_Value", cols = 1, widths = 110)
}

# =========================================================================
# 11.14 Input_Diagnostic  (R values; severity conditionally formatted)
# =========================================================================
addWorksheet(wb, "Input_Diagnostic")
if (n_id > 0) {
  writeData(wb, "Input_Diagnostic", as.data.frame(diag_out), headerStyle = st_hdr)
  style_sheet("Input_Diagnostic", names(diag_out), n_id,
              wrap_cols = which(names(diag_out) == "problem"))
  sev_col <- which(names(diag_out) == "severity")
  conditionalFormatting(wb, "Input_Diagnostic", cols = sev_col, rows = 2:r_id, rule = "FAIL",   type = "contains", style = cf_fail)
  conditionalFormatting(wb, "Input_Diagnostic", cols = sev_col, rows = 2:r_id, rule = "REVIEW", type = "contains", style = cf_rev)
} else {
  writeData(wb, "Input_Diagnostic",
            data.frame(scope = character(0), item_key = character(0), field = character(0),
                       problem = character(0), severity = character(0)), headerStyle = st_hdr)
  addStyle(wb, "Input_Diagnostic", st_hdr, rows = 1, cols = 1:5, gridExpand = TRUE)
}

# =========================================================================
# 11.15 QA_Checks  (C actual + D status = formulas; Excel-vs-R reconciliation)
# =========================================================================
qa_check <- c(
  "Intervention-cause key uniqueness",
  "Workbook FAIL-level issues",
  "Workbook REVIEW-level issues",
  "Every scenario paired to baseline",
  "No impossible negative states",
  "Stock/flow identity pop = well + sick + all-cause deaths",
  "Background mortality constant across cause (not duplicated)",
  "Cost reconciliation (components -> budget impact)",
  "Shared cost counted once per stratum/year",
  "CEA reconciliation (detail -> summary ratio)",
  "Excel vs R: deaths averted (anchor scenario)",
  "Excel vs R: discounted incremental cost (anchor scenario)",
  "Excel vs R: cost per death averted (anchor scenario)")
qa_expect <- c("0","0","0","TRUE","0","<= limit","1","<= tol","0","consistent",
               "match R","match R","match R")
qa_note <- c(
  "Each selected link key appears once",
  "Blocked links/scenarios excluded (see Selected_Interventions / Input_Diagnostic)",
  "Flagged but usable (e.g. cost not Indonesia-adjusted, missing optional component)",
  "Deaths averted = baseline - scenario at matched location/year/cause",
  "well/sick/new_cases/cause_deaths/population >= 0",
  "Per cause row; small residual from 95+ pooling / rounding",
  "all.mx taken once per stratum in the R Background sheet",
  "Excel component rows sum to Excel annual totals within tolerance",
  "shared-count-once components appear once per scenario-year",
  "Every Cost_Effectiveness row's internal reconciliation is consistent",
  "Excel CEA deaths averted reconciles to the R engine value",
  "Excel CEA discounted incremental cost reconciles to the R engine value",
  "Excel CEA USD per death averted reconciles to the R engine value")
# C (actual) formulas
qa_actual <- c(
  sprintf("COUNTIF('Selected_Interventions'!$U$2:$U$%d,\">1\")", r_si),
  sprintf("COUNTIF('Input_Diagnostic'!$E$2:$E$%d,\"FAIL\")", r_id),
  sprintf("COUNTIF('Input_Diagnostic'!$E$2:$E$%d,\"REVIEW\")", r_id),
  sprintf("COUNTBLANK('Annual_Mortality'!$G$2:$G$%d)=0", r_am),
  "'Calculation_Assumptions'!$B$15... placeholder",   # replaced below
  "'Calculation_Assumptions'!$B$13... placeholder",
  "'Calculation_Assumptions'!$B$14... placeholder",
  sprintf("ABS(SUM('Budget_Impact'!$D$2:$D$%d)-SUM('Annual_Cost'!$R$2:$R$%d))+ABS(SUM('Budget_Impact'!$C$2:$C$%d)-SUM('Annual_Cost'!$Q$2:$Q$%d))",
          r_bi, r_ac, r_bi, r_ac),
  sprintf("COUNTIF('Annual_Cost'!$AH$2:$AH$%d,\">1\")", r_ac),
  sprintf("IF(COUNTIF('Cost_Effectiveness'!$I$2:$I$%d,\"mismatch\")=0,\"consistent\",\"mismatch\")", r_ce),
  sprintf("INDEX('Cost_Effectiveness'!$C$2:$C$%d,MATCH('Calculation_Assumptions'!$B$20,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, r_ce),
  sprintf("INDEX('Cost_Effectiveness'!$F$2:$F$%d,MATCH('Calculation_Assumptions'!$B$20,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, r_ce),
  sprintf("INDEX('Cost_Effectiveness'!$G$2:$G$%d,MATCH('Calculation_Assumptions'!$B$20,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, r_ce))
qa_actual[5] <- "'Calculation_Assumptions'!$B$16"   # r_negative_state_count
qa_actual[6] <- "'Calculation_Assumptions'!$B$14"   # r_stock_flow_max_residual
qa_actual[7] <- "'Calculation_Assumptions'!$B$15"   # r_background_distinct_count
# D (status) formulas -- each references its own C{r}
qa_status <- c(
  "IF(C2=0,\"PASS\",\"FAIL\")",
  "IF(C3=0,\"PASS\",\"REVIEW\")",
  "IF(C4=0,\"PASS\",\"REVIEW\")",
  "IF(C5=TRUE,\"PASS\",\"FAIL\")",
  "IF(C6=0,\"PASS\",\"FAIL\")",
  "IF(C7<='Calculation_Assumptions'!$B$12,\"PASS\",\"REVIEW\")",
  "IF(C8=1,\"PASS\",\"FAIL\")",
  "IF(C9<='Calculation_Assumptions'!$B$11,\"PASS\",\"FAIL\")",
  "IF(C10=0,\"PASS\",\"FAIL\")",
  "IF(C11=\"consistent\",\"PASS\",\"FAIL\")",
  "IF(ABS(C12-'Calculation_Assumptions'!$B$17)<=ABS('Calculation_Assumptions'!$B$17)*0.000001+0.5,\"PASS\",\"FAIL\")",
  "IF(ABS(C13-'Calculation_Assumptions'!$B$18)<=ABS('Calculation_Assumptions'!$B$18)*0.000001+1,\"PASS\",\"FAIL\")",
  "IF(ABS(C14-'Calculation_Assumptions'!$B$19)<=ABS('Calculation_Assumptions'!$B$19)*0.000001+1,\"PASS\",\"FAIL\")")
qa_df <- data.frame(check = qa_check, expected = qa_expect,
                    actual = NA, status = NA_character_, note = qa_note, stringsAsFactors = FALSE)
addWorksheet(wb, "QA_Checks")
writeData(wb, "QA_Checks", qa_df, headerStyle = st_hdr)
writeFormula(wb, "QA_Checks", startCol = 3, startRow = 2, x = qa_actual)
writeFormula(wb, "QA_Checks", startCol = 4, startRow = 2, x = qa_status)
n_qa <- nrow(qa_df); r_qa <- n_qa + 1L
style_sheet("QA_Checks", names(qa_df), n_qa, formula_cols = c(3, 4),
            wrap_cols = 5, filter = TRUE)
conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "PASS",   type = "contains", style = cf_pass)
conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "FAIL",   type = "contains", style = cf_fail)
conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "REVIEW", type = "contains", style = cf_rev)

# =========================================================================
# 11.16 Methods_and_Sources + Calculation_Map
# =========================================================================
methods_f <- rbind(as.data.frame(methods), data.frame(
  method_id = c("M12","M13","M14"),
  concept = c("Excel formula lineage","PIN audit quantities","Economic value boundary"),
  formula_or_rule = c(
    "Formula-derived cells are light blue and depend on visible grey R source/helper cells; Calculation_Map lists the dependency chain.",
    "r_quantity_scenario/baseline (Annual_Cost AF:AG) retain the full-precision R population quantity used before the PIN fraction.",
    "Model 08 monetary value columns are R source values; only the cost link, supplementary BCR and net benefit are recalculated here."),
  source = c("This workbook","Model 09 and this workbook","Model 08 / Model 09"),
  stringsAsFactors = FALSE))
addWorksheet(wb, "Methods_and_Sources")
writeData(wb, "Methods_and_Sources", methods_f, headerStyle = st_hdr)
style_sheet("Methods_and_Sources", names(methods_f), nrow(methods_f),
            wrap_cols = c(3, 4), filter = FALSE, max_w = 90)
setColWidths(wb, "Methods_and_Sources", cols = 1:4, widths = c(10, 26, 88, 58))

cmap <- data.table(
  output_sheet = c("Selected_Interventions","Cost_Components","Annual_Mortality",
                   "Annual_Cost","Annual_Cost","Budget_Impact","Cost_Effectiveness",
                   "Economic_Value","QA_Checks","Run_Metadata","Companion R workbook"),
  formula_columns = c("P, U:V","S:V, X","H, J","J:AE, AH","AF:AG","C:H","C:I","M:Q",
                      "C:D","B8:B16 (subset)","none"),
  depends_on = c("selected health links (J:M)","Selected_Interventions",
                 "R aggregates E:G, I","Cost_Components; Calculation_Assumptions; R quantity AF:AG",
                 "full-precision Model 06 output","Annual_Cost",
                 "Annual_Mortality; Budget_Impact; Calculation_Assumptions",
                 "Cost_Effectiveness; Model 08 values B:L","calculation + diagnostic sheets",
                 "Calculation_Assumptions","Model 06 / Model 09"),
  calculation = c("Adjusted effect at target; key uniqueness/status",
                  "Coverage link (INDEX/MATCH) and cost-readiness rule",
                  "Deaths averted and cases averted",
                  "Coverage path, PIN, annual + discounted cost, shared-cost QA",
                  "Scenario/baseline quantity before the PIN fraction",
                  "Annual and cumulative cost by scenario",
                  "Cumulative health, cost, cost/death, dominance, reconciliation",
                  "Cost link, supplementary benefit-cost ratio and net benefit",
                  "Invariant recomputation and Excel-vs-R reconciliation status",
                  "Metadata pulled from the assumptions controls",
                  "Detailed Model_State_Trace and Background_Mortality"),
  source_role = c(rep("Workbook formula", 4), "R value exposed for audit",
                  rep("Workbook formula", 4), "Workbook formula", "R source data"))
addWorksheet(wb, "Calculation_Map")
writeData(wb, "Calculation_Map", cmap, headerStyle = st_hdr)
style_sheet("Calculation_Map", names(cmap), nrow(cmap), wrap_cols = c(3, 4), filter = FALSE, max_w = 56)
setColWidths(wb, "Calculation_Map", cols = 1:5, widths = c(22, 18, 34, 52, 26))

# =========================================================================
# 11.17 worksheet order, recalc-on-open, save
# =========================================================================
desired_order <- c("README","Run_Metadata","Selected_Interventions","Blocked_Links",
                   "Cost_Components","Annual_Mortality","Annual_Cost","Budget_Impact",
                   "Cost_Effectiveness","Economic_Value","QA_Checks","Input_Diagnostic",
                   "Methods_and_Sources","Calculation_Assumptions","Calculation_Map")
desired_order <- desired_order[desired_order %in% names(wb)]   # drop any conditionally-absent sheet
worksheetOrder(wb) <- match(desired_order, names(wb))
# force Excel to recalculate every formula on open (even in manual-calc mode)
wb$workbook$calcPr <- '<calcPr calcId="191029" fullCalcOnLoad="1"/>'
strip_dangling_drawings(wb)

if (!dir.exists(dirname(cost_value_formulae_file)))
  dir.create(dirname(cost_value_formulae_file), recursive = TRUE)
saveWorkbook(wb, cost_value_formulae_file, overwrite = TRUE)
message("  Wrote formula workbook: ", cost_value_formulae_file)


message(sprintf("  Clinical scenarios: %s", paste(produced, collapse = ", ")))
message(sprintf("  Clinical QA: %d PASS / %d REVIEW / %d FAIL",
                sum(qa_dt$status == "PASS"), sum(qa_dt$status == "REVIEW"),
                sum(qa_dt$status == "FAIL")))

}  # end clinical (run_clinical_interventions) block



# =====================================================================
# source_public_health_cost_value()  --  Model 09 Section 12 builder
# Builds output/indonesia_cost_value_public_health_formulae.xlsx from the
# current-run public-health catalogue (public_health_inputs) and the
# public-health scenarios in the shared Model 06 output (mo_all).
# Fully formatted, formula-driven; exposure-based effects + per-capita policy
# costs. Reuses the clinical workbook's styling/audit conventions.
# =====================================================================
source_public_health_cost_value <- function() {
  stopifnot(exists("public_health_inputs"), !is.null(public_health_inputs),
            exists("public_health_scenarios"), !is.null(public_health_scenarios),
            exists("mo_all"))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  phi  <- public_health_inputs
  phs  <- public_health_scenarios
  PA   <- phi$assumptions
  out_file <- if (exists("public_health_cost_value_formulae_file"))
    public_health_cost_value_formulae_file else
    paste0(wd_outp, "indonesia_cost_value_public_health_formulae.xlsx")
  message("  Building public-health formula workbook: ", out_file)

  yr_start <- as.integer(PA$analysis_start_year); yr_end <- as.integer(PA$analysis_end_year)
  analysis_yrs <- yr_start:yr_end
  disc_rate    <- PA$cost_discount_rate
  ramp_years   <- max(PA$policy_cost_ramp_years, 1)
  policy_start <- as.integer(PA$policy_start_year)
  base_id      <- phi$baseline_scenario_id %||% "baseline"
  base_impl    <- 0

  mo <- as.data.table(mo_all)[scenario %in% names(phs) & year %in% analysis_yrs]
  if (!nrow(mo))
    stop("no public-health scenarios found in the Model 06 output.", call. = FALSE)
  produced    <- intersect(names(phs), unique(mo$scenario))
  comparators <- setdiff(produced, base_id)
  loc_run     <- unique(mo$location)[1]
  scen_lab    <- vapply(phs, function(s) s$scenario_label %||% s$scenario_id, character(1))

  ## ---- health: annual mortality (baseline vs scenario) --------------------
  mort <- mo[, .(cases = sum(newcases), cause_deaths = sum(dead)),
             by = .(scenario, year, cause)]
  base_mort <- mo[scenario == base_id,
                  .(base_deaths = sum(dead), base_cases = sum(newcases)), by = .(year, cause)]
  mort <- merge(mort, base_mort, by = c("year", "cause"), all.x = TRUE)
  mort[, scenario_label := scen_lab[scenario]]
  mort <- mort[scenario %in% comparators]
  setcolorder(mort, c("scenario", "scenario_label", "year", "cause",
                      "cases", "cause_deaths", "base_cases", "base_deaths"))
  setorder(mort, scenario, year, cause)

  ## ---- dedup population by age/sex/year per scenario (never over causes) ---
  pop_for <- function(scn, a0, a1, sx) {
    d <- unique(mo[scenario == scn & age >= a0 & age <= a1, .(year, age, sex, pop)])
    if (!identical(sx, "Both")) d <- d[sex == sx]
    m <- merge(data.table(year = analysis_yrs),
               d[, .(pop = sum(pop)), by = year], by = "year", all.x = TRUE)
    m[is.na(pop), pop := 0]; m$pop
  }

  ## ---- component costing (per intervention; shared-count-once) -------------
  costs <- copy(phi$costs)
  disc_factor <- 1 / (1 + disc_rate)^(analysis_yrs - yr_start)
  cimpl <- pmin(pmax((analysis_yrs - policy_start + 1) / ramp_years, 0), 1)
  cost_rows <- list()
  for (scn in comparators) {
    ids   <- phs[[scn]]$intervention_ids
    comps <- costs[intervention_id %in% ids & cost_ready == TRUE]
    if (!nrow(comps)) next
    for (i in seq_len(nrow(comps))) {
      cr <- comps[i]
      pop_s <- pop_for(scn, cr$c_age_start, cr$c_age_stop, cr$c_sex)
      pop_b <- pop_for(base_id, cr$c_age_start, cr$c_age_stop, cr$c_sex)
      pin_s <- pop_s * cr$population_in_need_fraction
      pin_b <- pop_b * cr$population_in_need_fraction
      cost_s <- pin_s * cimpl     * cr$frequency_per_year * cr$unit_cost_usd
      cost_b <- pin_b * base_impl * cr$frequency_per_year * cr$unit_cost_usd
      cost_rows[[length(cost_rows) + 1L]] <- data.table(
        scenario = scn, year = analysis_yrs,
        intervention_id = cr$intervention_id, cost_record_id = cr$cost_record_id,
        cost_component_key = cr$cost_component_key, cost_join_key = cr$cost_join_key,
        cost_scope = cr$cost_scope, population_in_need_measure = cr$population_in_need_measure,
        population_in_need_fraction = cr$population_in_need_fraction,
        frequency_per_year = cr$frequency_per_year, unit_cost_usd = cr$unit_cost_usd,
        pop_scenario = pop_s, pop_baseline = pop_b,
        cost_impl_frac = cimpl, pin_scenario = pin_s, pin_baseline = pin_b,
        annual_cost_baseline = cost_b, annual_cost_scenario = cost_s,
        annual_cost_incremental = cost_s - cost_b, discount_factor = disc_factor,
        disc_cost_baseline = cost_b * disc_factor, disc_cost_scenario = cost_s * disc_factor,
        disc_cost_incremental = (cost_s - cost_b) * disc_factor,
        indonesia_adjusted_flag = cr$indonesia_adjusted_flag, price_year = cr$price_year,
        review_status = cr$cost_review)
    }
  }
  annual_cost <- if (length(cost_rows)) rbindlist(cost_rows) else data.table()

  ## ---- budget impact ------------------------------------------------------
  if (nrow(annual_cost)) {
    bi <- annual_cost[, .(baseline_cost = sum(annual_cost_baseline),
                          scenario_cost = sum(annual_cost_scenario),
                          incremental_cost = sum(annual_cost_incremental),
                          disc_incremental_cost = sum(disc_cost_incremental)),
                      by = .(scenario, year)]
    setorder(bi, scenario, year)
    bi[, cumulative_incremental_cost := cumsum(incremental_cost), by = scenario]
    bi[, cumulative_disc_incremental_cost := cumsum(disc_incremental_cost), by = scenario]
  } else bi <- data.table()

  ## ---- cost-effectiveness -------------------------------------------------
  da <- mort[, .(deaths_averted = sum(base_deaths - cause_deaths, na.rm = TRUE),
                 cases_averted  = sum(base_cases  - cases, na.rm = TRUE)), by = scenario]
  ic <- if (nrow(bi)) bi[, .(incremental_cost = sum(incremental_cost),
                             disc_incremental_cost = sum(disc_incremental_cost)), by = scenario] else
    data.table(scenario = comparators, incremental_cost = 0, disc_incremental_cost = 0)
  cea <- merge(da, ic, by = "scenario", all.x = TRUE)
  cea[is.na(incremental_cost), incremental_cost := 0]
  cea[is.na(disc_incremental_cost), disc_incremental_cost := 0]
  cea[, scenario_label := scen_lab[scenario]]
  cea[, cost_per_death_averted := NA_real_]
  cea[deaths_averted > 0, cost_per_death_averted := disc_incremental_cost / deaths_averted]
  cea[, dominance := "USD per death averted"]
  cea[deaths_averted > 0 & disc_incremental_cost < 0, dominance := "Dominant (more health, lower cost)"]
  cea[deaths_averted <= 0 & disc_incremental_cost > 0, dominance := "Dominated (less/no health, higher cost)"]
  cea[deaths_averted <= 0 & disc_incremental_cost <= 0, dominance := "No deaths averted; ratio not defined"]
  setcolorder(cea, c("scenario", "scenario_label", "deaths_averted", "cases_averted",
                     "incremental_cost", "disc_incremental_cost", "cost_per_death_averted", "dominance"))
  setorder(cea, -deaths_averted)

  ## ---- R QA anchors -------------------------------------------------------
  tol <- 1e-6
  negc   <- mo[, sum(well < -tol | sick < -tol | newcases < -tol | dead < -tol | pop < -tol)]
  maxres <- mo[, max(abs(pop - (well + sick + all.mx)))]
  ndist  <- mo[, .(n = uniqueN(round(all.mx, 6))), by = .(scenario, year, age, sex)][, max(n)]
  n_bad_trans <- phi$valid_links[model_transition != "incidence", .N]
  anchor_scn <- if ("all_public_health" %in% cea$scenario) "all_public_health" else
    if (nrow(cea)) cea$scenario[1] else NA_character_
  ar <- cea[scenario == anchor_scn]
  r_da  <- if (nrow(ar)) ar$deaths_averted[1] else NA_real_
  r_dic <- if (nrow(ar)) ar$disc_incremental_cost[1] else NA_real_
  r_cpd <- if (nrow(ar)) ar$cost_per_death_averted[1] else NA_real_

  ## ---- display tables -----------------------------------------------------
  vl <- copy(phi$valid_links)
  sel_out <- vl[, .(intervention_id, intervention_cause_key, intervention_name,
                    risk_id, cause_id, cause_code, effect_model,
                    baseline_exposure, target_exposure, response_value, paf_value,
                    lag_model, lag_parameter, exposure_start_year, exposure_target_year,
                    full_effect_at_target = NA_real_, exposure_reduction_abs = NA_real_,
                    exposure_reduction_rel = NA_real_, cost_join_key,
                    review_status = effect_review, key_count = NA_real_,
                    formula_status = NA_character_,
                    parent_package_id, intervention_role, tfa_effect_method)]
  setorder(sel_out, intervention_id, cause_code)

  blocked_out <- phi$blocked_links[, .(intervention_id, intervention_cause_key, cause_id,
                                       transition_from, transition_to, effect_model, problem)]

  expo_out <- phi$exposure[, .(intervention_id, risk_id, risk_exposure_measure, exposure_unit,
                               baseline_exposure, reduction_method, red_or_target, exposure_floor,
                               target_exposure = NA_real_, absolute_reduction = NA_real_,
                               relative_reduction = NA_real_, start_year, target_year,
                               scale_up_shape, review_status = exposure_review)]

  eff_out <- phi$links[, .(intervention_cause_key, intervention_id, cause_id, cause_code,
                           effect_model, response_parameter, response_value, paf_value,
                           lag_model, lag_parameter, baseline_exposure, target_exposure,
                           full_effect_at_target = NA_real_, model_transition,
                           valid = as.integer(valid), review_status = effect_review)]
  setorder(eff_out, intervention_id, cause_code)

  # Policy levers with the new fiscal/regulatory/hierarchy fields; the derived
  # gap / price-change / tax-delta / reduction cells are LIVE Excel formulas.
  Lvp <- as.data.table(phi$policy_levers_processed)
  lev_out <- Lvp[, .(lever_id, intervention_id, component, lever_method,
                     parent_package_id, intervention_role,
                     fiscal_baseline_tax_level, fiscal_target_tax_level, fiscal_tax_level_unit,
                     regulatory_baseline_level, regulatory_target_level,
                     regulatory_baseline_score = reg_baseline_score,
                     regulatory_target_score   = reg_target_score,
                     effect_parameter,
                     implementation_gap = NA_real_, implied_price_change = NA_real_,
                     fiscal_tax_delta = NA_real_, policy_reduction = NA_real_,
                     estimated_risk_reduction_wb, review_status = lever_review, qa_status = lever_qa)]

  # Cost components with the new allocation fields; allocated_child_cost and
  # cost_ready are LIVE formulas (package total x share; cost-readiness rule).
  cc_out <- phi$costs[, .(cost_record_id, cost_component_key, cost_join_key, cost_scope,
                          intervention_id, parent_package_id, scenario_role = cost_scenario_role,
                          cost_component, population_in_need_measure,
                          population_in_need_fraction, frequency_per_year, unit_cost_usd,
                          cost_allocation_share, package_total_cost_usd_per_capita, allocation_method,
                          allocated_child_cost = NA_real_,
                          price_year, indonesia_adjusted_flag, source_country,
                          review_status = cost_review, cost_ready = NA_real_, notes)]
  diag_out <- phi$validation

  # ------------------------------------------------------------------------
  # WRITE WORKBOOK
  # ------------------------------------------------------------------------
  int2col <- openxlsx::int2col
  frows <- function(fn, rows) vapply(rows, fn, character(1))
  # Excel column letter for a data-frame column BY NAME. Formula targets are
  # derived through this so that inserting/reordering columns never silently
  # misaligns a writeFormula() reference.
  xlc <- function(nm_vec, name) int2col(match(name, nm_vec))
  C_HDR <- "#1F4E78"; C_FORMULA <- "#DDEBF7"; C_RSRC <- "#F2F2F2"; C_INPUT <- "#FFF2CC"
  st_hdr   <- createStyle(fontColour = "#FFFFFF", fgFill = C_HDR, textDecoration = "bold",
                          halign = "center", valign = "center", wrapText = TRUE,
                          border = "TopBottomLeftRight", borderColour = "#8EA9C1")
  st_title <- createStyle(fontColour = "#FFFFFF", fgFill = C_HDR, textDecoration = "bold",
                          fontSize = 13, valign = "center")
  st_formula <- createStyle(fgFill = C_FORMULA)
  st_rsrc    <- createStyle(fgFill = C_RSRC)
  st_input   <- createStyle(fgFill = C_INPUT)
  st_wrap    <- createStyle(valign = "top", wrapText = TRUE)
  cf_pass <- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")
  cf_fail <- createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")
  cf_rev  <- createStyle(bgFill = "#FFEB9C", fontColour = "#9C6500")

  fmt_of2 <- function(col) {
    cl <- tolower(col)
    if (grepl("full_effect|_reduction_rel$|relative_reduction|estimated_risk", cl)) return("0.0000")
    if (grepl("response_value|effect_value|paf_value|lag_parameter|baseline_value|target_value|_score$|implementation_gap|price_change|tax_delta|policy_reduction", cl)) return("0.0000")
    if (grepl("frequency", cl)) return("0.00")
    if (grepl("impl_frac|discount_factor|_ratio$", cl)) return("0.000")
    if (grepl("^year$|_year$|price_year|start_year|target_year", cl)) return("0")
    if (grepl("baseline_exposure|target_exposure|exposure_floor|red_or_target|absolute_reduction|reduction_abs|exposure$", cl)) return("0.0000")
    if (grepl("_fraction$|^fraction$|population_in_need_fraction", cl)) return("0.0%")
    if (grepl("allocation_share", cl)) return("0.000")
    if (grepl("unit_cost|package_total_cost|allocated_child_cost", cl)) return("#,##0.0000")
    if (grepl("per_death|per_case", cl)) return("#,##0")
    if (grepl("cost|value|budget|pin_|_cost$", cl)) return("#,##0")
    if (grepl("death|case|population|averted|pop_|_count$|^count$|residual|distinct|negative|key_count", cl)) return("#,##0")
    NA_character_
  }
  wb <- createWorkbook()
  modifyBaseFont(wb, fontName = "Carlito", fontSize = 11)
  style_sheet <- function(sheet, nm, nrow_data, formula_cols = integer(0),
                          rsource_cols = integer(0), input_cols = integer(0),
                          header_row = 1L, wrap_cols = integer(0), filter = TRUE,
                          min_w = 11, max_w = 46) {
    ncol <- length(nm)
    addStyle(wb, sheet, st_hdr, rows = header_row, cols = seq_len(ncol), gridExpand = TRUE)
    if (nrow_data > 0) {
      dr <- (header_row + 1L):(header_row + nrow_data)
      for (j in formula_cols) addStyle(wb, sheet, st_formula, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (j in rsource_cols) addStyle(wb, sheet, st_rsrc,    rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (j in input_cols)   addStyle(wb, sheet, st_input,   rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (j in seq_len(ncol)) { f <- fmt_of2(nm[j])
        if (!is.na(f)) addStyle(wb, sheet, createStyle(numFmt = f), rows = dr, cols = j, gridExpand = TRUE, stack = TRUE) }
      for (j in wrap_cols) addStyle(wb, sheet, st_wrap, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    }
    freezePane(wb, sheet, firstActiveRow = header_row + 1L, firstActiveCol = 1L)
    if (filter) addFilter(wb, sheet, rows = header_row, cols = seq_len(ncol))
    setColWidths(wb, sheet, cols = seq_len(ncol), widths = pmin(pmax(nchar(nm) + 2L, min_w), max_w))
    setRowHeights(wb, sheet, rows = header_row, heights = 28)
  }

  ## ===== Calculation_Assumptions =========================================
  ca <- data.table(
    parameter_id = c("analysis_start_year","analysis_end_year","baseline_scenario_id",
                     "policy_start_year","exposure_target_year","policy_cost_ramp_years",
                     "cost_discount_rate","reporting_price_year","source_cost_price_year",
                     "currency","economic_perspective","scale_up_shape",
                     "baseline_implementation_fraction","formula_tolerance",
                     "stock_flow_residual_limit",
                     "r_negative_state_count","r_stock_flow_max_residual","r_background_distinct_count",
                     "r_deaths_averted_anchor","r_disc_incremental_cost_anchor",
                     "r_cost_per_death_anchor","qa_anchor_scenario"),
    value = list(yr_start, yr_end, base_id, policy_start, as.integer(PA$exposure_target_year),
                 ramp_years, disc_rate, as.integer(PA$cost_price_year),
                 as.integer(PA$source_cost_price_year %||% NA), PA$currency,
                 PA$economic_perspective, PA$scale_up_shape, base_impl, 0.001, 1000,
                 as.integer(negc), round(as.numeric(maxres), 2), as.integer(ndist),
                 as.numeric(r_da), as.numeric(r_dic), as.numeric(r_cpd), anchor_scn),
    unit = c("year","year","scenario id","year","year","years","proportion/year","USD year",
             "USD year","currency","text","text","proportion","USD/count","persons","count","persons",
             "count","deaths","USD","USD/death","scenario id"),
    role = c(rep("formula control", 9), "metadata","metadata","formula control",
             "formula control","QA control","QA control", rep("R QA source",3),
             rep("R reconciliation source",4)),
    description = c("First model and discount year","Last model year",
                   "No-new-policy comparator","First policy implementation year",
                   "Year full exposure reduction is reached","Years to full policy cost",
                   "Annual discount rate applied to costs","Reporting price year",
                   "Price year of source unit costs","Reporting currency",
                   "Economic evaluation perspective","Cost/effect scale-up shape",
                   "Baseline (counterfactual) policy implementation fraction",
                   "Absolute reconciliation tolerance",
                   "Persons tolerance for the stock/flow identity check",
                   "Impossible negative state count (R)",
                   "Max stock/flow residual (R)","Max distinct all-cause mx across cause (R)",
                   "R deaths averted for the anchor scenario",
                   "R discounted incremental cost for the anchor scenario",
                   "R USD per death averted for the anchor scenario",
                   "Scenario used for Excel-vs-R reconciliation"),
    source = c(rep(basename(phi$inputs_path), 12), "Model 09",
               "Workbook QA rule","Workbook QA rule", rep("Model 09 current run",3),
               rep("Model 09 current run (R CEA)",4)))
  addWorksheet(wb, "Calculation_Assumptions")
  writeData(wb, "Calculation_Assumptions",
            data.frame(parameter_id="parameter_id", value="value", unit="unit",
                       role="role", description="description", source="source"),
            colNames = FALSE, startRow = 1)
  writeData(wb, "Calculation_Assumptions", ca$parameter_id, startCol = 1, startRow = 2, colNames = FALSE)
  writeData(wb, "Calculation_Assumptions", as.data.frame(ca[, .(unit, role, description, source)]),
            startCol = 3, startRow = 2, colNames = FALSE)
  for (i in seq_len(nrow(ca)))
    writeData(wb, "Calculation_Assumptions", ca$value[[i]], startCol = 2, startRow = 1 + i, colNames = FALSE)
  addStyle(wb, "Calculation_Assumptions", st_hdr, rows = 1, cols = 1:6, gridExpand = TRUE)
  addStyle(wb, "Calculation_Assumptions", st_input, rows = 2:16, cols = 2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Calculation_Assumptions", st_rsrc,  rows = 17:23, cols = 2, gridExpand = TRUE, stack = TRUE)
  ca_fmt <- c("0","0",NA,"0","0","0.0","0.0%","0","0",NA,NA,NA,"0.000","0.000","#,##0",
              "#,##0","#,##0.0","0","#,##0","#,##0","#,##0.00",NA)
  for (i in seq_along(ca_fmt)) if (!is.na(ca_fmt[i]))
    addStyle(wb, "Calculation_Assumptions", createStyle(numFmt = ca_fmt[i]), rows = 1 + i, cols = 2, stack = TRUE)
  addStyle(wb, "Calculation_Assumptions", st_wrap, rows = 2:(nrow(ca)+1), cols = 5, gridExpand = TRUE, stack = TRUE)
  freezePane(wb, "Calculation_Assumptions", firstActiveRow = 2)
  addFilter(wb, "Calculation_Assumptions", rows = 1, cols = 1:6)
  setColWidths(wb, "Calculation_Assumptions", cols = 1:6, widths = c(32, 18, 14, 22, 60, 40))
  setRowHeights(wb, "Calculation_Assumptions", rows = 1, heights = 28)
  # cell refs
  cA_start <- "'Calculation_Assumptions'!$B$2"; cA_end <- "'Calculation_Assumptions'!$B$3"
  cA_base  <- "'Calculation_Assumptions'!$B$4"; cA_pstart <- "'Calculation_Assumptions'!$B$5"
  cA_etgt  <- "'Calculation_Assumptions'!$B$6"; cA_ramp <- "'Calculation_Assumptions'!$B$7"
  cA_disc  <- "'Calculation_Assumptions'!$B$8"; cA_bimpl <- "'Calculation_Assumptions'!$B$14"
  cA_tol   <- "'Calculation_Assumptions'!$B$15"; cA_reslim <- "'Calculation_Assumptions'!$B$16"
  cA_neg   <- "'Calculation_Assumptions'!$B$17"; cA_resid <- "'Calculation_Assumptions'!$B$18"
  cA_bgd   <- "'Calculation_Assumptions'!$B$19"; cA_rda <- "'Calculation_Assumptions'!$B$20"
  cA_rdic  <- "'Calculation_Assumptions'!$B$21"; cA_rcpd <- "'Calculation_Assumptions'!$B$22"
  cA_anch  <- "'Calculation_Assumptions'!$B$23"

  ## ===== README ==========================================================
  readme <- data.table(section = c(
    "Purpose","How to read","Scenarios","Baseline pairing","Effect model","Exposure path",
    "Lag","Costing","Shared costs","Budget impact","Cost-effectiveness","Economic value",
    "QA & reconciliation","Colour legend","Deferred"),
    detail = c(
    "Costing, budget impact and mortality-based cost-effectiveness for the public-health (fiscal/regulatory) policies selected in the public-health input workbook.",
    "Grey cells are R-generated source values; light-blue cells are LIVE Excel formulas; pale-yellow cells on Calculation_Assumptions are editable controls. Change a yellow control and the blue results recompute. Calculation_Map lists the dependency chain.",
    "Baseline + one standalone scenario per runnable intervention + one JOINT scenario per parent package (tobacco, salt) + a combined 'all_public_health' scenario. Membership and the tobacco/salt package structure derive from the workbook Scenario_Hierarchy (Model 04). Parent-package cases/deaths come from a single joint run, never summed from standalone children; package cost = sum of selected child costs. See Scenario_Hierarchy, Child_Intervention_Summary and Parent_Package_Summary.",
    "Deaths averted = baseline deaths - scenario deaths, matched at year x cause (both-sex totals).",
    "Incidence effect = exposure-based: prevalence-shift RR (tobacco), log-linear RR per unit reduction (alcohol/sodium/SSB and the DEFAULT industrial-TFA path, RR per 1 percentage-point energy). The optional TFA PAF path (PAF x implementation gap) is used only when Assumptions.tfa_effect_method='PAF'. Fiscal levers use baseline->target tax change x price elasticity; regulatory levers use the none/partial/full implementation gap. NO clinical coverage-adjustment formula is used.",
    "Achieved exposure pt(t) ramps linearly baseline->target over start_year..target_year, floored; exposure reductions are shown as formulas on Exposure_Targets.",
    "immediate_after_full_implementation: effect tracks the exposure path. delayed_exponential_remaining_effect (tobacco): full target effect accrues as 1-(1-rate)^(years since start).",
    "annual_cost = population(t) x PIN fraction x implementation_fraction(t) x frequency x unit_cost. Public-wide policies use the deduplicated total population (once per age/sex/year, never per cause).",
    "Shared policy costs (cost_scope 'shared-count-once', ...__C_SHARED) are counted once per intervention/scenario/year, never once per affected cause.",
    "Budget impact reports UNDISCOUNTED baseline, scenario, incremental and cumulative incremental cost; discounted incremental cost is a separate column.",
    "USD per death averted = cumulative discounted incremental cost / cumulative (undiscounted) deaths averted. Not a DALY-based ICER; DALYs are deferred.",
    "Value of statistical life (Model 08) covers the clinical CVD scenarios only and does not reconcile with public-health scenarios; see the Economic_Value note.",
    "QA_Checks recomputes each invariant in Excel and reconciles the Excel cost-effectiveness headline against the R engine values on Calculation_Assumptions. PASS/REVIEW/FAIL are conditionally formatted.",
    "Header dark-blue; formula-derived light-blue; R source/helper grey; editable controls pale-yellow; PASS green; REVIEW amber; FAIL red.",
    "DALYs/YLL/YLD/disability weights are deferred; the principal cost-effectiveness result is USD per death averted."))
  addWorksheet(wb, "README")
  writeData(wb, "README", "Indonesia NCD - public-health cost & value workbook (formula edition)", startRow = 1)
  addStyle(wb, "README", st_title, rows = 1, cols = 1)
  writeData(wb, "README", readme, startRow = 3, headerStyle = st_hdr)
  setColWidths(wb, "README", cols = 1:2, widths = c(22, 122))
  addStyle(wb, "README", st_wrap, rows = 4:(nrow(readme) + 3), cols = 2, gridExpand = TRUE, stack = TRUE)
  setRowHeights(wb, "README", rows = 1, heights = 22)

  ## ===== Run_Metadata ====================================================
  meta <- data.table(item = c(
    "Workbook title","Run date","Model / pipeline","Public-health input workbook",
    "Model output source","Location","Analysis years","Baseline scenario",
    "Scenarios costed","Cost discount rate","Reporting price year","Currency",
    "Economic perspective","Policy start / target year","Cost ramp years",
    "Health outcomes (DALYs/YLL/YLD)","R version","openxlsx / data.table",
    "TFA effect method","Public-health scenarios (n)"),
    value = c("Indonesia NCD public-health - cost & value", as.character(Sys.Date()),
    "CVD FAIR Choices pipeline (Models 00-06 -> 09), public-health family",
    phi$inputs_path, "output/out_model/model_output_*.rds (Model 06)", loc_run,
    paste0(yr_start, "-", yr_end), base_id, paste(comparators, collapse = ", "),
    sprintf("%.1f%%", 100 * disc_rate), as.character(PA$cost_price_year), PA$currency,
    PA$economic_perspective, paste0(policy_start, " / ", PA$exposure_target_year),
    as.character(ramp_years), "Out of scope in this stage (deferred)",
    R.version.string, paste0(as.character(packageVersion("openxlsx")), " / ",
                             as.character(packageVersion("data.table"))),
    as.character(PA$tfa_effect_method %||% "RR"),
    paste0(length(comparators), " comparators + baseline")))
  addWorksheet(wb, "Run_Metadata")
  writeData(wb, "Run_Metadata", meta, headerStyle = st_hdr)
  writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 8,
               x = paste0(cA_start, "&\"-\"&", cA_end))
  writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 9, x = cA_base)
  writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 11, x = cA_disc)
  addStyle(wb, "Run_Metadata", st_formula, rows = c(8,9,11), cols = 2, stack = TRUE)
  addStyle(wb, "Run_Metadata", createStyle(numFmt = "0.0%"), rows = 11, cols = 2, stack = TRUE)
  setColWidths(wb, "Run_Metadata", cols = 1:2, widths = c(30, 74))
  freezePane(wb, "Run_Metadata", firstActiveRow = 2); setRowHeights(wb, "Run_Metadata", rows = 1, heights = 28)

  ## ===== Selected_Interventions ==========================================
  n_si <- nrow(sel_out); r_si <- n_si + 1L
  addWorksheet(wb, "Selected_Interventions")
  writeData(wb, "Selected_Interventions", as.data.frame(sel_out), headerStyle = st_hdr)
  # P full_effect (col16) model-specific; Q abs (17)=H-I; R rel (18)=(H-I)/H; U key_count(21); V status(22)
  fe_formula <- function(r) {
    m <- sel_out$effect_model[r - 1L]
    if (identical(m, "direct_smoking_prevalence_shift_rr"))
      sprintf("1-(1+I%d*(J%d-1))/(1+H%d*(J%d-1))", r, r, r, r)
    else if (identical(m, "direct_loglinear_rr_per_unit_reduction"))
      sprintf("1-1/(J%d^(H%d-I%d))", r, r, r)
    else sprintf("IFERROR(K%d*J%d,0)", r, r)
  }
  writeFormula(wb, "Selected_Interventions", startCol = 16, startRow = 2, x = frows(fe_formula, 2:r_si))
  writeFormula(wb, "Selected_Interventions", startCol = 17, startRow = 2, x = frows(function(r) sprintf("H%d-I%d", r, r), 2:r_si))
  writeFormula(wb, "Selected_Interventions", startCol = 18, startRow = 2, x = frows(function(r) sprintf("IF(H%d=0,0,(H%d-I%d)/H%d)", r, r, r, r), 2:r_si))
  writeFormula(wb, "Selected_Interventions", startCol = 21, startRow = 2, x = frows(function(r) sprintf("COUNTIF($B$2:$B$%d,B%d)", r_si, r), 2:r_si))
  writeFormula(wb, "Selected_Interventions", startCol = 22, startRow = 2, x = frows(function(r) sprintf("IF(U%d=1,\"OK\",\"DUPLICATE KEY\")", r), 2:r_si))
  style_sheet("Selected_Interventions", names(sel_out), n_si, formula_cols = c(16,17,18,21,22))

  ## ===== Blocked_Links ===================================================
  addWorksheet(wb, "Blocked_Links")
  if (nrow(blocked_out)) {
    writeData(wb, "Blocked_Links", as.data.frame(blocked_out), headerStyle = st_hdr)
    style_sheet("Blocked_Links", names(blocked_out), nrow(blocked_out),
                wrap_cols = which(names(blocked_out) == "problem"))
    addStyle(wb, "Blocked_Links", st_input, rows = 2:(nrow(blocked_out)+1),
             cols = which(names(blocked_out) == "problem"), gridExpand = TRUE, stack = TRUE)
  } else {
    writeData(wb, "Blocked_Links", data.frame(note = "No blocked public-health links in this run."),
              headerStyle = st_hdr)
    setColWidths(wb, "Blocked_Links", cols = 1, widths = 80)
  }

  ## ===== Policy_Levers ===================================================
  n_lv <- nrow(lev_out); r_lv <- max(n_lv + 1L, 2L)
  addWorksheet(wb, "Policy_Levers")
  writeData(wb, "Policy_Levers", as.data.frame(lev_out), headerStyle = st_hdr)
  # Derived fiscal/regulatory cells are LIVE formulas (M04-M06); targets by name.
  Lm  <- xlc(names(lev_out), "lever_method");             Lfb <- xlc(names(lev_out), "fiscal_baseline_tax_level")
  Lft <- xlc(names(lev_out), "fiscal_target_tax_level");  Lrb <- xlc(names(lev_out), "regulatory_baseline_score")
  Lrt <- xlc(names(lev_out), "regulatory_target_score");  Lep <- xlc(names(lev_out), "effect_parameter")
  Lig <- xlc(names(lev_out), "implementation_gap");       Lipc<- xlc(names(lev_out), "implied_price_change")
  Ltd <- xlc(names(lev_out), "fiscal_tax_delta")
  cig <- match("implementation_gap", names(lev_out)); cipc <- match("implied_price_change", names(lev_out))
  ctd <- match("fiscal_tax_delta", names(lev_out));   cpr  <- match("policy_reduction", names(lev_out))
  if (n_lv > 0) {
    R <- 2:r_lv
    writeFormula(wb, "Policy_Levers", startCol = cig, startRow = 2, x = frows(function(r)
      sprintf("IF(%s%d=\"regulatory_gap_multiplicative\",MAX(0,%s%d-%s%d),\"\")", Lm,r,Lrt,r,Lrb,r), R))
    writeFormula(wb, "Policy_Levers", startCol = ctd, startRow = 2, x = frows(function(r)
      sprintf("IF(%s%d=\"price_elasticity\",MAX(0,%s%d-%s%d),\"\")", Lm,r,Lft,r,Lfb,r), R))
    writeFormula(wb, "Policy_Levers", startCol = cipc, startRow = 2, x = frows(function(r)
      sprintf("IF(%s%d=\"tax_share_to_price_elasticity\",(1-%s%d)/(1-%s%d)-1,IF(%s%d=\"price_elasticity\",%s%d,\"\"))",
              Lm,r,Lfb,r,Lft,r,Lm,r,Ltd,r), R))
    writeFormula(wb, "Policy_Levers", startCol = cpr, startRow = 2, x = frows(function(r)
      sprintf("IF(%s%d=\"regulatory_gap_multiplicative\",%s%d*%s%d,IF(OR(%s%d=\"price_elasticity\",%s%d=\"tax_share_to_price_elasticity\"),ABS(%s%d)*%s%d,\"\"))",
              Lm,r,Lep,r,Lig,r, Lm,r,Lm,r, Lep,r,Lipc,r), R))
  }
  style_sheet("Policy_Levers", names(lev_out), n_lv, formula_cols = c(cig,cipc,ctd,cpr),
              rsource_cols = setdiff(seq_along(lev_out), c(cig,cipc,ctd,cpr)))
  rev_c <- which(names(lev_out) == "review_status")
  if (length(rev_c) && n_lv) {
    conditionalFormatting(wb, "Policy_Levers", cols = rev_c, rows = 2:r_lv, rule = "Ready", type = "contains", style = cf_pass)
    conditionalFormatting(wb, "Policy_Levers", cols = rev_c, rows = 2:r_lv, rule = "Review", type = "contains", style = cf_rev)
    conditionalFormatting(wb, "Policy_Levers", cols = rev_c, rows = 2:r_lv, rule = "Missing", type = "contains", style = cf_rev)
  }

  ## ===== Exposure_Targets ================================================
  n_ex <- nrow(expo_out); r_ex <- n_ex + 1L
  addWorksheet(wb, "Exposure_Targets")
  writeData(wb, "Exposure_Targets", as.data.frame(expo_out), headerStyle = st_hdr)
  # E baseline(5) F method(6) G red_or_target(7) H floor(8) I target(9) J abs(10) K rel(11)
  tgt_formula <- function(r) {
    meth <- tolower(expo_out$reduction_method[r - 1L])
    if (meth == "relative") sprintf("MAX(H%d,E%d*(1-G%d))", r, r, r)
    else if (meth == "absolute") sprintf("MAX(H%d,E%d-G%d)", r, r, r)
    else sprintf("MAX(H%d,G%d)", r, r)
  }
  writeFormula(wb, "Exposure_Targets", startCol = 9, startRow = 2, x = frows(tgt_formula, 2:r_ex))
  writeFormula(wb, "Exposure_Targets", startCol = 10, startRow = 2, x = frows(function(r) sprintf("E%d-I%d", r, r), 2:r_ex))
  writeFormula(wb, "Exposure_Targets", startCol = 11, startRow = 2, x = frows(function(r) sprintf("IF(E%d=0,0,(E%d-I%d)/E%d)", r, r, r, r), 2:r_ex))
  style_sheet("Exposure_Targets", names(expo_out), n_ex, formula_cols = c(9,10,11))

  ## ===== Effect_Parameters ===============================================
  n_ef <- nrow(eff_out); r_ef <- n_ef + 1L
  addWorksheet(wb, "Effect_Parameters")
  writeData(wb, "Effect_Parameters", as.data.frame(eff_out), headerStyle = st_hdr)
  # E model(5) G response(7) H paf(8) K baseline(11) L target(12) M full_effect(13)
  fe2 <- function(r) {
    m <- eff_out$effect_model[r - 1L]
    if (identical(m, "direct_smoking_prevalence_shift_rr"))
      sprintf("1-(1+L%d*(G%d-1))/(1+K%d*(G%d-1))", r, r, r, r)
    else if (identical(m, "direct_loglinear_rr_per_unit_reduction"))
      sprintf("1-1/(G%d^(K%d-L%d))", r, r, r)
    else sprintf("IFERROR(H%d*G%d,0)", r, r)
  }
  writeFormula(wb, "Effect_Parameters", startCol = 13, startRow = 2, x = frows(fe2, 2:r_ef))
  style_sheet("Effect_Parameters", names(eff_out), n_ef, formula_cols = 13)
  vcol <- which(names(eff_out) == "valid")
  conditionalFormatting(wb, "Effect_Parameters", cols = vcol, rows = 2:r_ef, rule = "==0", style = cf_rev)

  ## ===== Cost_Components =================================================
  n_cc <- nrow(cc_out); r_cc <- n_cc + 1L
  addWorksheet(wb, "Cost_Components")
  writeData(wb, "Cost_Components", as.data.frame(cc_out), headerStyle = st_hdr)
  # Formula targets derived BY NAME so the new allocation columns cannot misalign
  # them. allocated_child_cost = package total x allocation share (M13, falling back
  # to the row unit cost); cost_ready checks unit cost, PIN fraction and PIN measure.
  Cpm <- xlc(names(cc_out), "population_in_need_measure")
  Cpf <- xlc(names(cc_out), "population_in_need_fraction")
  Cuc <- xlc(names(cc_out), "unit_cost_usd")
  Cshare <- xlc(names(cc_out), "cost_allocation_share")
  Cptot  <- xlc(names(cc_out), "package_total_cost_usd_per_capita")
  c_alloc <- match("allocated_child_cost", names(cc_out))
  c_ready <- match("cost_ready", names(cc_out))
  if (n_cc > 0) {
    writeFormula(wb, "Cost_Components", startCol = c_alloc, startRow = 2, x = frows(function(r)
      sprintf("IFERROR(%s%d*%s%d,%s%d)", Cptot, r, Cshare, r, Cuc, r), 2:r_cc))
    writeFormula(wb, "Cost_Components", startCol = c_ready, startRow = 2, x = frows(function(r)
      sprintf("IF(AND(%s%d>=0,%s%d<>\"\",%s%d>=0,%s%d<=1,OR(%s%d=\"all\",%s%d=\"prevalence\",%s%d=\"incidence\")),1,0)",
              Cuc, r, Cpf, r, Cpf, r, Cpf, r, Cpm, r, Cpm, r, Cpm, r), 2:r_cc))
  }
  style_sheet("Cost_Components", names(cc_out), n_cc, formula_cols = c(c_alloc, c_ready),
              wrap_cols = which(names(cc_out) %in% c("cost_component","notes")))
  ia_c <- which(names(cc_out) == "indonesia_adjusted_flag")
  conditionalFormatting(wb, "Cost_Components", cols = ia_c, rows = 2:r_cc, rule = "==0", style = cf_rev)

  ## ===== Annual_Mortality ================================================
  am <- copy(mort); am[, `:=`(deaths_averted = NA_real_, cases_averted = NA_real_)]
  n_am <- nrow(am); r_am <- n_am + 1L
  addWorksheet(wb, "Annual_Mortality")
  writeData(wb, "Annual_Mortality", as.data.frame(am), headerStyle = st_hdr)
  # A scn B label C year D cause E cases F cause_deaths G base_cases H base_deaths I averted J cases_averted
  writeFormula(wb, "Annual_Mortality", startCol = 9, startRow = 2, x = frows(function(r) sprintf("H%d-F%d", r, r), 2:r_am))
  writeFormula(wb, "Annual_Mortality", startCol = 10, startRow = 2, x = frows(function(r) sprintf("G%d-E%d", r, r), 2:r_am))
  style_sheet("Annual_Mortality", names(am), n_am, formula_cols = c(9,10), rsource_cols = c(5,6,7,8))

  ## ===== Annual_Cost =====================================================
  ac_cols <- c("scenario","year","intervention_id","cost_record_id","cost_component_key",
               "cost_join_key","cost_scope","population_in_need_measure","population_in_need_fraction",
               "frequency_per_year","unit_cost_usd","pop_scenario","pop_baseline","cost_impl_frac",
               "pin_scenario","pin_baseline","annual_cost_baseline","annual_cost_scenario",
               "annual_cost_incremental","discount_factor","disc_cost_baseline","disc_cost_scenario",
               "disc_cost_incremental","indonesia_adjusted_flag","price_year","review_status",
               "shared_duplicate_count")
  n_ac <- nrow(annual_cost); r_ac <- max(n_ac + 1L, 2L)
  if (n_ac > 0) {
    ac <- data.frame(scenario = annual_cost$scenario, year = annual_cost$year,
                     intervention_id = annual_cost$intervention_id, cost_record_id = annual_cost$cost_record_id,
                     cost_component_key = annual_cost$cost_component_key, cost_join_key = annual_cost$cost_join_key,
                     cost_scope = annual_cost$cost_scope, population_in_need_measure = annual_cost$population_in_need_measure,
                     population_in_need_fraction = annual_cost$population_in_need_fraction,
                     frequency_per_year = annual_cost$frequency_per_year, unit_cost_usd = annual_cost$unit_cost_usd,
                     pop_scenario = annual_cost$pop_scenario, pop_baseline = annual_cost$pop_baseline,
                     stringsAsFactors = FALSE)
    for (cn in ac_cols[14:23]) ac[[cn]] <- NA_real_
    ac$indonesia_adjusted_flag <- annual_cost$indonesia_adjusted_flag
    ac$price_year <- annual_cost$price_year
    ac$review_status <- annual_cost$review_status
    ac$shared_duplicate_count <- NA_real_
    ac <- ac[, ac_cols]
  } else ac <- as.data.frame(setNames(replicate(length(ac_cols), logical(0), simplify = FALSE), ac_cols))
  addWorksheet(wb, "Annual_Cost")
  writeData(wb, "Annual_Cost", ac, headerStyle = st_hdr)
  if (n_ac > 0) {
    R <- 2:r_ac; wf <- function(col, fn) writeFormula(wb, "Annual_Cost", startCol = col, startRow = 2, x = frows(fn, R))
    wf(14, function(r) sprintf("MIN(MAX((B%d-%s+1)/%s,0),1)", r, cA_pstart, cA_ramp))     # cost_impl_frac
    wf(15, function(r) sprintf("L%d*I%d", r, r))                                          # pin_scenario
    wf(16, function(r) sprintf("M%d*I%d", r, r))                                          # pin_baseline
    wf(17, function(r) sprintf("P%d*%s*J%d*K%d", r, cA_bimpl, r, r))                      # annual_cost_baseline
    wf(18, function(r) sprintf("O%d*N%d*J%d*K%d", r, r, r, r))                            # annual_cost_scenario
    wf(19, function(r) sprintf("R%d-Q%d", r, r))                                          # annual_cost_incremental
    wf(20, function(r) sprintf("1/(1+%s)^(B%d-%s)", cA_disc, r, cA_start))                # discount_factor
    wf(21, function(r) sprintf("Q%d*T%d", r, r))                                          # disc_cost_baseline
    wf(22, function(r) sprintf("R%d*T%d", r, r))                                          # disc_cost_scenario
    wf(23, function(r) sprintf("S%d*T%d", r, r))                                          # disc_cost_incremental
    wf(27, function(r) sprintf("IF(G%d=\"shared-count-once\",COUNTIFS($A$2:$A$%d,A%d,$B$2:$B$%d,B%d,$D$2:$D$%d,D%d),1)",
                               r, r_ac, r, r_ac, r, r_ac, r))                             # shared_duplicate_count
  }
  style_sheet("Annual_Cost", ac_cols, n_ac, formula_cols = c(14:23, 27), rsource_cols = c(9,10,11,12,13))

  ## ===== Budget_Impact ===================================================
  bud_cols <- c("scenario","year","baseline_cost","scenario_cost","incremental_cost",
                "disc_incremental_cost","cumulative_incremental_cost","cumulative_disc_incremental_cost")
  n_bi <- nrow(bi); r_bi <- max(n_bi + 1L, 2L)
  if (n_bi > 0) { bud <- data.frame(scenario = bi$scenario, year = bi$year, stringsAsFactors = FALSE)
    for (cn in bud_cols[3:8]) bud[[cn]] <- NA_real_
  } else bud <- as.data.frame(setNames(replicate(8, logical(0), simplify = FALSE), bud_cols))
  addWorksheet(wb, "Budget_Impact")
  writeData(wb, "Budget_Impact", bud, headerStyle = st_hdr)
  if (n_bi > 0) {
    R <- 2:r_bi
    sac <- function(tgt, r) sprintf("SUMIFS('Annual_Cost'!$%s$2:$%s$%d,'Annual_Cost'!$A$2:$A$%d,A%d,'Annual_Cost'!$B$2:$B$%d,B%d)",
                                    tgt, tgt, r_ac, r_ac, r, r_ac, r)
    writeFormula(wb, "Budget_Impact", startCol = 3, startRow = 2, x = frows(function(r) sac("Q", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 4, startRow = 2, x = frows(function(r) sac("R", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 5, startRow = 2, x = frows(function(r) sac("S", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 6, startRow = 2, x = frows(function(r) sac("W", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 7, startRow = 2, x = frows(function(r) sprintf("SUMIFS($E$2:E%d,$A$2:A%d,A%d)", r, r, r), R))
    writeFormula(wb, "Budget_Impact", startCol = 8, startRow = 2, x = frows(function(r) sprintf("SUMIFS($F$2:F%d,$A$2:A%d,A%d)", r, r, r), R))
  }
  style_sheet("Budget_Impact", bud_cols, n_bi, formula_cols = 3:8)

  ## ===== Cost_Effectiveness ==============================================
  ce_cols <- c("scenario","scenario_label","deaths_averted","cases_averted","incremental_cost",
               "disc_incremental_cost","cost_per_death_averted","dominance","reconciliation_status")
  n_ce <- nrow(cea); r_ce <- max(n_ce + 1L, 2L)
  ce <- data.frame(scenario = cea$scenario, scenario_label = cea$scenario_label, stringsAsFactors = FALSE)
  for (cn in ce_cols[3:7]) ce[[cn]] <- NA_real_
  ce$dominance <- NA_character_; ce$reconciliation_status <- NA_character_
  addWorksheet(wb, "Cost_Effectiveness")
  writeData(wb, "Cost_Effectiveness", ce, headerStyle = st_hdr)
  if (n_ce > 0) {
    R <- 2:r_ce
    writeFormula(wb, "Cost_Effectiveness", startCol = 3, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Annual_Mortality'!$I$2:$I$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", r_am, r_am, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 4, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Annual_Mortality'!$J$2:$J$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", r_am, r_am, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 5, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Budget_Impact'!$E$2:$E$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 6, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Budget_Impact'!$F$2:$F$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 7, startRow = 2, x = frows(function(r)
      sprintf("IF(C%d>0,F%d/C%d,\"\")", r, r, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 8, startRow = 2, x = frows(function(r)
      sprintf("IF(AND(C%d>0,F%d<0),\"Dominant (more health, lower cost)\",IF(AND(C%d<=0,F%d>0),\"Dominated (less/no health, higher cost)\",IF(AND(C%d<=0,F%d<=0),\"No deaths averted; ratio not defined\",\"USD per death averted\")))",
              r, r, r, r, r, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 9, startRow = 2, x = frows(function(r)
      sprintf("IF(AND(ABS(F%d-SUMIFS('Budget_Impact'!$F$2:$F$%d,'Budget_Impact'!$A$2:$A$%d,A%d))<=%s,ABS(C%d-SUMIFS('Annual_Mortality'!$I$2:$I$%d,'Annual_Mortality'!$A$2:$A$%d,A%d))<=%s),\"consistent\",\"mismatch\")",
              r, r_bi, r_bi, r, cA_tol, r, r_am, r_am, r, cA_tol), R))
  }
  style_sheet("Cost_Effectiveness", ce_cols, n_ce, formula_cols = 3:9, wrap_cols = c(2, 8))
  conditionalFormatting(wb, "Cost_Effectiveness", cols = 9, rows = 2:r_ce, rule = "consistent", type = "contains", style = cf_pass)
  conditionalFormatting(wb, "Cost_Effectiveness", cols = 9, rows = 2:r_ce, rule = "mismatch",   type = "contains", style = cf_fail)
  conditionalFormatting(wb, "Cost_Effectiveness", cols = 8, rows = 2:r_ce, rule = "Dominated",  type = "contains", style = cf_rev)

  ## ===== Economic_Value (note) ==========================================
  addWorksheet(wb, "Economic_Value")
  ev_note <- data.table(note = c(
    "Economic value (value of statistical life, VSL/VSLY) is produced by Model 08 for the clinical CVD scenarios only.",
    "Model 08 does not currently run the public-health scenario ids (I_PH_* / all_public_health), so its monetary-value",
    "results cannot be reconciled against this run's public-health scenarios and are intentionally NOT reproduced here.",
    "The principal public-health result in this workbook is USD per death averted (Cost_Effectiveness).",
    "To populate a public-health economic-value view, re-run Model 08 on the public-health scenarios and extend this sheet."))
  writeData(wb, "Economic_Value", ev_note, headerStyle = st_hdr)
  addStyle(wb, "Economic_Value", st_wrap, rows = 2:(nrow(ev_note)+1), cols = 1, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, "Economic_Value", cols = 1, widths = 120)
  freezePane(wb, "Economic_Value", firstActiveRow = 2)

  ## ===== QA_Checks =======================================================
  qa_check <- c("Selected intervention-cause key uniqueness","Workbook FAIL-level issues",
                "Workbook REVIEW-level issues","Every scenario paired to baseline",
                "Public-health transitions are well -> sick incidence","No impossible negative states",
                "Stock/flow identity pop = well + sick + all-cause deaths",
                "Background mortality constant across cause","Cost reconciliation (components -> budget impact)",
                "Shared cost counted once per stratum/year","Annual reconciles to cumulative (budget impact)",
                "CEA reconciliation (detail -> summary)","Excel vs R: deaths averted (anchor)",
                "Excel vs R: discounted incremental cost (anchor)","Excel vs R: cost per death averted (anchor)")
  qa_expect <- c("0","0","0","0","0","0","<= limit","1","<= tol","0","<= tol","consistent",
                 "match R","match R","match R")
  qa_note <- c("Each selected link key appears once",
               "Blocked links excluded (see Blocked_Links / Input_Diagnostic)",
               "Flagged but usable (e.g. cost not Indonesia-adjusted; provisional PAF)",
               "Deaths averted = baseline - scenario at matched year/cause",
               "All modeled public-health effects map to incidence (no case fatality)",
               "well/sick/new_cases/deaths/population >= 0",
               "Per cause row; small residual from 95+ pooling / rounding",
               "all.mx taken once per stratum (population not duplicated across causes)",
               "Excel component rows sum to Excel annual totals within tolerance",
               "shared-count-once components appear once per scenario-year",
               "Last-year cumulative equals the sum of annual incremental cost",
               "Every Cost_Effectiveness row's internal reconciliation is consistent",
               "Excel CEA deaths averted reconciles to the R engine value",
               "Excel CEA discounted incremental cost reconciles to the R engine value",
               "Excel CEA USD per death averted reconciles to the R engine value")
  qa_actual <- c(
    sprintf("COUNTIF('Selected_Interventions'!$U$2:$U$%d,\">1\")", r_si),
    sprintf("COUNTIF('Input_Diagnostic'!$E$2:$E$%d,\"FAIL\")", max(nrow(diag_out)+1L,2L)),
    sprintf("COUNTIF('Input_Diagnostic'!$E$2:$E$%d,\"REVIEW\")", max(nrow(diag_out)+1L,2L)),
    sprintf("COUNTBLANK('Annual_Mortality'!$H$2:$H$%d)", r_am),
    sprintf("COUNTIF('Effect_Parameters'!$N$2:$N$%d,\"<>incidence\")", r_ef),
    cA_neg, cA_resid, cA_bgd,
    sprintf("ABS(SUM('Budget_Impact'!$D$2:$D$%d)-SUM('Annual_Cost'!$R$2:$R$%d))+ABS(SUM('Budget_Impact'!$C$2:$C$%d)-SUM('Annual_Cost'!$Q$2:$Q$%d))",
            r_bi, r_ac, r_bi, r_ac),
    sprintf("COUNTIF('Annual_Cost'!$AA$2:$AA$%d,\">1\")", r_ac),
    # check 11 (annual -> cumulative): sum of last-year cumulative == total incremental
    sprintf("ABS(SUMIFS('Budget_Impact'!$G$2:$G$%d,'Budget_Impact'!$B$2:$B$%d,%s)-SUM('Budget_Impact'!$E$2:$E$%d))",
            r_bi, r_bi, cA_end, r_bi),
    sprintf("IF(COUNTIF('Cost_Effectiveness'!$I$2:$I$%d,\"mismatch\")=0,\"consistent\",\"mismatch\")", r_ce),
    sprintf("INDEX('Cost_Effectiveness'!$C$2:$C$%d,MATCH(%s,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, cA_anch, r_ce),
    sprintf("INDEX('Cost_Effectiveness'!$F$2:$F$%d,MATCH(%s,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, cA_anch, r_ce),
    sprintf("INDEX('Cost_Effectiveness'!$G$2:$G$%d,MATCH(%s,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, cA_anch, r_ce))
  qa_status <- c(
    "IF(C2=0,\"PASS\",\"FAIL\")","IF(C3=0,\"PASS\",\"REVIEW\")","IF(C4=0,\"PASS\",\"REVIEW\")",
    "IF(C5=0,\"PASS\",\"FAIL\")","IF(C6=0,\"PASS\",\"FAIL\")","IF(C7=0,\"PASS\",\"FAIL\")",
    sprintf("IF(C8<=%s,\"PASS\",\"REVIEW\")", cA_reslim),
    "IF(C9=1,\"PASS\",\"FAIL\")",
    sprintf("IF(C10<=%s,\"PASS\",\"FAIL\")", cA_tol),
    "IF(C11=0,\"PASS\",\"FAIL\")",
    sprintf("IF(C12<=%s,\"PASS\",\"FAIL\")", cA_tol),
    "IF(C13=\"consistent\",\"PASS\",\"FAIL\")",
    sprintf("IF(ABS(C14-%s)<=ABS(%s)*0.000001+0.5,\"PASS\",\"FAIL\")", cA_rda, cA_rda),
    sprintf("IF(ABS(C15-%s)<=ABS(%s)*0.000001+1,\"PASS\",\"FAIL\")", cA_rdic, cA_rdic),
    sprintf("IF(ABS(C16-%s)<=ABS(%s)*0.000001+1,\"PASS\",\"FAIL\")", cA_rcpd, cA_rcpd))
  # --- appended hierarchy / cost-allocation checks (Scenario_Hierarchy, Cost_Components,
  # Annual_Mortality all exist in the final workbook; letters/rows derived by name) ---
  r_sh   <- nrow(phi$hierarchy) + 1L
  pk_all <- phi$package_ids
  tob_pk <- c(pk_all[grepl("TOB", pk_all)],  "I_PH_TOBACCO_POLICY")[1]
  salt_pk<- c(pk_all[grepl("SALT", pk_all)], "I_PH_SALT_POLICY")[1]
  SHpar  <- xlc(names(phi$hierarchy), "parent_scenario_id"); SHrole <- xlc(names(phi$hierarchy), "scenario_role")
  Ccpp   <- xlc(names(cc_out), "parent_package_id");         Ccsh   <- xlc(names(cc_out), "cost_allocation_share")
  qa_check  <- c(qa_check, "Tobacco package child count","Salt package child count",
                 "Child cost-allocation shares sum to 1 (per package)","Parent packages produced as joint runs")
  qa_expect <- c(qa_expect, "3","4","<= tol","> 0")
  qa_note   <- c(qa_note, "Scenario_Hierarchy: clean air, media, ad ban",
                 "Scenario_Hierarchy: reformulation, FOPL, media, institutions",
                 "Cost_Components allocation shares sum to 1 within each package (M13/M14)",
                 "Package cases/deaths come from a produced JOINT scenario, never summed from children")
  qa_actual <- c(qa_actual,
    sprintf("COUNTIFS('Scenario_Hierarchy'!$%s$2:$%s$%d,\"%s\",'Scenario_Hierarchy'!$%s$2:$%s$%d,\"child\")",
            SHpar, SHpar, r_sh, tob_pk, SHrole, SHrole, r_sh),
    sprintf("COUNTIFS('Scenario_Hierarchy'!$%s$2:$%s$%d,\"%s\",'Scenario_Hierarchy'!$%s$2:$%s$%d,\"child\")",
            SHpar, SHpar, r_sh, salt_pk, SHrole, SHrole, r_sh),
    sprintf("ABS(SUMIFS('Cost_Components'!$%s$2:$%s$%d,'Cost_Components'!$%s$2:$%s$%d,\"%s\")-1)+ABS(SUMIFS('Cost_Components'!$%s$2:$%s$%d,'Cost_Components'!$%s$2:$%s$%d,\"%s\")-1)",
            Ccsh, Ccsh, r_cc, Ccpp, Ccpp, r_cc, tob_pk, Ccsh, Ccsh, r_cc, Ccpp, Ccpp, r_cc, salt_pk),
    sprintf("MIN(COUNTIF('Annual_Mortality'!$A$2:$A$%d,\"%s\"),COUNTIF('Annual_Mortality'!$A$2:$A$%d,\"%s\"))",
            r_am, tob_pk, r_am, salt_pk))
  qa_status <- c(qa_status,
    "IF(C17=3,\"PASS\",\"FAIL\")","IF(C18=4,\"PASS\",\"FAIL\")",
    sprintf("IF(C19<=%s,\"PASS\",\"FAIL\")", cA_tol),
    "IF(C20>0,\"PASS\",\"FAIL\")")
  qa_df <- data.frame(check = qa_check, expected = qa_expect, actual = NA,
                      status = NA_character_, note = qa_note, stringsAsFactors = FALSE)
  addWorksheet(wb, "QA_Checks")
  writeData(wb, "QA_Checks", qa_df, headerStyle = st_hdr)
  writeFormula(wb, "QA_Checks", startCol = 3, startRow = 2, x = qa_actual)
  writeFormula(wb, "QA_Checks", startCol = 4, startRow = 2, x = qa_status)
  n_qa <- nrow(qa_df); r_qa <- n_qa + 1L
  style_sheet("QA_Checks", names(qa_df), n_qa, formula_cols = c(3,4), wrap_cols = 5)
  conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "PASS",   type = "contains", style = cf_pass)
  conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "FAIL",   type = "contains", style = cf_fail)
  conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "REVIEW", type = "contains", style = cf_rev)

  ## ===== Input_Diagnostic ================================================
  addWorksheet(wb, "Input_Diagnostic")
  n_id <- nrow(diag_out); r_id <- max(n_id + 1L, 2L)
  if (n_id > 0) {
    writeData(wb, "Input_Diagnostic", as.data.frame(diag_out), headerStyle = st_hdr)
    style_sheet("Input_Diagnostic", names(diag_out), n_id, wrap_cols = which(names(diag_out) == "problem"))
    sev_col <- which(names(diag_out) == "severity")
    conditionalFormatting(wb, "Input_Diagnostic", cols = sev_col, rows = 2:r_id, rule = "FAIL",   type = "contains", style = cf_fail)
    conditionalFormatting(wb, "Input_Diagnostic", cols = sev_col, rows = 2:r_id, rule = "REVIEW", type = "contains", style = cf_rev)
  } else {
    writeData(wb, "Input_Diagnostic", data.frame(scope=character(0), item_key=character(0),
              field=character(0), problem=character(0), severity=character(0)), headerStyle = st_hdr)
    addStyle(wb, "Input_Diagnostic", st_hdr, rows = 1, cols = 1:5, gridExpand = TRUE)
  }

  ## ===== Methods_and_Sources =============================================
  methods <- data.table(
    method_id = sprintf("M%02d", 1:19),
    concept = c("Exposure target","Exposure path","Regulatory score","Regulatory effect",
                "Price-change tax","Tax-share tax","Prevalence-shift RR effect","Log-linear RR effect",
                "TFA effect method (RR default / PAF optional)","Effect lag","Incidence application",
                "Policy implementation fraction","Annual policy cost","Shared cost","Child cost allocation",
                "Parent package cost","Package health outcomes","Discounting","Cost-effectiveness"),
    formula_or_rule = c(
      "relative: max(floor, baseline*(1-reduction)); absolute: max(floor, baseline-reduction); level: max(floor, target)",
      "pt(t) linear baseline->target over start_year..target_year, then held; floored at exposure_floor",
      "implementation score: none=0; partial=0.5; full=1",
      "implementation_gap = max(0, target_score - baseline_score); regulatory reduction = full_component_effect * implementation_gap",
      "fiscal_tax_delta = max(0, target_tax - baseline_tax); reduction = abs(price_elasticity) * implied_price_change",
      "tobacco excise share: implied_price_change = (1 - baseline_share)/(1 - target_share) - 1",
      "effect = 1 - (1 + pt*(RR-1)) / (1 + p0*(RR-1))   [tobacco smoking prevalence shift]",
      "effect = 1 - 1/(RR^(p0-pt))   [alcohol, sodium, SSB, and DEFAULT industrial-TFA path: RR per 1 percentage-point energy, e.g. RR_TFA_IHD_1PCT=1.10 ~ sqrt(1.21)]",
      "RR (default): log-linear RR per unit reduction, no PAF required. PAF (optional, tfa_effect_method=PAF): effect = optional_PAF * implementation_gap",
      "immediate: effect tracks exposure path; delayed_exponential: full_effect * (1-(1-rate)^(years since start))",
      "cause-specific incidence x (1 - effect_t); multiple policies combine multiplicatively on the surviving fraction",
      "implementation_fraction(t) = min(max((t - policy_start_year + 1)/policy_cost_ramp_years, 0), 1)",
      "annual_cost = population(t) * PIN_fraction * implementation_fraction(t) * frequency * unit_cost",
      "shared-count-once costs counted once per intervention & scenario-year, never once per affected cause",
      "child cost = package_total_cost_per_capita * cost_allocation_share (shares sum to 1 within a package)",
      "parent package cost = sum of selected child costs; the parent_reference cost row (selected_for_base_case=0) is NOT charged",
      "parent-package cases/deaths come from ONE joint scenario run with all children applied together; never sum standalone child outcomes",
      "discount_factor(t) = 1/(1+cost_discount_rate)^(t - analysis_start_year); costs discounted, deaths undiscounted",
      "USD per death (or case) averted = cumulative discounted incremental cost / cumulative averted; blank + 'no incremental health effect' when nothing averted"),
    source = c(rep("NCD Countdown supplement (Countdown_Methods sheet) + public-health input workbook", 19)))
  # TFA base-case RR references retained for provenance.
  tfa_refs <- data.table(method_id = c("REF","REF"), concept = c("TFA source RR","TFA source RR"),
    formula_or_rule = c("~RR 1.21 per 2 percentage-points energy (converted to ~1.10 per 1 pp in the workbook)",
                        "supporting cohort evidence"),
    source = c("https://www.ahajournals.org/doi/10.1161/CIRCULATIONAHA.118.038160",
               "https://www.ahajournals.org/doi/10.1161/JAHA.115.002891"))
  methods <- rbind(methods, tfa_refs)
  addWorksheet(wb, "Methods_and_Sources")
  writeData(wb, "Methods_and_Sources", methods, headerStyle = st_hdr)
  style_sheet("Methods_and_Sources", names(methods), nrow(methods), wrap_cols = c(3,4), filter = FALSE, max_w = 90)
  setColWidths(wb, "Methods_and_Sources", cols = 1:4, widths = c(10, 28, 92, 56))

  ## ===== Calculation_Map =================================================
  cmap <- data.table(
    output_sheet = c("Policy_Levers","Selected_Interventions","Exposure_Targets","Effect_Parameters",
                     "Cost_Components","Annual_Mortality","Annual_Cost","Budget_Impact","Cost_Effectiveness",
                     "Child_Intervention_Summary","Parent_Package_Summary","QA_Checks","Run_Metadata"),
    formula_columns = c("implementation_gap,implied_price_change,fiscal_tax_delta,policy_reduction",
                        "full_effect,reductions,key_count,status","target,abs,rel","full_effect",
                        "allocated_child_cost,cost_ready","deaths_averted,cases_averted",
                        "impl_frac,PIN,annual+disc cost,shared QA","incremental+cumulative cost",
                        "health,cost,ICER,dominance,reconciliation","modeled/baseline/averted,cost,ICER",
                        "modeled/baseline/averted,cost,ICER","actual,status","B (subset)"),
    depends_on = c("lever_method + fiscal/regulatory inputs","effect_model + exposures","baseline/method/reduction",
                   "effect_model + exposures","cost inputs + allocation shares","R deaths/cases",
                   "Cost_Components; Calculation_Assumptions; R population","Annual_Cost",
                   "Annual_Mortality; Budget_Impact; Calculation_Assumptions",
                   "Annual_Mortality; Budget_Impact (standalone scenarios)",
                   "Annual_Mortality; Budget_Impact (JOINT package/combined runs)",
                   "calculation + diagnostic sheets","Calculation_Assumptions"),
    calculation = c("Regulatory gap, tax price change/delta, reproduced relative exposure reduction",
                    "Full effect at target; exposure reductions; key uniqueness",
                    "Exposure target and absolute/relative reductions","Full effect at target",
                    "Child cost = package total x share; cost-readiness rule","Deaths and cases averted",
                    "Implementation fraction, PIN, annual/discounted cost, shared-cost QA",
                    "Annual and cumulative cost by scenario",
                    "Cumulative health, cost, cost/death, dominance, reconciliation",
                    "Per child intervention: health, cost, cost per case/death averted, status",
                    "Per parent package (joint run) + combined: health, cost, cost per case/death averted",
                    "Invariant recomputation and Excel-vs-R reconciliation","Metadata pulled from controls"))
  addWorksheet(wb, "Calculation_Map")
  writeData(wb, "Calculation_Map", cmap, headerStyle = st_hdr)
  style_sheet("Calculation_Map", names(cmap), nrow(cmap), wrap_cols = c(3,4), filter = FALSE, max_w = 60)
  setColWidths(wb, "Calculation_Map", cols = 1:4, widths = c(22, 30, 40, 52))

  ## ===== Scenario_Hierarchy ==============================================
  # Workbook hierarchy (parent packages <-> child/standalone interventions) with
  # the current run's runnable / parent-package flags. Source (R-generated) view.
  sh_out <- as.data.frame(phi$hierarchy)
  addWorksheet(wb, "Scenario_Hierarchy")
  writeData(wb, "Scenario_Hierarchy", sh_out, headerStyle = st_hdr)
  style_sheet("Scenario_Hierarchy", names(sh_out), nrow(sh_out),
              rsource_cols = seq_along(sh_out),
              wrap_cols = which(names(sh_out) %in% c("parent_aggregation_rule","outcome_reporting_rule",
                                                     "cost_reporting_rule","source_note")))

  ## ===== Risk_Response ===================================================
  rr_out <- as.data.frame(phi$risk_response)
  addWorksheet(wb, "Risk_Response")
  writeData(wb, "Risk_Response", rr_out, headerStyle = st_hdr)
  style_sheet("Risk_Response", names(rr_out), nrow(rr_out), rsource_cols = seq_along(rr_out),
              wrap_cols = which(names(rr_out) %in% c("response_name","derivation","source","notes")))

  ## ===== Child_Intervention_Summary / Parent_Package_Summary =============
  # Per-scenario health + cost + cost-effectiveness, formula-driven from
  # Annual_Mortality and Budget_Impact (package rows come from the JOINT package
  # run, never summed from standalone children -- criteria 10 & 19).
  sc <- as.data.table(phi$scenario_catalogue); setnames(sc, "scenario_id", "scenario")
  sc <- sc[scenario %in% comparators]
  summ_cols <- c("scenario","scenario_label","scenario_level","scenario_role","parent_package_id",
                 "parent_package_name","intervention_ids","component_order",
                 "modeled_cases","baseline_cases","cases_averted","modeled_deaths","baseline_deaths",
                 "deaths_averted","incremental_cost","disc_incremental_cost","cost_per_case_averted",
                 "cost_per_death_averted","status")
  Sca <- xlc(summ_cols, "cases_averted"); Sda <- xlc(summ_cols, "deaths_averted")
  Sdic <- xlc(summ_cols, "disc_incremental_cost")
  write_summary <- function(sheet, dt) {
    d <- as.data.frame(dt[, .(scenario, scenario_label, scenario_level, scenario_role,
                              parent_package_id, parent_package_name, intervention_ids, component_order)],
                       stringsAsFactors = FALSE)
    for (cn in summ_cols[9:16]) d[[cn]] <- NA_real_
    d$cost_per_case_averted <- NA_real_; d$cost_per_death_averted <- NA_real_; d$status <- NA_character_
    d <- d[, summ_cols]; n <- nrow(d); rr2 <- max(n + 1L, 2L)
    addWorksheet(wb, sheet); writeData(wb, sheet, d, headerStyle = st_hdr)
    if (n > 0) {
      R <- 2:rr2
      amf <- function(col) function(r) sprintf(
        "SUMIFS('Annual_Mortality'!$%s$2:$%s$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", col, col, r_am, r_am, r)
      bif <- function(col) function(r) sprintf(
        "SUMIFS('Budget_Impact'!$%s$2:$%s$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", col, col, r_bi, r_bi, r)
      writeFormula(wb, sheet, startCol = 9,  startRow = 2, x = frows(amf("E"), R))   # modeled_cases
      writeFormula(wb, sheet, startCol = 10, startRow = 2, x = frows(amf("G"), R))   # baseline_cases
      writeFormula(wb, sheet, startCol = 11, startRow = 2, x = frows(amf("J"), R))   # cases_averted
      writeFormula(wb, sheet, startCol = 12, startRow = 2, x = frows(amf("F"), R))   # modeled_deaths
      writeFormula(wb, sheet, startCol = 13, startRow = 2, x = frows(amf("H"), R))   # baseline_deaths
      writeFormula(wb, sheet, startCol = 14, startRow = 2, x = frows(amf("I"), R))   # deaths_averted
      writeFormula(wb, sheet, startCol = 15, startRow = 2, x = frows(bif("E"), R))   # incremental_cost
      writeFormula(wb, sheet, startCol = 16, startRow = 2, x = frows(bif("F"), R))   # disc_incremental_cost
      writeFormula(wb, sheet, startCol = 17, startRow = 2, x = frows(function(r)
        sprintf("IF(%s%d>0,%s%d/%s%d,\"\")", Sca, r, Sdic, r, Sca, r), R))           # cost per case averted
      writeFormula(wb, sheet, startCol = 18, startRow = 2, x = frows(function(r)
        sprintf("IF(%s%d>0,%s%d/%s%d,\"\")", Sda, r, Sdic, r, Sda, r), R))           # cost per death averted
      writeFormula(wb, sheet, startCol = 19, startRow = 2, x = frows(function(r)
        sprintf("IF(AND(%s%d<=0,%s%d<=0),\"no incremental health effect\",IF(AND(%s%d<0,OR(%s%d>0,%s%d>0)),\"Dominant (more health, lower cost)\",IF(%s%d>0,\"USD per death averted\",\"USD per case averted\")))",
                Sca, r, Sda, r, Sdic, r, Sca, r, Sda, r, Sda, r), R))                # status
    }
    style_sheet(sheet, summ_cols, n, formula_cols = 9:19, wrap_cols = c(2, 7, 19))
  }
  write_summary("Child_Intervention_Summary", sc[scenario_level == "standalone"])
  write_summary("Parent_Package_Summary",     sc[scenario_level %in% c("package", "combined")])

  ## ===== order, recalc, strip, save ======================================
  desired_order <- c("README","Run_Metadata","Scenario_Hierarchy","Selected_Interventions","Blocked_Links",
                     "Policy_Levers","Exposure_Targets","Effect_Parameters","Risk_Response","Cost_Components",
                     "Annual_Mortality","Annual_Cost","Budget_Impact","Cost_Effectiveness",
                     "Child_Intervention_Summary","Parent_Package_Summary","Economic_Value","QA_Checks",
                     "Input_Diagnostic","Methods_and_Sources","Calculation_Assumptions","Calculation_Map")
  desired_order <- desired_order[desired_order %in% names(wb)]
  worksheetOrder(wb) <- match(desired_order, names(wb))
  wb$workbook$calcPr <- '<calcPr calcId="191029" fullCalcOnLoad="1"/>'
  if (exists("strip_dangling_drawings")) strip_dangling_drawings(wb)
  if (!dir.exists(dirname(out_file))) dir.create(dirname(out_file), recursive = TRUE)
  saveWorkbook(wb, out_file, overwrite = TRUE)
  message("  Wrote public-health formula workbook: ", out_file)
  message(sprintf("  Public-health scenarios: %s", paste(comparators, collapse = ", ")))
  invisible(out_file)
}

#===========================================================================
# 12. PUBLIC-HEALTH cost/value formula workbook ----
#---------------------------------------------------------------------------
# Written only when run_public_health_interventions = TRUE. Consumes ONLY the
# current-run public-health catalogue (public_health_inputs from Model 04) and
# the public-health scenarios in the shared Model 06 output. Mirrors the clinical
# formatting/audit pattern but adapts every sheet and formula to exposure-based
# public-health effects and per-capita policy costs. Writes exactly one file:
#   output/indonesia_cost_value_public_health_formulae.xlsx
# It is a fully-formatted, formula-driven workbook (not a copy of the clinical
# one and not an unformatted data dump).
#===========================================================================
if (isTRUE(run_public_health_interventions)) {
  ph_ok <- tryCatch({ source_public_health_cost_value(); TRUE },
                    error = function(e) { message("  Public-health workbook FAILED: ",
                                                  conditionMessage(e)); FALSE })
}

message("=== Model 09 complete ===")
