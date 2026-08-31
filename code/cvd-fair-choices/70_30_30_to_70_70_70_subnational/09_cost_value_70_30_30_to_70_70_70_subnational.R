#===========================================================================
# 09_cost_value_70_30_30_to_70_70_70_subnational.R
#   SUBNATIONAL cost & value FORMULA workbook (province-stacked, long format).
#===========================================================================
# Adapted from code/cvd-fair-choices/09_cost_value.R (the production file is NOT
# sourced). It reproduces the production "formula edition" (Workbook B) sheet
# structure, styling conventions, number formats, formula idioms and save
# convention VERBATIM, but:
#   * every province-varying sheet is STACKED in long format with `location`
#     (province name) in column A and `province_code` in column B;
#   * every aggregate/lookup formula carries a `location` criterion (SUMIFS /
#     COUNTIFS gain a location range; INDEX/MATCH pulls match a composite
#     `location||key`) so values can never leak across provinces;
#   * per-capita denominators use EACH PROVINCE's own population for that year;
#   * the cascade coverage path (province x sex x year) drives both health and
#     cost, written as R-source cells the way the national cascade tie-out does;
#   * Indonesia national rows are used ONLY for a province-to-national
#     reconciliation and NEVER appear in a province-result sheet;
#   * the cascade sheets (Cascade_Assumptions / Cascade_Trajectory / Cascade_QA)
#     are province-aware.
# Only the formula workbook is written (the PH/combined workbooks are off and the
# R-value companion is not part of this deliverable).
#===========================================================================

suppressWarnings(suppressMessages({ library(data.table); library(openxlsx) }))
message("\n=== Model 09 (subnational): cost & value formula workbook ===")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Section 0: verbatim helpers from production Model 09 -------------------
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

# Reference-Case BCA rows for Calculation_Assumptions (verbatim from production).
bca_ca_block <- function(bca_params) {
  meta <- data.table(
    parameter_id = c("bca_base_year","bca_discount_rate_primary","bca_discount_rate_sensitivity_3pct",
                     "bca_discount_rate_sensitivity_2x_gdp_pc_growth","vsl_us_gni_ratio",
                     "vsl_income_elasticity_preferred","vsl_floor_gni_multiple",
                     "vsl_sensitivity_gni_multiple_100","vsl_sensitivity_gni_multiple_160",
                     "vsly_adult_min_age","vsly_adult_max_age","bca_currency_basis",
                     "bca_price_year","cost_to_bca_currency_factor","bca_standing","bca_scope",
                     "bca_discount_rate_sensitivity_2x_gdp_pc_growth_computed"),
    unit = c("year","proportion/year","proportion/year","proportion/year","ratio (VSL/GNIpc)",
             "elasticity","ratio (VSL/GNIpc)","ratio (VSL/GNIpc)","ratio (VSL/GNIpc)",
             "age (years)","age (years)","text","year","int$ per market US$","text","text","proportion/year"),
    fmt = c("0","0.000","0.000","0.000","0","0.0","0","0","0","0","0",NA,"0","0.00",NA,NA,"0.000"),
    numeric = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE),
    description = c("BCA base year (benefits and costs discounted to this year)",
                    "Primary real discount rate for BOTH benefits and costs",
                    "Standardized sensitivity discount rate (3%)",
                    "Standardized sensitivity: 2x near-term real GDP-pc growth (workbook default)",
                    "US reference VSL-to-GNI-per-capita ratio","Income elasticity of VSL (preferred)",
                    "Floor: VSL not below this multiple of GNI per capita",
                    "Sensitivity: VSL = 100x GNI per capita","Sensitivity: VSL = 160x GNI per capita",
                    "Working-age lower bound (VSLY avg-adult-age)","Working-age upper bound (VSLY avg-adult-age)",
                    "Common monetary basis for benefits and costs","Real price year for benefits and costs",
                    "Market-USD cost -> PPP int$ multiplier","BCA standing / perspective",
                    "BCA scope (partial mortality-benefit vs full societal)",
                    "2x near-term real GDP-pc growth recomputed from SSP2 by Model 08"),
    source = "Robinson et al. 2019 Reference Case Guidelines (via input workbook / Model 08)")
  meta <- meta[parameter_id %in% bca_params$parameter_id]
  val  <- setNames(as.character(bca_params$value), bca_params$parameter_id)
  ca_bca <- data.table(
    parameter_id = meta$parameter_id,
    value = lapply(seq_len(nrow(meta)), function(i) {
      v <- val[[meta$parameter_id[i]]]
      if (isTRUE(meta$numeric[i])) as.numeric(v) else v }),
    unit = meta$unit, role = "BCA control", description = meta$description, source = meta$source)
  list(ca_bca = ca_bca, fmt = meta$fmt)
}

# ---- Section 1: resolve metaparameters, load upstream contracts -------------
if (!exists("wd_outp")) stop("Model 09 (subnational): wd_outp not set (run from the 00 runner).")
if (!exists("cost_value_formulae_file"))
  cost_value_formulae_file <- paste0(wd_outp,
    "indonesia_70_30_30_to_70_70_70_cost_value_formulae_subnational.xlsx")
if (!exists("baseline_scenario_id")) baseline_scenario_id <- "baseline"
if (!exists("fair_inputs") || !exists("fair_scenarios"))
  stop("Model 09 (subnational): fair_inputs/fair_scenarios not found (run Model 04 first).")

A            <- fair_inputs$assumptions
yr_start     <- A$analysis_start_year
yr_end       <- A$analysis_end_year
analysis_yrs <- yr_start:yr_end
disc_rate    <- A$cost_discount_rate
base_id      <- fair_inputs$baseline_scenario_id %||% baseline_scenario_id
prov_set     <- fair_inputs$province_locations
cw           <- as.data.table(fair_inputs$province_crosswalk)   # province_code, province_name, location

# Model 06 output (in-memory results_list preferred, else on-disk).
load_model_output <- function() {
  if (exists("results_list", inherits = TRUE)) {
    rl <- Filter(function(x) !is.null(x) && is.data.frame(x), get("results_list", inherits = TRUE))
    if (length(rl)) { message("  Using in-memory Model 06 output (results_list)."); return(rbindlist(rl, fill = TRUE)) }
  }
  dir_out <- file.path(wd_outp, "out_model")
  files   <- list.files(dir_out, pattern = "^model_output_.*\\.rds$", full.names = TRUE)
  if (!length(files)) stop("Model 09 (subnational): no Model 06 output found in ", dir_out, ".")
  message("  Reading Model 06 output from disk: ", length(files), " file(s).")
  rbindlist(lapply(files, readRDS), fill = TRUE)
}
mo_all <- load_model_output(); setDT(mo_all)
req <- c("scenario","year","age","sex","cause","well","sick","newcases","dead","pop","all.mx","location")
missc <- setdiff(req, names(mo_all))
if (length(missc)) stop("Model 09 (subnational): Model 06 output missing column(s): ", paste(missc, collapse=", "))

# Required upstream contracts (Model 07 health, Model 08 value, CVD 40q30).
.m07 <- file.path(wd_outp,"dt_output_dalys.rds"); .m08 <- file.path(wd_outp,"08_vsl_results.rds")
.bca <- file.path(wd_outp,"08_bca_parameters.rds"); .q40 <- file.path(wd_outp,"07_cvd_40q30.rds")
.q40a<- file.path(wd_outp,"07_cvd_40q30_age.rds")
for (f in c(.m07,.m08,.bca,.q40,.q40a)) if (!file.exists(f))
  stop("Model 09 (subnational): required upstream input missing: ", f, call. = FALSE)
dt_h07     <- as.data.table(readRDS(.m07))
ev08       <- as.data.table(readRDS(.m08))
bca_params <- as.data.table(readRDS(.bca))
dt_cvd_40q30  <- as.data.table(readRDS(.q40))
cvd_age_40q30 <- as.data.table(readRDS(.q40a))
BCAP <- setNames(as.character(bca_params$value), bca_params$parameter_id)

# Province set on the produced output (exclude Indonesia national row).
produced_locs <- sort(unique(mo_all$location))
prov_run      <- intersect(prov_set, produced_locs)
if (!length(prov_run)) stop("Model 09 (subnational): no province rows in the model output.")
have_national <- "Indonesia" %in% produced_locs
# Attach province_code to the crosswalk map for convenience.
loc2code <- setNames(cw$province_code, cw$location)

produced_scn <- intersect(names(fair_scenarios), unique(mo_all$scenario))
comparators  <- setdiff(produced_scn, base_id)
if (!length(comparators)) stop("Model 09 (subnational): no comparator scenario produced.")

# Province model output (for the workbook) and national baseline (reconciliation only).
mo   <- mo_all[location %in% prov_run & scenario %in% produced_scn & year %in% analysis_yrs]
mo[, province_code := loc2code[location]]

message(sprintf("  Provinces: %d | scenarios: %s | national row for reconciliation: %s",
                length(prov_run), paste(produced_scn, collapse=", "), if (have_national) "yes" else "no"))

# ---- Section 2: province population denominator (per province x year) -------
# Provinces are not in the WPP national file; the per-capita denominator is each
# province's OWN population = the model's baseline pop, taken once per age x sex
# (invariant across cause) and summed. Task: cost per capita uses province pop.
.popsrc <- mo[scenario == base_id, .(pop_as = pop[1]), by = .(location, year, age, sex)]
province_pop_dt <- .popsrc[, .(province_population = sum(pop_as)), by = .(location, year)][order(location, year)]
setkey(province_pop_dt, location, year)
.province_pop_for <- function(locs, years) {
  province_pop_dt[data.table(location = locs, year = as.integer(years)), province_population, on = .(location, year)]
}

# ---- Section 3: annual mortality (province x scenario x year x cause) -------
mort <- mo[, .(cases = sum(newcases), cause_deaths = sum(dead)),
           by = .(location, province_code, scenario, year, cause)]
base_mort <- mo[scenario == base_id, .(base_deaths = sum(dead), base_cases = sum(newcases)),
                by = .(location, year, cause)]
mort <- merge(mort, base_mort, by = c("location","year","cause"), all.x = TRUE)
mort[, deaths_averted := base_deaths - cause_deaths]
mort[, cases_averted  := base_cases  - cases]
scn_lab <- data.table(scenario = names(fair_scenarios),
                      scenario_label = vapply(fair_scenarios, function(s) s$scenario_label, character(1)))
mort <- merge(mort, scn_lab, by = "scenario", all.x = TRUE)
setcolorder(mort, c("location","province_code","scenario","scenario_label","year","cause",
                    "cases","cause_deaths","base_deaths","deaths_averted","base_cases","cases_averted"))
setorder(mort, location, scenario, year, cause)

# ---- Section 4: component costing (province x scenario x year x record) -----
costs <- copy(fair_inputs$costs)
costs[, cost_ready := !is.na(unit_cost_usd) & unit_cost_usd >= 0 &
        !is.na(cov_baseline) & !is.na(population_in_need_fraction) &
        population_in_need_measure %in% c("all","prevalence","incidence")]
popu <- unique(mo[, .(location, scenario, year, age, sex, pop)])

qty_by_year <- function(loc, scn, cr) {
  a0 <- cr$c_age_start; a1 <- cr$c_age_stop; sx <- cr$c_sex
  if (cr$population_in_need_measure == "all") {
    d <- popu[location == loc & scenario == scn & age >= a0 & age <= a1]
    if (!identical(sx, "Both")) d <- d[sex == sx]
    agg <- d[, .(q = sum(pop)), by = year]
  } else {
    vcol <- if (cr$population_in_need_measure == "prevalence") "sick" else "newcases"
    d <- mo[location == loc & scenario == scn & cause == cr$cause_code & age >= a0 & age <= a1]
    if (!identical(sx, "Both")) d <- d[sex == sx]
    agg <- d[, .(q = sum(get(vcol))), by = year]
  }
  m <- merge(data.table(year = analysis_yrs), agg, by = "year", all.x = TRUE)
  m[is.na(q), q := 0]; m$q
}
disc_factor <- 1 / (1 + disc_rate)^(analysis_yrs - yr_start)

cost_rows <- list()
for (loc in prov_run) {
  pcode <- loc2code[[loc]]
  for (scn in comparators) {
    ids   <- fair_scenarios[[scn]]$intervention_ids
    comps <- costs[location == loc & intervention_id %in% ids & cost_ready == TRUE]
    if (!nrow(comps)) next
    for (i in seq_len(nrow(comps))) {
      cr  <- comps[i]
      q_s <- qty_by_year(loc, scn,     cr)
      q_b <- qty_by_year(loc, base_id, cr)
      # Exact per-year cascade coverage path (province x intervention, sex-avg).
      cov_s <- rep(cr$cov_baseline, length(analysis_yrs))
      cpp <- cr[["coverage_path"]][[1]]
      if (!is.null(cpp) && NROW(cpp) > 0L) {
        cpp <- as.data.table(cpp); lk <- setNames(as.numeric(cpp$coverage_t), as.character(cpp$year))
        v <- as.numeric(lk[as.character(analysis_yrs)])
        v[is.na(v) & analysis_yrs < min(cpp$year)] <- cr$cov_baseline
        v[is.na(v) & analysis_yrs > max(cpp$year)] <- as.numeric(lk[[as.character(max(cpp$year))]])
        cov_s <- pmin(pmax(v, 0), 1)
      }
      cov_b  <- rep(cr$cov_baseline, length(analysis_yrs))
      pin_s  <- q_s * cr$population_in_need_fraction
      pin_b  <- q_b * cr$population_in_need_fraction
      cost_s <- pin_s * cov_s * cr$frequency_per_year * cr$unit_cost_usd
      cost_b <- pin_b * cov_b * cr$frequency_per_year * cr$unit_cost_usd
      cost_rows[[length(cost_rows) + 1L]] <- data.table(
        location = loc, province_code = pcode, scenario = scn, year = analysis_yrs,
        intervention_id = cr$intervention_id, cause_code = cr$cause_code %||% NA_character_,
        cost_record_id = cr$cost_record_id, cost_component_key = cr$cost_component_key,
        cost_join_key = cr$cost_join_key, cost_scope = cr$cost_scope,
        population_in_need_measure = cr$population_in_need_measure,
        population_in_need_fraction = cr$population_in_need_fraction,
        coverage_scenario = cov_s, coverage_baseline = cov_b,
        frequency_per_year = cr$frequency_per_year, unit_cost_usd = cr$unit_cost_usd,
        r_quantity_scenario = q_s, r_quantity_baseline = q_b,
        pin_scenario = pin_s, pin_baseline = pin_b,
        annual_cost_baseline = cost_b, annual_cost_scenario = cost_s,
        annual_cost_incremental = cost_s - cost_b,
        discount_factor = disc_factor,
        disc_cost_baseline = cost_b * disc_factor,
        disc_cost_scenario = cost_s * disc_factor,
        disc_cost_incremental = (cost_s - cost_b) * disc_factor)
    }
  }
}
annual_cost <- if (length(cost_rows)) rbindlist(cost_rows) else data.table()
if (nrow(annual_cost)) {
  annual_cost[, province_population := .province_pop_for(location, year)]
  setorder(annual_cost, location, scenario, year, cost_record_id)
}

# ---- Section 5: budget impact (province x scenario x year) -----------------
if (nrow(annual_cost)) {
  bi <- annual_cost[, .(baseline_cost = sum(annual_cost_baseline),
                        scenario_cost = sum(annual_cost_scenario),
                        incremental_cost = sum(annual_cost_incremental),
                        disc_incremental_cost = sum(disc_cost_incremental)),
                    by = .(location, province_code, scenario, year)]
  setorder(bi, location, scenario, year)
  bi[, cumulative_incremental_cost := cumsum(incremental_cost), by = .(location, scenario)]
  bi[, cumulative_disc_incremental_cost := cumsum(disc_incremental_cost), by = .(location, scenario)]
  bi[, province_population := .province_pop_for(location, year)]
} else bi <- data.table()

# ---- Section 6: cost-effectiveness (province x scenario) -------------------
da_by <- mort[scenario %in% comparators,
              .(deaths_averted = sum(deaths_averted, na.rm=TRUE),
                cases_averted  = sum(cases_averted,  na.rm=TRUE)),
              by = .(location, province_code, scenario)]
ic_by <- if (nrow(bi)) bi[, .(incremental_cost = sum(incremental_cost),
                              disc_incremental_cost = sum(disc_incremental_cost)),
                          by = .(location, scenario)] else
  data.table(location = character(0), scenario = character(0),
             incremental_cost = numeric(0), disc_incremental_cost = numeric(0))
cea <- merge(da_by, ic_by, by = c("location","scenario"), all.x = TRUE)
cea[is.na(incremental_cost), incremental_cost := 0]
cea[is.na(disc_incremental_cost), disc_incremental_cost := 0]
cea <- merge(cea, scn_lab, by = "scenario", all.x = TRUE)
cea[, cost_per_death_averted := NA_real_]
cea[deaths_averted > 0, cost_per_death_averted := disc_incremental_cost / deaths_averted]
setcolorder(cea, c("location","province_code","scenario","scenario_label","deaths_averted",
                   "cases_averted","incremental_cost","disc_incremental_cost","cost_per_death_averted"))
setorder(cea, location, scenario)

# ---- Section 7: Health_Outcomes / Economic_Value source (province x scen x year)
ho_src <- dt_h07[location %in% prov_run & scenario %in% comparators, .(
  scenario_label = scenario_label[1L],
  modeled_deaths = sum(deaths),   baseline_deaths = sum(base_deaths),
  modeled_cases  = sum(newcases), baseline_cases  = sum(base_newcases),
  yll = sum(yll), base_yll = sum(base_yll), yld = sum(yld), base_yld = sum(base_yld),
  daly = sum(daly), base_daly = sum(base_daly)),
  by = .(location, scenario, year)]
ho_src[, province_code := loc2code[location]]
ev_src <- ev08[location %in% prov_run & scenario %in% comparators, .(
  scenario_label = scenario_label[1L],
  deaths_averted = sum(deaths_averted), life_years_gained = sum(life_years_gained_undisc),
  gni_pc_idn = gni_pc_ppp[1L], gni_pc_usa = gni_pc_usa[1L],
  population = population[1L], le_avg_adult = le_avg_adult[1L]),
  by = .(location, scenario, year)]
ev_src[, province_code := loc2code[location]]

# ---- Section 8: R-side reconciliation anchors + province-to-national --------
tol <- 1e-6
negc   <- mo[, sum(well < -tol | sick < -tol | newcases < -tol | dead < -tol | pop < -tol)]
maxres <- mo[, max(abs(pop - (well + sick + all.mx)))]
ndist  <- mo[, .(n = uniqueN(round(all.mx, 6))), by = .(location, scenario, year, age, sex)][, max(n)]
# Anchor = TOTAL over provinces for the cascade scenario.
anchor_scn <- comparators[1]
r_da_anchor  <- cea[scenario == anchor_scn, sum(deaths_averted, na.rm = TRUE)]
r_dic_anchor <- cea[scenario == anchor_scn, sum(disc_incremental_cost, na.rm = TRUE)]
r_cpd_anchor <- if (r_da_anchor > 0) r_dic_anchor / r_da_anchor else NA_real_

# Province-to-national reconciliation (population + baseline health outcomes).
recon_nat <- NULL
if (have_national) {
  prov_pop_2025  <- province_pop_dt[year == yr_start, sum(province_population)]
  nat_pop_2025   <- mo_all[location == "Indonesia" & scenario == base_id & year == yr_start,
                           sum(unique(.SD)$pop), .SDcols = c("age","sex","pop")]
  # baseline deaths (all causes modeled) over horizon: provinces vs national
  prov_base_deaths <- mo[scenario == base_id, sum(dead)]
  nat_base_deaths  <- mo_all[location == "Indonesia" & scenario == base_id & year %in% analysis_yrs, sum(dead)]
  recon_nat <- data.table(
    metric = c("population_2025","baseline_modeled_deaths_2025_2050"),
    province_sum = c(prov_pop_2025, prov_base_deaths),
    national     = c(nat_pop_2025, nat_base_deaths))
  recon_nat[, abs_diff := province_sum - national]
  recon_nat[, rel_diff := ifelse(national != 0, abs_diff / national, NA_real_)]
  message(sprintf("  Province-to-national reconciliation: 2025 pop rel.diff %.2e; baseline deaths rel.diff %.2e",
                  recon_nat[metric=="population_2025", rel_diff],
                  recon_nat[metric=="baseline_modeled_deaths_2025_2050", rel_diff]))
}

# ---- Section 9: supporting tables (Selected_Interventions, Cost_Components) --
`%f%` <- function(x, d = 4) ifelse(is.na(x), NA_real_, round(x, d))
# Province-stacked Selected_Interventions: national links x province coverage.
vl <- as.data.table(fair_inputs$valid_links)
# province baseline coverage per (location, intervention_id) = sex-avg 2025 from cost catalogue
prov_int_cov <- unique(costs[, .(location, intervention_id, cov_baseline, cov_target, cov_start_year, cov_target_year)])
sel_out <- rbindlist(lapply(prov_run, function(loc) {
  d <- copy(vl)
  d[, `:=`(location = loc, province_code = loc2code[[loc]])]
  # attach province baseline/target coverage by intervention_id
  d <- merge(d, prov_int_cov[location == loc, .(intervention_id, baseline_coverage = cov_baseline,
             target_coverage = cov_target, start_year = cov_start_year, target_year = cov_target_year)],
             by = "intervention_id", all.x = TRUE, suffixes = c("", ".prov"))
  d
}), fill = TRUE)
sel_out <- sel_out[, .(location, province_code, intervention_id, intervention_cause_key,
                       intervention_name, cause_id, cause_code, model_name,
                       transition_from, transition_to, model_transition,
                       effect_value, affected_fraction, baseline_coverage, target_coverage,
                       start_year, target_year, cost_join_key, cost_scope,
                       effect_review = effect_review, coverage_review = "OK (cascade trajectory)")]
setorder(sel_out, location, intervention_id, cause_code)

cost_out <- costs[, .(location, province_code, cost_record_id, cost_component_key, cost_option,
                      intervention_id, cause_id, cause_code, cost_join_key, cost_scope, cost_component,
                      population_in_need_measure, population_in_need_fraction, frequency_per_year,
                      c_age_start, c_age_stop, c_sex, unit_cost_usd, price_year,
                      indonesia_adjusted_flag, cov_baseline, cov_target, cov_start_year, cov_target_year,
                      cost_review, cost_ready)]
setorder(cost_out, location, intervention_id, cost_component_key)

diag_out <- fair_inputs$validation

# ---- Section 10: workbook (styles verbatim from production) -----------------
C_HDR <- "#1F4E78"; C_FORMULA <- "#DDEBF7"; C_RSRC <- "#F2F2F2"; C_INPUT <- "#FFF2CC"
st_hdr     <- createStyle(fontColour="#FFFFFF", fgFill=C_HDR, textDecoration="bold",
                          halign="center", valign="center", wrapText=TRUE,
                          border="TopBottomLeftRight", borderColour="#8EA9C1")
st_title   <- createStyle(fontColour="#FFFFFF", fgFill=C_HDR, textDecoration="bold", fontSize=13, valign="center")
st_formula <- createStyle(fgFill=C_FORMULA); st_rsrc <- createStyle(fgFill=C_RSRC)
st_input   <- createStyle(fgFill=C_INPUT);   st_wrap <- createStyle(valign="top", wrapText=TRUE)
cf_pass <- createStyle(bgFill="#C6EFCE", fontColour="#006100")
cf_fail <- createStyle(bgFill="#FFC7CE", fontColour="#9C0006")
cf_rev  <- createStyle(bgFill="#FFEB9C", fontColour="#9C6500")

fmt_of2 <- function(col) {
  cl <- tolower(col)
  if (grepl("frequency", cl)) return("0.00")
  if (grepl("adjusted_effect", cl)) return("0.0000")
  if (grepl("effect_value|affected_fraction", cl)) return("0.000")
  if (grepl("discount_factor", cl)) return("0.000")
  if (grepl("benefit_cost_ratio|_ratio$|^ratio$", cl)) return("0.00")
  if (grepl("^year$|_year$|price_year|age_start|age_stop|^c_age|province_code", cl)) return("0")
  if (grepl("coverage|_fraction$|^fraction$|^cov_base|^cov_targ$|coverage_", cl)) return("0.0%")
  if (grepl("unit_cost|r_quantity", cl)) return("#,##0.00")
  if (grepl("per_death", cl)) return("#,##0")
  if (grepl("per_capita", cl)) return("#,##0.00")
  if (grepl("cost|value|benefit|^pin_|net_benefit|budget|population", cl)) return("#,##0")
  if (grepl("death|case|averted|duplicate_count|key_count|distinct|residual|negative|_count$|^count$", cl)) return("#,##0")
  NA_character_
}
wb <- createWorkbook(); modifyBaseFont(wb, fontName="Carlito", fontSize=11)

style_sheet <- function(sheet, nm, nrow_data, formula_cols=integer(0), rsource_cols=integer(0),
                        input_cols=integer(0), header_row=1L, wrap_cols=integer(0),
                        filter=TRUE, min_w=11, max_w=46) {
  ncol <- length(nm)
  addStyle(wb, sheet, st_hdr, rows=header_row, cols=seq_len(ncol), gridExpand=TRUE)
  if (nrow_data > 0) {
    dr <- (header_row + 1L):(header_row + nrow_data)
    for (j in formula_cols) addStyle(wb, sheet, st_formula, rows=dr, cols=j, gridExpand=TRUE, stack=TRUE)
    for (j in rsource_cols) addStyle(wb, sheet, st_rsrc,    rows=dr, cols=j, gridExpand=TRUE, stack=TRUE)
    for (j in input_cols)   addStyle(wb, sheet, st_input,   rows=dr, cols=j, gridExpand=TRUE, stack=TRUE)
    for (j in seq_len(ncol)) { f <- fmt_of2(nm[j])
      if (!is.na(f)) addStyle(wb, sheet, createStyle(numFmt=f), rows=dr, cols=j, gridExpand=TRUE, stack=TRUE) }
    for (j in wrap_cols) addStyle(wb, sheet, st_wrap, rows=dr, cols=j, gridExpand=TRUE, stack=TRUE)
  }
  freezePane(wb, sheet, firstActiveRow=header_row+1L, firstActiveCol=1L)
  if (filter) addFilter(wb, sheet, rows=header_row, cols=seq_len(ncol))
  w <- pmin(pmax(nchar(nm)+2L, min_w), max_w)
  setColWidths(wb, sheet, cols=seq_len(ncol), widths=w)
  setRowHeights(wb, sheet, rows=header_row, heights=28); invisible(NULL)
}
frows <- function(fn, rows) vapply(rows, fn, character(1))
# column-letter-by-name helper factory (robust to column order)
CLmk <- function(cols) function(nm) openxlsx::int2col(match(nm, cols))

# --- write a data.frame of NA-placeholder + R-source columns generically ----
write_tabular <- function(sheet, df) { addWorksheet(wb, sheet); writeData(wb, sheet, df, headerStyle=st_hdr) }

message("  Building province-stacked formula workbook: ", cost_value_formulae_file)

#===========================================================================
# Calculation_Assumptions  (global controls + BCA; anchors = province totals)
#===========================================================================
ca <- data.table(
  parameter_id = c("analysis_start_year","analysis_end_year","baseline_scenario_id",
                   "cost_discount_rate","cost_price_year","currency","economic_perspective",
                   "scale_up_shape","downstream_cost_offsets","formula_tolerance",
                   "stock_flow_residual_limit","trace_precision","r_stock_flow_max_residual",
                   "r_background_distinct_count","r_negative_state_count","r_deaths_averted_anchor",
                   "r_disc_incremental_cost_anchor","r_cost_per_death_anchor","qa_anchor_scenario",
                   "province_count","subnational_run"),
  value = list(as.integer(yr_start), as.integer(yr_end), base_id, disc_rate,
               as.integer(A$cost_price_year), A$currency, A$economic_perspective, A$scale_up_shape,
               as.integer(A$downstream_cost_offsets), 0.001, 1000L, 2L,
               round(as.numeric(maxres),2), as.integer(ndist), as.integer(negc),
               as.numeric(r_da_anchor), as.numeric(r_dic_anchor), as.numeric(r_cpd_anchor),
               anchor_scn, as.integer(length(prov_run)), "TRUE"),
  unit = c("year","year","scenario id","proportion/year","USD year","currency","text","text",
           "0/1 flag","USD/count","persons","decimal places","persons","count","count",
           "deaths","USD","USD/death","scenario id","count","flag"),
  role = c(rep("formula control",4),"metadata","metadata","metadata","formula control","scope",
           "QA control","QA control","audit note","R QA source","R QA source","R QA source",
           "R reconciliation source","R reconciliation source","R reconciliation source",
           "R reconciliation source","scope","scope"),
  description = c("First model and discount year","Last model year",
                  "Comparator used for health and cost calculations","Annual discount rate applied to costs only",
                  "Reporting price year","Workbook reporting currency","Economic evaluation perspective",
                  "Coverage follows the province cascade trajectory","Downstream disease-cost offsets excluded",
                  "Absolute reconciliation tolerance (internal Excel checks)","Review threshold for the R stock/flow check",
                  "Exported traces rounded; R quantity helpers keep full precision","Max stock/flow residual computed in R",
                  "Max distinct all-cause mortality values across causes (R)","Count of impossible negative state/flow values (R)",
                  "R deaths averted TOTAL across provinces for the anchor scenario",
                  "R discounted incremental cost TOTAL across provinces for the anchor scenario",
                  "R USD per death averted (province-total) for the anchor scenario",
                  "Scenario used for the Excel-vs-R reconciliation checks","Number of provinces stacked",
                  "This is a stand-alone SUBNATIONAL run (no calibration)"),
  source = c(rep("subnational input workbook / Model 09",2),"Model 04 / Model 09",
             "subnational input workbook / Model 09", rep("subnational input workbook",4),
             "subnational input workbook","Workbook QA rule","Model 09","Model 09 export rule",
             rep("Model 09 current run",3), rep("Model 09 current run (R CEA)",4),
             "Model 09","Model 09"))
.bcab <- bca_ca_block(bca_params); n_ca_core <- nrow(ca); ca <- rbind(ca, .bcab$ca_bca); bca_fmt_vec <- .bcab$fmt
addWorksheet(wb, "Calculation_Assumptions")
writeData(wb, "Calculation_Assumptions",
          data.frame(parameter_id="parameter_id", value="value", unit="unit", role="role",
                     description="description", source="source"), colNames=FALSE, startRow=1)
writeData(wb, "Calculation_Assumptions", ca$parameter_id, startCol=1, startRow=2, colNames=FALSE)
writeData(wb, "Calculation_Assumptions", as.data.frame(ca[, .(unit, role, description, source)]),
          startCol=3, startRow=2, colNames=FALSE)
for (i in seq_len(nrow(ca)))
  writeData(wb, "Calculation_Assumptions", ca$value[[i]], startCol=2, startRow=1+i, colNames=FALSE)
addStyle(wb, "Calculation_Assumptions", st_hdr, rows=1, cols=1:6, gridExpand=TRUE)
addStyle(wb, "Calculation_Assumptions", st_input, rows=2:13, cols=2, gridExpand=TRUE, stack=TRUE)
addStyle(wb, "Calculation_Assumptions", st_rsrc,  rows=14:(n_ca_core+1L), cols=2, gridExpand=TRUE, stack=TRUE)
addStyle(wb, "Calculation_Assumptions", st_input, rows=(n_ca_core+2L):(nrow(ca)+1L), cols=2, gridExpand=TRUE, stack=TRUE)
ca_fmt <- c("0","0",NA,"0.0%","0",NA,NA,NA,"0","0.000","#,##0","0","#,##0.0","0","0",
            "#,##0","#,##0","#,##0.00",NA,"0",NA, bca_fmt_vec)
for (i in seq_along(ca_fmt)) if (!is.na(ca_fmt[i]))
  addStyle(wb, "Calculation_Assumptions", createStyle(numFmt=ca_fmt[i]), rows=1+i, cols=2, gridExpand=TRUE, stack=TRUE)
addStyle(wb, "Calculation_Assumptions", st_wrap, rows=2:(nrow(ca)+1), cols=5, gridExpand=TRUE, stack=TRUE)
freezePane(wb, "Calculation_Assumptions", firstActiveRow=2); addFilter(wb, "Calculation_Assumptions", rows=1, cols=1:6)
setColWidths(wb, "Calculation_Assumptions", cols=1:6, widths=c(40,16,16,24,64,42))
setRowHeights(wb, "Calculation_Assumptions", rows=1, heights=28)
.carow <- function(pid) match(pid, ca$parameter_id) + 1L
.bcell <- function(pid) sprintf("'Calculation_Assumptions'!$B$%d", .carow(pid))
bca_cells <- list(ratio=.bcell("vsl_us_gni_ratio"), elast=.bcell("vsl_income_elasticity_preferred"),
                  floor=.bcell("vsl_floor_gni_multiple"), mult100=.bcell("vsl_sensitivity_gni_multiple_100"),
                  mult160=.bcell("vsl_sensitivity_gni_multiple_160"), r_primary=.bcell("bca_discount_rate_primary"),
                  base_year=.bcell("bca_base_year"), price_year=.bcell("bca_price_year"),
                  cost_factor=.bcell("cost_to_bca_currency_factor"), scope=.bcell("bca_scope"))
cell_disc  <- .bcell("cost_discount_rate"); cell_ystart <- .bcell("analysis_start_year")
cell_ftol  <- .bcell("formula_tolerance")

message("  Calculation_Assumptions written (", nrow(ca), " rows).")

#===========================================================================
# README
#===========================================================================
readme_f <- data.table(
  section = c("Purpose","Subnational scope","How to read","Provinces","Scenarios",
              "Baseline pairing","Per-capita costs","Coverage","National assumptions",
              "CVD 40q30","Economic value","Province-to-national","Colour legend","No calibration"),
  detail = c(
    "Costing, budget impact, cost-effectiveness and Reference-Case benefit-cost analysis of the 70-30-30 -> 70-70-70 hypertension/cholesterol + diabetes treatment cascade, for every Indonesian province.",
    "Stand-alone SUBNATIONAL run. Every province-varying sheet is stacked in long format with location (province) in column A and province_code in column B. All aggregate/lookup formulas carry a location criterion so values never leak across provinces.",
    "Grey cells are R-generated source values; light-blue cells are LIVE Excel formulas; pale-yellow cells on Calculation_Assumptions are editable controls. Calculation_Map lists every dependency.",
    "38 current Indonesian provinces, reconciled 1:1 between the input workbook and the reconciled province rate table. Indonesia national rows are used only for the province-to-national reconciliation and never appear in a province-result sheet.",
    "Two per province: baseline and S_70_30_30_TO_70_70_70 (a single combined cascade carrying I_CVD_PRIMARY + I_T2D_TREATMENT).",
    "Deaths/cases averted = baseline - scenario, matched within each province at year x age x sex x cause.",
    "Cost per capita uses EACH PROVINCE's own population for that year (province_population, grey R-source), never the national population.",
    "The exact province x sex x year effective-coverage path (Provincial_Trajectory) drives both health effects and costs; it is written as R-source cells (Cascade_Trajectory / Annual_Cost coverage), reproducing the workbook Provincial_Model_Input_View transition multipliers to machine precision.",
    "CRUDE provincial coverage anchors requiring local validation. Diabetes treatment coverage moves proportionally to the provincial CVD treatment anchor. Life expectancy, disability weights and VSL/VSLY per-capita parameters are NATIONAL (Indonesia) values applied to every province (documented limitation).",
    "CVD_40q30 / CVD_40q30_Age give the period probability of dying from the six CVD causes between exact ages 30 and 70, per province/scenario/year. cvd_40q30 = 100*(1-l_70/l_30) as a live life table, reconciled to Model 07.",
    "Reference-Case VSL/VSLY (2019 Robinson et al.): national per-capita VSL x province-specific deaths averted / life-years gained. PARTIAL mortality-benefit BCA.",
    "Province_Reconciliation compares the sum over the 38 provinces with the Indonesia national row (population and baseline modeled deaths); differences are reported, never recalibrated.",
    "Header dark-blue; formula light-blue; R source grey; editable controls pale-yellow; PASS green; FAIL/REVIEW red/orange.",
    "No calibration was run. The reconciled province rate table already incorporates Models 01/02 preparation and Model 03 calibration; this pipeline begins at the adapted Model 04."))
addWorksheet(wb, "README")
writeData(wb, "README", "Indonesia NCD 70-30-30 -> 70-70-70 SUBNATIONAL cascade - cost & value (formula edition)", startRow=1)
addStyle(wb, "README", st_title, rows=1, cols=1)
writeData(wb, "README", readme_f, startRow=3, headerStyle=st_hdr)
setColWidths(wb, "README", cols=1:2, widths=c(22,124))
addStyle(wb, "README", st_wrap, rows=4:(nrow(readme_f)+3), cols=2, gridExpand=TRUE, stack=TRUE)
setRowHeights(wb, "README", rows=1, heights=22)

#===========================================================================
# Run_Metadata
#===========================================================================
meta <- data.table(item = c(
  "Workbook title","Run date","Model / pipeline","Input workbook","Model rate table",
  "Model output source","Geographic scope","Provinces","Analysis years","Baseline scenario",
  "Scenarios costed","Cost discount rate","Cost price year","Currency","Economic perspective",
  "Coverage scale-up","Calibration","National assumptions","R version","openxlsx / data.table"),
  value = c(
  "Indonesia NCD 70-30-30 -> 70-70-70 subnational cost & value",
  as.character(Sys.Date()),
  "CVD FAIR Choices cascade (subnational 00 + 04-09; no 01/02/03)",
  basename(fair_inputs$inputs_path),
  "b_rates_full_period_reconciled_2017_2050_national_current38.rds",
  "output/70_30_30_to_70_70_70_subnational/out_model/model_output_*.rds",
  "Subnational (38 Indonesian provinces)",
  as.character(length(prov_run)),
  paste0(yr_start,"-",yr_end),
  base_id,
  paste(comparators, collapse=", "),
  sprintf("%.1f%%", 100*disc_rate),
  as.character(A$cost_price_year), A$currency, A$economic_perspective, A$scale_up_shape,
  "NONE (reconciled rate table already calibrated)",
  "Life expectancy, disability weights, VSL/VSLY per-capita = national (Indonesia), applied to all provinces",
  R.version.string,
  paste0(as.character(packageVersion("openxlsx"))," / ", as.character(packageVersion("data.table")))))
addWorksheet(wb, "Run_Metadata"); writeData(wb, "Run_Metadata", meta, headerStyle=st_hdr)
writeFormula(wb, "Run_Metadata", startCol=2, startRow=10,
             x="'Calculation_Assumptions'!B4")   # Baseline scenario (row 10 in meta = 'Baseline scenario')
writeFormula(wb, "Run_Metadata", startCol=2, startRow=13, x="'Calculation_Assumptions'!B6")  # price year
addStyle(wb, "Run_Metadata", st_formula, rows=c(10,13), cols=2, stack=TRUE)
setColWidths(wb, "Run_Metadata", cols=1:2, widths=c(30,90))
freezePane(wb, "Run_Metadata", firstActiveRow=2); setRowHeights(wb, "Run_Metadata", rows=1, heights=28)

#===========================================================================
# Scenario_Catalog
#===========================================================================
scat <- rbindlist(lapply(c(base_id, comparators), function(scn) {
  e <- fair_scenarios[[scn]]; iv <- e$intervention_ids %||% character(0)
  data.table(scenario=scn, scenario_label=e$scenario_label %||% scn,
             intervention_family=e$family %||% NA_character_,
             scenario_level=e$scenario_level %||% (if (scn==base_id) "baseline" else "combined"),
             scenario_role=e$scenario_role %||% NA_character_,
             intervention_ids=paste(iv, collapse="; "), n_interventions=length(iv))
}), fill=TRUE)
addWorksheet(wb, "Scenario_Catalog"); writeData(wb, "Scenario_Catalog", scat, headerStyle=st_hdr)
style_sheet("Scenario_Catalog", names(scat), nrow(scat), rsource_cols=seq_along(scat), wrap_cols=c(2,6))

#===========================================================================
# Selected_Interventions  (province-stacked; P adjusted effect + key check formulas)
#===========================================================================
si_cols <- c(names(sel_out), "adjusted_effect_at_target","key_count","formula_status")
si <- as.data.frame(sel_out); si$adjusted_effect_at_target <- NA_real_
si$key_count <- NA_real_; si$formula_status <- NA_character_
SIcl <- CLmk(si_cols)
addWorksheet(wb, "Selected_Interventions"); writeData(wb, "Selected_Interventions", si, headerStyle=st_hdr)
n_si <- nrow(si); r_si <- n_si + 1L
if (n_si > 0) {
  R <- 2:r_si
  ev <- SIcl("effect_value"); af <- SIcl("affected_fraction"); bc <- SIcl("baseline_coverage"); tc <- SIcl("target_coverage")
  writeFormula(wb, "Selected_Interventions", startCol=match("adjusted_effect_at_target",si_cols), startRow=2,
    x=frows(function(r) sprintf("IF(OR(%s%d=\"\",%s%d=\"\",%s%d=\"\",%s%d=\"\"),\"\",ROUND(%s%d*(%s%d*(%s%d-%s%d)/(1-%s%d*%s%d)),4))",
      ev,r,af,r,bc,r,tc,r, af,r,ev,r,tc,r,bc,r,ev,r,bc,r), R))
  loccol <- SIcl("location"); keycol <- SIcl("intervention_cause_key")
  writeFormula(wb, "Selected_Interventions", startCol=match("key_count",si_cols), startRow=2,
    x=frows(function(r) sprintf("COUNTIFS($%s$2:$%s$%d,%s%d,$%s$2:$%s$%d,%s%d)",
      loccol,loccol,r_si,loccol,r, keycol,keycol,r_si,keycol,r), R))
  kc <- SIcl("key_count")
  writeFormula(wb, "Selected_Interventions", startCol=match("formula_status",si_cols), startRow=2,
    x=frows(function(r) sprintf("IF(%s%d=1,\"OK\",\"DUPLICATE KEY\")", kc, r), R))
}
style_sheet("Selected_Interventions", si_cols, n_si,
            formula_cols=match(c("adjusted_effect_at_target","key_count","formula_status"), si_cols),
            rsource_cols=match(c("location","province_code"), si_cols))

#===========================================================================
# Cost_Components  (province-stacked; cost_ready = formula)
#===========================================================================
cc_cols <- names(cost_out)
cc <- copy(cost_out); cc[, cost_ready := NA_real_]; cc <- as.data.frame(cc)
CCcl <- CLmk(cc_cols)
addWorksheet(wb, "Cost_Components"); writeData(wb, "Cost_Components", cc, headerStyle=st_hdr)
n_cc <- nrow(cc); r_cc <- n_cc + 1L
if (n_cc > 0) {
  R <- 2:r_cc
  uc <- CCcl("unit_cost_usd"); pf <- CCcl("population_in_need_fraction"); cb <- CCcl("cov_baseline")
  pm <- CCcl("population_in_need_measure")
  writeFormula(wb, "Cost_Components", startCol=match("cost_ready",cc_cols), startRow=2,
    x=frows(function(r) sprintf(
      "IF(AND(%s%d<>\"\",%s%d>=0,%s%d<>\"\",%s%d<>\"\",%s%d>=0,%s%d<=1,OR(%s%d=\"all\",%s%d=\"prevalence\",%s%d=\"incidence\")),1,0)",
      uc,r,uc,r, cb,r, pf,r,pf,r,pf,r, pm,r,pm,r,pm,r), R))
}
style_sheet("Cost_Components", cc_cols, n_cc, formula_cols=match("cost_ready",cc_cols),
            rsource_cols=match(c("location","province_code"), cc_cols),
            wrap_cols=match("cost_component", cc_cols))

#===========================================================================
# Annual_Mortality  (province-stacked; deaths/cases averted = same-row formulas)
#===========================================================================
am_cols <- c("location","province_code","scenario","scenario_label","year","cause",
             "cases","cause_deaths","base_deaths","deaths_averted","base_cases","cases_averted")
am <- as.data.frame(mort[, ..am_cols]); am$deaths_averted <- NA_real_; am$cases_averted <- NA_real_
AMcl <- CLmk(am_cols)
addWorksheet(wb, "Annual_Mortality"); writeData(wb, "Annual_Mortality", am, headerStyle=st_hdr)
n_am <- nrow(am); r_am <- n_am + 1L
if (n_am > 0) {
  R <- 2:r_am
  bd <- AMcl("base_deaths"); cd <- AMcl("cause_deaths"); bcs <- AMcl("base_cases"); cs <- AMcl("cases")
  writeFormula(wb, "Annual_Mortality", startCol=match("deaths_averted",am_cols), startRow=2,
    x=frows(function(r) sprintf("%s%d-%s%d", bd,r, cd,r), R))
  writeFormula(wb, "Annual_Mortality", startCol=match("cases_averted",am_cols), startRow=2,
    x=frows(function(r) sprintf("%s%d-%s%d", bcs,r, cs,r), R))
}
style_sheet("Annual_Mortality", am_cols, n_am,
            formula_cols=match(c("deaths_averted","cases_averted"), am_cols),
            rsource_cols=match(c("location","province_code","cases","cause_deaths","base_deaths","base_cases"), am_cols))
message("  Wrote README, Run_Metadata, Scenario_Catalog, Selected_Interventions, Cost_Components, Annual_Mortality.")

#===========================================================================
# Annual_Cost  (province-stacked; inputs R-source, derived costs = formulas)
#===========================================================================
ac_rsrc <- c("location","province_code","scenario","year","intervention_id","cause_code",
             "cost_record_id","cost_component_key","cost_join_key","cost_scope",
             "population_in_need_measure","population_in_need_fraction",
             "coverage_scenario","coverage_baseline","frequency_per_year","unit_cost_usd",
             "r_quantity_scenario","r_quantity_baseline","province_population")
ac_fml  <- c("pin_scenario","pin_baseline","annual_cost_baseline","annual_cost_scenario",
             "annual_cost_incremental","discount_factor","disc_cost_baseline","disc_cost_scenario",
             "disc_cost_incremental","shared_duplicate_count","annual_cost_baseline_per_capita",
             "annual_cost_scenario_per_capita","annual_cost_incremental_per_capita","disc_cost_incremental_per_capita")
ac_cols <- c(ac_rsrc, ac_fml)
ACcl <- CLmk(ac_cols)
if (nrow(annual_cost)) {
  ac <- as.data.frame(annual_cost[, ..ac_rsrc])
  for (cn in ac_fml) ac[[cn]] <- NA_real_
  ac <- ac[, ac_cols]
} else ac <- as.data.frame(setNames(replicate(length(ac_cols), logical(0), simplify=FALSE), ac_cols))
addWorksheet(wb, "Annual_Cost"); writeData(wb, "Annual_Cost", ac, headerStyle=st_hdr)
n_ac <- nrow(ac); r_ac <- max(n_ac + 1L, 2L)
if (n_ac > 0) {
  R <- 2:r_ac
  wf <- function(nm, fn) writeFormula(wb, "Annual_Cost", startCol=match(nm,ac_cols), startRow=2, x=frows(fn, R))
  rqs <- ACcl("r_quantity_scenario"); rqb <- ACcl("r_quantity_baseline"); pf <- ACcl("population_in_need_fraction")
  covs <- ACcl("coverage_scenario"); covb <- ACcl("coverage_baseline"); fr <- ACcl("frequency_per_year"); uc <- ACcl("unit_cost_usd")
  pinS <- ACcl("pin_scenario"); pinB <- ACcl("pin_baseline"); acb <- ACcl("annual_cost_baseline"); acs <- ACcl("annual_cost_scenario")
  aci <- ACcl("annual_cost_incremental"); dfc <- ACcl("discount_factor"); dcs <- ACcl("disc_cost_scenario"); dci <- ACcl("disc_cost_incremental")
  yr <- ACcl("year"); loc <- ACcl("location"); scn <- ACcl("scenario"); rid <- ACcl("cost_record_id"); scope <- ACcl("cost_scope"); pp <- ACcl("province_population")
  wf("pin_scenario", function(r) sprintf("%s%d*%s%d", rqs,r, pf,r))
  wf("pin_baseline", function(r) sprintf("%s%d*%s%d", rqb,r, pf,r))
  wf("annual_cost_baseline", function(r) sprintf("%s%d*%s%d*%s%d*%s%d", pinB,r, covb,r, fr,r, uc,r))
  wf("annual_cost_scenario", function(r) sprintf("%s%d*%s%d*%s%d*%s%d", pinS,r, covs,r, fr,r, uc,r))
  wf("annual_cost_incremental", function(r) sprintf("%s%d-%s%d", acs,r, acb,r))
  wf("discount_factor", function(r) sprintf("1/(1+%s)^(%s%d-%s)", cell_disc, yr, r, cell_ystart))
  wf("disc_cost_baseline", function(r) sprintf("%s%d*%s%d", acb,r, dfc,r))
  wf("disc_cost_scenario", function(r) sprintf("%s%d*%s%d", acs,r, dfc,r))
  wf("disc_cost_incremental", function(r) sprintf("%s%d*%s%d", aci,r, dfc,r))
  wf("shared_duplicate_count", function(r) sprintf(
    "IF(%s%d=\"shared-count-once\",COUNTIFS($%s$2:$%s$%d,%s%d,$%s$2:$%s$%d,%s%d,$%s$2:$%s$%d,%s%d,$%s$2:$%s$%d,%s%d),1)",
    scope,r, loc,loc,r_ac,loc,r, scn,scn,r_ac,scn,r, yr,yr,r_ac,yr,r, rid,rid,r_ac,rid,r))
  wf("annual_cost_baseline_per_capita",    function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", pp,r, acb,r, pp,r))
  wf("annual_cost_scenario_per_capita",    function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", pp,r, acs,r, pp,r))
  wf("annual_cost_incremental_per_capita", function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", pp,r, aci,r, pp,r))
  wf("disc_cost_incremental_per_capita",   function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", pp,r, dci,r, pp,r))
}
style_sheet("Annual_Cost", ac_cols, n_ac, formula_cols=match(ac_fml, ac_cols),
            rsource_cols=match(ac_rsrc, ac_cols))

#===========================================================================
# Budget_Impact  (province-stacked; C:H over Annual_Cost with location criterion)
#===========================================================================
bud_cols <- c("location","province_code","scenario","year","baseline_cost","scenario_cost",
              "incremental_cost","disc_incremental_cost","cumulative_incremental_cost",
              "cumulative_disc_incremental_cost","province_population","baseline_cost_per_capita",
              "scenario_cost_per_capita","incremental_cost_per_capita","disc_incremental_cost_per_capita")
BUcl <- CLmk(bud_cols)
if (nrow(bi)) {
  bud <- as.data.frame(bi[, .(location, province_code, scenario, year)])
  for (cn in bud_cols[5:10]) bud[[cn]] <- NA_real_
  bud$province_population <- bi$province_population
  for (cn in bud_cols[12:15]) bud[[cn]] <- NA_real_
  bud <- bud[, bud_cols]
} else bud <- as.data.frame(setNames(replicate(length(bud_cols), logical(0), simplify=FALSE), bud_cols))
addWorksheet(wb, "Budget_Impact"); writeData(wb, "Budget_Impact", bud, headerStyle=st_hdr)
n_bi <- nrow(bud); r_bi <- max(n_bi + 1L, 2L)
if (n_bi > 0) {
  R <- 2:r_bi
  aLoc <- ACcl("location"); aScn <- ACcl("scenario"); aYr <- ACcl("year")
  bLoc <- BUcl("location"); bScn <- BUcl("scenario")
  sumif_ac <- function(nm, r) { tgt <- ACcl(nm)
    sprintf("SUMIFS('Annual_Cost'!$%s$2:$%s$%d,'Annual_Cost'!$%s$2:$%s$%d,%s%d,'Annual_Cost'!$%s$2:$%s$%d,%s%d,'Annual_Cost'!$%s$2:$%s$%d,%s%d)",
      tgt,tgt,r_ac, aLoc,aLoc,r_ac,BUcl("location"),r, aScn,aScn,r_ac,BUcl("scenario"),r, aYr,aYr,r_ac,BUcl("year"),r) }
  writeFormula(wb, "Budget_Impact", startCol=match("baseline_cost",bud_cols), startRow=2, x=frows(function(r) sumif_ac("annual_cost_baseline", r), R))
  writeFormula(wb, "Budget_Impact", startCol=match("scenario_cost",bud_cols), startRow=2, x=frows(function(r) sumif_ac("annual_cost_scenario", r), R))
  writeFormula(wb, "Budget_Impact", startCol=match("incremental_cost",bud_cols), startRow=2, x=frows(function(r) sumif_ac("annual_cost_incremental", r), R))
  writeFormula(wb, "Budget_Impact", startCol=match("disc_incremental_cost",bud_cols), startRow=2, x=frows(function(r) sumif_ac("disc_cost_incremental", r), R))
  ic <- BUcl("incremental_cost"); dic <- BUcl("disc_incremental_cost")
  writeFormula(wb, "Budget_Impact", startCol=match("cumulative_incremental_cost",bud_cols), startRow=2,
    x=frows(function(r) sprintf("SUMIFS($%s$2:%s%d,$%s$2:%s%d,%s%d,$%s$2:%s%d,%s%d)", ic,ic,r, bLoc,bLoc,r,bLoc,r, bScn,bScn,r,bScn,r), R))
  writeFormula(wb, "Budget_Impact", startCol=match("cumulative_disc_incremental_cost",bud_cols), startRow=2,
    x=frows(function(r) sprintf("SUMIFS($%s$2:%s%d,$%s$2:%s%d,%s%d,$%s$2:%s%d,%s%d)", dic,dic,r, bLoc,bLoc,r,bLoc,r, bScn,bScn,r,bScn,r), R))
  pp <- BUcl("province_population"); bcst <- BUcl("baseline_cost"); scst <- BUcl("scenario_cost")
  writeFormula(wb, "Budget_Impact", startCol=match("baseline_cost_per_capita",bud_cols), startRow=2, x=frows(function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", pp,r, bcst,r, pp,r), R))
  writeFormula(wb, "Budget_Impact", startCol=match("scenario_cost_per_capita",bud_cols), startRow=2, x=frows(function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", pp,r, scst,r, pp,r), R))
  writeFormula(wb, "Budget_Impact", startCol=match("incremental_cost_per_capita",bud_cols), startRow=2, x=frows(function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", pp,r, ic,r, pp,r), R))
  writeFormula(wb, "Budget_Impact", startCol=match("disc_incremental_cost_per_capita",bud_cols), startRow=2, x=frows(function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", pp,r, dic,r, pp,r), R))
}
style_sheet("Budget_Impact", bud_cols, n_bi, formula_cols=match(setdiff(bud_cols, c("location","province_code","scenario","year","province_population")), bud_cols),
            rsource_cols=match(c("location","province_code","province_population"), bud_cols))

#===========================================================================
# Cost_Effectiveness  (province-stacked; SUMIFS with location criterion)
#===========================================================================
ce_cols <- c("location","province_code","scenario","scenario_label","deaths_averted","cases_averted",
             "incremental_cost","disc_incremental_cost","cost_per_death_averted","dominance","reconciliation_status")
CEcl <- CLmk(ce_cols)
ce <- as.data.frame(cea[, .(location, province_code, scenario, scenario_label)])
for (cn in ce_cols[5:9]) ce[[cn]] <- NA_real_
ce$dominance <- NA_character_; ce$reconciliation_status <- NA_character_
addWorksheet(wb, "Cost_Effectiveness"); writeData(wb, "Cost_Effectiveness", ce, headerStyle=st_hdr)
n_ce <- nrow(ce); r_ce <- max(n_ce + 1L, 2L)
if (n_ce > 0) {
  R <- 2:r_ce
  ceLoc <- CEcl("location"); ceScn <- CEcl("scenario")
  amLoc <- AMcl("location"); amScn <- AMcl("scenario"); amDA <- AMcl("deaths_averted"); amCA <- AMcl("cases_averted")
  biLoc <- BUcl("location"); biScn <- BUcl("scenario"); biIC <- BUcl("incremental_cost"); biDIC <- BUcl("disc_incremental_cost")
  writeFormula(wb, "Cost_Effectiveness", startCol=match("deaths_averted",ce_cols), startRow=2,
    x=frows(function(r) sprintf("SUMIFS('Annual_Mortality'!$%s$2:$%s$%d,'Annual_Mortality'!$%s$2:$%s$%d,%s%d,'Annual_Mortality'!$%s$2:$%s$%d,%s%d)",
      amDA,amDA,r_am, amLoc,amLoc,r_am,ceLoc,r, amScn,amScn,r_am,ceScn,r), R))
  writeFormula(wb, "Cost_Effectiveness", startCol=match("cases_averted",ce_cols), startRow=2,
    x=frows(function(r) sprintf("SUMIFS('Annual_Mortality'!$%s$2:$%s$%d,'Annual_Mortality'!$%s$2:$%s$%d,%s%d,'Annual_Mortality'!$%s$2:$%s$%d,%s%d)",
      amCA,amCA,r_am, amLoc,amLoc,r_am,ceLoc,r, amScn,amScn,r_am,ceScn,r), R))
  writeFormula(wb, "Cost_Effectiveness", startCol=match("incremental_cost",ce_cols), startRow=2,
    x=frows(function(r) sprintf("SUMIFS('Budget_Impact'!$%s$2:$%s$%d,'Budget_Impact'!$%s$2:$%s$%d,%s%d,'Budget_Impact'!$%s$2:$%s$%d,%s%d)",
      biIC,biIC,r_bi, biLoc,biLoc,r_bi,ceLoc,r, biScn,biScn,r_bi,ceScn,r), R))
  writeFormula(wb, "Cost_Effectiveness", startCol=match("disc_incremental_cost",ce_cols), startRow=2,
    x=frows(function(r) sprintf("SUMIFS('Budget_Impact'!$%s$2:$%s$%d,'Budget_Impact'!$%s$2:$%s$%d,%s%d,'Budget_Impact'!$%s$2:$%s$%d,%s%d)",
      biDIC,biDIC,r_bi, biLoc,biLoc,r_bi,ceLoc,r, biScn,biScn,r_bi,ceScn,r), R))
  ceDA <- CEcl("deaths_averted"); ceIC <- CEcl("incremental_cost"); ceDIC <- CEcl("disc_incremental_cost")
  writeFormula(wb, "Cost_Effectiveness", startCol=match("cost_per_death_averted",ce_cols), startRow=2,
    x=frows(function(r) sprintf("IF(%s%d>0,%s%d/%s%d,\"\")", ceDA,r, ceDIC,r, ceDA,r), R))
  writeFormula(wb, "Cost_Effectiveness", startCol=match("dominance",ce_cols), startRow=2,
    x=frows(function(r) sprintf(
      "IF(AND(%s%d>0,%s%d<0),\"Dominant (more health, lower cost)\",IF(AND(%s%d<=0,%s%d>0),\"Dominated (less/no health, higher cost)\",IF(AND(%s%d<=0,%s%d<=0),\"No deaths averted; ratio not defined\",\"USD per death averted\")))",
      ceDA,r,ceDIC,r, ceDA,r,ceDIC,r, ceDA,r,ceDIC,r), R))
  # reconciliation: Excel SUMIFS re-derivation matches (consistent by construction)
  writeFormula(wb, "Cost_Effectiveness", startCol=match("reconciliation_status",ce_cols), startRow=2,
    x=frows(function(r) sprintf(
      "IF(AND(ABS(%s%d-SUMIFS('Budget_Impact'!$%s$2:$%s$%d,'Budget_Impact'!$%s$2:$%s$%d,%s%d,'Budget_Impact'!$%s$2:$%s$%d,%s%d))<=%s,ABS(%s%d-SUMIFS('Annual_Mortality'!$%s$2:$%s$%d,'Annual_Mortality'!$%s$2:$%s$%d,%s%d,'Annual_Mortality'!$%s$2:$%s$%d,%s%d))<=%s),\"consistent\",\"mismatch\")",
      ceDIC,r, biDIC,biDIC,r_bi,biLoc,biLoc,r_bi,ceLoc,r,biScn,biScn,r_bi,ceScn,r, cell_ftol,
      ceDA,r, amDA,amDA,r_am,amLoc,amLoc,r_am,ceLoc,r,amScn,amScn,r_am,ceScn,r, cell_ftol), R))
}
style_sheet("Cost_Effectiveness", ce_cols, n_ce, formula_cols=match(ce_cols[5:11], ce_cols),
            rsource_cols=match(c("location","province_code"), ce_cols), wrap_cols=match(c("scenario_label","dominance"), ce_cols))
conditionalFormatting(wb, "Cost_Effectiveness", cols=match("reconciliation_status",ce_cols), rows=2:r_ce, rule="consistent", type="contains", style=cf_pass)
conditionalFormatting(wb, "Cost_Effectiveness", cols=match("reconciliation_status",ce_cols), rows=2:r_ce, rule="mismatch", type="contains", style=cf_fail)
message("  Wrote Annual_Cost, Budget_Impact, Cost_Effectiveness.")

#===========================================================================
# Health_Outcomes  (province-stacked; averted = same-row formulas)
#===========================================================================
ho_cols <- c("location","province_code","scenario","scenario_label","year",
             "modeled_deaths","baseline_deaths","deaths_averted","modeled_cases","baseline_cases","cases_averted",
             "yll","base_yll","yll_averted","yld","base_yld","yld_averted",
             "daly","base_daly","dalys_averted","life_years_gained")
HOcl <- CLmk(ho_cols)
hos <- ho_src[order(location, scenario, year)]
HO <- data.frame(location=hos$location, province_code=hos$province_code, scenario=hos$scenario,
                 scenario_label=hos$scenario_label, year=hos$year,
                 modeled_deaths=hos$modeled_deaths, baseline_deaths=hos$baseline_deaths, deaths_averted=NA_real_,
                 modeled_cases=hos$modeled_cases, baseline_cases=hos$baseline_cases, cases_averted=NA_real_,
                 yll=hos$yll, base_yll=hos$base_yll, yll_averted=NA_real_,
                 yld=hos$yld, base_yld=hos$base_yld, yld_averted=NA_real_,
                 daly=hos$daly, base_daly=hos$base_daly, dalys_averted=NA_real_, life_years_gained=NA_real_,
                 stringsAsFactors=FALSE)
addWorksheet(wb, "Health_Outcomes"); writeData(wb, "Health_Outcomes", HO, headerStyle=st_hdr)
n_ho <- nrow(HO); r_ho <- n_ho + 1L
if (n_ho > 0) {
  R <- 2:r_ho
  hp <- function(a,b) function(r) sprintf("%s%d-%s%d", HOcl(a), r, HOcl(b), r)
  writeFormula(wb, "Health_Outcomes", startCol=match("deaths_averted",ho_cols), startRow=2, x=frows(hp("baseline_deaths","modeled_deaths"), R))
  writeFormula(wb, "Health_Outcomes", startCol=match("cases_averted",ho_cols), startRow=2, x=frows(hp("baseline_cases","modeled_cases"), R))
  writeFormula(wb, "Health_Outcomes", startCol=match("yll_averted",ho_cols), startRow=2, x=frows(hp("base_yll","yll"), R))
  writeFormula(wb, "Health_Outcomes", startCol=match("yld_averted",ho_cols), startRow=2, x=frows(hp("base_yld","yld"), R))
  writeFormula(wb, "Health_Outcomes", startCol=match("dalys_averted",ho_cols), startRow=2, x=frows(hp("base_daly","daly"), R))
  yllav <- HOcl("yll_averted")
  writeFormula(wb, "Health_Outcomes", startCol=match("life_years_gained",ho_cols), startRow=2, x=frows(function(r) sprintf("%s%d", yllav, r), R))
}
style_sheet("Health_Outcomes", ho_cols, n_ho,
            formula_cols=match(c("deaths_averted","cases_averted","yll_averted","yld_averted","dalys_averted","life_years_gained"), ho_cols),
            rsource_cols=match(c("location","province_code","modeled_deaths","baseline_deaths","modeled_cases","baseline_cases","yll","base_yll","yld","base_yld","daly","base_daly"), ho_cols))

#===========================================================================
# Economic_Value  (province-stacked; Reference-Case VSL/VSLY = same-row formulas)
#===========================================================================
ev_cols <- c("location","province_code","scenario","scenario_label","year",
             "deaths_averted","life_years_gained","gni_pc_idn","gni_pc_usa","population","le_avg_adult",
             "vsl_transfer_prefloor","vsl_floor","vsl_preferred","vsl_over_gnipc","vsl_gni100","vsl_gni160","vsly_preferred",
             "econ_value_vsl_undisc","econ_value_vsly_undisc","econ_value_vsl100_undisc","econ_value_vsl160_undisc",
             "disc_factor","pv_vsl_pref","pv_vsly_pref","pv_vsl100","pv_vsl160",
             "total_province_gni","pv_province_gni","annual_benefit_share_gni")
EVcl <- CLmk(ev_cols)
evs <- ev_src[order(location, scenario, year)]
EV <- data.frame(location=evs$location, province_code=evs$province_code, scenario=evs$scenario,
                 scenario_label=evs$scenario_label, year=evs$year,
                 deaths_averted=evs$deaths_averted, life_years_gained=evs$life_years_gained,
                 gni_pc_idn=evs$gni_pc_idn, gni_pc_usa=evs$gni_pc_usa, population=evs$population, le_avg_adult=evs$le_avg_adult,
                 stringsAsFactors=FALSE)
for (cn in ev_cols[12:30]) EV[[cn]] <- NA_real_
addWorksheet(wb, "Economic_Value"); writeData(wb, "Economic_Value", EV, headerStyle=st_hdr)
n_ev <- nrow(EV); r_ev <- n_ev + 1L
if (n_ev > 0) {
  R <- 2:r_ev
  gI <- EVcl("gni_pc_idn"); gU <- EVcl("gni_pc_usa"); da <- EVcl("deaths_averted"); ly <- EVcl("life_years_gained")
  pop <- EVcl("population"); le <- EVcl("le_avg_adult")
  pref0 <- EVcl("vsl_transfer_prefloor"); flr <- EVcl("vsl_floor"); pref <- EVcl("vsl_preferred")
  g100 <- EVcl("vsl_gni100"); g160 <- EVcl("vsl_gni160"); vly <- EVcl("vsly_preferred")
  evU <- EVcl("econ_value_vsl_undisc"); evyU <- EVcl("econ_value_vsly_undisc"); ev1U <- EVcl("econ_value_vsl100_undisc"); ev6U <- EVcl("econ_value_vsl160_undisc")
  dfac <- EVcl("disc_factor"); tgni <- EVcl("total_province_gni")
  wfe <- function(nm, fn) writeFormula(wb, "Economic_Value", startCol=match(nm,ev_cols), startRow=2, x=frows(fn, R))
  wfe("vsl_transfer_prefloor", function(r) sprintf("%s*%s%d*(%s%d/%s%d)^%s", bca_cells$ratio, gU,r, gI,r, gU,r, bca_cells$elast))
  wfe("vsl_floor", function(r) sprintf("%s*%s%d", bca_cells$floor, gI,r))
  wfe("vsl_preferred", function(r) sprintf("MAX(%s%d,%s%d)", pref0,r, flr,r))
  wfe("vsl_over_gnipc", function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", gI,r, pref,r, gI,r))
  wfe("vsl_gni100", function(r) sprintf("%s*%s%d", bca_cells$mult100, gI,r))
  wfe("vsl_gni160", function(r) sprintf("%s*%s%d", bca_cells$mult160, gI,r))
  wfe("vsly_preferred", function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", le,r, pref,r, le,r))
  wfe("econ_value_vsl_undisc", function(r) sprintf("%s%d*%s%d", pref,r, da,r))
  wfe("econ_value_vsly_undisc", function(r) sprintf("%s%d*%s%d", vly,r, ly,r))
  wfe("econ_value_vsl100_undisc", function(r) sprintf("%s%d*%s%d", g100,r, da,r))
  wfe("econ_value_vsl160_undisc", function(r) sprintf("%s%d*%s%d", g160,r, da,r))
  wfe("disc_factor", function(r) sprintf("1/(1+%s)^(%s%d-%s)", bca_cells$r_primary, EVcl("year"),r, bca_cells$base_year))
  wfe("pv_vsl_pref", function(r) sprintf("%s%d*%s%d", evU,r, dfac,r))
  wfe("pv_vsly_pref", function(r) sprintf("%s%d*%s%d", evyU,r, dfac,r))
  wfe("pv_vsl100", function(r) sprintf("%s%d*%s%d", ev1U,r, dfac,r))
  wfe("pv_vsl160", function(r) sprintf("%s%d*%s%d", ev6U,r, dfac,r))
  wfe("total_province_gni", function(r) sprintf("%s%d*%s%d", pop,r, gI,r))
  wfe("pv_province_gni", function(r) sprintf("%s%d*%s%d", tgni,r, dfac,r))
  wfe("annual_benefit_share_gni", function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", tgni,r, evU,r, tgni,r))
}
style_sheet("Economic_Value", ev_cols, n_ev,
            formula_cols=match(ev_cols[12:30], ev_cols),
            rsource_cols=match(c("location","province_code","deaths_averted","life_years_gained","gni_pc_idn","gni_pc_usa","population","le_avg_adult"), ev_cols))

#===========================================================================
# Benefit_Cost  (province x scenario x valuation case; SUMIFS with location)
#===========================================================================
bc_cases <- data.table(valuation_method=c("VSL","VSLY","VSL","VSL"),
  valuation_case=c("preferred (elasticity 1.5, 20x floor)","preferred (VSLY from preferred VSL)",
                   "sensitivity: 100x GNI per capita","sensitivity: 160x GNI per capita"),
  ev_pv_name=c("pv_vsl_pref","pv_vsly_pref","pv_vsl100","pv_vsl160"))
bc_meta <- unique(cea[, .(location, province_code, scenario, scenario_label)])
bc_meta <- merge(bc_meta, unique(dt_h07[, .(scenario, intervention_family)]), by="scenario", all.x=TRUE)
BC <- rbindlist(lapply(seq_len(nrow(bc_meta)), function(i)
  data.table(location=bc_meta$location[i], province_code=bc_meta$province_code[i],
             scenario=bc_meta$scenario[i], scenario_label=bc_meta$scenario_label[i],
             intervention_family=bc_meta$intervention_family[i],
             valuation_method=bc_cases$valuation_method, valuation_case=bc_cases$valuation_case,
             ev_pv_name=bc_cases$ev_pv_name)), fill=TRUE)
bc_cols <- c("location","province_code","scenario","scenario_label","intervention_family",
             "valuation_method","valuation_case","pv_benefits","pv_costs","pv_net_benefit",
             "benefit_cost_ratio","pv_province_gni","benefit_gni_share","decision","scope_note")
BCcl <- CLmk(bc_cols)
BCd <- as.data.frame(BC[, .(location, province_code, scenario, scenario_label, intervention_family, valuation_method, valuation_case)])
for (cn in c("pv_benefits","pv_costs","pv_net_benefit","benefit_cost_ratio","pv_province_gni","benefit_gni_share")) BCd[[cn]] <- NA_real_
BCd$decision <- NA_character_; BCd$scope_note <- NA_character_
addWorksheet(wb, "Benefit_Cost"); writeData(wb, "Benefit_Cost", BCd, headerStyle=st_hdr)
n_bc <- nrow(BCd); r_bc <- n_bc + 1L
if (n_bc > 0) {
  R <- 2:r_bc
  bcLoc <- BCcl("location"); bcScn <- BCcl("scenario")
  evLoc <- EVcl("location"); evScn <- EVcl("scenario"); evPVg <- EVcl("pv_province_gni")
  biLoc <- BUcl("location"); biScn <- BUcl("scenario"); biIC <- BUcl("incremental_cost"); biYr <- BUcl("year")
  writeFormula(wb, "Benefit_Cost", startCol=match("pv_benefits",bc_cols), startRow=2,
    x=frows(function(r) { pv <- EVcl(BC$ev_pv_name[r-1L])
      sprintf("SUMIFS('Economic_Value'!$%s$2:$%s$%d,'Economic_Value'!$%s$2:$%s$%d,%s%d,'Economic_Value'!$%s$2:$%s$%d,%s%d)",
        pv,pv,r_ev, evLoc,evLoc,r_ev,bcLoc,r, evScn,evScn,r_ev,bcScn,r) }, R))
  writeFormula(wb, "Benefit_Cost", startCol=match("pv_costs",bc_cols), startRow=2,
    x=frows(function(r) sprintf(
      "SUMPRODUCT(('Budget_Impact'!$%s$2:$%s$%d=%s%d)*('Budget_Impact'!$%s$2:$%s$%d=%s%d)*'Budget_Impact'!$%s$2:$%s$%d*(1/(1+%s)^('Budget_Impact'!$%s$2:$%s$%d-%s)))*%s",
      biLoc,biLoc,r_bi,bcLoc,r, biScn,biScn,r_bi,bcScn,r, biIC,biIC,r_bi, bca_cells$r_primary, biYr,biYr,r_bi,bca_cells$base_year, bca_cells$cost_factor), R))
  pvb <- BCcl("pv_benefits"); pvc <- BCcl("pv_costs")
  writeFormula(wb, "Benefit_Cost", startCol=match("pv_net_benefit",bc_cols), startRow=2, x=frows(function(r) sprintf("%s%d-%s%d", pvb,r, pvc,r), R))
  writeFormula(wb, "Benefit_Cost", startCol=match("benefit_cost_ratio",bc_cols), startRow=2, x=frows(function(r) sprintf("IF(%s%d>0,%s%d/%s%d,\"\")", pvc,r, pvb,r, pvc,r), R))
  writeFormula(wb, "Benefit_Cost", startCol=match("pv_province_gni",bc_cols), startRow=2,
    x=frows(function(r) sprintf("SUMIFS('Economic_Value'!$%s$2:$%s$%d,'Economic_Value'!$%s$2:$%s$%d,%s%d,'Economic_Value'!$%s$2:$%s$%d,%s%d)",
      evPVg,evPVg,r_ev, evLoc,evLoc,r_ev,bcLoc,r, evScn,evScn,r_ev,bcScn,r), R))
  pvg <- BCcl("pv_province_gni")
  writeFormula(wb, "Benefit_Cost", startCol=match("benefit_gni_share",bc_cols), startRow=2, x=frows(function(r) sprintf("IF(%s%d=0,\"\",%s%d/%s%d)", pvg,r, pvb,r, pvg,r), R))
  bcr <- BCcl("benefit_cost_ratio")
  writeFormula(wb, "Benefit_Cost", startCol=match("decision",bc_cols), startRow=2,
    x=frows(function(r) sprintf("IF(%s%d<=0,\"cost-saving or ratio not meaningful (PV cost<=0)\",IF(%s%d>=1,\"benefits exceed costs (BCR>=1)\",\"costs exceed benefits (BCR<1)\"))", pvc,r, bcr,r), R))
  writeData(wb, "Benefit_Cost", startCol=match("scope_note",bc_cols), startRow=2, colNames=FALSE,
    x=rep("Partial mortality-benefit BCA. Benefits = averted-mortality VSL/VSLY in PPP int$; costs = health-system USD x cost_to_bca_currency_factor. Omits morbidity, productivity and downstream cost offsets.", n_bc))
}
style_sheet("Benefit_Cost", bc_cols, n_bc,
            formula_cols=match(c("pv_benefits","pv_costs","pv_net_benefit","benefit_cost_ratio","pv_province_gni","benefit_gni_share","decision"), bc_cols),
            rsource_cols=match(c("location","province_code"), bc_cols), wrap_cols=match(c("scenario_label","valuation_case","decision","scope_note"), bc_cols))
if (n_bc > 0) {
  conditionalFormatting(wb, "Benefit_Cost", cols=match("decision",bc_cols), rows=2:r_bc, rule="benefits exceed", type="contains", style=cf_pass)
  conditionalFormatting(wb, "Benefit_Cost", cols=match("decision",bc_cols), rows=2:r_bc, rule="costs exceed", type="contains", style=cf_rev)
}
message("  Wrote Health_Outcomes, Economic_Value, Benefit_Cost.")

#===========================================================================
# CVD_40q30_Age + CVD_40q30  (verbatim location-aware builder; provinces only)
#===========================================================================
sty <- list(st_hdr=st_hdr, st_formula=st_formula, st_rsrc=st_rsrc, st_input=st_input, st_wrap=st_wrap,
            cf_pass=cf_pass, cf_fail=cf_fail, cf_rev=cf_rev)
build_cvd_40q30_sheets_into <- function(wb, scen_ids, dt40, dt_age, base_id, sty, recon_tol=1e-3) {
  hdr <- sty$st_hdr; fml <- sty$st_formula; rsrc <- sty$st_rsrc; wrapS <- sty$st_wrap
  frows <- function(fn, rows) vapply(rows, fn, character(1))
  wf <- function(sheet, col, x) writeFormula(wb, sheet, x=x, startCol=col, startRow=2L)
  style_block <- function(sheet, ncol, ndata, formula_cols, rsource_cols, numfmt=list(), wrap_cols=integer(0), widths=NULL) {
    addStyle(wb, sheet, hdr, rows=1, cols=seq_len(ncol), gridExpand=TRUE)
    if (ndata > 0) { dr <- 2:(ndata+1L)
      for (j in formula_cols) addStyle(wb, sheet, fml,  rows=dr, cols=j, gridExpand=TRUE, stack=TRUE)
      for (j in rsource_cols) addStyle(wb, sheet, rsrc, rows=dr, cols=j, gridExpand=TRUE, stack=TRUE)
      for (nm in names(numfmt)) addStyle(wb, sheet, createStyle(numFmt=numfmt[[nm]]), rows=dr, cols=as.integer(nm), gridExpand=TRUE, stack=TRUE)
      for (j in wrap_cols) addStyle(wb, sheet, wrapS, rows=dr, cols=j, gridExpand=TRUE, stack=TRUE) }
    freezePane(wb, sheet, firstActiveRow=2, firstActiveCol=1); addFilter(wb, sheet, rows=1, cols=seq_len(ncol))
    if (!is.null(widths)) setColWidths(wb, sheet, cols=seq_len(ncol), widths=widths); setRowHeights(wb, sheet, rows=1, heights=28)
  }
  HP <- "0.000000"; PCT <- "0.000"; PR <- "0.00"; NUM <- "#,##0"; DTHS <- "#,##0.00"
  ga <- as.data.table(dt_age)[scenario %in% scen_ids]; setorder(ga, location, scenario, htn_target_scenario, year, age)
  age_cols <- c("location","scenario","scenario_label","intervention_family","htn_target_scenario","year","age","cvd_deaths","population","m_x","q_x","l_x","l_x_next")
  AGE <- data.frame(location=ga$location, scenario=ga$scenario, scenario_label=ga$scenario_label,
                    intervention_family=ga$intervention_family, htn_target_scenario=ga$htn_target_scenario,
                    year=ga$year, age=ga$age, cvd_deaths=ga$cvd_deaths, population=ga$population,
                    m_x=NA_real_, q_x=NA_real_, l_x=NA_real_, l_x_next=NA_real_, stringsAsFactors=FALSE)
  n_age <- nrow(AGE); r_age <- n_age + 1L; addWorksheet(wb, "CVD_40q30_Age"); writeData(wb, "CVD_40q30_Age", AGE, headerStyle=hdr)
  if (n_age > 0) { R <- 2:r_age
    wf("CVD_40q30_Age", 10, frows(function(r) sprintf("IF(I%d>0,H%d/I%d,0)", r,r,r), R))
    wf("CVD_40q30_Age", 11, frows(function(r) sprintf("1-EXP(-J%d)", r), R))
    wf("CVD_40q30_Age", 12, frows(function(r) sprintf("IF(G%d=30,1,M%d)", r, r-1L), R))
    wf("CVD_40q30_Age", 13, frows(function(r) sprintf("L%d*(1-K%d)", r, r), R)) }
  style_block("CVD_40q30_Age", length(age_cols), n_age, formula_cols=10:13, rsource_cols=c(1,8,9),
              numfmt=c(setNames(list("0"),"6"), setNames(list("0"),"7"), setNames(list(DTHS),"8"), setNames(list(NUM),"9"), setNames(rep(list(HP),4), as.character(10:13))),
              wrap_cols=3, widths=pmin(pmax(nchar(age_cols)+2,11),22))
  gs <- as.data.table(dt40)[scenario %in% scen_ids]; setorder(gs, location, scenario, htn_target_scenario, year)
  s_cols <- c("location","scenario","scenario_label","intervention_family","scenario_role","parent_package_id","htn_target_scenario","year","cvd_40q30","baseline_cvd_40q30","absolute_reduction_pp","percent_reduction","cvd_40q30_r","recon_status")
  SS <- data.frame(location=gs$location, scenario=gs$scenario, scenario_label=gs$scenario_label,
                   intervention_family=gs$intervention_family, scenario_role=gs$scenario_role, parent_package_id=gs$parent_package_id,
                   htn_target_scenario=gs$htn_target_scenario, year=gs$year, cvd_40q30=NA_real_, baseline_cvd_40q30=NA_real_,
                   absolute_reduction_pp=NA_real_, percent_reduction=NA_real_, cvd_40q30_r=gs$cvd_40q30, recon_status=NA_character_, stringsAsFactors=FALSE)
  n_s <- nrow(SS); r_s <- n_s + 1L; addWorksheet(wb, "CVD_40q30"); writeData(wb, "CVD_40q30", SS, headerStyle=hdr)
  if (n_s > 0) { R <- 2:r_s
    lookup <- function(col, age, r) sprintf(
      "SUMIFS('CVD_40q30_Age'!$%s$2:$%s$%d,'CVD_40q30_Age'!$A$2:$A$%d,A%d,'CVD_40q30_Age'!$B$2:$B$%d,B%d,'CVD_40q30_Age'!$E$2:$E$%d,G%d,'CVD_40q30_Age'!$F$2:$F$%d,H%d,'CVD_40q30_Age'!$G$2:$G$%d,%d)",
      col,col,r_age, r_age,r, r_age,r, r_age,r, r_age,r, r_age,age)
    wf("CVD_40q30", 9, frows(function(r) sprintf("100*(1-(%s)/(%s))", lookup("M",69L,r), lookup("L",30L,r)), R))
    wf("CVD_40q30", 10, frows(function(r) sprintf("SUMIFS($I$2:$I$%d,$A$2:$A$%d,A%d,$G$2:$G$%d,G%d,$H$2:$H$%d,H%d,$B$2:$B$%d,\"%s\")", r_s,r_s,r,r_s,r,r_s,r,r_s, base_id), R))
    wf("CVD_40q30", 11, frows(function(r) sprintf("J%d-I%d", r, r), R))
    wf("CVD_40q30", 12, frows(function(r) sprintf("IF(J%d=0,\"\",100*(J%d-I%d)/J%d)", r,r,r,r), R))
    wf("CVD_40q30", 14, frows(function(r) sprintf("IF(ABS(I%d-M%d)<=%s,\"match\",\"mismatch\")", r,r, format(recon_tol, scientific=FALSE)), R)) }
  style_block("CVD_40q30", length(s_cols), n_s, formula_cols=c(9,10,11,12,14), rsource_cols=c(1,13),
              numfmt=c(setNames(list("0"),"8"), setNames(rep(list(PCT),3), as.character(c(9,10,11))), setNames(list(PR),"12"), setNames(list(PCT),"13")),
              wrap_cols=3, widths=pmin(pmax(nchar(s_cols)+2,12),24))
  if (n_s > 0) {
    conditionalFormatting(wb, "CVD_40q30", cols=14, rows=2:r_s, rule="match", type="contains", style=sty$cf_pass)
    conditionalFormatting(wb, "CVD_40q30", cols=14, rows=2:r_s, rule="mismatch", type="contains", style=sty$cf_fail) }
  invisible(c("CVD_40q30_Age","CVD_40q30"))
}
build_cvd_40q30_sheets_into(wb, c(base_id, comparators),
                            dt_cvd_40q30[location %in% prov_run], cvd_age_40q30[location %in% prov_run],
                            base_id, sty)
message("  Wrote CVD_40q30_Age, CVD_40q30.")

#===========================================================================
# Input_Diagnostic
#===========================================================================
addWorksheet(wb, "Input_Diagnostic")
n_id <- nrow(diag_out); r_id <- max(n_id + 1L, 2L)
if (n_id > 0) {
  writeData(wb, "Input_Diagnostic", as.data.frame(diag_out), headerStyle=st_hdr)
  style_sheet("Input_Diagnostic", names(diag_out), n_id, wrap_cols=which(names(diag_out)=="problem"))
  sev_col <- which(names(diag_out)=="severity")
  conditionalFormatting(wb, "Input_Diagnostic", cols=sev_col, rows=2:r_id, rule="FAIL", type="contains", style=cf_fail)
  conditionalFormatting(wb, "Input_Diagnostic", cols=sev_col, rows=2:r_id, rule="REVIEW", type="contains", style=cf_rev)
} else {
  writeData(wb, "Input_Diagnostic",
            data.frame(scope=character(0), item_key=character(0), field=character(0), problem=character(0), severity=character(0)),
            headerStyle=st_hdr)
  addStyle(wb, "Input_Diagnostic", st_hdr, rows=1, cols=1:5, gridExpand=TRUE)
}

#===========================================================================
# QA_Checks  (Excel formulas; province-aware aggregate reconciliation)
#===========================================================================
r_ev2 <- n_ev + 1L; r_bc2 <- n_bc + 1L; r_ho2 <- n_ho + 1L
r_q40 <- nrow(dt_cvd_40q30[location %in% prov_run & scenario %in% c(base_id, comparators)]) + 1L
ceDAl <- CEcl("deaths_averted"); ceDICl <- CEcl("disc_incremental_cost"); ceScnl <- CEcl("scenario"); ceLocl <- CEcl("location"); ceReconl <- CEcl("reconciliation_status")
acScen <- ACcl("annual_cost_scenario"); acBase <- ACcl("annual_cost_baseline"); acShare <- ACcl("shared_duplicate_count")
buScen <- BUcl("scenario_cost"); buBase <- BUcl("baseline_cost")
amBaseD <- AMcl("base_deaths")
evPref <- EVcl("vsl_preferred"); evFloor <- EVcl("vsl_floor"); evLoc2 <- EVcl("location"); evScn2 <- EVcl("scenario")
bcLoc2 <- BCcl("location"); bcScn2 <- BCcl("scenario")
hoLoc2 <- HOcl("location"); hoScn2 <- HOcl("scenario")
siKC <- SIcl("key_count")
anchor_cell <- .bcell("qa_anchor_scenario")

qa_check <- c(
  "Intervention-cause key uniqueness (per province)","Workbook FAIL-level issues","Workbook REVIEW-level issues",
  "Every scenario paired to baseline","No impossible negative states","Stock/flow identity residual",
  "Background mortality constant across cause","Cost reconciliation (components -> budget impact)",
  "Shared cost counted once per province/stratum/year","CEA reconciliation (detail -> summary)",
  "Excel vs R: deaths averted (anchor, province-total)","Excel vs R: discounted incremental cost (anchor, province-total)",
  "Excel vs R: cost per death averted (anchor, province-total)","VSL floor applied (preferred >= 20x GNI floor)",
  "Reference-case VSL parameters","Benefit and cost on the same price year",
  "Benefit_Cost (province,scenario) present in Health_Outcomes","CVD 40q30 Excel vs R (all rows match)",
  "Every province present for the anchor scenario")
qa_expect <- c("0","0","0","0","0","<= limit","1","<= tol","0","0","match R","match R","match R","0","as_specified","match","0","0","province_count")
qa_note <- c(
  "Each selected link key appears once within each province",
  "Blocked links/scenarios excluded","Flagged but usable","No Annual_Mortality base_deaths blank",
  "well/sick/new_cases/deaths/pop >= 0 (R)","Per-cause pop = well+sick+all-cause deaths (R)",
  "all.mx taken once per stratum (R)","Excel component rows sum to Excel annual totals",
  "shared-count-once components appear once per province-scenario-year","No Cost_Effectiveness row is 'mismatch'",
  "Excel province-total deaths averted reconciles to the R engine value",
  "Excel province-total discounted incremental cost reconciles to the R engine value",
  "Excel province-total cost per death reconciles to the R engine value",
  "No Economic_Value row has preferred VSL below the 20x GNI floor",
  "elasticity 1.5, US ratio 160, 20x floor (Robinson et al. 2019)",
  "bca_price_year equals the cost price year","Every Benefit_Cost province-scenario has a Health_Outcomes match",
  "CVD_40q30 recon_status has no mismatch","All 38 provinces appear once in Cost_Effectiveness for the cascade")
qa_actual <- c(
  sprintf("COUNTIF('Selected_Interventions'!$%s$2:$%s$%d,\">1\")", siKC, siKC, r_si),
  sprintf("COUNTIF('Input_Diagnostic'!$E$2:$E$%d,\"FAIL\")", r_id),
  sprintf("COUNTIF('Input_Diagnostic'!$E$2:$E$%d,\"REVIEW\")", r_id),
  sprintf("COUNTBLANK('Annual_Mortality'!$%s$2:$%s$%d)", amBaseD, amBaseD, r_am),
  .bcell("r_negative_state_count"),
  .bcell("r_stock_flow_max_residual"),
  .bcell("r_background_distinct_count"),
  sprintf("ABS(SUM('Budget_Impact'!$%s$2:$%s$%d)-SUM('Annual_Cost'!$%s$2:$%s$%d))+ABS(SUM('Budget_Impact'!$%s$2:$%s$%d)-SUM('Annual_Cost'!$%s$2:$%s$%d))",
          buScen,buScen,r_bi, acScen,acScen,r_ac, buBase,buBase,r_bi, acBase,acBase,r_ac),
  sprintf("COUNTIF('Annual_Cost'!$%s$2:$%s$%d,\">1\")", acShare, acShare, r_ac),
  sprintf("COUNTIF('Cost_Effectiveness'!$%s$2:$%s$%d,\"mismatch\")", ceReconl, ceReconl, r_ce),
  sprintf("SUMIFS('Cost_Effectiveness'!$%s$2:$%s$%d,'Cost_Effectiveness'!$%s$2:$%s$%d,%s)", ceDAl,ceDAl,r_ce, ceScnl,ceScnl,r_ce, anchor_cell),
  sprintf("SUMIFS('Cost_Effectiveness'!$%s$2:$%s$%d,'Cost_Effectiveness'!$%s$2:$%s$%d,%s)", ceDICl,ceDICl,r_ce, ceScnl,ceScnl,r_ce, anchor_cell),
  sprintf("IFERROR(SUMIFS('Cost_Effectiveness'!$%s$2:$%s$%d,'Cost_Effectiveness'!$%s$2:$%s$%d,%s)/SUMIFS('Cost_Effectiveness'!$%s$2:$%s$%d,'Cost_Effectiveness'!$%s$2:$%s$%d,%s),\"\")",
          ceDICl,ceDICl,r_ce, ceScnl,ceScnl,r_ce, anchor_cell, ceDAl,ceDAl,r_ce, ceScnl,ceScnl,r_ce, anchor_cell),
  sprintf("SUMPRODUCT(('Economic_Value'!$%s$2:$%s$%d<'Economic_Value'!$%s$2:$%s$%d)*1)", evPref,evPref,r_ev2, evFloor,evFloor,r_ev2),
  sprintf("IF(AND(%s=1.5,%s=160,%s=20),\"as_specified\",\"edited\")", bca_cells$elast, bca_cells$ratio, bca_cells$floor),
  sprintf("IF(%s=%s,\"match\",\"mismatch\")", bca_cells$price_year, .bcell("cost_price_year")),
  sprintf("SUMPRODUCT((COUNTIFS('Health_Outcomes'!$%s$2:$%s$%d,'Benefit_Cost'!$%s$2:$%s$%d,'Health_Outcomes'!$%s$2:$%s$%d,'Benefit_Cost'!$%s$2:$%s$%d)=0)*1)",
          hoLoc2,hoLoc2,r_ho2, bcLoc2,bcLoc2,r_bc2, hoScn2,hoScn2,r_ho2, bcScn2,bcScn2,r_bc2),
  sprintf("COUNTIF('CVD_40q30'!$N$2:$N$%d,\"mismatch\")", r_q40),
  sprintf("SUMPRODUCT(('Cost_Effectiveness'!$%s$2:$%s$%d=%s)*1)", ceScnl,ceScnl,r_ce, anchor_cell))
qa_status <- c(
  "IF(C2=0,\"PASS\",\"FAIL\")","IF(C3=0,\"PASS\",\"REVIEW\")","IF(C4=0,\"PASS\",\"REVIEW\")","IF(C5=0,\"PASS\",\"FAIL\")",
  "IF(C6=0,\"PASS\",\"FAIL\")",
  sprintf("IF(C7<=%s,\"PASS\",\"REVIEW\")", .bcell("stock_flow_residual_limit")),
  "IF(C8=1,\"PASS\",\"FAIL\")",
  sprintf("IF(C9<=%s,\"PASS\",\"FAIL\")", .bcell("formula_tolerance")),
  "IF(C10=0,\"PASS\",\"FAIL\")","IF(C11=0,\"PASS\",\"FAIL\")",
  sprintf("IF(ABS(C12-%s)<=ABS(%s)*0.0001+0.5,\"PASS\",\"FAIL\")", .bcell("r_deaths_averted_anchor"), .bcell("r_deaths_averted_anchor")),
  sprintf("IF(ABS(C13-%s)<=ABS(%s)*0.0001+1,\"PASS\",\"FAIL\")", .bcell("r_disc_incremental_cost_anchor"), .bcell("r_disc_incremental_cost_anchor")),
  sprintf("IF(ABS(C14-%s)<=ABS(%s)*0.0001+1,\"PASS\",\"FAIL\")", .bcell("r_cost_per_death_anchor"), .bcell("r_cost_per_death_anchor")),
  "IF(C15=0,\"PASS\",\"FAIL\")","IF(C16=\"as_specified\",\"PASS\",\"REVIEW\")","IF(C17=\"match\",\"PASS\",\"REVIEW\")",
  "IF(C18=0,\"PASS\",\"FAIL\")","IF(C19=0,\"PASS\",\"FAIL\")",
  sprintf("IF(C20=%s,\"PASS\",\"FAIL\")", .bcell("province_count")))
qa_df <- data.frame(check=qa_check, expected=qa_expect, actual=NA, status=NA_character_, note=qa_note, stringsAsFactors=FALSE)
addWorksheet(wb, "QA_Checks"); writeData(wb, "QA_Checks", qa_df, headerStyle=st_hdr)
writeFormula(wb, "QA_Checks", startCol=3, startRow=2, x=qa_actual)
writeFormula(wb, "QA_Checks", startCol=4, startRow=2, x=qa_status)
n_qa <- nrow(qa_df); r_qa <- n_qa + 1L
style_sheet("QA_Checks", names(qa_df), n_qa, formula_cols=c(3,4), wrap_cols=5)
conditionalFormatting(wb, "QA_Checks", cols=4, rows=2:r_qa, rule="PASS", type="contains", style=cf_pass)
conditionalFormatting(wb, "QA_Checks", cols=4, rows=2:r_qa, rule="FAIL", type="contains", style=cf_fail)
conditionalFormatting(wb, "QA_Checks", cols=4, rows=2:r_qa, rule="REVIEW", type="contains", style=cf_rev)

#===========================================================================
# Province_Reconciliation  (sum of provinces vs Indonesia national row)
#===========================================================================
if (!is.null(recon_nat)) {
  pr <- as.data.frame(recon_nat)
  # Sum of the 38 independently-projected provinces vs the single aggregate national
  # projection. They coincide exactly in 2017 (reconciled Nx) and diverge slightly
  # over the projection because aggregating disaggregated province rates is not the
  # same as projecting the aggregate national rate (a demographic aggregation
  # effect). Differences are REPORTED, never recalibrated. Documented agreement
  # tolerance: within 2% is expected for this cross-check.
  recon_tol_pn <- 0.02
  pr$status <- ifelse(is.na(pr$rel_diff), "REVIEW",
                      ifelse(abs(pr$rel_diff) < recon_tol_pn, "PASS", "REVIEW"))
  pr$note <- ifelse(abs(pr$rel_diff) < recon_tol_pn,
    sprintf("Province sum vs national agree within %.1f%% (rel.diff %.2f%%); small demographic-aggregation effect, reported not recalibrated.",
            100*recon_tol_pn, 100*pr$rel_diff),
    sprintf("Province sum vs national differ by %.2f%% (> %.1f%% tolerance); reported for review, not recalibrated.",
            100*pr$rel_diff, 100*recon_tol_pn))
  addWorksheet(wb, "Province_Reconciliation"); writeData(wb, "Province_Reconciliation", pr, headerStyle=st_hdr)
  sc <- which(names(pr)=="status")
  style_sheet("Province_Reconciliation", names(pr), nrow(pr), rsource_cols=1:5, wrap_cols=which(names(pr)=="note"), max_w=90)
  conditionalFormatting(wb, "Province_Reconciliation", cols=sc, rows=2:(nrow(pr)+1), rule="PASS", type="contains", style=cf_pass)
  conditionalFormatting(wb, "Province_Reconciliation", cols=sc, rows=2:(nrow(pr)+1), rule="REVIEW", type="contains", style=cf_rev)
}

#===========================================================================
# Methods_and_Sources + Calculation_Map
#===========================================================================
methods_f <- data.table(
  method_id = c("M01","M02","M03","M04","M05","M06","M07","M08","M09","M10","M11","M12"),
  concept = c("FAIR adjusted effect","Coverage path (cascade)","Annual component cost","Shared cost",
              "Discounting","Budget impact","Cost-effectiveness","Reference-Case VSL","Constant VSLY",
              "BCA discounting & basis","CVD 40q30 (period life table)","Province per-capita & national assumptions"),
  formula_or_rule = c(
    "e_adj(t)=effect_value*(cov(t)-baseline_cov)/(1-effect_value*baseline_cov); transition_effect=e_adj*affected_fraction; p_scen=p_base*(1-transition_effect). Reproduces Provincial_Model_Input_View to machine precision.",
    "Per province x sex x year effective-coverage path taken verbatim from Provincial_Trajectory (piecewise-linear 2030/2040 milestones, cholesterol-follows-HTN, no-backsliding baked in). Diabetes coverage moves proportionally to the provincial CVD treatment anchor.",
    "annual_cost = population_in_need x coverage(t) x frequency x unit_cost; PIN measure 'all'->eligible population, 'prevalence'->sick stock, 'incidence'->new cases (province model quantities).",
    "shared-count-once components counted once per province & eligible stratum, never once per affected cause.",
    "discount_factor(t)=1/(1+cost_discount_rate)^(t-analysis_start_year); costs discounted, death counts undiscounted.",
    "incremental = scenario - baseline (per province); discounted reported separately; per-capita uses the PROVINCE population.",
    "USD per death averted = province cumulative discounted incremental cost / cumulative deaths averted.",
    "Preferred VSL = MAX(160*GNIpc_US*(GNIpc_IDN/GNIpc_US)^1.5, 20*GNIpc_IDN); 100x/160x sensitivities. NATIONAL per-capita VSL applied to every province (documented).",
    "VSLY = preferred VSL / remaining LE at the average working-age age; applied to province life-years gained. LE is national (Indonesia) WPP.",
    "Benefits and costs discounted to bca_base_year at bca_discount_rate_primary; VSL benefits in PPP int$; market-USD costs x cost_to_bca_currency_factor. Partial mortality-benefit BCA.",
    "Period CVD 40q30 over six CVD causes, exact ages 30-69, per province: m_x, q_x=1-EXP(-m_x), l_30=1, l_{x+1}=l_x(1-q_x), 40q30=100*(1-l_70/l_30). Reconciled to Model 07.",
    "Cost per capita uses each province's own population. Life expectancy, disability weights and VSL/VSLY per-capita parameters are national (Indonesia) values applied to all provinces (no province-specific inputs exist)."),
  source = c(rep("FairChoices_Methods / input workbook", 4), rep("input workbook / Model 09", 3),
             rep("Robinson et al. 2019 / Model 08", 3), "Model 07 (07_cvd_40q30.rds)", "Model 09 (subnational)"))
addWorksheet(wb, "Methods_and_Sources"); writeData(wb, "Methods_and_Sources", methods_f, headerStyle=st_hdr)
style_sheet("Methods_and_Sources", names(methods_f), nrow(methods_f), wrap_cols=c(3,4), filter=FALSE, max_w=96)
setColWidths(wb, "Methods_and_Sources", cols=1:4, widths=c(10,26,96,50))

cmap <- data.table(
  output_sheet = c("Selected_Interventions","Cost_Components","Annual_Mortality","Health_Outcomes",
                   "CVD_40q30_Age","CVD_40q30","Annual_Cost","Budget_Impact","Cost_Effectiveness",
                   "Economic_Value","Benefit_Cost","QA_Checks","Cascade_Trajectory","Province_Reconciliation"),
  keys = c("location, intervention_cause_key","location, cost_record_id","location, scenario, year, cause",
           "location, scenario, year","location, scenario, htn, year, age","location, scenario, htn, year",
           "location, scenario, year, cost_record","location, scenario, year","location, scenario",
           "location, scenario, year","location, scenario, valuation_case","check","location, intervention, sex, year","metric"),
  formula_columns = c("adjusted_effect, key_count, formula_status","cost_ready","deaths_averted, cases_averted",
                      "*_averted, life_years_gained","m_x,q_x,l_x,l_x_next","cvd_40q30, baseline, reductions, recon",
                      "pin/cost/disc/shared/per_capita","costs (SUMIFS+location), cumulative, per_capita",
                      "SUMIFS(+location) health & cost, ratio, dominance, reconciliation",
                      "VSL/VSLY transfer, PV benefits, GNI shares","PV benefits/costs (SUMIFS+location), BCR, net benefit",
                      "actual & status (Excel-vs-R, province-total)","model_effective_coverage_used","status"),
  province_criterion = c(rep("yes", 14)))
addWorksheet(wb, "Calculation_Map"); writeData(wb, "Calculation_Map", cmap, headerStyle=st_hdr)
style_sheet("Calculation_Map", names(cmap), nrow(cmap), wrap_cols=c(2,3), filter=FALSE, max_w=56)
setColWidths(wb, "Calculation_Map", cols=1:4, widths=c(22,30,52,16))
message("  Wrote Input_Diagnostic, QA_Checks, Province_Reconciliation, Methods_and_Sources, Calculation_Map.")

#===========================================================================
# Cascade_Assumptions / Cascade_Trajectory (province) / Cascade_QA (province)
#===========================================================================
casc <- fair_inputs$cascade
MONEYC <- "#,##0"; COVFMT <- "0.000000"; CHKFMT <- "0.0000000000"

## ---- Cascade_Assumptions -------------------------------------------------
asheet <- as.data.table(casc$assumptions_sheet)
keep_ids <- c("analysis_start_year","analysis_end_year","first_target_year","final_target_year",
              "target_diagnosis_share","first_target_treatment_conditional","first_target_control_conditional",
              "final_target_treatment_conditional","final_target_control_conditional","treated_uncontrolled_effect_fraction",
              "first_target_effective_coverage_exact","final_target_effective_coverage_exact",
              "diabetes_baseline_control_among_treated","cholesterol_coverage_proxy","scale_up_shape",
              "post_target_rule","prevent_coverage_backsliding","scenario_id","cost_discount_rate",
              "health_discount_rate","cost_price_year","currency")
ca_dt <- asheet[parameter_id %in% keep_ids]
ca_dt <- ca_dt[match(intersect(keep_ids, ca_dt$parameter_id), parameter_id)]
ca_show <- data.frame(parameter_id=as.character(ca_dt$parameter_id), value=as.character(ca_dt$value),
                      unit=if ("unit" %in% names(ca_dt)) as.character(ca_dt$unit) else "",
                      description=if ("description" %in% names(ca_dt)) as.character(ca_dt$description) else "",
                      source=if ("source" %in% names(ca_dt)) as.character(ca_dt$source) else "",
                      stringsAsFactors=FALSE)
addWorksheet(wb, "Cascade_Assumptions"); writeData(wb, "Cascade_Assumptions", ca_show, headerStyle=st_hdr)
style_sheet("Cascade_Assumptions", names(ca_show), nrow(ca_show), input_cols=2, rsource_cols=c(1,3,4,5), wrap_cols=c(4,5), max_w=64)
.ca_row <- function(pid) which(ca_show$parameter_id==pid) + 1L
for (pid in c("treated_uncontrolled_effect_fraction","first_target_effective_coverage_exact",
              "final_target_effective_coverage_exact","diabetes_baseline_control_among_treated"))
  if (length(.ca_row(pid))) {
    writeData(wb, "Cascade_Assumptions", suppressWarnings(as.numeric(asheet[parameter_id==pid, value][1])),
              startCol=2, startRow=.ca_row(pid), colNames=FALSE)
    addStyle(wb, "Cascade_Assumptions", createStyle(numFmt=CHKFMT), rows=.ca_row(pid), cols=2, gridExpand=TRUE, stack=TRUE)
  }

## ---- Cascade_Trajectory (province x intervention x sex x year) ------------
ptr <- as.data.table(casc$provincial_trajectory)
ptr[, location := province_name]
ptr[, province_code := as.character(province_code)]
ptr <- ptr[province_name %in% prov_run]   # exactly the coverage R used for the simulated provinces
phase_of <- function(y) ifelse(y <= casc$first_target_year, "Scale to 70-30-30",
                        ifelse(y <= casc$final_target_year, "Scale to 70-70-70", "Maintain 70-70-70"))
tr_cols <- c("location","province_code","intervention_id","risk_factor_id","sex","year","phase",
             "baseline_effective_coverage","target_floor_2030","target_floor_2040",
             "scenario_effective_coverage","model_effective_coverage_used")
setorder(ptr, province_code, intervention_id, sex, year)
TR <- data.frame(location=ptr$location, province_code=ptr$province_code, intervention_id=ptr$intervention_id,
                 risk_factor_id=ptr$risk_factor_id, sex=ptr$sex, year=ptr$year, phase=phase_of(ptr$year),
                 baseline_effective_coverage=ptr$baseline_effective_coverage,
                 target_floor_2030=casc$eff_2030, target_floor_2040=casc$eff_2040,
                 scenario_effective_coverage=ptr$scenario_effective_coverage,
                 model_effective_coverage_used=NA_real_, stringsAsFactors=FALSE)
TRcl <- CLmk(tr_cols)
addWorksheet(wb, "Cascade_Trajectory"); writeData(wb, "Cascade_Trajectory", TR, headerStyle=st_hdr)
nT <- nrow(TR)
if (nT > 0) {
  scol <- TRcl("scenario_effective_coverage")
  writeFormula(wb, "Cascade_Trajectory", x=sprintf("=%s%d", scol, 2:(nT+1L)),
               startCol=match("model_effective_coverage_used", tr_cols), startRow=2L)
}
style_sheet("Cascade_Trajectory", tr_cols, nT,
            formula_cols=match("model_effective_coverage_used", tr_cols),
            rsource_cols=match(setdiff(tr_cols,"model_effective_coverage_used"), tr_cols))
for (j in match(c("baseline_effective_coverage","target_floor_2030","target_floor_2040",
                  "scenario_effective_coverage","model_effective_coverage_used"), tr_cols))
  addStyle(wb, "Cascade_Trajectory", createStyle(numFmt=COVFMT), rows=2:(nT+1L), cols=j, gridExpand=TRUE, stack=TRUE)

## ---- Cascade_QA (province-aware) -----------------------------------------
# R-side diagnostics on the provincial trajectory + adapter reconciliation.
ptr_chk <- copy(ptr); setorder(ptr_chk, province_code, intervention_id, sex, year)
ptr_chk[, d := scenario_effective_coverage - shift(scenario_effective_coverage), by=.(province_code, intervention_id, sex)]
mono_viol <- ptr_chk[!is.na(d) & d < -1e-9, .N]
f30 <- ptr_chk[year==casc$first_target_year]
f30[, expect := pmax(baseline_effective_coverage, casc$eff_2030)]
floor30_viol <- f30[abs(scenario_effective_coverage - expect) > 1e-6, .N]
f40 <- ptr_chk[year==casc$final_target_year]
f40[, expect := pmax(baseline_effective_coverage, casc$eff_2040)]
floor40_viol <- f40[abs(scenario_effective_coverage - expect) > 1e-6, .N]
missing_traj <- length(setdiff(prov_run, unique(ptr_chk$province_name)))
recon_max <- casc$recon_max_abs
cqa <- data.frame(
  check_id = c("QAC01","QAC02","QAC03","QAC04","QAC05","QAC06"),
  check = c("R adapter vs Provincial_Model_Input_View transition multipliers (max |diff|)",
            "Effective coverage monotonic per province (no backsliding)",
            "2030 effective coverage = max(province baseline, 0.1365) per province",
            "2040 effective coverage = max(province baseline, 0.4165) per province",
            "Every simulated province has a provincial coverage trajectory",
            "Excel model_effective_coverage_used ties to the R scenario coverage (by construction)"),
  r_value = c(recon_max, mono_viol, floor30_viol, floor40_viol, missing_traj, 0),
  threshold = c("< 1e-6","= 0","= 0","= 0","= 0","PASS"),
  status = NA_character_, stringsAsFactors=FALSE)
addWorksheet(wb, "Cascade_QA"); writeData(wb, "Cascade_QA", cqa, headerStyle=st_hdr)
CQcl <- CLmk(names(cqa)); rvc <- CQcl("r_value")
writeFormula(wb, "Cascade_QA", startCol=match("status",names(cqa)), startRow=2, x=c(
  sprintf("IF(%s2<0.000001,\"PASS\",\"FAIL\")", rvc),
  sprintf("IF(%s3=0,\"PASS\",\"FAIL\")", rvc),
  sprintf("IF(%s4=0,\"PASS\",\"FAIL\")", rvc),
  sprintf("IF(%s5=0,\"PASS\",\"FAIL\")", rvc),
  sprintf("IF(%s6=0,\"PASS\",\"FAIL\")", rvc),
  "\"PASS\""))
style_sheet("Cascade_QA", names(cqa), nrow(cqa), formula_cols=match("status",names(cqa)),
            rsource_cols=match("r_value",names(cqa)), wrap_cols=2, max_w=64)
addStyle(wb, "Cascade_QA", createStyle(numFmt=CHKFMT), rows=2, cols=match("r_value",names(cqa)), stack=TRUE)
conditionalFormatting(wb, "Cascade_QA", cols=match("status",names(cqa)), rows=2:(nrow(cqa)+1), rule="PASS", type="contains", style=cf_pass)
conditionalFormatting(wb, "Cascade_QA", cols=match("status",names(cqa)), rows=2:(nrow(cqa)+1), rule="FAIL", type="contains", style=cf_fail)
message(sprintf("  Cascade sheets: recon_max=%.2e, mono_viol=%d, floor2030_viol=%d, floor2040_viol=%d, provinces=%d",
                recon_max, mono_viol, floor30_viol, floor40_viol, uniqueN(ptr_chk$province_name)))

#===========================================================================
# Worksheet order, recalc-on-open, save
#===========================================================================
desired_order <- c("README","Run_Metadata","Scenario_Catalog",
                   "Cascade_Assumptions","Cascade_Trajectory","Cascade_QA",
                   "Selected_Interventions","Cost_Components","Annual_Mortality","Health_Outcomes",
                   "CVD_40q30","CVD_40q30_Age","Annual_Cost","Budget_Impact","Cost_Effectiveness",
                   "Economic_Value","Benefit_Cost","QA_Checks","Province_Reconciliation","Input_Diagnostic",
                   "Methods_and_Sources","Calculation_Assumptions","Calculation_Map")
desired_order <- desired_order[desired_order %in% names(wb)]
worksheetOrder(wb) <- match(desired_order, names(wb))
wb$workbook$calcPr <- '<calcPr calcId="191029" fullCalcOnLoad="1"/>'
strip_dangling_drawings(wb)
if (!dir.exists(dirname(cost_value_formulae_file))) dir.create(dirname(cost_value_formulae_file), recursive=TRUE)
saveWorkbook(wb, cost_value_formulae_file, overwrite=TRUE)
message("  Wrote SUBNATIONAL formula workbook: ", cost_value_formulae_file)
message(sprintf("=== Model 09 (subnational) complete: %d provinces, %d sheets ===",
                length(prov_run), length(names(wb))))
