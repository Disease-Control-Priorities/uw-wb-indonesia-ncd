#===========================================================================
# 04_define_interventions_70_30_30_to_70_70_70_subnational.R
#   SUBNATIONAL (province) cascade catalogue builder.
#---------------------------------------------------------------------------
# Adapted from `.build_cascade_catalogue()` in the production
#   code/cvd-fair-choices/04_define_interventions_indonesia.R
# (copied and made province-aware; the production file is NOT sourced). The FAIR
# incremental-effect algebra, the {baseline + single cascade} scenario contract,
# the effect-row schema consumed by Model 06's calculate_fair_workbook_impact(),
# and the fair_inputs shape consumed by Model 09 are all preserved verbatim.
#
# WHAT DIFFERS FROM THE NATIONAL CASCADE BUILDER
#   * Geography is now an explicit dimension. The per-year effective-coverage path
#     is taken VERBATIM from the workbook's `Provincial_Trajectory`
#     (per province x intervention x sex x year) instead of the national
#     `Coverage_Trajectory`, and the R-computed transition multiplier is
#     reconciled against `Provincial_Model_Input_View` (per province x link x sex
#     x year) -- a hard FAIL on any mismatch > 1e-6.
#   * Engine effect rows carry a `location` (= province_name) column; Model 06
#     filters them per province. Cost records are province-expanded, each with its
#     province's sex-averaged coverage path.
#   * The scenario catalogue is still exactly {baseline, S_70_30_30_TO_70_70_70};
#     the two component interventions live only inside the cascade scenario.
#
# FAIR Choices coverage-adjusted effect (verified to reproduce the workbook's
# Provincial_Model_Input_View$transition_multiplier to machine precision):
#   delta_cov(t)       = scenario_effective_coverage(t) - baseline_effective_coverage
#   adjusted_effect(t) = effect_value * delta_cov(t) / (1 - effect_value*baseline_coverage)
#   transition_effect  = adjusted_effect(t) * affected_fraction
#   p_scenario(t)      = p_baseline(t) * (1 - transition_effect(t))
#===========================================================================

suppressPackageStartupMessages({ library(data.table); library(readxl) })

.build_cascade_catalogue_subnational <- function(inputs_path,
                                                 cause_map,
                                                 b_rates,
                                                 strict              = FALSE,
                                                 baseline_id         = "baseline",
                                                 cascade_scenario_id = "S_70_30_30_TO_70_70_70",
                                                 cascade_family      = "cascade_70_30_30_to_70_70_70_subnational") {

  if (!file.exists(inputs_path))
    stop("Subnational cascade: input workbook not found: ", inputs_path)

  req_sheets <- c("Assumptions", "Intervention_Cause_Map", "Effect_Sizes",
                  "Cost_Components", "Provincial_Framework", "Provincial_Trajectory",
                  "Provincial_Model_Input_View", "QA_Checks")
  have <- readxl::excel_sheets(inputs_path)
  miss <- setdiff(req_sheets, have)
  if (length(miss))
    stop("Subnational cascade: workbook missing required sheet(s): ", paste(miss, collapse = ", "))

  rd   <- function(sheet) as.data.table(readxl::read_excel(inputs_path, sheet = sheet))
  numv <- function(x) suppressWarnings(as.numeric(x))
  chrv <- function(x) trimws(as.character(x))

  issues <- data.table(scope = character(), item_key = character(), field = character(),
                       problem = character(), severity = character())
  add_issue <- function(scope, item_key, field, problem, severity = "FAIL")
    issues <<- rbind(issues, data.table(scope = scope, item_key = as.character(item_key),
                                        field = field, problem = problem, severity = severity))

  ## -- Assumptions (parameter_id -> value) ------------------------------------
  asmp <- rd("Assumptions")
  A    <- setNames(as.character(asmp$value), as.character(asmp$parameter_id))
  getA <- function(id, default = NA) if (id %in% names(A)) A[[id]] else default

  analysis_start_year <- as.integer(numv(getA("analysis_start_year", 2025)))
  analysis_end_year   <- as.integer(numv(getA("analysis_end_year",   2050)))
  first_target_year   <- as.integer(numv(getA("first_target_year",   2030)))
  final_target_year   <- as.integer(numv(getA("final_target_year",   2040)))
  cost_discount_rate  <- numv(getA("cost_discount_rate",   0.03))
  health_discount_rate<- numv(getA("health_discount_rate", 0.03))
  cost_price_year     <- as.integer(numv(getA("cost_price_year", 2023)))
  eff_2030 <- numv(getA("first_target_effective_coverage_exact", NA_real_))
  eff_2040 <- numv(getA("final_target_effective_coverage_exact", NA_real_))
  partial_effect_fraction <- numv(getA("treated_uncontrolled_effect_fraction", 0.5))
  prevent_backsliding <- as.integer(numv(getA("prevent_coverage_backsliding", 1)))
  scen_id_wb  <- chrv(getA("scenario_id", cascade_scenario_id))
  if (!is.na(scen_id_wb) && nzchar(scen_id_wb) && !identical(scen_id_wb, cascade_scenario_id))
    add_issue("assumptions", "scenario_id", "scenario_id",
              sprintf("workbook scenario_id '%s' != expected '%s'", scen_id_wb, cascade_scenario_id),
              "REVIEW")
  if (is.na(eff_2030) || is.na(eff_2040))
    add_issue("assumptions", "effective_coverage", "value",
              "first/final_target_effective_coverage_exact missing or non-numeric", "FAIL")

  ## -- cause_id -> model cause short code --------------------------------------
  cause_id2code <- c(C_RHD = "rhd", C_IHD = "ihd", C_IS = "istroke",
                     C_ICH = "hstroke", C_HHD = "hhd", C_CMP = "cmd", C_T2D = "dm2")
  bad_codes <- setdiff(unname(cause_id2code), names(cause_map))
  if (length(bad_codes))
    stop("Subnational cascade: cause translation maps to code(s) absent from cause_map: ",
         paste(bad_codes, collapse = ", "))
  translate_transition <- function(from, to) {
    from <- tolower(chrv(from)); to <- tolower(chrv(to))
    out <- rep(NA_character_, length(from))
    out[from == "well" & to == "sick"] <- "incidence"
    out[from %in% c("sick", "sick_severe", "sick_hf") & grepl("^dead", to)] <- "case_fatality"
    out
  }
  # Workbook sex labels ("Men"/"Women") -> model rate-table labels ("Male"/"Female").
  sex_wb2model <- c(Men = "Male", Women = "Female",
                    Male = "Male", Female = "Female", Both = "Both")

  ## -- Read the contract sheets -----------------------------------------------
  map <- rd("Intervention_Cause_Map")
  eff <- rd("Effect_Sizes")
  cst <- rd("Cost_Components")
  pfw <- rd("Provincial_Framework")            # province_code, province_name, ...
  ptr <- rd("Provincial_Trajectory")           # province x intervention x sex x year
  pmiv<- rd("Provincial_Model_Input_View")     # province x link x sex x year (targets)

  #===========================================================================
  # Province set + crosswalk (workbook <-> reconciled b_rates), fail on mismatch
  #===========================================================================
  if (!all(c("province_code", "province_name") %in% names(pfw)))
    stop("Subnational cascade: Provincial_Framework must carry province_code + province_name.")
  cw <- unique(pfw[, .(province_code = chrv(province_code), province_name = chrv(province_name))])
  cw[, location := province_name]   # crosswalk is an EXACT identity on names (validated below)
  if (anyDuplicated(cw$province_code) || anyDuplicated(cw$province_name))
    stop("Subnational cascade: duplicated province_code/province_name in Provincial_Framework.")

  wb_provs    <- sort(unique(cw$location))
  rates_provs <- sort(setdiff(unique(as.character(b_rates$location)), "Indonesia"))
  miss_in_rates <- setdiff(wb_provs, rates_provs)   # in workbook, absent from rates
  extra_in_rates<- setdiff(rates_provs, wb_provs)   # in rates, absent from workbook
  cat("\n--- Province set reconciliation (workbook <-> b_rates) ---------------\n")
  cat(sprintf("Workbook provinces: %d | b_rates provinces: %d\n", length(wb_provs), length(rates_provs)))
  if (length(miss_in_rates)) cat("  In workbook but NOT in b_rates: ", paste(miss_in_rates, collapse = ", "), "\n")
  if (length(extra_in_rates)) cat("  In b_rates but NOT in workbook: ", paste(extra_in_rates, collapse = ", "), "\n")
  if (length(miss_in_rates) || length(extra_in_rates))
    stop("Subnational cascade: province sets do not reconcile after the explicit crosswalk. ",
         "Resolve the mismatch above (no fuzzy matching, no row-order joins).", call. = FALSE)
  cat(sprintf("Province set EXACT equality: TRUE (%d provinces)\n", length(wb_provs)))
  province_locations <- wb_provs

  ## -- Effect sizes (national clinical estimates; same for every province) -----
  eff_k <- eff[, .(intervention_cause_key,
                   effect_value      = numv(effect_value),
                   affected_fraction = numv(affected_fraction),
                   e_age_start       = numv(age_start),
                   e_age_stop        = numv(age_stop),
                   e_sex             = chrv(sex),
                   effect_review     = chrv(review_status))]
  eff_n <- eff_k[, .(n_eff = .N), by = intervention_cause_key]

  ## -- Included links (include_flag == 1) -------------------------------------
  norm_flag <- function(x) {
    raw <- trimws(as.character(x)); num <- suppressWarnings(as.numeric(raw))
    if (any(is.na(num) | !(num %in% c(0, 1))))
      stop("Intervention_Cause_Map: include_flag must be exactly 0 or 1 for every row.")
    as.integer(round(num))
  }
  map[, include_flag := norm_flag(include_flag)]
  map_sel <- map[include_flag == 1L]
  if (!nrow(map_sel)) stop("Subnational cascade: no links with include_flag == 1.")

  ## -- Provincial trajectory (per province x intervention x sex x year) --------
  ptr_k <- ptr[, .(province_code = chrv(province_code),
                   province_name = chrv(province_name),
                   intervention_id = chrv(intervention_id),
                   risk_factor_id  = chrv(risk_factor_id),
                   sex             = chrv(sex),
                   year            = as.integer(year),
                   baseline_effective_coverage = numv(baseline_effective_coverage),
                   scenario_effective_coverage = numv(scenario_effective_coverage))]
  # Monotonicity / range guard (mirrors workbook QA; no-backsliding rule).
  setorder(ptr_k, province_code, intervention_id, sex, year)
  ptr_k[, d := scenario_effective_coverage - shift(scenario_effective_coverage),
        by = .(province_code, intervention_id, sex)]
  if (nrow(ptr_k[!is.na(d) & d < -1e-9]))
    add_issue("coverage", "Provincial_Trajectory", "scenario_effective_coverage",
              "coverage decreases in some province/year (violates no-backsliding)", "FAIL")
  if (nrow(ptr_k[scenario_effective_coverage < -1e-9 | scenario_effective_coverage > 1 + 1e-9]))
    add_issue("coverage", "Provincial_Trajectory", "scenario_effective_coverage",
              "coverage outside [0,1]", "FAIL")
  ptr_k[, d := NULL]

  ## -- Assemble link table + per-link validation (national contract) ----------
  L <- merge(map_sel[, .(intervention_cause_key = chrv(intervention_cause_key),
                         intervention_id = chrv(intervention_id),
                         intervention_name = chrv(intervention_name),
                         cause_id = chrv(cause_id), model_name = chrv(model_name),
                         cost_join_key = chrv(cost_join_key), cost_scope = chrv(cost_scope),
                         transition_from = chrv(transition_from), transition_to = chrv(transition_to))],
             eff_k, by = "intervention_cause_key", all.x = TRUE)
  L <- merge(L, eff_n, by = "intervention_cause_key", all.x = TRUE)
  L[is.na(n_eff), n_eff := 0L]
  L[, model_transition := translate_transition(transition_from, transition_to)]
  L[, cause_code       := cause_id2code[cause_id]]
  ints_with_traj <- unique(ptr_k$intervention_id)
  L[, has_traj := intervention_id %in% ints_with_traj]

  in01 <- function(x) !is.na(x) & x >= 0 & x <= 1
  L[, problem := ""]
  padd <- function(cond, msg) { cond[is.na(cond)] <- FALSE
    L[cond, problem := paste0(problem, ifelse(nchar(problem) > 0L, "; ", ""), msg)] }
  padd(L$n_eff != 1L,               "effect match != 1")
  padd(!in01(L$effect_value),       "effect_value missing/out of [0,1]")
  padd(!in01(L$affected_fraction),  "affected_fraction missing/out of [0,1]")
  padd(is.na(L$model_transition),   "transition label not mapped to model")
  padd(is.na(L$cause_code),         "cause_id absent from cause_map")
  padd(!L$has_traj,                 "no coverage trajectory for intervention_id")
  L[, valid := problem == ""]
  for (i in which(!L$valid))
    add_issue("health_link", L$intervention_cause_key[i], "effect/coverage", L$problem[i], "FAIL")

  valid_links   <- L[valid == TRUE]
  runnable_ints <- unique(valid_links$intervention_id)
  blocked_ints  <- setdiff(unique(map_sel$intervention_id), runnable_ints)

  #===========================================================================
  # Province-keyed engine effect rows (one per province x valid link x sex).
  # Each carries the province-specific baseline effective coverage (FAIR anchor)
  # and the exact per-year `coverage_path` from Provincial_Trajectory.
  #===========================================================================
  build_engine_rows <- function(links, provinces) {
    if (!nrow(links)) return(NULL)
    out <- vector("list", 0L)
    for (pn in provinces) {
      pcode <- cw[location == pn, province_code][1]
      for (i in seq_len(nrow(links))) {
        lk <- links[i]
        for (sx_wb in c("Men", "Women")) {
          cp <- ptr_k[province_name == pn & intervention_id == lk$intervention_id & sex == sx_wb,
                      .(year, coverage_t = scenario_effective_coverage)][order(year)]
          if (!nrow(cp)) {
            add_issue("health_link", paste0(pn, "/", lk$intervention_cause_key), "coverage_path",
                      sprintf("no Provincial_Trajectory rows for %s/%s/%s", pn, lk$intervention_id, sx_wb), "FAIL")
            next
          }
          base_cov <- ptr_k[province_name == pn & intervention_id == lk$intervention_id & sex == sx_wb,
                            baseline_effective_coverage][1]
          out[[length(out) + 1L]] <- data.table(
            location               = pn,
            province_code          = pcode,
            intervention_id        = lk$intervention_id,
            intervention_cause_key = lk$intervention_cause_key,
            cause_code             = lk$cause_code,
            model_transition       = lk$model_transition,
            effect_value           = lk$effect_value,
            affected_fraction      = lk$affected_fraction,
            baseline_coverage      = base_cov,
            target_coverage        = eff_2040,
            start_year             = analysis_start_year,
            target_year            = final_target_year,
            age_start              = lk$e_age_start,
            age_stop               = lk$e_age_stop,
            sex                    = unname(sex_wb2model[[sx_wb]]),
            coverage_path          = list(cp))
        }
      }
    }
    if (length(out)) rbindlist(out) else NULL
  }
  engine_rows <- build_engine_rows(valid_links, province_locations)

  #===========================================================================
  # Reconciliation: the ENGINE-applied path (Provincial_Trajectory + national
  # effect sizes) must reproduce Provincial_Model_Input_View$transition_multiplier
  # for every (province x link x sex x year). Hard FAIL on any |diff| > 1e-6.
  #===========================================================================
  # Expand each engine row's coverage_path into (loc, link, sex, year) and apply
  # the FAIR formula with the row's baseline_coverage + link effect_value/af.
  apply_coverage_adjustment <- function(effect_size, coverage_t, coverage_0) {
    if (is.na(coverage_0) || coverage_0 == 0) effect_size * coverage_t
    else effect_size * (coverage_t - coverage_0) / (1 - effect_size * coverage_0)
  }
  recon_rows <- vector("list", 0L)
  if (!is.null(engine_rows)) for (i in seq_len(nrow(engine_rows))) {
    r  <- engine_rows[i]
    cp <- as.data.table(r$coverage_path[[1]])
    e  <- apply_coverage_adjustment(r$effect_value, cp$coverage_t, r$baseline_coverage)
    tm <- 1 - r$affected_fraction * e
    recon_rows[[length(recon_rows) + 1L]] <- data.table(
      location = r$location, intervention_cause_key = r$intervention_cause_key,
      sex = r$sex, year = cp$year, tm_engine = tm)
  }
  recon_engine <- if (length(recon_rows)) rbindlist(recon_rows) else
    data.table(location = character(), intervention_cause_key = character(),
               sex = character(), year = integer(), tm_engine = numeric())

  pmiv_k <- pmiv[, .(location = chrv(province_name),
                     intervention_cause_key = chrv(intervention_cause_key),
                     sex_wb = chrv(sex), year = as.integer(year),
                     tm_wb = numv(transition_multiplier),
                     base_wb = numv(baseline_effective_coverage),
                     cov_wb  = numv(scenario_effective_coverage),
                     ev_wb   = numv(effect_value), af_wb = numv(affected_fraction))]
  pmiv_k[, sex := unname(sex_wb2model[sex_wb])]

  recon <- merge(recon_engine, pmiv_k,
                 by = c("location", "intervention_cause_key", "sex", "year"), all = TRUE)
  n_unmatched <- recon[is.na(tm_engine) | is.na(tm_wb), .N]
  if (n_unmatched > 0L)
    add_issue("reconciliation", "Provincial_Model_Input_View", "coverage_key",
              sprintf("%d (province,link,sex,year) key(s) present on only one side", n_unmatched), "FAIL")
  recon[, dabs := abs(tm_engine - tm_wb)]
  max_recon <- suppressWarnings(max(recon$dabs, na.rm = TRUE))
  if (is.finite(max_recon) && max_recon > 1e-6)
    add_issue("reconciliation", "Provincial_Model_Input_View", "transition_multiplier",
              sprintf("engine multiplier differs from workbook by up to %.3e", max_recon), "FAIL")

  ## -- Cost components (province-expanded; cascade coverage path) --------------
  sel_int <- unique(map_sel$intervention_id)
  C <- cst[, .(cost_record_id, cost_component_key, cost_option,
               selected_for_base_case      = as.integer(numv(selected_for_base_case)),
               intervention_id, cause_id, cost_join_key, cost_component,
               population_in_need_measure  = tolower(chrv(population_in_need_measure)),
               population_in_need_fraction = numv(population_in_need_fraction),
               frequency_per_year          = numv(frequency_per_year),
               c_age_start = numv(age_start), c_age_stop = numv(age_stop),
               c_sex = chrv(sex),
               unit_cost_usd = numv(unit_cost_usd),
               price_year    = as.integer(numv(price_year)),
               indonesia_adjusted_flag = as.integer(numv(indonesia_adjusted_flag)),
               cost_review   = chrv(review_status))]
  C <- C[intervention_id %in% sel_int]
  C[, n_sel := sum(selected_for_base_case == 1L, na.rm = TRUE), by = cost_component_key]
  sel_counts <- unique(C[, .(cost_component_key, n_sel)])
  for (i in seq_len(nrow(sel_counts))) {
    if (isTRUE(sel_counts$n_sel[i] > 1L))
      add_issue("cost", sel_counts$cost_component_key[i], "selected_for_base_case",
                "more than one base-case option selected", "FAIL")
    if (isTRUE(sel_counts$n_sel[i] == 0L))
      add_issue("cost", sel_counts$cost_component_key[i], "selected_for_base_case",
                "no base-case cost option selected (component omitted from costing)", "REVIEW")
  }
  Cbase0 <- C[selected_for_base_case == 1L]
  valid_cjk <- unique(chrv(map$cost_join_key))
  Cbase0[, cause_code := cause_id2code[cause_id]]
  padc <- function(dt, cond, field, msg, severity) {
    cond[is.na(cond)] <- FALSE
    if (any(cond)) for (i in which(cond)) add_issue("cost", dt$cost_record_id[i], field, msg, severity)
  }
  padc(Cbase0, is.na(Cbase0$unit_cost_usd) | Cbase0$unit_cost_usd < 0, "unit_cost_usd",
       "missing or negative unit cost on a selected base-case row", "FAIL")
  padc(Cbase0, is.na(Cbase0$frequency_per_year) | Cbase0$frequency_per_year < 0, "frequency_per_year",
       "missing or negative frequency", "FAIL")
  padc(Cbase0, is.na(Cbase0$population_in_need_fraction) |
         Cbase0$population_in_need_fraction < 0 | Cbase0$population_in_need_fraction > 1,
       "population_in_need_fraction", "PIN fraction missing/out of [0,1]", "FAIL")
  padc(Cbase0, !(chrv(Cbase0$cost_join_key) %in% valid_cjk), "cost_join_key",
       "cost_join_key not present in Intervention_Cause_Map", "FAIL")
  padc(Cbase0, !(Cbase0$population_in_need_measure %in% c("all", "prevalence", "incidence")),
       "population_in_need_measure", "unsupported PIN measure", "FAIL")
  padc(Cbase0, Cbase0$indonesia_adjusted_flag == 0L, "indonesia_adjusted_flag",
       "cost not Indonesia-adjusted (flagged; not treated as ready)", "REVIEW")
  scope_by_cjk <- unique(map[, .(cost_join_key = chrv(cost_join_key), cost_scope = chrv(cost_scope))])
  scope_by_cjk <- scope_by_cjk[, .(cost_scope = cost_scope[1]), by = cost_join_key]
  Cbase0[, cost_join_key := chrv(cost_join_key)]
  Cbase0 <- merge(Cbase0, scope_by_cjk, by = "cost_join_key", all.x = TRUE)

  # Province x intervention sex-averaged coverage path (cost records are sex="Both").
  ptr_avg <- ptr_k[, .(coverage_t = mean(scenario_effective_coverage),
                       base_avg   = mean(baseline_effective_coverage)),
                   by = .(province_name, intervention_id, year)][order(province_name, intervention_id, year)]
  cov_int_u <- ptr_avg[, .(cov_baseline = base_avg[which.min(year)],
                           cov_target   = eff_2040,
                           cov_start_year = analysis_start_year,
                           cov_target_year = final_target_year),
                       by = .(province_name, intervention_id)]

  # Province-expand the base-case cost catalogue.
  Cbase <- Cbase0[, as.list(.SD)][rep(seq_len(nrow(Cbase0)), each = length(province_locations))]
  Cbase[, location := rep(province_locations, times = nrow(Cbase0))]
  Cbase <- merge(Cbase, cw[, .(location, province_code)], by = "location", all.x = TRUE)
  Cbase <- merge(Cbase, cov_int_u, by.x = c("location", "intervention_id"),
                 by.y = c("province_name", "intervention_id"), all.x = TRUE)
  padc(Cbase, is.na(Cbase$cov_baseline), "coverage",
       "no cascade coverage trajectory found for province cost record", "FAIL")
  cost_cov_path <- lapply(seq_len(nrow(Cbase)), function(j)
    ptr_avg[province_name == Cbase$location[j] & intervention_id == Cbase$intervention_id[j],
            .(year, coverage_t)][order(year)])
  Cbase[, coverage_path := cost_cov_path]

  ## -- Scenario catalogue (baseline + the single cascade scenario) ------------
  scen <- list()
  scen[[baseline_id]] <- list(scenario_id = baseline_id,
                              scenario_label = "Baseline (no new intervention)",
                              intervention_ids = character(0),
                              interventions = character(0),
                              fair_effect_rows = NULL,
                              family = "baseline")
  scen[[cascade_scenario_id]] <- list(
    scenario_id      = cascade_scenario_id,
    scenario_label   = "70-30-30 -> 70-70-70 hypertension/cholesterol + diabetes cascade (subnational)",
    intervention_ids = runnable_ints,
    interventions    = "fair_wb",
    fair_effect_rows = engine_rows,
    family           = cascade_family,
    scenario_role    = "combined",
    scenario_level   = "combined",
    parent_package_id = NA_character_,
    component_intervention_ids = runnable_ints)

  ## -- Assemble fair_inputs (consumed by Model 09) ----------------------------
  fair_inputs <- list(
    links          = L,
    valid_links    = valid_links,
    blocked_links  = L[valid == FALSE],
    costs          = Cbase,
    cost_all       = C,
    validation     = issues,
    cause_translation = data.table(cause_id = names(cause_id2code), cause_code = unname(cause_id2code)),
    runnable_interventions = runnable_ints,
    blocked_interventions  = blocked_ints,
    inputs_path    = inputs_path,
    baseline_scenario_id = baseline_id,
    province_crosswalk = cw,
    province_locations = province_locations,
    # Cascade-specific bundle for Model 09's Cascade_* sheets (province-aware).
    cascade = list(
      scenario_id = cascade_scenario_id, family = cascade_family, subnational = TRUE,
      analysis_start_year = analysis_start_year, analysis_end_year = analysis_end_year,
      first_target_year = first_target_year, final_target_year = final_target_year,
      eff_2030 = eff_2030, eff_2040 = eff_2040,
      partial_effect_fraction = partial_effect_fraction,
      prevent_coverage_backsliding = prevent_backsliding,
      provincial_framework = pfw,
      provincial_trajectory = ptr_k, provincial_model_input_view = pmiv,
      qa_checks = rd("QA_Checks"), assumptions_sheet = asmp,
      recon = recon, recon_max_abs = max_recon, province_crosswalk = cw),
    assumptions    = list(
      analysis_start_year     = analysis_start_year,
      analysis_end_year       = analysis_end_year,
      intervention_start_year = analysis_start_year,
      coverage_target_year    = final_target_year,
      target_coverage_default = eff_2040,
      cost_discount_rate      = cost_discount_rate,
      health_discount_rate    = health_discount_rate,
      cost_price_year         = cost_price_year,
      currency                = getA("currency", "USD"),
      scale_up_shape          = getA("scale_up_shape", "piecewise_linear"),
      downstream_cost_offsets = as.integer(numv(getA("downstream_cost_offsets", 0))),
      economic_perspective    = getA("economic_perspective", "societal")))

  ## -- Report + scope assertion ------------------------------------------------
  n_fail <- sum(issues$severity == "FAIL"); n_rev <- sum(issues$severity == "REVIEW")
  cat("\n--- 70-30-30 -> 70-70-70 SUBNATIONAL cascade catalogue --------------\n")
  cat(sprintf("Workbook: %s\n", inputs_path))
  cat(sprintf("Provinces: %d | Included links: %d | valid: %d | invalid: %d\n",
              length(province_locations), nrow(map_sel), nrow(valid_links), nrow(L[valid == FALSE])))
  cat(sprintf("Component interventions (%d): %s\n", length(runnable_ints), paste(runnable_ints, collapse = ", ")))
  cat(sprintf("Engine effect rows (province x link x sex): %d\n",
              if (is.null(engine_rows)) 0L else nrow(engine_rows)))
  cat(sprintf("Effective coverage milestones: 2030 = %.10g | 2040 = %.10g\n", eff_2030, eff_2040))
  cat(sprintf("Provincial_Model_Input_View reconciliation max |diff|: %.3e\n", max_recon))
  cat(sprintf("Province-expanded base-case cost rows: %d\n", nrow(Cbase)))
  cat(sprintf("Validation issues: %d FAIL, %d REVIEW\n", n_fail, n_rev))
  if (nrow(issues)) { cat("Consolidated validation diagnostic:\n"); print(issues) }
  cat(sprintf("Scenarios built (%d): %s\n", length(scen), paste(names(scen), collapse = ", ")))
  cat("---------------------------------------------------------------------\n\n")

  if (!setequal(names(scen), c(baseline_id, cascade_scenario_id)))
    stop("Subnational cascade: scenario catalogue must be exactly {", baseline_id, ", ",
         cascade_scenario_id, "}; got {", paste(names(scen), collapse = ", "), "}.", call. = FALSE)
  if (strict && n_fail > 0L)
    stop("Subnational cascade: strict_model_input_validation = TRUE and ", n_fail,
         " FAIL-level workbook issue(s) present.", call. = FALSE)
  if (n_fail > 0L)
    stop("Subnational cascade: ", n_fail, " FAIL-level issue(s) in the cascade workbook (see ",
         "diagnostic above); the cascade run requires a clean catalogue.", call. = FALSE)

  list(scenarios = scen, inputs = fair_inputs)
}

# --- Build the subnational cascade catalogue (opt-in guard) ------------------
if (!isTRUE(get0("run_cascade_70_30_30_to_70_70_70", ifnotfound = FALSE)) ||
    !isTRUE(get0("run_subnational", ifnotfound = FALSE)))
  stop("04 (subnational): this builder requires run_cascade_70_30_30_to_70_70_70 == TRUE and ",
       "run_subnational == TRUE (set by the subnational 00 runner).", call. = FALSE)

.built <- .build_cascade_catalogue_subnational(
  inputs_path         = model_inputs_file,
  cause_map           = cause_map,
  b_rates             = b_rates,
  strict              = strict_model_input_validation,
  baseline_id         = baseline_scenario_id,
  cascade_scenario_id = if (exists("cascade_scenario_id")) cascade_scenario_id else "S_70_30_30_TO_70_70_70",
  cascade_family      = if (exists("cascade_family")) cascade_family else "cascade_70_30_30_to_70_70_70_subnational")
fair_scenarios <- .built$scenarios
fair_inputs    <- .built$inputs
public_health_scenarios <- NULL
combined_scenarios      <- NULL
cascade_provinces  <- fair_inputs$province_locations
rm(.built)
