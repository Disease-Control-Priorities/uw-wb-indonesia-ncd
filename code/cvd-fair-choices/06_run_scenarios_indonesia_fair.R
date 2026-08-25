# #############################################################################
# # 06_run_scenarios_indonesia_fair.R
# #
# # DROP-IN ALTERNATIVE to 06_run_scenarios_indonesia.R that ADDS a Disease
# # Control Priorities (DCP) / FAIR-Choices cardiovascular-disease intervention
# # PACKAGE as a new intervention ("fair_cvd"). It is a self-contained copy of
# # 06_run_scenarios_indonesia.R; the original file is left untouched. To use it,
# # simply source THIS file instead of 06_run_scenarios_indonesia.R from
# # 00_run_model_indonesia.R. It depends on the same upstream objects created by
# # earlier sourced scripts (data.in, b_rates, inc, dt_gbd_rr, repYear, the path
# # variables wd / wd_raw / wd_data / wd_outp, etc.) and reuses the existing
# # helpers (calculate_coverage_by_year(), apply_coverage_adjustment(), ...).
# #
# # ---------------------------------------------------------------------------
# # FAIR-Choices health-impact identity
# # ---------------------------------------------------------------------------
# # FAIR-Choices computes health impact as:
# #
# #     Health_Impact = Population x Disease_Rate x Coverage x Efficacy x Time
# #
# # i.e. an intervention's effect size acts MULTIPLICATIVELY on the relevant
# # rate (mortality / case-fatality, incidence, or prevalence) for the affected
# # proportion of the population in the stated age/sex band, scaled by coverage
# # and phased over the time horizon. (Methods:
# #   https://fairchoices.w.uib.no/documentation/fairchoices-methods/#summary )
# #
# # FAIR-Choices applies coverage with the standard "adjusted effect" formula
# #
# #     e_adj = e_crude * (cov_target - cov_baseline)
# #             ---------------------------------------
# #               1 - ( e_crude * cov_baseline )
# #
# # and then    rate_adjusted = rate_background + rate_cause * (1 - e_adj).
# #
# # This is EXACTLY the algebra already implemented in this code base by
# # apply_coverage_adjustment() (which reduces to e_crude * cov_target when the
# # baseline coverage is 0). We therefore reuse it rather than re-deriving it.
# #
# # ---------------------------------------------------------------------------
# # Source table for every effect size
# # ---------------------------------------------------------------------------
# #   data/raw/FAIR-Choices/taxonomy_effect_size.xlsx  (single sheet "Sheet 1")
# #   loaded via read_excel(paste0(wd_raw, "FAIR-Choices/taxonomy_effect_size.xlsx"))
# #   filtered to  Sub-group == "Cardiovascular diseases".
# # No effect size is invented in code: every value traces to a row of that table.
# #
# # ---------------------------------------------------------------------------
# # Reduction vs residual-RR interpretation  (DECISION)
# # ---------------------------------------------------------------------------
# # The FAIR-Choices methods store the Mortality / Incidence / Prevalence values
# # as PROPORTIONAL REDUCTIONS (effectiveness e in [0,1], e.g. 0.54 = a 54%
# # reduction), NOT as residual relative risks. This is confirmed by the methods
# # page, whose adjusted-effect and rate equations use (1 - e_adj) and an e that
# # enters the e_crude*(cov_target-cov_baseline)/(1 - e_crude*cov_baseline)
# # transform (a residual-RR convention would instead use (1 - value) up front).
# # Accordingly we use:
# #
# #     effect_size = Affected_Proportion x value x Coverage(year)
# #     rate_new    = rate * (1 - effect_size)
# #
# # We DO NOT additionally compute (1 - value): the tabulated value already IS
# # the reduction, so transforming it twice would be wrong.
# #
# # ---------------------------------------------------------------------------
# # Disease / condition mapping (table "Affected condition" -> model cause)
# # ---------------------------------------------------------------------------
# #   Ischemic heart disease          -> ihd
# #   Ischemic stroke                 -> istroke
# #   Intracerebral hemorrhage        -> hstroke
# #   Hypertensive heart disease      -> hhd
# #   Cardiomyopathy and myocarditis  -> cmd
# #   Rheumatic heart disease         -> rhd
# # Conditions with no model cause (Lower extremity peripheral arterial disease,
# # SRHD, ...) are DROPPED with a message. Mapped causes that are absent from
# # intervention_rates at run time (e.g. when rhd/cmd are not carried) are also
# # skipped gracefully with a message.
# #
# # Column -> rate mapping (per task spec):
# #   Mortality  column -> case-fatality (CF) effect   (eff_cf, CF)
# #   Incidence  column -> incidence     (IR) effect   (eff_ir, IR)
# # NOTE/ASSUMPTION: in FAIR-Choices the "Mortality" effect reduces the
# # cause-specific MORTALITY RATE. In this 4-state (well/sick/dead) model the
# # cause-specific death rate is sick*CF, so a mortality reduction is represented
# # as a CF reduction - this is the faithful analogue in this model and is what
# # the task specifies. The Disability, Prevalence, Fertility and Stillbirths
# # columns have no corresponding transition rate in this CVD micro-simulation
# # and are therefore NOT applied (documented, not silently ignored): a bundle
# # whose only non-missing effect is Prevalence/Disability contributes no CF/IR
# # change.
# #
# # ---------------------------------------------------------------------------
# # DEFAULT PARAMETERS  (ALL overridable with local Indonesian program data)
# # ---------------------------------------------------------------------------
# # Every parameter below is a DEFAULT sourced from the taxonomy table or from a
# # transparent coverage assumption; each can be edited or replaced by passing a
# # modified parameter object / different arguments:
# #   * Effect size (efficacy)   <- taxonomy "Mortality"/"Incidence" column
# #   * Affected proportion      <- taxonomy "Affected Proportion" column
# #   * Age band / target sex    <- taxonomy "Age start"/"Age stop"/"Affected gender"
# #   * Package membership       <- fair_bundles (default = full CVD primary +
# #                                  secondary + acute package, all mapped bundles)
# #   * Coverage scale-up        <- fair_start_year (2026), fair_target_year (2050),
# #                                  fair_target_coverage (0.80), fair_baseline_coverage (0)
# #   * Per-bundle coverage       <- fair_bundle_coverage (named overrides; default NULL)
# # The parameter table itself is exposed as the global `dt_fair_params`
# # (built by load_fair_cvd_params()) and can be inspected, edited, or rebuilt
# # from local evidence and passed in via the `fair_params` argument.
# # #############################################################################

# #...........................................................
# # Interventions and Targets ----
# #...........................................................

#...........................................................
## GBD Relative Risks Setup ----
#...........................................................

# Load and prepare GBD relative risks (same as OLD model)
dt_gbd_rr <- as.data.table(read_excel(paste0(wd_raw, "IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15_HTN.xlsx"),
                                      sheet = "Sheet1", range = "A3:AB20"))

dt_gbd_rr[, c("Category / Units", "Morbidity / Mortality", "Sex", "All-age") := NULL]
dt_gbd_rr[, `20-24 years` := `25-29 years`]
dt_gbd_rr[, (2:8) := NULL]

dt_gbd_rr <- melt(dt_gbd_rr,
                  id.vars = c("Risk-Outcome"),
                  variable.name = "age",
                  value.name = "rr_per_10mmhg")

dt_gbd_rr[, age := gsub(" years", "", age)]
dt_gbd_rr[, rr_per_10mmhg := as.numeric(sub("^\\s*([0-9.]+).*", "\\1", rr_per_10mmhg))]

dt_gbd_rr[, cause := fcase(
  `Risk-Outcome` == "Ischaemic heart disease", "ihd",
  `Risk-Outcome` == "Ischaemic stroke", "istroke",
  `Risk-Outcome` == "Intracerebral hemorrhage", "hstroke",
  `Risk-Outcome` == "Hypertensive heart disease", "hhd",
  # `Risk-Outcome` == "Rheumatic heart disease", "rhd",
  # `Risk-Outcome` == "Other cardiomyopathy", "cmd", # ??Cardiomyopathy and myocarditis not a category in RR, so using this
  default = NA_character_
)]
dt_gbd_rr[, `Risk-Outcome` := NULL]
#dt_gbd_rr <- dt_gbd_rr[cause %in% c("ihd", "istroke", "hstroke", "hhd","rhd","cmd")]
dt_gbd_rr <- dt_gbd_rr[cause %in% c("ihd", "istroke", "hstroke", "hhd")]

# Expand age groups to single years
expand_age <- function(age_group) {
  if (grepl("\\+", age_group)) {
    start <- as.numeric(sub("\\+", "", age_group))
    return(start:95)
  } else {
    bounds <- as.numeric(unlist(strsplit(age_group, "-")))
    return(bounds[1]:bounds[2])
  }
}

dt_expanded <- dt_gbd_rr[, .(age_single = expand_age(age)), by = .(age, rr_per_10mmhg, cause)]
dt_expanded[, age_single := as.integer(age_single)]
dt_expanded[, age := age_single]
dt_expanded[, age_single := NULL]
dt_gbd_rr <- copy(dt_expanded)

#...........................................................
## Helpers - Reusable components ----
#...........................................................

# # ETIHAD relative risks for 10 mmHg BP reduction
# ETIHAD_RR <- data.table(
#   cause = c("ihd", "hhd", "istroke", "hstroke", "aod"),
#   rr_per_10mmhg = c(0.83, 0.72, 0.73, 0.73, 0.93)
# )

ETIHAD_RR <- fread(paste0(wd_data, "ettehad_rr_bp_reduction_10mmHg.csv"))

# rename to cause column
ETIHAD_RR[, cause := fcase(
  Cause == "Coronary heart disease", "ihd",
  Cause == "Heart failure", "hhd",
  Cause == "Stroke", "istroke",
  default = NA_character_
)]

# keep only relevant causes
ETIHAD_RR <- ETIHAD_RR[cause %in% c("ihd", "hhd", "istroke","hstroke"),
                       c("cause","SBP_Category","RR"),with=F]

# hstroke from istroke
etihad_hstroke_rr <- ETIHAD_RR[cause=="istroke",]
etihad_hstroke_rr[, cause := "hstroke"]
ETIHAD_RR <- rbind(ETIHAD_RR, etihad_hstroke_rr)

#remove bp total
ETIHAD_RR <- ETIHAD_RR[SBP_Category != "Total", ]

#rename columns
setnames(ETIHAD_RR, c("SBP_Category", "RR"), c("bp_cat", "rr_per_10mmhg"))

# Standard BP categories used in model
bp_full <- c("<120", "120-129", "130-139", "140-149",
             "150-159", "160-169", "170-179", "180+")

# mapping function
map_bp <- function(x){
  fcase(
    x %in% c("<120", "120-129", "<130")        , "<130",
    x == "130-139"                             , "130-139",
    x == "140-149"                             , "140-149",
    x == "150-159"                             , "150-159",
    x %in% c("160-169","170-179","180+","≥160"), ">=160"
  )
}

# build mapping table
bp_map <- data.table(
  bp_cat_full = bp_full,
  bp_cat = map_bp(bp_full)
)

# merge only on matching categories (carry forward RR)
expanded <- bp_map[
  ETIHAD_RR,
  on = .(bp_cat),
  allow.cartesian = TRUE
][
  , .(cause, bp_cat_full, rr = rr_per_10mmhg)
][
  order(cause, bp_cat_full)
]

ETIHAD_RR <- copy(expanded)

#rename etihad
setnames(ETIHAD_RR, c("bp_cat_full", "rr"), c("bp_cat", "rr_per_10mmhg"))

# cleaning
rm(expanded, etihad_hstroke_rr)

# Calculate coverage for a given year based on linear scale-up
# from start_year (0% coverage) to target_year (target_coverage%)
calculate_coverage_by_year <- function(year, 
                                       start_year = 2026, 
                                       target_year = 2050,
                                       target_coverage = 0.50) {
  
  # Years elapsed since start
  
  # # original
  # years_elapsed <- pmax(0, year - start_year)
  
  # no delay
  years_elapsed <- pmax(0, year - start_year + 1)
  
  # Total years in scale-up period
  # # original
  # total_years <- target_year - start_year
  
  # no delay
  total_years <- target_year - start_year + 1
  
  # Linear interpolation
  coverage <- pmin(target_coverage * years_elapsed / total_years, 
                   target_coverage)
  
  # Before start_year, coverage is 0
  coverage[year < start_year] <- 0
  
  # After target_year, coverage is at target
  coverage[year > target_year] <- target_coverage
  
  return(coverage)
}

# Vectorized version for data.table
add_coverage_by_year <- function(dt, 
                                 year_col = "year",
                                 start_year = 2026,
                                 target_year = 2050,
                                 target_coverage = 0.50,
                                 coverage_col = "coverage_t") {
  
  dt[, (coverage_col) := calculate_coverage_by_year(
    get(year_col), 
    start_year, 
    target_year, 
    target_coverage
  )]
  
  return(dt)
}

# Calculate weighted average coverage across hypertensive BP bins
# Used for case fatality calculations
calculate_aggregate_coverage <- function(dt, 
                                         hypertensive_bins = c("140-149", "150-159", 
                                                               "160-169", "170-179", "180+"),
                                         bp_col = "bp_cat",
                                         coverage_col = "coverage_t",
                                         prob_col = "prob",
                                         grouping_vars = c("age", "sex", "location", 
                                                           "cause", "year"),
                                         hypertensive_only = NULL) {
  
  # hypertensive_only = NULL then use all bins (default behavior)
  # hypertensive_only = TRUE then use only hypertensive bins
  # hypertensive_only = FALSE then explicitly use all bins
  
  
  dt_use <- copy(dt)
  
  if (!is.null(hypertensive_only) && hypertensive_only == TRUE) {
    dt_use <- dt_use[get(bp_col) %in% hypertensive_bins]
  }
  
  # Compute weighted mean coverage
  coverage_agg <- dt_use[, 
                         .(
                           coverage_agg = weighted.mean(get(coverage_col), 
                                                        get(prob_col), 
                                                        na.rm = TRUE)
                         ),
                         by = grouping_vars
  ]
  
  return(coverage_agg)
}

# Import from excel .xlsx file ettehad_rr_bp_reduction_effects
ETIHAD_RR_BIN<- as.data.table(read_excel(paste0(wd_data, "ettehad_rr_bp_reduction_effects.xlsx"), 
                                         sheet = "Sheet1")) 

# Calculate cumulative ETIHAD effect size for BP bins
# For each bin, calculate cumulative effect of reducing BP by 10 mmHg steps

calculate_etihad_cumulative_rr <- function(bp_cat, 
                                           cause_name, 
                                           diabetes_weight = 0.1,
                                           etihad_rr_table = ETIHAD_RR_BIN) {
  
  # Input lengths must match
  if (length(bp_cat) != length(cause_name)) {
    stop("bp_cat and cause_name must have the same length")
  }
  
  # Build key for matching
  lookup_key <- paste(cause_name, bp_cat, sep = "_")
  table_key  <- paste(etihad_rr_table$cause, etihad_rr_table$bp_cat, sep = "_")
  
  # Indices in lookup table
  idx <- match(lookup_key, table_key)
  
  if (any(is.na(idx))) {
    stop("Some bp_cat–cause combinations were not found in ETIHAD_RR")
  }
  
  # Extract effect sizes from table
  effect_no_diab <- etihad_rr_table$effect_size_nodiabetes[idx]
  effect_diab    <- etihad_rr_table$effect_size_diabetes[idx]
  
  # Weighted average
  effect_weighted <- 
    (1 - diabetes_weight) * effect_no_diab +
    diabetes_weight   * effect_diab
  
  return(effect_weighted)
}


# Load GBD relative risks (per 10 mmHg increase)
# Expected format: columns for age, sex, cause, rr_per_10mmhg
# dt_gbd_rr <- readRDS("path/to/gbd_rr_data.rds")

# Assign GBD relative risks based on BP category
# RRs are relative to <120 mmHg reference
get_gbd_relative_risks <- function(bp_cat, age, cause, dt_gbd_rr = NULL) {
  
  if (is.null(dt_gbd_rr)) {
    dt_gbd_rr <- get("dt_gbd_rr", envir = .GlobalEnv)
  }
  
  # Calculate midpoint SBP for each category
  sbp_midpoint <- case_when(
    bp_cat == "<120" ~ 110,
    bp_cat == "120-129" ~ 125,
    bp_cat == "130-139" ~ 135,
    bp_cat == "140-149" ~ 145,
    bp_cat == "150-159" ~ 155,
    bp_cat == "160-169" ~ 165,
    bp_cat == "170-179" ~ 175,
    bp_cat == "180+" ~ 185,
    TRUE ~ NA_real_
  )
  
  # 2. Increment from 120
  inc_10 <- (sbp_midpoint - 120) / 10
  
  # 3. Merge single-row-by-row using a fast join
  # Create temporary table with input values
  tmp <- data.table(age = age, cause = cause)
  
  # Join RRs
  tmp <- dt_gbd_rr[tmp, on = c("age", "cause")]
  
  rr10 <- tmp$rr_per_10mmhg
  
  # 4. Compute RR
  rr <- ifelse(inc_10 > 0, rr10^inc_10, 1)
  
  return(rr)
}

# Expand age groups to single-year ages
expand_to_single_year_ages <- function(dt) {
  dt[, age := as.numeric(substr(age, 1, 2))]
  dt <- dt[rep(seq_len(nrow(dt)), each = 5)]
  dt[, age2 := rep(1:5, nrow(dt)/5)][, age := age + age2 - 1]
  
  over90 <- dt[age == 89]
  over90 <- over90[rep(seq_len(nrow(over90)), each = 6)]
  over90[, age2 := rep(1:6, nrow(over90)/6)][, age := age + age2]
  
  rbindlist(list(dt, over90))[, age2 := NULL]
}

# Calculate BP probabilities with optional treatment effect

# That last line overwrites everything and makes covinc always 0, 
# so treatment never shifts BP probabilities. since we apply ETTEHAD 
# via coverage-by-bin, so any redistribution across BP bins, it’s currently disabled.

get.bp.prob <- function(DT, rx, drugaroc = "baseline") {
  # Select appropriate coverage increment variable
  cov_var <- switch(
    drugaroc,
    "baseline" = "aroc2",
    "p75" = "p_change2",
    "p975" = "a_change2",
    "ideal" = "ideal",
    stop("Invalid 'drugaroc' argument. Must be one of: baseline, p75, p975, ideal")
  )
  
  # Apply coverage increment only if antihypertensive treatment (rx == 1)
  DT[, covinc := if (rx == 1) get(cov_var) else aroc2]
  
  # Patch missing coverage increments to 0 ommit aroc
  DT[, covinc := 0]
  
  # Define BP cutpoints
  bp_breaks <- c(-Inf, 120, 130, 140, 150, 160, 170, 180, Inf)
  bp_labels <- c("<120", "120-129", "130-139", "140-149", 
                 "150-159", "160-169", "170-179", "180+")
  
  # Compute BP category probabilities using vectorized operations
  for (i in seq_along(bp_labels)) {
    lower <- bp_breaks[i]
    upper <- bp_breaks[i + 1]
    DT[bp_cat == bp_labels[i], 
       prob := pnorm(upper, Mean, stdev) - pnorm(lower, Mean, stdev)]
  }
  
  # Adjust probabilities for antihypertensive treatment coverage
  if (rx == 1) {
    DT[, shift := prob * covinc]
    DT[bp_cat %in% c("<120", "120-129", "130-139"), shift := 0]
    
    # Compute reallocation between BP bins by diabetes status
    DT[, add130 := sum(shift * diabetes), by = .(age, sex, Year)]
    DT[, add140 := sum(shift * (1 - diabetes)), by = .(age, sex, Year)]
    
    # Update category probabilities
    DT[, prob := prob - shift]
    DT[bp_cat == "120-129", prob := prob + add130]
    DT[bp_cat == "130-139", prob := prob + add140]
  }
  
  # Return relevant variables
  return(DT[, .(age, sex, Year, bp_cat, prob, location)])
}

# Calculate baseline incidence rates using GBD RRs
calculate_baseline_incidence_gbd <- function(bp_prob, intervention_rates, 
                                             Country, dt_gbd_rr) {
  cat("  - Calculating baseline incidence with GBD RRs\n")
  
  # Expand to single-year ages
  bp_prob <- expand_to_single_year_ages(bp_prob)
  
  # Add GBD relative risks for all causes
  causes <- c("ihd", "hhd", "istroke", "hstroke", "aod")
  
  for (cause in causes) {
    col_name <- paste0("RRi_", toupper(cause))
    bp_prob[, (col_name) := get_gbd_relative_risks(bp_cat, age,
                                                   cause, dt_gbd_rr)]
  }
  
  # Calculate alphas (normalization factors)
  # alpha = sum(prob * RR) across all BP categories
  alphas <- bp_prob[, .(
    ihd = sum(prob * RRi_IHD),
    istroke = sum(prob * RRi_ISTROKE),
    hstroke = sum(prob * RRi_HSTROKE),
    hhd = sum(prob * RRi_HHD),
    aod = sum(prob * RRi_AOD)
  ), by = .(age, sex, location, Year)]
  
  alphas <- melt(alphas, id.vars = c("age", "sex", "location", "Year"),
                 variable.name = "cause", value.name = "alpha")
  
  # Prepare RRi data (long format)
  rris <- bp_prob[, .(age, sex, Year, location, bp_cat, prob, 
                      RRi_IHD, RRi_HHD, RRi_ISTROKE, RRi_AOD)]
  rris[, RRi_HSTROKE := RRi_ISTROKE]
  
  setnames(rris, 
           c("RRi_IHD", "RRi_HHD", "RRi_ISTROKE", "RRi_HSTROKE", "RRi_AOD"),
           c("ihd", "hhd", "istroke", "hstroke", "aod"))
  
  rris <- melt(rris, id.vars = c("age", "sex", "location", "bp_cat", "prob", "Year"),
               variable.name = "cause", value.name = "RRi")
  
  # Merge with alphas
  bp_prob_full <- merge(rris, alphas, 
                        by = c("age", "sex", "location", "cause", "Year"))
  setnames(bp_prob_full, "Year", "year")
  
  # Merge with intervention rates
  dt <- merge(intervention_rates[location == Country], bp_prob_full,
              by = c("age", "sex", "location", "cause", "year"))
  
  # Calculate BP bin-specific baseline incidence: IR_bin = (RRi * IR) / alpha
  dt[, IR_bin := (RRi * IR) / alpha]
  
  return(dt)
}

# # Calculate Ettehad-based effect size for BP reduction

apply_coverage_adjustment <- function(effect_size, 
                                      coverage_t, 
                                      coverage_0 = 0) {
  
  if (all(coverage_0 == 0)) {
    # Simplified formula when baseline coverage is 0
    return(effect_size * coverage_t)
  } else {
    # Full formula when baseline coverage > 0
    numerator <- effect_size * (coverage_t - coverage_0)
    denominator <- 1 - effect_size * coverage_0
    return(numerator / denominator)
  }
}
#...........................................................
## Anti hypertensive therapy ----
#...........................................................

dt_hbp_control <- readRDS(file = paste0(wd_data,"hbp_control_data.rds"))


# For aim 2 (150 million more controlled people) upload targets file

dt_hbp_targets <- fread(paste0(wd_data,"htn_control_targets_by_loc.csv"))

calculate_antihypertensive_impact_etihad <- function(intervention_rates, 
                                                     Country, 
                                                     DT.in,
                                                     dt_gbd_rr,
                                                     target_control = 0.50,
                                                     drugcov = "p75",
                                                     start_year = 2026,
                                                     target_year = 2050,
                                                     baseline_ctrl = 0) {
  cat(" - Calculating antihypertensive impact using ETIHAD effect sizes\n")
  
  # Clamp baseline control to [0,1]
  baseline_ctrl <- max(min(baseline_ctrl, 1), 0)
  
  # Step 1: Get baseline BP distribution (no treatment)
  bp_prob_base <- get.bp.prob(DT.in, rx = 0, drugaroc = "baseline")
  
  # Step 2: Calculate baseline bin-specific incidence using GBD RRs
  dt_baseline <- calculate_baseline_incidence_gbd(
    copy(bp_prob_base), intervention_rates, Country, dt_gbd_rr
  )
  
  # Step 3: Add ETIHAD effect sizes for each BP bin and cause
  causes  <- c("ihd", "hhd", "istroke", "hstroke", "aod")
  bp_cats <- c("<120", "120-129", "130-139", "140-149", 
               "150-159", "160-169", "170-179", "180+")
  
  # Build ETIHAD effect-size table (via diabetes-weighted effects)
  etihad_effects <- dt_baseline[, .(N = mean(pop)),
                                by = .(location, year, age, sex, bp_cat, cause)]
  
  diabetes_prop <- expand_to_single_year_ages(DT.in)
  diabetes_prop <- diabetes_prop[, .(location, Year, age, sex, bp_cat, diabetes)]
  setnames(diabetes_prop, "Year", "year")
  
  etihad_effects <- merge(etihad_effects, diabetes_prop, all.x = TRUE)
  
  etihad_effects[, etihad_effect :=
                   calculate_etihad_cumulative_rr(bp_cat, cause,
                                                  diabetes_weight = diabetes)]
  etihad_effects[, c("diabetes", "N") := NULL]
  
  dt_baseline <- merge(
    dt_baseline, etihad_effects,
    by = c("location", "year", "age", "sex", "bp_cat", "cause"),
    all.x = TRUE
  )
  
  # # Step 4: Coverage Scale-Up
  # 4. Coverage scale-up (NEW LOGIC)
  
  # Increment relative to baseline control
  incr_target <- max(target_control - baseline_ctrl, 0)
  
  # Build scale-up curve as if baseline = 0 then incr_target
  dt_baseline <- add_coverage_by_year(
    dt_baseline,
    year_col        = "year",
    start_year      = start_year,
    target_year     = target_year,
    target_coverage = incr_target,
    coverage_col    = "coverage_increment"
  )
  
  # Hypertensive BP bins eligible for treatment
  hypertensive_bins <- c("140-149", "150-159",
                         "160-169", "170-179", "180+")
  
  # Initialize coverage variables
  dt_baseline[, `:=`(coverage_0 = 0, coverage_t = 0)]
  
  # Apply baseline and scale-up to hypertensive bins only
  dt_baseline[bp_cat %in% hypertensive_bins,
              `:=`(
                coverage_0 = baseline_ctrl,
                coverage_t = baseline_ctrl + coverage_increment
              )]
  
  # After target year, hold at final coverage
  dt_baseline[year > target_year & bp_cat %in% hypertensive_bins,
              coverage_t := baseline_ctrl + incr_target]
  
  # Bound between baseline and 1
  dt_baseline[bp_cat %in% hypertensive_bins,
              coverage_t := pmin(pmax(coverage_t, baseline_ctrl), 1)]
  
  # Non-hypertensive bins: no coverage
  dt_baseline[!bp_cat %in% hypertensive_bins,
              `:=`(coverage_t = 0, coverage_0 = 0)]
  
  dt_baseline[, coverage_increment := NULL]
  
  
  # Step 5: Apply coverage-adjusted ETIHAD effect sizes using helper
  # effect_size_t is the *incremental* effect from moving from coverage_0 to coverage_t
  dt_baseline[, effect_size_t := apply_coverage_adjustment(
    etihad_effect,
    coverage_t,
    coverage_0 = coverage_0
  )]
  
  # Step 6: New bin-specific incidence
  dt_baseline[, IR_bin_new := IR_bin * (1 - effect_size_t)]
  
  # Step 7: Population-weighted average incidence
  dt_baseline[, IR_new := sum(IR_bin_new * prob),
              by = .(age, sex, location, cause, year)]
  
  # If no scale up keep flat
  #baseline_ctrl
  if(baseline_ctrl>=target_control){
    dt_baseline[, IR_new := IR]
  }
  
  # Prior to the intervention start, force baseline
  dt_baseline[year < start_year, IR_new := IR]
  
  # Step 8: Effect ratio for incidence
  
  dt_baseline[, eff_ir := IR_new / IR]
  
  # Step 9: Case fatality reduction – use *incremental* coverage,
  # not the absolute coverage, so baseline control isn't double counted.
  cf_etihad <- data.table(
    cause = c("ihd", "istroke", "hstroke", "hhd", "aod"),
    cf_reduction_per_control = c(0.24, 0.36, 0.76, 0.20, 0.047)
  )
  
  dt_baseline <- merge(dt_baseline, cf_etihad, by = "cause", all.x = TRUE)
  
  # incremental coverage above baseline
  dt_baseline[, coverage_delta := pmax(coverage_t - coverage_0, 0)]
  
  coverage_aggregate <- calculate_aggregate_coverage(
    dt_baseline,
    hypertensive_bins = hypertensive_bins,
    bp_col       = "bp_cat",
    coverage_col = "coverage_delta",
    prob_col     = "prob",
    grouping_vars = c("age", "sex", "location", "cause", "year"),
    hypertensive_only = TRUE
  )
  
  dt_baseline <- merge(
    dt_baseline, coverage_aggregate,
    by = c("age", "sex", "location", "cause", "year"),
    all.x = TRUE
  )
  dt_baseline[is.na(coverage_agg), coverage_agg := 0]
  
  # Apply CF reduction only to the incremental coverage
  dt_baseline[, CF_new := CF * (1 - cf_reduction_per_control * coverage_agg)]
  dt_baseline[cause == "aod" & age < 60, CF_new := CF]  # keep your original restriction
  dt_baseline[, eff_cf := CF_new / CF]
  
  # Step 10: Collapse to final output (remove BP bin dimension)
  dt_final <- unique(dt_baseline[, .(
    age, sex, location, cause, year,
    IR = IR_new, CF = CF_new,
    BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, ALL.mx,
    eff_ir, eff_cf
  )])
  
  setorder(dt_final, year, sex, location, cause, age)
  
  cat("  - ETIHAD effect sizes applied successfully\n")
  cat("  - Baseline control =", baseline_ctrl, 
      "; target control =", target_control,
      "by", target_year, "\n")
  
  return(dt_final)
  
}


## Anti hypertensive therapy - Diabetes subgroup 

calculate_antihypertensive_diabetes <- function(intervention_rates,
                                                Country,
                                                DT.in,
                                                dt_gbd_rr,
                                                target_control_diabetes = 0.80,
                                                baseline_ctrl_diabetes  = 0,
                                                start_year              = 2026,
                                                target_year             = 2030) {
  
  cat(" - Calculating antihypertensive impact (diabetes subgroup) using ETIHAD effect sizes\n")
  
  baseline_ctrl_diabetes <- max(min(baseline_ctrl_diabetes, 1), 0)
  
  # Step 1: Baseline BP distribution
  bp_prob_base <- get.bp.prob(DT.in, rx = 0, drugaroc = "baseline")
  
  # Step 2: Baseline bin-specific incidence using GBD RRs
  dt_baseline <- calculate_baseline_incidence_gbd(
    copy(bp_prob_base), intervention_rates, Country, dt_gbd_rr
  )
  
  # Step 3: ETIHAD effect sizes (unchanged — full effect size as per Ettehad)
  etihad_effects <- dt_baseline[, .(N = mean(pop)),
                                by = .(location, year, age, sex, bp_cat, cause)]
  
  diabetes_prop <- expand_to_single_year_ages(DT.in)
  diabetes_prop <- diabetes_prop[, .(location, Year, age, sex, bp_cat, diabetes)]
  setnames(diabetes_prop, "Year", "year")
  
  etihad_effects <- merge(etihad_effects, diabetes_prop, all.x = TRUE)
  
  # ? Check, here diabetes weight is 1, so effect size is fully applied to the 
  # diabetic population, and 0 to non-diabetic population.
  
  # Update: here we apply the diabetes-weighted effect size, which applies the full ETIHAD effect to the diabetic population
  # diabetes_weight = 1 means full effect for diabetics, 0 means no effect for diabetics.

  etihad_effects[, etihad_effect := calculate_etihad_cumulative_rr(
    bp_cat, cause, diabetes_weight = 1
  )]
  
  # etihad_effects[, etihad_effect := calculate_etihad_cumulative_rr(
  #   bp_cat, cause, diabetes_weight = diabetes
  # )]
  etihad_effects[, c("diabetes", "N") := NULL]
  
  dt_baseline <- merge(
    dt_baseline, etihad_effects,
    by = c("location", "year", "age", "sex", "bp_cat", "cause"),
    all.x = TRUE
  )
  
  # Merge diabetes prevalence — needed for downscaling in Steps 6 and 9
  dt_baseline <- merge(dt_baseline, diabetes_prop,
                       by = c("location", "year", "age", "sex", "bp_cat"),
                       all.x = TRUE)
  
  # Step 4: Coverage scale-up among diabetics
  # coverage_0 and coverage_t are within-diabetic-subgroup rates [0,1]
  # Diabetes downscaling happens explicitly in Steps 6 and 9
  hypertensive_bins <- c("140-149", "150-159", "160-169", "170-179", "180+")
  
  incr_diab <- max(target_control_diabetes - baseline_ctrl_diabetes, 0)
  
  dt_baseline <- add_coverage_by_year(
    dt_baseline,
    year_col        = "year",
    start_year      = start_year,
    target_year     = target_year,
    target_coverage = incr_diab,
    coverage_col    = "coverage_increment_diab"
  )
  
  dt_baseline[, `:=`(coverage_0 = 0, coverage_t = 0)]
  
  dt_baseline[bp_cat %in% hypertensive_bins, `:=`(
    coverage_0 = baseline_ctrl_diabetes,
    coverage_t = baseline_ctrl_diabetes + coverage_increment_diab
  )]
  dt_baseline[bp_cat %in% hypertensive_bins,
              coverage_t := pmin(pmax(coverage_t, coverage_0), 1)]
  
  dt_baseline[!bp_cat %in% hypertensive_bins,
              `:=`(coverage_t = 0, coverage_0 = 0)]
  
  dt_baseline[, coverage_increment_diab := NULL]
  
  # Step 5: ETIHAD effect size — as-is, no diabetes scaling here
  dt_baseline[, effect_size_t := apply_coverage_adjustment(
    etihad_effect,
    coverage_t,
    coverage_0 = coverage_0
  )]
  
  # Step 6: FIX — downscale by diabetes prevalence
  # effect_size_t is the full Ettehad effect; only the diabetic fraction
  # of the bin actually receives this treatment, so multiply by diabetes
  dt_baseline[, IR_bin_new := IR_bin * (1 - diabetes * effect_size_t)]
  
  # Step 7: Population-weighted average incidence
  dt_baseline[, IR_new := sum(IR_bin_new * prob),
              by = .(age, sex, location, cause, year)]
  
  if (baseline_ctrl_diabetes >= target_control_diabetes) {
    dt_baseline[, IR_new := IR]
  }
  
  dt_baseline[year < start_year, IR_new := IR]
  
  # Step 8: Effect ratio
  dt_baseline[, eff_ir := IR_new / IR]
  
  # Step 9: Case fatality reduction
  cf_etihad <- data.table(
    cause = c("ihd", "istroke", "hstroke", "hhd", "aod"),
    cf_reduction_per_control = c(0.24, 0.36, 0.76, 0.20, 0.047)
  )
  
  dt_baseline <- merge(dt_baseline, cf_etihad, by = "cause", all.x = TRUE)
  
  # FIX — downscale coverage_delta by diabetes prevalence before aggregating
  # so CF reduction applies only to the diabetic fraction of the population
  dt_baseline[, coverage_delta := pmax(coverage_t - coverage_0, 0)]
  dt_baseline[, coverage_delta_diab := diabetes * coverage_delta]
  
  coverage_aggregate <- calculate_aggregate_coverage(
    dt_baseline,
    hypertensive_bins = hypertensive_bins,
    bp_col            = "bp_cat",
    coverage_col      = "coverage_delta_diab",
    prob_col          = "prob",
    grouping_vars     = c("age", "sex", "location", "cause", "year"),
    hypertensive_only = TRUE
  )
  
  dt_baseline <- merge(
    dt_baseline, coverage_aggregate,
    by = c("age", "sex", "location", "cause", "year"),
    all.x = TRUE
  )
  dt_baseline[is.na(coverage_agg), coverage_agg := 0]
  
  dt_baseline[, CF_new := CF * (1 - cf_reduction_per_control * coverage_agg)]
  dt_baseline[cause == "aod" & age < 60, CF_new := CF]
  dt_baseline[, eff_cf := CF_new / CF]
  
  # Patch: effect size only to diabetes population to multiplicative effect in markov model

  # 1) Extract raisedBP from DT.in (collapse bp_cat duplicates)
  raisedBP_dt <- unique(DT.in[, .(location, Year, age, sex, raisedBP)])
  
  # 2) Expand to single-year ages to match dt_baseline (which is age-continuous)
  raisedBP_dt <- expand_to_single_year_ages(raisedBP_dt)
  
  # 3) Align year variable name
  setnames(raisedBP_dt, "Year", "year")
  
  # 4) Keep only merge keys + raisedBP (avoid accidental extra cols)
  raisedBP_dt <- raisedBP_dt[, .(location, year, age, sex, raisedBP)]
  
  # 5) Merge into dt_baseline
  dt_baseline <- merge(
    dt_baseline,
    raisedBP_dt,
    by = c("location", "year", "age", "sex"),
    all.x = TRUE
  )
  
  # Safety
  dt_baseline[is.na(raisedBP), raisedBP := 0]
  
  dt_baseline[, eff_ir := 1 * (1-diabetes*raisedBP) + (eff_ir * diabetes*raisedBP)]
  dt_baseline[, eff_cf := 1 * (1-diabetes*raisedBP) + (eff_cf * diabetes*raisedBP)]
  
  # Step 10: Collapse to final output
  dt_final <- unique(dt_baseline[, .(
    age, sex, location, cause, year,
    IR = IR_new, CF = CF_new,
    BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, ALL.mx,
    eff_ir, eff_cf
  )])
  
  setorder(dt_final, year, sex, location, cause, age)
  
  cat("  - ETIHAD effect sizes applied successfully (diabetes subgroup)\n")
  cat("  - Baseline diabetes BP control =", baseline_ctrl_diabetes,
      "; target =", target_control_diabetes,
      "by", target_year, "\n")
  
  return(dt_final)
}

#...........................................................
## Sodium reduction  ----
#...........................................................
# Prepare sodium data (run once at setup)
## Sodium reduction
# prepare_sodium_data <- function(data.in, wd_data) {
#   dt_sodium_scenarios <- readRDS(file = paste0(wd_data, "Sodium/", "sodium_policy_scenarios.rds"))
#   dt_sodium_scenarios <- dt_sodium_scenarios[year == 2024, .(location, sodium_current)]
#   
#   data.in <- merge(data.in, dt_sodium_scenarios, by = "location", all.x = TRUE)
#   data.in[!is.na(sodium_current), salt := sodium_current * 2.5]
#   data.in[, sodium_current := NULL]
#   
#   return(data.in)
# }

prepare_sodium_data <- function(data.in, wd_data) {
  dt_sodium_scenarios <- readRDS(file = paste0(wd_data,"sodium_policy_scenarios.rds"))
  dt_sodium_scenarios <- dt_sodium_scenarios[year == 2024, .(location, sodium_current)]
  
  data.in <- merge(data.in, dt_sodium_scenarios, by = "location", all.x = TRUE)
  data.in[!is.na(sodium_current), salt := sodium_current]
  data.in[, sodium_current := NULL]
  
  return(data.in)
}

data.in <- prepare_sodium_data(data.in,wd_data)

apply_salt_reduction <- function(DT.in, salteff, saltmet, saltyear1 = 2026, saltyear2) {
  if (saltmet == "percent") {
    DT.in[, salt_target := salt * (1 - salteff)]
    DT.in[salt_target<2, salt_target:=2]
    DT.in[salt > 0, salt := salt - salt_target]
    DT.in[salt < 2, salt := 2]
  } else if (saltmet == "target") {
    DT.in[, salt := salt - salteff]
    DT.in[salt < 0, salt := 0]
  } else if (saltmet == "app") {
    DT.in[, salt := salteff]
  }
  
  if (salteff != 0) {
    DT.in[Year >= saltyear1 & Year <= saltyear2, 
          Mean := Mean - (((2.8 * raisedBP) + ((1 - raisedBP) * 1.0)) * 
                            salt * (Year - saltyear1 + 1) / (saltyear2 - saltyear1 + 1))]
    
    DT.in[Year > saltyear2, 
          Mean := Mean - (((2.8 * raisedBP) + ((1 - raisedBP) * 1.0)) * salt)]
  }
  
  return(DT.in)
}


# Sodium should also use ETIHAD effect sizes for consistency
calculate_sodium_impact_etihad <- function(intervention_rates, 
                                           Country, 
                                           DT.in, 
                                           salteff,
                                           saltmet,
                                           saltyear1 = 2026,
                                           saltyear2 = 2050,
                                           dt_gbd_rr) {
  cat(" - Calculating sodium impact using ETIHAD effect sizes\n")
  
  # Step 1: Get baseline BP distribution (no sodium intervention)
  bp_prob_base <- get.bp.prob(DT.in, rx = 0, drugaroc = "baseline")
  
  # Step 2: Calculate baseline bin-specific incidence using GBD RRs
  dt_baseline <- calculate_baseline_incidence_gbd(
    copy(bp_prob_base), intervention_rates, Country, dt_gbd_rr
  )
  
  # Step 3: Calculate sodium reduction and BP shift over time
  # Merge salt data from DT.in
  salt_info <- unique(DT.in[, .(age, sex, salt, raisedBP, Year,aroc)])
  setnames(salt_info, "Year", "year")
  
  # function to split age "20-24" into 20:24
  expand_age <- function(x){
    if (x == "85plus") return(85:95)  # adjust as needed
    bounds <- as.numeric(unlist(strsplit(x, "-")))
    seq(bounds[1], bounds[2])
  }
  
  # expand table
  dt_expanded <- salt_info[, .(
    age_single = expand_age(age)
  ), by = .(age, sex, salt, raisedBP,aroc,year)]
  
  # reorder columns
  dt_expanded <- dt_expanded[, .(age = age_single, sex, salt, raisedBP,aroc,year)]
  
  dt_baseline <- merge(dt_baseline, dt_expanded, by = c("age", "sex", "year"), all.x = TRUE)
  
  # Calculate target salt reduction based on method
  if (saltmet == "percent") {
    # salteff is percentage reduction (e.g., 0.3 = 30% reduction)
    dt_baseline[, salt_target := salt * salteff]
  } else if (saltmet == "target") {
    # salteff is absolute target reduction in grams
    dt_baseline[, salt_target := pmin(salt, salteff)]
  } else if (saltmet == "app") {
    # salteff is target intake level
    dt_baseline[, salt_target := pmax(0, salt - salteff)]
  }
  
  # Apply minimum salt intake of 2g
  
  dt_baseline[, salt_target := ifelse(salt - salt_target < 2, salt - 2, salt_target)]
  
  # Step 4: Apply linear progressive decline in sodium intake
  # During scale-up period (saltyear1 to saltyear2): linear progression
  dt_baseline[year >= saltyear1 & year <= saltyear2,
              salt_reduction := salt_target * (year - saltyear1 + 1) / (saltyear2 - saltyear1 + 1)]
  
  # After scale-up period: full reduction achieved
  dt_baseline[year > saltyear2,
              salt_reduction := salt_target]
  
  # Before intervention: no reduction
  dt_baseline[year < saltyear1,
              salt_reduction := 0]
  
  dt_baseline[is.na(salt_reduction) | salt_reduction < 0, salt_reduction := 0]
  
  # # Step 4: Apply progressive decline in sodium intake
  
  # Apply Filippini dose-response to get SBP reduction
  # Progressive BP lowering as sodium intake decreases
  dt_baseline[, sbp_reduction := ((2.8 * raisedBP) + ((1 - raisedBP) * 1.0)) * salt_reduction]
  
  # Step 5: Calculate ETIHAD effect sizes based on BP reduction
  # Number of 10 mmHg reductions achieved through sodium intervention
  #dt_baseline[, n_steps_sodium := sbp_reduction / 10]
  
  # Get ETIHAD relative risks per cause
  dt_baseline <- merge(dt_baseline, ETIHAD_RR, by = c("bp_cat","cause"), all.x = TRUE)
  
  etihad_effects <- dt_baseline[,list(N=mean(pop)),by=list(location,year,age,sex,bp_cat, cause)]
  
  diabetes_prop <- expand_to_single_year_ages(DT.in)
  diabetes_prop <- diabetes_prop[,c("location","Year","age","sex","bp_cat", "diabetes"),with=F]
  
  setnames(diabetes_prop, "Year", "year")
  
  # merge diabetes proportion
  etihad_effects <- merge(etihad_effects,diabetes_prop,all.x = T)
  
  etihad_effects[, etihad_effect := calculate_etihad_cumulative_rr(bp_cat, cause,diabetes_weight = diabetes)]
  
  etihad_effects[,c("diabetes","N"):=NULL]
  # Merge ETIHAD effects into baseline data
  dt_baseline <- merge(dt_baseline, etihad_effects, 
                       by = c("location","year","age","sex","bp_cat", "cause"), all.x = TRUE)
  
  # Effect size from sodium intervention
  
  dt_baseline[, etihad_effect := (1-rr_per_10mmhg)]
  dt_baseline[, etihad_effect_sodium := etihad_effect * 0.1 * sbp_reduction]
  
  # Step 6: Apply effect sizes to incidence
  # IR_bin_new = IR_bin * (1 - effect_size)
  dt_baseline[, IR_bin_new := IR_bin * (1 - etihad_effect_sodium)]
  
  # Step 7: Calculate population-weighted average incidence
  # Using BASELINE population proportions (prob) - these stay constant
  dt_baseline[, IR_new := sum(IR_bin_new * prob), 
              by = .(age, sex, location, cause, year)]
  
  # Before intervention: no effect
  dt_baseline[year < saltyear1, IR_new := IR]
  
  # Step 8: Calculate effect ratio
  dt_baseline[, eff_ir := IR_new / IR]
  
  # Step 9: Apply case fatality reduction
  # CF reduction factors from ETIHAD (different from IR reductions)
  cf_etihad <- data.table(
    cause = c("ihd", "istroke", "hstroke", "hhd", "aod"),
    cf_reduction_per_control = c(0.24, 0.36, 0.76, 0.20, 0.047)
  )
  
  dt_baseline <- merge(dt_baseline, cf_etihad, by = "cause", all.x = TRUE)
  
  # Apply CF Trend AROC reduction (except for AOD in younger ages)
  #dt_baseline[, CF_new := CF * (1 - cf_reduction_per_control * control_agg)] 
  #dt_baseline[, CF_new := CF * (1 - cf_reduction_per_control * aroc)]
  
  # ?? Test
  # dt_baseline[, CF_new := CF * (1 - cf_reduction_per_control * (1 - eff_ir))]
  
  #dt_baseline[, CF_new := (CF * eff_ir) + ((CF * (1 - cf_reduction_per_control)) * (1 - eff_ir))]
  #dt_baseline[cause == "aod" & age < 60, CF_new := CF]
  
  # No secondary effect on case fatality from sodium reduction
  dt_baseline[, CF_new := CF]
  dt_baseline[, eff_cf := CF_new / CF]
  
  # Step 10: Collapse to final output (remove BP bin dimension)
  dt_final <- unique(dt_baseline[, .(
    age, sex, location, cause, year,
    IR = IR_new, CF = CF_new,
    BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, ALL.mx,
    eff_ir, eff_cf
  )])
  
  setorder(dt_final, year, sex, location, cause, age)
  
  cat("  - ETIHAD effect sizes applied successfully to sodium intervention\n")
  cat("  - Sodium reduction scales linearly from 0 (", saltyear1, ") to full reduction (", saltyear2, ")\n")
  cat("  - Using", saltmet, "method with salteff =", salteff, "\n")
  
  return(dt_final)
}

#...........................................................
## TFA Policy ----
#...........................................................

dt_tfa_scenarios <- as.data.table(readRDS(file = paste0(wd_data,"tfa_policy_scenarios.rds")))

# subset from base year   
dt_tfa_scenarios <- dt_tfa_scenarios[year>=2017,]

# Convert to percent scale
dt_tfa_scenarios[, tfa_current := tfa_current * 100]
dt_tfa_scenarios[, tfa_target  := tfa_target * 100]

# Function to calculate IHD mortality reduction from trans fat intake reduction

calculate_tfa_impact <- function(dt_tfa_scenarios,
                                 intervention_rates,
                                 Country,
                                 target_tfa = 0,
                                 policy_start_year = 2027) {
  cat("  - Calculating TFA impact\n")
  
  #..................................
  # STEP 1: Subset country-specific intervention table
  #..................................
  
  dt <- intervention_rates[location == Country]
  
  #..................................
  # STEP 2: Merge in country-specific trans-fat exposure levels
  #..................................
  
  dt <- merge(dt, dt_tfa_scenarios[location == Country],
              by = c("location", "year"), all.x = TRUE)
  
  #..................................
  # STEP 3: Compute reduction in TFA exposure (delta)
  #  - Before policy start year then no reduction
  #  - After policy start year  then  difference between current and target
  #..................................
  
  dt[, delta := 0]
  dt[year >= policy_start_year, delta := pmax(tfa_current - target_tfa, 0)]
  
  #..................................
  # STEP 4: Assign age-specific relative risk (RR) per 1% of energy from TFA
  #  These correspond to GBD-based RR gradients across age groups
  #..................................
  
  dt[, rr_per_1percent := fcase(
    age >= 20 & age <= 24, 1.21,
    age >= 25 & age <= 29, 1.20,
    age >= 30 & age <= 34, 1.19,
    age >= 35 & age <= 39, 1.18,
    age >= 40 & age <= 44, 1.17,
    age >= 45 & age <= 49, 1.16,
    age >= 50 & age <= 54, 1.15,
    age >= 55 & age <= 59, 1.14,
    age >= 60 & age <= 64, 1.13,
    age >= 65 & age <= 69, 1.11,
    age >= 70 & age <= 74, 1.10,
    age >= 75 & age <= 79, 1.09,
    age >= 80, 1.07,
    default = 1
  )]
  
  #..................................
  # STEP 5: Convert TFA reduction to mortality effect size
  #
  # The denominator ensures:
  #  - Effect size stays in [0,1]
  #  - Consistent scaling across heterogeneous baseline exposures
  #..................................
  
  dt[, effect_size := (delta * (rr_per_1percent - 1)) /
       ((tfa_current * (rr_per_1percent - 1)) + 1)]
  
  dt[is.na(effect_size) | effect_size < 0, effect_size := 0]
  dt[effect_size > 1, effect_size := 1]
  
  #..................................
  # STEP 6: Apply case fatality (CF) reduction ONLY for Ischemic Heart Disease
  #
  # TFA policy has no measured effect on:
  #   - incidence
  #   - non-IHD mortality
  #
  # So: apply effect ONLY to CF of IHD.
  #..................................
  
  dt[, CF_0 := CF]
  dt[cause == "ihd", CF := CF * (1 - effect_size)]
  
  dt[, eff_cf := eff_cf * (1 - effect_size)]
  
  dt[, c("tfa_current", "tfa_target", "CF_0", "delta", 
         "effect_size", "rr_per_1percent") := NULL]
  
  setorder(dt, year, sex, location, cause, age)
  return(dt)
}


#...........................................................
## Statins ----
#...........................................................

# Compute scale up scenario targeting DM high risk population
dt_statin_scenarios <- readRDS(file = paste0(wd_data,"statin_data.rds"))

# # Add Diabetes Proportion
# dt_diabetes <- data.in[,c("location","age","sex","diabetes"),with=F]
# dt_diabetes <- unique(dt_diabetes)
# setnames(dt_diabetes, c("age"), c("age_group"))

# High Fasting Plasma Glucose Attributable Fraction 
# for IHD and ischaemic stroke, using country- & cause-specific AFs supplied in dt_af_statins.

dt_af_statins <- readRDS(file = paste0(wd_data,"af_statins.rds"))

# Function to calculate statins impact on IHD and stroke

# RR of major CVD events per 1.0 mmol/L reduction in LDL cholesterol at 1 year after randomization:
# Statin vs. control incidence 0.78 (0.77 - 0.81)
# RR coronary heart disease 0·80, 99% CI 0·74–0·87
# Source: Trials 2015
# https://doi.org/10.1016/S0140-6736(10)61350-5
# This is applied directly to incidence of IHD and stroke


# Attributable fraction for statins
# GBD 2021 Risk factor attribution High Fasting Plasma glucose
# https://vizhub.healthdata.org/gbd-compare/

## Statins

calculate_statins_impact <- function(dt_statin_scenarios,
                                     intervention_rates,
                                     Country,
                                     dt_af_statins,
                                     adherence_ir = 1,   # primary prevention adherence (IR)
                                     adherence_cf = 1,   # secondary prevention adherence (CF)
                                     prop_athero_stroke = 0.60,
                                     statin_target_coverage = 0.60,
                                     statin_start_year = 2026,
                                     statin_target_year = 2050,
                                     baseline_statin_coverage = NULL) {
  cat("  - Calculating statins impact\n")
  
  #..........................................................
  # STEP 1: Define relative risks from major statin trials
  #..........................................................
  
  rr_ir_ihd      <- 0.74
  rr_ir_istroke  <- 0.80
  rr_cf_ihd      <- 0.80
  rr_cf_istroke  <- 0.96
  
  #..........................................................
  # STEP 2: Default attributable fractions for IHD & ischaemic stroke
  #..........................................................
  
  af_ihd     <- 0.1497
  af_istroke <- 0.1161
  
  # Subset country-specific intervention table
  dt <- intervention_rates[location == Country]
  
  # Age groups (kept just in case needed downstream)
  gbd_breaks <- c(seq(20, 85, 5), Inf)
  gbd_labels <- c(paste0(seq(20, 80, 5), "-", seq(24, 84, 5)), "85plus")
  dt[, age_group := as.character(
    cut(age, breaks = gbd_breaks, labels = gbd_labels,
        right = FALSE, include.lowest = TRUE)
  )]
  
  #..........................................................
  # STEP 3: Merge statin baseline scenario
  #  We will OVERRIDE statins_uptake_delta with linear scale-up.
  #..........................................................
  
  dt <- merge(dt,
              dt_statin_scenarios[location == Country],
              by = c("location", "year"),
              all.x = TRUE)
  
  #..........................................................
  # STEP 4: Determine baseline statin coverage and increment
  #   baseline_statin_coverage: coverage at statin_start_year
  #   statin_target_coverage  : total coverage by statin_target_year
  #   incr_target             : additional coverage above baseline
  #..........................................................
  
  if (!is.null(baseline_statin_coverage)) {
    baseline_cov <- baseline_statin_coverage
  } else {
    baseline_cov <- dt[year == statin_start_year & cause == "ihd",
                       mean(statins_current, na.rm = TRUE)]
  }
  
  if (is.na(baseline_cov)) baseline_cov <- 0
  baseline_cov <- max(min(baseline_cov, 1), 0)
  
  incr_target <- max(statin_target_coverage - baseline_cov, 0)
  
  # Build linear scale-up in incremental coverage (relative to baseline)
  # Using the same helper as antihypertensives: coverage_t in [0, incr_target]
  dt[, statins_uptake_delta := calculate_coverage_by_year(
    year,
    start_year      = statin_start_year,
    target_year     = statin_target_year,
    target_coverage = incr_target
  )]
  
  # Set baseline coverage (current coverage used in denominator)
  dt[, statins_current := baseline_cov]
  
  # Before start year: ensure no additional coverage
  dt[year < statin_start_year, statins_uptake_delta := 0]
  
  # Safety: clamp to [0, 1]
  dt[is.na(statins_uptake_delta) | statins_uptake_delta < 0, statins_uptake_delta := 0]
  dt[statins_uptake_delta > 1, statins_uptake_delta := 1]
  
  #..........................................................
  # STEP 5: Merge attributable fractions
  #..........................................................
  
  dt <- merge(dt, dt_af_statins, by = c("location", "cause"), all.x = TRUE)
  
  dt[is.na(af_statins) & cause == "ihd",     af_statins := af_ihd]
  dt[is.na(af_statins) & cause == "istroke", af_statins := af_istroke]
  
  dt[, IR_0     := IR]
  dt[, CF_0     := CF]
  dt[, eff_ir_0 := eff_ir]
  dt[, eff_cf_0 := eff_cf]
  
  #..........................................................
  # STEP 6: Compute effect sizes for IR (CF do not use AF)
  #
  #   effect_size = AF × 1-RR × Δcoverage × adherence
  #                  ______________________________
  #                  (1 − 1-RR × baseline_coverage × adherence)
  #
  # where Δcoverage = statins_uptake_delta and baseline_coverage = statins_current
  #..........................................................
  
  dt[, `:=`(
    effect_size_cf = fcase(
      cause == "ihd",
      (1- rr_cf_ihd) * statins_uptake_delta * adherence_cf /
        (1 - (1-rr_cf_ihd) * (statins_current * adherence_cf)),
      
      cause == "istroke",
      prop_athero_stroke * (1-rr_cf_istroke) *
        statins_uptake_delta * adherence_cf /
        (1 - (1-rr_cf_istroke) * (statins_current * adherence_cf)),
      
      default = NA_real_
    ),
    
    effect_size_ir = fcase(
      cause == "ihd",
      af_statins * (1-rr_ir_ihd) * (statins_uptake_delta * adherence_ir) /
        (1 - (1-rr_ir_ihd) * (statins_current * adherence_ir)),
      
      cause == "istroke",
      af_statins * (1-rr_ir_istroke) * statins_uptake_delta * adherence_ir /
        (1 - (1-rr_ir_istroke) * (statins_current * adherence_ir)),
      
      default = NA_real_
    )
  )]
  
  # If baseline already >= target, incr_target == 0 → effect_size_* == 0 automatically
  # but just in case numerical noise:
  if (incr_target == 0 || baseline_cov >= statin_target_coverage) {
    dt[, `:=`(effect_size_ir = 0, effect_size_cf = 0)]
  }
  
  #..........................................................
  # STEP 7: Apply statin effects ONLY for adults ≥40
  #   CF_new = CF × (1 − effect_size_cf)
  #   IR_new = IR × (1 − effect_size_ir)
  #..........................................................
  
  dt[age >= 40 & cause %in% c("ihd", "istroke"),
     `:=`(
       CF = CF * (1 - effect_size_cf),
       IR = IR * (1 - effect_size_ir)
     )]
  
  #..........................................................
  # STEP 8: Update effect ratios for tracking
  #..........................................................
  
  dt[!is.na(effect_size_ir), eff_ir := eff_ir_0 * (1 - effect_size_ir)]
  dt[!is.na(effect_size_cf), eff_cf := eff_cf_0 * (1 - effect_size_cf)]
  
  dt[, c("statins_uptake", "statins_target",
         "statins_uptake_lag", "statins_uptake_delta_lag",
         "CF_0", "IR_0", "eff_ir_0", "eff_cf_0",
         "effect_size_ir", "effect_size_cf",
         "af_statins", "age_group") := NULL]
  
  # keep statins_current & statins_uptake_delta if you want diagnostics
  # otherwise you can also drop them:
  # dt[, c("statins_current", "statins_uptake_delta") := NULL]
  
  setorder(dt, year, sex, location, cause, age)
  
  if (dt[, any(is.na(CF))] || dt[, any(is.na(IR))]) {
    stop("Computation produced NA values in CF or IR.", call. = FALSE)
  }
  
  cat("    Baseline statin coverage:", round(baseline_cov, 3), "\n")
  cat("    Target coverage:", round(statin_target_coverage, 3),
      "by", statin_target_year, "\n")
  
  dt[]
}

#...........................................................
## FAIR-Choices CVD package ----
#...........................................................
#
# See the file header for the full FAIR-Choices formula and the
# proportional-reduction interpretation decision. This section adds:
#   1. fair_condition_map  - table "Affected condition" -> model cause
#   2. load_fair_cvd_params() - reads taxonomy_effect_size.xlsx into a tidy,
#        editable parameter data.table (defaults; fully overridable)
#   3. dt_fair_params      - the default CVD parameter table (built once here)
#   4. fair_default_bundles - default package membership (full CVD package)
#   5. calculate_fair_cvd_impact() - the new intervention function, built in
#        the style of calculate_tfa_impact() / calculate_statins_impact():
#        it acts directly on intervention_rates, shifts CF (Mortality column)
#        and IR (Incidence column) and updates the eff_cf/eff_ir tracking
#        ratios multiplicatively.

# (1) Affected condition (taxonomy string) -> internal model cause code.
#     Conditions absent here (e.g. "Lower extremity peripheral arterial
#     disease", "SRHD") have no model cause and are dropped with a message.
fair_condition_map <- c(
  "Ischemic heart disease"         = "ihd",
  "Ischemic stroke"                = "istroke",
  "Intracerebral hemorrhage"       = "hstroke",
  "Hypertensive heart disease"     = "hhd",
  "Cardiomyopathy and myocarditis" = "cmd",
  "Rheumatic heart disease"        = "rhd"
)

# Helper: treat the literal strings "NA"/"None"/"" as missing.
.fair_clean_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("NA", "None", "none", "NaN", "")] <- NA_character_
  x
}

# (2) Load + tidy the FAIR-Choices CVD effect-size table.
#     Returns a data.table with one row per (bundle x affected condition):
#       bundle, condition, cause, affected_proportion, age_start, age_stop,
#       gender, duration_effect, mort_eff (-> CF), inc_eff (-> IR),
#       disab_eff, prev_eff   (the last two retained for transparency only).
#     EVERY value traces to a row of the source xlsx - none invented here.
load_fair_cvd_params <- function(path = paste0(wd_raw, "FAIR-Choices/taxonomy_effect_size.xlsx"),
                                 condition_map = fair_condition_map,
                                 verbose = TRUE) {

  raw <- as.data.table(read_excel(path))   # single sheet "Sheet 1"

  # Keep only cardiovascular-disease
  raw <- raw[.fair_clean_chr(`Sub-group`) == "Cardiovascular diseases"]
  
  # Keep one cardiovascular-disease interventions for model
  raw <- raw[model_inclusion=="Include",]

  # Numeric columns are routed through .fair_clean_chr() first so that literal
  # "NA"/"None" strings become NA cleanly (no "NAs introduced by coercion").
  num <- function(x) as.numeric(.fair_clean_chr(x))
  out <- data.table(
    bundle              = .fair_clean_chr(raw[["Intervention bundle"]]),
    condition           = .fair_clean_chr(raw[["Affected condition"]]),
    affected_proportion = num(raw[["Affected Proportion"]]),
    age_start           = num(raw[["Age start"]]),
    age_stop            = num(raw[["Age stop"]]),
    gender              = .fair_clean_chr(raw[["Affected gender"]]),
    duration_effect     = num(raw[["Duration effect"]]),
    mort_eff            = num(raw[["Mortality"]]),    # -> CF reduction
    disab_eff           = num(raw[["Disability"]]),   # not applied (no state)
    inc_eff             = num(raw[["Incidence"]]),    # -> IR reduction
    prev_eff            = num(raw[["Prevalence"]])    # not applied (no state)
  )

  # Map condition -> model cause; flag and drop conditions with no model cause.
  out[, cause := condition_map[condition]]

  dropped <- unique(out[is.na(cause), condition])
  if (length(dropped) > 0 && verbose) {
    cat("  - FAIR: dropping conditions with no model cause:",
        paste(dropped, collapse = "; "), "\n")
  }
  out <- out[!is.na(cause)]

  # Default targeted gender to "Both" if missing.
  out[is.na(gender), gender := "Both"]

  setcolorder(out, c("bundle", "condition", "cause", "affected_proportion",
                     "age_start", "age_stop", "gender", "duration_effect",
                     "mort_eff", "inc_eff", "disab_eff", "prev_eff"))
  out[]
}

# (3) Default parameter table (built once; users may edit it in place or rebuild
#     load_fair_cvd_params(path = "<local file>") and pass via `fair_params`).
dt_fair_params <- load_fair_cvd_params()

# (4) Default package membership = the FULL CVD primary + secondary + acute
#     package, i.e. every CVD bundle that maps onto a model cause. Override by
#     passing `fair_bundles = c(...)` a subset of these names.
fair_default_bundles <- sort(unique(dt_fair_params$bundle))

# (5) The new intervention function.
#
#   effect_size(bundle, cause, age, sex, year) =
#       Affected_Proportion x e_adj
#   with e_adj = apply_coverage_adjustment(value, Coverage(year), cov_baseline),
#   value = tabulated proportional reduction (Mortality column -> CF effect,
#   Incidence column -> IR effect), and Coverage(year) the linear scale-up from
#   fair_start_year to fair_target_year (reusing calculate_coverage_by_year()).
#   When several bundles affect the same cause/metric they are combined
#   MULTIPLICATIVELY on the surviving fraction: prod(1 - effect_size_b), exactly
#   as the rest of the model combines effects via Reduce(`*`, ...).
calculate_fair_cvd_impact <- function(intervention_rates,
                                      Country,
                                      fair_params            = NULL,  # NULL -> dt_fair_params
                                      fair_bundles           = NULL,  # NULL -> fair_default_bundles
                                      fair_start_year        = 2026,
                                      fair_target_year       = 2050,
                                      fair_target_coverage   = 0.80,
                                      fair_baseline_coverage = 0,
                                      fair_bundle_coverage   = NULL) { # named overrides

  cat("  - Calculating FAIR-Choices CVD package impact\n")

  # Resolve defaults (kept overridable)
  if (is.null(fair_params))  fair_params  <- dt_fair_params
  if (is.null(fair_bundles)) fair_bundles <- fair_default_bundles
  fair_params <- as.data.table(copy(fair_params))

  # Subset country-specific intervention table (matches TFA/statins style)
  dt <- intervention_rates[location == Country]

  # Restrict to the requested package and to causes actually present in the
  # model output (handles rhd/cmd not being carried, gracefully + with message).
  params <- fair_params[bundle %in% fair_bundles]

  present_causes <- unique(dt$cause)
  missing_causes <- setdiff(unique(params$cause), present_causes)
  if (length(missing_causes) > 0) {
    cat("  - FAIR: mapped cause(s) not present in intervention_rates, skipped:",
        paste(missing_causes, collapse = ", "), "\n")
  }
  params <- params[cause %in% present_causes]

  # Clamp baseline coverage to [0,1]
  fair_baseline_coverage <- max(min(fair_baseline_coverage, 1), 0)

  # Per-bundle target coverage (default = fair_target_coverage; overridable)
  bundle_cov <- function(b) {
    if (!is.null(fair_bundle_coverage) && !is.null(fair_bundle_coverage[[b]])) {
      return(fair_bundle_coverage[[b]])
    }
    fair_target_coverage
  }

  # Surviving fractions: 1 = no effect. We accumulate the multiplicative
  # (1 - effect_size) product across all bundles for CF and for IR separately.
  dt[, `:=`(fair_surv_cf = 1, fair_surv_ir = 1)]

  # Build the list of (param row, metric) pairs that actually carry a CF or IR
  # effect (Mortality -> CF, Incidence -> IR). Prevalence/Disability ignored.
  apply_metric <- function(value_col, surv_col) {
    rows <- params[!is.na(get(value_col)) & get(value_col) > 0]
    if (nrow(rows) == 0) return(invisible(NULL))

    for (i in seq_len(nrow(rows))) {
      r <- rows[i]

      # Coverage path for this bundle (vectorised over dt$year)
      cov_t <- calculate_coverage_by_year(
        dt$year,
        start_year      = fair_start_year,
        target_year     = fair_target_year,
        target_coverage = bundle_cov(r$bundle)
      )

      # e_adj: FAIR coverage-adjusted efficacy (reduces to value*cov_t when
      # baseline coverage is 0). value is a PROPORTIONAL REDUCTION already.
      e_adj <- apply_coverage_adjustment(
        effect_size = r[[value_col]],
        coverage_t  = cov_t,
        coverage_0  = fair_baseline_coverage
      )

      # Population-level effect on the rate for this bundle/cause/metric.
      es <- r$affected_proportion * e_adj
      es[!is.finite(es)] <- 0
      es <- pmin(pmax(es, 0), 1)   # keep in [0,1]

      # Restrict to the affected cause, age band, targeted sex and active years.
      gender_ok <- if (identical(r$gender, "Both")) rep(TRUE, nrow(dt)) else
        dt$sex == r$gender
      mask <- dt$cause == r$cause &
        dt$age  >= r$age_start &
        dt$age  <= r$age_stop &
        dt$year >= fair_start_year &
        gender_ok
      es[!mask] <- 0

      dt[, (surv_col) := get(surv_col) * (1 - es)]
    }
    invisible(NULL)
  }

  apply_metric("mort_eff", "fair_surv_cf")   # Mortality -> case fatality
  apply_metric("inc_eff",  "fair_surv_ir")   # Incidence -> incidence

  # Apply combined surviving fractions to the rates and tracking ratios.
  dt[, `:=`(
    CF     = CF     * fair_surv_cf,
    IR     = IR     * fair_surv_ir,
    eff_cf = eff_cf * fair_surv_cf,
    eff_ir = eff_ir * fair_surv_ir
  )]

  # Clamp like the rest of the model: rates in [0, 0.99]; never NA / out of range.
  dt[CF > 0.99, CF := 0.99]
  dt[IR > 0.99, IR := 0.99]
  dt[CF < 0, CF := 0]
  dt[IR < 0, IR := 0]

  dt[, c("fair_surv_cf", "fair_surv_ir") := NULL]

  if (dt[, any(is.na(CF))] || dt[, any(is.na(IR))]) {
    stop("FAIR computation produced NA values in CF or IR.", call. = FALSE)
  }

  setorder(dt, year, sex, location, cause, age)

  cat("    Bundles applied:", paste(unique(params$bundle), collapse = "; "), "\n")
  cat("    Coverage scales 0 (", fair_start_year, ") -> ",
      fair_target_coverage, " (", fair_target_year, "); baseline cov = ",
      fair_baseline_coverage, "\n", sep = "")

  dt[]
}

#...........................................................
# FAIR-Choices WORKBOOK-driven impact (Model 04 catalogue) ----
#...........................................................
# Applies the validated per-link effect rows built in Model 04
# (fair_scenarios[[scenario]]$fair_effect_rows) to incidence (IR / eff_ir) and
# case fatality (CF / eff_cf). This is the workbook-driven analogue of
# calculate_fair_cvd_impact(): every parameter (effect_value, affected_fraction,
# baseline/target coverage, start/target year, age band, sex, and the mapped
# model transition) is row-specific and comes from indonesia_model_inputs.xlsx.
# Coverage uses the row's ABSOLUTE path (baseline -> target) fed to the existing
# apply_coverage_adjustment() FAIR formula. Multiple rows acting on the same
# cause/transition combine MULTIPLICATIVELY on the surviving fraction
# (order-invariant). NO new Markov states: workbook sick_hf / sick_severe rows
# were already collapsed onto "sick" via affected_fraction in Model 04.
calculate_fair_workbook_impact <- function(intervention_rates, Country, effect_rows) {

  cat("  - Calculating FAIR-Choices (workbook) package impact\n")

  if (is.null(effect_rows) || nrow(effect_rows) == 0) {
    cat("    (no effect rows supplied; rates returned unchanged)\n")
    return(intervention_rates[])
  }

  dt <- copy(intervention_rates)
  er <- as.data.table(copy(effect_rows))

  present <- unique(dt$cause)
  miss    <- setdiff(unique(er$cause_code), present)
  if (length(miss) > 0)
    cat("    FAIR wb: mapped cause(s) not present in rates, skipped:",
        paste(miss, collapse = ", "), "\n")
  er <- er[cause_code %in% present]

  # Surviving fractions (1 = no effect); accumulate (1 - effect) products.
  dt[, `:=`(fair_surv_ir = 1, fair_surv_cf = 1)]

  for (i in seq_len(nrow(er))) {
    r <- er[i]

    base_cov <- r$baseline_coverage
    tgt_cov  <- r$target_coverage
    sy       <- r$start_year
    ty       <- r$target_year

    # Absolute coverage path: baseline before start; linear baseline -> target
    # between start and target (inclusive, "no delay"); target thereafter.
    span  <- max(ty - sy + 1, 1)
    frac  <- pmin(pmax((dt$year - sy + 1) / span, 0), 1)
    cov_t <- base_cov + (tgt_cov - base_cov) * frac
    cov_t[dt$year <  sy] <- base_cov
    cov_t[dt$year >  ty] <- tgt_cov
    cov_t <- pmin(pmax(cov_t, 0), 1)

    # FAIR coverage-adjusted effect, then the affected fraction.
    e_adj <- apply_coverage_adjustment(effect_size = r$effect_value,
                                       coverage_t  = cov_t,
                                       coverage_0  = base_cov)
    es <- r$affected_fraction * e_adj
    es[!is.finite(es)] <- 0
    es <- pmin(pmax(es, 0), 1)

    gender_ok <- if (identical(r$sex, "Both")) rep(TRUE, nrow(dt)) else dt$sex == r$sex
    mask <- dt$cause == r$cause_code &
            dt$age  >= r$age_start &
            dt$age  <= r$age_stop &
            dt$year >= sy &
            gender_ok
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

  dt[, `:=`(
    CF     = CF     * fair_surv_cf,
    IR     = IR     * fair_surv_ir,
    eff_cf = eff_cf * fair_surv_cf,
    eff_ir = eff_ir * fair_surv_ir
  )]

  dt[CF > 0.99, CF := 0.99]
  dt[IR > 0.99, IR := 0.99]
  dt[CF < 0, CF := 0]
  dt[IR < 0, IR := 0]
  dt[, c("fair_surv_ir", "fair_surv_cf") := NULL]

  if (dt[, any(is.na(CF))] || dt[, any(is.na(IR))])
    stop("FAIR workbook computation produced NA in CF or IR.", call. = FALSE)

  setorder(dt, year, sex, location, cause, age)
  cat("    Applied", nrow(er), "workbook effect row(s) across cause(s):",
      paste(sort(unique(er$cause_code)), collapse = ", "), "\n")
  dt[]
}

#...........................................................
# PUBLIC-HEALTH WORKBOOK-driven transition helpers (M12/M16/M17) ----
#...........................................................
# Small, testable functions kept separate from the apply loop so magnitude
# (full-effect RR), timing (Jha / exponential lag) and transition application
# (rate-ratio -> annual probability) are each isolated and unit-testable.

# M17: convert a RATE ratio to an annual transition PROBABILITY. In this model IR
# (well->sick) and CF (sick->dead) are ANNUAL PROBABILITIES (Model 05 rebalances
# so BG.mx+covid.mx+IR<=1 and ...+CF<=1; Model 06 uses newcases=well*IR and
# deaths=sick*CF), so a rate ratio rr acts through the continuous-rate scale:
#   p1 = 1 - (1 - p0)^rr    (NEVER p0*rr or p0*(1-effect)).
rate_ratio_to_probability <- function(p0, rr) {
  if (any(!is.finite(p0)) || any(p0 < -1e-9 | p0 > 1 + 1e-9))
    stop("rate_ratio_to_probability: p0 must be a probability in [0,1].", call. = FALSE)
  if (any(!is.finite(rr)) || any(rr < 0))
    stop("rate_ratio_to_probability: rr must be finite and >= 0.", call. = FALSE)
  p0 <- pmin(pmax(p0, 0), 1)
  1 - (1 - p0)^rr
}

# Normalized-exponential SENSITIVITY timing (B7): fraction of full effect accrued
# for a quitting cohort that has been quit `years_since_cessation` years,
# normalized to 1 at full_effect_year (default 10). Monotone, in [0,1].
tobacco_lag_fraction <- function(years_since_cessation, lag_rate = 0.0616, full_effect_year = 10L) {
  if (!is.finite(lag_rate) || lag_rate <= 0 || lag_rate > 1)
    stop("tobacco_lag_fraction: lag_rate must be in (0,1].", call. = FALSE)
  if (!is.finite(full_effect_year) || full_effect_year <= 0)
    stop("tobacco_lag_fraction: full_effect_year must be > 0.", call. = FALSE)
  years       <- pmax(years_since_cessation, 0)
  denominator <- 1 - (1 - lag_rate)^full_effect_year
  fraction    <- (1 - (1 - lag_rate)^years) / denominator
  pmin(pmax(fraction, 0), 1)
}

# Age -> Jha band index lookup using the config's band definitions.
.tobacco_band_index <- function(age, bands) {
  idx <- rep(NA_integer_, length(age))
  for (b in seq_len(nrow(bands)))
    idx[age >= bands$age_lo[b] & age <= bands$age_hi[b]] <- b
  idx
}

# Build the effective tobacco rate ratio RR_effective by (sex, single-year age,
# year) for ONE effect row, combining the full-effect RR with the cohort-weighted
# timing scalar (M12) and, for mortality, the M16 residual-risk model. Returns a
# data.table(sex, age, year, rr_eff). Separates magnitude from timing:
#   RR_effective = 1 - lambda_bar * (1 - RR_full).
calculate_tobacco_transition_effects <- function(expo_by_year, yrs, r, cfg) {
  sm    <- cfg$scalar_matrix
  bands <- unique(sm[, .(sex, age_lo, age_hi)])
  p0    <- r$baseline_exposure

  # Annual intervention-attributable quitting cohorts q_u = max(p_{u-1}-p_u, 0)
  # (B6). The year BEFORE the first modeled year is assumed at baseline p0.
  # `ee` is the achieved exposure p_t by year (aligned with yy).
  ord     <- order(yrs)
  yy      <- yrs[ord]; ee <- expo_by_year[ord]
  prev    <- c(p0, ee[-length(ee)])
  q_u     <- pmax(prev - ee, 0)
  names(q_u) <- yy

  base_timing <- identical(cfg$timing_mode, "jha_piecewise_shared_scalar")

  out <- vector("list", nrow(bands))
  for (b in seq_len(nrow(bands))) {
    bs  <- bands$sex[b]; alo <- bands$age_lo[b]; ahi <- bands$age_hi[b]
    lam_lt3  <- sm[sex == bs & age_lo == alo & duration == "LT3",  lambda][1]
    lam_y39  <- sm[sex == bs & age_lo == alo & duration == "Y3_9", lambda][1]
    lam_ge10 <- sm[sex == bs & age_lo == alo & duration == "GE10", lambda][1]

    # Full-effect RR for this (sex, band), evaluated at the CURRENT-YEAR exposure
    # p_t (vector `ee`), per M08 (incidence) / M16 (case fatality). Using p_t (not
    # the final target) is required so that, combined with lambda_bar normalized by
    # quitting-so-far, RR_effective exactly reproduces the mechanistic incidence /
    # mortality multiplier during the exposure ramp (verified identity).
    if (identical(r$model_transition, "incidence")) {
      RRc <- r$response_value                                   # cause-specific smoking RR (M08)
      RR_full <- (1 + ee * (RRc - 1)) / (1 + p0 * (RRc - 1))    # per-year vector
    } else {                                                    # case_fatality (M16)
      RRc <- if (identical(bs, "Male")) cfg$vasc_rr_male else cfg$vasc_rr_female
      if (!is.finite(RRc)) RRc <- cfg$vasc_rr_pooled            # pooled fallback only
      erd10 <- cfg$erd10_by_band[sex == bs & age_lo == alo, erd10][1]
      if (!is.finite(erd10)) erd10 <- cfg$erd10_pooled
      RRq     <- 1 + (1 - erd10) * (RRc - 1)                    # residual RR at 10+ yrs cessation
      RR_full <- (1 + ee * (RRc - 1) + (p0 - ee) * (RRq - 1)) / (1 + p0 * (RRc - 1))  # per-year vector
    }

    # Cohort-weighted timing scalar lambda_bar_t for each modeled year.
    lam_bar <- vapply(yy, function(t) {
      idx <- which(yy <= t)
      if (!length(idx)) return(0)
      qw <- q_u[idx]; denom <- sum(qw)
      if (denom <= 0) return(0)
      k <- t - yy[idx]                                          # years since cessation per cohort
      if (base_timing) {
        lam_k <- ifelse(k < 3, lam_lt3, ifelse(k <= 9, lam_y39, lam_ge10))
      } else {
        lam_k <- tobacco_lag_fraction(k, cfg$lag_rate, cfg$full_effect_year)
      }
      sum(qw * lam_k) / denom
    }, numeric(1))

    rr_eff_year <- 1 - lam_bar * (1 - RR_full)                  # RR_effective per year
    # Expand this band to single-year ages within the row's applicable age range.
    # Join rr_eff BY YEAR (order-independent) rather than rep()-ing into a CJ,
    # so the per-year value always lands on the correct (age, year) cell.
    a_lo <- max(alo, r$age_start); a_hi <- min(ahi, r$age_stop)
    if (a_lo > a_hi) next
    yr_tab <- data.table(year = yy, rr_eff = rr_eff_year)
    out[[b]] <- CJ(sex = bs, age = a_lo:a_hi, year = yy)[yr_tab, on = "year", rr_eff := i.rr_eff][]
  }
  rbindlist(out, use.names = TRUE)
}

#...........................................................
# PUBLIC-HEALTH WORKBOOK-driven impact (Model 04 PH catalogue) ----
#...........................................................
# Applies the validated public-health effect rows built by Model 04
# (public_health_scenarios[[scenario]]$ph_effect_rows) to BOTH modeled
# transitions -- well -> sick (incidence, IR/eff_ir) AND sick -> dead (case
# fatality, CF/eff_cf) -- mirroring the clinical calculate_fair_workbook_impact()
# dispatch. Every row is applied on the COMPLETE key (cause, age, sex, year,
# model_transition) so a well->sick effect can never touch sick->dead.
#
# For each effect row and model year t:
#   1. achieved exposure pt(t): linear baseline -> target over [start,target];
#   2. tobacco-CVD rows (jha_piecewise_shared_scalar / normalized_exponential_lag):
#      RR_effective = 1 - lambda_bar*(1-RR_full), with RR_full = M08 (incidence)
#      or M16 (sick->dead, sex/age-specific vascular mortality) and lambda_bar the
#      cohort-weighted Jha (or exponential) timing scalar -- age/sex/year-specific;
#   3. other rows keep the exposure-path proportional effect and are expressed as
#      a rate ratio RR = 1 - realized (log-linear/immediate, tobacco-T2DM proxy
#      delayed-exponential, optional TFA PAF);
#   4. rate ratios COMBINE MULTIPLICATIVELY per transition, then the baseline
#      probability is converted ONCE via M17 (rate_ratio_to_probability). This is
#      the only change to the incidence application vs the legacy p*RR form; for
#      small annual probabilities the two agree to <0.1%.
# Public-health effects act on incidence AND case fatality (workbook contract).
calculate_public_health_workbook_impact <- function(intervention_rates, Country,
                                                    effect_rows, tobacco_config = NULL) {

  cat("  - Calculating public-health (workbook) policy impact\n")

  if (is.null(effect_rows) || nrow(effect_rows) == 0) {
    cat("    (no public-health effect rows supplied; rates returned unchanged)\n")
    return(intervention_rates[])
  }

  # Exposure-tracking proportional reduction for the NON-tobacco-CVD rows
  # (log-linear/immediate, delayed-exponential proxy, optional TFA PAF).
  ph_effect_from_exposure <- function(model, p0, pt, RR, paf) {
    if (identical(model, "direct_smoking_prevalence_shift_rr"))
      return(1 - (1 + pt * (RR - 1)) / (1 + p0 * (RR - 1)))
    if (identical(model, "direct_loglinear_rr_per_unit_reduction"))
      return(1 - 1 / (RR ^ (p0 - pt)))
    if (grepl("^tfa_attributable_ihd_PAF", model))
      return(rep(ifelse(is.na(paf), 0, paf) * ifelse(is.na(RR), 0, RR), length(pt)))
    stop("PH wb: unsupported effect_model '", model, "'.", call. = FALSE)
  }

  dt <- copy(intervention_rates)
  er <- as.data.table(copy(effect_rows))

  present <- unique(dt$cause)
  miss    <- setdiff(unique(er$cause_code), present)
  if (length(miss) > 0)
    cat("    PH wb: mapped cause(s) not present in rates, skipped:",
        paste(miss, collapse = ", "), "\n")
  er <- er[cause_code %in% present]

  yrs <- sort(unique(dt$year))

  # Combined RATE RATIOS per transition (1 = no effect). Effects accumulate
  # multiplicatively on the rate-ratio scale; the baseline probability is
  # converted to a new probability ONCE, after the loop (M17).
  dt[, `:=`(ph_rr_ir = 1, ph_rr_cf = 1)]

  n_cf_applied <- 0L; n_ir_applied <- 0L; n_age80_hits <- 0L
  tobacco_lags <- c("jha_piecewise_shared_scalar", "normalized_exponential_lag")

  for (i in seq_len(nrow(er))) {
    r <- er[i]

    # Guard: no valid transition should reach here (Model 04 drops them), but
    # fail loud rather than silently skip a sick->dead link.
    if (!r$model_transition %in% c("incidence", "case_fatality"))
      stop("PH wb: link ", r$intervention_cause_key, " has unmapped model_transition '",
           r$model_transition, "'.", call. = FALSE)

    p0   <- r$baseline_exposure; ptgt <- r$target_exposure
    flr  <- r$exposure_floor;    sy   <- r$start_year; ty <- r$target_year

    # Annual achieved exposure path pt(t) over the modeled years.
    span   <- max(ty - sy + 1, 1)
    fracy  <- pmin(pmax((yrs - sy + 1) / span, 0), 1)
    expo_y <- p0 + (ptgt - p0) * fracy
    expo_y[yrs < sy] <- p0
    expo_y[yrs > ty] <- ptgt
    if (!is.na(flr)) expo_y <- pmax(expo_y, flr)

    is_tobacco_cvd <- (r$lag_model %in% tobacco_lags) && !is.null(tobacco_config) &&
                      !is.null(tobacco_config$scalar_matrix)

    if (is_tobacco_cvd) {
      # ---- Jha (or exponential) cohort-timed tobacco-CVD effect (M12/M08/M16) --
      eff_tab <- calculate_tobacco_transition_effects(expo_y, yrs, r, tobacco_config)
      if (nrow(eff_tab)) {
        # rr_eff is defined on this row's cause only; join by (sex, age, year).
        col <- if (identical(r$model_transition, "case_fatality")) "ph_rr_cf" else "ph_rr_ir"
        et <- eff_tab[, .(sex, age, year, rr_eff)]
        dt[et, on = .(sex, age, year),
           (col) := get(col) * fifelse(cause == r$cause_code & is.finite(i.rr_eff), i.rr_eff, 1)]
        n_age80_hits <- n_age80_hits + nrow(et[age >= 80])
        if (identical(r$model_transition, "case_fatality")) n_cf_applied <- n_cf_applied + 1L
        else n_ir_applied <- n_ir_applied + 1L
      }
    } else {
      # ---- Exposure-path proportional effect for the other rows ---------------
      RR  <- r$response_value; paf <- r$paf_value; Efull <- r$full_effect_at_target
      # realized proportional reduction per modeled year
      if (identical(r$lag_model, "immediate_after_full_implementation")) {
        realized_y <- ph_effect_from_exposure(r$effect_model, p0, expo_y, RR, paf)
      } else if (identical(r$lag_model, "delayed_exponential_remaining_effect")) {
        yrs_since  <- pmax(0, yrs - sy)
        realized_y <- Efull * (1 - (1 - r$lag_parameter) ^ yrs_since)
      } else {
        stop("PH wb: unsupported lag_model '", r$lag_model, "' for link ",
             r$intervention_cause_key, call. = FALSE)
      }
      realized_y[yrs < sy] <- 0
      realized_y[!is.finite(realized_y)] <- 0
      realized_y <- pmin(pmax(realized_y, 0), 1)
      rr_eff_y   <- 1 - realized_y                              # express as a rate ratio
      ry <- data.table(year = yrs, rr_eff = rr_eff_y)

      gender_ok <- identical(r$sex, "Both")
      col <- if (identical(r$model_transition, "case_fatality")) "ph_rr_cf" else "ph_rr_ir"
      dt[ry, on = .(year),
         (col) := get(col) * fifelse(
           cause == r$cause_code & age >= r$age_start & age <= r$age_stop &
             (gender_ok | sex == r$sex) & is.finite(i.rr_eff), i.rr_eff, 1)]
      if (identical(r$model_transition, "case_fatality")) n_cf_applied <- n_cf_applied + 1L
      else n_ir_applied <- n_ir_applied + 1L
    }
  }

  # ---- Convert combined rate ratios to probabilities ONCE (M17) --------------
  dt[, `:=`(IR_pre_ph = IR, CF_pre_ph = CF)]
  dt[, IR := rate_ratio_to_probability(IR, ph_rr_ir)]
  dt[, CF := rate_ratio_to_probability(CF, ph_rr_cf)]
  # Track the public-health surviving fraction into eff_ir/eff_cf so PH stacks
  # with any clinical effects already applied (order-invariant, multiplicative).
  dt[, eff_ir := eff_ir * fifelse(IR_pre_ph > 0, IR / IR_pre_ph, 1)]
  dt[, eff_cf := eff_cf * fifelse(CF_pre_ph > 0, CF / CF_pre_ph, 1)]

  dt[IR > 0.99, IR := 0.99]; dt[IR < 0, IR := 0]
  dt[CF > 0.99, CF := 0.99]; dt[CF < 0, CF := 0]
  dt[, c("ph_rr_ir", "ph_rr_cf", "IR_pre_ph", "CF_pre_ph") := NULL]

  if (dt[, any(is.na(IR))] || dt[, any(is.na(CF))])
    stop("PH workbook computation produced NA in IR or CF.", call. = FALSE)

  setorder(dt, year, sex, location, cause, age)
  cat(sprintf("    Applied %d PH effect row(s): %d incidence, %d case-fatality; cause(s): %s\n",
      nrow(er), n_ir_applied, n_cf_applied, paste(sort(unique(er$cause_code)), collapse = ", ")))
  if (n_cf_applied > 0L && n_age80_hits > 0L)
    cat(sprintf("    (tobacco sick->dead: ages 80-95 use the documented 60-79 Jha scalars)\n"))
  dt[]
}

#...........................................................
# Model. Project.all function ----
#...........................................................
#

# #Test
# Country <-"China"
# saltmet <-"percent"
# salteff <- 0.3
# saltyear1 <- 2025
# saltyear2 <- 2030
# drugcov <- "p75"
# intervention <- "sodium"
# interventions <- c("statins","tfa")
#
# baseline_ctrl  <- 0.1585683
# #baseline_ctrl  <- 0
# target_control <- 0.5
#
# control_start_year  <- 2025
# control_target_year <- 2030
#
# coverage_0 <- baseline_ctrl
# target_year <- control_target_year
# start_year <- control_start_year
#
#
# tfa
# tfa_target_tfa        <- 0         # target % energy from TFA
# tfa_policy_start_year <- 2026      # flexible start year

# baseline_statin_coverage <- 0.04737402
# statin_target_coverage <- 0.60
# statin_start_year      <- 2025
# statin_target_year     <- 2050
# adherence <- 1
# prop_athero_stroke <- 0.6

project.all <- function(Country, 
                        interventions = c("antihypertensive", "sodium", "tfa", "statins"),
                        #explicit hypertension control parameters
                        target_control,
                        control_start_year,
                        control_target_year,
                        # explicit Statins params
                        statin_target_coverage,
                        statin_start_year,
                        statin_target_year,
                        adherence_ir = adherence_ir,
                        adherence_cf = adherence_cf,
                        #explicit sodium reduction parameters
                        saltmet = "percent", 
                        salteff = 0.3, 
                        saltyear1 = 2026, 
                        saltyear2 = 2030,
                        # explicit TFA policy parameters (NEW)
                        tfa_target_tfa        = 0,      # target %E from TFA
                        tfa_policy_start_year = 2027,   # first year policy is active
                        #Implicit hypertension control parameters
                        drugcov = "p75",  ## this not binding but keep temporally
                        baseline_ctrl = NULL,   # use provided or extract from dt_hbp_control
                        dt_hbp_targets = NULL, # optional data.table with country-specific HTN control targets (location, htncov2_2030)
                        htn_target_col = "htncov2_aspirational",  # NEW: dynamic column
                        target_control_diabetes = 0.80,   # NEW
                        baseline_ctrl_diabetes  = NULL,   # NEW — inferred from dt_hbp_control
                        baseline_statin_coverage = NULL,  # implicit statins parameters
                        # FAIR-Choices CVD package parameters (defaults from
                        # taxonomy_effect_size.xlsx; ALL overridable here)
                        fair_params            = NULL,  # NULL -> default dt_fair_params
                        fair_bundles           = NULL,  # NULL -> full default CVD package
                        fair_start_year        = 2026,
                        fair_target_year       = 2050,
                        fair_target_coverage   = 0.80,
                        fair_baseline_coverage = 0,
                        fair_bundle_coverage   = NULL,  # named per-bundle coverage overrides
                        # FAIR-Choices WORKBOOK effect rows (validated Model 04
                        # catalogue); drives the "fair_wb" intervention.
                        fair_effect_rows       = NULL,
                        # PUBLIC-HEALTH WORKBOOK effect rows (validated Model 04 PH
                        # catalogue); drives the "ph_wb" intervention.
                        ph_effect_rows         = NULL,
                        # Parsed tobacco Jha timing / vascular-mortality config
                        # (public_health_inputs$tobacco_effect_config from Model 04);
                        # required for the tobacco sick->dead and Jha-timed rows.
                        ph_tobacco_config      = NULL
) {

  cat("\n========================================\n")
  cat("STARTING PROJECTION FOR:", Country, "\n")
  cat("Interventions:", paste(interventions, collapse = ", "), "\n")
  cat("========================================\n\n")
  
  # Validate interventions
  valid_interventions <- c("antihypertensive", "antihypertensive_diabetes", "sodium", "tfa", "statins", "fair_cvd", "fair_wb", "ph_wb")

  if (!all(interventions %in% valid_interventions)) {
    stop("Invalid intervention(s). Must be one or more of: ", 
         paste(valid_interventions, collapse = ", "))
  }
  
  # Preliminaries
  base_rates <- b_rates[location == Country & year>= 2017]
  
  # Get BP distribution data
  DT <- unique(data.in[location == Country][, Year := 2017][, -c("Lower95", "Upper95")])
  DT.in <- as.data.table(left_join(
    DT[rep(seq(1, nrow(DT)), 34)][, Year := repYear(.I)], 
    inc %>% select(-location), 
    by = c("iso3", "Year")
  ))
  
  #...............................................................
  # Baseline for htn control
  
  if (!is.null(baseline_ctrl)) {
    baseline_ctrl_loc <- baseline_ctrl
  } else {
    baseline_ctrl_loc <- dt_hbp_control[
      year == 2024 & location == Country,
      mean(baseline_ctrl, na.rm = TRUE)
    ]
  }
  baseline_ctrl_loc <- max(min(baseline_ctrl_loc, 1), 0)
  
  # Force all AROC-related variables in DT.in to zero (no checks)
  
  DT.in[, c("aroc",
            "aroc2",
            "p_change",
            "p_change2",
            "a_change",
            "a_change2",
            "ideal",
            "drugaroc") := 0]
  
  #...............................................................
  # specific targets for htn control
  
  # Country-specific HTN target 
  # Priority: country-specific target > user-defined target > default (0.50)
  
  if (!is.null(dt_hbp_targets) &&
      Country %in% dt_hbp_targets$location &&
      htn_target_col %in% names(dt_hbp_targets)) {
    
    target_control <- dt_hbp_targets[location == Country, get(htn_target_col)]
    cat("  HTN target: country-specific [", htn_target_col, "] =",
        round(target_control, 4), "\n")
    
  } else {
    # target_control keeps whatever was passed in
    cat("  HTN target: user-defined =", round(target_control, 4), "\n")
  }
  
  # Existing general population baseline
  baseline_ctrl_loc <- dt_hbp_control[
    year == 2024 & location == Country,
    mean(baseline_ctrl, na.rm = TRUE)
  ]
  baseline_ctrl_loc <- max(min(baseline_ctrl_loc, 1), 0)
  
  # NEW: diabetes-specific baseline
  if (!is.null(baseline_ctrl_diabetes)) {
    baseline_ctrl_diab <- baseline_ctrl_diabetes
  } else {
    baseline_ctrl_diab <- dt_hbp_targets[
      location == Country,
      mean(htn_ctrl_diabetes, na.rm = TRUE)
    ]
  }
  # Fall back to general baseline if missing
  if (is.na(baseline_ctrl_diab)) baseline_ctrl_diab <- baseline_ctrl_loc
  baseline_ctrl_diab <- max(min(baseline_ctrl_diab, 1), 0)
  
  #...............................................................
  # Baseline for sodium intervention
  
  DT.in.sodium <- copy(DT.in)
  
  #...............................................................
  # Baseline for statins intervention
  
  # Determine baseline statin coverage
  if (!is.null(baseline_statin_coverage)) {
    
    baseline_statin_cov <- baseline_statin_coverage
    
  } else {
    
    baseline_statin_cov <- dt_statin_scenarios[
      location == Country & year == 2024,
      mean(statins_current, na.rm = TRUE)
    ]
  }
  
  # Clamp to [0,1]
  baseline_statin_cov <- max(min(baseline_statin_cov, 1), 0)
  
  #...............................................................
  # Initialize baseline scenario
  intervention_rates <- copy(base_rates)
  intervention_rates[, `:=`(
    eff_ir = 1,
    eff_cf = 1,
    intervention = "baseline"
  )]
  
  # Store baseline for combining BP-related interventions
  intervention_rates_bau <- copy(intervention_rates)
  
  # Track which interventions have been applied
  applied_interventions <- character()
  intervention_label <- "baseline"
  
  # Store individual intervention effects for multiplicative combination
  intervention_effects <- list()
  
  #..................................
  ## Apply Antihypertensive Intervention ----
  #..................................
  
  if ("antihypertensive" %in% interventions) {
    cat("\n=== Applying Antihypertensive Therapy ===\n")
    
    # Calculate with treatment using new method
    intervention_rates_drug <- calculate_antihypertensive_impact_etihad(
      intervention_rates_bau, 
      Country, 
      DT.in, 
      dt_gbd_rr,
      # NEW PARAMETERIZATION
      target_control      = target_control,
      baseline_ctrl       = baseline_ctrl_loc,
      drugcov             = drugcov,
      start_year          = control_start_year,
      target_year         = control_target_year
    )
    
    # Store effects
    intervention_effects[["antihypertensive"]] <- 
      intervention_rates_drug[, .(age, sex, location, cause, year, 
                                  eff_ir_bp = eff_ir, eff_cf_bp = eff_cf)]
    
    applied_interventions <- c(applied_interventions, "BP")
  }
  
  #..................................
  ## Apply Antihypertensive-Diabetes Intervention ----
  #..................................
  
  if ("antihypertensive_diabetes" %in% interventions) {
    cat("\n=== Applying Antihypertensive Therapy (Diabetes Subgroup) ===\n")
    
    intervention_rates_diab <- calculate_antihypertensive_diabetes(
      intervention_rates_bau,
      Country,
      DT.in,
      dt_gbd_rr,
      target_control_diabetes = target_control_diabetes,
      baseline_ctrl_diabetes  = baseline_ctrl_diab,
      start_year              = control_start_year,
      target_year             = control_target_year
    )
    
    intervention_effects[["antihypertensive_diabetes"]] <-
      intervention_rates_diab[, .(age, sex, location, cause, year,
                                  eff_ir_bp_diab = eff_ir,
                                  eff_cf_bp_diab = eff_cf)]
    
    applied_interventions <- c(applied_interventions, "BP_diabetes")
  }
  
  #..................................
  ## Apply Sodium Intervention ----
  #..................................
  
  if ("sodium" %in% interventions) {
    cat("\n=== Applying Sodium Intervention ===\n")
    
    intervention_rates_sodium <- calculate_sodium_impact_etihad(
      intervention_rates_bau, Country, DT.in.sodium, salteff, saltmet,
      saltyear1, saltyear2, dt_gbd_rr)
    
    # Store effects
    intervention_effects[["sodium"]] <- 
      intervention_rates_sodium[, .(age, sex, location, cause, year, 
                                    eff_ir_salt = eff_ir, eff_cf_salt = eff_cf)]
    
    applied_interventions <- c(applied_interventions, "Salt")
  }
  
  #..................................
  ## Combine BP-related interventions ----
  ## (Antihypertensive + Antihypertensive Diabetes + Sodium)
  #..................................
  
  if (length(intervention_effects) > 0) {
    cat("\n=== Combining BP-related intervention effects ===\n")
    
    # Start with baseline
    intervention_rates <- copy(intervention_rates_bau)
    
    # Merge all BP-related effects into intervention_rates
    for (int_name in names(intervention_effects)) {
      intervention_rates <- merge(
        intervention_rates,
        intervention_effects[[int_name]],
        by = c("age", "sex", "location", "cause", "year"),
        all.x = TRUE
      )
    }
    
    # Handle missing values before combining (e.g. small countries, unmatched rows)
    # Any missing effect column defaults to 1 (no effect)
    effect_cols_all <- grep("^eff_(ir|cf)_", names(intervention_rates), value = TRUE)
    for (col in effect_cols_all) {
      intervention_rates[is.na(get(col)), (col) := 1]
    }
    
    # Dynamically collect effect columns per metric
    # Each intervention stores its effects as eff_ir_* and eff_cf_*
    ir_cols <- grep("^eff_ir_", names(intervention_rates), value = TRUE)
    cf_cols <- grep("^eff_cf_", names(intervention_rates), value = TRUE)
    
    cat("  Combining IR effects:", paste(ir_cols, collapse = " × "), "\n")
    cat("  Combining CF effects:", paste(cf_cols, collapse = " × "), "\n")
    
    # Multiplicative combination across all active interventions
    intervention_rates[, eff_ir := Reduce(`*`, .SD), .SDcols = ir_cols]
    intervention_rates[, eff_cf := Reduce(`*`, .SD), .SDcols = cf_cols]
    
    # Apply combined effects to rates
    intervention_rates[, `:=`(
      CF = CF * eff_cf,
      IR = IR * eff_ir
    )]
    
    # Clean up all temporary effect columns
    intervention_rates[, (effect_cols_all) := NULL]
    
    cat("  Combined effects applied to CF and IR\n")
  }
  
  #..................................
  ## Apply TFA Intervention ----
  #..................................
  
  if ("tfa" %in% interventions) {
    cat("\n=== Applying TFA Intervention ===\n")
    
    intervention_rates <- calculate_tfa_impact(
      dt_tfa_scenarios      = dt_tfa_scenarios,
      intervention_rates    = intervention_rates,
      Country               = Country,
      target_tfa            = tfa_target_tfa,
      policy_start_year     = tfa_policy_start_year
    )
    
    applied_interventions <- c(applied_interventions, "TFA")
  }
  
  #..................................
  ## Apply Statins Intervention ----
  #..................................
  
  if ("statins" %in% interventions) {
    cat("\n=== Applying Statins Intervention ===\n")
    
    intervention_rates <- calculate_statins_impact(
      dt_statin_scenarios,
      intervention_rates,
      Country,
      dt_af_statins,
      adherence_ir = adherence_ir,
      adherence_cf = adherence_cf,
      prop_athero_stroke     = 0.60,
      statin_target_coverage = statin_target_coverage,
      statin_start_year      = statin_start_year,
      statin_target_year     = statin_target_year,
      # you can also pass baseline_statin_coverage = some_value if needed
      baseline_statin_coverage  = baseline_statin_cov
    )
    
    applied_interventions <- c(applied_interventions, "Statins")
  }

  #..................................
  ## Apply FAIR-Choices CVD Package ----
  ## (acts directly on rates, like TFA/statins, in the post-BP stage)
  #..................................

  if ("fair_cvd" %in% interventions) {
    cat("\n=== Applying FAIR-Choices CVD Package ===\n")

    intervention_rates <- calculate_fair_cvd_impact(
      intervention_rates     = intervention_rates,
      Country                = Country,
      fair_params            = fair_params,            # NULL -> default dt_fair_params
      fair_bundles           = fair_bundles,           # NULL -> full default CVD package
      fair_start_year        = fair_start_year,
      fair_target_year       = fair_target_year,
      fair_target_coverage   = fair_target_coverage,
      fair_baseline_coverage = fair_baseline_coverage,
      fair_bundle_coverage   = fair_bundle_coverage
    )

    applied_interventions <- c(applied_interventions, "FAIR")
  }

  #..................................
  ## Apply FAIR-Choices WORKBOOK Package (Model 04 catalogue) ----
  ## Workbook-driven scenarios use this branch (interventions == "fair_wb").
  #..................................

  if ("fair_wb" %in% interventions) {
    cat("\n=== Applying FAIR-Choices (workbook) Package ===\n")

    intervention_rates <- calculate_fair_workbook_impact(
      intervention_rates = intervention_rates,
      Country            = Country,
      effect_rows        = fair_effect_rows
    )

    applied_interventions <- c(applied_interventions, "FAIR")
  }

  #..................................
  ## Apply PUBLIC-HEALTH WORKBOOK Package (Model 04 PH catalogue) ----
  ## Public-health scenarios use this branch (interventions == "ph_wb").
  #..................................

  if ("ph_wb" %in% interventions) {
    cat("\n=== Applying Public-Health (workbook) Package ===\n")

    intervention_rates <- calculate_public_health_workbook_impact(
      intervention_rates = intervention_rates,
      Country            = Country,
      effect_rows        = ph_effect_rows,
      tobacco_config     = ph_tobacco_config
    )

    applied_interventions <- c(applied_interventions, "PublicHealth")
  }

  # Create intervention label
  if (length(applied_interventions) > 0) {
    intervention_label <- paste(applied_interventions, collapse = " + ")
  }
  intervention_rates[, intervention := intervention_label]
  
  #..................................
  ## Initial States ----
  #..................................
  
  cat("\n=== Setting Initial Population States ===\n")
  
  ## Full-lifecycle initial states: seed year 2017 (all ages) and age-0 newborns
  ## (all years). Newborns come from that year's single-age population Nx; for
  ## CVD/dm2 in infants PREVt0 ~ 0, so newborns are almost entirely "well". This
  ## REPLACES the old age-20 exogenous boundary -- there is no age-20 reseed.
  intervention_rates[year == 2017 | age == min_model_age, `:=`(
    sick = Nx * PREVt0,
    dead = Nx * DIS.mx.t0,
    well = Nx * (1 - (PREVt0 + BG.mx)),
    pop = Nx,
    all.mx = Nx * DIS.mx.t0 + Nx * BG.mx
  )]

  intervention_rates[CF > 0.99, CF := 0.99]
  intervention_rates[IR > 0.99, IR := 0.99]

  setorder(intervention_rates, sex, location, cause, intervention, age)

  a_lo <- min_model_age   # 0  : newborn entry age
  a_hi <- max_model_age   # 95 : open-ended terminal (95+) age

  #..................................
  ## STATE TRANSITIONS ----
  #..................................

  cat("\n=== Running State Transition Model ===\n")
  cat("Projecting from 2017 to 2058...\n")

  for(i in 1:41) {
    if (i %% 10 == 0) cat("  Year", 2017 + i, "\n")

    b2 <- intervention_rates[year <= 2017 + i & year >= 2017 + i - 1]
    setorder(b2, sex, location, cause, intervention, age, year)  # shift() reads year yr-1
    b2[, age2 := age + 1]

    b2[, newcases2 := shift(well) * IR,
       by = .(sex, location, cause, age, intervention)]

    b2[, sick2 := shift(sick) * (1 - (CF + BG.mx + covid.mx)) + shift(well) * IR,
       by = .(sex, location, cause, age, intervention)]
    b2[sick2 < 0, sick2 := 0]

    b2[, dead2 := shift(sick) * CF,
       by = .(sex, location, cause, age, intervention)]
    b2[dead2 < 0, dead2 := 0]

    b2[, pop2 := shift(pop) - shift(all.mx),
       by = .(sex, location, cause, age, intervention)]
    b2[pop2 < 0, pop2 := 0]

    b2[, all.mx2 := sum(dead2),
       by = .(sex, location, year, age, intervention)]
    b2[, all.mx2 := all.mx2 + (pop2 * BG.mx.all) + (pop2 * covid.mx)]
    b2[all.mx2 < 0, all.mx2 := 0]

    b2[, well2 := pop2 - all.mx2 - sick2]
    b2[well2 < 0, well2 := 0]

    ## Aged cohorts land at age2 = age+1. Pool age2 > a_hi into the open-ended
    ## terminal group (95+): this both ages survivors from 94 into 95+
    ## (age2 == 95) AND retains survivors already in 95+ (age2 == 96), summing the
    ## count states. Age 0 is not updated here (reseeded as newborns above).
    upd <- b2[year == 2017 + i & age2 > a_lo]
    upd[age2 > a_hi, age2 := a_hi]
    upd <- upd[, .(newcases = sum(newcases2), sick = sum(sick2), dead = sum(dead2),
                   well = sum(well2), pop = sum(pop2), all.mx = sum(all.mx2)),
               by = .(age = age2, year, sex, location, cause, intervention)]

    intervention_rates[upd, on = .(year, age, sex, location, cause, intervention), `:=`(
      newcases = i.newcases,
      sick     = i.sick,
      dead     = i.dead,
      well     = i.well,
      pop      = i.pop,
      all.mx   = i.all.mx
    )]
  }
  
  cat("\n=== Projection Complete ===\n")
  cat("Final intervention label:", intervention_label, "\n\n")
  
  out.df <- intervention_rates[, .(
    age, cause, sex, year, well, sick, newcases,
    dead, pop, all.mx, intervention, location, eff_ir, eff_cf
  )]
  
  return(out.df)
}

# #...........................................................
# # Checking inputs ----
# #...........................................................

# # Check location names (the key to merge))

b_rates[CF>=1, CF:=0.99]
b_rates[IR>=1, IR:=0.99]
b_rates[CF<0, CF:=0]
b_rates[IR<0, IR:=0]

# #...........................................................
# # Example Usage ----
# #...........................................................
# 
# # Run just antihypertensive therapy
# results_bp_only <- project.all(
#   Country = "China",
#   interventions = c("antihypertensive"),
#   drugcov = "p75"
# )
# 
# # Run antihypertensive + statins
# results_bp_statins <- project.all(
#   Country = "China",
#   interventions = c("antihypertensive", "statins"),
#   drugcov = "p75"
# )
# 
# # Run sodium + TFA + statins
# results_sodium_tfa_statins <- project.all(
#   Country = "China",
#   interventions = c("sodium", "tfa", "statins"),
#   saltmet = "percent",
#   salteff = 0.3,
#   saltyear1 = 2025,
#   saltyear2 = 2030
# )
# 
# ##Run all four interventions
# results_all <- project.all(
#   Country = "China",
#   interventions = c("antihypertensive", "sodium", "tfa", "statins"),
#   saltmet = "percent",
#   salteff = 0.3,
#   saltyear1 = 2025,
#   saltyear2 = 2030,
#   drugcov = "p75"
# )
# # 
# # # Run baseline (no interventions) - useful for comparison
# results_baseline <- project.all(
#   Country = "China",
#   interventions = character(0)  # Empty vector = no interventions
# )

#...........................................................
## Batch Runner for Multiple Scenarios ----
#...........................................................

run_multiple_scenarios <- function(Country,
                                   scenario_list,
                                   target_control,
                                   control_start_year,
                                   control_target_year,
                                   statin_target_coverage,
                                   statin_start_year,
                                   statin_target_year,
                                   adherence_ir = 1,
                                   adherence_cf = 1,
                                   saltmet = "percent",
                                   salteff = 0.3,
                                   saltyear1 = 2026,
                                   saltyear2 = 2030,
                                   tfa_target_tfa        = 0,
                                   tfa_policy_start_year = 2027,
                                   drugcov = "p75",
                                   baseline_ctrl = NULL,
                                   dt_hbp_targets = NULL,
                                   htn_target_col = "htncov2_aspirational",  # NEW
                                   target_control_diabetes = 0.80,
                                   baseline_ctrl_diabetes  = NULL,
                                   baseline_statin_coverage = NULL,
                                   # FAIR-Choices CVD package parameters (overridable)
                                   fair_params            = NULL,
                                   fair_bundles           = NULL,
                                   fair_start_year        = 2026,
                                   fair_target_year       = 2050,
                                   fair_target_coverage   = 0.80,
                                   fair_baseline_coverage = 0,
                                   fair_bundle_coverage   = NULL,
                                   # Parsed tobacco Jha timing / mortality config (Model 04);
                                   # threaded to every public-health scenario run.
                                   ph_tobacco_config      = NULL) {

  results <- list()
  
  for (scenario_name in names(scenario_list)) {
    cat("\n##########################################\n")
    cat("RUNNING SCENARIO:", scenario_name, "\n")
    cat("##########################################\n")

    # Scenario entries may be either a legacy character vector of intervention
    # tokens, OR a richer list (workbook-driven, from Model 04) carrying
    # $interventions, $fair_effect_rows (clinical) and/or $ph_effect_rows
    # (public health) plus a $family trace tag. Support both.
    entry <- scenario_list[[scenario_name]]
    if (is.list(entry) && !is.null(entry$interventions)) {
      ints_arg <- entry$interventions
      fer_arg  <- entry$fair_effect_rows
      per_arg  <- entry$ph_effect_rows
      fam_arg  <- if (!is.null(entry$family)) entry$family else NA_character_
      role_arg <- if (!is.null(entry$scenario_role)) entry$scenario_role else NA_character_
      ppid_arg <- if (!is.null(entry$parent_package_id)) entry$parent_package_id else NA_character_
      lvl_arg  <- if (!is.null(entry$scenario_level)) entry$scenario_level else NA_character_
    } else {
      ints_arg <- entry
      fer_arg  <- NULL
      per_arg  <- NULL
      fam_arg  <- NA_character_
      role_arg <- NA_character_
      ppid_arg <- NA_character_
      lvl_arg  <- NA_character_
    }

    results[[scenario_name]] <- project.all(
      Country             = Country,
      interventions       = ints_arg,
      fair_effect_rows    = fer_arg,
      ph_effect_rows      = per_arg,
      ph_tobacco_config   = ph_tobacco_config,
      target_control      = target_control,
      control_start_year  = control_start_year,
      control_target_year = control_target_year,
      statin_target_coverage   = statin_target_coverage,
      statin_start_year        = statin_start_year,
      statin_target_year       = statin_target_year,
      adherence_ir             = adherence_ir,
      adherence_cf             = adherence_cf,
      saltmet   = saltmet,
      salteff   = salteff,
      saltyear1 = saltyear1,
      saltyear2 = saltyear2,
      tfa_target_tfa        = tfa_target_tfa,
      tfa_policy_start_year = tfa_policy_start_year,
      drugcov        = drugcov,
      baseline_ctrl  = baseline_ctrl,
      dt_hbp_targets = dt_hbp_targets,
      htn_target_col = htn_target_col,          # passed through
      target_control_diabetes = target_control_diabetes,
      baseline_ctrl_diabetes  = baseline_ctrl_diabetes,
      baseline_statin_coverage = baseline_statin_coverage,
      # FAIR-Choices CVD package parameters
      fair_params            = fair_params,
      fair_bundles           = fair_bundles,
      fair_start_year        = fair_start_year,
      fair_target_year       = fair_target_year,
      fair_target_coverage   = fair_target_coverage,
      fair_baseline_coverage = fair_baseline_coverage,
      fair_bundle_coverage   = fair_bundle_coverage
    )

    # Trace the intervention family + public-health hierarchy on every output row
    # for unambiguous downstream selection (baseline / clinical / public_health;
    # scenario_role = standalone / child / package / combined; parent package id).
    if (is.data.frame(results[[scenario_name]])) {
      results[[scenario_name]][, intervention_family := fam_arg]
      results[[scenario_name]][, scenario_role := role_arg]
      results[[scenario_name]][, parent_package_id := ppid_arg]
      results[[scenario_name]][, scenario_level := lvl_arg]
    }
  }

  combined_results <- rbindlist(results, idcol = "scenario")
  return(combined_results)
}

# The three HTN target columns to loop over
#htn_target_cols <- c("htncov2_aspirational", "htncov2_ambitious", "htncov2_progress")
htn_target_cols <- c("htncov2_ambitious")

# # HTN-only scenarios
# scenarios <- list(
#   baseline = character(0),
#   bp_only  = "antihypertensive"
# )

# Scenarios: baseline + antihypertensive only (you can expand this list with more combinations as needed)
# # Example: Run multiple scenarios
scenarios <- list(
  baseline = character(0),
  bp_only = "antihypertensive",
  sodium_only = "sodium",
  tfa_only = "tfa",
  statins_only = "statins",
  bp_sodium = c("antihypertensive", "sodium"),
  bp_sodium_tfa = c("antihypertensive", "sodium", "tfa"),
  all_four = c("antihypertensive", "sodium", "tfa", "statins"),
  # FAIR-Choices CVD package scenarios
  fair_only     = "fair_cvd",
  all_plus_fair = c("antihypertensive", "sodium", "tfa", "statins", "fair_cvd")
)


# 
# all_results <- run_multiple_scenarios(
#   Country = "Indonesia",
#   scenario_list = scenarios,
#   #explicit hypertension control parameters
#   target_control = 0.5,
#   control_start_year = 2026,
#   control_target_year = 2040,
#   # explicit statins parameters
#   statin_target_coverage = 0.60,
#   statin_start_year      = 2026,
#   statin_target_year     = 2050,
#   adherence_ir = 0.575,
#   adherence_cf = 0.664,
#   #explicit sodium reduction parameters
#   saltmet = "percent",
#   salteff = 0.3,
#   saltyear1 = 2026,
#   saltyear2 = 2030,
#   # explicit TFA parameters (NEW)
#   tfa_target_tfa        = 0,    # 0% of energy from TFA
#   tfa_policy_start_year = 2028, # <-- flexible start year (2026 lagged two years)
#   #Implicit hypertension control parameters
#   drugcov = "p75",
#   baseline_ctrl       = 0.04996622,
#   baseline_statin_coverage = NULL,
#   htn_target_col = "htncov2_aspirational",
#   target_control_diabetes = 0.80,
#   dt_hbp_targets = dt_hbp_targets
# )

#...........................................................
## Comparison Helper Functions ----
#...........................................................

compare_scenarios <- function(results_dt, 
                              metric = "dead",
                              years = c(2030, 2040, 2050),
                              reference_scenario = "baseline") {
  #' Compare outcomes across scenarios
  #' 
  #' @param results_dt Data.table with results from run_multiple_scenarios()
  #' @param metric Character, which metric to compare (dead, newcases, sick, etc.)
  #' @param years Numeric vector, which years to compare
  #' @param reference_scenario Character, scenario to use as reference
  #' 
  #' @return Data.table with comparisons
  
  comparison <- results_dt[year %in% years, 
                           .(total = sum(get(metric))),
                           by = .(scenario, year, intervention)]
  
  if (reference_scenario %in% comparison$scenario) {
    ref_values <- comparison[scenario == reference_scenario, 
                             .(year, intervention, ref_total = total)]
    
    comparison <- merge(comparison, ref_values, 
                        by = c("year", "intervention"), 
                        all.x = TRUE)
    
    comparison[, `:=`(
      absolute_difference = total - ref_total,
      percent_change = (total - ref_total) / ref_total * 100,
      averted = ref_total - total
    )]
  }
  
  setorder(comparison, year, scenario)
  return(comparison)
}

# # Example usage:
# deaths_comparison <- compare_scenarios(
#   all_results,
#   metric = "dead",
#   years = c(2030, 2040, 2050),
#   reference_scenario = "baseline"
# )

calculate_cumulative_impact <- function(results_dt,
                                        metric = "dead",
                                        start_year = 2025,
                                        end_year = 2050) {
  #' Calculate cumulative impact over time period
  #' 
  #' @param results_dt Data.table with results from run_multiple_scenarios()
  #' @param metric Character, which metric to sum
  #' @param start_year Numeric, starting year
  #' @param end_year Numeric, ending year
  #' 
  #' @return Data.table with cumulative totals and differences vs baseline
  
  # Compute cumulative totals
  cumulative <- results_dt[year >= start_year & year <= end_year,
                           .(cumulative_total = sum(get(metric))),
                           by = .(scenario, intervention)]
  
  # Get baseline value
  baseline_val <- cumulative[scenario == "baseline", cumulative_total]
  
  # Add difference columns
  cumulative[, diff_vs_baseline := abs(cumulative_total - baseline_val)]
  cumulative[, diff_pct_vs_baseline := abs(100 * (cumulative_total - baseline_val) / baseline_val)]
  
  # Order output
  setorder(cumulative, scenario)
  
  return(cumulative)
}

# # # Example:
# cumulative_deaths <- calculate_cumulative_impact(
#   all_results,
#   metric = "dead",
#   start_year = 2025,
#   end_year = 2050
# )


#...........................................................
## Validation Helper ----
#...........................................................

validate_intervention_results <- function(results_dt) {
  #' Run basic validation checks on results
  #' 
  #' @param results_dt Data.table with model results
  #' 
  #' @return List with validation results and any issues found
  
  issues <- list()
  
  # Check for negative values
  neg_cols <- c("well", "sick", "dead", "pop", "newcases")
  for (col in neg_cols) {
    if (results_dt[, any(get(col) < 0, na.rm = TRUE)]) {
      issues[[paste0("negative_", col)]] <- 
        results_dt[get(col) < 0, .(scenario, year, age, sex, cause, value = get(col))]
    }
  }
  
  # Check for NA values
  na_cols <- c("eff_ir", "eff_cf", "dead", "newcases")
  for (col in na_cols) {
    if (results_dt[, any(is.na(get(col)))]) {
      issues[[paste0("na_", col)]] <- 
        results_dt[is.na(get(col)), .(scenario, year, age, sex, cause)]
    }
  }
  
  # Check population consistency
  pop_check <- results_dt[, .(
    total_population = sum(well + sick, na.rm = TRUE),
    recorded_pop = sum(pop, na.rm = TRUE)
  ), by = .(scenario, year)]
  
  pop_check[, diff := abs(total_population - recorded_pop)]
  if (pop_check[, any(diff > 0.01 * recorded_pop)]) {
    issues[["population_mismatch"]] <- pop_check[diff > 0.01 * recorded_pop]
  }
  
  # Check that effects are bounded
  if (results_dt[, any(eff_ir < 0 | eff_ir > 2, na.rm = TRUE)]) {
    issues[["eff_ir_out_of_bounds"]] <- 
      results_dt[eff_ir < 0 | eff_ir > 2, .(scenario, year, age, cause, eff_ir)]
  }
  
  if (results_dt[, any(eff_cf < 0 | eff_cf > 2, na.rm = TRUE)]) {
    issues[["eff_cf_out_of_bounds"]] <- 
      results_dt[eff_cf < 0 | eff_cf > 2, .(scenario, year, age, cause, eff_cf)]
  }
  
  validation_result <- list(
    passed = length(issues) == 0,
    n_issues = length(issues),
    issues = issues
  )
  
  if (validation_result$passed) {
    cat("\n Ok All validation checks passed!\n")
  } else {
    cat("\n Not Ok Validation found", length(issues), "issue(s):\n")
    print(names(issues))
  }
  
  return(validation_result)
}

# Example:
#validation <- validate_intervention_results(all_results)


#...........................................................
# Parallel execution across countries----
#...........................................................

#...........................................................
## Parameters ----
#...........................................................
# 1. Define your intervention parameters BEFORE starting cluster

# HTN target columns in dt_hbp_targets
htn_target_cols <- c("htncov2_aspirational")

# # Scenarios: baseline + antihypertensive only (you can expand this list with more combinations as needed)
# scenarios <- list(
#   baseline = character(0),
#   bp_only = "antihypertensive",
#   sodium_only = "sodium",
#   tfa_only = "tfa",
#   statins_only = "statins",
#   bp_sodium = c("antihypertensive", "sodium"),
#   bp_sodium_tfa = c("antihypertensive", "sodium", "tfa"),
#   all_four = c("antihypertensive", "sodium", "tfa", "statins"),
#   # FAIR-Choices CVD package scenarios
#   fair_only     = "fair_cvd",
#   all_plus_fair = c("antihypertensive", "sodium", "tfa", "statins", "fair_cvd")
# )

# WORKBOOK-DRIVEN scenarios: the scenario catalogue(s) are built by Model 04
# from the input workbook(s). Which intervention FAMILIES run is controlled by
# the Model 00 switches run_clinical_interventions / run_public_health_interventions.
# The baseline (no-new-intervention) comparator is ALWAYS included exactly once
# (required for valid deaths-averted / incremental-cost comparisons). Clinical
# and public-health scenario ids are collision-safe (clinical: I_* / all;
# public health: I_PH_* / all_public_health). No intervention list, effect size,
# coverage/exposure target or cause map is hard-coded here.
if (!exists("run_public_health_interventions")) run_public_health_interventions <- FALSE
if (!exists("run_clinical_interventions"))      run_clinical_interventions      <- TRUE
if (!isTRUE(run_public_health_interventions) && !isTRUE(run_clinical_interventions))
  stop("Model 06: both intervention-family switches are FALSE; nothing to run. ",
       "Set run_clinical_interventions and/or run_public_health_interventions to TRUE.",
       call. = FALSE)

baseline_id <- if (exists("baseline_scenario_id")) baseline_scenario_id else "baseline"

scenarios <- list()
# Baseline (shared comparator, included once) -- take it from whichever enabled
# catalogue defines it; both families use the same id/label.
if (isTRUE(run_clinical_interventions) && exists("fair_scenarios") &&
    baseline_id %in% names(fair_scenarios)) {
  scenarios[[baseline_id]] <- fair_scenarios[[baseline_id]]
} else if (isTRUE(run_public_health_interventions) && exists("public_health_scenarios") &&
           !is.null(public_health_scenarios) &&
           baseline_id %in% names(public_health_scenarios)) {
  scenarios[[baseline_id]] <- public_health_scenarios[[baseline_id]]
}
if (is.null(scenarios[[baseline_id]]))
  scenarios[[baseline_id]] <- list(scenario_id = baseline_id,
                                   scenario_label = "Baseline (no new intervention)",
                                   intervention_ids = character(0),
                                   interventions = character(0))
scenarios[[baseline_id]]$family <- "baseline"

# Clinical (FAIR Choices) family
if (isTRUE(run_clinical_interventions)) {
  if (!exists("fair_scenarios"))
    stop("Model 06: run_clinical_interventions = TRUE but `fair_scenarios` not found. ",
         "Source Model 04 (04_define_interventions_indonesia.R) first.", call. = FALSE)
  for (nm in setdiff(names(fair_scenarios), baseline_id)) {
    ent <- fair_scenarios[[nm]]
    if (is.null(ent$family)) ent$family <- "clinical"
    scenarios[[nm]] <- ent
  }
}

# Public-health family
if (isTRUE(run_public_health_interventions)) {
  if (!exists("public_health_scenarios") || is.null(public_health_scenarios))
    stop("Model 06: run_public_health_interventions = TRUE but `public_health_scenarios` ",
         "not found. Source Model 04 (04_define_interventions_indonesia.R) first.", call. = FALSE)
  for (nm in setdiff(names(public_health_scenarios), baseline_id)) {
    if (nm %in% names(scenarios))
      stop("Model 06: scenario id collision between families: '", nm,
           "'. Rename one catalogue's scenario id.", call. = FALSE)
    ent <- public_health_scenarios[[nm]]
    if (is.null(ent$family)) ent$family <- "public_health"
    scenarios[[nm]] <- ent
  }
}

# Joint clinical + public-health family: the genuine combined scenario is run
# ONLY when both families are enabled. It is a single project.all() run applying
# fair_wb + ph_wb once each to the same baseline-rate copy (never an arithmetic
# combination of the separate `all` / `all_public_health` outputs).
if (isTRUE(run_clinical_interventions) && isTRUE(run_public_health_interventions) &&
    exists("combined_scenarios") && !is.null(combined_scenarios)) {
  for (nm in setdiff(names(combined_scenarios), baseline_id)) {
    if (nm %in% names(scenarios))
      stop("Model 06: joint scenario id collision: '", nm,
           "'. Rename the combined-catalogue scenario id.", call. = FALSE)
    ent <- combined_scenarios[[nm]]
    if (is.null(ent$family)) ent$family <- "clinical_public_health"
    scenarios[[nm]] <- ent
  }
}

cat(sprintf("\nModel 06 families: clinical=%s, public_health=%s | scenarios (%d): %s\n\n",
            isTRUE(run_clinical_interventions), isTRUE(run_public_health_interventions),
            length(scenarios), paste(names(scenarios), collapse = ", ")))

# Parsed tobacco Jha timing / vascular-mortality config from Model 04. Required
# by the public-health apply path for the tobacco sick->dead and Jha-timed
# incidence rows; NULL when the PH family is off (clinical runs never use it).
ph_tobacco_config <- if (exists("public_health_inputs") && !is.null(public_health_inputs))
  public_health_inputs$tobacco_effect_config else NULL

# explicit hypertension control parameters
target_control <- 0.5
control_start_year <- 2026
control_target_year <- 2040
target_control_diabetes <- 0.8


# explicit sodium reduction parameters
saltmet <- "percent"
salteff <- 0.3
saltyear1 <- 2026
saltyear2 <- 2030
drugcov <- "p75"

## TFA (NEW explicit params)
tfa_target_tfa        <- 0         # target % energy from TFA
tfa_policy_start_year <- 2028      # flexible start year (effect laget two years)

## Statins (NEW explicit params to match calculate_statins_impact)
statin_target_coverage <- 0.60
statin_start_year      <- 2026
statin_target_year     <- 2050
adherence_ir <-  0.575
adherence_cf <- 0.664

#baseline_statin_coverage <- NULL   # let project.all infer from dt_statin_scenarios

# 2. Detect and start cluster
ncores <- 6
cl     <- makeCluster(ncores)
registerDoParallel(cl)

clusterExport(
  cl,
  varlist = c(
    "project.all",
    "run_multiple_scenarios",
    "get.bp.prob",
    "get_gbd_relative_risks",
    "expand_to_single_year_ages",
    "calculate_baseline_incidence_gbd",
    "calculate_etihad_cumulative_rr",
    "calculate_coverage_by_year",
    "add_coverage_by_year",
    "calculate_aggregate_coverage",
    "apply_coverage_adjustment",
    "calculate_antihypertensive_impact_etihad",
    "calculate_sodium_impact_etihad",
    "calculate_tfa_impact",
    "calculate_statins_impact",
    "calculate_fair_cvd_impact",
    "calculate_fair_workbook_impact",
    "calculate_public_health_workbook_impact",
    "calculate_tobacco_transition_effects",
    "rate_ratio_to_probability",
    "tobacco_lag_fraction",
    ".tobacco_band_index",
    "ph_tobacco_config",
    "load_fair_cvd_params",
    ".fair_clean_chr",
    "dt_fair_params",
    "fair_condition_map",
    "fair_default_bundles",
    "repYear",
    "data.in",
    "b_rates",
    "inc",
    "dt_hbp_control",
    "dt_hbp_targets",
    "dt_gbd_rr",
    "ETIHAD_RR",
    "ETIHAD_RR_BIN",
    "dt_tfa_scenarios",
    "dt_statin_scenarios",
    "dt_af_statins",
    "scenarios",
    "wd_outp",
    "control_start_year",
    "control_target_year",
    "drugcov",
    "htn_target_cols",
    "calculate_antihypertensive_diabetes",
    "target_control_diabetes"
  ),
  envir = globalenv()
)

clusterEvalQ(cl, {
  library(data.table)
  library(dplyr)
})

# 5. Define your countries and scenarios
locs <- unique(data.in$location)
locs <- locs[!locs %in% c("Greenland", "Bermuda")]  # Exclusions

locs <- c("Indonesia")

#...........................................................
## Parallel execution: loop over countries × target scenarios ----
#...........................................................


# One job per (country, target_col)
jobs <- CJ(location = locs, target_col = htn_target_cols)

time_start <- Sys.time()

results_list <- foreach(
  job_idx        = seq_len(nrow(jobs)),
  .packages      = c("data.table", "dplyr"),
  .errorhandling = "pass",
  .verbose       = TRUE
) %dopar% {
  
  country    <- jobs$location[job_idx]
  target_col <- jobs$target_col[job_idx]
  
  log_file <- file.path(
    wd_outp, "out_model",
    paste0("log_fair_", country, "_", target_col, ".txt")
  )
  sink(log_file, split = FALSE)
  
  cat("\n==============================\n")
  cat("Country      :", country,    "\n")
  cat("Target column:", target_col, "\n")
  cat("Time         :", as.character(Sys.time()), "\n")
  cat("==============================\n")
  
  res <- tryCatch({
    run_multiple_scenarios(
      Country             = country,
      scenario_list       = scenarios,
      target_control      = 0.5,           # fallback if country not in dt_hbp_targets
      control_start_year  = control_start_year,
      control_target_year = control_target_year,
      drugcov             = drugcov,
      baseline_ctrl       = NULL,          # inferred from dt_hbp_control
      dt_hbp_targets      = dt_hbp_targets,
      htn_target_col      = target_col,    # key: drives which column is read
      target_control_diabetes = target_control_diabetes,
      statin_target_coverage   = 0.60,
      statin_start_year        = 2026,
      statin_target_year       = 2050,
      adherence_ir             = 1,
      adherence_cf             = 1,
      baseline_statin_coverage = NULL,
      saltmet   = "percent",
      salteff   = 0.3,                       # sodium reduction
      saltyear1 = 2026,
      saltyear2 = 2030,
      tfa_target_tfa        = 0,
      tfa_policy_start_year = 2028,
      # FAIR-Choices CVD package (defaults from taxonomy_effect_size.xlsx;
      # edit these or pass fair_params/fair_bundles/fair_bundle_coverage to
      # substitute local Indonesian program-data evidence)
      fair_params            = NULL,   # NULL -> default dt_fair_params
      fair_bundles           = NULL,   # NULL -> full default CVD package
      fair_start_year        = 2026,
      fair_target_year       = 2050,
      fair_target_coverage   = 0.80,
      fair_baseline_coverage = 0,
      fair_bundle_coverage   = NULL,
      ph_tobacco_config      = ph_tobacco_config
    )
  }, error = function(e) {
    cat("ERROR in", country, "/", target_col, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(res)) {
    res[, htn_target_scenario := target_col]
    
    output_file <- file.path(
      wd_outp, "out_model",
      paste0("model_output_", country, "_", target_col, ".rds")
    )
    saveRDS(res, file = output_file)
    cat("Saved:", output_file, "\n")
  } else {
    cat("No results to save for", country, "/", target_col, "\n")
  }
  
  sink()
  res
}

time_end <- Sys.time()
cat("Total runtime:", round(difftime(time_end, time_start, units = "mins"), 1), "minutes\n")

stopCluster(cl)

# Check which countries succeeded
successful <- sapply(results_list, function(x) !is.null(x))
cat("\nSuccessful runs:", sum(successful), "out of", length(locs), "\n")
cat("Failed countries:", paste(locs[!successful], collapse = ", "), "\n")

# Combine all results (if not too large)
#all_results <- rbindlist(results_list, fill = TRUE)

#...........................................................
# Cleaning up the workspace ----
#...........................................................

rm(list = ls()[sapply(ls(), function(x) is.data.frame(get(x)))])
rm(is,bpcats,locs,i,time1,time2)
