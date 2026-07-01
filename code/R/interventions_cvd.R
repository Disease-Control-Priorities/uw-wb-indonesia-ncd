################################################################################
# INDONESIA INTEGRATED NCD MODEL — CVD INTERVENTION LIBRARY
# R/interventions_cvd.R
# ─────────────────────────────────────────────────────────────────────────────
# V1 SIMPLIFIED INTERVENTION INTERFACE
#
# Implements CVD intervention modifiers. Each function returns:
#   eff_ir[ages]  — incidence multiplier  (< 1 = reduction)
#   eff_cf[ages]  — case-fatality multiplier (< 1 = reduction)
#
# INTERVENTIONS:
#   1. Antihypertensive treatment
#   2. Statin therapy
#   3. Dietary sodium reduction
#   4. Trans-fatty acid (TFA) elimination
#   5. Diabetes-specific BP control
#
# V1 SIMPLIFICATIONS (relative to the UW CVD parent model):
#   BP control: uses population-level SBP reduction (delta_cov × 10 mmHg per
#     treated patient), not a full BP-bin redistribution. bp_dist is empty in
#     V1 because hbp_control_data.rds supplies coverage trajectories but not
#     Indonesia's SBP bin distribution. The Ettehad table is loaded but only
#     the CF reduction column is used directly; the IR effect goes through the
#     GBD RR-per-10mmHg dose-response instead of bin-specific Ettehad weights.
#     This is the dominant simplification vs the parent model.
#   Statins: uses hardcoded meta-analytic IR/CF reductions with an age taper.
#     Parent model used attributable fractions and primary/secondary prevention
#     stratification. Update to V2 when eligibility data is available.
#   Sodium/TFA: generic RR-based modifiers. Parent model used exposure shifts
#     and distributional assumptions. Acceptable for V1 sensitivity analysis.
#
# DATA FILES (loaded lazily on first call to apply_cvd_interventions()):
#   data/gbd/ettehad_rr_bp_reduction_effects.xlsx  — Ettehad CF effect sizes
#   data/hbp/hbp_control_data.rds                  — Indonesia BP coverage traj.
#   data/gbd/gbd_rr_sbp.rds                        — GBD RR per 10 mmHg SBP
#
# INTERFACE:
#   apply_cvd_interventions(year, sex, ages, cause_id, scenario)
#   → list(eff_ir = numeric[length(ages)], eff_cf = numeric[length(ages)])
#
# Note: %||% is defined in engine.R and available when this file is sourced.
################################################################################

# Do not attach packages in library files. Use namespace-qualified calls throughout.
# Packages are loaded by R/packages.R before any R/ file is sourced.

################################################################################
# 1  LOAD INTERVENTION DATA FILES
# Lazy loading: data is loaded on first call to apply_cvd_interventions(),
# not at source time. This prevents sourcing interventions_cvd.R from failing
# when optional data files are absent (e.g., during testing or calibration).
################################################################################

.CVD_INT_ENV <- new.env(parent = emptyenv())
.CVD_INT_ENV$loaded <- FALSE

.load_cvd_intervention_data <- function() {

  # ── Ettehad CF effect-size table ────────────────────────────────────────────
  ettehad_raw <- readxl::read_excel(here::here("data", "gbd",
                                               "ettehad_rr_bp_reduction_effects.xlsx"),
                                    sheet = 1)
  .CVD_INT_ENV$etihad_rr <- as.data.frame(ettehad_raw) |>
    dplyr::mutate(cause = dplyr::case_when(
      cause %in% c("istroke", "ischemic_stroke") ~ "ischemic_stroke",
      cause %in% c("hstroke", "ich")             ~ "ich",
      TRUE                                        ~ cause
    ))
  message("  Ettehad RR table: ", nrow(.CVD_INT_ENV$etihad_rr), " rows | causes: ",
          paste(unique(.CVD_INT_ENV$etihad_rr$cause), collapse = ", "))

  # ── BP coverage trajectory (Indonesia) ──────────────────────────────────────
  hbp_raw <- readRDS(here::here("data", "hbp", "hbp_control_data.rds"))

  if (!is.data.frame(hbp_raw)) {
    hbp_raw <- as.data.frame(hbp_raw)
  }

  names(hbp_raw) <- tolower(names(hbp_raw))

  if (!"year" %in% names(hbp_raw)) {
    stop("hbp_control_data.rds must contain a year column.", call. = FALSE)
  }

  if (!"baseline_ctrl" %in% names(hbp_raw)) {
    if ("control_scaled" %in% names(hbp_raw)) {
      hbp_raw$baseline_ctrl <- hbp_raw$control_scaled
    } else if ("controlled" %in% names(hbp_raw)) {
      hbp_raw$baseline_ctrl <- hbp_raw$controlled
    } else if ("control" %in% names(hbp_raw)) {
      hbp_raw$baseline_ctrl <- hbp_raw$control
    } else {
      stop(
        "hbp_control_data.rds must contain baseline_ctrl, control_scaled, controlled, or control.",
        call. = FALSE
      )
    }
  }

  hbp_raw$year <- as.integer(hbp_raw$year)
  hbp_raw$baseline_ctrl <- as.numeric(hbp_raw$baseline_ctrl)

  loc_ok <- rep(FALSE, nrow(hbp_raw))
  loc_cols <- intersect(
    c("iso3", "location", "location_name", "country", "country_name"),
    names(hbp_raw)
  )

  if (length(loc_cols) == 0) {
    # If the file has no location column, assume it is already Indonesia-specific.
    loc_ok <- rep(TRUE, nrow(hbp_raw))
  } else {
    if ("iso3" %in% names(hbp_raw)) {
      loc_ok <- loc_ok | toupper(hbp_raw$iso3) == "IDN"
    }
    for (lc in intersect(c("location", "location_name", "country", "country_name"),
                         names(hbp_raw))) {
      loc_ok <- loc_ok | grepl("Indonesia", hbp_raw[[lc]], ignore.case = TRUE)
    }
  }

  idn_cov <- hbp_raw[loc_ok, , drop = FALSE] |>
    dplyr::filter(!is.na(year), !is.na(baseline_ctrl)) |>
    dplyr::arrange(year)

  if (nrow(idn_cov) == 0) {
    warning(
      "hbp_control_data.rds: Indonesia coverage not found — using baseline_ctrl = 0.044.",
      call. = FALSE
    )

    .CVD_INT_ENV$bp_coverage <- data.frame(
      year = 2025L,
      baseline_ctrl = 0.044,
      control_scaled = 0.044,
      stringsAsFactors = FALSE
    )
  } else {
    .CVD_INT_ENV$bp_coverage <- idn_cov

    base_yr <- idn_cov$year[which.min(abs(idn_cov$year - 2025))]
    base_ctrl <- idn_cov$baseline_ctrl[idn_cov$year == base_yr][1]

    message("  BP coverage (Indonesia): baseline_ctrl = ",
            round(base_ctrl, 3), " at year ", base_yr)
  }

  # V1: no SBP bin distribution — IR effect uses population-level SBP reduction.
  .CVD_INT_ENV$bp_dist <- data.frame()

  # ── GBD SBP RRs per 10 mmHg ─────────────────────────────────────────────────
  # Default: primary file (hhd rr = 1.0, no SBP incidence effect on HHD).
  # For sensitivity: call reload_cvd_rr(here("data","gbd","gbd_rr_sbp_hhd_sens.rds"))
  # HHD convention in the primary file:
  #   eff_ir = 1.0 (SBP→HHD incidence excluded; no public BoP curve)
  #   eff_cf = 1 - 0.20 × delta_cov (Ettehad CF effect retained)
  #   Net effect on HHD direct mortality = eff_cf only.
  #   This is documented as the primary V1 convention in 01b_prepare_sbp_rr.R.
  .CVD_INT_ENV$gbd_rr <- readRDS(here::here("data", "gbd", "gbd_rr_sbp.rds"))
  message("  GBD SBP RR: ", nrow(.CVD_INT_ENV$gbd_rr), " rows")
  message("  HHD RR: ", unique(.CVD_INT_ENV$gbd_rr$rr_per_10mmhg[
    .CVD_INT_ENV$gbd_rr$cause == "hhd"]), " (1.0 = no SBP IR effect in primary)")

  .CVD_INT_ENV$loaded <- TRUE
}

# Called at the start of apply_cvd_interventions() — loads data on first call only.
.ensure_loaded <- function() {
  if (!isTRUE(.CVD_INT_ENV$loaded)) .load_cvd_intervention_data()
}

#' Swap the SBP RR table without reloading all intervention data.
#' Used by 06_run_scenarios.R for the HHD sensitivity scenario.
#' @param rr_path  Full path to the replacement gbd_rr_sbp*.rds file.
reload_cvd_rr <- function(rr_path) {
  if (!file.exists(rr_path))
    stop("RR file not found: ", rr_path, call. = FALSE)
  if (!isTRUE(.CVD_INT_ENV$loaded))
    .load_cvd_intervention_data()
  rr <- readRDS(rr_path)
  needed <- c("age", "sex", "cause", "rr_per_10mmhg")
  missing_cols <- setdiff(needed, names(rr))
  if (length(missing_cols) > 0)
    stop("RR file missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  if (!"hhd" %in% rr$cause)
    stop("RR file does not include hhd rows; downstream HHD lookup will fail.",
         call. = FALSE)
  .CVD_INT_ENV$gbd_rr <- rr
  hhd_range <- range(rr$rr_per_10mmhg[rr$cause == "hhd"], na.rm = TRUE)
  message("  RR table reloaded: ", basename(rr_path),
          " | HHD RR range = ", round(hhd_range[1], 3),
          "\u2013", round(hhd_range[2], 3))
}

# Case-fatality reductions per unit coverage increase (Ettehad et al. Lancet 2016)
.CF_ETIHAD <- data.frame(
  cause  = c("ihd", "ischemic_stroke", "ich", "hhd"),
  cf_red = c(0.24,  0.36,              0.76,  0.20),
  stringsAsFactors = FALSE
)

# Statin effect sizes (UW CVD model / published meta-analyses)
.STATIN_EFFECTS <- data.frame(
  cause  = c("ihd", "ischemic_stroke", "ich", "hhd"),
  ir_red = c(0.25,  0.18,              0.00,  0.10),
  cf_red = c(0.10,  0.05,              0.00,  0.05),
  stringsAsFactors = FALSE
)

# TFA elimination effects (WHO/GBD estimates)
.TFA_EFFECTS <- data.frame(
  cause  = c("ihd", "ischemic_stroke", "ich", "hhd"),
  ir_red = c(0.07,  0.03,              0.01,  0.03),
  cf_red = c(0.03,  0.01,              0.00,  0.01),
  stringsAsFactors = FALSE
)

# Sodium reduction: ~1.7 mmHg SBP per 1 g/day reduction
.SODIUM_SBP_PER_GRAM <- 1.7

################################################################################
# 2  COVERAGE RAMP
################################################################################

.ramp <- function(year, bsln, target, start_yr = 2025L, speed = "fast") {
  end_yr <- if (speed == "fast") start_yr + 5L else start_yr + 10L
  if (year <  start_yr) return(bsln)
  if (year >= end_yr)   return(target)
  bsln + (target - bsln) * (year - start_yr) / (end_yr - start_yr)
}

.target_year_from_speed <- function(start_yr = 2025L, speed = "fast") {
  if (identical(speed, "fast")) {
    start_yr + 5L
  } else {
    start_yr + 10L
  }
}

.lookup_rr10 <- function(gbd_rr, age, sex, cause_id, default = 1.0) {
  vals <- gbd_rr$rr_per_10mmhg[
    gbd_rr$age == age &
      gbd_rr$sex == sex &
      gbd_rr$cause == cause_id
  ]

  vals <- vals[is.finite(vals)]

  if (length(vals) == 0) {
    return(default)
  }

  vals[1]
}

################################################################################
# 3  ETTEHAD CUMULATIVE EFFECT BY BP BIN
################################################################################

.calculate_etihad_effect <- function(bp_cat, cause_id, diabetes_weight = 0.10) {
  et <- .CVD_INT_ENV$etihad_rr

  key <- paste(et$cause, et$bp_cat, sep = "_")
  idx <- match(paste(cause_id, bp_cat, sep = "_"), key)

  out <- numeric(length(bp_cat))
  ok <- !is.na(idx)

  if (any(!ok)) {
    warning(
      "Ettehad RR missing for: ",
      paste(paste(cause_id, bp_cat[!ok], sep = "/"), collapse = ", "),
      " — using zero effect for missing cells only.",
      call. = FALSE
    )
  }

  out[ok] <- (1 - diabetes_weight) * et$effect_size_nodiabetes[idx[ok]] +
    diabetes_weight * et$effect_size_diabetes[idx[ok]]

  out
}

################################################################################
# 4  GET BP DISTRIBUTION FOR AGE/SEX
################################################################################

.get_bp_probs <- function(sex_label, ages) {
  BP_CATS <- c("<120", "120-129", "130-139", "140-149",
               "150-159", "160-169", "170-179", "180+")

  if (is.null(.CVD_INT_ENV$bp_dist) || nrow(.CVD_INT_ENV$bp_dist) == 0) {
    prob_mat <- matrix(1 / length(BP_CATS), nrow = length(ages), ncol = length(BP_CATS))
    colnames(prob_mat) <- BP_CATS
    return(prob_mat)
  }

  bd <- .CVD_INT_ENV$bp_dist
  sex_match <- if (sex_label == "Female") c("Female", "female", "F", "Women")
               else                        c("Male",   "male",   "M", "Men")
  bd_sex <- bd |> dplyr::filter(sex %in% sex_match)
  if (nrow(bd_sex) == 0) bd_sex <- bd

  prob_mat <- matrix(1 / length(BP_CATS), nrow = length(ages), ncol = length(BP_CATS))
  colnames(prob_mat) <- BP_CATS

  if ("age" %in% colnames(bd_sex)) {
    avail_ages <- sort(unique(bd_sex$age))
    for (i in seq_along(ages)) {
      ag      <- ages[i]
      nearest <- avail_ages[which.min(abs(avail_ages - ag))]
      row_data <- bd_sex |> dplyr::filter(age == nearest) |>
        dplyr::select(bp_cat, prob) |> dplyr::filter(bp_cat %in% BP_CATS)
      for (j in seq_along(BP_CATS)) {
        v <- row_data$prob[row_data$bp_cat == BP_CATS[j]]
        if (length(v) > 0 && !is.na(v)) prob_mat[i, j] <- v
      }
      rs <- sum(prob_mat[i, ])
      if (rs > 0) prob_mat[i, ] <- prob_mat[i, ] / rs
    }
  }

  prob_mat
}

################################################################################
# 5  ANTIHYPERTENSIVE INTERVENTION
# V1 implementation: population-level SBP reduction.
#   IR effect: eff_ir = rr_per_10mmhg ^ (-delta_cov × SBP_PER_TREATED / 10)
#     where SBP_PER_TREATED = 10 mmHg is the average SBP reduction per newly
#     treated patient (consistent with Ettehad et al. meta-analysis).
#   CF effect: from Ettehad table, applied as 1 - cf_red × delta_cov.
# This is equivalent to assuming all hypertensives have the same BP level and
# the same treatment effect. A full bin-redistribution approach (V2) would
# weight effects across the SBP distribution using .get_bp_probs().
################################################################################

.SBP_REDUCTION_PER_TREATED <- 10.0   # mmHg reduction per treated patient

compute_antihypertensive_effects <- function(year, sex, ages, cause_id,
                                             baseline_ctrl = NULL,
                                             target_ctrl   = 0.70,
                                             start_yr      = 2025L,
                                             speed         = "fast") {
  eff_ir <- rep(1.0, length(ages))
  eff_cf <- rep(1.0, length(ages))

  if (is.null(baseline_ctrl)) {
    cov_data <- .CVD_INT_ENV$bp_coverage
    if (!is.null(cov_data) && nrow(cov_data) > 0) {
      nearest_yr    <- cov_data$year[which.min(abs(cov_data$year - start_yr))]
      baseline_ctrl <- cov_data$baseline_ctrl[cov_data$year == nearest_yr][1]
    } else {
      baseline_ctrl <- 0.044
    }
  }

  cov_t     <- .ramp(year, baseline_ctrl, target_ctrl, start_yr, speed)
  delta_cov <- max(cov_t - baseline_ctrl, 0)
  if (delta_cov <= 0) return(list(eff_ir = eff_ir, eff_cf = eff_cf))

  cf_red <- .CF_ETIHAD$cf_red[.CF_ETIHAD$cause == cause_id]
  if (length(cf_red) == 0) cf_red <- 0
  eff_cf <- rep(1 - cf_red * delta_cov, length(ages))

  gbd_rr      <- .CVD_INT_ENV$gbd_rr
  sbp_red_pop <- delta_cov * .SBP_REDUCTION_PER_TREATED

  for (i in seq_along(ages)) {
    ag   <- ages[i]
    rr10 <- .lookup_rr10(
      gbd_rr   = gbd_rr,
      age      = ag,
      sex      = sex,
      cause_id = cause_id,
      default  = 1.0
    )
    eff_ir[i] <- rr10^(-sbp_red_pop / 10)
  }

  list(eff_ir = pmax(eff_ir, 0.01), eff_cf = pmax(eff_cf, 0.01))
}

################################################################################
# 6  STATIN THERAPY
# V1 implementation: hardcoded meta-analytic IR/CF reductions with age taper.
# Reductions from CTT Collaboration (Lancet 2010) and UW CVD model parameters.
# V2: add primary vs. secondary prevention stratification and eligibility data.
################################################################################

compute_statins_effects <- function(year, sex, ages, cause_id,
                                    baseline_cov = 0.05,
                                    target_cov   = 0.50,
                                    start_yr     = 2025L,
                                    speed        = "fast") {
  eff_ir <- rep(1.0, length(ages))
  eff_cf <- rep(1.0, length(ages))

  cov_t     <- .ramp(year, baseline_cov, target_cov, start_yr, speed)
  delta_cov <- max(cov_t - baseline_cov, 0)
  if (delta_cov <= 0) return(list(eff_ir = eff_ir, eff_cf = eff_cf))

  se <- .STATIN_EFFECTS[.STATIN_EFFECTS$cause == cause_id, ]
  if (nrow(se) == 0) return(list(eff_ir = eff_ir, eff_cf = eff_cf))

  # Statins primarily benefit ages 40–80; attenuate outside this range
  age_weight <- pmin(pmax((ages - 35) / 5, 0), 1) *
                pmin(pmax((85 - ages) / 5, 0), 1)

  eff_ir <- 1 - se$ir_red * delta_cov * age_weight
  eff_cf <- 1 - se$cf_red * delta_cov * age_weight

  list(eff_ir = pmax(eff_ir, 0.01), eff_cf = pmax(eff_cf, 0.01))
}

################################################################################
# 7  DIETARY SODIUM REDUCTION
################################################################################

compute_sodium_effects <- function(year, sex, ages, cause_id,
                                   sodium_g_reduction = 2.0,
                                   start_yr           = 2025L,
                                   speed              = "fast") {
  eff_ir <- rep(1.0, length(ages))
  eff_cf <- rep(1.0, length(ages))

  target_yr <- .target_year_from_speed(start_yr, speed)

  scale <- if (year < start_yr) {
    0
  } else if (year >= target_yr) {
    1
  } else {
    (year - start_yr) / (target_yr - start_yr)
  }

  if (scale <= 0 || sodium_g_reduction <= 0) {
    return(list(eff_ir = eff_ir, eff_cf = eff_cf))
  }

  sbp_reduction <- sodium_g_reduction * .SODIUM_SBP_PER_GRAM * scale
  gbd_rr <- .CVD_INT_ENV$gbd_rr

  for (i in seq_along(ages)) {
    ag <- ages[i]

    rr10 <- .lookup_rr10(
      gbd_rr   = gbd_rr,
      age      = ag,
      sex      = sex,
      cause_id = cause_id,
      default  = 1.0
    )

    eff_ir[i] <- rr10^(-sbp_reduction / 10)
  }

  list(eff_ir = pmax(eff_ir, 0.01), eff_cf = eff_cf)
}

################################################################################
# 8  TFA ELIMINATION
################################################################################

compute_tfa_effects <- function(year, sex, ages, cause_id,
                                start_yr = 2025L,
                                speed    = "fast") {
  eff_ir <- rep(1.0, length(ages))
  eff_cf <- rep(1.0, length(ages))

  target_yr <- .target_year_from_speed(start_yr, speed)

  scale <- if (year < start_yr) {
    0
  } else if (year >= target_yr) {
    1
  } else {
    (year - start_yr) / (target_yr - start_yr)
  }

  if (scale <= 0) {
    return(list(eff_ir = eff_ir, eff_cf = eff_cf))
  }

  te <- .TFA_EFFECTS[.TFA_EFFECTS$cause == cause_id, ]
  if (nrow(te) == 0) {
    return(list(eff_ir = eff_ir, eff_cf = eff_cf))
  }

  eff_ir <- rep(1 - te$ir_red * scale, length(ages))
  eff_cf <- rep(1 - te$cf_red * scale, length(ages))

  list(eff_ir = pmax(eff_ir, 0.01), eff_cf = pmax(eff_cf, 0.01))
}

################################################################################
# 9  DIABETES-SPECIFIC BP CONTROL
################################################################################

compute_diabetes_bp_effects <- function(year, sex, ages, cause_id,
                                        diabetes_prev = NULL,
                                        baseline_ctrl = 0.20,
                                        target_ctrl   = 0.80,
                                        start_yr      = 2025L,
                                        speed         = "fast") {
  eff_ir <- rep(1.0, length(ages))
  eff_cf <- rep(1.0, length(ages))

  cov_t     <- .ramp(year, baseline_ctrl, target_ctrl, start_yr, speed)
  delta_cov <- max(cov_t - baseline_ctrl, 0)
  if (delta_cov <= 0) return(list(eff_ir = eff_ir, eff_cf = eff_cf))

  # Default: approximate T2D prevalence by age (Indonesia GBD 2023)
  if (is.null(diabetes_prev))
    diabetes_prev <- pmin(pmax((ages - 30) * 0.003, 0), 0.18)

  gbd_rr  <- .CVD_INT_ENV$gbd_rr
  BP_CATS <- c("<120","120-129","130-139","140-149","150-159","160-169","170-179","180+")
  BP_MID  <- c(110,  125,      135,      145,      155,      165,      175,      185)
  prob_mat <- .get_bp_probs(sex, ages)

  for (i in seq_along(ages)) {
    ag   <- ages[i]
    d_wt <- diabetes_prev[i]
    rr10 <- .lookup_rr10(
      gbd_rr   = gbd_rr,
      age      = ag,
      sex      = sex,
      cause_id = cause_id,
      default  = 1.0
    )

    inc_steps  <- (BP_MID - 110) / 10
    rr_bin     <- rr10^inc_steps
    etihad_eff <- .calculate_etihad_effect(BP_CATS, cause_id,
                                            diabetes_weight = d_wt)
    prob_row    <- prob_mat[i, ]
    ir_bin_new  <- rr_bin * (1 - etihad_eff * delta_cov)
    ir_bin_base <- rr_bin
    ir_new  <- sum(ir_bin_new  * prob_row)
    ir_base <- sum(ir_bin_base * prob_row)

    subgroup_eff_ir <- if (ir_base > 0) ir_new / ir_base else 1.0
    eff_ir[i] <- 1 - d_wt * (1 - subgroup_eff_ir)
  }

  cf_red <- .CF_ETIHAD$cf_red[.CF_ETIHAD$cause == cause_id]
  if (length(cf_red) > 0)
    eff_cf <- 1 - cf_red * delta_cov * diabetes_prev

  list(eff_ir = pmax(eff_ir, 0.01), eff_cf = pmax(eff_cf, 0.01))
}

################################################################################
# 10  MAIN INTERFACE — apply_cvd_interventions()
################################################################################

#' Compute combined CVD intervention effects for a given year/sex/cause.
#'
#' @param year       integer projection year
#' @param sex        "Female" or "Male"
#' @param ages       integer vector 0:100
#' @param cause_id   "ihd", "ischemic_stroke", "ich", or "hhd"
#' @param scenario   named list with keys:
#'   bp_on, bp_baseline_ctrl, bp_target_ctrl, statins_on, statins_baseline_cov,
#'   statins_target_cov, sodium_on, sodium_g_red, tfa_on, diabetes_bp_on, speed
#' @return list(eff_ir = numeric[length(ages)], eff_cf = numeric[length(ages)])

apply_cvd_interventions <- function(year, sex, ages, cause_id, scenario) {
  .ensure_loaded()   # loads data on first call; no-op thereafter
  eff_ir <- rep(1.0, length(ages))
  eff_cf <- rep(1.0, length(ages))
  spd    <- scenario$speed %||% "fast"

  if (isTRUE(scenario$bp_on)) {
    res    <- compute_antihypertensive_effects(
      year, sex, ages, cause_id,
      baseline_ctrl = scenario$bp_baseline_ctrl %||% NULL,
      target_ctrl   = scenario$bp_target_ctrl   %||% 0.70,
      speed         = spd)
    eff_ir <- eff_ir * res$eff_ir
    eff_cf <- eff_cf * res$eff_cf
  }

  if (isTRUE(scenario$statins_on)) {
    res    <- compute_statins_effects(
      year, sex, ages, cause_id,
      baseline_cov = scenario$statins_baseline_cov %||% 0.05,
      target_cov   = scenario$statins_target_cov   %||% 0.50,
      speed        = spd)
    eff_ir <- eff_ir * res$eff_ir
    eff_cf <- eff_cf * res$eff_cf
  }

  if (isTRUE(scenario$sodium_on)) {
    res <- compute_sodium_effects(
      year               = year,
      sex                = sex,
      ages               = ages,
      cause_id           = cause_id,
      sodium_g_reduction = scenario$sodium_g_red %||% 2.0,
      speed              = spd
    )
    eff_ir <- eff_ir * res$eff_ir
    eff_cf <- eff_cf * res$eff_cf
  }

  if (isTRUE(scenario$tfa_on)) {
    res <- compute_tfa_effects(
      year     = year,
      sex      = sex,
      ages     = ages,
      cause_id = cause_id,
      speed    = spd
    )
    eff_ir <- eff_ir * res$eff_ir
    eff_cf <- eff_cf * res$eff_cf
  }

  if (isTRUE(scenario$diabetes_bp_on)) {
    res    <- compute_diabetes_bp_effects(year, sex, ages, cause_id, speed = spd)
    eff_ir <- eff_ir * res$eff_ir
    eff_cf <- eff_cf * res$eff_cf
  }

  list(eff_ir = pmax(eff_ir, 0.001), eff_cf = pmax(eff_cf, 0.001))
}

################################################################################
# 11  INITIALISE (called once at source time)
################################################################################

message("── R/interventions_cvd.R loaded (V1 simplified interface) ─────────────────")
message("  Data loaded lazily on first call to apply_cvd_interventions()")
message("  V1 note: BP uses population-level SBP reduction; full bin redistribution deferred to V2")
