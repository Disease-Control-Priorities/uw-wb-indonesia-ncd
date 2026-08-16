################################################################################
# INDONESIA INTEGRATED NCD MODEL — CALIBRATE DISEASE MODULES
# scripts/04_calibrate_modules.R
# ─────────────────────────────────────────────────────────────────────────────
# Fits TPMs for CVD TPM causes (IHD, ischemic stroke, ICH), stores direct-mortality
# base rates for HHD, and fits the 7-state cervical cancer TPM.
#
# OUTPUTS (data/model/calibration/):
#   cvd_calibration_v1.rds  — list of 4 CVD causes, each containing:
#                               $tpm           list(Female=..., Male=...)
#                               $base_mx_cause list(Female=..., Male=...)
#                               $base_bgmx     list(Female=..., Male=...)
#                               $model_type    "tpm" or "direct_mortality"
#   cervical_tpm.Rda        — cervical_tpm list(Female = tpm_list)
#   cvd_calib_check_all.csv — calibration residuals by cause × sex × age
#   plots/                  — calibration diagnostic plots
#
# ── CALIBRATION DESIGN ────────────────────────────────────────────────────────
# IHD (and other TPM causes): 5-state Markov
#   well → incident → prevalent → dead_cause | dead_bg
#   Residual method for p_prev_dead:
#     deaths_remaining = max(mx_cause − ir × p_inc_dead, 0)
#     p_prev_dead      = deaths_remaining / prev
#   This avoids the steady-state back-solve failure mode when mx >> ir×CFR.
#
# Cervical cancer: 7-state Markov
#   well → precancer → local → regional → distant → dead_cause | dead_bg
#   CONCORD-3 stage CFRs scaled to GBD-implied prevalent-pool mortality via
#   ratio correction (floored at 0.10 to preserve stage ordering).
#
# HHD: direct_mortality (no TPM — GBD does not provide incidence)
#   mx_cause = mx_WPP × frac_cause; calibration confirms store of base rates.
#
# INTERPOLATION:
#   interp_piecewise_constant() from engine.R (piecewise-constant, GBD 5-yr
#   groups → single-year ages). Do NOT use interp_piecewise_loglinear() for
#   calibration: the non-monotone age patterns in some causes (cervical ca,
#   HHD) make log-linear interpolation unreliable.
################################################################################

library(here)
source(here("R", "packages.R"))
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
  library(purrr); library(stringr); library(scales); library(ggplot2)
})

source(here::here("R", "engine.R"))          # AGES, SEXES, interp_piecewise_constant
source(here::here("R", "cause_registry.R"))  # SHARED_PARAMS, CVD_CAUSE_MAP, CVD_MODEL_TYPE

# ── PATHS ─────────────────────────────────────────────────────────────────────
GBD_FILE  <- here::here("data", "gbd", "gbd_measures_full.csv")
WPP_FILE  <- here::here("data", "wpp", "indonesia_ncd_demography.Rda")
OUT_DIR   <- here::here("data", "model", "calibration")
PLOT_DIR  <- file.path(OUT_DIR, "plots")
dir.create(OUT_DIR,  recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

CALIB_YEAR <- SHARED_PARAMS$calib_year   # 2023
TOL        <- SHARED_PARAMS$tol_calib    # 0.05
VAL_AGES   <- 30:69

################################################################################
# 1  LOAD DATA
################################################################################

message("\n── Loading calibration inputs ──────────────────────────────────────────")

load(WPP_FILE)   # provides: sf.wpp, get.lt, locations
p_idn     <- sf.wpp[["IDN"]]
WPP_START <- 2025L

# WPP 2025 all-cause mortality rates (index 1 = year 2025)
# Used as background mortality envelope for calibration.
# Note: WPP projection starts at 2025; used as the nearest available WPP
# background envelope for GBD 2023 calibration. This two-year mismatch is a
# V1 approximation; test sensitivity in V2 once WPP subnational data are ready.
mx_wpp_2025 <- p_idn$mx[1L, , ]   # [2 × 101], sex 1 = Female, 2 = Male

gbd_full <- read_csv(GBD_FILE, show_col_types = FALSE) |>
  filter(year == CALIB_YEAR)

message("  GBD 2023 rows           : ", comma(nrow(gbd_full)))
message("  WPP Female mx(60, 2025) : ", round(mx_wpp_2025[1L, 61L], 5))

################################################################################
# 2  CVD CALIBRATION
#
# calibrate_cvd_cause() fits a 5-state TPM for one CVD cause:
#   States: well(1), incident(2), prevalent(3), dead_cause(4), dead_bg(5)
#   Returns: list(tpm, base_mx_cause, base_bgmx) — base rates needed for
#            year-varying scaling at projection time (engine.R scale_cvd_tpm_to_year).
#
# calibrate_direct_mortality_cause() stores base rates for direct-mortality
#   causes (HHD); no TPM is built.
################################################################################

message("\n── Calibrating all 4 CVD causes ────────────────────────────────────────")

calibrate_cvd_cause <- function(gbd_cause_name) {

  calibrate_sex <- function(sex_label) {
    s_idx <- if (sex_label == "Female") 1L else 2L
    gbd_cvd  <- gbd_full |> filter(sex == sex_label, cause == gbd_cause_name)
    inc_5yr  <- gbd_cvd |> filter(measure == "Incidence") |>
      select(age_mid, rate = rate_per100k) |> mutate(rate = rate / 1e5)
    mx_5yr   <- gbd_cvd |> filter(measure == "Deaths") |>
      select(age_mid, rate = rate_per100k) |> mutate(rate = rate / 1e5)
    prev_5yr <- gbd_cvd |> filter(measure == "Prevalence") |>
      select(age_mid, rate = rate_per100k) |> mutate(rate = rate / 1e5)

    if (nrow(mx_5yr) == 0 || nrow(prev_5yr) == 0)
      stop("Missing GBD Deaths or Prevalence for: ", gbd_cause_name, " / ", sex_label)

    if (nrow(inc_5yr) == 0) {
      # GBD does not serve HHD incidence. Derive IR so the incident path
      # carries all deaths: IR = mx / ACUTE_CFR. With p_inc_dead = ACUTE_CFR,
      # deaths_from_incident = IR × ACUTE_CFR = mx exactly, giving
      # deaths_remaining = 0 and p_prev_dead = 0 (perfect calibration).
      ACUTE_CFR_HHD <- 0.05
      message("    No incidence for ", gbd_cause_name, " / ", sex_label,
              " — deriving IR from incident-path formula (ACUTE_CFR = ", ACUTE_CFR_HHD, ")")
      inc_5yr <- mx_5yr |> mutate(rate = rate / ACUTE_CFR_HHD) |> select(age_mid, rate)
    }

    inc_1yr  <- interp_piecewise_constant(inc_5yr$age_mid,  inc_5yr$rate)
    mx_1yr   <- interp_piecewise_constant(mx_5yr$age_mid,   mx_5yr$rate)
    prev_1yr <- interp_piecewise_constant(prev_5yr$age_mid, prev_5yr$rate)

    mx_all_1yr <- mx_wpp_2025[s_idx, ]
    bgmx_1yr   <- pmax(mx_all_1yr - mx_1yr, 0)

    tpm_list <- lapply(AGES, function(a) {
      idx  <- a + 1L
      ir   <- pmax(inc_1yr[idx],  0)
      mx_c <- pmax(mx_1yr[idx],   0)
      bgmx <- bgmx_1yr[idx]

      # Approximate initial state shares used by the engine initializer.
      # Death allocation must use these shares, not incidence alone.
      p_prev_state <- pmin(pmax(prev_1yr[idx], 0), 0.999)
      p_inc_state  <- pmin((1 - p_prev_state) * (1 - exp(-ir)), 0.20)

      # Row 1: well
      p_well_inc  <- pmin(1 - exp(-ir), 0.999)
      p_well_bgmx <- pmin(bgmx, pmax(0, 0.999 - p_well_inc))
      p_well_well <- pmax(1 - p_well_inc - p_well_bgmx, 0)

      # Row 2: incident
      # Start from GBD deaths/incidence acute CFR, but do not allow the
      # incident state to consume the whole cause-specific death rate.
      ACUTE_DEATH_SHARE_MAX <- 0.80

      p_inc_dead_raw <- if (ir > 1e-12) mx_c / ir else 0
      p_inc_dead_cap <- if (p_inc_state > 1e-12) {
        (ACUTE_DEATH_SHARE_MAX * mx_c) / p_inc_state
      } else {
        0
      }

      p_inc_dead <- pmin(
        pmax(p_inc_dead_raw, 0),
        pmax(p_inc_dead_cap, 0),
        0.95
      )

      p_inc_bgmx <- pmin(bgmx, pmax(0, 0.999 - p_inc_dead))
      p_inc_prev <- pmax(1 - p_inc_dead - p_inc_bgmx, 0)

      # Row 3: prevalent
      # Residual cause-specific mortality is allocated to the prevalent state
      # using the actual prevalent state share.
      p_prev_bgmx <- pmin(bgmx, 0.95)

      deaths_from_incident <- p_inc_state * p_inc_dead
      deaths_remaining     <- pmax(mx_c - deaths_from_incident, 0)

      p_prev_dead <- if (p_prev_state > 1e-12) {
        deaths_remaining / p_prev_state
      } else {
        0
      }

      p_prev_dead <- pmin(
        pmax(p_prev_dead, 0),
        pmax(1 - p_prev_bgmx, 0)
      )

      p_prev_prev <- pmax(1 - p_prev_dead - p_prev_bgmx, 0)

      tpm <- matrix(0, 5, 5)
      tpm[1,1] <- p_well_well;  tpm[1,2] <- p_well_inc;  tpm[1,5] <- p_well_bgmx
      tpm[2,3] <- p_inc_prev;   tpm[2,4] <- p_inc_dead;  tpm[2,5] <- p_inc_bgmx
      tpm[3,3] <- p_prev_prev;  tpm[3,4] <- p_prev_dead; tpm[3,5] <- p_prev_bgmx
      tpm[4,4] <- 1;  tpm[5,5] <- 1

      rs <- rowSums(tpm)
      if (any(abs(rs - 1) > 1e-9))
        warning("CVD TPM row sums off at age ", a, " / ", sex_label, ": ",
                paste(round(rs, 6), collapse = ", "))
      tpm
    })
    names(tpm_list) <- as.character(AGES)
    list(tpm = tpm_list, base_mx_cause = mx_1yr, base_bgmx = bgmx_1yr)
  }

  f <- calibrate_sex("Female")
  m <- calibrate_sex("Male")
  list(
    tpm           = list(Female = f$tpm,           Male = m$tpm),
    base_mx_cause = list(Female = f$base_mx_cause, Male = m$base_mx_cause),
    base_bgmx     = list(Female = f$base_bgmx,     Male = m$base_bgmx)
  )
}

calibrate_direct_mortality_cause <- function(gbd_cause_name) {
  calibrate_sex <- function(sex_label) {
    s_idx  <- if (sex_label == "Female") 1L else 2L
    gbd_dm <- gbd_full |> filter(sex == sex_label, cause == gbd_cause_name)
    mx_5yr <- gbd_dm |> filter(measure == "Deaths") |>
      select(age_mid, rate = rate_per100k) |> mutate(rate = rate / 1e5)
    if (nrow(mx_5yr) == 0)
      stop("Missing GBD deaths for: ", gbd_cause_name, " / ", sex_label)
    mx_1yr   <- interp_piecewise_constant(mx_5yr$age_mid, mx_5yr$rate)
    bgmx_1yr <- pmax(mx_wpp_2025[s_idx, ] - mx_1yr, 0)
    list(base_mx_cause = mx_1yr, base_bgmx = bgmx_1yr)
  }
  f <- calibrate_sex("Female"); m <- calibrate_sex("Male")
  list(
    tpm           = list(Female = NULL,            Male = NULL),
    base_mx_cause = list(Female = f$base_mx_cause, Male = m$base_mx_cause),
    base_bgmx     = list(Female = f$base_bgmx,     Male = m$base_bgmx),
    model_type    = "direct_mortality"
  )
}

cvd_calibration <- lapply(names(CVD_CAUSE_MAP), function(mid) {
  message("  ", mid, " (", CVD_CAUSE_MAP[[mid]], ") ...")
  if (CVD_MODEL_TYPE[[mid]] == "direct_mortality")
    calibrate_direct_mortality_cause(CVD_CAUSE_MAP[[mid]])
  else
    calibrate_cvd_cause(CVD_CAUSE_MAP[[mid]])
})
names(cvd_calibration) <- names(CVD_CAUSE_MAP)

ihd_tpm <- cvd_calibration$ihd$tpm   # alias used in plots below

################################################################################
# 3  VALIDATE CVD CALIBRATION
################################################################################

message("\n── Validating all 4 CVD causes ─────────────────────────────────────────")

# One-step simulation from GBD prevalence; compares module mx to GBD mx.
validate_cvd_cause <- function(tpm_list, sex_label, gbd_cause_name,
                                val_ages = VAL_AGES) {
  s_idx   <- if (sex_label == "Female") 1L else 2L
  gbd_cvd <- gbd_full |> filter(sex == sex_label, cause == gbd_cause_name)
  mx_5yr   <- gbd_cvd |> filter(measure == "Deaths") |>
    select(age_mid, mx_gbd = rate_per100k) |> mutate(mx_gbd = mx_gbd / 1e5)
  prev_5yr <- gbd_cvd |> filter(measure == "Prevalence") |>
    select(age_mid, prev_gbd = rate_per100k) |> mutate(prev_gbd = prev_gbd / 1e5)
  inc_5yr  <- gbd_cvd |> filter(measure == "Incidence") |>
    select(age_mid, inc_gbd = rate_per100k) |> mutate(inc_gbd = inc_gbd / 1e5)
  if (nrow(inc_5yr) == 0) {
    ACUTE_CFR_HHD <- 0.05
    inc_5yr <- mx_5yr |> mutate(inc_gbd = mx_gbd / ACUTE_CFR_HHD) |>
      select(age_mid, inc_gbd)
  }
  mx_1yr   <- interp_piecewise_constant(mx_5yr$age_mid,   mx_5yr$mx_gbd)
  prev_1yr <- interp_piecewise_constant(prev_5yr$age_mid, prev_5yr$prev_gbd)
  inc_1yr  <- interp_piecewise_constant(inc_5yr$age_mid,  inc_5yr$inc_gbd)

  map_dfr(val_ages, function(a) {
    idx   <- a + 1L; tpm <- tpm_list[[as.character(a)]]
    prev  <- pmin(prev_1yr[idx], 0.999)
    p_inc <- pmin((1 - prev) * (1 - exp(-pmax(inc_1yr[idx], 0))), 0.20)
    sv    <- c(well = pmax(1 - prev - p_inc, 0), incident = p_inc,
               prevalent = prev, dead_cause = 0, dead_bg = 0)
    sv    <- sv / sum(sv)
    sv2   <- as.numeric(sv %*% tpm)
    tibble(sex = sex_label, age = a,
           mx_gbd    = mx_1yr[idx] * 1e5,
           mx_module = (sv2[4] - sv[4]) * 1e5,
           rel_error = ((sv2[4] - sv[4]) - mx_1yr[idx]) / pmax(mx_1yr[idx], 1e-9))
  })
}

validate_direct_mortality_cause <- function(calib, gbd_cause_name,
                                             val_ages = VAL_AGES) {
  bind_rows(lapply(SEXES, function(sex_label) {
    gbd_dm <- gbd_full |> filter(sex == sex_label, cause == gbd_cause_name)
    mx_5yr <- gbd_dm |> filter(measure == "Deaths") |>
      select(age_mid, mx_gbd = rate_per100k) |> mutate(mx_gbd = mx_gbd / 1e5)
    mx_gbd_1yr    <- interp_piecewise_constant(mx_5yr$age_mid, mx_5yr$mx_gbd)
    mx_module_1yr <- calib$base_mx_cause[[sex_label]]
    map_dfr(val_ages, function(a) {
      idx <- a + 1L
      tibble(sex = sex_label, age = a,
             mx_gbd    = mx_gbd_1yr[idx] * 1e5,
             mx_module = mx_module_1yr[idx] * 1e5,
             rel_error = (mx_module_1yr[idx] - mx_gbd_1yr[idx]) /
               pmax(mx_gbd_1yr[idx], 1e-9))
    })
  }))
}

val_summary <- list()
for (mid in names(CVD_CAUSE_MAP)) {
  gn <- CVD_CAUSE_MAP[[mid]]
  if (CVD_MODEL_TYPE[[mid]] == "direct_mortality") {
    res_all <- validate_direct_mortality_cause(cvd_calibration[[mid]], gn)
    for (sx in SEXES) {
      res   <- res_all |> filter(sex == sx)
      pass  <- sum(abs(res$rel_error) <= TOL)
      message(sprintf("  %-20s %-6s : %d/%d within %d%% tol",
                       mid, sx, pass, nrow(res), round(TOL * 100)))
      val_summary[[paste(mid, sx)]] <- res
    }
    next
  }
  for (sx in SEXES) {
    res  <- validate_cvd_cause(cvd_calibration[[mid]]$tpm[[sx]], sx, gn)
    pass <- sum(abs(res$rel_error) <= TOL)
    rate <- pass / nrow(res)
    msg  <- sprintf("  %-20s %-6s : %d/%d within %d%% tol",
                     mid, sx, pass, nrow(res), round(TOL * 100))
    if (rate < 0.80) {
      if (mid %in% c("ihd", "ischemic_stroke", "ich")) {
        stop(msg, "\n  Core V1 CVD module failed calibration (<80% within tolerance). ",
             "Do not proceed with baseline run — recheck GBD inputs and residual method.")
      } else {
        warning(msg, " — pass rate below 80%, check module before headline outputs.")
      }
    } else {
      message(msg)
    }
    val_summary[[paste(mid, sx)]] <- res
  }
}

write_csv(bind_rows(val_summary, .id = "cause_sex"),
          file.path(OUT_DIR, "cvd_calib_check_all.csv"))

# Convenience object for plots
ihd_check <- bind_rows(
  validate_cvd_cause(ihd_tpm$Female, "Female", "Ischemic heart disease"),
  validate_cvd_cause(ihd_tpm$Male,   "Male",   "Ischemic heart disease")
)

################################################################################
# 4  CALIBRATE CERVICAL CANCER
#
# States: well(1), precancer(2), local(3), regional(4), distant(5),
#         dead_cause(6), dead_bg(7)
#
# CONCORD-3 stage CFRs (annual) are scaled to GBD-implied prevalent-pool
# mortality via ratio correction:
#   cfr_scale = (mx_cervical / prev) / cfr_concord_weighted
#   cfr_adj   = cfr_concord × cfr_scale   (floor 0.10, ceiling 1.0)
# This corrects for CONCORD-3 describing newly diagnosed patients while GBD
# prevalence includes long-term survivors with much lower annual mortality.
################################################################################

message("\n── Calibrating cervical cancer ─────────────────────────────────────────")

# Source: CONCORD-3 Southeast Asia age-standardised 5-yr net survival
CERVICAL_STAGE_CFR <- list(
  local    = 0.063,   # 1 - 0.72^(1/5)
  regional = 0.183,   # 1 - 0.40^(1/5)
  distant  = 0.334    # 1 - 0.17^(1/5)
)

# WHO/ICO Indonesia country profile 2023; reflects late-stage presentation
# typical of ~15% screening coverage
CERVICAL_STAGE_DIST <- list(local = 0.22, regional = 0.40, distant = 0.38)

calibrate_cervical <- function(sex_label = "Female") {
  s_idx    <- 1L   # Female only
  gbd_cerv <- gbd_full |> filter(sex == sex_label, cause == "Cervical cancer")

  inc_5yr  <- gbd_cerv |> filter(measure == "Incidence") |>
    select(age_mid, rate = rate_per100k) |> mutate(rate = rate / 1e5)
  mx_5yr   <- gbd_cerv |> filter(measure == "Deaths") |>
    select(age_mid, rate = rate_per100k) |> mutate(rate = rate / 1e5)
  prev_5yr <- gbd_cerv |> filter(measure == "Prevalence") |>
    select(age_mid, rate = rate_per100k) |> mutate(rate = rate / 1e5)

  inc_1yr  <- interp_piecewise_constant(inc_5yr$age_mid,  inc_5yr$rate)
  mx_1yr   <- interp_piecewise_constant(mx_5yr$age_mid,   mx_5yr$rate)
  prev_1yr <- interp_piecewise_constant(prev_5yr$age_mid, prev_5yr$rate)

  mx_all_1yr <- mx_wpp_2025[s_idx, ]
  bgmx_1yr   <- pmax(mx_all_1yr - mx_1yr, 0)

  plcl_1yr <- prev_1yr * CERVICAL_STAGE_DIST$local
  prgn_1yr <- prev_1yr * CERVICAL_STAGE_DIST$regional
  pdst_1yr <- prev_1yr * CERVICAL_STAGE_DIST$distant

  cfr_concord_weighted <- CERVICAL_STAGE_DIST$local    * CERVICAL_STAGE_CFR$local   +
                          CERVICAL_STAGE_DIST$regional * CERVICAL_STAGE_CFR$regional +
                          CERVICAL_STAGE_DIST$distant  * CERVICAL_STAGE_CFR$distant
  # cfr_concord_weighted ≈ 0.214

  tpm_list <- lapply(AGES, function(a) {
    idx  <- a + 1L
    ir   <- pmax(inc_1yr[idx],  0)
    bgmx <- bgmx_1yr[idx]
    prev <- pmax(prev_1yr[idx], 1e-9)
    plcl <- pmax(plcl_1yr[idx], 1e-9)
    prgn <- pmax(prgn_1yr[idx], 1e-9)
    pdst <- pmax(pdst_1yr[idx], 1e-9)

    # Scale CONCORD-3 CFRs so weighted average matches GBD-implied CFR.
    # Floor at 0.10 preserves stage ordering; ceiling at 1.0 prevents inflation.
    cfr_implied <- pmax(mx_1yr[idx], 0) / pmax(prev, 1e-9)
    cfr_scale   <- if (cfr_concord_weighted > 0)
                     pmax(pmin(cfr_implied / cfr_concord_weighted, 1.0), 0.10)
                   else 1.0
    cfr_lcl <- CERVICAL_STAGE_CFR$local    * cfr_scale
    cfr_rgn <- CERVICAL_STAGE_CFR$regional * cfr_scale
    cfr_dst <- CERVICAL_STAGE_CFR$distant  * cfr_scale

    # Row 1: well
    p_well_prc  <- pmin(ir * 10, 0.30)          # CIN incidence ≈ 10× cancer incidence
    p_well_bgmx <- pmin(bgmx, 1 - p_well_prc)
    p_well_well <- pmax(1 - p_well_prc - p_well_bgmx, 0)

    # Row 2: precancer (progression rate back-solved from cancer incidence / precancer pool)
    pprc       <- inc_1yr[idx] * 3               # precancer prevalence ≈ 3× incident cancer
    p_prc_lcl  <- pmin(ir / pmax(pprc, 1e-9), 0.50)
    p_prc_bgmx <- pmin(bgmx, 1 - p_prc_lcl)
    p_prc_prc  <- pmax(1 - p_prc_lcl - p_prc_bgmx, 0)

    # Row 3: local cancer
    p_lcl_dead <- cfr_lcl
    p_lcl_bgmx <- pmin(bgmx, pmax(0, 0.99 - p_lcl_dead))
    p_lcl_rgn  <- pmax(ir / plcl - cfr_lcl - bgmx, 0.01)
    p_lcl_rgn  <- pmin(p_lcl_rgn, pmax(0, 0.99 - p_lcl_dead - p_lcl_bgmx))
    p_lcl_lcl  <- pmax(1 - p_lcl_rgn - p_lcl_dead - p_lcl_bgmx, 0)

    # Row 4: regional cancer
    inflow_rgn <- plcl * p_lcl_rgn
    p_rgn_dead <- cfr_rgn
    p_rgn_bgmx <- pmin(bgmx, pmax(0, 0.99 - p_rgn_dead))
    p_rgn_dst  <- pmax(inflow_rgn / prgn - cfr_rgn - bgmx, 0.01)
    p_rgn_dst  <- pmin(p_rgn_dst, pmax(0, 0.99 - p_rgn_dead - p_rgn_bgmx))
    p_rgn_rgn  <- pmax(1 - p_rgn_dst - p_rgn_dead - p_rgn_bgmx, 0)

    # Row 5: distant cancer
    p_dst_dead <- cfr_dst
    p_dst_bgmx <- pmin(bgmx, pmax(0, 0.99 - p_dst_dead))
    p_dst_dst  <- pmax(1 - p_dst_dead - p_dst_bgmx, 0)

    # Assemble 7×7 TPM
    tpm <- matrix(0, 7, 7)
    tpm[1,1] <- p_well_well; tpm[1,2] <- p_well_prc; tpm[1,7] <- p_well_bgmx
    tpm[2,2] <- p_prc_prc;  tpm[2,3] <- p_prc_lcl;  tpm[2,7] <- p_prc_bgmx
    tpm[3,3] <- p_lcl_lcl;  tpm[3,4] <- p_lcl_rgn;  tpm[3,6] <- p_lcl_dead; tpm[3,7] <- p_lcl_bgmx
    tpm[4,4] <- p_rgn_rgn;  tpm[4,5] <- p_rgn_dst;  tpm[4,6] <- p_rgn_dead; tpm[4,7] <- p_rgn_bgmx
    tpm[5,5] <- p_dst_dst;  tpm[5,6] <- p_dst_dead; tpm[5,7] <- p_dst_bgmx
    tpm[6,6] <- 1;  tpm[7,7] <- 1

    rs <- rowSums(tpm)
    if (any(abs(rs - 1) > 1e-9))
      warning("Cervical TPM row sums off at age ", a, ": ",
              paste(round(rs, 6), collapse = ", "))
    tpm
  })
  names(tpm_list) <- as.character(AGES)
  tpm_list
}

# Male = NULL: the runtime engine (engine.R make_cervical_module) enforces
# zero male burden via all-well init and identity TPMs.
cervical_tpm <- list(Female = calibrate_cervical("Female"), Male = NULL)

################################################################################
# 5  VALIDATE CERVICAL
################################################################################

validate_cervical <- function(tpm_list, sex_label = "Female", val_ages = 15:69) {
  gbd_mx   <- gbd_full |>
    filter(sex == sex_label, cause == "Cervical cancer", measure == "Deaths") |>
    select(age_mid, mx_gbd = rate_per100k) |> mutate(mx_gbd = mx_gbd / 1e5)
  gbd_prev <- gbd_full |>
    filter(sex == sex_label, cause == "Cervical cancer", measure == "Prevalence") |>
    select(age_mid, prev_gbd = rate_per100k) |> mutate(prev_gbd = prev_gbd / 1e5)
  gbd_inc  <- gbd_full |>
    filter(sex == sex_label, cause == "Cervical cancer", measure == "Incidence") |>
    select(age_mid, inc_gbd = rate_per100k) |> mutate(inc_gbd = inc_gbd / 1e5)

  mx_gbd_1yr   <- interp_piecewise_constant(gbd_mx$age_mid,   gbd_mx$mx_gbd)
  prev_gbd_1yr <- interp_piecewise_constant(gbd_prev$age_mid, gbd_prev$prev_gbd)
  inc_gbd_1yr  <- interp_piecewise_constant(gbd_inc$age_mid,  gbd_inc$inc_gbd)

  map_dfr(val_ages, function(a) {
    idx  <- a + 1L; tpm <- tpm_list[[as.character(a)]]
    prev <- pmin(prev_gbd_1yr[idx], 0.999)
    pprc <- pmin(3 * pmax(inc_gbd_1yr[idx], 0), max(1 - prev, 0))
    sv   <- c(well = pmax(1 - prev - pprc, 0), precancer = pprc,
              local  = prev * CERVICAL_STAGE_DIST$local,
              regional = prev * CERVICAL_STAGE_DIST$regional,
              distant  = prev * CERVICAL_STAGE_DIST$distant,
              dead_cause = 0, dead_bg = 0)
    sv   <- sv / sum(sv)
    sv2  <- as.numeric(sv %*% tpm)
    alive_start <- sum(sv[1:5])
    mx_module   <- if (alive_start < 1e-12) 0 else (sv2[6] - sv[6]) / alive_start
    tibble(sex = sex_label, age = a,
           mx_gbd    = mx_gbd_1yr[idx],
           mx_module = mx_module,
           rel_error = (mx_module - mx_gbd_1yr[idx]) / pmax(mx_gbd_1yr[idx], 1e-9),
           within_tol = abs((mx_module - mx_gbd_1yr[idx]) / pmax(mx_gbd_1yr[idx], 1e-9)) <= TOL)
  })
}

cerv_check  <- validate_cervical(cervical_tpm$Female)
n_pass_cerv <- sum(cerv_check$within_tol)
n_tot_cerv  <- nrow(cerv_check)
message("  Cervical: ", n_pass_cerv, "/", n_tot_cerv,
        " ages within ", TOL * 100, "% tolerance")
if (n_pass_cerv < n_tot_cerv) {
  message("  Ages outside tolerance:")
  print(cerv_check |> filter(!within_tol) |>
          select(sex, age, mx_gbd, mx_module, rel_error) |>
          mutate(across(c(mx_gbd, mx_module), ~ round(.x * 1e5, 3)),
                 rel_error = round(rel_error * 100, 1)))
}

################################################################################
# 6  WRITE OUTPUTS
################################################################################

message("\n── Writing calibration outputs ─────────────────────────────────────────")

saveRDS(cvd_calibration,      file.path(OUT_DIR, "cvd_calibration_v1.rds"))
save(cervical_tpm,    file = file.path(OUT_DIR, "cervical_tpm.Rda"))

message("  cvd_calibration_v1.rds   — 4 CVD causes (TPMs + base rates)")
message("  cervical_tpm.Rda         — cervical 7-state TPM (Female only)")

################################################################################
# 7  CALIBRATION PLOTS
################################################################################

message("\n── Generating calibration plots ────────────────────────────────────────")

theme_cal <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA),
        strip.text       = element_text(face = "bold", size = 9),
        plot.title       = element_text(face = "bold", size = 11),
        plot.subtitle    = element_text(size = 8, colour = "grey40"))

# C1: IHD observed vs fitted mx
p_c1 <- ihd_check |>
  ggplot(aes(x = mx_gbd * 1e5, y = mx_module * 1e5, colour = sex)) +
  geom_abline(slope = 1,    intercept = 0, colour = "grey60", linetype = "dashed") +
  geom_abline(slope = 1.05, intercept = 0, colour = "grey80", linetype = "dotted") +
  geom_abline(slope = 0.95, intercept = 0, colour = "grey80", linetype = "dotted") +
  geom_point(size = 2, alpha = 0.8) +
  geom_text(aes(label = age), size = 2, nudge_x = 0.5, check_overlap = TRUE) +
  scale_colour_manual(values = c(Female = "#C0392B", Male = "#1F6BAE")) +
  facet_wrap(~ sex) +
  labs(title    = "C1: IHD — module mx vs GBD (2023, ages 30–69)",
       subtitle = "Dashed = 1:1 | Dotted = ±5% | per 100,000",
       x = "GBD observed mx", y = "Module predicted mx", colour = NULL) +
  theme_cal + theme(legend.position = "none")
ggsave(file.path(PLOT_DIR, "c1_ihd_obs_vs_fitted.png"), p_c1,
       width = 10, height = 5, dpi = 150)

# C2: Cervical observed vs fitted mx
p_c2 <- cerv_check |>
  ggplot(aes(x = mx_gbd * 1e5, y = mx_module * 1e5)) +
  geom_abline(slope = 1,    intercept = 0, colour = "grey60", linetype = "dashed") +
  geom_abline(slope = 1.05, intercept = 0, colour = "grey80", linetype = "dotted") +
  geom_abline(slope = 0.95, intercept = 0, colour = "grey80", linetype = "dotted") +
  geom_point(size = 2, alpha = 0.8, colour = "#2980B9") +
  geom_text(aes(label = age), size = 2, nudge_x = 0.02, check_overlap = TRUE) +
  labs(title = "C2: Cervical — module mx vs GBD (2023, Female, ages 15–69)",
       x = "GBD observed mx", y = "Module predicted mx") +
  theme_cal
ggsave(file.path(PLOT_DIR, "c2_cervical_obs_vs_fitted.png"), p_c2,
       width = 7, height = 6, dpi = 150)

# C3: Calibration residuals by age
p_c3 <- bind_rows(ihd_check  |> mutate(cause = "IHD"),
                  cerv_check |> mutate(cause = "Cervical cancer")) |>
  ggplot(aes(x = age, y = rel_error * 100, colour = sex)) +
  geom_hline(yintercept = 0,  colour = "grey40") +
  geom_hline(yintercept =  5, linetype = "dashed", colour = "grey60") +
  geom_hline(yintercept = -5, linetype = "dashed", colour = "grey60") +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_line(linewidth = 0.5, alpha = 0.6) +
  facet_wrap(~ cause, scales = "free_x") +
  scale_colour_manual(values = c(Female = "#C0392B", Male = "#1F6BAE")) +
  labs(title    = "C3: Calibration residuals by age",
       subtitle = "(Module mx − GBD mx) / GBD mx × 100 | Dashed = ±5%",
       x = "Age", y = "Relative error (%)", colour = NULL) +
  theme_cal
ggsave(file.path(PLOT_DIR, "c3_calibration_residuals.png"), p_c3,
       width = 12, height = 5, dpi = 150)

# C4: IHD transition probability profiles
tp_ihd <- map_dfr(c("Female","Male"), function(sx) {
  map_dfr(VAL_AGES, function(a) {
    tpm <- ihd_tpm[[sx]][[as.character(a)]]
    tibble(sex = sx, age = a,
           `Well→Incident\n(incidence)` = tpm[1, 2],
           `Incident→Dead\n(acute CFR)` = tpm[2, 4],
           `Prevalent→Dead\n(prev CFR)` = tpm[3, 4],
           `Background\n(bgmx)`         = tpm[1, 5])
  })
}) |> pivot_longer(-c(sex, age), names_to = "transition", values_to = "prob")

p_c4 <- tp_ihd |>
  ggplot(aes(x = age, y = prob, colour = sex)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ transition, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c(Female = "#C0392B", Male = "#1F6BAE")) +
  scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
  labs(title = "C4: IHD transition probability profiles (calibrated 2023)",
       x = "Age", y = "Annual probability", colour = NULL) +
  theme_cal
ggsave(file.path(PLOT_DIR, "c4_ihd_tp_profiles.png"), p_c4,
       width = 10, height = 7, dpi = 150)

message("  Plots saved to: ", normalizePath(PLOT_DIR))
message("\n── 04_calibrate_modules.R complete ─────────────────────────────────────")
message("  Next: scripts/05_run_baseline.R")
