################################################################################
# INDONESIA INTEGRATED NCD MODEL — CAUSE FRACTION FITTING AND PROJECTION
# scripts/03_build_cause_fractions.R
# ─────────────────────────────────────────────────────────────────────────────
# Fits logit-space linear trends through 7 GBD anchor years (2000–2023) for
# each cause × age × sex combination, then projects fractions annually from
# 2000 to 2155 for use in the WPP/GBD integration layer.
#
# The core identity at every projection year t is:
#   mx_cause(t) = mx_WPP(t) × frac_cause(t)
# where frac_cause(t) = invlogit(logit(frac_2023) + slope × (t - 2023))
#
# This mirrors the Countdown delay-curve method (PrepData.R), extended to
# 23 causes and 7 anchor years.
#
# INPUTS:
#   data/gbd/gbd_cause_fractions.csv   (from 01_prepare_gbd_inputs.R)
#   data/gbd/gbd_measures_full.csv     (from 01_prepare_gbd_inputs.R)
#
# OUTPUTS (written to data/gbd/):
#   gbd_frac_fitted.csv        fitted frac_base and logit_slope per
#                              cause × age_mid × sex (5-yr age groups)
#   gbd_frac_annual.csv        projected fractions at every integer year
#                              2000–2100, at 5-yr age groups
#   gbd_frac_annual_1yr.csv    same but interpolated to single-year ages 0–100
#                              (used by the projection engine at runtime)
#   gbd_incidence_fitted.csv   fitted log-incidence slopes for CVD TPM causes
#   gbd_incidence_annual.csv   projected CVD incidence rates at 5-yr age groups
#   gbd_incidence_annual_1yr.csv projected CVD incidence rates at single-year ages
#
# DESIGN NOTES:
#   - 2021 is EXCLUDED from trend fitting. Indonesia was severely affected by
#     COVID-19 in 2021 (Delta wave peak). Including it would pull slopes toward
#     the shock rather than the underlying epidemiological trend.
#     2021 remains in gbd_cause_fractions.csv but is filtered out here.
#
#   - Smithson-Verkuilen (SV) correction applied before logit at every step:
#       frac_adj = (frac + c) / (1 + 2c),  c = 1/(2n) where n = N_ANCHOR = 6
#       c = 1/12 ≈ 0.083
#     This is the principled recommendation from Smithson & Verkuilen (2006,
#     Psychological Methods 11(1):54-71): c = 1/(2n) ties the boundary
#     correction to the number of observations.
#     LOO cross-validation confirmed no meaningful sensitivity to c in
#     [0.01, 0.50]; the S&V formula provides a citable, data-grounded choice.
#
#   - Trend fitting uses mean of consecutive interval slopes:
#       di_k = (logit_sv(frac_{k+1}) - logit_sv(frac_k)) / (year_{k+1} - year_k)
#       di   = mean(di_1 … di_K)
#     With anchor years 2000, 2005, 2010, 2015, 2019, 2023 this gives 5
#     intervals with equal weight. This is more robust than OLS (which can
#     amplify end-of-series shocks) or two-endpoint slope (which discards
#     interior data).
################################################################################

rm(list = ls())

library(here)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(scales)
})

source(here("R", "cause_registry.R"))

MODEL_GBD_CAUSES <- unique(vapply(
  CAUSE_REGISTRY,
  function(x) x$gbd_cause_name,
  character(1)
))

# ── PATHS ─────────────────────────────────────────────────────────────────────
FRAC_FILE     <- here("data", "gbd", "gbd_cause_fractions.csv")
MEASURES_FILE <- here("data", "gbd", "gbd_measures_full.csv")
OUT_DIR       <- here("data", "gbd")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ── CONSTANTS ─────────────────────────────────────────────────────────────────
# 2021 excluded from fitting (COVID-19 mortality shock)
ANCHOR_YEARS <- c(2000, 2005, 2010, 2015, 2019, 2023)
N_ANCHOR     <- length(ANCHOR_YEARS)     # 6

# Smithson-Verkuilen (SV) correction constant: c = 1/(2n)
SV_C <- 1 / (2 * N_ANCHOR)              # ≈ 0.083

# Projection range — run 2000:2155 to cover WPP horizon
PROJ_YEARS_FULL <- 2000:2155
# Annual output for the model engine uses 2000:2100
PROJ_YEARS_OUT  <- 2000:2100

# Reference year for trend anchors: use last calibration anchor
REF_YEAR <- 2023L

# Single-age/sex grid used for WPP-aligned model inputs. These are local
# constants for this script; do not rely on runtime engine globals here.
SEXES <- c("Female", "Male")
AGES  <- 0:100

# Shared trend offset for cause fractions and incidence. The default preserves
# the existing V1 logit/log-slope trajectories. Change this one constant to
# "taper_to_2100" for a damped sensitivity that applies consistently to both
# mortality fractions and incidence.
TREND_DAMPING_MODE <- "linear"  # options: "linear", "taper_to_2100", "flat_after_2050"

projection_offset <- function(year, ref_year = REF_YEAR,
                              mode = TREND_DAMPING_MODE,
                              taper_end = 2100L) {
  year <- as.integer(year)
  if (mode == "linear") {
    return(year - ref_year)
  }
  if (mode == "flat_after_2050") {
    out <- ifelse(year <= ref_year, year - ref_year,
                  pmin(year, 2050L) - ref_year)
    return(out)
  }
  if (mode == "taper_to_2100") {
    vapply(year, function(y) {
      if (y <= ref_year) return(y - ref_year)
      yrs <- seq.int(ref_year + 1L, y)
      # Annual slope weight tapers linearly from 1 after REF_YEAR to 0 at taper_end.
      w <- pmax(0, (taper_end - yrs + 1) / (taper_end - ref_year))
      sum(w)
    }, numeric(1))
  } else {
    stop("Unknown TREND_DAMPING_MODE: ", mode, call. = FALSE)
  }
}

################################################################################
# 1  LOAD AND PREPARE
################################################################################

message("\n── Loading cause fractions ───────────────────────────────────────────────")

frac_raw <- read_csv(FRAC_FILE, show_col_types = FALSE)
message("  Rows loaded   : ", comma(nrow(frac_raw)))
message("  Causes        : ", n_distinct(frac_raw$cause))
message("  Years present : ", paste(sort(unique(frac_raw$year)), collapse = ", "))

# Filter to anchor years, excluding 2021 COVID shock.
# Keep only IDN national (location_id 11) in case raw file has subnational rows.
frac_fit <- frac_raw |>
  filter(
    year        %in% ANCHOR_YEARS,
    location_id == 11L,
    cause       %in% MODEL_GBD_CAUSES
  ) |>
  arrange(cause, sex, age_mid, year)

message("  After filtering to anchor years (excl. 2021): ", comma(nrow(frac_fit)))

################################################################################
# 2  SMITHSON-VERKUILEN HELPERS
# The SV correction maps [0,1] fractions to the open interval before logit.
# Critically, the inverse must undo the SV correction after plogis(); otherwise
# plogis(logit_sv(p)) returns the SV-adjusted scale, not the original scale.
# With c ≈ 0.083, a true zero would back-transform to ~7.1% — inflating rare
# causes and potentially breaching the WPP all-cause envelope.
#
# Correct round-trip:   sv_inv(plogis(logit_sv(p)))  == p  (up to float precision)
# Bug if you write:     plogis(logit_sv(p))           != p
################################################################################

# Forward SV correction: maps [0,1] → (0,1)
sv <- function(p, c = SV_C) {
  (pmin(pmax(p, 0), 1) + c) / (1 + 2 * c)
}

# Inverse SV correction: maps (0,1) → [0,1]
sv_inv <- function(p_adj, c = SV_C) {
  p <- p_adj * (1 + 2 * c) - c
  pmin(pmax(p, 0), 1)
}

# Forward: fraction → logit(sv(fraction))
logit_sv <- function(p, c = SV_C) {
  qlogis(sv(p, c))
}

# Inverse: logit(sv(fraction)) → original fraction scale
inv_logit_sv <- function(x, c = SV_C) {
  sv_inv(plogis(x), c)
}

frac_fit <- frac_fit |>
  mutate(logit_sv = logit_sv(pmax(frac, 0)))

################################################################################
# 3  FIT MEAN-OF-INTERVALS LOGIT SLOPE
# For each cause × sex × age_mid combination, compute the mean of consecutive
# annual interval slopes: di = mean( Δlogit / Δyear ) across anchor years.
################################################################################

message("\n── Fitting logit-space trends ────────────────────────────────────────────")

slopes <- frac_fit |>
  group_by(cause, sex, age_mid) |>
  arrange(year, .by_group = TRUE) |>
  summarise(
    frac_ref    = frac[year == REF_YEAR][1],
    logit_ref   = logit_sv(pmax(frac_ref, 0)),
    # Mean of consecutive interval slopes (robust vs OLS against end-of-series shocks)
    logit_slope = {
      yrs <- year; lgs <- logit_sv
      mean(diff(lgs) / diff(yrs), na.rm = TRUE)
    },
    n_obs = n(),
    .groups = "drop"
  )

message("  Slope rows : ", comma(nrow(slopes)))
message("  Slope range: [",
        round(min(slopes$logit_slope, na.rm = TRUE), 4), ", ",
        round(max(slopes$logit_slope, na.rm = TRUE), 4), "]")

bad_slopes <- slopes |>
  dplyr::filter(
    is.na(frac_ref) |
      is.na(logit_ref) |
      is.na(logit_slope) |
      n_obs < 2
  )

if (nrow(bad_slopes) > 0) {
  stop(
    "Cause-fraction projection has insufficient anchor data. First rows:\n",
    paste(utils::capture.output(print(utils::head(bad_slopes, 20))), collapse = "\n"),
    call. = FALSE
  )
}

################################################################################
# 4  PROJECT FRACTIONS ANNUALLY
# For each year t:
#   logit_frac(t) = logit_ref + slope × (t - REF_YEAR)
#   frac(t)       = inv_logit_sv(logit_frac(t))   ← must invert SV correction
#
# MUST use inv_logit_sv(), not plogis() directly. plogis() returns the
# SV-adjusted scale; inv_logit_sv() back-transforms to the original scale.
################################################################################

message("\n── Projecting annual fractions ───────────────────────────────────────────")

frac_annual <- slopes |>
  crossing(year = PROJ_YEARS_OUT) |>
  mutate(
    trend_offset = projection_offset(year),
    logit_frac   = logit_ref + logit_slope * trend_offset,
    frac         = inv_logit_sv(logit_frac),
    frac         = pmin(pmax(frac, 0), 1),
    damping_mode = TREND_DAMPING_MODE
  ) |>
  select(cause, sex, age_mid, year, frac, logit_frac, logit_slope,
         trend_offset, damping_mode) |>
  arrange(cause, sex, age_mid, year)

message("  frac_annual rows: ", comma(nrow(frac_annual)))
message("  Year range      : ", min(frac_annual$year), "–", max(frac_annual$year))

# Anchor reproduction test: at REF_YEAR, projected fracs must equal frac_ref
# (up to floating-point). If this fails, the SV round-trip is broken.
anchor_check <- frac_annual |>
  filter(year == REF_YEAR) |>
  left_join(slopes |> select(cause, sex, age_mid, frac_ref),
            by = c("cause", "sex", "age_mid")) |>
  summarise(max_abs_error = max(abs(frac - frac_ref), na.rm = TRUE))

if (anchor_check$max_abs_error > 1e-6) {
  stop(sprintf(
    "Anchor reproduction FAILED: max |frac(REF_YEAR) - frac_ref| = %.2e (threshold 1e-6).\n",
    anchor_check$max_abs_error),
    "Check inv_logit_sv() and logit_sv() implementation."
  )
}
message(sprintf("  Anchor test passed: max |frac(%d) - frac_ref| = %.2e ✓", REF_YEAR,
                anchor_check$max_abs_error))

################################################################################
# 5  INTERPOLATE TO SINGLE-YEAR AGES
# Each single-year age maps to its enclosing 5-year GBD age group
# (piecewise-constant / block assignment — matching engine.R approach).
# Output: one row per cause × sex × single-year age × projection year.
################################################################################

message("\n── Interpolating to single-year ages ─────────────────────────────────────")

# GBD age-group boundaries (lower bounds of each group)
AGE_BREAKS <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45,
                50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 101)

# For each single-year age, find the enclosing GBD 5-yr group midpoint
gbd_mids <- sort(unique(frac_annual$age_mid))

assign_gbd_mid <- function(age) {
  b <- findInterval(age, AGE_BREAKS, rightmost.closed = TRUE)
  b <- min(max(b, 1L), length(gbd_mids))
  gbd_mids[b]
}

age_map <- tibble(
  age     = AGES,
  age_mid = vapply(AGES, assign_gbd_mid, numeric(1))
)

frac_annual_1yr <- age_map |>
  left_join(frac_annual, by = "age_mid", relationship = "many-to-many") |>
  select(cause, sex, age, age_mid, year, frac, logit_frac, logit_slope,
         trend_offset, damping_mode) |>
  arrange(cause, sex, age, year)

# Normalization cap: if total cause fractions across all causes at a given
# age × sex × year exceed 0.99, scale proportionally so the engine always has
# positive residual for background mortality. Fractions already below 0.99 are
# unchanged (frac_sum / 0.99 < 1 → multiplier > 1 → no reduction).
CAP <- 0.99
frac_annual_1yr <- frac_annual_1yr |>
  group_by(sex, age, year) |>
  mutate(
    frac_sum = sum(frac, na.rm = TRUE),
    frac     = if_else(frac_sum > CAP, frac * CAP / frac_sum, frac)
  ) |>
  ungroup() |>
  select(-frac_sum)

message("  frac_annual_1yr rows: ", comma(nrow(frac_annual_1yr)))

################################################################################
# 6  PROJECT CVD INCIDENCE RATES
# The CVD Markov modules need an explicit incidence driver so the annual
# WPP x cause-fraction mortality target does not force all change into hidden
# CFR inflation. Incidence is projected in log-rate space using the same
# anchor years, same mean-of-consecutive-interval slopes, and same trend offset
# convention as the cause-fraction layer. HHD has no GBD incidence and remains
# a direct-mortality module, so it is represented with method
# "direct_mortality_no_incidence" and NA inc_rate.
################################################################################

message("\n── Projecting CVD incidence-rate targets ───────────────────────────────")

if (!file.exists(MEASURES_FILE)) {
  stop("Missing ", MEASURES_FILE,
       ". Run scripts/01_prepare_gbd_inputs.R before script 03.", call. = FALSE)
}

module_map <- tibble::tibble(
  module     = names(CVD_CAUSE_MAP),
  cause      = unname(unlist(CVD_CAUSE_MAP)),
  model_type = unname(unlist(CVD_MODEL_TYPE[names(CVD_CAUSE_MAP)]))
)

tpm_module_map <- module_map |>
  dplyr::filter(model_type != "direct_mortality")

measures_raw <- readr::read_csv(MEASURES_FILE, show_col_types = FALSE)

inc_fit <- measures_raw |>
  dplyr::filter(
    year %in% ANCHOR_YEARS,
    location_id == 11L,
    measure == "Incidence"
  ) |>
  dplyr::inner_join(tpm_module_map, by = "cause") |>
  dplyr::mutate(
    inc_rate = pmax(rate_per100k / 1e5, 1e-12),
    log_inc  = log(inc_rate)
  ) |>
  dplyr::arrange(module, cause, sex, age_mid, year)

message("  Incidence anchor rows: ", scales::comma(nrow(inc_fit)))

inc_slopes <- inc_fit |>
  dplyr::group_by(module, cause, sex, age_mid) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::summarise(
    inc_ref = inc_rate[year == REF_YEAR][1],
    log_inc_ref = log(pmax(inc_ref, 1e-12)),
    log_inc_slope = {
      yrs <- year; lgs <- log_inc
      mean(diff(lgs) / diff(yrs), na.rm = TRUE)
    },
    n_obs = dplyr::n(),
    projection_method = "log_rate_mean_interval_slope",
    damping_mode = TREND_DAMPING_MODE,
    .groups = "drop"
  )

bad_inc_slopes <- inc_slopes |>
  dplyr::filter(is.na(inc_ref) | is.na(log_inc_ref) | is.na(log_inc_slope) | n_obs < 2)

if (nrow(bad_inc_slopes) > 0) {
  stop(
    "Incidence projection has insufficient anchor data. First rows:\n",
    paste(utils::capture.output(print(utils::head(bad_inc_slopes, 20))), collapse = "\n"),
    call. = FALSE
  )
}

inc_annual <- inc_slopes |>
  tidyr::crossing(year = PROJ_YEARS_OUT) |>
  dplyr::mutate(
    trend_offset = projection_offset(year),
    log_inc      = log_inc_ref + log_inc_slope * trend_offset,
    inc_rate     = pmax(exp(log_inc), 0)
  ) |>
  dplyr::select(module, cause, sex, age_mid, year, inc_rate, log_inc,
                inc_ref, log_inc_ref, log_inc_slope, trend_offset,
                projection_method, damping_mode, n_obs) |>
  dplyr::arrange(module, cause, sex, age_mid, year)

inc_anchor_check <- inc_annual |>
  dplyr::filter(year == REF_YEAR) |>
  dplyr::left_join(inc_slopes |> dplyr::select(module, cause, sex, age_mid, inc_ref),
                   by = c("module", "cause", "sex", "age_mid")) |>
  dplyr::summarise(max_abs_error = max(abs(inc_rate - inc_ref.y), na.rm = TRUE))

if (inc_anchor_check$max_abs_error > 1e-10) {
  stop(sprintf(
    "Incidence anchor reproduction FAILED: max |inc(%d) - inc_ref| = %.2e.",
    REF_YEAR, inc_anchor_check$max_abs_error
  ), call. = FALSE)
}

inc_annual_1yr <- inc_annual |>
  dplyr::left_join(age_map, by = "age_mid", relationship = "many-to-many") |>
  dplyr::select(module, cause, sex, age, age_mid, year, inc_rate, log_inc,
                inc_ref, log_inc_ref, log_inc_slope, trend_offset,
                projection_method, damping_mode, n_obs) |>
  dplyr::arrange(module, cause, sex, age, year)

# HHD placeholder: the runtime module is direct_mortality and does not consume IR.
hhd_placeholder <- tidyr::expand_grid(
  module = "hhd",
  cause  = CVD_CAUSE_MAP[["hhd"]],
  sex    = SEXES,
  age    = AGES,
  year   = PROJ_YEARS_OUT
) |>
  dplyr::left_join(age_map, by = "age") |>
  dplyr::mutate(
    inc_rate = NA_real_,
    log_inc = NA_real_,
    inc_ref = NA_real_,
    log_inc_ref = NA_real_,
    log_inc_slope = NA_real_,
    trend_offset = projection_offset(year),
    projection_method = "direct_mortality_no_incidence",
    damping_mode = TREND_DAMPING_MODE,
    n_obs = NA_integer_
  ) |>
  dplyr::select(module, cause, sex, age, age_mid, year, inc_rate, log_inc,
                inc_ref, log_inc_ref, log_inc_slope, trend_offset,
                projection_method, damping_mode, n_obs)

inc_annual_1yr <- dplyr::bind_rows(inc_annual_1yr, hhd_placeholder) |>
  dplyr::arrange(module, sex, age, year)

message("  inc_annual rows      : ", scales::comma(nrow(inc_annual)))
message("  inc_annual_1yr rows  : ", scales::comma(nrow(inc_annual_1yr)))
message("  Incidence damping    : ", TREND_DAMPING_MODE)

################################################################################
# 7  WRITE OUTPUTS
################################################################################

message("\n── Writing outputs ───────────────────────────────────────────────────────")

write_csv(
  slopes |> select(cause, sex, age_mid, frac_ref, logit_ref, logit_slope, n_obs),
  file.path(OUT_DIR, "gbd_frac_fitted.csv")
)

write_csv(frac_annual,     file.path(OUT_DIR, "gbd_frac_annual.csv"))
write_csv(frac_annual_1yr, file.path(OUT_DIR, "gbd_frac_annual_1yr.csv"))
write_csv(inc_slopes,      file.path(OUT_DIR, "gbd_incidence_fitted.csv"))
write_csv(inc_annual,      file.path(OUT_DIR, "gbd_incidence_annual.csv"))
write_csv(inc_annual_1yr,  file.path(OUT_DIR, "gbd_incidence_annual_1yr.csv"))

message("  gbd_frac_fitted.csv          — ", comma(nrow(slopes)),          " rows ✓")
message("  gbd_frac_annual.csv          — ", comma(nrow(frac_annual)),     " rows ✓")
message("  gbd_frac_annual_1yr.csv      — ", comma(nrow(frac_annual_1yr)), " rows ✓")
message("  gbd_incidence_fitted.csv     — ", comma(nrow(inc_slopes)),      " rows ✓")
message("  gbd_incidence_annual.csv     — ", comma(nrow(inc_annual)),      " rows ✓")
message("  gbd_incidence_annual_1yr.csv — ", comma(nrow(inc_annual_1yr)),  " rows ✓")

################################################################################
# 8  SENSE CHECKS
################################################################################

message("\n── Sense checks ──────────────────────────────────────────────────────────")

frac_sum_check <- frac_annual_1yr |>
  group_by(sex, age, year) |>
  summarise(frac_sum = sum(frac, na.rm = TRUE), .groups = "drop")

message(sprintf("  Fraction sum range: [%.4f, %.4f]",
                min(frac_sum_check$frac_sum), max(frac_sum_check$frac_sum)))

# After normalization, no cell should exceed CAP. Any violation indicates a bug.
n_over_cap <- sum(frac_sum_check$frac_sum > CAP + 1e-9, na.rm = TRUE)
if (n_over_cap > 0) {
  stop(n_over_cap, " cells where sum(frac) > ", CAP,
       " after normalization. This is a bug — normalization should have caught this.")
} else {
  message("  All fraction sums ≤ ", CAP, " after normalization ✓")
}

# IHD fraction trajectory: should decline or stay stable over time
message("\n  IHD fraction trajectory — Female, age 60:")
frac_annual_1yr |>
  filter(cause == "Ischemic heart disease", sex == "Female", age == 60,
         year %in% c(2025, 2050, 2075, 2100)) |>
  select(year, frac) |>
  mutate(frac = round(frac, 4)) |>
  print()

message("\n── 03_build_cause_fractions.R complete ──────────────────────────────────")
message("  Key output for projection engine: gbd_frac_annual_1yr.csv")
message("  Incidence targets for CVD TPM modules: gbd_incidence_annual_1yr.csv")
message("  Next: 04_calibrate_modules.R")
