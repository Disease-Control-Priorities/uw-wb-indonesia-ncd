################################################################################
# INDONESIA INTEGRATED NCD MODEL — RUN BASELINE V1
# scripts/05_run_baseline.R
# ─────────────────────────────────────────────────────────────────────────────
# V1 scope: 4 CVD causes (IHD, ischemic stroke, ICH, HHD) + cervical cancer.
# Year-varying CVD TPMs:
#   - mx_cause(t) remains anchored to WPP mx × projected GBD cause fraction
#   - CVD incidence rates are projected explicitly from GBD incidence history
#   - baseline annual CFRs are derived residually from current state counts
#     and then stored for scenario runs, so scenarios do not re-solve CFR
#
# PREREQUISITES (must have been run in order):
#   01_prepare_gbd_inputs.R   → data/gbd/gbd_measures_full.csv
#   02_build_demography.R     → data/wpp/indonesia_ncd_demography.Rda
#   03_build_cause_fractions.R → data/gbd/gbd_frac_annual_1yr.csv
#                                data/gbd/gbd_incidence_annual_1yr.csv
#   04_calibrate_modules.R    → data/model/calibration/cvd_calibration_v1.rds
#                                data/model/calibration/cervical_tpm.Rda
################################################################################

rm(list = ls())

if (!requireNamespace("here", quietly = TRUE))
  stop("Package 'here' is required. Install with: install.packages('here')", call. = FALSE)
source(here::here("R", "packages.R"))
library(here)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(scales)
  library(abind)
})

source(here("R", "engine.R"))
source(here("R", "cause_registry.R"))   # provides CVD_CAUSE_MAP, CVD_MODEL_TYPE, SHARED_PARAMS
source(here("R", "validation.R"))       # provides run_core_invariant_suite()

# ── PATHS ─────────────────────────────────────────────────────────────────────
WPP_FILE  <- here("data", "wpp", "indonesia_ncd_demography.Rda")
CALIB_DIR <- here("data", "model", "calibration")
GBD_FILE  <- here("data", "gbd", "gbd_measures_full.csv")
FRAC_FILE <- here("data", "gbd", "gbd_frac_annual_1yr.csv")
INC_FILE  <- here("data", "gbd", "gbd_incidence_annual_1yr.csv")
OUT_DIR   <- here("data", "model", "baseline")
PLOT_DIR  <- file.path(OUT_DIR, "plots")
dir.create(OUT_DIR,  recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

# Projection and warm-up years come from SHARED_PARAMS (defined in cause_registry.R).
# Edit SHARED_PARAMS there — not here — to ensure all scripts stay synchronised.
PROJ_YEARS <- SHARED_PARAMS$run_years      # 2025:2100

# WARMUP_CYCLES is defined centrally in cause_registry.R. V1 initializes
# modules from GBD prevalence/incidence anchors, so warmup is disabled by
# default to avoid moving the baseline away from the GBD/WPP target.
WARMUP_CYCLES <- SHARED_PARAMS$warmup_cycles

################################################################################
# 1  LOAD INPUTS
################################################################################

message("\n── Loading inputs ───────────────────────────────────────────────────────")

load(WPP_FILE)   # provides: sf.wpp, get.lt, locations
p_idn <- sf.wpp[["IDN"]]

cvd_calib     <- readRDS(file.path(CALIB_DIR, "cvd_calibration_v1.rds"))
load(file.path(CALIB_DIR, "cervical_tpm.Rda"))     # provides: cervical_tpm
gbd_full      <- read_csv(GBD_FILE,  show_col_types = FALSE) |>
                   filter(year == SHARED_PARAMS$calib_year)
cause_frac_df <- read_csv(FRAC_FILE, show_col_types = FALSE)
if (!file.exists(INC_FILE)) {
  stop("Missing ", INC_FILE,
       ". Re-run scripts/03_build_cause_fractions.R; it now writes projected CVD incidence targets.",
       call. = FALSE)
}
incidence_rate_df <- read_csv(INC_FILE, show_col_types = FALSE)

message("  WPP years (IDN)  : ", p_idn$years[1], "–", p_idn$years[length(p_idn$years)])
message("  Female mx(60,2025): ", round(p_idn$mx[1L, 1L, 61L], 5))
message("  GBD rows at ", SHARED_PARAMS$calib_year, " : ", comma(nrow(gbd_full)))
message("  Cause fractions  : ", comma(nrow(cause_frac_df)),
        " rows | ", n_distinct(cause_frac_df$cause), " causes | years ",
        min(cause_frac_df$year), "–", max(cause_frac_df$year))
message("  Incidence targets: ", comma(nrow(incidence_rate_df)),
        " rows | modules ", paste(sort(unique(incidence_rate_df$module)), collapse = ", "),
        " | years ", min(incidence_rate_df$year), "–", max(incidence_rate_df$year))

################################################################################
# 2  BUILD DEMOGRAPHIC BACKBONE
# project_population_exposure() replicates the get.par() CCPM projection to
# produce a [year × sex × age] population count array.
# We extend the WPP object to include year 2101 so that run_cause_module()
# has a closing interval for the last projection year (2100).
################################################################################

message("\n── Building demographic backbone ───────────────────────────────────────")

n_end <- which(p_idn$years == 2101L)
if (length(n_end) == 0) {
  # WPP data ends at 2100: extend by one year, holding final-year values constant
  n_end <- which(p_idn$years == 2100L)
  stopifnot(length(n_end) > 0)
  p_extended <- list(
    years    = c(p_idn$years[1:n_end], 2101L),
    base.pop = p_idn$base.pop,
    mx       = abind(p_idn$mx[1:n_end, , ],
                     array(p_idn$mx[n_end, , ], dim = c(1, 2, 101)), along = 1),
    mig      = abind(p_idn$mig[1:n_end, , ],
                     array(p_idn$mig[n_end, , ], dim = c(1, 2, 101)), along = 1),
    asfr     = rbind(p_idn$asfr[1:n_end, ], p_idn$asfr[n_end, ]),
    srb      = c(p_idn$srb[1:n_end], p_idn$srb[n_end])
  )
} else {
  p_extended <- list(
    years    = p_idn$years[1:n_end],
    base.pop = p_idn$base.pop,
    mx       = p_idn$mx[1:n_end, , ],
    mig      = p_idn$mig[1:n_end, , ],
    asfr     = p_idn$asfr[1:n_end, ],
    srb      = p_idn$srb[1:n_end]
  )
}

pop_array <- project_population_exposure(p_extended, get_lt_fn = get.lt)

message("  Backbone years  : ", min(p_extended$years), "–", max(p_extended$years))
message("  Population array: ", paste(dim(pop_array), collapse = " × "))
message("  Total pop 2025  : ", comma(round(sum(pop_array[1L, , ]))))

if (abs(sum(pop_array[1L, , ]) - sum(p_idn$base.pop)) > 1)
  stop("Backbone 2025 population != WPP base.pop. ",
       "Check project_population_exposure() in engine.R.")

# Long-format WPP mortality rates (used by compute_q4030 and validation)
wpp_mx_df <- bind_rows(lapply(seq_along(p_extended$years), function(ti) {
  yr <- p_extended$years[ti]
  bind_rows(
    tibble(year = yr, sex = "Female", age = AGES, mx_wpp = p_extended$mx[ti, 1L, ]),
    tibble(year = yr, sex = "Male",   age = AGES, mx_wpp = p_extended$mx[ti, 2L, ])
  )
}))

# Long-format population (used by compute_deaths_averted)
pop_df <- bind_rows(lapply(seq_along(p_extended$years), function(ti) {
  yr <- p_extended$years[ti]
  bind_rows(
    tibble(year = yr, sex = "Female", age = AGES, pop = pop_array[ti, 1L, ]),
    tibble(year = yr, sex = "Male",   age = AGES, pop = pop_array[ti, 2L, ])
  )
}))

# Authoritative reporting baseline for active modules:
# WPP all-cause mx x projected GBD cause fraction.
ACTIVE_MODULE_CAUSE_MAP <- c(
  unlist(CVD_CAUSE_MAP, use.names = TRUE),
  cervical_ca = "Cervical cancer"
)

MODULES_TO_ANCHOR <- names(ACTIVE_MODULE_CAUSE_MAP)

target_mx_df <- make_module_target_mx(
  cause_frac_df    = cause_frac_df,
  wpp_mx_df        = dplyr::filter(wpp_mx_df, year %in% PROJ_YEARS),
  pop_df           = dplyr::filter(pop_df, year %in% PROJ_YEARS),
  module_cause_map = ACTIVE_MODULE_CAUSE_MAP,
  years            = PROJ_YEARS
)

################################################################################
# 3  BUILD MODULE SPECS
# CVD_CAUSE_MAP and CVD_MODEL_TYPE are defined in R/cause_registry.R.
# make_cvd_module() and make_cervical_module() are defined in R/engine.R.
################################################################################

message("\n── Building module specs ───────────────────────────────────────────────")

cvd_modules <- imap(CVD_CAUSE_MAP, function(gbd_name, mid) {
  if (CVD_MODEL_TYPE[[mid]] == "direct_mortality") {
    make_direct_cvd_mortality_module(
      module_id      = mid,
      gbd_cause_name = gbd_name,
      cause_frac_df  = cause_frac_df,
      wpp_mx_df      = wpp_mx_df
    )
  } else {
    make_cvd_module(
      module_id      = mid,
      gbd_cause_name = gbd_name,
      cvd_tpm        = cvd_calib[[mid]]$tpm,
      gbd_full_df    = gbd_full,
      cause_frac_df     = cause_frac_df,
      wpp_mx_df         = wpp_mx_df,
      base_mx_cause     = cvd_calib[[mid]]$base_mx_cause,
      base_bgmx         = cvd_calib[[mid]]$base_bgmx,
      incidence_rate_df = incidence_rate_df
    )
  }
})

cerv_mod    <- make_cervical_module(cervical_tpm, gbd_full)
all_modules <- c(cvd_modules, list(cervical_ca = cerv_mod))

message("  Modules active    : ", paste(names(all_modules), collapse = ", "))
message("  Year-varying TPMs : CVD projected IR + residual CFR schedule; mortality target = WPP x fraction")

################################################################################
# 4  RUN BASELINE
################################################################################

message("\n── Running baseline (", length(all_modules),
        " modules, ", min(PROJ_YEARS), "–", max(PROJ_YEARS), ") ────────────────")

baseline_scenario <- list(
  bp_on = FALSE, statins_on = FALSE, sodium_on = FALSE,
  tfa_on = FALSE, diabetes_bp_on = FALSE, cancer_on = FALSE,
  # Baseline pass derives and stores the annual CVD CFR schedule from projected
  # IR + WPP x fraction mortality target. Scenario runs must use this fixed
  # schedule and must not re-solve CFR.
  derive_baseline_cfr_schedule = TRUE
)

baseline_result <- run_module_set(
  modules      = all_modules,
  pop_array    = pop_array,
  proj_years   = PROJ_YEARS,
  warmup_cycles = seq_len(WARMUP_CYCLES),   # engine uses length() only; year values unused
  scenario     = baseline_scenario
)

mx_baseline_raw <- baseline_result$mx

cvd_tpm_schedule <- dplyr::bind_rows(lapply(cvd_modules, function(mod) {
  if (!is.null(mod$get_baseline_tpm_schedule)) mod$get_baseline_tpm_schedule() else tibble::tibble()
}))

cvd_tpm_diagnostics <- dplyr::bind_rows(lapply(cvd_modules, function(mod) {
  if (!is.null(mod$get_baseline_tpm_diagnostics)) mod$get_baseline_tpm_diagnostics() else tibble::tibble()
}))

validate_cvd_annual_tpm_diagnostics(cvd_tpm_diagnostics)
message("  Annual CVD residual-CFR schedule rows: ", comma(nrow(cvd_tpm_schedule)))

mx_baseline <- anchor_baseline_mx_to_targets(
  mx_df             = mx_baseline_raw,
  target_mx_df      = target_mx_df,
  modules_to_anchor = MODULES_TO_ANCHOR
)

validate_baseline_anchor(
  mx_df         = mx_baseline,
  target_mx_df = target_mx_df,
  modules      = MODULES_TO_ANCHOR
)

baseline_anchor_diag <- summarise_anchor_diagnostics(
  raw_mx_df      = mx_baseline_raw,
  anchored_mx_df = mx_baseline,
  target_mx_df   = target_mx_df
)

message("  Total output rows : ", comma(nrow(mx_baseline)))
message("  Modules completed : ", paste(unique(mx_baseline$module), collapse = ", "))
message("  Baseline anchored : WPP mx x projected GBD cause fraction ✓")

################################################################################
# 5  VALIDATE
################################################################################

message("\n── Validating outputs ──────────────────────────────────────────────────")

assert_zero_male_cervical(mx_baseline)
message("  Male cervical mx = 0 ✓")

run_core_invariant_suite(
  pop_array           = pop_array,
  pop_df              = filter(pop_df, year %in% PROJ_YEARS),
  wpp_mx_df           = filter(wpp_mx_df, year %in% PROJ_YEARS),
  mx_df               = mx_baseline,
  p_wpp               = p_extended,
  expected_start_year = 2025L
)
message("  Invariant suite passed ✓")

message("\n── CVD module envelope checks ──────────────────────────────────────────")
for (mid in names(CVD_CAUSE_MAP)) {
  env_chk <- mx_baseline |>
    filter(module == mid) |>
    left_join(filter(wpp_mx_df, year %in% PROJ_YEARS),
              by = c("year", "sex", "age")) |>
    summarise(cells = n(),
              viol  = sum(mx_cause > mx_wpp + 1e-12, na.rm = TRUE),
              max_r = round(max(mx_cause / pmax(mx_wpp, 1e-12), na.rm = TRUE), 4))
  message(sprintf("  %-20s cells: %d | violations: %d | max ratio: %.4f",
                  mid, env_chk$cells, env_chk$viol, env_chk$max_r))
  if (env_chk$viol > 0)
    stop("WPP envelope violation in baseline module '", mid, "' (",
         env_chk$viol, " cells). ",
         "Cause-specific mx exceeds all-cause mx — check cause fractions and calibration.")
}

message("\n  Spot checks (Female, age 60, 2025):")
spot <- mx_baseline |> filter(year == 2025, sex == "Female", age == 60)
for (mid in c("ihd", "ischemic_stroke", "ich", "hhd", "cervical_ca")) {
  v <- spot$mx_cause[spot$module == mid]
  if (length(v)) message(sprintf("    %-20s : %.3f per 100k", mid, v * 1e5))
}

################################################################################
# 6  40q30
################################################################################

message("\n── Computing 40q30 ─────────────────────────────────────────────────────")

q4030_baseline <- compute_q4030(
  mx_df          = mx_baseline,
  wpp_mx_df      = filter(wpp_mx_df, year %in% PROJ_YEARS),
  baseline_mx_df = NULL   # baseline: no delta adjustment needed
)

cat("\n  All-cause 40q30:\n")
q4030_baseline |>
  filter(year %in% c(2025, 2050), cause == "All-cause") |>
  select(year, sex, q4030_pct) |>
  print()

################################################################################
# 7  WRITE OUTPUTS
################################################################################

message("\n── Writing outputs ─────────────────────────────────────────────────────")

# Non-NCD mx: all-cause minus modelled NCD causes (needed by 06_run_scenarios.R)
mx_ncd_sum <- mx_baseline |>
  group_by(year, sex, age) |>
  summarise(mx_ncd = sum(mx_cause, na.rm = TRUE), .groups = "drop")

mx_nonncd <- wpp_mx_df |>
  filter(year %in% PROJ_YEARS) |>
  left_join(mx_ncd_sum, by = c("year", "sex", "age")) |>
  mutate(mx_ncd = replace_na(mx_ncd, 0),
         mx_nonncd = pmax(mx_wpp - mx_ncd, 0))

write_csv(mx_baseline,          file.path(OUT_DIR, "baseline_mx_module.csv"))
write_csv(mx_baseline_raw,      file.path(OUT_DIR, "baseline_mx_module_raw_state_model.csv"))
write_csv(target_mx_df,         file.path(OUT_DIR, "baseline_mx_module_targets.csv"))
write_csv(baseline_anchor_diag, file.path(OUT_DIR, "baseline_anchor_diagnostics.csv"))
write_csv(cvd_tpm_schedule,     file.path(OUT_DIR, "baseline_cvd_tpm_schedule.csv"))
write_csv(cvd_tpm_diagnostics,  file.path(OUT_DIR, "baseline_cvd_tpm_diagnostics.csv"))
write_csv(mx_nonncd,            file.path(OUT_DIR, "baseline_mx_nonncd.csv"))
write_csv(q4030_baseline,       file.path(OUT_DIR, "baseline_q4030.csv"))
saveRDS(pop_df,                 file.path(OUT_DIR, "pop_df.rds"))
saveRDS(wpp_mx_df,        file.path(OUT_DIR, "wpp_mx_df.rds"))
saveRDS(pop_array,        file.path(OUT_DIR, "pop_array.rds"))
saveRDS(p_extended,       file.path(OUT_DIR, "p_extended.rds"))
saveRDS(cause_frac_df,    file.path(OUT_DIR, "cause_frac_df.rds"))
saveRDS(incidence_rate_df, file.path(OUT_DIR, "incidence_rate_df.rds"))
saveRDS(cvd_tpm_schedule, file.path(OUT_DIR, "baseline_cvd_tpm_schedule.rds"))
saveRDS(cvd_tpm_diagnostics, file.path(OUT_DIR, "baseline_cvd_tpm_diagnostics.rds"))

message("  Outputs written to: ", normalizePath(OUT_DIR))

################################################################################
# 8  DIAGNOSTIC PLOTS
################################################################################

message("\n── Generating diagnostic plots ─────────────────────────────────────────")

theme_b <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.background  = element_rect(fill = "grey92", colour = NA),
        plot.title        = element_text(face = "bold", size = 11),
        legend.position   = "bottom")

CVD_COLS <- c(ihd            = "#C0392B",
              ischemic_stroke = "#E74C3C",
              ich             = "#F39C12",
              hhd             = "#E67E22",
              cervical_ca     = "#8E44AD")

# B1: Cause-specific 40q30 baseline trajectories
p_b1 <- q4030_baseline |>
  filter(cause %in% c(names(CVD_CAUSE_MAP), "cervical_ca")) |>
  ggplot(aes(x = year, y = q4030_pct, colour = cause,
             linetype = factor(sex, levels = SEXES))) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = CVD_COLS) +
  labs(title    = "B1: Cause-specific 40q30 baseline",
       subtitle = "Indonesia V1 | 2025–2100",
       x = NULL, y = "40q30 (%)", colour = NULL, linetype = "Sex") +
  theme_b
ggsave(file.path(PLOT_DIR, "b1_baseline_q4030.png"),
       p_b1, width = 10, height = 5, dpi = 150)

# B2: Age profile of cause-specific mx at 2025 and 2050
p_b2 <- mx_baseline |>
  filter(year %in% c(2025, 2050), age >= 20, age <= 80) |>
  ggplot(aes(x = age, y = mx_cause * 1e5, colour = factor(year),
             linetype = factor(sex, levels = SEXES))) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ module, scales = "free_y") +
  scale_colour_manual(values = c("2025" = "#1F6BAE", "2050" = "#C0392B")) +
  labs(title   = "B2: Cause-specific mx age profile",
       x = "Age", y = "mx (per 100k)", colour = "Year", linetype = "Sex") +
  theme_b
ggsave(file.path(PLOT_DIR, "b2_baseline_mx_profile.png"),
       p_b2, width = 14, height = 7, dpi = 150)

# ── WPP validation (deck figure → outputs/figures/wpp_validation.png) ────────
# Compares model all-cause deaths (backbone × CCPM population) against WPP 2024
# published deaths from data/wpp/WPP2024_Demographic_Indicators_Medium.csv.
# The ratio ≈ 1.0 validates the demographic spine.
# Note: WPP file stores deaths in thousands; model computes raw deaths.
OUT_FIGURES <- here("outputs", "figures")
dir.create(OUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

WPP_INDICATORS_FILE <- here("data", "wpp", "WPP2024_Demographic_Indicators_Medium.csv")
if (!file.exists(WPP_INDICATORS_FILE)) {
  message("  wpp_validation.png skipped — WPP2024_Demographic_Indicators_Medium.csv ",
          "not found at data/wpp/. Save it there to enable this validation figure.")
} else {
  message("  Generating WPP validation figure ...")

  # WPP published deaths (thousands → persons)
  wpp_pub <- read_csv(WPP_INDICATORS_FILE, show_col_types = FALSE,
                      locale = locale(encoding = "UTF-8")) |>
    filter(ISO3_code == "IDN", Time %in% PROJ_YEARS) |>
    select(year = Time,
           Female = DeathsFemale,
           Male   = DeathsMale) |>
    mutate(year   = as.integer(year),
           Female = as.numeric(Female) * 1e3,
           Male   = as.numeric(Male)   * 1e3) |>
    pivot_longer(c(Female, Male), names_to = "sex", values_to = "deaths") |>
    mutate(source = "WPP 2024 (published)")

  # Model deaths: WPP mx × CCPM-projected population, summed over ages
  model_pub <- wpp_mx_df |>
    filter(year %in% PROJ_YEARS) |>
    left_join(pop_df |> filter(year %in% PROJ_YEARS),
              by = c("year", "sex", "age")) |>
    group_by(year, sex) |>
    summarise(deaths = sum(mx_wpp * pop, na.rm = TRUE), .groups = "drop") |>
    mutate(source = "Model (WPP backbone)")

  val_df <- bind_rows(wpp_pub, model_pub)

  # Ratio: model / WPP published
  ratio_df <- model_pub |>
    rename(deaths_model = deaths) |>
    left_join(wpp_pub |> rename(deaths_wpp = deaths) |> select(-source),
              by = c("year", "sex")) |>
    mutate(ratio = deaths_model / pmax(deaths_wpp, 1))

  # Total (both sexes combined)
  total_df <- val_df |>
    group_by(year, source) |>
    summarise(deaths = sum(deaths), .groups = "drop") |>
    mutate(sex = "Total")

  ratio_total <- ratio_df |>
    group_by(year) |>
    summarise(deaths_model = sum(deaths_model),
              deaths_wpp   = sum(deaths_wpp), .groups = "drop") |>
    mutate(ratio = deaths_model / pmax(deaths_wpp, 1),
           sex   = "Total")

  ratio_all <- bind_rows(ratio_df |> select(year, sex, ratio), ratio_total)

  SEX_COLS <- c(Female = "#C0392B", Male = "#1F6BAE", Total = "#2C3E50")

  # Draw WPP first (solid, thinner) then Model on top (dashed, slightly thicker)
  # so the Model line is not hidden beneath WPP when they nearly overlap.
  plot_df <- bind_rows(val_df, total_df) |>
    filter(sex %in% c("Female", "Male", "Total")) |>
    mutate(source = factor(source,
                           levels = c("WPP 2024 (published)",
                                      "Model (WPP backbone)")))  # Model drawn last = on top

  p_top <- plot_df |>
    ggplot(aes(x = year, y = deaths / 1e6,
               colour = sex, linetype = source, linewidth = source)) +
    geom_line() +
    scale_colour_manual(values = SEX_COLS) +
    scale_linetype_manual(values = c("Model (WPP backbone)" = "dashed",
                                     "WPP 2024 (published)" = "solid")) +
    scale_linewidth_manual(values = c("Model (WPP backbone)" = 1.0,
                                      "WPP 2024 (published)" = 0.55),
                           guide = "none") +
    labs(title    = "Model all-cause deaths vs WPP 2024 (published)",
         subtitle = "Model (dashed, drawn on top) vs WPP published (solid). Lines nearly overlap — see ratio panel.",
         x = NULL, y = "Annual deaths (millions)",
         colour = NULL, linetype = NULL) +
    theme_b

  # Bottom panel: percent difference (model - WPP) / WPP × 100
  # More intuitive than ratio: positive = model above WPP, zero = exact match.
  pct_diff_df <- ratio_all |>
    mutate(pct_diff = (ratio - 1) * 100)

  p_bot <- pct_diff_df |>
    ggplot(aes(x = year, y = pct_diff, colour = sex)) +
    geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
    geom_line(linewidth = 0.7) +
    scale_colour_manual(values = SEX_COLS) +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    labs(subtitle = "Model above WPP (positive) — expected: model uses end-of-year population, WPP uses mid-year",
         x = NULL, y = "(Model \u2212 WPP) / WPP \u00d7 100", colour = NULL) +
    theme_b

  p_wpp_val <- cowplot::plot_grid(p_top, p_bot, ncol = 1,
                                   rel_heights = c(1.6, 1),
                                   align = "v", axis = "lr")

  ggsave(file.path(OUT_FIGURES, "wpp_validation.png"),
         p_wpp_val, width = 9, height = 7, dpi = 150)
  message("  wpp_validation.png → outputs/figures/ ✓")
}

message("  Plots saved to: ", normalizePath(PLOT_DIR))
message("\n── 05_run_baseline.R complete ───────────────────────────────────────────")
message("  Next: 06_run_scenarios.R")
