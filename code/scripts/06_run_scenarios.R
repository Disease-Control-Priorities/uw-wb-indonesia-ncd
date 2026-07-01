################################################################################
# INDONESIA INTEGRATED NCD MODEL — RUN INTERVENTION SCENARIOS V1
# scripts/06_run_scenarios.R
# ─────────────────────────────────────────────────────────────────────────────
# V1 scope: 4 CVD causes with simplified intervention effects adapted from
# the UW CVD parent model. The intervention interface is modular, but V1 does
# not fully reproduce the parent model's BP-bin redistribution or statin
# eligibility machinery — see R/interventions_cvd.R and README §7.
#
# SCENARIOS (10 CVD + 2 cervical):
#   baseline        no intervention
#   bp_fast         antihypertensive, fast (5yr to target)
#   bp_slow         antihypertensive, slow (10yr to target)
#   statins_fast    statins, fast
#   statins_slow    statins, slow
#   sodium          sodium reduction 2g/day
#   tfa             TFA elimination
#   diabetes_bp     diabetes-specific BP control, fast
#   all_fast        all CVD interventions, fast
#   all_slow        all CVD interventions, slow
#   cancer_fast     cervical screening + treatment, fast (provisional)
#   cancer_slow     cervical screening + treatment, slow
#
# PREREQUISITES: 05_run_baseline.R must have been run first.
# Loads pop_array, wpp_mx_df, pop_df, p_extended, cause_frac_df, projected
# incidence targets, and the fixed baseline CVD TPM schedule from
# data/model/baseline/ to ensure scenarios use identical demographics and do
# not re-solve annual CFR after interventions.
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
})

source(here("R", "engine.R"))
source(here("R", "cause_registry.R"))    # CVD_CAUSE_MAP, CVD_MODEL_TYPE, SHARED_PARAMS
source(here("R", "interventions_cvd.R")) # apply_cvd_interventions()
source(here("R", "validation.R"))        # run_core_invariant_suite()

# ── PATHS ─────────────────────────────────────────────────────────────────────
CALIB_DIR    <- here("data", "model", "calibration")
BASELINE_DIR <- here("data", "model", "baseline")
GBD_FILE     <- here("data", "gbd", "gbd_measures_full.csv")
OUT_DIR      <- here("data", "model", "scenarios")
PLOT_DIR     <- file.path(OUT_DIR, "plots")
dir.create(OUT_DIR,  recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

PROJ_YEARS    <- SHARED_PARAMS$run_years
WARMUP_CYCLES <- SHARED_PARAMS$warmup_cycles  # base-year stabilisation cycles (not historical)

################################################################################
# SCENARIO GRID
# Each scenario is a named list of intervention flags passed to tpm_fun()
# and mx_fun() inside make_cvd_module() / make_direct_cvd_mortality_module().
################################################################################

SCENARIOS <- list(

  baseline = list(
    label         = "Baseline (no intervention)",
    bp_on         = FALSE, statins_on    = FALSE, sodium_on = FALSE,
    tfa_on        = FALSE, diabetes_bp_on = FALSE, cancer_on = FALSE
  ),

  # ── Antihypertensive ────────────────────────────────────────────────────────
  # bp_baseline_ctrl = NULL: use Indonesia-specific value from hbp_control_data.rds
  # baseline_ctrl ≈ 0.044 at 2025 — proportion of hypertensives with controlled BP.
  # INTERPRETATION NOTE: scaling from 4.4% to 70% controlled is a large intervention
  # (delta_cov ≈ 0.656). This drives substantial deaths averted. The scale is
  # consistent with WHO/UW CVD model targets but should be interpreted as an
  # ambitious best-case scenario, not a central projection. The key question for
  # V2 is whether "controlled BP" here (GBD/NCD RisC definition) is fully
  # consistent with the Ettehad trial populations and the BoP RR reference context.
  bp_fast = list(
    label         = "BP control — fast (5yr)",
    bp_on         = TRUE,  bp_baseline_ctrl = NULL, bp_target_ctrl = 0.70,
    statins_on    = FALSE, sodium_on = FALSE, tfa_on = FALSE,
    diabetes_bp_on = FALSE, cancer_on = FALSE, speed = "fast"
  ),
  bp_slow = list(
    label         = "BP control — slow (10yr)",
    bp_on         = TRUE,  bp_baseline_ctrl = NULL, bp_target_ctrl = 0.70,
    statins_on    = FALSE, sodium_on = FALSE, tfa_on = FALSE,
    diabetes_bp_on = FALSE, cancer_on = FALSE, speed = "slow"
  ),

  # ── Statins ──────────────────────────────────────────────────────────────────
  statins_fast = list(
    label         = "Statins — fast (5yr)",
    bp_on         = FALSE, statins_on = TRUE,
    statins_baseline_cov = 0.05, statins_target_cov = 0.50,
    sodium_on     = FALSE, tfa_on = FALSE, diabetes_bp_on = FALSE,
    cancer_on     = FALSE, speed = "fast"
  ),
  statins_slow = list(
    label         = "Statins — slow (10yr)",
    bp_on         = FALSE, statins_on = TRUE,
    statins_baseline_cov = 0.05, statins_target_cov = 0.50,
    sodium_on     = FALSE, tfa_on = FALSE, diabetes_bp_on = FALSE,
    cancer_on     = FALSE, speed = "slow"
  ),

  # ── Sodium ────────────────────────────────────────────────────────────────────
  sodium = list(
    label         = "Sodium reduction (2g/day)",
    bp_on         = FALSE, statins_on = FALSE, sodium_on = TRUE,
    sodium_g_red  = 2.0,   tfa_on = FALSE, diabetes_bp_on = FALSE,
    cancer_on     = FALSE, speed = "fast"
  ),

  # ── TFA ───────────────────────────────────────────────────────────────────────
  tfa = list(
    label         = "TFA elimination",
    bp_on         = FALSE, statins_on = FALSE, sodium_on = FALSE,
    tfa_on        = TRUE,  diabetes_bp_on = FALSE,
    cancer_on     = FALSE, speed = "fast"
  ),

  # ── Diabetes BP ───────────────────────────────────────────────────────────────
  diabetes_bp = list(
    label         = "Diabetes BP control — fast",
    bp_on         = FALSE, statins_on = FALSE, sodium_on = FALSE,
    tfa_on        = FALSE,
    # NOTE: diabetes_bp_on = TRUE here uses a uniform BP-bin distribution
    # (bp_dist is empty). This scenario is retained for comparison but produces
    # an artificially inflated estimate. See all_fast comment for full explanation.
    diabetes_bp_on = TRUE,
    cancer_on     = FALSE, speed = "fast"
  ),

  # ── All CVD ───────────────────────────────────────────────────────────────────
  all_fast = list(
    label         = "All CVD interventions — fast",
    bp_on         = TRUE,  bp_baseline_ctrl = NULL, bp_target_ctrl = 0.70,
    statins_on    = TRUE,  statins_baseline_cov = 0.05, statins_target_cov = 0.50,
    sodium_on     = TRUE,  sodium_g_red = 2.0,
    tfa_on        = TRUE,
    # diabetes_bp_on disabled in V1: compute_diabetes_bp_effects() requires a
    # real SBP distribution (bp_dist). With bp_dist empty, .get_bp_probs()
    # returns a uniform 1/8 across all BP bins — an artificial assumption that
    # inflates the effect. Re-enable when GHDx SBP exposure data is loaded (V2).
    diabetes_bp_on = FALSE,
    cancer_on     = FALSE, speed = "fast"
  ),
  all_slow = list(
    label         = "All CVD interventions — slow",
    bp_on         = TRUE,  bp_baseline_ctrl = NULL, bp_target_ctrl = 0.70,
    statins_on    = TRUE,  statins_baseline_cov = 0.05, statins_target_cov = 0.50,
    sodium_on     = TRUE,  sodium_g_red = 2.0,
    tfa_on        = TRUE,
    # diabetes_bp_on disabled in V1 — see all_fast comment above.
    diabetes_bp_on = FALSE,
    cancer_on     = FALSE, speed = "slow"
  ),

  # ── Cancer (cervical, provisional) ─────────────────────────────────────────────
  cancer_fast = list(
    label         = "Cervical cancer package — fast",
    bp_on         = FALSE, statins_on = FALSE, sodium_on = FALSE,
    tfa_on        = FALSE, diabetes_bp_on = FALSE,
    cancer_on     = TRUE,  speed = "fast"
  ),
  cancer_slow = list(
    label         = "Cervical cancer package — slow",
    bp_on         = FALSE, statins_on = FALSE, sodium_on = FALSE,
    tfa_on        = FALSE, diabetes_bp_on = FALSE,
    cancer_on     = TRUE,  speed = "slow"
  )
)

################################################################################
# 1  LOAD INPUTS
# Load demographics and population objects from the baseline run to ensure
# scenarios use identical exposure denominators.
################################################################################

message("\n── Loading inputs ───────────────────────────────────────────────────────")

cvd_calib     <- readRDS(file.path(CALIB_DIR, "cvd_calibration_v1.rds"))
load(file.path(CALIB_DIR, "cervical_tpm.Rda"))     # provides: cervical_tpm
gbd_full      <- read_csv(GBD_FILE, show_col_types = FALSE) |>
                   filter(year == SHARED_PARAMS$calib_year)

pop_array     <- readRDS(file.path(BASELINE_DIR, "pop_array.rds"))
pop_df        <- readRDS(file.path(BASELINE_DIR, "pop_df.rds"))
wpp_mx_df     <- readRDS(file.path(BASELINE_DIR, "wpp_mx_df.rds"))
p_extended    <- readRDS(file.path(BASELINE_DIR, "p_extended.rds"))
cause_frac_df <- readRDS(file.path(BASELINE_DIR, "cause_frac_df.rds"))
incidence_rate_df <- readRDS(file.path(BASELINE_DIR, "incidence_rate_df.rds"))
baseline_cvd_tpm_schedule <- readRDS(file.path(BASELINE_DIR, "baseline_cvd_tpm_schedule.rds"))
baseline_cvd_tpm_diagnostics <- readRDS(file.path(BASELINE_DIR, "baseline_cvd_tpm_diagnostics.rds"))
validate_cvd_annual_tpm_diagnostics(baseline_cvd_tpm_diagnostics)

message("  Population array dims : ", paste(dim(pop_array), collapse = " × "))
message("  pop_df rows           : ", comma(nrow(pop_df)))
message("  cause_frac_df rows    : ", comma(nrow(cause_frac_df)))
message("  incidence targets rows: ", comma(nrow(incidence_rate_df)))
message("  fixed CVD TPM rows    : ", comma(nrow(baseline_cvd_tpm_schedule)))

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
# 2  BUILD MODULE SPECS
# Same construction as 05_run_baseline.R. Both scripts must use the same
# module factory calls so baseline and scenario TPMs are consistent.
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
      cause_frac_df         = cause_frac_df,
      wpp_mx_df             = wpp_mx_df,
      base_mx_cause         = cvd_calib[[mid]]$base_mx_cause,
      base_bgmx             = cvd_calib[[mid]]$base_bgmx,
      incidence_rate_df     = incidence_rate_df,
      baseline_tpm_schedule = baseline_cvd_tpm_schedule
    )
  }
})

cerv_mod    <- make_cervical_module(cervical_tpm, gbd_full)
all_modules <- c(cvd_modules, list(cervical_ca = cerv_mod))

################################################################################
# 3  RUN ALL SCENARIOS
################################################################################

message("\n── Running ", length(SCENARIOS), " scenarios ──────────────────────────────────────────────")

all_scenario_mx_raw <- bind_rows(imap(SCENARIOS, function(scen, scen_id) {
  message("  [", scen_id, "] ", scen$label)

  res <- run_module_set(
    modules       = all_modules,
    pop_array     = pop_array,
    proj_years    = PROJ_YEARS,
    warmup_cycles = seq_len(WARMUP_CYCLES),
    scenario      = scen
  )

  res$mx |>
    dplyr::mutate(scenario = scen_id, .before = 1)
}))

baseline_raw_mx <- all_scenario_mx_raw |>
  dplyr::filter(scenario == "baseline") |>
  dplyr::select(-scenario)

all_scenario_mx <- anchor_scenario_mx_to_targets(
  mx_df              = all_scenario_mx_raw,
  baseline_raw_mx_df = baseline_raw_mx,
  target_mx_df       = target_mx_df,
  modules_to_anchor  = MODULES_TO_ANCHOR
)

validate_baseline_anchor(
  mx_df = all_scenario_mx |>
    dplyr::filter(scenario == "baseline") |>
    dplyr::select(-scenario),
  target_mx_df = target_mx_df,
  modules      = MODULES_TO_ANCHOR
)

message("  Total scenario mx rows: ", comma(nrow(all_scenario_mx)))
message("  Scenario mx anchored : baseline target x raw scenario relative effect ✓")
message("  Scenario CVD TPMs use fixed baseline residual-CFR schedule; CFR is not re-solved in intervention runs ✓")
assert_zero_male_cervical(all_scenario_mx |> filter(scenario == "baseline"))
message("  Male cervical mx = 0 across all scenarios ✓")

################################################################################
# 4  INVARIANT CHECKS
################################################################################

run_core_invariant_suite(
  pop_array           = pop_array,
  pop_df              = filter(pop_df, year %in% PROJ_YEARS),
  wpp_mx_df           = filter(wpp_mx_df, year %in% PROJ_YEARS),
  mx_df               = all_scenario_mx |>
                          filter(scenario == "baseline") |>
                          select(-scenario),
  p_wpp               = p_extended,
  expected_start_year = 2025L
)

# CVD envelope check by scenario and cause
message("\n── CVD scenario envelope checks ─────────────────────────────────────────")
cvd_env_check <- all_scenario_mx |>
  filter(module %in% names(CVD_CAUSE_MAP)) |>
  left_join(filter(wpp_mx_df, year %in% PROJ_YEARS),
            by = c("year", "sex", "age")) |>
  group_by(scenario, module) |>
  summarise(cells      = n(),
            violations = sum(mx_cause > mx_wpp + 1e-12, na.rm = TRUE),
            max_ratio  = round(max(mx_cause / pmax(mx_wpp, 1e-12), na.rm = TRUE), 4),
            .groups    = "drop")
print(cvd_env_check)

if (any(cvd_env_check$violations > 0)) {
  bad <- cvd_env_check |> dplyr::filter(violations > 0)
  print(bad)
  stop(
    sum(bad$violations),
    " CVD scenario envelope violation(s) detected. ",
    "Cause-specific mx exceeds WPP all-cause mx — check intervention effects, ",
    "cause fractions, or calibration."
  )
} else {
  message("  All CVD modules inside WPP envelope across all scenarios ✓")
}

################################################################################
# 5  DEATHS AVERTED
################################################################################

message("\n── Computing deaths averted ─────────────────────────────────────────────")

deaths_averted <- compute_deaths_averted(
  mx_df  = all_scenario_mx,
  pop_df = filter(pop_df, year %in% PROJ_YEARS)
)

neg_check <- deaths_averted |>
  filter(scenario != "baseline") |>
  group_by(scenario, module) |>
  summarise(cum_averted = sum(averted_raw, na.rm = TRUE), .groups = "drop") |>
  filter(cum_averted < 0)

if (nrow(neg_check) > 0) {
  warning("Negative cumulative deaths averted in some scenario/module combinations:")
  print(neg_check)
} else {
  message("  No negative cumulative deaths averted ✓")
}

# Cumulative summaries: 2025–2050 and 2025–2100 (both cumulative from 2025).
# Both periods are written here so 07_make_outputs.R can use them directly.
SCEN_PERIODS <- list("2025-2050" = c(2025L, 2050L), "2025-2100" = c(2025L, 2100L))

cum_averted_all <- bind_rows(lapply(names(SCEN_PERIODS), function(lab) {
  yr <- SCEN_PERIODS[[lab]]
  deaths_averted |>
    filter(year >= yr[1], year <= yr[2]) |>
    group_by(scenario, module) |>
    summarise(
      cum_baseline_deaths = sum(deaths_base,     na.rm = TRUE),
      cum_deaths          = sum(deaths,          na.rm = TRUE),
      cum_averted         = sum(averted_display, na.rm = TRUE),
      pct_reduction       = round(100 * sum(averted_display) /
                                    pmax(sum(deaths_base), 1), 2),
      .groups = "drop"
    ) |>
    mutate(period = lab, .before = 1)
}))

################################################################################
# 6  40q30
################################################################################

message("\n── Computing scenario 40q30 ─────────────────────────────────────────────")

# Baseline module mx is needed to adjust the all-cause life table for each
# scenario. Without this, scenario reductions never appear in all-cause 40q30.
baseline_mx_for_q40 <- all_scenario_mx |>
  filter(scenario == "baseline") |>
  select(-scenario)

q4030_scen <- bind_rows(imap(SCENARIOS, function(scen, scen_id) {
  mx_s  <- all_scenario_mx |> filter(scenario == scen_id) |> select(-scenario)
  bsln  <- if (scen_id == "baseline") NULL else baseline_mx_for_q40
  compute_q4030(
    mx_df          = mx_s,
    wpp_mx_df      = filter(wpp_mx_df, year %in% PROJ_YEARS),
    baseline_mx_df = bsln
  ) |>
    mutate(scenario = scen_id, .before = 1)
}))

################################################################################
# 7  WRITE OUTPUTS
################################################################################

message("\n── Writing outputs ──────────────────────────────────────────────────────")

write_csv(all_scenario_mx,     file.path(OUT_DIR, "scenario_mx.csv"))
write_csv(all_scenario_mx_raw, file.path(OUT_DIR, "scenario_mx_raw_state_model.csv"))
write_csv(deaths_averted,      file.path(OUT_DIR, "scenario_deaths_averted.csv"))
write_csv(cum_averted_all,     file.path(OUT_DIR, "scenario_cum_averted.csv"))
write_csv(q4030_scen,          file.path(OUT_DIR, "scenario_q4030.csv"))

message("  scenario_mx.csv               — ", comma(nrow(all_scenario_mx)),  " rows ✓")
message("  scenario_mx_raw_state_model.csv — ", comma(nrow(all_scenario_mx_raw)), " rows ✓")
message("  scenario_deaths_averted.csv   — ", comma(nrow(deaths_averted)),   " rows ✓")
message("  scenario_cum_averted.csv      — ", comma(nrow(cum_averted_all)),  " rows ✓  (periods: 2025-2050, 2025-2100)")
message("  scenario_q4030.csv            — ", comma(nrow(q4030_scen)),       " rows ✓")

################################################################################
# 8  DIAGNOSTIC PLOTS
################################################################################

message("\n── Generating diagnostic plots ─────────────────────────────────────────")

theme_s <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        legend.position   = "bottom",
        strip.background  = element_rect(fill = "grey92", colour = NA),
        plot.title        = element_text(face = "bold", size = 11))

CVD_SCEN_COLS <- c(
  baseline    = "#000000",
  bp_fast     = "#C0392B", bp_slow      = "#E74C3C",
  statins_fast = "#2980B9", statins_slow = "#3498DB",
  sodium      = "#27AE60", tfa          = "#F39C12",
  diabetes_bp = "#8E44AD",
  all_fast    = "#E67E22", all_slow     = "#D35400",
  cancer_fast = "#7F8C8D", cancer_slow  = "#95A5A6"
)

# S1: Cumulative CVD deaths averted 2025–2050 by cause and scenario
p_s1 <- cum_averted_all |>
  filter(period == "2025-2050",
         module %in% names(CVD_CAUSE_MAP), scenario != "baseline") |>
  mutate(
    scenario_lab = factor(scenario, levels = names(SCENARIOS)),
    module_lab   = recode(module,
                          ihd = "IHD", ischemic_stroke = "Isch. stroke",
                          ich = "ICH", hhd = "HHD")
  ) |>
  ggplot(aes(x = scenario_lab, y = cum_averted / 1e3, fill = module_lab)) +
  geom_col(position = "stack") +
  facet_wrap(~ "2025-2050 cumulative deaths averted") +
  scale_fill_manual(values = c(IHD = "#C0392B", "Isch. stroke" = "#E74C3C",
                                ICH = "#F39C12", HHD = "#E67E22")) +
  coord_flip() +
  labs(title    = "S1: Cumulative CVD deaths averted 2025–2050",
       subtitle = "Indonesia V1 | all 4 CVD causes stacked",
       x = NULL, y = "Deaths averted (thousands)", fill = NULL) +
  theme_s
ggsave(file.path(PLOT_DIR, "s1_cum_averted_by_cause.png"),
       p_s1, width = 10, height = 6, dpi = 150)

# S2: IHD 40q30 trajectories under CVD scenarios — Female
cvd_scens <- c("baseline", "bp_fast", "bp_slow", "statins_fast", "all_fast", "all_slow")
p_s2 <- q4030_scen |>
  filter(scenario %in% cvd_scens, cause == "ihd", sex == "Female") |>
  ggplot(aes(x = year, y = q4030_pct, colour = scenario)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = CVD_SCEN_COLS[cvd_scens]) +
  labs(title    = "S2: IHD 40q30 — Female",
       subtitle = "Indonesia V1 | CVD scenarios",
       x = NULL, y = "IHD 40q30 (%)", colour = NULL) +
  theme_s
ggsave(file.path(PLOT_DIR, "s2_ihd_q4030_scenarios.png"),
       p_s2, width = 10, height = 5, dpi = 150)

message("  Plots saved to: ", normalizePath(PLOT_DIR))
################################################################################
# 8  HHD SENSITIVITY — IHD-PROXY SBP RR
# Sidecar analysis: not included in scenario_mx.csv or scenario_cum_averted.csv.
# 07_make_outputs.R will NOT include this unless explicitly modified to read
# sensitivity_hhd_proxy_cum_averted.csv alongside the primary scenario files.
# Re-runs all_fast only with the HHD proxy RR to quantify how much the headline
# result depends on the HHD SBP incidence exclusion decision.
# Outputs: data/model/scenarios/sensitivity_hhd_proxy_{mx,cum_averted}.csv
################################################################################

sens_rr_file <- here("data", "gbd", "gbd_rr_sbp_hhd_sens.rds")

if (file.exists(sens_rr_file)) {
  message("\n── HHD sensitivity run (all_fast, hhd IHD-proxy RR) ────────────────────")

  reload_cvd_rr(sens_rr_file)

  sens_res <- run_module_set(
    modules       = all_modules,
    pop_array     = pop_array,
    proj_years    = PROJ_YEARS,
    warmup_cycles = seq_len(WARMUP_CYCLES),
    scenario      = SCENARIOS[["all_fast"]]
  )
  sens_mx_raw <- sens_res$mx |>
    dplyr::mutate(scenario = "all_fast_hhd_sens", .before = 1)

  sens_mx <- anchor_scenario_mx_to_targets(
    mx_df              = sens_mx_raw,
    baseline_raw_mx_df = baseline_raw_mx,
    target_mx_df       = target_mx_df,
    modules_to_anchor  = MODULES_TO_ANCHOR
  )

  # Bind baseline so compute_deaths_averted() has a reference scenario
  sens_averted <- compute_deaths_averted(
    mx_df  = bind_rows(
      all_scenario_mx |> filter(scenario == "baseline"),
      sens_mx
    ),
    pop_df = filter(pop_df, year %in% PROJ_YEARS)
  ) |>
    filter(scenario == "all_fast_hhd_sens")

  sens_cum <- bind_rows(lapply(names(SCEN_PERIODS), function(lab) {
    yr <- SCEN_PERIODS[[lab]]
    sens_averted |>
      filter(year >= yr[1], year <= yr[2]) |>
      group_by(scenario, module) |>
      summarise(
        cum_baseline_deaths = sum(deaths_base,     na.rm = TRUE),
        cum_averted         = sum(averted_display, na.rm = TRUE),
        pct_reduction       = round(
          100 * sum(averted_display, na.rm = TRUE) /
            pmax(sum(deaths_base, na.rm = TRUE), 1), 2),
        .groups = "drop"
      ) |>
      mutate(period = lab, .before = 1)
  }))

  write_csv(sens_mx,     file.path(OUT_DIR, "sensitivity_hhd_proxy_mx.csv"))
  write_csv(sens_mx_raw, file.path(OUT_DIR, "sensitivity_hhd_proxy_mx_raw_state_model.csv"))
  write_csv(sens_cum,    file.path(OUT_DIR, "sensitivity_hhd_proxy_cum_averted.csv"))

  reload_cvd_rr(here("data", "gbd", "gbd_rr_sbp.rds"))   # restore primary

  primary_total <- cum_averted_all |>
    filter(scenario == "all_fast", period == "2025-2050") |>
    summarise(total = sum(cum_averted, na.rm = TRUE)) |> pull(total)
  sens_total <- sens_cum |>
    filter(period == "2025-2050") |>
    summarise(total = sum(cum_averted, na.rm = TRUE)) |> pull(total)

  message("  Primary all_fast 2025-2050        : ", round(primary_total / 1e6, 2), "M")
  message("  Sensitivity (HHD proxy) 2025-2050 : ", round(sens_total    / 1e6, 2), "M")
  message("  Difference (HHD proxy effect)     : ",
          round((sens_total - primary_total) / 1e6, 2), "M")
  message("  sensitivity_hhd_proxy_mx.csv ✓")
  message("  sensitivity_hhd_proxy_cum_averted.csv ✓")
} else {
  message("\n  HHD sensitivity skipped — ", basename(sens_rr_file), " not found.")
  message("  Run 01b_prepare_sbp_rr.R first to generate the sensitivity RDS.")
}

message("\n── 06_run_scenarios.R complete ─────────────────────────────────────────")
message("  Next: 07_make_outputs.R")
