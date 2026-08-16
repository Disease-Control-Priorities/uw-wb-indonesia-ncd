################################################################################
# INDONESIA INTEGRATED NCD MODEL — GBD DATA PREPARATION
# scripts/01_prepare_gbd_inputs.R
# ─────────────────────────────────────────────────────────────────────────────
# Run ONCE to convert raw GBD downloads into clean model inputs.
# All data comes from the GBD Results Tool (vizhub.healthdata.org/gbd-results).
#
# ── DOWNLOAD 1: Disease measures ──────────────────────────────────────────────
# Save to:  data/gbd/raw/disease/
#   GBD Estimate : Disease/Injury
#   Measure      : Deaths, Incidence, Prevalence, YLDs
#   Metric       : Rate, Number
#   Years        : 2000, 2005, 2010, 2015, 2019, 2021, 2023
#   Age          : <1 year, 12-23 months, 2-4 years, 5-9 years, …, 95+ years
#   Sex          : Male, Female
#   Location     : Indonesia
#   Cause        : 23 disease causes + All causes (see NCD_CAUSES below)
#
# ── DOWNLOAD 2: Risk factor PAFs ──────────────────────────────────────────────
# Save to:  data/gbd/raw/risks/
#   GBD Estimate : Risk factor
#   Measure      : Deaths
#   Metric       : Percent
#   Years        : 2000, 2005, 2010, 2015, 2019, 2021, 2023
#   Age          : same age groups as Download 1
#   Sex          : Male, Female
#   Location     : Indonesia
#   Risk         : 15 risk factors (see RISK_FACTORS below)
#   Cause        : same 23 disease causes as Download 1
#
# ── OUTPUT FILES (written to data/gbd/) ───────────────────────────────────────
#   gbd_cause_deaths.csv        cause-specific death rates (Rate)
#   gbd_cause_deaths_n.csv      cause-specific death counts (Number)
#   gbd_allcause_mx.csv         all-cause death rates
#   gbd_allcause_n.csv          all-cause death counts
#   gbd_cause_fractions.csv     cause fractions: frac_c = mx_c / mx_all
#   gbd_measures_full.csv       all measures × NCD causes (Rate)
#   gbd_measures_full_n.csv     all measures × NCD causes (Number)
#   gbd_risk_paf.csv            risk-factor PAFs (Percent metric)
################################################################################

rm(list = ls())

library(here)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(scales)
})

# ── PATHS ─────────────────────────────────────────────────────────────────────
RAW_DISEASE_DIR <- here("data", "gbd", "raw", "disease")
RAW_RISK_DIR    <- here("data", "gbd", "raw", "risks")
OUT_DIR         <- here("data", "gbd")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ── ANALYTICAL CONSTANTS ──────────────────────────────────────────────────────

# 23 NCD causes for the integrated model: CVD (4), Cancer (16), Other NCD (3).
# cause_deaths retains ALL causes in the download; this list is used for
# measures_full filtering and sense checks only.
NCD_CAUSES <- c(
  # CVD — core module (4)
  "Ischemic heart disease",
  "Ischemic stroke",
  "Intracerebral hemorrhage",
  "Hypertensive heart disease",
  # Cancer — simple state Well/Sick/Dead (11)
  "Bladder cancer",
  "Esophageal cancer",
  "Liver cancer",
  "Nasopharynx cancer",
  "Other pharynx cancer",
  "Lip and oral cavity cancer",
  "Ovarian cancer",
  "Pancreatic cancer",
  "Stomach cancer",
  "Thyroid cancer",
  "Uterine cancer",
  # Cancer — staged local/regional/distant (3)
  "Tracheal, bronchus, and lung cancer",
  "Breast cancer",
  "Prostate cancer",
  # Cancer — staged with pre-cancerous lesion (2)
  "Cervical cancer",
  "Colon and rectum cancer",
  # Future NCD modules (3)
  "Diabetes mellitus type 2",
  "Chronic obstructive pulmonary disease",
  "Alzheimer's disease and other dementias"
)

# 7 anchor years — same for both downloads
TARGET_YEARS <- c(2000, 2005, 2010, 2015, 2019, 2021, 2023)

# 15 risk factors downloaded as PAFs (GBD Estimate = Risk factor,
# Measure = Deaths, Metric = Percent).
# Exact rei_name strings as they appear in the GBD Results Tool.
RISK_FACTORS <- c(
  "Smoking",
  "High systolic blood pressure",
  "High LDL cholesterol",
  "High fasting plasma glucose",
  "High body-mass index",
  "High alcohol use",
  "Low physical activity",
  "Diet low in fruits",
  "Diet low in vegetables",
  "Diet low in whole grains",
  "Diet high in sodium",
  "Diet high in red meat",
  "Diet low in fiber",
  "Diet low in legumes",
  "Diet high in sugar-sweetened beverages"
)

# Indonesia national location_id in GBD
IDN_NATIONAL_ID <- 11L

################################################################################
# AGE LABEL LOOKUP
# GBD Results Tool uses string age labels; convert to numeric midpoints.
# "Age-standardized" and "All Ages" are not in the LUT and map to NA,
# which causes them to be dropped by filter(!is.na(age_mid)).
################################################################################

AGE_LUT <- c(
  "<1 year"      =  0,
  "12-23 months" =  1.5,
  "2-4 years"    =  3,
  "5-9 years"    =  7,
  "10-14 years"  = 12,
  "15-19 years"  = 17,
  "20-24 years"  = 22,
  "25-29 years"  = 27,
  "30-34 years"  = 32,
  "35-39 years"  = 37,
  "40-44 years"  = 42,
  "45-49 years"  = 47,
  "50-54 years"  = 52,
  "55-59 years"  = 57,
  "60-64 years"  = 62,
  "65-69 years"  = 67,
  "70-74 years"  = 72,
  "75-79 years"  = 77,
  "80-84 years"  = 82,
  "85-89 years"  = 87,
  "90-94 years"  = 92,
  "95+ years"    = 97
)

normalise_age_mid <- function(age_name) {
  unname(AGE_LUT[str_trim(age_name)])
}

################################################################################
# 1  READ BOTH FOLDERS
################################################################################

message("\n── Reading disease folder ────────────────────────────────────────────────")

disease_files <- list.files(RAW_DISEASE_DIR, pattern = "\\.csv$",
                            full.names = TRUE, ignore.case = TRUE)
if (length(disease_files) == 0) stop("No CSV files found in: ", RAW_DISEASE_DIR)
message("  Found ", length(disease_files), " file(s)")

disease_all <- map_dfr(disease_files, read_csv, show_col_types = FALSE) |>
  rename_with(~ str_replace_all(tolower(.x), " ", "_"))
message("  Total rows loaded: ", comma(nrow(disease_all)))

message("\n── Reading risks folder ──────────────────────────────────────────────────")

risk_files <- list.files(RAW_RISK_DIR, pattern = "\\.csv$",
                         full.names = TRUE, ignore.case = TRUE)
if (length(risk_files) == 0) stop("No CSV files found in: ", RAW_RISK_DIR)
message("  Found ", length(risk_files), " file(s)")

risk_all <- map_dfr(risk_files, read_csv, show_col_types = FALSE) |>
  rename_with(~ str_replace_all(tolower(.x), " ", "_"))
message("  Total rows loaded: ", comma(nrow(risk_all)))

# Restrict to Indonesia national. Subnational rows kept in raw files for V3.
all_locs <- disease_all |>
  distinct(location_id, location_name) |>
  arrange(location_name)
message("\n  Locations in download (", nrow(all_locs), "):")
print(all_locs, n = 50)

if (!IDN_NATIONAL_ID %in% all_locs$location_id)
  stop("Indonesia national (location_id 11) not found. ",
       "Ensure 'Indonesia' was selected in the GBD Results Tool.")

TARGET_LOCS <- IDN_NATIONAL_ID

################################################################################
# 2  DISEASE MEASURES
################################################################################

message("\n── Processing disease measures ───────────────────────────────────────────")

# Filter to Indonesia, target years, and valid single-year age groups.
found_years_disease <- sort(intersect(TARGET_YEARS, unique(disease_all$year)))
message("  Years found: ", paste(found_years_disease, collapse = ", "))

disease_base <- disease_all |>
  filter(
    location_id %in% TARGET_LOCS,
    year        %in% found_years_disease,
    sex_name    %in% c("Male", "Female")
  ) |>
  mutate(age_mid = normalise_age_mid(age_name)) |>
  filter(!is.na(age_mid))

message("  Rows after filter: ", comma(nrow(disease_base)))

# ── 2a  Cause-specific death rates (Rate metric) ──────────────────────────────
cause_deaths <- disease_base |>
  filter(
    metric_name  == "Rate",
    measure_name == "Deaths",
    cause_name   != "All causes"
  ) |>
  select(
    location_id, location_name,
    sex      = sex_name,
    age_mid, age_name,
    cause_id, cause = cause_name,
    year,
    rate_per100k = val,
    rate_lower   = lower,
    rate_upper   = upper
  ) |>
  arrange(location_id, sex, cause, year, age_mid)

message("  cause_deaths rows: ", comma(nrow(cause_deaths)))

# ── 2b  Cause-specific death counts (Number metric) ──────────────────────────
cause_deaths_n <- disease_base |>
  filter(
    metric_name  == "Number",
    measure_name == "Deaths",
    cause_name   != "All causes"
  ) |>
  select(
    location_id, location_name,
    sex      = sex_name,
    age_mid, age_name,
    cause_id, cause = cause_name,
    year,
    count       = val,
    count_lower = lower,
    count_upper = upper
  ) |>
  arrange(location_id, sex, cause, year, age_mid)

message("  cause_deaths_n rows: ", comma(nrow(cause_deaths_n)))

# ── 2c  All-cause mortality rates and counts ──────────────────────────────────
allcause_mx <- disease_base |>
  filter(metric_name == "Rate",   measure_name == "Deaths",
         cause_name  == "All causes") |>
  select(location_id, location_name, sex = sex_name, age_mid, age_name,
         year, mx_all = val, mx_lower = lower, mx_upper = upper) |>
  arrange(location_id, sex, year, age_mid)

allcause_n <- disease_base |>
  filter(metric_name == "Number", measure_name == "Deaths",
         cause_name  == "All causes") |>
  select(location_id, location_name, sex = sex_name, age_mid, age_name,
         year, deaths_all = val, deaths_lower = lower, deaths_upper = upper) |>
  arrange(location_id, sex, year, age_mid)

message("  allcause_mx rows: ", comma(nrow(allcause_mx)))

# ── 2d  Cause fractions: frac_c = mx_c / mx_all ──────────────────────────────
# Used by 03_build_cause_fractions.R as its primary input.
cause_fractions <- cause_deaths |>
  left_join(
    allcause_mx |> select(location_id, sex, age_mid, year, mx_all),
    by = c("location_id", "sex", "age_mid", "year")
  ) |>
  mutate(
    frac      = if_else(mx_all > 0, rate_per100k / (mx_all), 0),
    frac      = pmin(pmax(frac, 0), 1),
    logit_frac = log(pmax(frac, 1e-9) / pmax(1 - frac, 1e-9)),
    frac_zero  = frac == 0
  ) |>
  select(
    location_id, location_name,
    sex, age_mid, age_name,
    cause_id, cause,
    year,
    rate_per100k, mx_all, frac, logit_frac, frac_zero
  ) |>
  arrange(location_id, sex, cause, year, age_mid)

message("  cause_fractions rows: ", comma(nrow(cause_fractions)))
message("  Fraction range      : [",
        round(min(cause_fractions$frac, na.rm = TRUE), 5), ", ",
        round(max(cause_fractions$frac, na.rm = TRUE), 5), "]")
message("  Zero-fraction rows  : ",
        comma(sum(cause_fractions$frac_zero, na.rm = TRUE)),
        " (preserved for downstream modelling)")

# ── 2e  All measures — Rate ────────────────────────────────────────────────────
measures_full <- disease_base |>
  filter(
    metric_name == "Rate",
    cause_name  %in% c(NCD_CAUSES, "All causes")
  ) |>
  mutate(measure = str_extract(measure_name, "^[^(]+") |> str_trim()) |>
  select(
    location_id, location_name,
    sex      = sex_name,
    age_mid, age_name,
    cause_id, cause = cause_name,
    measure, year,
    rate_per100k = val,
    rate_lower   = lower,
    rate_upper   = upper
  ) |>
  arrange(location_id, sex, cause, measure, year, age_mid)

message("  measures_full rows  : ", comma(nrow(measures_full)))
message("  Measures present    : ",
        paste(sort(unique(measures_full$measure)), collapse = ", "))

# ── 2f  All measures — Number ─────────────────────────────────────────────────
measures_full_n <- disease_base |>
  filter(
    metric_name == "Number",
    cause_name  %in% c(NCD_CAUSES, "All causes")
  ) |>
  mutate(measure = str_extract(measure_name, "^[^(]+") |> str_trim()) |>
  select(
    location_id, location_name,
    sex      = sex_name,
    age_mid, age_name,
    cause_id, cause = cause_name,
    measure, year,
    count       = val,
    count_lower = lower,
    count_upper = upper
  ) |>
  arrange(location_id, sex, cause, measure, year, age_mid)

message("  measures_full_n rows: ", comma(nrow(measures_full_n)))

################################################################################
# 3  RISK-FACTOR PAFs
################################################################################

message("\n── Processing risk-factor PAFs ──────────────────────────────────────────")

found_years_risk <- sort(intersect(TARGET_YEARS, unique(risk_all$year)))

paf_base <- risk_all |>
  filter(
    location_id  %in% TARGET_LOCS,
    year         %in% found_years_risk,
    sex_name     %in% c("Male", "Female"),
    metric_name  == "Percent",
    measure_name == "Deaths",
    rei_name     %in% RISK_FACTORS,
    cause_name   %in% NCD_CAUSES
  ) |>
  mutate(age_mid = normalise_age_mid(age_name)) |>
  filter(!is.na(age_mid))

paf_clean <- paf_base |>
  select(
    location_id, location_name,
    sex      = sex_name,
    age_mid, age_name,
    cause_id, cause = cause_name,
    rei_id,  risk  = rei_name,
    year,
    paf       = val,
    paf_lower = lower,
    paf_upper = upper
  ) |>
  arrange(location_id, sex, risk, cause, year, age_mid)

# Auto-detect and correct 0–100 scale (GBD sometimes uses percent, not decimal)
paf_range <- range(paf_clean$paf, na.rm = TRUE)
message(sprintf("  PAF range before cleaning: [%.4f, %.4f]",
                paf_range[1], paf_range[2]))

if (paf_range[2] > 1) {
  message("  Rescaling PAFs from 0-100 to 0-1")
  paf_clean <- paf_clean |>
    mutate(across(c(paf, paf_lower, paf_upper), ~ .x / 100))
}

# Floor negative PAFs at 0 (below-TMREL artefact in GBD)
n_negative <- sum(paf_clean$paf < 0, na.rm = TRUE)
if (n_negative > 0) {
  message("  Negative PAFs: ", n_negative,
          " rows (flooring to 0 — below-TMREL artefact)")
  paf_clean <- paf_clean |>
    mutate(across(c(paf, paf_lower, paf_upper), ~ pmax(.x, 0)))
}

message(sprintf("  PAF range after cleaning : [%.4f, %.4f]",
                min(paf_clean$paf), max(paf_clean$paf)))
message("  PAF rows  : ", comma(nrow(paf_clean)))
message("  Risks     : ", paste(sort(unique(paf_clean$risk)),  collapse = ", "))
message("  Causes    : ", paste(sort(unique(paf_clean$cause)), collapse = ", "))

################################################################################
# 4  WRITE OUTPUTS
################################################################################

message("\n── Writing outputs ───────────────────────────────────────────────────────")

write_csv(cause_deaths,    file.path(OUT_DIR, "gbd_cause_deaths.csv"))
write_csv(cause_deaths_n,  file.path(OUT_DIR, "gbd_cause_deaths_n.csv"))
write_csv(allcause_mx,     file.path(OUT_DIR, "gbd_allcause_mx.csv"))
write_csv(allcause_n,      file.path(OUT_DIR, "gbd_allcause_n.csv"))
write_csv(cause_fractions, file.path(OUT_DIR, "gbd_cause_fractions.csv"))
write_csv(measures_full,   file.path(OUT_DIR, "gbd_measures_full.csv"))
write_csv(measures_full_n, file.path(OUT_DIR, "gbd_measures_full_n.csv"))
write_csv(paf_clean,       file.path(OUT_DIR, "gbd_risk_paf.csv"))

message("  gbd_cause_deaths.csv      — ", comma(nrow(cause_deaths)),    " rows ✓")
message("  gbd_cause_deaths_n.csv    — ", comma(nrow(cause_deaths_n)),  " rows ✓")
message("  gbd_allcause_mx.csv       — ", comma(nrow(allcause_mx)),     " rows ✓")
message("  gbd_allcause_n.csv        — ", comma(nrow(allcause_n)),      " rows ✓")
message("  gbd_cause_fractions.csv   — ", comma(nrow(cause_fractions)), " rows ✓")
message("  gbd_measures_full.csv     — ", comma(nrow(measures_full)),   " rows ✓")
message("  gbd_measures_full_n.csv   — ", comma(nrow(measures_full_n)), " rows ✓")
message("  gbd_risk_paf.csv          — ", comma(nrow(paf_clean)),       " rows ✓")

################################################################################
# 5  SENSE CHECKS
################################################################################

message("\n── Sense checks ──────────────────────────────────────────────────────────")

message("\n  Cause coverage (death rates):")
cause_deaths |>
  filter(cause %in% NCD_CAUSES) |>
  group_by(cause) |>
  summarise(n_years = n_distinct(year),
            n_ages  = n_distinct(age_mid),
            n_sexes = n_distinct(sex), .groups = "drop") |>
  print(n = 25)

message("\n  IHD cause fractions, age 60-64, 2019 and 2023:")
cause_fractions |>
  filter(cause == "Ischemic heart disease", age_mid == 62,
         year %in% c(2019, 2023)) |>
  select(location_name, sex, year, rate_per100k, mx_all, frac) |>
  arrange(year, sex) |>
  print(n = 10)

message("\n  Fraction sum check (should all be ≤ 1):")
cause_fractions |>
  group_by(location_id, sex, age_mid, year) |>
  summarise(frac_sum = sum(frac, na.rm = TRUE), .groups = "drop") |>
  summarise(min_sum  = round(min(frac_sum),  4),
            mean_sum = round(mean(frac_sum), 4),
            max_sum  = round(max(frac_sum),  4),
            n_over1  = sum(frac_sum > 1.001)) |>
  print()

message("\n  Years present in cause_deaths: ",
        paste(sort(unique(cause_deaths$year)), collapse = ", "))

message("\n── 01_prepare_gbd_inputs.R complete ─────────────────────────────────────")
message("  Key outputs for downstream pipeline:")
message("    gbd_cause_fractions.csv  → cause-fraction layer (WPP/GBD integration)")
message("    gbd_measures_full.csv    → disease module IR, CF, DALY inputs")
message("    gbd_allcause_mx.csv      → WPP/GBD scale-factor cross-check")
message("    gbd_risk_paf.csv         → PAF engine risk-factor inputs")
message("  Next: 01b_prepare_sbp_rr.R")

################################################################################
# INDONESIA INTEGRATED NCD MODEL — SBP DOSE-RESPONSE RELATIVE RISK
# scripts/01b_prepare_sbp_rr.R
# ─────────────────────────────────────────────────────────────────────────────
# Produces:
#   data/gbd/gbd_rr_sbp.rds             PRIMARY — ihd, ischemic_stroke, ich
#                                        HHD excluded; hhd rr_per_10mmhg = 1.0
#   data/gbd/gbd_rr_sbp_hhd_sens.rds    SENSITIVITY — adds hhd proxy row
#   outputs/validation/sbp_rr_summary.csv
#   outputs/validation/sbp_rr_key_values.csv
#   outputs/validation/sbp_rr_source_metadata.csv
#
# ── METHODOLOGY ───────────────────────────────────────────────────────────────
# GBD 2021-inspired implementation based on:
#   GBD 2021 Risk Factors Collaborators. Lancet 2024;403:2162-2203.
#   Supplementary Appendix 1 (Methods), p.279.
#   Saved: docs/references/gbd2021_risk_factors_appendix1_lancet2024.pdf
#
# Two-step formula (GBD appendix p.279):
#
#   STEP 1 — Base RR from Burden of Proof MR-BRT curve:
#     log_rr_base(cause) = log(RR_BoP(SBP_ref+10)) - log(RR_BoP(SBP_ref))
#
#   STEP 2 — Age attenuation from Singh et al. 2013:
#     attenuation(cause,age) = log(RR_Singh(cause,age)) / log(RR_Singh(cause,55-64))
#     Singh reports 55-64; its midpoint approximates the GBD 60-64 reference age.
#
#   COMBINED:
#     RR(cause,age) = exp(log_rr_base × attenuation)
#
# APPROXIMATION NOTE: Uses a single local per-10 mmHg slope at REF_SBP rather
# than category-specific curve differences. The BoP curve is nearly linear
# between 120-145 mmHg; the slope diagnostic below quantifies this error.
# A fully category-specific implementation is deferred to V2.
#
# ── PRIMARY INPUTS ────────────────────────────────────────────────────────────
# INPUT 1: BoP curves — vizhub.healthdata.org/burden-of-proof/
#   data/gbd/raw/risks/sbp_rr/ihd.csv, stroke.csv
#   Columns: Risk (SBP mmHg), "Log relative risk of outcome"
#
# INPUT 2: Singh et al. 2013, Figure 1, pooled PSC + APCSC
#   PLoS ONE 2013;8(7):e65174. doi:10.1371/journal.pone.0065174
#   docs/references/singh_2013_sbp_rr_plos_one.pdf
#
# ── CAUSE MAPPING ─────────────────────────────────────────────────────────────
# PRIMARY (gbd_rr_sbp.rds):
#   ihd             → IHD BoP × IHD Singh attenuation
#   ischemic_stroke → Stroke BoP × ischemic stroke Singh attenuation
#   ich             → Stroke BoP × haemorrhagic stroke Singh attenuation
#                     (GBD appendix p.279: subtypes share one BoP model)
#   hhd             → rr_per_10mmhg = 1.0
#                     no SBP incidence RR effect in primary;
#                     simplified Ettehad CF/direct-mortality effect may still be
#                     applied downstream in interventions_cvd.R
#
# SENSITIVITY (gbd_rr_sbp_hhd_sens.rds):
#   hhd             → IHD BoP × HHD Singh attenuation
#                     Pragmatic proxy: borrows IHD base slope, applies HHD
#                     age attenuation. This is a modelling assumption, not a
#                     GBD output. Provided separately for sensitivity analysis.
################################################################################

rm(list = ls())

if (!requireNamespace("here", quietly = TRUE))
  stop("Package 'here' is required.", call. = FALSE)
source(here::here("R", "packages.R"))
library(here)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble)
})

BOP_DIR   <- here("data", "gbd", "raw", "risks", "sbp_rr")
OUT_DIR   <- here("data", "gbd")
VAL_DIR   <- here("outputs", "validation")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(VAL_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Check inputs ──────────────────────────────────────────────────────────────
for (f in c(file.path(BOP_DIR, "ihd.csv"), file.path(BOP_DIR, "stroke.csv"))) {
  if (!file.exists(f))
    stop("Missing BoP file: ", f,
         "\nDownload from vizhub.healthdata.org/burden-of-proof/", call. = FALSE)
}

# ── Read BoP curves ───────────────────────────────────────────────────────────
bop_ihd    <- read_csv(file.path(BOP_DIR, "ihd.csv"),    show_col_types = FALSE)
bop_stroke <- read_csv(file.path(BOP_DIR, "stroke.csv"), show_col_types = FALSE)

message("  BoP IHD    : ", nrow(bop_ihd), " rows | SBP ",
        round(min(bop_ihd$Risk)), "\u2013", round(max(bop_ihd$Risk)), " mmHg")
message("  BoP Stroke : ", nrow(bop_stroke), " rows | SBP ",
        round(min(bop_stroke$Risk)), "\u2013", round(max(bop_stroke$Risk)), " mmHg")

# ── Interpolation helper ──────────────────────────────────────────────────────
interp_log_rr <- function(rows, sbp) {
  xs <- rows$Risk
  ys <- rows[["Log relative risk of outcome"]]
  if (sbp <= xs[1])          return(0)
  if (sbp >= xs[length(xs)]) return(ys[length(ys)])
  i <- max(which(xs <= sbp))
  w <- (sbp - xs[i]) / (xs[i + 1] - xs[i])
  ys[i] + w * (ys[i + 1] - ys[i])
}

rr_at <- function(rows, sbp)
  exp(interp_log_rr(rows, sbp + 10) - interp_log_rr(rows, sbp))

# ── STEP 1: slope stability diagnostic ───────────────────────────────────────
# Computes per-10 mmHg RR at candidate reference SBPs (120-140 mmHg) and
# reports the max % deviation from REF_SBP. This substantiates the claim
# that the local-slope approximation has small error in this range.
REF_SBP   <- 130
CHECK_SBP <- c(120, 125, 130, 135, 140)

slope_diag <- tibble(
  sbp        = CHECK_SBP,
  rr_ihd     = sapply(CHECK_SBP, \(s) rr_at(bop_ihd,    s)),
  rr_stroke  = sapply(CHECK_SBP, \(s) rr_at(bop_stroke, s))
) |>
  mutate(
    pct_dev_ihd    = 100 * abs(rr_ihd    - rr_ihd[sbp == REF_SBP])    /
      rr_ihd[sbp == REF_SBP],
    pct_dev_stroke = 100 * abs(rr_stroke - rr_stroke[sbp == REF_SBP]) /
      rr_stroke[sbp == REF_SBP]
  )

max_dev <- max(slope_diag$pct_dev_ihd, slope_diag$pct_dev_stroke)

message("\n  STEP 1 \u2014 Slope stability (per-10 mmHg RR at candidate SBPs):")
print(slope_diag |> mutate(across(where(is.double), \(x) round(x, 3))))
message("  Max deviation from REF_SBP=", REF_SBP, " across 120-140 mmHg: ",
        round(max_dev, 2), "%")

log_rr_base <- list(
  ihd    = log(rr_at(bop_ihd,    REF_SBP)),
  stroke = log(rr_at(bop_stroke, REF_SBP))
)
message("\n  Base log-RR at SBP=", REF_SBP, " mmHg:")
message("    IHD    : ", round(log_rr_base$ihd,    4),
        "  (RR = ", round(exp(log_rr_base$ihd),    4), ")")
message("    Stroke : ", round(log_rr_base$stroke, 4),
        "  (RR = ", round(exp(log_rr_base$stroke), 4), ")")

# ── STEP 2: Singh et al. 2013 age attenuation ────────────────────────────────
# Figure 1, pooled PSC + APCSC row. Reference: 55-64 age band.
# Singh reports 55-64; its midpoint approximates the GBD 60-64 reference age.
SINGH_REF_AGE <- 55

singh_rr <- tribble(
  ~age_lo, ~age_hi,  ~ihd,  ~ischemic_stroke,  ~ich,  ~hhd,
  0,      34,  1.68,             2.05,   2.11,  2.86,
  35,      44,  1.68,             2.05,   2.11,  2.86,
  45,      54,  1.56,             1.83,   1.89,  2.49,
  55,      64,  1.45,             1.63,   1.66,  2.16,  # reference row
  65,      74,  1.33,             1.44,   1.46,  1.88,
  75,      84,  1.26,             1.28,   1.29,  1.63,
  85,     100,  1.14,             1.10,   1.10,  1.37
)

singh_ref <- singh_rr |> filter(age_lo == SINGH_REF_AGE)

# ── STEP 3: compute age-specific RR for primary causes ───────────────────────
# PRIMARY causes: ihd, ischemic_stroke, ich
# HHD excluded from primary (see header). hhd row will be set to 1.0.
cause_base_primary <- c(
  ihd             = log_rr_base$ihd,
  ischemic_stroke = log_rr_base$stroke,
  ich             = log_rr_base$stroke
)
cause_singh_ref_primary <- c(
  ihd             = pull(singh_ref, ihd),
  ischemic_stroke = pull(singh_ref, ischemic_stroke),
  ich             = pull(singh_ref, ich)
)

rr_primary <- singh_rr |>
  select(age_lo, age_hi, ihd, ischemic_stroke, ich) |>
  pivot_longer(c(ihd, ischemic_stroke, ich),
               names_to = "cause", values_to = "rr_singh") |>
  mutate(
    singh_ref_val = cause_singh_ref_primary[cause],
    base_log_rr   = cause_base_primary[cause],
    attenuation   = log(rr_singh) / log(singh_ref_val),
    rr_per_10mmhg = exp(base_log_rr * attenuation),
    hhd_proxy     = FALSE
  ) |>
  rowwise() |>
  mutate(age = list(seq(age_lo, age_hi))) |>
  ungroup() |>
  unnest(age) |>
  filter(age <= 100) |>
  select(age, cause, rr_per_10mmhg, hhd_proxy)

# Add HHD with rr = 1.0: no SBP incidence RR effect in primary.
# A separate simplified Ettehad CF/direct-mortality effect is still applied
# downstream in interventions_cvd.R; HHD is not fully excluded from BP treatment.
rr_hhd_null <- tibble(
  age           = 0:100,
  cause         = "hhd",
  rr_per_10mmhg = 1.0,
  hhd_proxy     = FALSE
)

rr_all_primary <- bind_rows(rr_primary, rr_hhd_null)

# ── STEP 3b: HHD sensitivity proxy ───────────────────────────────────────────
# IHD BoP base × HHD Singh attenuation. Labelled as sensitivity only.
rr_hhd_sens <- singh_rr |>
  select(age_lo, age_hi, hhd) |>
  mutate(
    cause         = "hhd",
    rr_singh      = hhd,
    singh_ref_val = pull(singh_ref, hhd),
    base_log_rr   = log_rr_base$ihd,
    attenuation   = log(rr_singh) / log(singh_ref_val),
    rr_per_10mmhg = exp(base_log_rr * attenuation),
    hhd_proxy     = TRUE
  ) |>
  rowwise() |>
  mutate(age = list(seq(age_lo, age_hi))) |>
  ungroup() |>
  unnest(age) |>
  filter(age <= 100) |>
  select(age, cause, rr_per_10mmhg, hhd_proxy)

# ── Build final objects ───────────────────────────────────────────────────────
# GBD appendix p.279: "RRs are universal for all countries and sex categories"
expand_sexes <- function(df) {
  bind_rows(df |> mutate(sex = "Female"),
            df |> mutate(sex = "Male")) |>
    select(age, sex, cause, rr_per_10mmhg) |>
    arrange(cause, sex, age)
}

gbd_rr_sbp      <- expand_sexes(rr_all_primary)
gbd_rr_sbp_sens <- expand_sexes(
  bind_rows(rr_primary, rr_hhd_sens)
)

# ── Validate ──────────────────────────────────────────────────────────────────
stopifnot(
  "primary row count"     = nrow(gbd_rr_sbp)      == 101 * 2 * 4,
  "sensitivity row count" = nrow(gbd_rr_sbp_sens) == 101 * 2 * 4,
  "no NAs primary"        = !any(is.na(gbd_rr_sbp$rr_per_10mmhg)),
  "RR >= 1 primary"       = all(gbd_rr_sbp$rr_per_10mmhg >= 1.0),
  "RR <= 3 primary"       = all(gbd_rr_sbp$rr_per_10mmhg <= 3.0),
  "hhd primary = 1"       = all(
    gbd_rr_sbp$rr_per_10mmhg[gbd_rr_sbp$cause == "hhd"] == 1.0
  ),
  "sensitivity only changes HHD" = isTRUE(all.equal(
    gbd_rr_sbp      |> dplyr::filter(cause != "hhd") |> dplyr::arrange(cause, sex, age),
    gbd_rr_sbp_sens |> dplyr::filter(cause != "hhd") |> dplyr::arrange(cause, sex, age),
    check.attributes = FALSE
  ))
)

# ── Save model inputs ─────────────────────────────────────────────────────────
saveRDS(gbd_rr_sbp,      file.path(OUT_DIR, "gbd_rr_sbp.rds"))
saveRDS(gbd_rr_sbp_sens, file.path(OUT_DIR, "gbd_rr_sbp_hhd_sens.rds"))

# ── Audit outputs ─────────────────────────────────────────────────────────────
# sbp_rr_key_values.csv — spot values at key ages for both primary and sensitivity
key_ages <- c(40, 55, 65, 75)

key_vals <- bind_rows(
  gbd_rr_sbp      |> mutate(version = "primary"),
  gbd_rr_sbp_sens |> mutate(version = "sensitivity_hhd_proxy")
) |>
  filter(sex == "Female", age %in% key_ages) |>
  select(version, cause, age, rr_per_10mmhg) |>
  pivot_wider(names_from = age, values_from = rr_per_10mmhg,
              names_prefix = "age_") |>
  arrange(version, cause)

write_csv(key_vals, file.path(VAL_DIR, "sbp_rr_key_values.csv"))

# sbp_rr_summary.csv — per cause summary
summary_tbl <- gbd_rr_sbp |>
  filter(sex == "Female") |>
  group_by(cause) |>
  summarise(
    rr_min    = round(min(rr_per_10mmhg), 4),
    rr_max    = round(max(rr_per_10mmhg), 4),
    rr_age_40 = round(rr_per_10mmhg[age == 40], 4),
    rr_age_55 = round(rr_per_10mmhg[age == 55], 4),
    rr_age_65 = round(rr_per_10mmhg[age == 65], 4),
    rr_age_75 = round(rr_per_10mmhg[age == 75], 4),
    .groups   = "drop"
  ) |>
  mutate(
    source_base_curve  = case_when(
      cause == "ihd"             ~ "BoP IHD (ihd.csv)",
      cause == "ischemic_stroke" ~ "BoP Stroke (stroke.csv)",
      cause == "ich"             ~ "BoP Stroke (stroke.csv)",
      cause == "hhd"             ~ "None — RR=1.0 (excluded from SBP incidence RR pathway)"
    ),
    source_attenuation = case_when(
      cause == "hhd" ~ "None",
      TRUE           ~ "Singh et al. 2013, Figure 1, pooled PSC+APCSC"
    ),
    hhd_proxy_flag = cause == "hhd"
  )

write_csv(summary_tbl, file.path(VAL_DIR, "sbp_rr_summary.csv"))

# sbp_rr_source_metadata.csv
metadata <- tibble(
  item  = c("Base curve",
            "Age attenuation",
            "Reference SBP",
            "Reference age",
            "Sex/country assumption",
            "HHD primary",
            "HHD sensitivity",
            "Approximation"),
  value = c(
    "GBD 2021 Burden of Proof MR-BRT (vizhub.healthdata.org/burden-of-proof/)",
    "Singh et al. PLoS ONE 2013;8(7):e65174, Figure 1 pooled PSC+APCSC",
    paste0(REF_SBP, " mmHg (approx Indonesia mean adult SBP, GBD 2023 exposure)"),
    "55-64 Singh band; midpoint approximates GBD 60-64 reference age (appendix p.279)",
    "Universal — GBD appendix p.279: 'RRs are universal for all countries and sex categories'",
    "rr_per_10mmhg = 1.0; HHD excluded from SBP incidence RR pathway; simplified Ettehad CF/direct-mortality effect may still be applied downstream",
    paste0("IHD BoP base x HHD Singh attenuation; saved as gbd_rr_sbp_hhd_sens.rds. ",
           "This is a sensitivity assumption, not a GBD output."),
    paste0("Local slope at REF_SBP; max slope variation 120-140 mmHg: ",
           round(max_dev, 2), "% (computed above)")
  )
)

write_csv(metadata,   file.path(VAL_DIR, "sbp_rr_source_metadata.csv"))
write_csv(slope_diag |> mutate(across(where(is.double), \(x) round(x, 4))),
          file.path(VAL_DIR, "sbp_rr_slope_diagnostic.csv"))

# ── Report ────────────────────────────────────────────────────────────────────
message("\n── 01b_prepare_sbp_rr.R complete ────────────────────────────────────────")
message("  PRIMARY  : data/gbd/gbd_rr_sbp.rds")
message("             ihd, ischemic_stroke, ich — BoP base x Singh attenuation")
message("             hhd — rr_per_10mmhg = 1.0 (excluded from SBP incidence RR pathway;")
message("                   simplified Ettehad CF/direct-mortality effect retained in primary V1)")
message("  SENSITIV : data/gbd/gbd_rr_sbp_hhd_sens.rds")
message("             hhd proxy: IHD BoP base x HHD Singh attenuation")
message("  Max slope deviation 120-140 mmHg: ", round(max_dev, 2), "%")
message("  Method   : GBD 2021-inspired (local-slope approximation; V2 = category-specific)")
message("\n  Audit outputs:")
message("    outputs/validation/sbp_rr_key_values.csv")
message("    outputs/validation/sbp_rr_summary.csv")
message("    outputs/validation/sbp_rr_source_metadata.csv")
message("\n  Key values (Female, primary):")
gbd_rr_sbp |>
  filter(sex == "Female", age %in% key_ages) |>
  pivot_wider(names_from = cause, values_from = rr_per_10mmhg) |>
  select(age, ihd, ischemic_stroke, ich, hhd) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()
message("  Next: 02_build_demography.R")

