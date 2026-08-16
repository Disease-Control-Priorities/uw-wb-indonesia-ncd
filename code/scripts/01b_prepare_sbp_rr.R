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
