# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A population-based discrete-time Markov (state-transition) simulation framework evaluating health and economic impacts of NCD policies in Indonesia. Focuses on CVD (IHD, ischemic stroke, intracerebral hemorrhage, hypertensive heart disease) and cancer. Projects outcomes 2025–2050 under policy scenarios.

**Core design**: Population → Risk Factors → Markov transitions → Outcomes (deaths, DALYs, costs). Conditional independence across diseases; PAF-based incidence; no comorbidity modeled (v1.0).

## Running the Model

```r
# Full pipeline — sources all scripts in order
source("code/cvd/00_run_model.R")
```

Configuration flags at the top of `00_run_model.R` control which pipeline stages execute:
- `run_calibration_par` — parallel calibration
- `run_adjustment_model` — post-calibration adjustments
- `run_adjustments_inputs` — baseline rate adjustments
- `run_bgmx_trend` — background mortality secular trends
- `run_CF_trend` / `run_CF_trend_80` — case fatality secular trends

Set flags to `FALSE` to skip expensive stages when intermediate `.rds` files already exist.

## Package Dependencies

No `renv.lock` exists. Install manually:

```r
install.packages(c("data.table", "tidyverse", "openxlsx", "readxl",
                   "countrycode", "stringr", "parallel", "doParallel",
                   "foreach", "gmodels", "forecast"))
```

## Pipeline Architecture

Scripts in `code/cvd/` run in numbered order:

| Script | Role |
|--------|------|
| `00_run_model.R` | Entry point; sources all others |
| `01_utils.R` | Utility functions: `get.bp.prob()`, TFA mortality reduction, ETIHAD RR calculations |
| `021_get_base_rates.R` | Extracts GBD 2023 mortality + prevalence; interpolates to single-year ages 20–95; emits `baseline_rates_part*.rds` |
| `022_get_tps.R` | Health-state transition probabilities |
| `023_get_tps_bgmx.R` | Background mortality transition probabilities |
| `031_calibration.R` | Calibrates to GBD estimates |
| `032_adjustments.R` | Enforces constraint IR + BG.mx ≤ 1; repeats baseline for projection period |
| `04_define_interventions.R` | Defines all policy scenarios (sodium, TFA, statins, HTN control); emits scenario `.rds` files and `covfxn2.csv` |
| `05_build_baseline.R` | Loads population + risk factor data; prepares Markov inputs |
| `06_run_scenarios.R` | Core simulation engine; applies interventions via ETIHAD/GBD RRs |
| `07_output_dalys.R` | Computes YLL, YLD, DALYs using GBD disability weights + WPP 2024 life tables |
| `08_economic_value_calculation.R` | Healthcare costs, productivity losses, cost-effectiveness |

## Intervention Scenario Logic (`04_define_interventions.R`)

**Sodium**: Linear interpolation 2025–2030 to target (−15% Progress, −30% Aspirational), then held flat. Affects BP distribution via `get.bp.prob()` coefficients.

**Trans-fat (TFA)**: RR = 1.28 per 2% TFA. Linear scale-down to 0%; once eliminated stays 0.

**Statins**: Linear scale-up to 60% coverage by 2050 (or logistic S-curve option).

**HTN control** (most complex, ~850 lines): Three scenarios using quadratic scale-up functions fit to NCD-RisC historical data (1990–2019). Outputs `covfxn2.csv` with columns: `location, year, control, Progress, Aspirational, Business_as_usual, p_change, a_change, aroc`.

## Markov Simulation Engine (`06_run_scenarios.R`)

State space per age/sex/location/cause/year: **Susceptible → Incident → Prevalent → Dead**.

Intervention effects applied via ETIHAD trial RRs per 10 mmHg BP reduction, coverage-adjusted:
```
IR_treated = IR_baseline × (1 − coverage × RR_effect)
```

Multi-intervention stacking order: Sodium → BP shift → IR reduction; Statins → direct IR reduction; TFA → IR via RR; BP control coverage → treatment effect.

## Data Layout

- `data/raw/` — GBD 2023, UNWPP 2024 population, IHME dietary risk factors, NCD-RisC HTN control, ETIHAD RR tables, GBD life tables
- `data/processed/` — intermediate `.rds` files (baseline rates, adjusted rates, scenario definitions)
- `output/` — final figures (PNG) and `dt_output_dalys.rds`
- `scenarios/` — R Markdown reports

## Quality Checks

No formal test suite. Validation is embedded:
- `021_get_base_rates.R`: `print(anyNA(data.out))` after joins
- `032_adjustments.R`: `test = ifelse(IR + BG.mx > 1, 1, 0)` flags invalid transition probabilities
- `05_build_baseline.R`: checks location coverage completeness
