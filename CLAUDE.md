# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A population-based discrete-time Markov (state-transition) simulation framework evaluating health and economic impacts of NCD policies in Indonesia. Two independent disease modules: **CVD** (IHD, ischemic stroke, intracerebral hemorrhage, hypertensive heart disease) and **Cancer** (16 cancer types from GBD 2021). Projects outcomes 2025–2050 under policy scenarios.

**Core design**: Population → Risk Factors → Markov transitions → Outcomes (deaths, DALYs, costs). Conditional independence across diseases; PAF-based incidence; no comorbidity modeled (v1.0).

## Directory Layout

```
├── code/
│   ├── cvd/             # CVD Markov model pipeline (numbered scripts)
│   ├── cancer/          # Cancer Markov + cohort component projection
│   │   ├── fnx/         # Function library (~20 reusable functions)
│   │   └── scripts/     # Numbered execution scripts (00–06)
│   ├── demography/      # Demographic models (placeholder)
│   └── utils/           # Shared utilities (placeholder)
├── data/
│   ├── raw/             # GBD 2023, UNWPP 2024, IHME, NCD-RisC, ETIHAD (not on GitHub)
│   ├── interim/         # Working/intermediate datasets
│   └── processed/       # Production-ready .rds/.csv/.xlsx files
├── output/
│   ├── paper/           # Publication-ready summary .rds files
│   └── slides/          # Presentation-ready .rds + .png files
├── scenarios/
│   ├── cvd/             # CVD R Markdown reports (aim1_report.Rmd)
│   └── cancer/          # Cancer R Markdown reports
├── reports/             # LaTeX/Beamer templates, bibliography, logos
├── tests/               # Test suite (placeholder)
└── docs/                # Documentation, diagrams, prompts
```

## Running the Models

### CVD pipeline

```r
source("code/cvd/00_run_model.R")
```

Configuration flags at the top of `00_run_model.R` control which pipeline stages execute:
- `run_calibration_par` — parallel calibration
- `run_adjustment_model` — post-calibration adjustments
- `run_adjustments_inputs` — baseline rate adjustments
- `run_bgmx_trend` — background mortality secular trends
- `run_CF_trend` / `run_CF_trend_80` — case fatality secular trends (80% = net of HTN control contribution)
- `run_CF_trend_ihme` — IHME-based case fatality trend (default `FALSE`)
- `run_aod_par` — dementia model (not yet implemented, default `FALSE`)

Set flags to `FALSE` to skip expensive stages when intermediate `.rds` files already exist.

### Cancer pipeline

Cancer scripts run sequentially from `code/cancer/scripts/` (00–06). Initialize with `library.R`, which loads packages, reads `settings.yml`, and sources all functions from `fnx/`. Requires internet access for remote helper functions (falls back to local paths via `settings.yml`).

## Package Dependencies

No `renv.lock` exists. Install manually:

```r
# CVD
install.packages(c("data.table", "dplyr", "tidyr", "ggplot2", "RColorBrewer",
                   "readxl", "countrycode", "stringr", "parallel", "doParallel",
                   "foreach", "gmodels", "forecast"))

# Cancer (additional)
install.packages(c("tidyverse", "yaml", "curl"))
```

## CVD Pipeline Architecture

`00_run_model.R` sources orchestration scripts that in turn source numbered sub-scripts:

| Script | Role |
|--------|------|
| `00_run_model.R` | Entry point; sources all others, defines config flags |
| `01_utils.R` | Utility functions: `get.bp.prob()`, TFA mortality reduction, ETIHAD RR calculations |
| `02_load_inputs.R` | Orchestrator — sources 020–023 below |
| → `021_get_base_rates.R` | Extracts GBD 2023 mortality + prevalence; interpolates to single-year ages 20–95; emits `baseline_rates_part*.rds` |
| → `022_get_tps.R` | Health-state transition probabilities |
| → `023_get_tps_bgmx.R` | Background mortality trend forecasts |
| `03_calibration.R` | Orchestrator — sources 031–032 below |
| → `031_calibration.R` | Calibrates to GBD estimates (can run in parallel) |
| → `032_adjustments.R` | Enforces constraint IR + BG.mx ≤ 1; repeats baseline for projection period |
| `04_define_interventions.R` | Defines all policy scenarios (sodium, TFA, statins, HTN control); emits scenario `.rds` files and `covfxn2.csv` |
| `05_build_baseline.R` | Loads population + risk factor data; prepares Markov inputs |
| `06_run_scenarios.R` | Core simulation engine; applies interventions via ETIHAD/GBD RRs |
| `07_output_dalys.R` | Computes YLL, YLD, DALYs using GBD disability weights + WPP 2024 life tables |
| `08_economic_value_calculation.R` | Healthcare costs, productivity losses, cost-effectiveness (VSL/VSLY) |

**Note**: `00_run_model.R` sources `06_run_scenarios_multiple.R` — this file does not currently exist on disk (likely renamed to `06_run_scenarios.R`). Update the source call if running end-to-end.

## Cancer Pipeline Architecture

Scripts in `code/cancer/scripts/` run in numbered order (00–06). Functions live in `code/cancer/fnx/` and are auto-sourced by `library.R`.

Key function groups:
- **Markov core**: `calc_bsln_tps_dt`, `calc_intv_tps`, `project_markov_trace_dt`, `correct_markov`
- **Population projection**: `run_ccpm` (cohort component projection model), `proj_ccpm_markov_wip` (WIP integration)
- **Metrics**: `calc_metric_dt`, `calc_rates`, `gen_lifetable`
- **Scenario processing**: `process_intv_scen_inputs`, `id_target_cc_methods`

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

## Quality Checks

No formal test suite. Validation is embedded:
- `021_get_base_rates.R`: `print(anyNA(data.out))` after joins
- `032_adjustments.R`: `test = ifelse(IR + BG.mx > 1, 1, 0)` flags invalid transition probabilities
- `05_build_baseline.R`: checks location coverage completeness
- Cancer: `correct_markov.R` validates/corrects Markov transition matrices
