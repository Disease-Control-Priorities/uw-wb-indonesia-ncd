# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A population-based discrete-time Markov (state-transition) simulation framework evaluating health and economic impacts of NCD policies in Indonesia. Two independent disease modules: **CVD** and **Cancer** (16 cancer types from GBD 2021). Projects outcomes 2025–2050 under policy scenarios.

The CVD module exists in two parallel forms: a **generic/SEARO** pipeline (`*.R`) and an **Indonesia-specific** pipeline (`*_indonesia.R`). The Indonesia pipeline is the active, maintained path — its entry point is `00_run_model_indonesia.R`. The Indonesia variant models six causes (IHD, ischemic stroke, intracerebral hemorrhage, hypertensive heart disease, rheumatic heart disease, cardiomyopathy & myocarditis); the generic variant models the first four.

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
│   ├── out_model/       # Per-run raw simulation .rds (gitignored; consumed by 09_validation)
│   ├── paper/           # Publication-ready summary .rds files
│   └── slides/          # Presentation-ready .rds + .png files
├── scenarios/
│   ├── cvd/             # CVD R Markdown reports (aim1_report.Rmd)
│   └── cancer/          # Cancer R Markdown reports
├── reports/             # LaTeX/Beamer; technical_presentation.Rmd/.tex, preamble, refs
├── tests/               # Test suite (placeholder)
└── docs/                # Documentation, diagrams, prompts.txt
```

A scratch directory `../temp/` (sibling of the repo) is auto-created by the run scripts for intermediate processing.

## Running the Models

### CVD pipeline

```r
# Active Indonesia pipeline:
source("code/cvd/00_run_model_indonesia.R")

# Generic/SEARO pipeline (legacy):
source("code/cvd/00_run_model.R")
```

The entry script hard-codes `wd` to an absolute local path — update it before running on another machine. Configuration flags near the top control which stages execute:
- `run_calibration_par` — parallel calibration
- `run_adjustment_model` — post-calibration adjustments (set `FALSE` when using transparent calibration, see Gotchas)
- `run_adjustments_inputs` — baseline rate adjustments
- `run_bgmx_trend` — background mortality secular trends
- `run_CF_trend` / `run_CF_trend_80` — case fatality secular trends (80% = net of HTN control contribution)
- `run_CF_trend_ihme` — IHME-based case fatality trend (default `FALSE`)
- `run_aod_par` — dementia model (not yet implemented, default `FALSE`)

Set flags to `FALSE` to skip expensive stages when intermediate `.rds` files already exist.

### Cancer pipeline

Cancer scripts run sequentially from `code/cancer/scripts/` (00–06). Initialize with `library.R`, which loads packages, reads `settings.yml`, and sources all functions from `fnx/`. Requires internet access for remote helper functions (falls back to local paths via `settings.yml`). Note: `settings.yml` is not committed to the repo and must be present locally; `library.R` resolves paths relative to a `root` ("R/") prefix it expects to be set.

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

The entry script sources orchestration scripts that in turn source numbered sub-scripts. Most stages have a generic version and an `_indonesia` version; the table below lists the role and notes the Indonesia variant where it diverges. The active `00_run_model_indonesia.R` sources the `_indonesia` variants of stages 02, 05, 06 and adds stage 09.

| Script (generic / `_indonesia`) | Role |
|--------|------|
| `00_run_model.R` / `00_run_model_indonesia.R` | Entry point; sets paths, defines config flags, sources all stages |
| `01_utils.R` | Utility functions: `get.bp.prob()`, TFA mortality reduction, ETIHAD RR calculations (shared) |
| `02_load_inputs(_indonesia).R` | Orchestrator — sources 021–023 below |
| → `021_get_base_rates(_indonesia).R` | Extracts GBD 2023 mortality + prevalence; interpolates to single-year ages 20–95; emits `baseline_rates_part*.rds`, `locs.rds` |
| → `022_get_tps(_indonesia).R` | Health-state transition probabilities; emits `tps_inpt_part*.rds` |
| → `023_get_tps_bgmx(_indonesia).R` | Background mortality trend forecasts |
| `03_calibration.R` | Orchestrator — sources 031 + 032 (the two-pass grid-search calibration) |
| → `031_calibration(_indonesia).R` | Calibrates to GBD estimates (±5% IR/CF grid; can run in parallel) |
| → `032_adjustments(_indonesia).R` | Second asymmetric grid; enforces IR + BG.mx ≤ 1; repeats baseline for projection period; emits `adjustments2023_age.csv` |
| `03_calibration_indonesia_transparent.R` | **Alternative** single-pass calibration that replaces 031+032 with one documented symmetric multiplicative search (per-combo or per-age-group), drop-in writing `adjusted_searo_part*.rds`. See header for usage. |
| `04_define_interventions.R` | Defines all policy scenarios (sodium, TFA, statins, HTN control); emits scenario `.rds` files and `covfxn2.csv` (shared) |
| `05_build_baseline(_indonesia).R` | Loads population + risk factor data; prepares Markov inputs (re-applies `adjustments2023_age.csv` when `run_adjustment_model == TRUE`) |
| `06_run_scenarios(_indonesia).R` | Core simulation engine; applies interventions via ETIHAD/GBD RRs; Indonesia variant runs in parallel and writes per-chunk `.rds` to `output/out_model/` |
| `07_output_dalys.R` | Computes YLL, YLD, DALYs using GBD disability weights + WPP 2024 life tables (shared) |
| `08_economic_value_calculation.R` | Healthcare costs, productivity losses, cost-effectiveness (VSL/VSLY) (shared) |
| `09_validation_indonesia.R` | Validation: compares baseline model deaths/rates against GBD 2023 and UNWPP 2024 by cause and sex (Indonesia only) |

**Note**: the legacy `00_run_model.R` sources `06_run_scenarios_multiple.R`, which does not exist on disk — use `06_run_scenarios.R` (or the Indonesia pipeline). `03_calibration.R` sources the *generic* `031`/`032`, not the `_indonesia` variants, even from the Indonesia entry point; to calibrate with the Indonesia-specific or transparent logic, source those scripts directly.

## Cancer Pipeline Architecture

Scripts in `code/cancer/scripts/` run in numbered order. Functions live in `code/cancer/fnx/` and are auto-sourced by `library.R`.

| Script | Role |
|--------|------|
| `00_read_data.R` | Read raw GBD/inputs |
| `01_process_country_param.R` | Country-level parameters |
| `02_process_bsln_scen.R` | Baseline scenario processing |
| `03_process_pop_proj_bsln_dt.R` | Baseline population projection (`*_old.R` is a superseded variant) |
| `04_process_intv_scen.R` | Intervention scenario processing |
| `05_process_pop_proj_intv_dt.R` | Intervention population projection |
| `06_dissertation_output.R` | Output/figures for write-up |

`Diagnostics.Rmd` (in `scripts/`) and `scratchpad.R` are exploratory, not part of the run order.

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
- `03_calibration_indonesia_transparent.R`: end-of-run `stopifnot` asserting IR/CF ∈ [0,1], IR/CF + BG.mx ≤ 1, and row-count/schema parity with input
- `09_validation_indonesia.R`: cross-source comparison of baseline mortality vs GBD 2023 and UNWPP 2024
- `05_build_baseline.R`: checks location coverage completeness
- Cancer: `correct_markov.R` validates/corrects Markov transition matrices

## Gotchas

- **Hard-coded paths**: CVD entry scripts set `wd` to an absolute Windows path; edit before running elsewhere.
- **Double-calibration**: if you use `03_calibration_indonesia_transparent.R`, set `run_adjustment_model <- FALSE` before stage 05, or `adjustments2023_age.csv` from 032 will be multiplied in a second time. See the script header.
- **Two CVD pipelines coexist**: the generic `*.R` and Indonesia `*_indonesia.R` scripts read/write overlapping intermediate files in `data/processed/`. Run one pipeline end-to-end rather than mixing stages.
- **Raw data not on GitHub**: `data/raw/**` is gitignored (only `.md` files kept). The model expects GBD 2023, UNWPP 2024, IHME, NCD-RisC, and ETIHAD inputs to be staged locally.
