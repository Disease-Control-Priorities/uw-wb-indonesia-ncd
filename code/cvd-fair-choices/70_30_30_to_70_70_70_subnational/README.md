# 70-30-30 → 70-70-70 SUBNATIONAL cascade run — README

A **stand-alone, isolated** province-level analysis of the Indonesia
hypertension/cholesterol + type-2-diabetes treatment cascade that scales from
**70-30-30** (70 % diagnosed, 30 % of diagnosed treated, 30 % of treated
controlled) by **2030** to **70-70-70** by **2040**, held thereafter — computed
**independently for every one of the 38 current Indonesian provinces**.

It reuses the analytic *logic* of the national FAIR-Choices cascade (Models
04–09) as **adapted copies living entirely in this directory**. It **never**
sources any production `code/cvd-fair-choices/00-09*.R` script at run time, and it
**never** runs Model 01 (utils), Model 02 (load inputs), or Model 03
(calibration).

## Deliberate numbering gap: there is no 01, 02 or 03

The reconciled province rate table

```
data/processed/b_rates_full_period_reconciled_2017_2050_national_current38.rds
```

is the **final, already-prepared and already-calibrated** model-rate input. It
incorporates the data preparation that would otherwise have happened in Models
01/02 **and** the calibration that would otherwise have happened in Model 03. The
runner therefore loads it **directly** and begins the analytical pipeline at the
adapted **Model 04** stage. Re-running 01/02/03 here is neither required nor
permitted, so **no `01`, `02` or `03` script exists in this directory**.

> Note: the task brief names the rate file with a `.R` extension; the actual
> serialized artifact is an **RDS** with the identical basename. The runner loads
> it robustly (inspecting the object, not trusting the extension) and fails
> loudly if it does not contain usable province data — it never silently
> substitutes a national-only file or invokes calibration.

## How to run

From a fresh R session, **from the repository root**:

```r
source("code/cvd-fair-choices/70_30_30_to_70_70_70_subnational/00_run_70_30_30_to_70_70_70_subnational.R")
```

or from a shell:

```bash
Rscript code/cvd-fair-choices/70_30_30_to_70_70_70_subnational/00_run_70_30_30_to_70_70_70_subnational.R
```

The runner detects the repository root from the `uw-wb-indonesia-ncd.Rproj`
marker (no hard-coded Windows/OneDrive paths; no `setwd()` dependency). It runs
the adapted Models 04–09 in order.

## Files

| File | Role |
|------|------|
| `00_run_70_30_30_to_70_70_70_subnational.R` | Runner: root detection, config, **direct b_rates load**, fail-fast collision guard, sources only 04–09, post-run isolation/collision validation. |
| `04_define_interventions_70_30_30_to_70_70_70_subnational.R` | Province-keyed cascade catalogue builder. Reads the provincial sheets, builds per-(province × link × sex) effect rows with the exact `coverage_path`, reconciles the R multiplier against `Provincial_Model_Input_View` (hard FAIL > 1e-6), province-expands the cost catalogue. |
| `05_build_baseline_70_30_30_to_70_70_70_subnational.R` | Keeps the 38 provinces (+ the national row, for reconciliation only) from the reconciled table, de-duplicates the key, runs fail-fast data-contract + sick→dead guards. |
| `06_run_scenarios_70_30_30_to_70_70_70_subnational.R` | Stripped Markov engine (b_rates slice + `fair_wb` effect application + well/sick/dead recurrence), run province-by-province. Effect rows are filtered per province. |
| `07_output_dalys_70_30_30_to_70_70_70_subnational.R` | Per-province deaths/cases/YLL/YLD/DALY/life-years and CVD 40q30. National disability weights and life expectancy are broadcast to every province (documented limitation). |
| `08_economic_value_70_30_30_to_70_70_70_subnational.R` | Reference-Case VSL/VSLY: national per-capita valuation × province-specific deaths/life-years. Province population from the model. |
| `09_cost_value_70_30_30_to_70_70_70_subnational.R` | The deliverable: one province-stacked **formula** workbook. |
| `README.md` | This file. |

## Inputs

* **Rates:** `data/processed/b_rates_full_period_reconciled_2017_2050_national_current38.rds`
  (39 locations = Indonesia + 38 provinces; 7 causes; ages 0–95; years 2017–2050).
* **Intervention / cost workbook:**
  `data/indonesia_70_30_30_to_70_70_70_inputs_subnational.xlsx` — the national
  cascade contract sheets (`Assumptions`, `Intervention_Cause_Map`,
  `Effect_Sizes`, `Cost_Components`, …) **plus** the provincial extensions
  (`Provincial_Framework`, `Provincial_Coverage_Source`, `Provincial_Cascade`,
  `Provincial_Trajectory`, `Provincial_Model_Input_View`). This workbook is
  consumed **unmodified**; it already carries all 16 Reference-Case BCA/VSL
  parameters and the exact per-province coverage path.

## Output

Everything lands **only** under `output/70_30_30_to_70_70_70_subnational/`:

```
output/70_30_30_to_70_70_70_subnational/
├── out_model/model_output_<Province>.rds                 (Model 06, per location)
├── dt_output_dalys.rds  07_life_expectancy_lookup.rds  07_disability_weights.rds
├── 07_cvd_40q30.rds  07_cvd_40q30_age.rds
├── 08_vsl_results.rds (+ .csv, summaries)  08_bca_parameters.rds
├── indonesia_70_30_30_to_70_70_70_cost_value_subnational.xlsx           (not emitted; base name)
└── indonesia_70_30_30_to_70_70_70_cost_value_formulae_subnational.xlsx  ← DELIVERABLE
```

The deliverable is the **province-stacked formula workbook**
`indonesia_70_30_30_to_70_70_70_cost_value_formulae_subnational.xlsx`: every
province-varying sheet is stacked in long format with `location` and
`province_code`, every aggregate/lookup formula carries a `location` criterion,
per-capita costs use each province's own population, and the workbook recalculates
on open. It follows the national formula workbook's sheet structure, styles and
formula conventions, adding province-aware `Cascade_Assumptions` /
`Cascade_Trajectory` / `Cascade_QA` and a `Province_Reconciliation` sheet.

## Cascade semantics (preserved from the national run)

* Hypertension and cholesterol coverage use the **provincial CVD treatment
  anchor**; cholesterol follows hypertension.
* Provincial diabetes treatment moves up/down **proportionally** to the
  provincial CVD treatment/capacity multiplier.
* Effective coverage = `controlled_share + 0.5 × treated_uncontrolled_share`
  (treated-but-uncontrolled receive half the effect); milestones 0.1365 (2030)
  and 0.4165 (2040).
* Scale-up is **piecewise-linear** to 2030 then 2040, held thereafter, with the
  **no-backsliding** rule: a province whose baseline effective coverage already
  exceeds a milestone uses `max(province baseline, target floor)` — coverage is
  never rounded before applying effects.
* The exact province × sex × intervention × year coverage path is read
  **verbatim** from `Provincial_Trajectory`; R reproduces the workbook's
  `Provincial_Model_Input_View` transition multipliers to machine precision
  (verified `max |diff| = 0`).

## Documented limitations (flagged in the workbook)

* Provincial coverage values are **crude anchors requiring local validation**.
* **National** (Indonesia) life expectancy, disability weights and VSL/VSLY
  per-capita parameters are applied to every province (no province-specific
  inputs exist); province-specific quantities (deaths, cases, DALYs, life-years,
  population) drive all province results.

## Isolation guarantees

* A fail-fast **collision guard** asserts every resolved output path is under
  `output/70_30_30_to_70_70_70_subnational/` and equals no ordinary output, and
  snapshots the ordinary outputs (mtime + size) so the post-run validation proves
  none was touched (clinical, public-health, combined **and** the national
  cascade tree).
* `run_public_health_interventions = FALSE`; the catalogue is asserted to be
  exactly `{baseline, S_70_30_30_TO_70_70_70}`.
* No production `00-09*.R` script is sourced; there is no 01/02/03 stage.
