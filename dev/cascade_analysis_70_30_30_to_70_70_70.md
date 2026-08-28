# Claude Code prompt — isolated 70-30-30 → 70-70-70 cascade run

**Goal.** Feed the dedicated cascade input workbook
`indonesia_70_30_30_to_70_70_70_inputs.xlsx` through the existing Indonesia NCD
pipeline (Models 04–09) and produce its **own** cost/value Excel formula
workbook — **without changing how any other intervention analysis runs, and
without its outputs overlapping, appending to, or overwriting any existing
output.** The change must be *minimally invasive*: a new standalone runner plus
the smallest possible backward-compatible patches to existing scripts.

Please inspect the actual code before editing and adapt to the real object and
path names if they differ from what is described here. Preserve the isolation,
backward-compatibility, exact-input, and output-scope requirements below no
matter what.

---

## 1. What already exists (inspect first, do not assume)

Read these before writing anything:

- `00_run_model_cvd_fair.R` — master runner. Defines the paths and the
  execution switches, then `source()`s Models 01–10 in order. Key objects:
  - `wd`, `wd_code`, `wd_data`, `wd_outp <- paste0(wd, "output/")`
  - `model_inputs_file <- paste0(wd, "data/indonesia_model_inputs.xlsx")` — the
    **clinical / FAIR-Choices** input workbook
  - `public_health_inputs_file`, `public_health_cost_value_formulae_file`,
    `combined_cost_value_formulae_file`
  - `cost_value_output_file <- paste0(wd_outp, "indonesia_model_cost_value.xlsx")`
  - `run_clinical_interventions <- TRUE`, `run_public_health_interventions <- TRUE`,
    `run_cost_value <- TRUE`, `baseline_scenario_id <- "baseline"`
- `04_define_interventions_indonesia.R` — builds the scenario catalogues
  (`fair_inputs`, `fair_scenarios`, `public_health_inputs`,
  `public_health_scenarios`, `combined_scenarios`).
- `06_run_scenarios_indonesia_fair.R` — assembles the `scenarios` list **gated by
  the two family switches**, runs each scenario, writes
  `output/out_model/model_output_<country>_<target>.rds`, and keeps the results in
  memory as `results_list`.
- `07_output_dalys.R` — writes `output/dt_output_dalys.rds`,
  `output/07_life_expectancy_lookup.rds`, `output/07_disability_weights.rds`,
  `output/07_cvd_40q30.rds`, `output/07_cvd_40q30_age.rds`.
- `08_economic_value_calculation.R` — **hard-codes `DIR_OUT <- file.path(wd, "output")`**
  and reads BCA parameters from the *Assumptions* sheet of the active input
  workbook(s). Writes `output/08_vsl_results.rds` (+ summary tables,
  `08_bca_parameters.rds`).
- `09_cost_value.R` — builds the cost/value workbooks with **openxlsx**
  (`createWorkbook` / `addWorksheet` / `writeData` / `freezePane` /
  `saveWorkbook`). Writes a *values* workbook (`cost_value_output_file`) and a
  *formula edition* (`cost_value_formulae_file`, derived by
  `sub("\\.xlsx$", "_formulae.xlsx", cost_value_output_file)`), plus the
  public-health workbook via `source_public_health_cost_value()`.

**Cascade input workbook** `indonesia_70_30_30_to_70_70_70_inputs.xlsx` (already
in `data/`) is a *bespoke cascade format*, **not** the clinical workbook format.
Its sheets are: `README`, `Assumptions`, `Dictionaries`,
`Intervention_Cause_Map`, `Effect_Sizes`, `Coverage`, `Coverage_Trajectory`,
`Cost_Components`, `Model_Input_View`, `FairChoices_Methods`, `Raw_NCDRisC`,
`Provincial_Framework`, `Scope_and_Sources`, `QA_Checks`, `ChangesLog`.

Ground truth already inside that workbook:
- Single scenario id: **`S_70_30_30_TO_70_70_70`** (see `Coverage_Trajectory`).
- Component intervention ids: **`I_CVD_PRIMARY`**, **`I_T2D_TREATMENT`**.
- Model causes: `C_IHD`, `C_IS`, `C_ICH`, `C_HHD`, `C_T2D` (5).
- Exact effective coverage: **0.1365 in 2030**, **0.4165 in 2040**, held
  thereafter; `partial_effect_fraction = 0.5` (half effect for treated-but-
  uncontrolled); `Assumptions` carries `analysis_start_year=2025`,
  `analysis_end_year=2050`, `first_target_year=2030`, `final_target_year=2040`,
  `target_diagnosis_share=0.7`, and the 30/30 → 70/70 treatment/control
  conditions. Cholesterol coverage follows hypertension. Diabetes costs are
  present in `Cost_Components`.
- The workbook's own `QA_Checks` already asserts 0.1365 / 0.4165 with "no rounded
  override".

---

## 2. Non-negotiable requirements

### A. Do not disturb any other analysis (minimally invasive)
- The ordinary runner `00_run_model_cvd_fair.R` must behave **exactly** as it does
  now: same scenario catalogues, same scenario ids and counts, same output paths
  and filenames, same clinical / public-health / combined workbooks.
- **Do not** change the default values of `run_clinical_interventions`,
  `run_public_health_interventions`, or `run_cost_value`.
- **Do not** add the cascade to `fair_scenarios`, `public_health_scenarios`, or
  `combined_scenarios` in any normal run.
- **Do not** overload either family switch to mean "cascade". The cascade is
  **opt-in and off by default**, controlled by a *new* flag
  `run_cascade_70_30_30_to_70_70_70` that is `FALSE`/absent unless the new runner
  sets it.
- Any edit to Models 04–09 must be an **additive, backward-compatible** branch
  guarded by that flag or by an optional argument that defaults to today's
  behavior. When the flag is absent/FALSE, the edited code path must be
  byte-for-byte equivalent to current behavior. No large refactors, no
  duplicated pipeline sections.

### B. Isolated inputs
Create a new thin runner, e.g. `00_run_70_30_30_to_70_70_70.R`, that launches a
**clean R session / isolated environment**, reuses the existing model scripts,
and explicitly sets:

```r
run_cascade_70_30_30_to_70_70_70 <- TRUE
run_clinical_interventions      <- TRUE   # cascade rides the clinical machinery
run_public_health_interventions <- FALSE  # PH family excluded from this run
run_cost_value                  <- TRUE

model_inputs_file <- file.path(wd, "data",
                               "indonesia_70_30_30_to_70_70_70_inputs.xlsx")
```

Read **all** assumptions, cascade values, effect sizes, coverage trajectories,
costs, and intervention→cause mappings from that workbook. Do **not** hard-code
the rounded 13.8% / 41.5% (or any cascade number) anywhere in R. Use the exact
workbook values (0.1365, 0.4165) and validate them after reading (fail clearly if
missing, duplicated, non-numeric, or inconsistent with the workbook's own cascade
arithmetic and `QA_Checks`).

Because the cascade workbook is a *different format* from the clinical workbook,
inspect exactly how Model 04 parses `model_inputs_file` into `fair_inputs` /
`fair_scenarios` (the `fair_effect_rows` / effect-row contract Model 06 consumes).
Then either (a) make the reader accept this workbook via a small,
flag-guarded adapter, or (b) build a dedicated cascade reader that emits the same
objects Model 06 expects for a single scenario `S_70_30_30_TO_70_70_70` (with
`I_CVD_PRIMARY` and `I_T2D_TREATMENT` retained as **traceable components** of that
one scenario, not as separate emitted scenarios). Prefer the smallest adapter
that keeps Model 06 unchanged.

### C. Isolated outputs — nothing overlaps
Use a dedicated output root and point **every** Model 06–09 output at it:

```r
cascade_output_dir <- file.path(wd, "output", "70_30_30_to_70_70_70")
dir.create(cascade_output_dir, recursive = TRUE, showWarnings = FALSE)
wd_outp <- paste0(cascade_output_dir, .Platform$file.sep)
```

All of these must land **only** under `cascade_output_dir`:
- Model 06 `out_model/model_output_*.rds`
- Model 07 `dt_output_dalys.rds`, `07_life_expectancy_lookup.rds`,
  `07_disability_weights.rds`, `07_cvd_40q30.rds`, `07_cvd_40q30_age.rds`
- Model 08 `08_vsl_results.rds` (+ summaries), `08_bca_parameters.rds`
- Model 09 audit + formula workbooks

The cascade run must **never** touch:
- `output/out_model/…` (ordinary)
- `output/dt_output_dalys.rds`, `output/08_vsl_results.rds`
- `cost_value_output_file` / `cost_value_formulae_file` (clinical)
- `public_health_cost_value_formulae_file`
- `combined_cost_value_formulae_file`

**Model 08 fix (backward-compatible).** Model 08 currently hard-codes
`DIR_OUT <- file.path(wd, "output")`. Change it to honor `wd_outp` when supplied
(`DIR_OUT <- if (exists("wd_outp")) sub("/+$","",wd_outp) else file.path(wd,"output")`),
keeping the current path as the default. Apply the same "honor `wd_outp`, default
unchanged" rule to any other hard-coded output path you find during inspection.

**Model 08 BCA parameters.** Model 08 reads BCA/VSL parameters
(`bca_base_year`, `bca_discount_rate_primary`, `vsl_us_gni_ratio`,
`vsl_income_elasticity_preferred`, `vsl_floor_gni_multiple`, `vsly_adult_min_age`,
`vsly_adult_max_age`, `cost_to_bca_currency_factor`, …) from the **Assumptions**
sheet of the active workbook. Confirm whether the cascade workbook's Assumptions
sheet carries this full set. If it does **not**, do **not** silently fall back to
the clinical workbook — add the missing BCA parameters to the cascade workbook's
Assumptions sheet (documented, sourced) so the isolated run is self-contained, and
fail loudly if any required BCA parameter is absent.

**Fail-fast collision check.** Before execution, assert that every resolved
cascade output path is under `cascade_output_dir` and that none equals an existing
ordinary clinical / public-health / combined output path. Stop with a clear error
if any collision is detected.

### D. Cascade modeling rules (all from the workbook)
- Scale from the modifiable baseline to exactly **0.1365** by **2030**, then to
  exactly **0.4165** by **2040**, held constant thereafter.
- Apply **half** the treatment effect to treated-but-uncontrolled patients
  (`partial_effect_fraction = 0.5`).
- Use the workbook's exact hypertension and diabetes cascade inputs; cholesterol
  treatment coverage follows hypertension coverage.
- Preserve any documented no-backsliding rule.
- Use the updated costs that include diabetes mellitus.
- Never substitute rounded display values for exact workbook values.

### E. Scenario scope
Use a dedicated namespace and keep the run to exactly two scenarios:

```r
cascade_scenario_id <- "S_70_30_30_TO_70_70_70"
cascade_family      <- "cascade_70_30_30_to_70_70_70"
```

The isolated run may contain **only** the shared no-new-intervention `baseline`
comparator and `S_70_30_30_TO_70_70_70`. After Model 04, assert the catalogue
contains no unrelated clinical / public-health / combined scenario. After Model
06, assert every modeled row carries one of the two permitted scenario ids.

---

## 3. The cascade cost/value formula workbook (its own file, no overlap)

Write it to a unique path:

```
output/70_30_30_to_70_70_70/indonesia_70_30_30_to_70_70_70_cost_value_formulae.xlsx
```

It must:
- Follow the **exact structure and conventions** of the clinical formula workbook
  produced by `09_cost_value.R` — same openxlsx build pattern, and, where the
  content applies, the same standard sheet names (`Health_Outcomes`,
  `Economic_Value`, `Benefit_Cost`, `CVD_40q30`, `CVD_40q30_Age`,
  `Cost_Components`, `Annual_Mortality`, `Annual_Cost`, `Budget_Impact`,
  `Cost_Effectiveness`, `Calculation_Assumptions`, `Selected_Interventions`,
  `Scenario_Catalog`, `QA_Checks`, `Methods_and_Sources`, `Calculation_Map`,
  `Run_Metadata`, `README`).
- Preserve colors, fonts, input/formula/source cell styles, number formats,
  freeze panes, filters, and QA conventions from the clinical workbook.
- Contain **live Excel formulas** wherever the clinical formula workbook uses
  formulas (annual costs, component costs, budget impact, health outcomes,
  economic value, benefit-cost, cost-effectiveness, discounting, baseline-vs-
  scenario comparisons, CVD 40q30 where applicable, reconciliation checks) — not
  pasted R numbers. Every derived value references a visible assumption/source
  cell; **no magic numbers** (cascade %, unit costs, discount rates) baked into
  formulas.
- Include **only** the `baseline` and `S_70_30_30_TO_70_70_70` scenarios, and only
  the hypertension/cholesterol + diabetes cost components from this cascade
  workbook. **Exclude** all other clinical, public-health, and combined
  interventions. Fail with a clear diagnostic if a selected cascade component
  lacks a required unit cost; verify diabetes costs are present.
- **Never** be appended to or merged into the clinical, public-health, or combined
  workbooks. Do **not** rename existing standard sheets to label them "cascade".

Add these cascade-specific sheets only where the standard structure doesn't
already cover them:
- `Cascade_Assumptions` — the exact editable inputs and their source-workbook
  cells (including how to change the diabetes control-rate baseline).
- `Cascade_Trajectory` — by year: baseline coverage, target cascade components,
  treated-but-uncontrolled share, controlled share, half-effect adjustment, exact
  effective coverage, and the final modeled coverage used by R.
- `Cascade_QA` — reconcile the Excel formulas to the R calculations and confirm
  the 2030 (0.1365) and 2040 (0.4165) milestones.

---

## 4. Validation to run and report

1. **Regression** — run the ordinary pipeline with the cascade disabled; confirm
   scenario ids, counts, and every output path/filename match pre-change behavior
   (diff the file list and the scenario catalogue).
2. **Isolation** — run the cascade runner; confirm every file written is under
   `output/70_30_30_to_70_70_70/`.
3. **Scenario scope** — cascade results contain only `baseline` and
   `S_70_30_30_TO_70_70_70`.
4. **Coverage (unrounded)** — effective coverage is `2030 = 0.1365`,
   `2040 = 0.4165`, `post-2040 = 0.4165` (floating-point tolerance, but print
   enough decimals to show rounded approximations were not used).
5. **Costs** — output includes the cascade hypertension/cholesterol **and**
   diabetes cost components, and no costs from unrelated interventions.
6. **Workbook scope** — every non-baseline row in every result sheet belongs to
   the cascade scenario.
7. **Formula errors** — scan the workbook for `#REF!`, `#DIV/0!`, `#VALUE!`,
   `#NAME?`, `#N/A`; none unexplained.
8. **Reconciliation** — principal Excel formula outputs reconcile to their
   R-calculated values within the clinical workbook's tolerance.
9. **Collision** — confirm no ordinary clinical / public-health / combined output
   file was created, modified, or overwritten by the cascade run (compare mtimes
   / hashes before and after).

---

## 5. Deliverables

1. The new standalone runner `00_run_70_30_30_to_70_70_70.R`.
2. Minimal, flag-guarded, backward-compatible patches to Models 04–09 (call out
   the Model 08 `DIR_OUT`/`wd_outp` fix and any adapter added to Model 04).
3. The separate cascade formula workbook at the path in §3.
4. A short **README** covering: how to run the cascade; which input workbook it
   uses; where its outputs are written; how to change the diabetes control-rate
   baseline; how exact effective coverage is calculated; how isolation from the
   other interventions is enforced.
5. A **change log** listing every modified file and why the change was necessary
   and safe.
6. The **validation results**, including the ordinary-run regression comparison
   and the full list of cascade output files.

**Before editing, inspect the repo and identify the smallest safe integration
points.** If real object names or paths differ from those above, adapt to the
actual code — but keep the isolation, backward-compatibility, exact-input, and
output-scope guarantees intact.
