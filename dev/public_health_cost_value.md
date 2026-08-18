# Claude Code prompt: public-health intervention pipeline and formula workbook

Work in this repository folder:

`code/cvd-fair-choices`

Modify only these four production scripts, and only where needed:

- `00_run_model_cvd_fair.R`
- `04_define_interventions_indonesia.R`
- `06_run_scenarios_indonesia_fair.R`
- `09_cost_value.R`

Do not edit other files. Do not refactor, rename, reformat, or rewrite unrelated functions or lines. Preserve existing clinical-intervention behavior unless a minimal compatibility change is essential. Before editing, inspect `git diff`, the current implementations of these four scripts, the attached public-health input workbook, and the attached clinical formula-output workbook.

## Reference files

Use the attached files as follows:

- `indonesia_model_inputs_ public_health.xlsx`: authoritative public-health input contract and source of truth for selected policies, intervention–risk–cause mappings, policy levers, exposure baselines and targets, response parameters, lags, costs, analysis years, discounting, and validation fields. In the repository, locate the corresponding actual input filename/path; do not silently create a second conflicting copy merely because the attachment name contains a space.
- `indonesia_model_cost_value_formulae.xlsx` (the attached clinical-intervention example): reference for workbook structure, formula lineage, styling, colors, filters, frozen panes, number formats, QA, and user experience. Do not copy clinical assumptions or clinical effect logic into the public-health analysis.

The new public-health deliverable must be:

`output/indonesia_cost_value_public_health_formulae.xlsx`

It must be a real, fully formatted, downloadable `.xlsx` workbook—not an unformatted data dump—and its derived results must contain working Excel formulas.

## Required behavior by script

### 1. `00_run_model_cvd_fair.R`

Add two clear logical execution parameters in the existing configuration section:

```r
run_public_health_interventions <- TRUE
run_clinical_interventions      <- TRUE
```

Both must be user-editable and independently honored:

- `TRUE/FALSE`: public-health interventions only;
- `FALSE/TRUE`: clinical interventions only;
- `TRUE/TRUE`: both families;
- `FALSE/FALSE`: stop early with a clear error because there is no intervention analysis to run.

Also declare, in Model 00 only, the public-health input path and public-health formula-workbook output path. Use names that cannot be confused with the existing clinical paths, for example:

```r
public_health_inputs_file <- paste0(wd, "data/indonesia_model_inputs_public_health.xlsx")
public_health_cost_value_formulae_file <- paste0(
  wd_outp, "indonesia_cost_value_public_health_formulae.xlsx"
)
```

First discover and use the actual repository filename. Keep execution-level switches and paths in Model 00; keep analytic parameters in the Excel input workbook. Do not duplicate exposure, effect, lag, cost, coverage, year, or discount assumptions in R.

Source Models 04, 06, and 09 so each model respects these switches. Do not skip the baseline model needed for valid comparisons. Keep the existing clinical output path and behavior when clinical interventions are enabled.

### 2. `04_define_interventions_indonesia.R`

Extend the existing workbook-driven catalogue logic with a separate, explicitly named public-health catalogue. Do not overwrite or overload `fair_inputs`/`fair_scenarios` in a way that breaks the clinical pipeline. Use distinct objects such as `public_health_inputs` and `public_health_scenarios`, while retaining the clinical objects for backward compatibility.

Read the public-health workbook by column names, not fixed cell coordinates, and reproduce its derived values in R rather than trusting cached Excel formula results. The workbook currently contains these relevant sheets:

- `Assumptions`
- `Dictionaries`
- `Intervention_Cause_Map`
- `Policy_Levers`
- `Exposure_Targets`
- `Effect_Parameters`
- `Risk_Response`
- `Model_Input_View`
- `Cost_Components`
- `Countdown_Methods`
- `Scope_and_Sources`
- `QA_Checks`

Absorb the public-health input schema as it exists. Important differences from the clinical workbook include:

- `policy_start_year` and `exposure_target_year`, rather than clinical coverage-year fields;
- `Effect_Parameters`, not `Effect_Sizes`;
- exposure trajectories in `Exposure_Targets`, not clinical treatment coverage trajectories;
- `population` as a public-health cost population-in-need measure; normalize this transparently to the model's all-population quantity where needed;
- public-health cause IDs such as `C_T2DM` and `C_CMYO`; map them explicitly to the Model 00 cause codes `dm2` and `cmd`, while preserving the existing mappings for `C_IHD`, `C_IS`, `C_ICH`, `C_HHD`, etc.;
- shared policy costs identified by `C_SHARED` and `shared-count-once`.

Only include links with `include_flag == 1`. Build baseline, one scenario per runnable public-health intervention, and a combined public-health scenario when at least two interventions are runnable. Use collision-safe scenario IDs, especially when clinical and public-health analyses are both enabled. Preserve a single baseline comparator and label every scenario with an intervention family/type.

Validate and retain an audit table for:

- duplicate or missing keys;
- missing or unsupported cause mappings;
- missing exposure rows or effect rows;
- invalid baseline/target exposure or dates;
- unsupported `reduction_method`, `effect_model`, or `lag_model`;
- transition mappings other than the intended `well -> sick` incidence transition;
- missing response parameters, required PAFs, or policy scores;
- ambiguous cost selections, bad cost joins, invalid PIN measures/fractions, or missing unit costs;
- price-year and Indonesia-adjustment review flags.

Respect `strict_model_input_validation`: strict mode stops on FAIL issues; non-strict mode excludes only invalid links/scenarios and reports them. Do not silently convert REVIEW items into missing results. In particular, the attached public-health costs are review/proxy estimates; keep them in the analysis when numerically usable and clearly flag their review/adjustment status.

Implement the public-health effect calculations from the workbook fields and methods, not the clinical FAIR coverage-adjustment formula. Support the effect models present in the attached workbook:

1. `direct_smoking_prevalence_shift_rr`

   For baseline prevalence `p0`, exposure at time `pt`, and cause-specific smoking RR `RR`, calculate the proportional incidence reduction as:

   ```text
   1 - (1 + pt * (RR - 1)) / (1 + p0 * (RR - 1))
   ```

2. `direct_loglinear_rr_per_unit_reduction`

   For absolute exposure reduction `delta` and `RR` per exposure unit, calculate:

   ```text
   1 - 1 / (RR ^ delta)
   ```

3. `tfa_attributable_ihd_PAF_x_policy_score`

   Calculate:

   ```text
   PAF * achieved_policy_score
   ```

Build the exposure path from `baseline_exposure`, `target_exposure`, `start_year`, `target_year`, and `scale_up_shape`. Apply `exposure_floor`, `reduction_method`, and any `desired_reduction_override`. Apply the workbook's `lag_model` and `lag_parameter`, including `delayed_exponential_remaining_effect` and `immediate_after_full_implementation`, consistently with `Countdown_Methods` and the workbook formulas. Do not invent a coverage parameter for population-wide policies.

Export to `public_health_inputs` all fields needed by Models 06 and 09: raw and normalized catalogue tables, validated effect rows, exposure paths/parameters, cost rows, assumptions, mappings, selected/runnable/blocked interventions, diagnostics, and source/review fields.

### 3. `06_run_scenarios_indonesia_fair.R`

Add the minimum new public-health calculation path needed to consume `public_health_scenarios` from Model 04.

For every public-health intervention–cause row and model year:

1. calculate the achieved exposure at that year from the input-defined trajectory;
2. calculate the full effect using its declared `effect_model`;
3. apply the declared lag model;
4. constrain the result to a valid proportional reduction;
5. apply it to the mapped `well -> sick` transition by multiplying the cause-specific incidence rate by the surviving fraction `1 - effect_t`;
6. combine multiple policies acting on the same cause/year multiplicatively on the surviving fraction, so results are order-independent and effects are not double-counted.

Do not apply the clinical `apply_coverage_adjustment()` formula to public-health exposure effects. Do not apply the public-health effects to case fatality unless the input contract explicitly contains and validates such a transition. The current workbook maps public-health effects to incidence.

Honor the two Model 00 switches when constructing and running scenarios. When both families are enabled, run both without losing either catalogue, without duplicate baseline rows, and without ambiguous scenario names. Add an `intervention_family` or equivalent trace field (`clinical`, `public_health`, `combined`, `baseline`) to outputs if needed for unambiguous downstream selection.

Remove or bypass hard-coded public-health parameters in Model 06 for the workbook-driven public-health branch. Values such as sodium reduction, TFA target, policy start/target years, tobacco/alcohol/SSB response parameters, and costs must come from the public-health workbook. Do not delete legacy functions that are still used elsewhere.

Preserve the existing Model 06 state/flow output contract required by Model 09, including scenario, year, age, sex, cause, well, sick, newcases, dead, pop, all.mx, location, eff_ir, and eff_cf. Extend it only when necessary and keep existing consumers working.

### 4. `09_cost_value.R`

Preserve the existing clinical formula-workbook output when clinical interventions are enabled. Add a separate public-health reporting path that consumes only the current-run public-health catalogue and its Model 06 scenarios and writes exactly:

`output/indonesia_cost_value_public_health_formulae.xlsx`

Do not merely rename or copy the clinical workbook. Reuse its formatting and audit pattern, while adapting the sheets and formulas to public-health exposure-based effects and policy costs.

The public-health workbook should contain, at minimum, these ordered sheets (minor naming changes are acceptable only if clearer):

1. `README`
2. `Run_Metadata`
3. `Selected_Interventions`
4. `Blocked_Links`
5. `Policy_Levers`
6. `Exposure_Targets`
7. `Effect_Parameters`
8. `Cost_Components`
9. `Annual_Health_Effects` or `Annual_Mortality`
10. `Annual_Cost`
11. `Budget_Impact`
12. `Cost_Effectiveness`
13. `Economic_Value` when current Model 08 results reconcile; otherwise include a clear explanatory note
14. `QA_Checks`
15. `Input_Diagnostic`
16. `Methods_and_Sources`
17. `Calculation_Assumptions`
18. `Calculation_Map`

Include the detailed model trace/background mortality sheets only if needed for auditability or already produced by the existing R-value workbook; do not duplicate huge tables without purpose.

The workbook must be formula-driven and auditable:

- R-source values may provide the model state/flow quantities and scenario mortality/cases.
- All feasible derived workbook outputs must use live Excel formulas, including exposure reductions, annual/cumulative deaths and cases averted, policy implementation fractions, PIN, annual costs, incremental costs, discount factors, budget impact, cumulative budget impact, cost per death averted, dominance labels, and reconciliation checks.
- Formula cells must link visibly across sheets; do not hard-code results that can be calculated from displayed source cells.
- Keep editable assumptions in `Calculation_Assumptions` and clearly distinguish them from R-source values and formulas.
- Set Excel to recalculate formulas on open.
- Quote worksheet names correctly in cross-sheet formulas.
- Prevent shared policy costs from being repeated once per cause: `shared-count-once` costs must be counted once per intervention/scenario/year.
- For public-health `population` PIN, use the deduplicated population once per age/sex/year, never summed over causes.
- Use undiscounted incremental cost as the budget-impact headline and discounted incremental cost for cost per death averted, consistent with the existing clinical workbook unless the input contract explicitly says otherwise.
- Do not introduce DALYs at this stage. The principal CEA result remains USD per death averted.
- Retain source, review status, price year, adjustment flag, and notes so provisional/proxy public-health costs remain transparent.

Match the attached clinical formula workbook's professional styling:

- dark-blue title/header style with white bold text;
- pale yellow for user-editable cells;
- light blue for formula-derived cells;
- light grey for R-source/locked source values;
- green/amber/red conditional formatting for PASS/REVIEW/FAIL;
- sensible currency, count, decimal, and percentage formats;
- frozen headers, filters, readable column widths, wrapped long notes, hidden gridlines where appropriate, and logical sheet ordering;
- clear README legend explaining the color convention and calculation flow.

Do not add macros, external workbook links, broken named ranges, dangling drawing relationships, or unsupported Excel features. The `.xlsx` must open cleanly in Microsoft Excel and LibreOffice.

## Formula and QA requirements

At minimum, add checks for:

- every selected intervention–cause key is unique;
- each included map row has exactly one exposure/effect match;
- exposure targets reconcile to their reduction method and floor;
- every effect model and lag model is supported;
- each modeled cause maps to a Model 00 cause code;
- public-health transitions are `well -> sick` / incidence;
- every scenario has a matched baseline by year and cause;
- no negative or impossible model states;
- population is not duplicated across causes for cost calculations;
- component costs reconcile to annual costs and budget impact;
- shared costs are counted once;
- annual results reconcile to cumulative results;
- deaths averted equal baseline deaths minus scenario deaths;
- cost-effectiveness totals reconcile to detail;
- selected anchor results in Excel reconcile to the R calculations within a documented tolerance;
- no formulas contain `#REF!`, `#DIV/0!`, `#VALUE!`, `#NAME?`, or unintended `#N/A`.

## Testing and acceptance criteria

Run the smallest complete Indonesia test that proves all three switch combinations that contain at least one enabled family:

1. public health only;
2. clinical only;
3. public health and clinical together.

Do not run expensive calibration unnecessarily; reuse valid current baseline/model artifacts when safe. If a full run is impossible because required raw data are unavailable, test the new catalogue/effect/workbook code with the available current model output and clearly identify the unexecuted dependency. Do not claim a successful end-to-end run if it was not performed.

For the public-health-only test, verify that:

- the selected interventions come from the public-health workbook rather than hard-coded vectors;
- tobacco, alcohol, sodium, TFA, and SSB use their input-defined parameters where selected and valid;
- diabetes mellitus type 2 and all included CVD causes map correctly;
- the output workbook is created at the exact requested path;
- every required sheet exists and contains data or an explicit explanatory note;
- formulas are present in the calculation sheets and point to the intended precedent sheets;
- shared policy costs are not multiplied by the number of causes;
- workbook formulas recalculate and key totals reconcile to R;
- the file opens successfully with a strict reader and LibreOffice/Excel-compatible validation.

Inspect the final workbook visually (at least representative top ranges from every sheet) and correct clipped headers, unreadable widths, broken filters/freeze panes, inconsistent formats, or formula errors.

## Required final response

When finished, report:

1. the exact files changed—there should be only the four authorized R scripts;
2. a concise summary of the new switch behavior and public-health data flow;
3. the exact output workbook path;
4. tests run and their results, including formula/error/reconciliation checks;
5. any unresolved input REVIEW/FAIL items or assumptions that prevented a complete run;
6. a concise `git diff --stat`.

Do not commit or push unless explicitly asked.
