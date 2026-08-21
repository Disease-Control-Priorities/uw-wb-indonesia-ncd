# Claude Code prompt: update the public-health model pipeline

Update the following FAIR Choices Indonesia model scripts so they fully support the revised public-health input structure and carry that structure through to the **formula-driven** output Excel workbook `indonesia_cost_value_public_health_formulae.xlsx`:

- `00_run_model_cvd_fair.R`
- `04_define_interventions_indonesia.R`
- `06_run_scenarios_indonesia_fair.R`
- `09_cost_value.R`

The supplied files may have download suffixes such as `(6)`, `(3)`, `(5)`, or `(2)`. Resolve them to their corresponding repository filenames before editing.

Use `data/indonesia_model_inputs_public_health.xlsx` as the authoritative public-health contract (the updated workbook; if your copy is named `indonesia_model_inputs_public_health_updated.xlsx`, treat that as the authoritative version and keep a fallback to the non-suffixed name only if the updated file is absent). Inspect its actual sheet and column names before coding. Do not rely on sheet positions, row numbers, cached Excel formula results, or hard-coded intervention lists.

## Scope discipline (read first)

- **Modify only the four named R scripts.** Do not edit, reformat, or "clean up" any other script, module, data file, or the input workbook.
- **Change the minimum number of lines needed.** Do not reflow, re-indent, rename, or restyle lines you are not functionally changing. No cosmetic diffs. No touching unrelated functions, comments, or clinical code paths.
- Preserve existing clinical behavior and every unrelated function exactly as-is.
- Prefer small helper functions and key-based joins over duplicated special-case code, but add them only where the new logic requires it.
- Do not hard-code row positions or Excel column letters when reading inputs; select columns by name. Where the formulae workbook must reference column letters, derive them from column names (e.g. `match()`) rather than hard-coding, so newly inserted columns do not silently misalign.
- Do not trust cached Excel formulas; reproduce calculations in R and use cached values only as QA comparators.
- Do not silently suppress validation failures.
- Fix directly related duplicate blocks or broken references only when they are inside the public-health code you are already changing.

## What changed in the input workbook (updated vs. previous)

Verify these against the live workbook before coding — the following is the observed diff and should match:

1. **NEW SHEET: `Scenario_Hierarchy`** (13 rows). Columns: `parent_scenario_id`, `parent_scenario_name`, `intervention_id`, `intervention_name`, `package_group`, `scenario_role`, `include_in_parent_scenario`, `standalone_scenario_id`, `parent_aggregation_rule`, `outcome_reporting_rule`, `cost_reporting_rule`, `component_order`, `source_note`. Defines how individual interventions roll up into parent (package) scenarios, whether each is included in its parent, how outcomes and costs are aggregated, and component ordering.

2. **`Policy_Levers`** — 13 NEW columns appended after `qa_status`, header otherwise unchanged: `parent_package_id`, `parent_package_name`, `intervention_role`, `fiscal_baseline_tax_level`, `fiscal_target_tax_level`, `fiscal_tax_level_unit`, `regulatory_baseline_level`, `regulatory_target_level`, `regulatory_baseline_score`, `regulatory_target_score`, `implementation_gap`, `implied_price_change`, `fiscal_tax_delta`.

3. **`Cost_Components`** — 5 NEW columns appended after `qa_status`, plus more rows (8 -> 15): `parent_package_id`, `cost_allocation_share`, `package_total_cost_usd_per_capita`, `allocation_method`, `scenario_role`.

4. **Row-count growth only (headers unchanged)** as interventions were expanded to individual level: `Intervention_Cause_Map` 29 -> 49, `Effect_Parameters` 29 -> 49, `Model_Input_View` 29 -> 49, `Dictionaries` 10 -> 13, `Assumptions` 23 -> 28, `Exposure_Targets` 8 -> 13, `Risk_Response` 27 -> 29.

Because several sheets grew in row count and three grew in column count, any code that reads fixed ranges, fixed row counts, or fixed column indices/letters against these sheets must be updated to be name- and length-driven.

## Required input structure

The updated workbook contains these sheets:

- `README`
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
- `Scenario_Hierarchy`

The intended executable public-health interventions are:

- `I_PH_TOBACCO_TAX`
- `I_PH_ALCOHOL_TAX`
- `I_PH_TOB_CLEAN_AIR`
- `I_PH_TOB_MEDIA`
- `I_PH_TOB_AD_BAN`
- `I_PH_ALCOHOL_POLICY`
- `I_PH_SALT_REFORM`
- `I_PH_SALT_FOPL`
- `I_PH_SALT_MEDIA`
- `I_PH_SALT_ENV`
- `I_PH_TFA_POLICY`
- `I_PH_SSB_TAX`

Each must be modeled and costed as an individual scalable intervention, with its own cases averted, deaths averted, costs, and cost-effectiveness results. Do not silently collapse them into the former package-level identifiers.

## Scenario hierarchy

Preserve the nested package structure using `Scenario_Hierarchy`.

- `I_PH_TOBACCO_POLICY` contains:
  - `I_PH_TOB_CLEAN_AIR`
  - `I_PH_TOB_MEDIA`
  - `I_PH_TOB_AD_BAN`
- `I_PH_SALT_POLICY` contains:
  - `I_PH_SALT_REFORM`
  - `I_PH_SALT_FOPL`
  - `I_PH_SALT_MEDIA`
  - `I_PH_SALT_ENV`

Build these relationships from the workbook (`Scenario_Hierarchy`, respecting `include_in_parent_scenario`, `parent_aggregation_rule`, `outcome_reporting_rule`, `component_order`) rather than hard-coding them where possible. The scenario catalogue must contain:

1. The baseline.
2. One standalone scenario for every valid executable child intervention (use `standalone_scenario_id`).
3. One joint scenario for each parent package.
4. Any additional combined scenario, such as `all_public_health`, only when defined or supported by the workbook contract.

A parent-package health result must come from one joint model run in which all its children are applied together. Never calculate package cases or deaths averted by summing standalone child results because effects can overlap and combine non-additively. Package cost may be calculated as the sum of its selected child costs.

Carry these trace fields through scenario construction, model output, and reporting where applicable:

- `scenario_id`
- `scenario_label`
- `scenario_level` or `scenario_role`
- `parent_package_id`
- `parent_package_name`
- `intervention_id`
- `intervention_ids`
- `intervention_family`
- hierarchy membership and order

## 1. TFA modeling

Change industrial trans-fatty-acid modeling so RR is the default method and PAF is optional.

The workbook has already been updated to use **RR = 1.10 per 1 percentage-point increase in energy from trans fat**. Treat this workbook value as authoritative. Do **not** recalculate it as `sqrt(1.21)` or overwrite it with 1.21. The supporting evidence reports approximately RR 1.21 per 2 percentage-point increase, which is being represented consistently in the input workbook as RR 1.10 per 1 percentage point.

For a reduction in TFA exposure measured in percentage points of energy, use:

```r
rr_per_1pct_energy <- 1.10
absolute_reduction <- baseline_exposure - achieved_exposure
incidence_reduction <- 1 - 1 / (rr_per_1pct_energy ^ absolute_reduction)
```

Read the RR from `Risk_Response` (for example, `RR_TFA_IHD_1PCT`) rather than hard-coding 1.10 in the production calculation. The code above documents the expected interpretation and provides a test value.

Use workbook parameters such as:

- `Assumptions$tfa_effect_method`
- `RR_TFA_IHD_1PCT`
- `PAF_TFA_IHD_OPTIONAL`

Expected behavior:

- `tfa_effect_method = "RR"` uses `direct_loglinear_rr_per_unit_reduction`.
- RR mode must not require a PAF and must not block `I_PH_TFA_POLICY`.
- `tfa_effect_method = "PAF"` may use `tfa_attributable_ihd_PAF_x_regulatory_gap` or the workbook's exact optional PAF model name.
- A missing PAF is relevant only when PAF mode is explicitly selected.
- Do not convert a missing PAF to a zero effect in RR mode.
- Validate that TFA exposure units are percentage points of energy before applying the RR formula.
- Apply TFA only to the cause mappings and age/sex ranges declared in the workbook.

Retain these references in the methods/source output:

- https://www.ahajournals.org/doi/10.1161/CIRCULATIONAHA.118.038160
- https://www.ahajournals.org/doi/10.1161/JAHA.115.002891

Do not route `I_PH_TFA_POLICY` through the legacy hard-coded TFA pathway when it is supplied through `ph_effect_rows`.

## 2. Fiscal-policy modeling

Tax interventions must use the change from baseline tax level to target tax level, price elasticity, and the resulting exposure change.

Read and preserve fields such as:

- `fiscal_baseline_tax_level`
- `fiscal_target_tax_level`
- `fiscal_tax_level_unit`
- `fiscal_tax_delta`
- `implied_price_change`
- price elasticity
- baseline and target exposure

Reproduce the workbook calculations in R and compare them with workbook-derived values for QA.

For a tobacco excise share expressed as a fraction of retail price, use the appropriate tax-share-to-price transformation:

```r
implied_price_change <-
  (1 - fiscal_baseline_tax_level) /
  (1 - fiscal_target_tax_level) - 1
```

For tax instruments represented as a direct proportional price change or tax-rate increment, use the workbook's declared method, generally:

```r
fiscal_tax_delta <- max(
  0,
  fiscal_target_tax_level - fiscal_baseline_tax_level
)
```

Then derive the consumption/exposure response using the declared elasticity:

```r
proportional_exposure_reduction <-
  abs(price_elasticity) * implied_price_change
```

Apply bounds, units, and exposure floors from the workbook. Do not model the target tax rate as though it were the incremental policy effect.

## 3. Regulatory-policy modeling

Regulatory effects must depend on movement between baseline and target implementation categories. Support at minimum:

- `none = 0`
- `partial = 0.5`
- `full = 1`

Calculate:

```r
implementation_gap <- max(
  0,
  regulatory_target_score - regulatory_baseline_score
)
```

Therefore:

- none to full applies the full effect;
- partial to full applies half the full effect;
- full to full produces no additional effect.

Read and retain separate fields for:

- `regulatory_baseline_level`
- `regulatory_target_level`
- `regulatory_baseline_score`
- `regulatory_target_score`
- `implementation_gap`

Scale the relevant full exposure or policy effect by `implementation_gap`. A valid full-to-full scenario should run and report zero incremental effect; it should not be blocked. If it has no incremental cases or deaths averted, report its cost-effectiveness ratio as `NA` or blank with a clear `no incremental health effect` status—not zero, infinity, or a divide-by-zero error.

## Changes by file

### `00_run_model_cvd_fair.R`

- Keep this file limited to paths, execution switches, and orchestration.
- Point `public_health_inputs_file` (around line ~113) to the updated workbook, preserving the existing `.ph_alt` fallback pattern so the pipeline still runs if only the previous filename is present.
- Retain independent clinical and public-health execution switches.
- Retain separate clinical and public-health output workbook paths, including `public_health_cost_value_formulae_file` -> `indonesia_cost_value_public_health_formulae.xlsx`.
- Do not duplicate analytic values, effects, tax levels, regulatory states, costs, or hierarchy definitions here.
- Source Models 04, 06, and 09 in the required order.
- Remove or correct redundant path fallback logic only if it is genuinely broken; otherwise leave path logic untouched.

### `04_define_interventions_indonesia.R`

Update `.build_public_health_catalogue()` and the public-health loader (`req_sheets` around line ~1935 and the `rd()` reader around ~1944) so it:

- Requires and reads `Scenario_Hierarchy` (add it to `req_sheets` and load via `rd()`).
- Reads all new fiscal, regulatory, hierarchy, and cost-allocation fields by name (the 13 new `Policy_Levers` columns and the 5 new `Cost_Components` columns) without dropping them and without hard-coding positions.
- Reproduces tax, regulatory, exposure-target, and effect calculations in R.
- Treats RR as the default TFA method and PAF as optional.
- Builds one executable scenario per child intervention.
- Builds parent packages as joint scenarios containing their child interventions.
- Preserves standalone interventions.
- Exports hierarchy and trace information in `public_health_inputs` (including `parent_package_id`, `parent_package_name`, `intervention_role`).
- Retains `intervention_cause_key`, `effect_key` or `response_key`, `exposure_key`, and `cost_join_key` traceability.
- Ensures every modeled public-health transition is `well -> sick` incidence.
- Does not block health modeling solely because a selected cost requires localization; flag this as `REVIEW`.
- Fails with one consolidated diagnostic for genuinely invalid selected rows.
- Does not silently fall back to the legacy aggregated workbook structure.

Update the supported effect models to include the new optional TFA PAF name if the workbook uses it.

Ensure engine effect rows contain enough information to reproduce:

- fiscal change;
- regulatory implementation gap;
- baseline and target exposure;
- achieved exposure path;
- TFA method;
- RR or optional PAF;
- hierarchy membership;
- intervention and cause keys.

### `06_run_scenarios_indonesia_fair.R`

Update `calculate_public_health_workbook_impact()`, `run_multiple_scenarios()`, and scenario assembly so:

- Every child intervention is run independently.
- Every parent package is run jointly using all its child `ph_effect_rows`.
- Multiple effects on the same cause combine multiplicatively on the surviving incidence fraction.
- Package outcomes are never formed by summing child outcomes.
- Tax and regulatory changes are reflected in the achieved exposure/effect path.
- TFA RR mode uses the log-linear RR formula and does not require PAF.
- PAF mode remains available only when selected.
- Scenario hierarchy and intervention identifiers are attached to model result rows.
- Baseline is included exactly once.
- Existing clinical scenarios and the legacy clinical model remain functional and untouched.
- Public-health workbook scenarios do not use hard-coded `salteff`, TFA targets, or other legacy defaults.
- The result remains compatible with Models 08 and 09 and includes at least `scenario`, `year`, `age`, `sex`, `cause`, `well`, `sick`, `newcases`, `dead`, `pop`, `all.mx`, `location`, and the new scenario trace fields.

### `09_cost_value.R`

Update `source_public_health_cost_value()` (the function beginning around line ~1372, which builds the workbook via `createWorkbook()` around line ~1583) and the public-health Excel output so the new structure is visible from inputs through results and cost-effectiveness.

**The public-health output is the formula edition `indonesia_cost_value_public_health_formulae.xlsx`.** It must remain formula-driven, not a values snapshot:

- Write live Excel formulas for derived exposure, effect, cost, cases/deaths averted, cumulative totals, and cost-effectiveness fields. Do not replace derived cells with pasted constants.
- Use source/input values or R model results as formula anchors.
- Reflect all new input structure — the scenario hierarchy, the new fiscal and regulatory `Policy_Levers` fields, and the new `Cost_Components` allocation fields — as live cells/columns and as inputs to the derived formulas, not as static text.
- When the new `Cost_Components` columns shift positions of existing columns, update every dependent structural reference — row counts such as `r_cc`/`n_cc`, the `frows(...)` helpers, and the INDEX/MATCH `writeFormula` column-letter targets (e.g. the coverage pulls around lines ~984-992) — so each formula still points at the correct column after the shift. Derive letters from column names, do not hard-code.
- Avoid volatile formulas. Set full recalculation on workbook open.
- Preserve formatting, filters, frozen panes, readable widths, formula/source color conventions, and conditional formatting.
- Ensure formulas contain no `#REF!`, `#DIV/0!`, broken ranges, or references to removed sheets.
- Update `Methods_and_Sources` and `Calculation_Map` to describe RR-based TFA, fiscal-delta, regulatory-gap, hierarchy, and package calculations.

The output workbook must provide, at minimum:

- run metadata;
- scenario hierarchy;
- validated interventions;
- intervention-cause mapping;
- policy levers (including the new fiscal and regulatory fields);
- exposure targets;
- effect parameters;
- risk-response parameters;
- cost components (including the new allocation fields);
- model/input trace;
- annual health outcomes;
- child-intervention summary;
- parent-package summary;
- annual cost;
- budget impact;
- cost-effectiveness;
- methods and sources;
- input diagnostics;
- QA checks.

Preserve existing sheet names where practical, but add the hierarchy and summary sheets or fields needed to make child and parent results explicit.

For every child intervention and parent package, report:

- modeled cases;
- baseline cases;
- cases averted;
- modeled deaths;
- baseline deaths;
- deaths averted;
- annual and cumulative cost;
- discounted incremental cost;
- cost per case averted;
- cost per death averted;
- dominance or status;
- parent/child hierarchy fields.

Cost child interventions using their child-specific `cost_join_key`. Do not assign the complete tobacco or salt package cost independently to every child.

Use the workbook's allocation fields, including where present:

- `parent_package_id`
- `cost_allocation_share`
- `package_total_cost_usd_per_capita`
- `allocation_method`
- `scenario_role`
- `selected_for_base_case`

Parent reference cost rows with `selected_for_base_case = 0` must not be charged in addition to child rows. Check that child allocation shares sum to one within each package and that package cost equals the sum of selected child costs.

## Validation and acceptance criteria

Add R-side and output-workbook QA checks confirming:

1. There are 12 executable individual public-health interventions when all expected rows are selected and valid.
2. The selected `Intervention_Cause_Map` contains the expected 48 unique `intervention_cause_key` rows.
3. No selected intervention-cause key is duplicated.
4. Every executable child has exactly one valid exposure row and its required effect rows.
5. Every selected child has a valid child-specific cost join.
6. The tobacco hierarchy has three children.
7. The salt hierarchy has four children.
8. Cost-allocation shares sum to one within each allocated package.
9. Parent cost equals the sum of child costs.
10. Parent health outcomes come from an actual produced joint scenario.
11. TFA RR mode runs with RR 1.10 per 1 percentage-point energy and without a PAF.
12. Optional TFA PAF mode works when explicitly selected and parameterized.
13. Tax effects respond to baseline-to-target tax change, not the target level alone.
14. Regulatory none-to-full, partial-to-full, and full-to-full produce implementation gaps of 1, 0.5, and 0.
15. Every produced scenario is paired with the baseline by year and cause.
16. Cases and deaths averted equal baseline minus scenario.
17. Model states and flows are nonnegative within numerical tolerance.
18. Excel summary formulas reconcile with the R results.
19. No parent-package outcomes are calculated by summing standalone child outcomes.
20. Clinical execution and clinical output remain unchanged.

The Indonesia-adjustment and price-year status of source costs may remain a `REVIEW` warning and must not prevent health modeling. Do not invent new baseline tax or implementation values. If genuinely missing parameterization is required, use recent WHO sources first, document the source and access date, and distinguish sourced inputs from assumptions.

## Implementation discipline

- Modify only the four named R scripts unless a test fixture is strictly necessary.
- Change the minimum lines required; no cosmetic or unrelated edits.
- Preserve existing clinical behavior and unrelated functions.
- Prefer small helper functions and key-based joins over duplicated special-case code.
- Do not hard-code row positions or Excel column letters when reading inputs.
- Do not trust cached Excel formulas; reproduce calculations in R and use cached values only as QA comparators.
- Do not silently suppress validation failures.
- Fix directly related duplicate blocks or broken references only within the public-health code you are already touching.
- Keep formulas and methods consistent between R and the generated Excel workbook.

## Verification

After implementation:

1. Parse all four scripts with `Rscript`.
2. Run targeted tests for the TFA RR/PAF switch, fiscal changes, regulatory gaps, hierarchy construction, and cost allocation.
3. Run the full public-health pipeline if the required model data are available.
4. Open or programmatically inspect the generated `indonesia_cost_value_public_health_formulae.xlsx`.
5. Confirm all expected sheets and scenario rows exist.
6. Confirm derived output cells are Excel formulas (not pasted constants) and that the new input fields are represented.
7. Confirm formula references are valid (no `#REF!`, `#DIV/0!`, broken ranges).
8. Reconcile child and parent summaries to detailed annual results.
9. Report any test that could not be run and the exact missing dependency.

At completion, provide:

- a concise summary of changes in each file;
- the final public-health scenario list;
- the TFA method and RR used in the test run;
- validation and reconciliation results;
- the generated output workbook path;
- any remaining `REVIEW` warnings;
- a focused diff of the four modified scripts.
