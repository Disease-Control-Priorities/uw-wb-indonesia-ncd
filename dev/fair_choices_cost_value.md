# Claude Code prompt: connect the FAIR Choices input workbook to Models 04/06 and add costing, budget impact, and cost-effectiveness output

Work in this repository:

`C:\Users\wrgar\OneDrive - UW\02Work\WorldBank-Indonesia\uw-wb-indonesia-ncd`

The relevant code directory is:

`code\cvd-fair-choices`

## Objective

Implement a **small, targeted extension** of the current Indonesia CVD FAIR Choices pipeline. Do not conduct a general refactor.

1. Update `04_define_interventions_indonesia.R` so the interventions, intervention–cause mappings, coverage trajectories, transition targets, and effect sizes used by the model come from the user's selections in `indonesia_model_inputs.xlsx`.
2. Update `06_run_scenarios_indonesia_fair.R` so it runs the scenarios declared by Model 04 and applies the correct annual FAIR Choices effects to the correct transitions.
3. Add `09_cost_value.R` to transform the existing model outputs into a transparent state/flow trace and produce one user-friendly workbook named `indonesia_model_cost_value.xlsx` containing costing, budget-impact, mortality-based cost-effectiveness, and QA results.
4. Make only the minimal changes needed in `00_run_model_cvd_fair.R` to hold execution-level metaparameters as the single source of truth and to source the new script.

Preserve the current baseline/calibration/demographic architecture and the existing contracts used by `07_output_dalys.R`, `08_economic_value_calculation.R`, and `09_validation_indonesia.R`.

## Context and short-term design decision

This is the pragmatic, short-term refactor discussed in the August 14 meeting, not the broader future architecture overhaul. The current input workbook is the authoritative contract for intervention–cause selection and economic inputs.

The current workbook is expected to contain these sheets:

- `README`
- `Assumptions`
- `Dictionaries`
- `Intervention_Cause_Map`
- `Effect_Sizes`
- `Coverage`
- `Cost_Components`
- `Model_Input_View`
- `FairChoices_Methods`
- `Scope_and_Sources`
- `QA_Checks`

Locate the actual workbook in the repository before editing code. Use the exact `.xlsx` file; do not create or refer to a separate `.xls` version. If the workbook is absent or more than one current copy is ambiguous, stop and report the exact issue rather than guessing.

The current expected footprint is eight unique interventions mapped to 17 intervention–cause rows across RHD, IHD, ischemic stroke, intracerebral hemorrhage, HHD, cardiomyopathy/myocarditis, and type 2 diabetes. Treat this only as a validation cross-check. **Do not hardcode this footprint**: future selections must remain workbook-driven.

### Illustrative output reference — useful but explicitly non-binding

An example workbook, `indonesia_model_cost_value(1).xlsx`, is provided to show the general level of usability, traceability, styling, and economic-analysis flow expected from Model 09. Inspect it if it is available, but treat it as a **working prototype**, not as a mandatory template or production input.

Relevant sheets in the example include:

- `README`
- `Economic_Parameters`
- `Coverage_Inputs`
- `Cost_Parameters`
- `Model_Trace`
- `Annual_Health`
- `Annual_Cost`
- `Budget_Impact`
- `Cost_Effectiveness`
- `QA_Checks`
- `Methods_and_Sources`

Its useful design ideas include:

- a minimum raw trace with `scenario`, `location`, `year`, `age`, `sex`, `cause_code`, `intervention`, `well`, `sick`, `new_cases`, `cause_deaths`, `background_deaths`, and `population`;
- exact baseline/comparison matching by location × year × age × sex × cause;
- separate editable parameters, coverage inputs, and cost-component inputs;
- annual cases, deaths, and component-cost calculation tables feeding budget-impact and cost-effectiveness summaries;
- explicit shared-cost handling, readiness indicators, QA gates, methods, and source documentation;
- the same colors and visual language as `indonesia_model_inputs.xlsx`.

You may simplify, merge, split, rename, reorder, or omit example sheets and formulas when doing so makes the production implementation more robust, smaller, faster, or easier to maintain. For example:

- omit the example's `Health_Weights` sheet and all YLL, YLD, DALY, life-expectancy, disability-weight, and DALY-discounting logic in this stage; those are explicitly deferred to later work;
- generate a long R-calculated annual cost table instead of copying hundreds of fragile Excel formulas;
- use explicit `cost_join_key` de-duplication instead of a hardcoded anchor cause when that is safer;
- support every selected scenario rather than the example's single baseline-versus-comparison setup;
- keep background mortality in a separate de-duplicated table when repeating it by cause would be misleading.

Do not copy the example's sample values, scenario names (`fair_only`/`all_plus_fair`), incomplete trace, placeholder assumptions, or formulas blindly. Do not overwrite `indonesia_model_cost_value(1).xlsx`. The production output remains `indonesia_model_cost_value.xlsx`. If the example workbook is unavailable, continue using the requirements below; its absence is not a blocker.

## First: inspect and establish a baseline

Before changing anything:

1. Run `git status --short` and preserve all unrelated user changes.
2. Read in full:
   - `00_run_model_cvd_fair.R`
   - `04_define_interventions_indonesia.R`
   - `05_build_baseline_indonesia.R`
   - `06_run_scenarios_indonesia_fair.R`
   - `07_output_dalys.R`
   - `08_economic_value_calculation.R`
   - `09_validation_indonesia.R`
   - `indonesia_model_inputs.xlsx`, including formulas, raw input columns, styles, QA rules, and keys
   - `indonesia_model_cost_value(1).xlsx`, if available, as a non-binding output/design example
3. Inspect representative output `.rds` files produced by Model 06 and document their actual grain and column semantics. In particular, determine exactly whether `dead` means incident cause-specific deaths, cumulative deaths, or another flow, and identify the actual background-mortality output. Do not infer these semantics from column names alone.
4. Run the current baseline and one currently working scenario, if the available data and runtime permit. Record baseline totals and output schemas so the targeted changes can be regression-tested.
5. Search for all downstream references to objects created in Models 04 and 06. Preserve those interfaces unless an additive change is necessary.

Do not install packages or edit data values merely to make a test pass. If required data or packages are unavailable, complete all safe static and targeted tests and report the exact blocker.

## Scope and change boundaries

Allowed changes:

- A small execution-configuration block and one new `source()` call in `00_run_model_cvd_fair.R`.
- Targeted changes to the FAIR Choices intervention/scenario block in `04_define_interventions_indonesia.R`.
- Targeted changes to scenario iteration, effect application, and additive output columns in `06_run_scenarios_indonesia_fair.R`.
- One new file: `09_cost_value.R`.
- Tests or a compact validation harness if the repository already has an appropriate location.

Do not:

- redesign Models 00–06;
- modify calibration, demographic projection, fertility, migration, unrelated interventions, or cause-loading logic;
- rename existing output columns or scenario fields consumed by Models 07 or 08;
- rewrite unrelated functions, comments, formatting, or code style;
- modify `07_output_dalys.R`, `08_economic_value_calculation.R`, or `09_validation_indonesia.R` unless a genuinely unavoidable compatibility defect is demonstrated first;
- add many new `.rds`, `.csv`, or temporary files;
- fabricate missing coverage, effect, cost, or transition inputs;
- implement deferred items such as detailed BP/statin risk distributions, CKD, peripheral vascular disease, rehabilitation, acute coronary syndrome treatment, or the web/Shiny tool.

Keep the diff narrow and reviewable.

## Source-of-truth rules

Use two clearly separated sources of truth:

- `00_run_model_cvd_fair.R`: execution-level metaparameters only, such as the input-workbook path, output-workbook path, run flag, baseline scenario identifier, and strict-validation flag.
- `indonesia_model_inputs.xlsx`: user-editable analytic inputs, including selected intervention–cause links, effects, affected fractions, coverage, cost components, the cost discount rate, analysis years, price year, and economic perspective.

Do not duplicate analytic assumptions in Model 00. Define each path/run flag once and reuse it downstream. Suggested names may be adapted to the repository's conventions, but the concepts should include:

- `model_inputs_file`
- `cost_value_output_file`
- `run_cost_value`
- `baseline_scenario_id`
- `strict_model_input_validation`

Add `source("09_cost_value.R")` after Models 07 and 08 and before the existing `source("09_validation_indonesia.R")`. Do not rename or remove the existing validation script merely because both files begin with `09`.

## Task 1A: make Model 04 workbook-driven

Read the required workbook sheets explicitly and validate their schemas. Do not rely on Excel's cached formula results. Read the raw input columns and reproduce essential derived fields in R from `Assumptions`, `Intervention_Cause_Map`, `Effect_Sizes`, `Coverage`, and `Cost_Components`.

Use the workbook keys exactly:

- `intervention_cause_key`: unique intervention × cause link and primary health-effect join key;
- `effect_key` and `coverage_key`: must resolve one-to-one to that link;
- `cost_join_key`: economic join key;
- `cost_scope`: either cause-specific or shared-count-once;
- `include_flag`: determines which intervention–cause links are selected;
- `selected_for_base_case`: selects one cost option within each `cost_component_key`.

Build a single validated intervention catalogue and scenario catalogue in Model 04 that match the existing object interfaces expected by Model 06. Scenario membership must be derived from selected workbook rows:

- always retain the existing baseline scenario;
- create/run the existing individual scenario for each distinct selected `intervention_id` using all of its selected cause links;
- preserve an existing combined scenario only if the current Model 04/06 already supports it, but derive its membership from the selected intervention IDs;
- do not invent new policy combinations that are not present in the current scenario design.

Preserve legacy scenario labels or aliases required by Models 07 and 08. If an internal stable `scenario_id` is needed, add it without breaking the existing `scenario`, `intervention`, or `htn_target_scenario` fields.

### Required validation in Model 04

Fail early with one consolidated, readable diagnostic table if any selected health scenario has:

- a duplicate or missing `intervention_cause_key`;
- zero or multiple effect/coverage matches;
- missing or invalid `effect_value`, `affected_fraction`, or baseline/target coverage;
- effect, affected fraction, or coverage outside `[0,1]`;
- target coverage below baseline coverage;
- an invalid start/target year;
- a transition label that cannot be mapped to the existing Markov model;
- an included cause absent from the Model 00 `cause_map`.

For costing, also validate:

- exactly one selected row per selected `cost_component_key`;
- nonmissing, nonnegative `unit_cost_usd`, frequency, and population-in-need fraction;
- a valid `cost_join_key`;
- shared cost rows have one unambiguous coverage trajectory;
- costs marked as not Indonesia-adjusted or in the wrong price year are explicitly flagged and are not silently treated as ready.

Known review items in the supplied workbook included missing type 2 diabetes baseline coverage, an unadjusted type 2 diabetes unit cost, a missing IHD chronic-HF follow-up cost, and a transition review for RHD surgery. Reinspect the current workbook because the user may have corrected them. Never fill unresolved values by assumption.

## Task 1B: implement the FAIR Choices effect correctly in Models 04/06

Use the workbook's `FairChoices_Methods` sheet as the local methodological record and cross-check the implementation against the official FAIR Choices methods page: `https://fairchoices.w.uib.no/documentation/fairchoices-methods/`.

For each selected intervention–cause link, use the FAIR Choices coverage adjustment:

`delta_coverage(t) = coverage(t) - baseline_coverage`

`adjusted_effect(t) = effect_value * delta_coverage(t) / (1 - effect_value * baseline_coverage)`

`transition_effect(t) = adjusted_effect(t) * affected_fraction`

For linear scale-up:

- before or at the intervention start, use baseline coverage;
- interpolate linearly from baseline coverage to target coverage between start year and target year;
- use target coverage from the target year onward;
- constrain and validate coverage within `[0,1]`.

Apply the resulting effect to the explicitly mapped transition:

`p_scenario(t) = p_baseline(t) * (1 - transition_effect(t))`

The workbook's current transition logic is:

- prevention: `well -> sick` (incidence);
- disease management: `sick -> cause-specific death` (case fatality);
- heart-failure/severe-disease labels: preserve their workbook meaning and map them to the current model only through an explicit translation table in Model 04.

Do not create new Markov states as part of this task. If the existing model represents the HF/severe subset through `affected_fraction`, preserve that approach. Do not silently reinterpret `sick_hf` or `sick_severe` as a generic transition without documenting and validating the mapping.

When more than one intervention acts on the same transition in an existing combined scenario, preserve the current validated combination rule. If the current code has no explicit rule, combine proportional effects multiplicatively on residual risk, make the result order-invariant, and validate that the resulting probability remains in `[0,1]`.

Model 06 must iterate over the scenario catalogue created by Model 04. It must not contain a second hardcoded intervention list, scenario list, effect size, coverage target, or cause map.

### Model 06 output contract

Preserve every existing column required downstream and make only additive output changes. The current-run model output must make these quantities available, with their timing and units documented:

- population;
- well stock;
- sick stock;
- incident cases/new `well -> sick` transitions, where available or safely derivable;
- cause-specific deaths/`sick -> dead` transitions;
- background-mortality deaths, de-duplicated at their proper population stratum;
- location, scenario identifiers, year, sex, age, and cause identifiers.

Do not count background mortality once per modeled cause. If the current cause-specific model structure repeats background mortality internally, create a de-duplicated background-mortality table/object for Model 09 rather than summing the repeated values.

Prefer exposing the current-run consolidated output in memory while retaining the existing `.rds` files needed by Models 07/08. Model 09 should consume the in-memory object when available and otherwise read only the exact current output contract. Do not create an additional state-trace `.rds` or CSV solely for Model 09.

## Task 2: add `09_cost_value.R`

Create `code/cvd-fair-choices/09_cost_value.R`. It must consume:

- the validated intervention/scenario/cost catalogues from Model 04;
- the current Model 06 state/flow output;
- the existing Model 08 economic-value output when available, without reimplementing VSL/VSLY calculations.

Do not calculate, import, or report DALYs, YLLs, YLDs, disability weights, or life-expectancy-based outcomes in Model 09. Model 07 may remain unchanged for backward compatibility, but Model 09 must not depend on its output. These outcomes are out of scope for this stage and will be added later.

It must perform the calculations in memory and write one final workbook:

`output/indonesia_model_cost_value.xlsx`

Do not create new intermediate `.rds` or `.csv` files unless an existing downstream interface makes one strictly necessary.

Use `indonesia_model_cost_value(1).xlsx` to understand the intended workflow and presentation, but implement the simplest production structure that satisfies the data contract and reconciliations. Do not make the production code depend on that example workbook at runtime.

### State/flow trace

Create a compact, auditable trace at the finest existing stable grain needed for costing, normally:

`location × scenario × year × sex × age × cause`

Include stable IDs and clearly labeled columns for population, well, sick, new cases, cause-specific deaths, and background deaths. If background deaths cannot be represented safely on cause rows without duplication, place them in a separate de-duplicated workbook table/sheet and explain the grain in the workbook README. Do not manufacture `new_cases` from stock differences unless the model timing equation supports it and the derivation is tested.

The example workbook's 13 raw fields are a good starting contract:

`scenario, location, year, age, sex, cause_code, intervention, well, sick, new_cases, cause_deaths, background_deaths, population`

Keep this minimum contract when it matches the real Model 06 semantics. Add fields such as `scenario_id`, `intervention_id`, `cause_id`, or pairing/QA fields when they improve auditability. Alter the raw contract if needed to prevent duplicated background deaths or to preserve the actual state-transition timing, and document the change.

### Costing logic

For each selected base-case cost component and year:

1. Map `population_in_need_measure` to the appropriate model quantity:
   - `all` -> eligible population;
   - `prevalence` -> eligible sick/prevalent stock;
   - `incidence` -> eligible new cases;
   - stop with a diagnostic for any unsupported measure.
2. Apply age and sex eligibility.
3. Calculate:

   `population_in_need = model_quantity * population_in_need_fraction`

   `annual_cost = population_in_need * coverage(t) * frequency_per_year * unit_cost_usd`

4. For the baseline comparator, use baseline states and baseline coverage.
5. For an intervention scenario, use scenario-specific states and that scenario's time-varying coverage.
6. Keep component-level rows so every total can be traced to `cost_record_id`, `cost_component_key`, and `cost_join_key`.
7. Count `shared-count-once`/`C_SHARED` components once per intervention and eligible stratum, never once per affected cause.
8. Do not reapply FAIR Choices cost markups when `indonesia_adjusted_flag = 1`.
9. Do not include downstream disease-cost offsets unless `downstream_cost_offsets = 1` and a valid, explicit cost source already exists.

Use the input workbook's cost discount rate and analysis start year:

`discount_factor_cost(t) = 1 / (1 + cost_discount_rate)^(t - analysis_start_year)`

Budget-impact results must report annual **undiscounted** baseline cost, scenario cost, incremental cost, and cumulative incremental cost. Discounted costs may be added as a separate economic-evaluation field, but must not replace the undiscounted budget-impact result.

### Cost-effectiveness logic

Pair every intervention result with the matching baseline at the same location/year/sex/age/cause grain before aggregation.

Calculate:

- deaths averted = baseline deaths - scenario deaths;
- incremental discounted cost;
- cost per death averted = cumulative incremental discounted cost / cumulative deaths averted over the analysis horizon.

Use undiscounted death counts for the denominator; discount costs only. Label the result clearly as `USD per death averted`, not as a DALY-based ICER.

The primary cost-per-death-averted result should be at scenario level across all affected causes so shared intervention costs are not duplicated. Provide cause-level outcome and cost breakdowns where meaningful, but keep shared costs in an explicit `C_SHARED`/all-affected-causes category rather than allocating them arbitrarily.

Handle dominance explicitly:

- more deaths averted and lower cost: `Dominant`;
- fewer deaths averted and higher cost: `Dominated`;
- zero or negative deaths averted in other cases: leave the ratio blank/`NA` and provide a status rather than printing a misleading ratio.

Do not classify scenarios as cost-effective unless an explicit threshold exists in the input workbook. If Model 08 results are available and reconcile by scenario/year, include VSL/VSLY economic value and a clearly labeled supplementary benefit–cost ratio or net benefit. Do not label benefit–cost analysis as cost-effectiveness analysis.

## Workbook content contract and preferred example structure

Produce a clear, user-friendly workbook containing the following **information**, regardless of how many sheets are used:

1. Purpose and run metadata — run date, model/input version, analysis years, cost discount rate, price year, baseline definition, scenarios, location, units, and package versions.
2. Inputs used — the validated intervention–cause catalogue, coverage trajectories, selected cost components, and all health/cost join keys.
3. Minimum model trace — the auditable population, well, sick, new-cases, cause-deaths, and background-mortality data required to reproduce the analysis.
4. Annual mortality results — exact baseline-versus-scenario pairing and cases, cause-specific deaths, and deaths averted.
5. Component costing — PIN quantity, PIN fraction, coverage, frequency, unit cost, annual cost, discount factor, and discounted cost by scenario/year/component.
6. Budget impact — annual and cumulative undiscounted baseline, scenario, and incremental costs, with discounted results kept separate.
7. Cost-effectiveness — incremental discounted costs, deaths averted, cost per death averted, and dominance status.
8. Economic value — reuse Model 08 VSL/VSLY results when available and reconcilable; otherwise record why they were omitted.
9. QA, methods, and sources — key uniqueness, input readiness, pairing, stock/flow checks, cost reconciliation, shared-cost checks, formulas/rules, and source provenance.

The example workbook's sheet arrangement is acceptable and may be retained, but it is not an acceptance criterion. A production workbook might use `README`, `Run_Metadata` or `Economic_Parameters`, `Selected_Interventions`, `Model_State_Trace` or `Model_Trace`, `Annual_Mortality`, `Cost_Component_Trace` or `Annual_Cost`, `Annual_Budget_Impact` or `Budget_Impact`, `Cost_Effectiveness`, `Economic_Value`, `QA_Checks`, and `Methods_and_Sources`. Combine or omit sheets when the same information remains easy to find, reproduce, and audit.

Support all selected scenarios. Do not limit the production workbook to the example's single editable `comparison_scenario`. If a single comparison selector is retained for a compact summary view, the detailed tables must still contain every scenario produced in the current run.

Avoid decorative dashboards or charts in this task. Keep tables filterable, freeze headers, use readable widths, and apply correct numeric formats for counts, percentages, USD, and cost per death averted.

Match the input workbook's visual system rather than inventing a new theme:

- Carlito font;
- dark blue `#1F4E78` headers with white bold text;
- light blue derived/formula areas (`#DDEBF7`/`#EAF3F8`);
- pale yellow `#FFF2CC` only for user-input or unresolved-input cells;
- green for passed checks and red/orange for failures/review items;
- consistent filters, freeze panes, borders, and number formats.

Where practical, make compact summary/CEA cells formula-linked to detailed sheets. Do not reproduce the example's large formula grids if an R-calculated long table is clearer and safer. Regardless of whether Excel formulas are used, calculate the same totals in R and reconcile them before saving so the workbook is correct even before manual inspection. Exclude the example workbook's DALY-specific fields, formulas, and sheets from the production output for now.

## Required QA and regression tests

At minimum, run and report:

1. `parse()`/syntax checks for every changed R file.
2. Workbook-schema and key-cardinality checks.
3. A numerical FAIR Choices formula check using at least one workbook row. For example, with effect `0.58`, baseline coverage `0.31`, target `0.80`, and affected fraction `0.80`, verify the adjusted transition effect is approximately `0.2772`.
4. Coverage trajectory checks: baseline at start, target at target year, monotonic linear interpolation, and bounds `[0,1]`.
5. Transition-probability bounds and, if applicable, combined-effect order invariance.
6. Baseline regression: baseline totals and schema remain unchanged within numerical tolerance.
7. Backward compatibility: Models 07 and 08 can still consume Model 06 outputs without modification.
8. State/flow checks: no impossible negative values and the model's documented stock-flow identities reconcile at representative strata.
9. Background mortality is not duplicated across causes.
10. Cost reconciliation: component rows sum exactly to annual budget-impact totals and scenario totals.
11. Shared-cost reconciliation: each selected shared component is counted once per eligible stratum/year.
12. CEA reconciliation: scenario-level discounted incremental cost and cumulative deaths averted equal the underlying annual/detail tables, and their ratio equals the reported cost per death averted.
13. Workbook inspection: every required information block is present somewhere in the workbook, headers/filters/freeze panes are usable, no formula errors appear, and representative totals match R results. Do not fail solely because production sheet names differ from the example.
14. `git diff --check` and a focused review of `git diff` to confirm no unrelated code/comments were changed.

If genuine missing user inputs prevent the full pipeline from running, do not invent them. The implementation must still pass syntax, schema, unit/formula, and fixture-level tests; the final report must list the exact workbook sheet, row/key, and field that blocks a full run.

## Completion criteria and final response

Do not stop after editing. Run the strongest feasible end-to-end test from Model 00 through Model 09. The task is complete only when either:

- the selected scenarios run, Models 07/08 remain compatible, the cost/value workbook is created, and all reconciliations pass; or
- the code is implemented and tested to the maximum feasible extent, and a genuine missing user input/data dependency is identified precisely without being fabricated.

In your final response provide:

- a concise summary of the implemented behavior;
- the exact files changed/created;
- the scenarios and intervention–cause links executed;
- tests run and their results;
- the path to `indonesia_model_cost_value.xlsx`;
- any unresolved user inputs or methodological decisions, identified by workbook key;
- confirmation that unrelated files and the baseline model logic were not changed.
