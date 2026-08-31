# Claude Code prompt: Indonesia 70-30-30 to 70-70-70 subnational analysis

You are working in the `uw-wb-indonesia-ncd` repository. Implement, run, and validate a complete Indonesia provincial analysis of the `70-30-30 -> 70-70-70` hypertension/cholesterol and diabetes cascade.

## Non-negotiable objective

Create a fully stand-alone subnational pipeline, separate from the production pipeline in:

```text
code/cvd-fair-choices/00-09*.R
```

The completed run must produce:

```text
output/70_30_30_to_70_70_70_subnational/indonesia_70_30_30_to_70_70_70_cost_value_formulae_subnational.xlsx
```

The final workbook must have the same analytical sheet structure, formula transparency, styling conventions, and calculation logic as the supplied national formula workbook, but contain results for every province included in the subnational input workbook and reconciled province rate data.

This is an implementation-and-execution task, not only a plan. Inspect the repository, implement the new pipeline, run a smoke test, run the full provincial analysis, generate the workbook, and report validation results.

## Absolute safety constraints

1. **Do not modify any existing production script in `code/cvd-fair-choices/00-09*.R`.** Treat those files as read-only references.
2. **Do not create, run, or source subnational or production `01_utils*` or `02_load_inputs*` scripts.** They are unnecessary because the specified `b_rates` file already contains the data prepared by those stages.
3. **Do not run, source, copy into the execution path, or otherwise invoke any `code/cvd-fair-choices/03*` script or any calibration process.** This includes random, Nelder-Mead, transparent, alignment, adjustment, or indirect calibration entry points. The specified `b_rates` file already contains the calibrated data required by this analysis.
4. Do not run `code/cvd-fair-choices/00_run_model_cvd_fair.R`.
5. Do not run the supplied national `00_run_70_30_30_to_70_70_70.R` unchanged: it currently sources `01`, `02`, and `03_calibration_indonesia_nelder_mead.R` and therefore violates this task.
6. Do not overwrite, append to, or change timestamps/content of ordinary production outputs under `output/`, including national clinical, public-health, combined, or national cascade outputs.
7. Do not modify unrelated code, reports, data, or configuration files.
8. Do not fabricate health or economic results and do not use placeholders. All final results must come from the supplied province rates, the subnational cascade workbook, and the adapted model logic.
9. Do not silently fall back to national rates, national populations, an alternate or older rate file, or a production output when provincial data are missing. Fail clearly instead.

Before implementation, record Git status and SHA-256 hashes of every existing production `code/cvd-fair-choices/00-09*.R` file and the ordinary production workbooks. Recheck them after the full run and fail if any changed. Preserve all pre-existing user changes in the worktree.

## Required sources

### Authoritative model rates

Use the province-level `b_rates` data at exactly:

```text
data/processed/b_rates_full_period_reconciled_2017_2050_national_current38.R
```

This file is the **final, already prepared and calibrated model-rate input**. It already contains the data processing that would otherwise have occurred in Models 01 and 02 and the calibration that would otherwise have occurred in Model 03. Therefore, load it directly in the new subnational 00 runner and begin the analytical pipeline at the adapted Model 04 stage. Do not repeat, reproduce, or rerun any 01, 02, or 03 logic.

Inspect the file rather than assuming its serialization from the extension. Build a safe, explicit loader directly in the new 00 runner for its actual format. The result must be a province-capable `data.table`/data frame with the model dimensions and transition inputs required by the baseline engine, including at minimum the available equivalents of:

```text
location, year, age, sex, cause, Nx, IR, CF, BG.mx
```

and any other fields actually required by the national 05/06 logic.

If the exact specified file does not exist or does not contain usable province data, stop with a precise diagnostic. Do not silently substitute the similarly named national `.rds` file or invoke calibration.

Use the 2017-2024 rows needed to seed/warm up the model and report the policy analysis for 2025-2050. Use the province-specific `Nx` values from this rate table; do not overwrite them with national UNWPP population.

### Authoritative intervention and cost inputs

Consume:

```text
data/indonesia_70_30_30_to_70_70_70_inputs_subnational.xlsx
```

The current workbook is a mock but already contains the national cascade contract and provincial extensions. Inspect every relevant sheet, its formulas, cached values, keys, and styles before changing anything. Required provincial sheets currently include:

```text
Provincial_Coverage_Source
Provincial_Cascade
Provincial_Trajectory
Provincial_Model_Input_View
```

The current workbook is designed for 38 provinces and has these expected cardinalities, which should be validated dynamically rather than blindly hard-coded:

- 76 province-sex rows in `Provincial_Cascade`;
- 3,952 rows in `Provincial_Trajectory` = 38 provinces × 2 interventions × 2 sexes × 26 years;
- 11,856 rows in `Provincial_Model_Input_View` = 38 provinces × 6 intervention-cause links × 2 sexes × 26 years.

Use stable keys—never worksheet row position—to join:

```text
province_code, province_name, intervention_id, intervention_cause_key,
cause_id, sex, year, transition_from, transition_to
```

If the mock workbook must be modified so R can consume it reliably, make only the minimum necessary changes to this workbook. Preserve its filename, sheet names/order, formulas, formatting, number formats, comments/notes, formulas, and existing source documentation. Update `ChangesLog`, `QA_Checks`, and `README` as appropriate. Do not replace formula-driven input logic with pasted derived values. Do not change epidemiological or cost assumptions merely to make the model pass.

If formula results lack cached values, do not treat blanks as zeros. Use a safe recalculation route on a copy, or expose a clearly documented formula-backed machine-readable range/sheet and validate it against the existing provincial sheets. Never modify the source workbook in place solely as a temporary recalculation step.

### Read-only references

Use the supplied files only to understand and mimic established logic, structure, naming, formulas, and formatting:

```text
00_run_70_30_30_to_70_70_70.R
00_run_model_cvd_fair.R
04_define_interventions_indonesia.R
05_build_baseline_indonesia.R
06_run_scenarios_indonesia_fair.R
07_output_dalys.R
08_economic_value_calculation.R
09_cost_value.R
indonesia_model_cost_value_formulae.xlsx
```

The other supplied national workbooks can be inspected for consistency, but are not model inputs for this run.

## Stand-alone code architecture

Create a clearly named dedicated directory, for example:

```text
code/cvd-fair-choices/70_30_30_to_70_70_70_subnational/
```

Inside it, create a self-contained subnational script series with unmistakable names, for example:

```text
00_run_70_30_30_to_70_70_70_subnational.R
04_define_interventions_70_30_30_to_70_70_70_subnational.R
05_build_baseline_70_30_30_to_70_70_70_subnational.R
06_run_scenarios_70_30_30_to_70_70_70_subnational.R
07_output_dalys_70_30_30_to_70_70_70_subnational.R
08_economic_value_70_30_30_to_70_70_70_subnational.R
09_cost_value_70_30_30_to_70_70_70_subnational.R
README.md
```

There must be **no executable subnational 01, 02, or 03 script**. Document the deliberate numbering gap in the new README: the supplied reconciled province `b_rates` file already incorporates the preparation formerly performed in 01/02 and the calibration formerly performed in 03, so those stages are neither required nor allowed.

The new 00 runner must load the prepared/calibrated `b_rates` object directly and then source only the new subnational 04-09 scripts. Do not source the production 00-09 scripts at runtime. If a small utility is essential, place it in the new 00 runner or the specific new 04-09 script that uses it; do not create a generic 01 utilities stage or a 02 data-loading stage. Reuse production logic by carefully copying and adapting only the required functions into the new files, preserving attribution in comments. Avoid broad refactoring.

Use repository-relative paths derived from a detected project root. Remove all hard-coded Windows/OneDrive paths and avoid `setwd()` dependencies where practical. The runner must work from a fresh R session and be reproducible with one command from the repository root.

## Analysis scope and model behavior

Run exactly two scenarios in every province:

```text
baseline
S_70_30_30_TO_70_70_70
```

The cascade scenario contains the two component interventions from the input workbook:

```text
I_CVD_PRIMARY
I_T2D_TREATMENT
```

Do not emit the two components as separate policy scenarios unless the current national cascade output explicitly does so. They should remain traceable components of the single combined cascade scenario.

Preserve the national cascade semantics:

- hypertension and cholesterol coverage use the provincial CVD treatment anchor;
- cholesterol coverage follows hypertension coverage;
- provincial diabetes treatment is moved up/down proportionally according to the provincial CVD treatment/capacity multiplier defined in the input workbook;
- the diagnosis-treatment-control cascade includes the treated-but-uncontrolled partial effect specified in `Assumptions`;
- the exact province × sex × intervention × year effective-coverage path comes from `Provincial_Trajectory`/`Provincial_Model_Input_View`;
- scale-up is piecewise linear to the 2030 and 2040 milestones and held thereafter;
- the no-backsliding rule applies. Therefore a province whose baseline effective coverage exceeds a nominal milestone uses `max(province baseline, target floor)`—do not force every province to exactly 0.1365 in 2030 or 0.4165 in 2040;
- do not round coverage before applying effects;
- health effects must reconcile to the input workbook transition multipliers to machine precision or a clearly justified numerical tolerance.

Translate workbook cause and sex labels explicitly and validate the mapping. At minimum preserve the current mappings:

```text
C_IHD -> ihd
C_IS  -> istroke
C_ICH -> hstroke
C_HHD -> hhd
C_T2D -> dm2
Men/Women -> Male/Female
```

Do not filter to `location == "Indonesia"`. Define the province set as the exact validated intersection of:

1. province names/codes in the subnational workbook; and
2. province locations present in the reconciled `b_rates` data.

Require set equality after applying an explicit, auditable province-name crosswalk. Print missing, extra, or duplicated locations and stop on unresolved mismatches. Do not use fuzzy matching or row order.

For each province, require uniqueness at the appropriate model key, normally:

```text
location × year × age × sex × cause
```

Validate complete year, age, sex, and cause coverage; finite and bounded transition probabilities; nonnegative population; and no duplicated rows before simulation.

Run the same well/sick/dead Markov logic and the same effect application rules as the current national cascade, but independently by province. Keep province identifiers through every intermediate result. Do not pool provinces before calculating outcomes.

## DALYs, value, and cost logic

Adapt the current 07, 08, and 09 logic province-by-province:

- deaths, cases, YLLs, YLDs, DALYs, and life-years gained must retain province keys;
- CVD probability of death between ages 30 and 70 must be calculated separately for each province, scenario, and year using the six current CVD causes;
- if province-specific life expectancy or disability-weight inputs do not exist, use the same national lookup used in the reference pipeline and clearly document that limitation; do not invent province-specific values;
- economic valuation may use the same national per-capita GNI/VSL/VSLY parameters for all provinces unless the repository contains an approved provincial alternative; document this explicitly;
- costs must use the exact per-year provincial coverage path used for health effects;
- population in need must be computed from the province model stock/flow (`all`, `prevalence`, or `incidence`) using the existing cost mapping;
- shared costs must be counted once per intervention and eligible stratum, never once per affected cause;
- cost per capita must use each province's population for that year, not the national population;
- discounting, price year, currency, and perspective must come from the input workbook/reference assumptions, not be hard-coded independently.

Use the current national sex-aggregation convention for cost records marked `Both`. Do not introduce a new weighting method silently. State the convention in `Methods_and_Sources` and validate that the same provincial trajectory drives both health and cost calculations.

## Required output workbook

Create one workbook with provinces stacked in long format, not 38 separate workbooks and not one worksheet per province. Include `province_code` and `province_name`/`location` in every sheet whose rows vary by province. All formulas that aggregate or look up values must include province as a criterion so values cannot leak across provinces.

Use the supplied national formula workbook as the structural/style reference. Preserve the standard sheet names and order when applicable:

```text
README
Run_Metadata
Scenario_Catalog
Cascade_Assumptions
Cascade_Trajectory
Cascade_QA
Selected_Interventions
Blocked_Links                 # only if applicable in the reference logic
Cost_Components
Annual_Mortality
Health_Outcomes
CVD_40q30
CVD_40q30_Age
Annual_Cost
Budget_Impact
Cost_Effectiveness
Economic_Value
Benefit_Cost
QA_Checks
Input_Diagnostic
Methods_and_Sources
Calculation_Assumptions
Calculation_Map
```

If a current national cascade formula workbook exists in the repository, inspect it and use its exact sheet order, headers, styles, colors, freeze panes, filters, number formats, and formula conventions as the primary reference. Otherwise, use the supplied national formula-edition workbook plus the cascade-specific logic in the current 09 script. Do not rename standard sheets merely to add “provincial” or “subnational.”

The workbook must be a genuine **formula edition**:

- R-generated source/anchor cells may be values and must retain the reference workbook's grey-source convention;
- all derived results must be live Excel formulas wherever the national formula workbook uses formulas, including baseline differences, deaths/cases/DALYs averted, coverage reconciliation, annual and discounted costs, budget impact, cost per capita, cost-effectiveness ratios, economic value, benefit-cost ratios, and QA status;
- formula references must use bounded ranges and explicit province criteria;
- formulas must safely handle zero denominators and missing values;
- do not paste derived results over formula cells;
- preserve the reference workbook's input/source/formula colors and number formats;
- set automatic/full recalculation on open.

`Cascade_Trajectory` must expose the exact province × sex × intervention × year coverage that R used. `Cascade_QA` must verify the province-specific target-floor/no-backsliding rule and reconcile the R multiplier against `Provincial_Model_Input_View`.

`Run_Metadata`, `README`, and `Methods_and_Sources` must state clearly:

- this is a stand-alone subnational run;
- no calibration was run;
- the exact `b_rates` and input-workbook paths;
- the province count and analysis horizon;
- the provincial coverage values are crude anchors requiring local validation;
- diabetes coverage is proportional to the provincial CVD treatment anchor;
- any national lookup/valuation assumptions shared across provinces;
- the formula/value color convention.

## Implementation and execution sequence

1. Inspect Git status, all supplied reference scripts, the two key inputs, and the national formula workbook before editing.
2. Write a short implementation plan tied to specific new files.
3. Record production-file and ordinary-output hashes/timestamps.
4. Implement only the stand-alone subnational 00 and 04, 05, 06, 07, 08, and 09 scripts. The new 00 runner must directly load the final prepared/calibrated `b_rates` file. Do not create or execute 01, 02, or 03.
5. Add fail-fast data-contract and collision guards before any long run.
6. Run static tests proving the new 00 runner directly loads the specified `b_rates` file, sources only the new subnational 04-09 scripts, and contains no call to 01, 02, 03, or any calibration/preparation process.
7. Run a one-province smoke test through workbook generation in a temporary/dedicated subnational test directory. The smoke test must not change production outputs.
8. Inspect smoke-test model states, coverage reconciliation, formulas, and workbook structure. Fix only the new subnational code/input workbook as allowed.
9. Run the full validated province set for 2025-2050. Use checkpoints/resume support within the dedicated subnational output directory if needed, but final aggregation must be deterministic and complete.
10. Generate the final formula workbook at the exact required path.
11. Recalculate a copy with a spreadsheet engine if available, then scan formulas and displayed results. Do not replace the final formula workbook with a values-only copy.
12. Recheck production-file hashes, ordinary-output hashes/timestamps, Git diff, and the complete validation suite.

## Required validation and acceptance tests

The task is complete only when all applicable checks pass.

### Isolation

- No existing production `code/cvd-fair-choices/00-09*.R` file changed.
- No 01, 02, or `03*` script was created, run, or sourced; the run log and source graph show this.
- The new 00 runner directly loads the specified final `b_rates` file and sources only the new 04-09 files in the dedicated subnational directory.
- Every file created by the run is under `output/70_30_30_to_70_70_70_subnational/`, except the explicitly allowed new code and any necessary minimal update to the stated subnational input workbook.
- Ordinary production output hashes/timestamps are unchanged.

### Input contract

- Province sets in `b_rates` and the workbook reconcile exactly after the explicit crosswalk.
- The current expected 38 provinces are included once each; if the validated input contains a different count, report it and explain rather than silently forcing 38.
- No duplicate `location × year × age × sex × cause` rows.
- Complete required years, ages, sexes, causes, and both scenarios.
- Provincial trajectory and model-input cardinalities reconcile to their dimensional products.
- Coverage is finite, in `[0,1]`, and nondecreasing after applying the no-backsliding rule.
- Transition multipliers are finite and within valid bounds.

### Model outputs

- Every province has baseline and `S_70_30_30_TO_70_70_70` results for every analysis year.
- No negative or impossible well, sick, dead, case, DALY, or population values beyond documented numerical tolerance.
- Population/state identities match the national engine's accepted tolerance.
- Baseline/scenario comparisons are paired by province and all other relevant dimensions.
- If a national row exists in the reconciled source, the sum of province results is compared with it for population and baseline health outcomes; report absolute and relative differences without recalibrating.

### Workbook

- Exact required filename and expected standard sheet names/order.
- No accidental `Indonesia` national-result rows mixed into province-result sheets.
- Every province appears in every applicable output sheet.
- Every aggregate/lookup formula includes province criteria.
- Formula columns actually contain formulas, not pasted values.
- No `#REF!`, `#DIV/0!`, `#VALUE!`, `#NAME?`, unintended `#N/A`, circular reference, or broken external link.
- No formulas reference production output workbooks or temporary smoke-test paths.
- Key Excel totals reconcile to independently calculated R anchors within tolerance.
- `QA_Checks` and `Cascade_QA` contain no unresolved `FAIL`; any `REVIEW` item is explained.
- The workbook opens normally, retains the reference styling, has readable columns/headers, and is set to recalculate on open.

## Final response

When finished, provide:

1. a concise summary of what was implemented and run;
2. the exact list of new files and the one input workbook modified, if any;
3. the final workbook path;
4. province count and scenario count;
5. smoke-test and full-run commands;
6. key QA/reconciliation results, including coverage/multiplier and province-to-national checks;
7. explicit confirmation that no production 00-09 script, no 01/02 preparation stage, no 03/calibration process, and no ordinary production output was modified or run;
8. any remaining `REVIEW` limitations, especially the crude provincial coverage anchors and use of national valuation/life-expectancy assumptions.

Do not stop after writing code. The final workbook and completed validation evidence are required deliverables.
