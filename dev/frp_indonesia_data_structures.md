# Claude Code Prompt: Indonesia FRP Data Structures

You are working in the Indonesia NCD modeling repository. Implement the requested task completely, test it, and report the results.

## Scope and non-negotiable constraints

Create exactly one new source-code file:

`code/cvd-fair-choices/10-frp_indonesia.r`

The script must generate:

`output/FRP_data_structures.xlsx`

Do not edit, reformat, rename, or delete any other source code, input, configuration, report, or workbook. The only permitted filesystem changes are:

1. creating `code/cvd-fair-choices/10-frp_indonesia.r`; and
2. creating or replacing `output/FRP_data_structures.xlsx` when the script is tested.

Do not run calibration or any upstream modeling scripts. Do not modify the model to run separately by wealth quintile. This task is a post-processing allocation of completed national model outputs.

Do not use any illustrative CSV, XLSX, or DOCX sample files. Use only the canonical repository files listed below.

## Goal

Distribute the completed national clinical-intervention model results across wealth quintiles Q1–Q5 to create the data structures needed for subsequent financial risk protection analysis.

This is not a quintile-specific model run. It is a transparent proportional allocation of national model outcomes using quintile-specific population and disease-burden shares. The allocated results must reaggregate to the original national model results.

## Canonical inputs

Use these files:

- National model output: `output/out_model/model_output_Indonesia_htncov2_aspirational.rds`
- Wealth-quintile population: `data/processed/indonesia_wealth_population.rds`
- Wealth-quintile disease burden: `data/processed/indonesia_wealth_burden.rds`
- Clinical-intervention costs: `output/indonesia_model_cost_value_formulae.xlsx`

Treat the population and burden RDS files as the sources of truth for the distribution across quintiles. Treat the cost/value workbook as the source of truth for clinical-intervention cost records, coverage, unit costs, price years, discounting assumptions, and national quantities.

Before coding, inspect the actual objects, columns, keys, classes, and workbook sheets. Read relevant existing code—especially Models 00, 06, and 09 and any workbook-writing utilities—to follow current naming conventions, styling, cause mappings, and dependencies. This inspection is read-only.

## Analysis scope

Include only:

- the baseline scenario;
- standalone clinical-intervention scenarios; and
- the combined clinical package if it is classified as clinical.

Exclude:

- all public-health scenarios;
- public-health packages; and
- combined clinical-plus-public-health scenarios.

Determine inclusion dynamically using fields such as `scenario`, `intervention_family`, `scenario_role`, and the canonical scenario catalog. Do not maintain a fragile hard-coded list if the model metadata can identify clinical scenarios.

Use analysis years 2025–2050, or the equivalent start and end years defined in the canonical cost workbook. Verify that this resolves to 2025–2050 and report any difference.

## Cause mapping

Create one explicit, validated mapping between the wealth-burden cause IDs and model cause codes. At minimum, support the repository’s actual equivalents of:

- `C_IHD` → `ihd`
- `C_IS` → `istroke`
- `C_ICH` → `hstroke`
- `C_HHD` → `hhd`
- `C_RHD` → `rhd`
- `C_CMD` or `C_CMP` → `cmd`
- `C_DM` or `C_T2D` → `dm2`

Inspect the real inputs and use their actual codes. Stop with an informative error if any included clinical cause is unmapped.

## Recompute national allocation shares

Do not assume any existing `population_share` or `allocation_share` column is correct without checking it.

### Population

Aggregate the wealth population data to the national level by:

`year × age_group × sex × quintile_id × wealth_quintile × quintile_order`

Sum:

- `population`
- `all_cause_deaths`

Then recompute:

- population share within `year × age_group × sex`;
- all-cause-death share within `year × age_group × sex`.

Also compute all-age, both-sex population and all-cause-death shares by year and quintile for the FRP-ready annual outputs.

### Disease burden

Aggregate the burden data to the national level by:

`year × cause_id × measure × quintile_id × wealth_quintile × quintile_order`

Sum `value`, then recompute:

`allocation_share = value / sum(value)`

within:

`year × cause_id × measure`

Never assume that each quintile represents 20% of the population.

If the source already contains an allocation-share column, compare it with the recomputed share and record any material discrepancy in QA. Use the recomputed national share.

## Allocation method

First aggregate the national model output to the grain needed by the downstream FRP analysis:

`scenario × year × cause`

Sum over age and sex. The visible FRP output should retain the required `age` and `sex` columns but populate them as `All` and `Both`. This avoids exceeding Excel’s row limit and reflects that the downstream FRP analysis needs annual condition volumes rather than single-year-age model traces.

Document this aggregation clearly. Do not imply that the model was run separately for each quintile.

Create five locations:

- `Indonesia_Q1`
- `Indonesia_Q2`
- `Indonesia_Q3`
- `Indonesia_Q4`
- `Indonesia_Q5`

Allocate national measures as follows:

- `pop`: all-age population share for the corresponding year and quintile;
- `all.mx`: all-cause-death share for the corresponding year and quintile;
- `sick`: burden allocation share for `Prevalence`;
- `newcases`: burden allocation share for `Incidence`;
- `dead`: burden allocation share for `Deaths`;
- `well`: calculate as the residual `pop - sick - all.mx`.

Confirm from the existing model code that this is the applicable state identity. If the repository uses a different documented identity, follow the repository definition and explain it in the script.

The sum over Q1–Q5 must reproduce the corresponding national value for every scenario, year, cause, and measure.

### Missing burden shares

Implement and document a deterministic fallback:

1. Use the exact `year × cause × measure × quintile` burden share when available.
2. If an exact burden distribution is unavailable, use the corresponding all-age population share for that year and quintile.
3. Flag every fallback as `NATIONAL_RATE_APPLIED` or an equally explicit status.
4. Summarize fallback counts by cause and measure in the README, QA output, console report, and script comments.
5. Never silently drop a cause, measure, scenario, year, or quintile.

Treat invalid, negative, infinite, or non-normalizable shares as errors.

## Cost allocation

Use the clinical rows in `output/indonesia_model_cost_value_formulae.xlsx`, especially the canonical contents of:

- `Annual_Cost`
- `Cost_Components`
- `Selected_Interventions`
- `Scenario_Catalog`
- `Calculation_Assumptions`

Do not use a different or older cost workbook.

Be aware that calculated cells in the formula workbook may lack cached Excel values. Do not silently read formulas as missing values and do not substitute another workbook. Use the numeric helper quantities and underlying input sheets to reconstruct the cost formulas. In particular, inspect and use fields such as:

- `r_quantity_scenario`
- `r_quantity_baseline`
- `population_in_need_fraction`
- `coverage_baseline`
- `coverage_target`
- coverage start and target years
- `frequency_per_year`
- `unit_cost_usd`
- `price_year`
- discount rate and base year.

Allocate national population-in-need quantities by quintile according to `population_in_need_measure`:

- `all`: population share for the cost record’s year, sex, and age window;
- `prevalence`: the relevant cause-year prevalence share;
- `incidence`: the relevant cause-year incidence share.

For age windows that partially overlap a five-year population band, prorate that band under a uniform-within-band assumption. Handle `95+` consistently with the model’s pooled age. Document this approximation.

For `shared-count-once` costs, allocate the shared quantity across quintiles once per cost record. Do not duplicate it across causes.

Unless the canonical inputs contain quintile-specific coverage, hold baseline and scenario coverage equal across quintiles and state this assumption clearly. Do not invent a coverage gradient.

## Excel workbook contract

Create `output/FRP_data_structures.xlsx` with these six visible worksheets, in this order:

1. `README`
2. `1_INPUT_epi_by_quintile`
3. `2_OUTPUT_volumes`
4. `3_OUTPUT_costs`
5. `4_RECONCILIATION`
6. `5_QUESTIONS`

You may add hidden `SOURCE_*` worksheets when necessary to make the visible workbook formula-driven and auditable. Hidden source sheets may contain imported national values, recomputed shares, cost parameters, and assumptions.

### 1_INPUT_epi_by_quintile

Use these exact columns:

`year, location, age, sex, cause_id, age_group, age_mid, population, all_cause_mx, all_cause_deaths, cause_fraction, incidence_rate, prevalence_rate, cause_mx, cause_deaths, incident_cases, prevalent_cases, epidemiology_source, incidence_available, prevalence_available, quintile_data_status`

Construct this as an audit/proxy baseline table from the allocated baseline results. Follow existing repository definitions for rates and denominators. Do not invent a rate definition when one already exists in the code.

Set `epidemiology_source` to wording that makes clear that this is post-model proportional allocation rather than a quintile-specific model run.

### 2_OUTPUT_volumes

Use these exact columns:

`scenario, location, year, cause, age, sex, well, sick, newcases, dead, pop, all.mx, intervention`

Populate `age = "All"` and `sex = "Both"`.

All allocated numeric output columns must be Excel formulas referring to hidden source values and allocation shares. Do not hard-code derived quintile outcomes.

### 3_OUTPUT_costs

Use these exact columns:

`scenario, location, year, intervention_id, cause_code, cost_record_id, cost_component_key, cost_join_key, cost_scope, population_in_need_measure, population_in_need_fraction, pin_baseline, pin_scenario, coverage_baseline, coverage_scenario, frequency_per_year, unit_cost_usd, price_year, annual_cost_baseline, annual_cost_scenario, annual_cost_incremental, disc_cost_incremental`

The following must be formulas:

- `pin_baseline`
- `pin_scenario`
- `coverage_scenario`
- `annual_cost_baseline`
- `annual_cost_scenario`
- `annual_cost_incremental`
- `disc_cost_incremental`

Reference the canonical source cells or hidden source sheets. Do not place magic numbers inside formulas.

### 4_RECONCILIATION

Use these exact columns:

`scenario, year, cause, measure, sum_over_quintiles, national_run, abs_diff, pct_diff, status`

Include reconciliation rows for at least:

- `well`
- `sick`
- `newcases`
- `dead`
- `pop`
- `all.mx`
- baseline cost
- scenario cost
- incremental cost
- discounted incremental cost.

Use Excel formulas for the five-quintile sum, differences, percentages, and status.

Use both:

- an absolute tolerance appropriate for floating-point counts; and
- a relative tolerance, defaulting to 0.1% unless the repository already defines a stricter standard.

Store tolerances in visible or hidden assumption cells and reference them in formulas.

### 5_QUESTIONS

Use these exact columns:

`#, Topic, Question, Your answer`

Populate it with the material methodological decisions and their implemented answers, including:

- uniform coverage across quintiles;
- all-age burden shares applied to annual outcomes;
- missing-share fallback;
- age-band prorating for cost allocation;
- health-system cost perspective;
- absence of a quintile-specific model run.

### README

Explain:

- purpose and scope;
- canonical input files;
- included clinical scenarios and years;
- allocation equations;
- cause mapping;
- cost allocation;
- formula conventions;
- assumptions;
- limitations;
- fallback rules;
- QA tolerances;
- generated date;
- how to interpret `Indonesia_Q1`–`Indonesia_Q5`.

Include a prominent statement that these are allocated national results, not results from five separately calibrated or simulated quintile models.

## Formula and formatting requirements

The workbook must be audit-friendly and formula-driven:

- raw imported inputs may be hard-coded on hidden source sheets;
- all derived visible results must be formulas;
- use bounded references rather than whole-column references;
- quote worksheet names in cross-sheet formulas;
- avoid volatile formulas;
- avoid circular references;
- set workbook calculation mode so Excel recalculates formulas when opened;
- preserve numeric types and use appropriate formats for counts, rates, percentages, and US dollars;
- freeze headers, apply filters, use readable widths, and follow the existing cost workbook’s professional style;
- visually distinguish raw inputs, formulas, QA statuses, assumptions, and warnings;
- add conditional formatting for PASS/REVIEW/FAIL;
- do not write beyond Excel’s row limit.

## Mandatory QA in R before saving

Implement programmatic checks that stop execution on material failures:

1. All required input files and columns exist.
2. Q1–Q5 are present for every required year.
3. Population shares sum to 1 within tolerance.
4. All-cause-death shares sum to 1 within tolerance.
5. Burden shares sum to 1 within tolerance for every available year-cause-measure key.
6. No invalid, negative, infinite, or unmapped allocations.
7. No duplicated output keys.
8. Every included national scenario-year-cause has five quintile rows.
9. Sum over quintiles reproduces the national model output for every measure.
10. Sum over quintiles reproduces the national population-in-need and all cost totals.
11. No materially negative `well` values.
12. No excluded public-health scenario enters the workbook.
13. No formula contains `#REF!`, broken worksheet references, or accidental external workbook links.
14. Workbook row counts remain within Excel limits.

Compute the reconciliations independently in R as well as through Excel formulas. Do not rely exclusively on uncalculated Excel formulas for QA.

Write the workbook to a temporary file first. Only move it to `output/FRP_data_structures.xlsx` after all mandatory R-side checks pass.

## Assumptions and limitations in the R script

At the top of `10-frp_indonesia.r`, include a detailed comment block documenting at least:

- proportional allocation does not reproduce a quintile-specific disease model;
- national intervention effect sizes are implicitly assumed constant across quintiles;
- coverage is held constant across quintiles unless the canonical input says otherwise;
- burden shares are all-age and both-sex unless the source provides more detail;
- all-age shares are therefore applied across annual outcomes;
- the fallback to population shares assumes equal rates across quintiles;
- age-band prorating assumes uniform population within a five-year band;
- allocated outcomes preserve national totals by construction;
- resulting FRP estimates should be interpreted as distributional scenarios rather than independently calibrated projections;
- uncertainty from allocation assumptions is not propagated unless explicitly available.

Also place these assumptions and limitations in the workbook README.

## Testing and final report

Run only `code/cvd-fair-choices/10-frp_indonesia.r`. Do not run Models 00–09 or calibration.

After execution, inspect the generated workbook and verify:

- worksheet order and exact headers;
- formulas in representative rows;
- no broken formula references;
- all QA statuses;
- no public-health scenarios;
- readable formatting;
- output file existence and size.

Then run `git status --short` and confirm that no unintended file changed.

In your final response, report:

- the new script created;
- the output workbook created;
- clinical scenarios and years included;
- number of allocated volume and cost rows;
- whether fallbacks were used, summarized by cause and measure;
- maximum absolute and relative reconciliation differences;
- QA results;
- assumptions and limitations;
- any unresolved issue.

Do not claim completion if any mandatory reconciliation fails.
