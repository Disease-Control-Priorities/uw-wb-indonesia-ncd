# Claude Code prompt: add CVD 40q30 and a joint clinical + public-health workbook

You are working in the repository containing `code/cvd-fair-choices`. Plan the change, implement it, run the relevant pipeline/tests, inspect the generated Excel files, and report the exact files changed and test results.

## Objective

Extend the existing workbook-driven Indonesia CVD pipeline so that it:

1. Runs a genuine **Clinical + Public Health** scenario containing every runnable clinical intervention and every runnable public-health intervention in the same Model 06 simulation.
2. Computes annual period **CVD 40q30** for every scenario from the single-year-age Model 06 output.
3. Adds formula-driven CVD 40q30 sheets to the clinical, public-health, and new combined formula workbooks.
4. Creates one new formula workbook containing the clinical results, public-health results, and the genuine joint scenario:
   `output/indonesia_model_cost_value_clinical_public_health_formulae.xlsx`.

Do not estimate the joint health effect by adding or otherwise combining results from separate runs. Apply both intervention families to the same transition-rate table and run the Markov model once.

## Files in scope

The request's reference to `000` means the existing Model 00 driver:

- `code/cvd-fair-choices/00_run_model_cvd_fair.R`
- `code/cvd-fair-choices/04_define_interventions_indonesia.R`
- `code/cvd-fair-choices/05_build_baseline_indonesia.R`
- `code/cvd-fair-choices/06_run_scenarios_indonesia_fair.R`
- `code/cvd-fair-choices/07_output_dalys.R`
- `code/cvd-fair-choices/09_cost_value.R`

Use these current workbooks as structural and styling references:

- `output/indonesia_model_cost_value_formulae.xlsx`
- `output/indonesia_cost_value_public_health_formulae.xlsx`

Do not modify other scripts, input workbooks, or unrelated files. Inspect Model 08 only to verify that it processes scenario IDs generically; do not edit it. If Model 08 prevents the new scenario from flowing through, report that blocker instead of silently expanding scope.

First inspect the current code and make a short implementation plan. Keep the diff targeted. In particular, do not force a change to Model 05 merely because it appears in the requested list: Model 05 should remain the single common baseline builder. Change it only if a minimal modification is actually required for the joint run or a necessary invariant; otherwise leave it unchanged and explain why.

## 1. Central configuration and the joint scenario

### Model 00

- Preserve the existing `run_clinical_interventions` and `run_public_health_interventions` switches and the single shared baseline.
- When both switches are `TRUE`, create/run the joint scenario automatically. Runs with only one family enabled must continue to work unchanged; both switches `FALSE` may retain the current early error.
- Declare the new combined workbook path once in Model 00:
  `combined_cost_value_formulae_file <- paste0(wd_outp, "indonesia_model_cost_value_clinical_public_health_formulae.xlsx")`.
- Declare the six CVD causes used by CVD 40q30 once, beside the central `cause_map`, using the current codes:
  `ihd`, `istroke`, `hstroke`, `hhd`, `rhd`, and `cmd`.
  Name the object clearly, for example `cvd_40q30_cause_codes`. Validate that all six are present in `cause_map`, are unique, and that the vector has length six. Do not include `dm2` or the all-cause envelope.
- Do not duplicate analytic assumptions from either input workbook in Model 00.

### Model 04

- Preserve the existing clinical `fair_scenarios` and public-health `public_health_scenarios` catalogues.
- After both catalogues have been validated, construct one joint scenario with a stable ID such as `all_clinical_public_health` and label `All clinical + public-health interventions (combined)`.
- The entry must carry:
  - `family = "clinical_public_health"`;
  - `scenario_level = "combined"` and `scenario_role = "combined"`;
  - the union of all runnable intervention IDs, retaining family provenance;
  - `interventions = c("fair_wb", "ph_wb")`;
  - the complete validated clinical `fair_effect_rows` used by the clinical `all` scenario;
  - the complete validated public-health `ph_effect_rows` used by `all_public_health`;
  - appropriate `NA` values for fields such as parent package ID that do not apply.
- Create the joint entry only when both families are enabled and both have at least one runnable intervention. Fail with a clear diagnostic if both switches are on but the required validated effect rows are unavailable.
- Ensure scenario IDs remain collision-safe and that the baseline appears exactly once.
- Extend the scenario catalogue/label metadata consumed by Models 06, 07, and 09. Do not overwrite either family catalogue.

### Models 05 and 06

- Keep Model 05 as the one common baseline. Do not build separate clinical and public-health baselines.
- Register the joint scenario in Model 06 when both families are enabled.
- The existing `project.all()` dispatch should apply `fair_wb` and `ph_wb` exactly once each to the same copy of the baseline rates before state initialization and projection. Pass both effect-row tables and the public-health tobacco timing configuration.
- Do not execute the clinical `all` scenario and the public-health `all_public_health` scenario inside the joint scenario, and do not add their output deaths, cases, DALYs, costs, or transition effects.
- Preserve the current within-family effect logic and scales. Do not invent a new effect-combination formula. The clinical engine and public-health engine should operate through their existing validated transition pathways in one run.
- Carry `intervention_family`, `scenario_role`, `scenario_level` if available, `parent_package_id`, and `htn_target_scenario` into the Model 06 output for unambiguous downstream filtering.
- Preserve the existing standalone clinical, standalone public-health, package, combined-within-family, and baseline scenarios.

## 2. Compute annual CVD 40q30 in Model 07

Use the Model 06 output already loaded by `07_output_dalys.R`. Compute the metric separately for every:

`location × scenario × htn_target_scenario × calendar year`.

This is a period measure for each calendar year; never pool deaths or population across years.

### Required method

Use exact ages 30 through 69 and the six configured CVD causes. At each exact age, combine female and male deaths before forming the rate:

\[
m_x=\frac{D^{CVD}_{x,F}+D^{CVD}_{x,M}}{N_{x,F}+N_{x,M}},
\qquad
q_x=1-\exp(-m_x).
\]

With \(l_{30}=1\), recursively calculate:

\[
l_{x+1}=l_x(1-q_x), \qquad
{}_{40}q_{30}=100\left(1-\frac{l_{70}}{l_{30}}\right).
\]

This must agree with `100 * (1 - lx[71] / lx[31])` when an R life-table vector begins at age 0, and with `100 * (1 - exp(-sum(m_x)))` for ages 30:69.

### Critical denominator rule

The Model 06 population is repeated on each cause row. Sum CVD deaths across the six causes, but take population **once per location/scenario/year/age/sex**, then sum female and male population. Never sum population across causes; doing so would multiply the denominator by six.

Before de-duplicating population, verify that the repeated population value is invariant across the six cause rows for each location/scenario/year/age/sex key. Fail loudly if it is not.

### Model 07 output contract

Create an auditable object, for example `dt_cvd_40q30`, and save it as:

`output/07_cvd_40q30.rds`.

Include at least:

- `location`, `scenario`, `scenario_label`, `intervention_family`, `scenario_role`, `parent_package_id`, `htn_target_scenario`, and `year`;
- `cvd_40q30` on the requested 0–100 percent scale;
- `baseline_cvd_40q30`, paired to the shared baseline on location/year/HTN-target scenario;
- `absolute_reduction_pp = baseline_cvd_40q30 - cvd_40q30`;
- `percent_reduction = 100 * (baseline_cvd_40q30 - cvd_40q30) / baseline_cvd_40q30`, returning `NA` with a diagnostic if the baseline is zero;
- optionally the R check based on `100 * (1 - exp(-sum(m_x)))` and the maximum reconciliation residual.

Also retain or construct an age-level audit table used by Model 09, with the sex-combined CVD deaths, de-duplicated population, `m_x`, `q_x`, `l_x`, and `l_{x+1}` for ages 30:69. Save it separately if that makes the workbook formulas clearer.

Add fail-fast QA for:

- exactly the configured six CVD causes;
- exactly 40 unique ages 30:69 per output key;
- both Female and Male rows present before sex aggregation (allow only an explicit, documented mapping of equivalent labels);
- positive, finite population and nonnegative finite deaths;
- no duplicate keys or missing baseline pairs;
- `0 <= cvd_40q30 <= 100`;
- life-table and exponential expressions agreeing within a tight numeric tolerance;
- baseline reduction equal to zero within tolerance.

Extend the scenario-label lookup so the new joint scenario is labeled correctly.

## 3. Add formula-driven CVD 40q30 sheets in Model 09

Model 09 must require and load `output/07_cvd_40q30.rds` (and the age audit contract, if separate), reconcile its scenario IDs against the current Model 06/07 run, and fail with an actionable message when it is missing or stale.

Add CVD 40q30 to **all three formula workbooks**:

1. `output/indonesia_model_cost_value_formulae.xlsx` — baseline plus clinical scenarios only;
2. `output/indonesia_cost_value_public_health_formulae.xlsx` — baseline plus public-health scenarios only;
3. `output/indonesia_model_cost_value_clinical_public_health_formulae.xlsx` — baseline, all clinical scenarios, all public-health scenarios, and `all_clinical_public_health`.

The opening request explicitly requires the public-health workbook even though it was omitted from one numbered bullet; treat it as required.

For each workbook add:

- `CVD_40q30_Age`: auditable age-level source rows for ages 30:69. Deaths and de-duplicated population may be grey R-source cells, but `m_x`, `q_x`, `l_x`, and `l_{x+1}` must be live Excel formulas.
- `CVD_40q30`: one row per location/scenario/HTN-target scenario/year, with live formulas for `cvd_40q30`, `baseline_cvd_40q30`, `absolute_reduction_pp`, and `percent_reduction`. Include an R-source anchor and a formula reconciliation status.

Use the existing formula-workbook conventions:

- dark-blue headers;
- blue formula cells, grey R-source cells, and yellow editable inputs where applicable;
- filters, frozen headers, readable widths, correct numeric formats, and the existing Carlito font;
- quoted cross-sheet references such as `='CVD_40q30_Age'!A1`;
- formulas stored as formulas, not hard-coded cached values;
- full recalculation on open.

Because `cvd_40q30` is stored on a 0–100 scale, display it and its percentage-point reduction with an ordinary numeric percent label/format (for example `0.000`), not Excel's fractional `%` format unless you deliberately store the underlying value on a 0–1 scale. Make the scale explicit in the column name or unit.

Add the method and dependencies to `README`, `Methods_and_Sources`, `Calculation_Map`, and `QA_Checks`. Preserve all existing sheets, formulas, styles, and their current order except for inserting the new CVD sheets in a logical location near `Annual_Mortality`/`Health_Outcomes`.

## 4. New combined clinical + public-health formula workbook

Create exactly one new file:

`output/indonesia_model_cost_value_clinical_public_health_formulae.xlsx`.

It must be generated by `09_cost_value.R` from the current in-memory/RDS contracts and both validated Model 04 catalogues. Do not make a values-only copy-and-paste of the two existing workbooks.

### Scenario contents

Include the shared baseline exactly once, all clinical scenarios, all public-health scenarios, and the genuine joint `all_clinical_public_health` scenario. Add an `intervention_family` field to scenario-level result tables where it is not already present, with values `baseline`, `clinical`, `public_health`, or `clinical_public_health`.

### Preserve workbook structure

- Preserve the existing common decision sheets and their formula logic: `Annual_Mortality`, `Health_Outcomes`, `Annual_Cost`, `Budget_Impact`, `Cost_Effectiveness`, `Economic_Value`, `Benefit_Cost`, diagnostics, assumptions, methods, and calculation map.
- Preserve family-specific input/audit sheets. Where two source workbooks have sheets with the same name but incompatible schemas, retain both with short, valid prefixes such as `CL_` and `PH_` rather than dropping columns or coercing unlike tables. Keep every sheet name within Excel's 31-character limit and update all cross-sheet formulas accordingly.
- Do not duplicate compatible common result sheets. Prefer one common result sheet containing all relevant scenario rows and the family trace.
- Keep existing source column names and order within each family block; add only fields needed for provenance or the joint scenario.

### Costing the joint scenario

Cost the joint scenario from its own Model 06 state/flow results:

- clinical components must use the joint scenario's relevant population-in-need quantities and the existing clinical costing rules;
- public-health components must use the joint scenario's population and the existing public-health costing rules;
- preserve family provenance on every cost component;
- guard against duplicate component IDs or shared costs across families by using family in the key or another explicit collision-safe method;
- joint annual and discounted incremental cost equals the sum of the clinical and public-health component costs calculated for the **joint scenario**, not the sum of the already-aggregated `all` and `all_public_health` scenario outputs.

Do not double count the baseline or shared within-family costs.

### Summary sheet

Add a first sheet named `Summary`. It must be formula-driven and cover all comparator scenarios, with the joint scenario visually highlighted. Include, at minimum:

- scenario ID, label, family, and intervention count;
- analysis start/end years;
- cumulative cases averted, deaths averted, YLLs/YLDs/DALYs averted, and life-years gained;
- CVD 40q30 at the analysis start year and end year;
- end-year absolute CVD 40q30 reduction in percentage points and relative percent reduction;
- cumulative incremental and discounted incremental cost;
- cost per death averted and, if already supported by the pipeline, cost per DALY averted;
- preferred PV benefits, PV costs, net benefits, and benefit-cost ratio from the existing BCA logic.

All derived summary cells must reference detailed sheets with live Excel formulas. Do not hard-code R-calculated headline results into the summary.

## 5. Testing and acceptance criteria

Run the smallest relevant tests first, then execute the end-to-end Indonesia pipeline needed to regenerate the outputs. Do not declare success from static inspection alone.

At minimum verify:

1. **Switch behavior**
   - clinical `TRUE`, public health `FALSE`: existing clinical scenarios/output still work and no joint scenario is created;
   - clinical `FALSE`, public health `TRUE`: existing public-health scenarios/output still work and no joint scenario is created;
   - both `TRUE`: baseline appears once and `all_clinical_public_health` appears once;
   - both `FALSE`: current clear error remains.
2. **Joint simulation**
   - the joint scenario carries both `fair_effect_rows` and `ph_effect_rows`;
   - each effect family is applied once;
   - the joint scenario is produced by one projection run, not an arithmetic combination of family outputs.
3. **CVD 40q30**
   - six CVD causes only, ages 30:69 only, population de-duplicated across causes;
   - a synthetic constant-rate test agrees with the closed form (for example, `m_x = 0.01` for 40 ages gives `100*(1-exp(-0.4))`);
   - R life-table, R exponential check, and Excel formula results reconcile within tolerance;
   - baseline reductions equal zero and every non-baseline scenario has a baseline pair.
4. **Workbooks**
   - all three `.xlsx` files open successfully and are valid ZIP/XLSX packages;
   - all expected existing sheets remain and the new CVD sheets are present;
   - the combined workbook contains every produced clinical and public-health scenario plus the joint scenario, with no duplicate baseline;
   - derived cells in the CVD sheets and `Summary` are formulas;
   - formulas contain no `#REF!`, `#DIV/0!`, `#VALUE!`, `#NAME?`, or unintended circular references after recalculation;
   - formula results reconcile with R-source anchors;
   - inspect every sheet visually or through a rendered preview to catch clipped headers, broken widths, unreadable formatting, or blank/broken tables.
5. **Regression checks**
   - existing clinical and public-health scenarios retain their prior results within numeric tolerance when the new joint scenario is added;
   - existing workbook formulas, BCA logic, sheet styling, and scenario filters are not broken;
   - no unrelated file changes are present.

If required raw data prevent a full run in the current environment, still run all feasible unit/contract tests, build a minimal synthetic fixture for the new CVD 40q30 and scenario-registry logic, and state exactly what could not be executed and why. Do not fabricate a successful end-to-end result.

## Final report

Return:

1. the implementation plan actually followed;
2. a concise summary of the changes by file;
3. the exact joint scenario ID and the six CVD cause codes used;
4. the generated workbook paths and their sheet lists;
5. the commands/tests run and pass/fail results;
6. any assumptions, blockers, or remaining limitations;
7. `git diff --stat` and confirmation that no out-of-scope files were modified.
