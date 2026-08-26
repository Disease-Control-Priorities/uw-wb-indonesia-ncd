# Claude Code prompt: make intervention flags binding and make the executive deck dynamic and BCA-free

You are working in the Indonesia NCD/CVD repository. Modify the active CVD FAIR Choices pipeline so that workbook `include_flag` values are authoritative from intervention definition through all downstream analyses and reporting. Preserve the existing benefit-cost analysis (BCA), Model 08, VSL/VSLY calculations, and BCA workbook outputs in the pipeline. Remove BCA **only from `reports/executive_slides.Rmd`**, where the economic-evaluation result displayed should be cost per death averted in US dollars. Make the executive Beamer deck compile for any valid subset of clinical and public-health interventions.

Work in **Plan → Test → Execute → Verify** order. Do not stop after writing a plan: implement the changes and run the permitted tests. Begin by inspecting `git status`, the relevant diffs, and the current repository paths. Preserve unrelated user changes and do not reformat or refactor unrelated code.

## Files in scope

Locate the exact repository paths rather than assuming capitalization, but the intended files are:

- `data/indonesia_model_inputs.xlsx`
- `data/indonesia_model_inputs_public_health_updated_mortality.xlsx`
- `code/cvd-fair-choices/00_run_model_cvd_fair.R`
- `code/cvd-fair-choices/04_define_interventions_indonesia.R`
- `code/cvd-fair-choices/05_build_baseline_indonesia.R`
- `code/cvd-fair-choices/06_run_scenarios_indonesia_fair.R`
- `code/cvd-fair-choices/07_output_dalys.R`
- `code/cvd-fair-choices/08_economic_value_calculation.R`
- `code/cvd-fair-choices/09_cost_value.R`
- `reports/executive_slides.Rmd`

Preserve these output filenames unless a current repository contract clearly differs:

- `output/indonesia_model_cost_value_formulae.xlsx`
- `output/indonesia_cost_value_public_health_formulae.xlsx`
- `output/indonesia_model_cost_value_clinical_public_health_formulae.xlsx`

Do not edit the input workbooks merely to change their current flags. Do not delete or alter BCA assumptions, methods, calculations, outputs, or workbook sheets. The task is to make the code honor the workbooks as supplied and to change only what the executive slides display.

## Non-negotiable execution restriction

**Do not run, source, or indirectly invoke `03_calibration_indonesia_nelder_mead.R` or any other calibration script. Do not run the complete Model 00 driver if it would source Model 03.** Use existing calibrated/baseline artifacts, a post-calibration harness, catalogue-level tests, mocks, or static checks. If a downstream integration test cannot be run without calibration, report it as not run and give the exact reason; do not circumvent this restriction.

## Observed current workbook state to reproduce

Confirm these values yourself before editing. In the supplied/current workbooks, the clinical `Intervention_Cause_Map` excludes all links for `I_RHD_SURGERY`. The public-health map excludes all links for `I_PH_SSB_TAX`, `I_PH_ALCOHOL_TAX`, and `I_PH_ALCOHOL_POLICY`. These IDs must not appear as active standalone scenarios, active package components, selected cost rows, health results, cost-effectiveness rows, or executive-deck rows in a run using those workbooks.

Do not special-case those IDs. They are regression examples for a general rule.

## 1. Make `include_flag` binding end to end

Treat `Intervention_Cause_Map$include_flag` in each workbook as the single authoritative row-level inclusion decision.

Required semantics:

1. Normalize and validate the flag explicitly. Accept only unambiguous `0`/`1` values (including numeric or safely coercible Excel values); produce a clear validation error for missing or invalid values. Do not silently treat `NA` as included.
2. A link with `include_flag == 0` must never enter validated effect rows, effect application, coverage/exposure joins, selected cost joins, scenario components, or reported selected-intervention tables.
3. An intervention is active only if it has at least one selected, valid, runnable link. If all its links are `0`, omit the intervention entirely. If only some links are `1`, retain the intervention but apply and report only those selected links.
4. Filter costs using the active intervention IDs and the selected link/cost keys. A cause-specific cost tied only to an excluded link must not survive. A legitimate shared-count-once intervention cost may remain once when at least one selected link for that intervention is active.
5. Parent/package scenarios must contain only active runnable children. Omit a parent with no active children. Never reinsert a child because it is present in `Scenario_Hierarchy`, `Policy_Levers`, `Exposure_Targets`, `Effect_Parameters`, `Coverage`, or `Cost_Components`.
6. Baseline must remain available exactly once. Individual, family aggregate, parent-package, and joint clinical-plus-public-health scenarios must be constructed from the active catalogues, not fixed ID vectors.
7. Handle zero, one, or many active interventions per family gracefully. A family aggregate may represent one active intervention if that is the clearest stable contract; otherwise the deck must not require the aggregate. Build the joint scenario only when both families have at least one runnable intervention. Do not fail merely because a valid flag configuration leaves fewer than a historical expected count.
8. Downstream outputs must reconcile to the exact current-run scenario set. Do not allow stale RDS or workbook rows from a previous, broader run to reintroduce excluded scenarios. Compare current declared/produced scenario IDs explicitly and either filter stale extras with a conspicuous diagnostic or fail with an actionable staleness error.

Specific known issues to fix:

- In Model 00/04, `run_ssb_diabetes_mortality` currently permits a public-health link whose workbook flag is `0` to be promoted to `1`. Remove that override from the active logic. A Model 00 execution switch may choose an intervention family or analytic timing method, but it must never override a row-level `include_flag`. If the exploratory SSB mortality link is desired later, its workbook flag must be changed to `1`.
- Replace Model 04 QA expectations hard-coded to 12 interventions, 48 incidence links, and 12/13 sick-to-dead links with data-driven invariants based on selected and runnable rows. Valid exclusions must not generate misleading warnings.
- Review Model 05 for intervention-specific guards/messages, especially the hard-coded SSB→T2DM discussion. Baseline validation should be generic and driven by modeled causes or selected runnable transition targets; an excluded intervention must not trigger an intervention-specific requirement.
- Verify Model 06 applies only the effect-row objects supplied by Model 04 and that no legacy/default FAIR or public-health list adds excluded interventions.
- Make Model 07 derive/filter its scenario set from the current catalogues/Model 06 run and preserve labels/family metadata without hard-coded intervention IDs.
- Make Model 09 build all selected-intervention, cost, mortality, CVD 40q30, budget-impact, cost-effectiveness, and scenario-catalog outputs from the current active catalogues.

Create a stable `Scenario_Catalog` sheet in each of the three formula workbooks if one is not already consistently available. At minimum include:

- `scenario`
- `scenario_label`
- `intervention_family`
- `scenario_level`
- `scenario_role`
- `parent_package_id`
- active `intervention_ids`
- `n_interventions`

Include baseline metadata or document a consistent baseline rule. This sheet must describe only the current run and must be the deck's authoritative row/label/order source.

## 2. Preserve BCA in the pipeline; exclude it only from the executive slides

This distinction is non-negotiable:

- **Pipeline and Excel outputs:** preserve the full existing BCA. Model 00 must continue to source Model 08 in its normal execution path. Model 08 must continue to calculate VSL/VSLY and write its existing BCA/economic-value artifacts. Model 09 must continue to consume those artifacts and write the existing `Economic_Value` and `Benefit_Cost` sheets, formulas, assumptions, QA checks, methods, README descriptions, summaries, and calculation maps in the clinical, public-health, and combined workbooks.
- **Executive slide deck only:** do not display BCA, VSL, VSLY, monetized benefits, net benefits, or benefit-cost ratios. In the slides, display cost per death averted in market US dollars as the economic-evaluation/value-for-money metric.

Do not delete, deactivate, deprecate, bypass, simplify, or rewrite Model 08 or the BCA sections of Model 09. Changes in Models 08 and 09 are permitted only where necessary to ensure that their scenario sets follow the binding `include_flag` contract. Preserve BCA methodology, inputs, currency conventions, formulas, sheet names, and output artifacts.

The existing `Cost_Effectiveness` result must remain transparent and formula-driven in each output workbook:

```text
cost_per_death_averted_usd = cumulative discounted incremental cost in market USD
                              / cumulative undiscounted deaths averted
```

Use the workbook's cost discount rate and cost/reporting price year for this cost-effectiveness metric; do not substitute the separate BCA discounting or BCA currency basis. Clearly label the unit as `USD per death averted` and state the price year and health-system/implementation-cost perspective. Preserve undiscounted incremental cost and discounted incremental cost as audit columns. If deaths averted are zero or negative, leave the ratio undefined and report a clear dominance/status label. If costs are negative and deaths averted are positive, label the scenario cost-saving/dominant without hiding the signed cost.

Keep workbook calculations auditable: derived Excel output cells remain formulas referencing visible source/assumption cells. Do not hard-code calculated results into formula areas.

## 3. Make `reports/executive_slides.Rmd` dynamic and BCA-free

Refactor the deck so it compiles for any valid active scenario subset, including the current exclusion of RHD surgery, SSB tax, alcohol tax, and alcohol advertising policy.

Requirements:

1. Remove every BCA/economic-return dependency and claim **from the R Markdown deck only**: do not read or require `Benefit_Cost` or `Economic_Value`; do not display VSL, VSLY, benefits, net benefits, BCR, PPP international dollars, Robinson BCA citations where no longer relevant, or prose such as “returns X per dollar spent.” Update the title/subtitle, aims, executive summary, source table, results tables, combined-package slide, implications, limitations, “things to remember,” appendix, and deck validation code accordingly. Do not make corresponding removals from Models 00, 08, 09, or the output workbooks.
2. The remaining decision metrics are deaths averted, percent reduction in deaths in 2050, CVD 40q30 and its percent reduction in 2050, incremental cost, and cost per death averted in US dollars. Keep the well–sick–dead/background-death TikZ explanation.
3. Read active scenario IDs, labels, family, level, role, package membership, and display order from the output `Scenario_Catalog`/selected-intervention metadata. Do not require fixed vectors such as `clin_ids`, `ph_ids`, `clin_order`, `ph_order`, or a hard-coded scenario crosswalk.
4. Hard-coded dictionaries may be used only as optional presentation overrides after filtering to active scenario IDs; every unknown future active ID must fall back safely to its workbook label. Never require an excluded ID to exist.
5. Build the clinical and public-health intervention-description tables from active workbook rows. Filter `Selected_Interventions`, `Policy_Levers`, and package children to active IDs before display. Do not mention excluded risks/interventions in narrative text generated for the current run.
6. Show family/package/joint slides only when those scenarios exist. If a family has no active comparators, omit its results section or show a concise “no interventions selected for this family” note without failing. If no joint scenario exists, omit joint-only claims and tables. All empty-table and `rbindlist(list())` cases must be guarded.
7. Prevent overflow as row counts vary. Use dynamic table pagination or a tested row limit/continuation-slide helper rather than assuming a fixed number of interventions. Continue to avoid double-counting child and parent package results; honor the hierarchy/reporting rule where present.
8. Deck required-sheet validation must be limited to the health/cost-effectiveness contract. The deck must not require `Benefit_Cost` or `Economic_Value`, although those sheets must continue to exist in pipeline outputs. Validate scenario-set consistency against `Scenario_Catalog`, baseline availability, requested horizon years, uniqueness, finite headline health/CE metrics, and absence of excluded IDs.
9. Remove the deck's current dependency on `Economic_Value` for national population and remove annual cost per capita from the slides unless it can be sourced from a non-BCA, current-run population contract. The requested slide value-for-money metric is cost per death averted. Do not remove `Economic_Value` from Model 09 workbooks.
10. Avoid an Excel-COM-only rendering dependency for the deck. It must compile in a clean non-Windows environment. Prefer an existing stable R-side current-run results contract or the smallest additional non-BCA results contract needed by the deck. The formula workbooks, including their BCA sheets, must remain formula-driven and unchanged in purpose. Slide compilation must not depend on Excel recalculating BCA formulas or on stale cached values.
11. Keep all displayed numbers programmatically sourced. No manually transcribed result values.

## 4. Tests to add and run

Use the repository's existing test conventions if present. Keep temporary workbook variants and test outputs outside tracked production inputs/outputs. Do not run calibration.

At minimum implement and run these tests:

### A. Flag-contract tests

- Current clinical workbook: `I_RHD_SURGERY` is absent from runnable interventions and scenario IDs.
- Current public-health workbook: `I_PH_SSB_TAX`, `I_PH_ALCOHOL_TAX`, and `I_PH_ALCOHOL_POLICY` are absent from runnable interventions and scenario IDs.
- No selected/effect/cost/package row for an excluded intervention survives into the active catalogues.
- Create a temporary workbook copy and change all rows of one currently included intervention from `1` to `0`; verify the intervention disappears everywhere without editing code.
- Create a temporary mixed-link case with one cause link `0` and another `1`; verify the intervention remains, the excluded cause/effect/cause-specific cost disappears, and valid shared-count-once cost behavior is preserved.
- Verify package membership is the intersection of hierarchy children and active runnable IDs.
- Verify no execution switch can turn a workbook `0` into an active link.

### B. Scenario/output contract tests

- Expected scenario IDs are derived from the active catalogues; compare exact sets across Model 06 output, Model 07 health/CVD 40q30 outputs, Model 09 scenario catalogues, and cost-effectiveness outputs.
- Excluded IDs do not appear in any generated workbook sheet that represents active interventions/results, including selected interventions, costs, health outcomes, annual mortality, annual cost, budget impact, CVD 40q30, and cost-effectiveness.
- Baseline appears exactly once where appropriate.
- Parent and family aggregate scenarios contain only active children.
- The joint scenario, when present, contains the union of active clinical and public-health IDs and is a genuine joint model run, not an arithmetic sum.

### C. Cost-effectiveness tests

- For every comparator with deaths averted > 0, recompute `discounted incremental cost / deaths averted` independently and match `cost_per_death_averted_usd` within a documented tolerance.
- Verify costs are market USD and the price year/discount rate come from the active workbook assumptions.
- Verify zero/negative-death and negative-cost edge cases produce the correct undefined/dominant/dominated status.
- Reopen each output workbook and verify formulas/reference ranges, sheet names, scenario counts, and no `#REF!`, `#DIV/0!`, `#VALUE!`, `#NAME?`, or unintended `#N/A` in key result ranges after recalculation with an available non-interactive office engine.
- Assert that `Economic_Value` and `Benefit_Cost` sheets and the Model 08 artifacts remain present and valid in the pipeline outputs.
- Verify that the scenario sets in Model 08 and the BCA sheets match the current active scenarios and do not reintroduce excluded interventions.
- Verify the BCA formulas, methods, assumptions, and currency basis are otherwise unchanged.

### D. Deck tests

- Render `reports/executive_slides.Rmd` using the current workbooks/results without Excel COM and without calibration.
- Verify the current deck contains no RHD surgery, SSB tax, alcohol tax, or alcohol advertising policy rows/claims.
- Render against at least one temporary alternative flag subset.
- Verify no BCA/VSL/VSLY/BCR/net-benefit text or missing-sheet dependency remains.
- Inspect the rendered PDF for table overflow, blank required fields, clipped rows, broken references, and empty conditional sections.

If full Models 06–09 are too expensive to rerun, run the strongest catalogue/unit tests plus a post-calibration integration test using existing artifacts. Clearly separate **passed**, **not run**, and **blocked** checks; never claim an unexecuted test passed.

## 5. Implementation constraints

- Keep workbook assumptions as the analytic source of truth and read columns by name.
- Do not change epidemiological effect sizes, coverage paths, exposure formulas, transition mathematics, costing quantities, BCA methodology, BCA parameters, currency conventions, or calibration outputs except where required to enforce inclusion.
- Do not run Model 03.
- Do not modify unrelated scripts or data.
- Avoid duplicated scenario lists and duplicated inclusion logic. Centralize the active-scenario contract in Model 04 and consume it downstream.
- Preserve all formula-driven Excel outputs, styling, Model 08 artifacts, and BCA content. Remove BCA content only from `reports/executive_slides.Rmd` and its rendered deck.
- Fail early with concise, actionable diagnostics for invalid flags or stale scenario sets.
- Update comments and labels only where needed to describe binding intervention flags. Do not remove BCA language from pipeline README/method sheets. Remove BCA/economic-return language only from the executive R Markdown and rendered slides.

## 6. Final response required from you

After implementation, report:

1. the root causes found;
2. the files changed and why;
3. the authoritative inclusion/scenario contract after the change;
4. confirmation that BCA/Model 08 and their workbook outputs were preserved, and a description of how BCA was excluded only from the executive slides;
5. the exact tests and render commands run, with pass/fail/not-run status;
6. confirmation that Model 03 calibration was not run;
7. the current active and excluded clinical/public-health intervention IDs observed in the generated outputs;
8. any remaining limitations or manual steps.

Do not claim completion until the current excluded interventions are absent end to end, the cost-per-death-averted formulas reconcile, Model 08/BCA pipeline outputs remain valid, and the BCA-free executive deck renders successfully for the current flag configuration.
