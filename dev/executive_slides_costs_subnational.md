# Claude Code task: update the Indonesia executive slide deck

Work in the current repository and modify **only**:

`reports/executive_slides.RMD`

Plan the work, implement it, and compile the deck successfully. Do not modify any other source, data, configuration, bibliography, workbook, or report file. Temporary compilation files may be written outside the repository or to the project's existing ignored build/output location, but the final Git diff must contain only `reports/executive_slides.RMD`.

## Objective

Correct the per-capita budget-impact measures already shown in the deck, make the cost dynamics clearer, and add an executive subnational results section for the isolated **70-30-30 to 70-70-70** analysis.

This is a minister-level deck. Every slide title must state the policy takeaway in plain language, not merely name the chart or method. Keep body text brief, nontechnical, and decision-oriented. Preserve the deck's established visual identity and its current conventions unless a targeted change below requires otherwise.

## Non-negotiable source and scope rules

1. The Excel formula-output workbooks under `output/` are the sources of truth for costs and populations. Do not hand-enter, transcribe, or hard-code a result.
2. For the three national cost-effectiveness tables, use the corresponding workbook:
   - clinical: `output/indonesia_model_cost_value_formulae.xlsx`
   - public health: `output/indonesia_cost_value_public_health_formulae.xlsx`
   - combined: `output/indonesia_model_cost_value_clinical_public_health_formulae.xlsx`
3. For the subnational section, locate and use the current isolated subnational formula workbook under the `output/70_30_30_to_70_70_70_subnational/` analysis directory. The expected filename is `indonesia_70_30_30_to_70_70_70_cost_value_formulae_subnational.xlsx`. If the repository uses the same file in a slightly different existing isolated cascade output directory, resolve it explicitly and document the resolved path in code. Never substitute the national cascade workbook.
4. Do not change any workbook, modeling script, RDS contract, input file, map file, or pipeline output.
5. Do not run calibration or the full modeling pipeline. Compile and test the slide deck against the current outputs.
6. Do not add benefit-cost/VSL/VSLY results to the executive deck.
7. Read source values by stable column names and keys, not fixed cell coordinates. Where a workbook field is formula-only and is not cached for R, reproduce the exact workbook formula in R from its authoritative workbook inputs; do not invent an alternative definition and do not require Excel COM. Validate the R calculation against any stored literal/reconciliation field available in the workbook.

## 1. Audit and correct the national per-capita formula

Review the calculations feeding chunks/tables `tbl-combined`, `tbl-ph-ce`, and `tbl-clin-ce`.

The intended discounted **average annual incremental cost per capita for 2026-2050** is:

\[
\frac{\text{cumulative discounted incremental cost over 2026-2050}}
{25 \times \text{2026 population}}.
\]

Treat the parentheses as essential. This is **not** cumulative cost divided by 25 and then multiplied by population. Confirm the definition against the formulas and labels in each respective workbook before changing the deck. The population value must come from the same respective workbook, with its population value for 2026 treated as authoritative. Do not use a separately loaded population RDS or recalculate population in the slides.

Important audit points:

- Confirm whether the cumulative workbook measure includes 2025. If it does, subtract/exclude 2025 so the numerator is exactly 2026-2050.
- Confirm that there are exactly 25 annual periods.
- Confirm the discount base and discount rate from workbook metadata/assumptions.
- Reconcile the cumulative numerator with the sum of annual discounted incremental costs for 2026-2050 to a tight numeric tolerance.
- Reconcile the 2026 population used in the denominator to the workbook's own 2026 population field.
- Fail loudly with an informative validation error if required sheets, columns, years, scenarios, or populations are absent or duplicated.
- Remove or correct stale comments and footnotes that currently describe the measure as the simple mean of annual per-capita costs using each year's population or cite `model_output_Indonesia_htncov2_aspirational.rds` as the denominator source.

## 2. Show two discounted per-capita cost measures in all three national tables

In `tbl-clin-ce`, `tbl-ph-ce`, and `tbl-combined`, replace the current undiscounted/discounted per-capita pair with exactly these two discounted columns:

1. **Average annual incremental cost per capita, 2026-2050**
   - cumulative discounted incremental cost for 2026-2050 / (25 x 2026 population), as defined above.
2. **Incremental cost per capita in 2050 at target coverage**
   - discounted incremental cost in 2050 / authoritative 2050 population from the same workbook.

Use concise, readable table headers, for example:

- `Avg. annual add'l cost/capita, 2026-50 (disc.)`
- `Add'l cost/capita in 2050 (disc.)`

Keep US-dollar formatting consistent and show enough decimal precision to avoid turning small public-health costs into zero. Preserve the other table measures unless space requires a careful layout adjustment. If necessary, modestly reduce font size or shorten headers, but maintain legibility and do not remove substantive health-impact or cost-effectiveness information without a strong reason.

For every table, the values must be drawn or exactly reconstructed from its corresponding workbook, keyed by scenario. Do not use the national combined workbook as a shortcut for the clinical or public-health tables.

## 3. Add annual incremental cost per capita to `tbl-cascade`

Add a row to `tbl-cascade` named **Annual incremental cost per capita (discounted)**.

For each displayed snapshot year, calculate:

\[
\frac{\text{discounted incremental cost in that year}}
{\text{population in that year}},
\]

using the isolated cascade workbook's authoritative annual discounted incremental cost and population fields. This is an annual, year-specific value, not a cumulative value and not the 2026-2050 average. Format it as US dollars per person and explain the definition in the slide's source/method footnote.

## 4. Add a two-panel stacked clinical budget-impact figure, 2025-2050

Add one minister-level results slide with a two-panel stacked time-series figure covering 2025-2050 and decomposed by clinical intervention. Use `output/indonesia_model_cost_value_formulae.xlsx` as the sole source of truth.

Interpret the requested two measures as:

- **Left panel:** cumulative **discounted total incremental cost** through each year, stacked by clinical intervention (use readable US$ millions/billions).
- **Right panel:** cumulative **discounted incremental cost per capita** through each year, stacked by clinical intervention. For year `t`, divide cumulative discounted incremental cost through `t` by the workbook's authoritative population in year `t`.

This interpretation avoids duplicating the same per-capita panel twice and clearly separates total budget impact from per-person budget impact. If the workbook already contains an explicitly defined cumulative discounted per-capita field that differs from this definition, stop and reconcile the discrepancy from the workbook formulas before plotting; use the workbook's documented definition and state it in the footnote.

Implementation requirements:

- Aggregate from workbook rows keyed by year, scenario, and `intervention_id` (or the exact equivalent fields present).
- Use only active, top-level clinical interventions; do not include baseline as an intervention and do not double-count shared cost components.
- Use the workbook's shared-cost/deduplication rule exactly.
- Keep the same intervention color across both panels and use the deck's clinical palette, extended accessibly if necessary.
- Order stack/legend consistently and use plain-language intervention labels already used in the deck.
- Make clear that 2025 is the base year and scale-up begins thereafter if that is what workbook metadata specifies.
- Add internal QA showing that intervention-level totals reconcile to the corresponding all-clinical scenario totals by year within tolerance.
- Give the slide a computed, high-level takeaway title reflecting the result rather than a generic title such as “Budget impact over time.”

## 5. Add a subnational 70-30-30 to 70-70-70 results section

Add the following five executive results slides using only the isolated subnational workbook and an existing Indonesia province boundary asset already in the repository. Do not download or add a new boundary file. Prefer `province_code` for spatial joins; use standardized province names only if no code exists. Validate that all 38 provinces are represented exactly once in each map-year dataset and report any unmatched or duplicated province before plotting.

The subnational workbook is expected to contain these data contracts; inspect them rather than assuming column positions:

- `Cascade_Trajectory`: `location`, `province_code`, `intervention_id`, `sex`, `year`, baseline/target/scenario/model effective coverage fields.
- `Annual_Mortality`: `location`, `province_code`, `scenario`, `year`, `cause`, modeled/base deaths and deaths averted.
- `Health_Outcomes`: province/year modeled and baseline deaths and deaths averted.
- `CVD_40q30`: `location`, `scenario`, `year`, modeled probability, baseline probability, absolute reduction, percent reduction, plus any literal R-reconciled field.
- `Annual_Cost` and/or `Budget_Impact`: province/year population, incremental and discounted incremental costs, and per-capita fields.
- `Cost_Effectiveness` and `Province_Reconciliation`: cumulative provincial impact and national/province-sum reconciliation checks.

### 5a. Provinces contribute very unevenly to national health gains

Create a Pareto-style contribution chart:

- Bars, left axis: cumulative deaths averted by province, 2025-2050.
- Line, right axis: cumulative share of the national total.
- Sort provinces from largest to smallest contribution.
- Use the workbook's official cascade health-impact scope and label it explicitly. If the official cumulative measure includes the six modeled CVD causes plus type-2 diabetes, say so; do not relabel it as CVD-only.
- Reconcile the sum of provincial deaths averted to the workbook's national or province-reconciliation total and state any documented residual difference.
- Use readable province labels; if all 38 cannot be legible horizontally, use an ordered horizontal layout, selective labeling, or another executive solution without hiding the distribution.

### 5b. Screening/effective coverage expands across every province

Create a two-panel province choropleth showing 2025 and 2040 coverage.

- Use the exact coverage measure that drives the CVD primary-care/screening cost and effect calculation in the workbook.
- If sex-specific rows exist, use the workbook's population weights or exact aggregation rule. Never take an unweighted sex mean unless the values are identical and this is verified.
- Label the metric precisely. Do not call treatment/control coverage “screening” unless the workbook definition supports that label; a safe label is “CVD primary-care effective coverage used by the model.”
- Use one common scale and legend across both years so geographic and time comparisons are valid.

### 5c. CVD deaths fall most where baseline gaps and burden are largest

Create a two-panel province choropleth of the **percentage reduction in CVD deaths** in 2030 and 2040.

- Restrict the numerator and denominator to the workbook's modeled CVD causes; do not include type-2 diabetes deaths in a measure labelled CVD deaths.
- Calculate `(baseline CVD deaths - scenario CVD deaths) / baseline CVD deaths x 100`, unless the workbook has an authoritative equivalent field, and reconcile both definitions.
- Use one common fill scale for both panels.

### 5d. Premature CVD mortality declines across provinces

Create a two-panel province choropleth of the **percentage reduction in the probability of death from CVD before age 70** in 2030 and 2040.

- Use `CVD_40q30.percent_reduction` or the exact authoritative equivalent.
- In all rendered titles, legends, and body text, use the plain-language metric name above; keep `40q30` only in internal code/object names if needed.
- Prefer the literal R-reconciled probability field where the formula result is not cached, and validate it against the workbook reconciliation status/tolerance.
- Use one common fill scale for both panels.

### 5e. The provincial financing requirement grows as coverage scales up

Create a two-panel province choropleth of **annual discounted incremental cost per capita** in 2030 and 2040.

- Use each province's discounted annual incremental cost divided by that province's population for the same year, or the workbook's authoritative equivalent per-capita field.
- This is annual, not cumulative.
- Format the legend in US dollars per person and use one common scale across both years.

## 6. Required caveat on every new subnational result slide

Every new subnational slide must include a brief, muted italic source/method footnote using the deck's `\srcmethod{}` convention. Each footnote must identify the isolated subnational workbook, define the plotted measure and years, and include a concise version of this validation caveat:

> Provincial baseline cascade estimates and province-adjusted costs are preliminary and require validation by Indonesia's Ministry of Health and BPJS before policy use.

Also state, concisely and professionally, the heterogeneity assumption:

> Provincial CVD treatment coverage is a crude anchor derived from observed CVD mortality and the modeled relationship between coverage differences and intervention effects; extreme values were capped. Provincial diabetes treatment coverage is adjusted in proportion to this CVD coverage anchor.

Do not imply that these are observed provincial cascade measurements. Correct spelling and use **baseline**, **Ministry**, **per capita**, and **time series** consistently.

## 7. Footnotes and executive language

- Every newly added results slide must have a source/method footnote.
- All national cost slides must retain the existing general caveat that costing results require further validation with BPJS and Ministry of Health data.
- The new subnational slides must use the stronger province-specific caveat above.
- Keep footnotes compact enough to remain legible in 16:9 Beamer output.
- Use high-level policy takeaway titles and executive body text throughout all slides touched by this task. Replace any newly exposed technical label with plain language.
- Do not render “40q30” in slide-facing text; render “Probability of death from CVD before age 70.”

## 8. Validation and successful compilation

Before editing, inspect the existing helper functions, object names, table builders, workbook loaders, and current cascade section. Reuse and minimally extend them rather than creating a parallel architecture.

Add compact, informative validation checks inside `reports/executive_slides.RMD` for:

- required files, sheets, and columns;
- scenario uniqueness and expected year coverage;
- exactly 25 years for 2026-2050;
- annual-to-cumulative discounted-cost reconciliation;
- authoritative 2026 and 2050 population denominators;
- no shared-cost double counting;
- national totals versus intervention sums;
- province totals versus the workbook's reconciliation output;
- 38 unique provinces and complete spatial joins;
- finite values and sensible ranges for percentages and per-capita costs;
- common scales across paired maps.

Then compile using the repository's established command/workflow for `reports/executive_slides.RMD`. If none is documented, use an appropriate direct render command such as:

```r
rmarkdown::render("reports/executive_slides.RMD", clean = TRUE)
```

Iterate until compilation succeeds. Inspect the PDF visually or render representative pages to images and check at minimum:

- no clipped titles, tables, legends, maps, labels, or footnotes;
- all maps show the complete Indonesian province geography available in the repository;
- paired maps use identical legends/scales;
- the Pareto line and right axis are correctly aligned;
- the two stacked cost panels reconcile and remain readable;
- no literal `NA`, `NaN`, `Inf`, raw underscores, or unescaped LaTeX characters appear;
- no slide is overcrowded.

If layout is crowded, shorten prose, simplify labels, or split content only where necessary while retaining all requested results. Do not solve layout problems by removing source/method notes or validation caveats.

Finally run:

```bash
git diff --check
git status --short
```

Confirm that the only modified tracked source file is `reports/executive_slides.RMD`.

## Final response

Report succinctly:

1. what was changed;
2. the exact formulas used for both national per-capita cost columns;
3. the workbook paths and sheets used;
4. the compilation command and whether it succeeded;
5. the output PDF path;
6. key reconciliation/QA results, including the 38-province spatial join;
7. confirmation that no other source file was modified.
