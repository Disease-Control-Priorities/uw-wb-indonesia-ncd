# Claude Code prompt: add undiscounted cost-result slides

Modify **only** `reports/executive_slides.RMD`.

Do not modify, create, rename, format, or delete any other repository file. Do not change the model pipeline, Excel workbooks, RDS files, LaTeX support files, or existing generated outputs. If you render for testing, direct all generated files to a temporary directory outside the repository and remove them afterward.

## Objective

Add an undiscounted counterpart for **every existing slide that reports a numeric cost result**. Preserve every current discounted cost-result slide; do not replace, remove, retitle, or convert it. Each new slide should be an exact structural and visual counterpart of its discounted slide, but every monetary result on the new slide must be undiscounted.

The source of truth is the relevant output Excel workbook for each analysis. Do not hard-code or manually transcribe results. The deck must continue to compile without Excel COM or manual Excel recalculation.

## Step 1: inventory all cost-result slides before editing

Inspect the entire Rmd and identify every slide that displays a numeric monetary result in a table, figure, map, block, bullet, banner, title, subtitle, caption, or footnote. At minimum, this includes the slides/chunks containing:

- `tbl-clin-ce`
- `fig-clin-budget`
- `tbl-ph-ce`
- `tbl-combined`
- `fig-casc-2panel`
- `tbl-cascade`
- the subnational 70-70-70 annual cost-per-capita map (`fig-sub-cost`)

Also inspect the executive-summary and policy/closing slides for numeric cost-per-death or other monetary results, including dynamically generated text such as `exec-block`, `exec-bullets`, `combined-block`, `remember-*`, and `closing-banner`. Duplicate any slide that actually renders a numeric monetary result. Do not duplicate a slide merely because it explains the costing method or mentions “cost” without reporting a numeric cost result.

Keep each new undiscounted slide immediately after, or otherwise clearly paired with, its discounted counterpart. Give all new chunks and objects unique, descriptive names, preferably with an `-undisc`/`_undisc` suffix. Do not introduce duplicate knitr chunk labels.

## Step 2: construct authoritative undiscounted cost data

Create a parallel set of undiscounted cost helpers and data objects. Do not alter the values or behavior of the existing discounted helpers and slides.

For every workbook family, first inspect the actual sheet schemas and use the workbook’s stored undiscounted fields where available. Typical concepts may be named `cost_incremental`, `incremental_cost`, `cumulative_incremental_cost`, `cost_baseline`, or `cost_scenario`, but do not assume names without checking the workbook. If a required formula column is uncached, reconstruct the undiscounted value from the same authoritative `Annual_Cost` inputs used by the workbook, but omit the discount factor. Use the existing validated literal-value companion only where the current Rmd already permits it and only after the existing same-run checks.

Apply the following definitions consistently:

- **Cumulative undiscounted additional cost** = sum of annual undiscounted incremental costs over the stated years.
- **Undiscounted cost per death averted** = cumulative undiscounted incremental cost divided by the same cumulative, undiscounted deaths averted used on the current slide.
- **Average annual undiscounted additional cost per capita, 2026–2050** = cumulative undiscounted incremental cost over 2026–2050 divided by `(25 × 2026 national population)`.
- **Undiscounted additional cost per capita in 2050** = undiscounted incremental cost in 2050 divided by 2050 national population.
- **Undiscounted cumulative cost per capita through year t** = cumulative undiscounted cost through year t divided by the population in year t.
- **Subnational annual undiscounted cost per capita** = that province’s annual undiscounted incremental cost divided by that province’s population; omit the existing discount multiplier from the reconstruction.

Preserve the workbook-specific costing logic, including population denominators, coverage/implementation quantities, scenario identity, intervention inclusion flags, shared-component de-duplication, and the rule that combined packages are single joint runs rather than sums of separate scenarios.

Add fail-loud validation appropriate to the available workbook fields. Reconcile annual undiscounted intervention totals to undiscounted `Budget_Impact` or `Cost_Effectiveness` totals when those fields are stored and readable. Where an authoritative total is not cached, validate internal identities such as annual-to-cumulative sums, scenario/year completeness, finite values, and shared-cost de-duplication. Do not weaken or remove any existing validation.

## Step 3: create the undiscounted slide counterparts

For each duplicated slide:

- Preserve its layout, ordering, colors, fonts, number formatting, intervention rows, health outcomes, time horizon, scenario labels, conditional behavior, and caveats.
- Change **all and only** monetary measures to their undiscounted equivalents. Health outcomes remain identical to the discounted slide.
- Explicitly label monetary columns, panels, legends, captions, and source/method notes as **undiscounted**. Remove “discounted,” “present value,” and discount-rate wording from the new slide wherever it describes the displayed monetary results.
- Keep the price year, currency, and perspective unchanged; undiscounted does not mean a different price year.
- Recalculate all derived monetary text, including total cost, cost per death averted, average annual per-capita cost, target-year per-capita cost, and any title/bullet/banner containing a cost number.
- Do not reuse a discounted scalar such as `cost_per_death_averted`, `disc_incremental_cost`, `combo_cpda`, `ph_cpda`, or `casc_cpda` on an undiscounted slide unless you have explicitly rebuilt it from undiscounted cost data.
- Give each new slide a short, high-level ministerial takeaway title. The title must communicate the main finding, not merely say “Undiscounted version.” It should nevertheless make the undiscounted basis clear, either in the title or a concise subtitle/visual label.
- Retain the preliminary-cost/BPJS–Ministry of Health validation caveat on the new cost slides.

For `fig-casc-2panel`, the undiscounted counterpart must keep the health panel unchanged and replace the cost panel with cumulative **undiscounted** baseline and intervention health-system costs. For `tbl-cascade`, convert baseline cost, intervention cost, additional cost, and annual additional cost per capita to undiscounted values while keeping deaths unchanged.

For the clinical budget-impact counterpart, use cumulative undiscounted incremental cost by intervention and cumulative undiscounted incremental cost per capita. Apply the same shared-component counting and year-by-year reconciliation used by the current discounted figure.

For the combined table counterpart, replace discounted total cost, cost per death averted, and both per-capita measures with undiscounted values. Do the equivalent conversion for clinical and public-health tables.

## Step 4: make the two requested improvements to existing figures

These are the only requested changes to existing slide output.

### A. Prevent label/line overlap in `fig-casc-2panel`

Adjust the endpoint labels so they are easy to read and do not sit on top of either line. Use a stable, deterministic solution such as carefully chosen endpoint offsets and/or `ggrepel`, with visible leader segments where helpful, adequate right-side plotting room, and no clipping. Apply the same readable labeling approach to the new undiscounted counterpart. Do not change the underlying series.

### B. Improve and extend `fig-40q30-cascade`

Make both of the following changes to the existing `fig-40q30-cascade` slide:

1. Adjust every endpoint label so it is easy to read and does not sit on top of its own line, another line, or another label. Use deterministic `ggrepel` settings and/or explicit endpoint offsets, visible leader segments where helpful, adequate right-side plotting room, and no clipping. Preserve the underlying trajectories.
2. Add a separate trajectory for **All public-health interventions**, using the public-health workbook's own all-public-health scenario and its stored, R-reconciled annual probability-of-death-before-70 series from `CVD_40q30`. Derive the scenario ID and display label from the validated `Scenario_Catalog`/existing family-aggregate logic; do not hard-code an ID if the current code already resolves `ph_all_id`. Read the trajectory from the public-health workbook itself—do not reuse the combined workbook, add individual intervention effects, or construct it from the tobacco-tax line. Validate that its definition, years, and baseline reconcile with the other plotted series before plotting it.

The completed figure should therefore show four explicitly modelled intervention trajectories: the 70-70-70 cascade, tobacco tax, all public-health interventions, and all clinical plus public-health interventions, alongside the existing comparator context. Give the new series a distinct, accessible color and update the title/subtitle, source/method note, comments, palette, and any validation or legend/label logic so the figure accurately describes all four lines. Keep the slide concise and suitable for ministers.

### C. Fix legends on all subnational 70-70-70 choropleth maps

The continuous color-bar legend must start at the **minimum strictly positive displayed value**, rather than showing a detached `0%` or `US$0` tick. Apply this consistently to all relevant subnational 70-70-70 maps, not only the cost map.

Implement this robustly:

- calculate the lower bound programmatically from finite values greater than zero;
- retain one common scale across the two years/panels on each slide;
- ensure zero values still receive the lowest scale color rather than becoming `NA`/grey, for example by squishing them to the positive lower bound for color mapping;
- guard sensibly against the edge case where no positive value exists;
- keep the legend labels and units appropriate to each map (`%` or US$).

Do not change the mapped data or replace zeros in the underlying analytical objects; this is a legend/color-scale presentation fix only. Apply the same legend rule to the new undiscounted subnational cost map.

## Guardrails

- Modify only `reports/executive_slides.RMD`.
- Preserve all existing discounted cost-result slides and their calculations. Apart from the requested `fig-casc-2panel` label placement, `fig-40q30-cascade` extension/label placement, and subnational map legend fixes, existing rendered slides must remain unchanged.
- Do not edit unrelated code, comments, slide ordering, prose, formatting, or chunks.
- Do not add BCA, VSL, VSLY, benefit-cost ratios, or downstream cost offsets.
- Do not hard-code scenario IDs beyond those already required by the existing validated contracts.
- Do not manually type model results into the Rmd.
- Do not use the combined workbook as a shortcut for clinical or public-health results; each family must use its own workbook.
- Preserve compilation without Excel COM and without requiring users to open/save workbooks in Excel.

## Verification and completion criteria

1. Render the complete deck from a clean R session, sending the output to a temporary directory outside the repository.
2. Confirm there are no duplicate chunk labels, missing objects, non-finite displayed values, LaTeX errors, or failed workbook reconciliations.
3. Confirm every numeric cost-result slide has exactly one clearly paired undiscounted counterpart and that purely methodological slides were not unnecessarily duplicated.
4. Confirm every monetary number on an undiscounted slide comes from an undiscounted calculation; health results match its discounted counterpart.
5. Confirm the endpoint labels on both cascade two-panel figures do not overlap the lines or each other.
6. Confirm `fig-40q30-cascade` contains the four required modelled intervention trajectories, including the all-public-health scenario read from its own workbook, and that no endpoint label overlaps a line or another label.
7. Confirm every subnational 70-70-70 color bar begins at the minimum positive displayed value and zeros are colored at the low end rather than shown as missing.
8. Run `git diff -- reports/executive_slides.RMD` and verify the diff is tightly scoped.
9. Run `git status --short` and confirm that no file other than `reports/executive_slides.RMD` was modified or created.

At the end, report:

- the complete inventory of discounted cost-result slides found;
- the new undiscounted counterpart added for each one;
- the undiscounted workbook fields or reconstruction used for each analysis family;
- the validations performed and render result;
- confirmation that only `reports/executive_slides.RMD` changed.
