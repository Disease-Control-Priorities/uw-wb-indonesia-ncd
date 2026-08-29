# Claude Code task: add annual population and per-capita costs to Model 09 outputs

Work in the existing repository and make a minimal, auditable change to the Indonesia CVD FAIR Choices costing pipeline.

## Objective

Modify the existing Model 09 costing script under `code/cvd-fair-choices/` so that every cost workbook it produces includes the national population denominator for each analysis year and annual cost-per-capita results. Then update the existing executive slide deck under `reports/` so that its per-capita cost metric is read from the Model 09 output, rather than recalculated in the slide code with a separate population denominator.

The authoritative population source is:

`out_model/model_output_Indonesia_htncov2_aspirational.rds`

## Strict scope

You may modify only:

1. The existing Model 09 script in `code/cvd-fair-choices/` (normally `09_cost_value.R`; use the repository's exact current filename).
2. `reports/executive_slides.Rmd` (the current deck corresponding to the supplied `executive_slides(4).Rmd`). If the repository differs only in filename case, use the existing file; do not create a duplicate.

Do not modify any other source code, configuration, input, report, or documentation file. Generated Model 09 output workbooks/RDS artifacts may be regenerated as part of testing, but do not hand-edit them. Do not refactor unrelated code or reformat untouched sections.

**Apply a surgical, line-level edit. Within the two authorized files, change only lines required to: (a) load/validate the requested annual population, (b) calculate/export annual per-capita costs, (c) add directly related QA/metadata/styles, and (d) replace the deck's existing per-capita calculation and its directly related dependency/comments/footnotes. Do not rewrite, reorder, rename, restyle, clean up, optimize, or otherwise touch any line unrelated to per-capita cost calculation. Preserve whitespace and formatting outside the smallest necessary edit hunks.**

**Do not run Model 03 or any calibration code.** Do not modify Models 00–08. In particular, do not modify the attached/current `07_output_dalys.R` or `08_economic_value_calculation.R`.

## Required workflow

### 1. Inspect and plan before editing

- Read the current Model 09 script, the executive deck, and the structure of `out_model/model_output_Indonesia_htncov2_aspirational.rds`.
- Identify every workbook produced by Model 09 in the current configuration, including all applicable variants:
  - clinical R-value workbook;
  - clinical formula workbook;
  - public-health formula workbook;
  - combined clinical + public-health formula workbook;
  - the dedicated 70-30-30 to 70-70-70 cascade workbook(s), if produced through the same Model 09 pathway.
- Identify every annual cost sheet or annual cost summary in those workbooks. At minimum inspect `Annual_Cost`, `Budget_Impact`, and any summary sheet that exposes annual cost or per-capita cost.
- In `reports/executive_slides.Rmd`, inspect the known per-capita pathway around:
  - the setup paths `wb_comb`, `wb_clin`, `wb_ph`, and `dalys_file`;
  - `.req_files`;
  - `pc_undisc()` and `pc_disc()` in `results-builders`;
  - the `percapita` chunk, which currently builds `percap_den = 25 * pop_2026` from `dt_output_dalys.rds`;
  - the per-capita source/method notes on the clinical, public-health, and combined results slides.
- Do not inspect or revise other deck sections except where an exact per-capita formula/note is present.
- Before editing, print a concise implementation plan naming the two source files to be changed, the workbook builders affected, the new columns, the population aggregation rule, and the tests you will run.

### 2. Construct one authoritative annual population table in Model 09

Load `out_model/model_output_Indonesia_htncov2_aspirational.rds` in Model 09 and inspect its actual object structure and column names; do not guess them.

Build exactly one validated Indonesia national population denominator per analysis year, using the population field in that RDS. The aggregation must:

- use the national Indonesia rows only if the file contains multiple geographic levels;
- cover the active analysis horizon, normally 2025–2050;
- de-duplicate population across cause, intervention, model-state, or scenario dimensions before summing;
- sum the unique age-by-sex population cells for each year (handle any terminal/open age group consistently with the model output);
- never sum the same population once per cause or once per scenario;
- produce exactly one finite, strictly positive population value per required year;
- fail early with a clear error if a required year is absent, duplicated inconsistently, non-finite, non-positive, or cannot be uniquely resolved.

Use an unambiguous name such as `national_population` or `population_total`. Do not call the national denominator simply `pop` in exported sheets where `pop_scenario`, `q_scenario`, population in need, or eligible population also appear.

If the RDS contains multiple scenario-like values, verify that population is invariant after de-duplication. Use the `htncov2_aspirational` population series requested here; do not silently substitute UNWPP data, `Economic_Value$population`, a slide-side denominator, or another output file. An optional reconciliation check against other population fields is welcome, but it must not change the authoritative source.

Resolve the RDS path robustly relative to the repository/current pipeline paths already used by Model 09. Do not add hard-coded machine-specific absolute paths.

### 3. Add population and annual per-capita cost fields to all relevant workbooks

Propagate the same annual national denominator consistently through every applicable Model 09 workbook builder: clinical R-value, clinical formula, public health, combined, and cascade outputs.

At minimum, each `Budget_Impact` row (one scenario-year) must expose these columns:

- `national_population`
- `baseline_cost_per_capita`
- `scenario_cost_per_capita`
- `incremental_cost_per_capita`
- `disc_incremental_cost_per_capita`

Definitions for year `t`:

```text
baseline_cost_per_capita(t)         = baseline_cost(t) / national_population(t)
scenario_cost_per_capita(t)         = scenario_cost(t) / national_population(t)
incremental_cost_per_capita(t)      = incremental_cost(t) / national_population(t)
disc_incremental_cost_per_capita(t) = disc_incremental_cost(t) / national_population(t)
```

These are annual costs divided by that same year's population. Do not divide a cumulative multi-year cost by one year's population. Do not divide again by 25 or by the number of analysis years.

Also add `national_population` and the corresponding row-level annual cost-per-capita fields to `Annual_Cost` wherever that sheet is produced, while preserving the distinction between:

- national population denominator;
- eligible/model quantity (`q_scenario`, `q_baseline`, `pop_scenario`, etc.);
- population in need (`pin_scenario`, `pin_baseline`).

For component rows, calculate the per-capita versions of the existing annual baseline, scenario, incremental, and discounted incremental cost columns using `national_population` for that year. Do not use the component's eligible population or PIN as the denominator.

If a workbook summary contains an explicitly annual cost result, add or update the corresponding annual per-capita field there as well. Do not invent a per-capita version of a cumulative/PV total unless it is explicitly and correctly labelled; annual per-capita values belong primarily in `Budget_Impact` and `Annual_Cost`.

For formula workbooks:

- retain the established visual conventions: R-source cells grey, Excel formula cells light blue, headers/styles/filters/freeze panes preserved;
- write `national_population` and the four per-capita columns in `Budget_Impact` as populated numeric R-source values so `openxlsx::read.xlsx()` can read them immediately during a clean deck render without opening/recalculating the workbook in Excel. Style these literal R-source columns grey. The values must be calculated in Model 09 from the same R `bi` numerators and requested annual population table;
- if useful for auditability, add separate guarded Excel reconciliation formulas/QA cells, but do not replace the readable numeric per-capita output fields with formula-only cells that have no cached values;
- in `Annual_Cost`, populated R-source values are also acceptable and preferred where they avoid uncached-formula problems; preserve the workbook's existing formula/audit conventions for all pre-existing columns;
- derive formula column references by column name or otherwise update every downstream reference safely after inserting columns—do not rely on stale hard-coded letters;
- update formulas in `Cost_Effectiveness`, `Benefit_Cost`, `Summary`, QA, and calculation maps only where column shifts require it; do not change their substantive definitions;
- update relevant README/method/source text inside the generated workbooks to state that national annual population comes from `out_model/model_output_Indonesia_htncov2_aspirational.rds` and annual cost per capita equals annual cost divided by same-year national population;
- apply an appropriate USD-per-person format (for example `#,##0.00` or `$#,##0.00`, consistent with existing workbook conventions), not an integer population format;
- preserve formula recalculation-on-open behavior.

For R-value workbooks, export populated numeric values—not formulas or blank placeholders—using the same definitions.

Add QA checks in Model 09, within the existing QA framework, for:

- one valid population denominator for every analysis year;
- identical `national_population` for all scenarios in the same year;
- `incremental_cost_per_capita * national_population == incremental_cost` within a scale-aware numerical tolerance for every scenario-year;
- equivalent reconciliation for baseline, scenario, and discounted incremental per-capita fields;
- no missing/non-finite per-capita values when the corresponding numerator is finite and population is positive.

Do not alter intervention selection, costing inputs, scale-up paths, mortality/DALY calculations, discounting, BCA logic, or scenario definitions.

### 4. Update the executive slide deck

Modify only `reports/executive_slides.Rmd`, and only the smallest per-capita-related hunks described here. The supplied deck establishes the current implementation precisely:

- it loads `output/09_deck_results.rds` for cumulative cost metrics;
- it declares `dalys_file <- output/dt_output_dalys.rds` solely as the per-capita denominator source;
- its `percapita` chunk reads the 2026 baseline population, builds `percap_den <- 25 * pop_2026`, and calculates cumulative cost divided by that fixed denominator;
- `pc_undisc()`, `pc_disc()`, and `percap_vals()` use that denominator;
- the clinical, public-health, and combined notes describe the `25 x 2026 population` method.

Replace exactly this per-capita pathway. Do not alter other calculations or presentation code.

- Read each family's populated annual per-capita values from its existing workbook `Budget_Impact` sheet:
  - clinical: `wb_clin`;
  - public health: `wb_ph`;
  - combined: `wb_comb`.
- Validate the required literal columns: `scenario`, `year`, `incremental_cost_per_capita`, and `disc_incremental_cost_per_capita` (and `national_population` for structural validation only). Fail clearly if they are absent, duplicated by scenario-year, non-finite over required years, or not readable as numeric values.
- Preserve the slide's existing definition as a **simple average annual additional cost per capita over 2026–2050**, but implement it as:

```text
mean(incremental_cost_per_capita[year %in% 2026:2050])
mean(disc_incremental_cost_per_capita[year %in% 2026:2050])
```

  for the requested scenario and matching workbook family. Do not include 2025 merely because its incremental cost is zero; retain the current 25-year 2026–2050 display horizon.
- The deck must not load or independently aggregate a population denominator and must not compute `cost / population`, `cumulative cost / population`, or `cost / 25 / population` for this metric.
- Remove `dalys_file` and `model_demography` from the deck's setup/required-file validation only if inspection confirms that `dt_output_dalys.rds` is no longer used anywhere else in the deck. If it has another use, retain the path for that other use but remove all use of it from per-capita costing. Do not remove any unrelated dependency.
- Update only `pc_undisc()`, `pc_disc()`, `percap_vals()`/`percap_callout()` and the immediate per-capita data-loading/validation code as necessary. Keep their existing public behavior and formatting (`fmt_usd_pc`) so downstream table chunks remain untouched wherever possible.
- Keep the existing clinical, public-health, and combined table definitions, columns, labels, layout, colors, row limits, scenario selection, and all non-per-capita cells exactly unchanged.
- Do not add an alternate/fallback denominator. Fail clearly if the expected workbook column is missing.
- Update only the per-capita clauses of the setup provenance comment and the three analytical-slide source/method notes. State that the two displayed columns are simple means of Model 09 `Budget_Impact` annual per-capita values for 2026–2050 and that Model 09 uses each year's Indonesia population from `out_model/model_output_Indonesia_htncov2_aspirational.rds`. Remove the obsolete `25 x 2026 population` language. Preserve every unrelated sentence in those comments/notes.
- Preserve the deck's existing design, ordering, narrative, tables, charts, and all unrelated results.

Unless strictly necessary to keep the per-capita method description accurate, do **not** change the cascade section, the costing schematic, the appendix equations, health results, CVD probability calculations, cost per death averted, BCA, figures, titles, policy messages, references, YAML/header, or LaTeX styling. The cascade workbook should still receive the new Model 09 population/per-capita columns, but the deck's cascade slides do not currently display per-capita cost and therefore must not be edited.

Because formula-only Excel cells may not have cached values until recalculated, inspect how the deck currently imports outputs. Ensure the selected Model 09 output exposes a populated value that the R Markdown render can read in a clean run. Do not solve this by recreating the population division in the slide deck. If necessary, have Model 09 export the per-capita values as R-populated numeric source fields while retaining the workbook's formula/audit design.

### 5. Execute and test with current outputs

Use the current existing outputs and the repository's normal run controls. Do not run Model 03/calibration. Run the smallest existing command that safely executes Model 09 with its required already-generated upstream objects; if the normal runner would invoke Model 03, use the repository's supported skip/reuse-current-output mechanism or source only the necessary post-calibration stages. Do not create a new runner or modify another file.

Test all workbook variants that the current outputs allow. If a variant cannot be regenerated because its current scenario inputs are unavailable, say so explicitly, but still verify its builder statically and do not fabricate inputs.

Required tests:

1. Model 09 completes without error using current upstream outputs and without executing Model 03.
2. The executive deck renders successfully using the generated Model 09 output.
3. Every generated workbook opens and contains the required population/per-capita columns in all relevant cost sheets.
4. All required analysis years appear once per scenario in `Budget_Impact`, and population is positive and identical across scenarios within year.
5. Independently recompute a sample of at least three scenario-year rows from each generated workbook and confirm the exported per-capita results within tolerance; include the first year, an intermediate year, and 2050 where available.
6. Formula workbooks contain valid formulas/references after the new columns are inserted; no `#REF!`, `#DIV/0!`, broken named references, or shifted downstream formulas.
7. Existing headline totals remain unchanged apart from the newly added columns: annual and cumulative costs, discounted costs, deaths/DALYs averted, cost per death averted, BCA values, and CVD premature-mortality metrics must reconcile to the pre-change/current outputs within tolerance.
8. Existing QA checks still pass, and the new population/per-capita QA checks pass.
9. Search the executive deck and confirm no independent population denominator or cost-per-population division remains for the reported per-capita cost metric.
10. Confirm that the deck's displayed values equal an independent mean of `Budget_Impact$incremental_cost_per_capita` and `Budget_Impact$disc_incremental_cost_per_capita` over 2026–2050 for sampled clinical, public-health, and combined scenarios.
11. Inspect `git diff --word-diff=plain` for both authorized files and revert any edit hunk not directly required for annual population/per-capita calculation, export, validation, or the three related deck notes.
12. Confirm with `git diff --name-only` and `git diff --check` that only the two authorized source files changed. Generated outputs should remain untracked/ignored or be clearly reported separately; do not commit them unless the repository already tracks those generated artifacts.

Use temporary one-line inspection commands for tests; do not add test scripts or other files.

## Final response

Report:

- the exact two source files modified;
- the population extraction/de-duplication rule actually implemented;
- the new columns added to each workbook/sheet;
- how the executive deck now reads the metric;
- the exact commands run, explicitly confirming Model 03 was not run;
- which workbook variants and slide render were tested and their results;
- QA/reconciliation results and confirmation that pre-existing headline outputs did not change;
- any workbook variant that could not be executed with the current outputs;
- `git diff --name-only` status.

Do not make unrelated improvements, do not commit, and do not modify any additional source file. A technically correct per-capita implementation with unrelated cleanup or edits is out of scope and must be reverted before reporting completion.
