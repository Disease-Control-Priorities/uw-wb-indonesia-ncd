# Claude Code prompt: connect Models 07–09 and implement a Reference-Case-consistent BCA

Work in this repository, primarily in `code/cvd-fair-choices/`.

## Objective

Repair the disconnected Excel-input → Model 00 → Models 07–09 pipeline so that:

1. `07_output_dalys.R` produces a stable, auditable health-outcome contract for every active clinical and public-health scenario, including deaths, life-years, YLLs, YLDs, DALYs, and their incremental values versus baseline;
2. `08_economic_value_calculation.R` consumes Model 07's contract, applies the **Reference Case Guidelines for Benefit-Cost Analysis in Global Health and Development**, and produces the annual source data Model 09 needs for both intervention families; and
3. `09_cost_value.R` consumes Models 07 and 08 and writes formula-driven economic-value and benefit-cost results into the user-facing Excel outputs.

Reference:

- Robinson LA, Hammitt JK, Cecchini M, et al. *Reference Case Guidelines for Benefit-Cost Analysis in Global Health and Development*. 2019. https://repository.chds.hsph.harvard.edu/repository/4005/
- Direct PDF: https://media.repository.chds.hsph.harvard.edu/static/filer_public/16/3c/163c0d02-1837-4e9b-b88b-d646bbea857c/2019_robinson_ref_case_guidelin_bca_proj_r_126.pdf

Implementation baseline files:

- `code/cvd-fair-choices/00_run_model_cvd_fair.R`
- `code/cvd-fair-choices/07_output_dalys.R`
- `code/cvd-fair-choices/08_economic_value_calculation.R`
- `code/cvd-fair-choices/09_cost_value.R`
- `data/indonesia_model_inputs.xlsx`
- `data/indonesia_model_inputs_public_health_updated_mortality.xlsx`
- current clinical and public-health cost/value Excel outputs.

## Ground yourself in the actual code before editing

These are real anchors in the current files — use them so you extend the existing machinery rather than reinventing it:

- **Model 00** already sources 07 → 08 → 09 → 10 in sequence and holds the single source of truth for execution switches (`run_clinical_interventions`, `run_public_health_interventions`, `run_cost_value`, `baseline_scenario_id`), paths (`wd`, `wd_outp`, `wd_data`, `model_inputs_file`, `public_health_inputs_file`, the two `*_formulae` output paths), and the central `cause_map` / age grid. Analytic assumptions live in the workbooks, never in Model 00.
- **Model 07** currently hard-codes `int_year <- 2026`, derives GBD disability weights as `val.yld / val.prevalence` by cause, builds a WPP life-expectancy table `lt_interp`, remaps Model 06 scenario IDs to legacy labels (`b.a.u`, `Antihypertensive Therapy`, `Sodium Reduction`, …), computes `yld`/`yll`/`daly`, and saves only `dt_output_dalys.rds`.
- **Model 08** independently reloads Model 06 outputs (`model_output_*.rds`), *rebuilds its own `lt_interp` from the raw WPP file* (the `if (!exists("lt_interp")) { … read_excel(LT_FILE) … }` block), and hard-codes VSL/discount constants in R: `US_VSL_RATIO <- 160`, `VSL_ELAST_HIC <- 0.8`, `VSL_ELAST_LMIC <- 1.2`, `VSL_ELAST_LOW <- 1.0`, `VSL_ELAST_HIGH <- 1.5`, `VSL_RATIO_FLOOR <- 20`, `ADULT_MIN_AGE <- 20L`, `DISC_RATES <- c(r1=.01, r3=.03, r5=.05)`, `BASE_YEAR <- 2026L`, `SUMMARY_YEARS <- c(2026,2030,2040,2050)`. It labels the 0.8/1.2 differential case `e1_2` as the "primary/reference case," writes `08_vsl_results.rds/.csv` and several summary tables, and projects GNI forward with SSP2 GDP growth.
- **Model 09** header explicitly says *"Does NOT depend on Model 07"* and *"Model 08 economic value (reused, when reconcilable)."* It loads Model 08 best-effort via `econ_value <- tryCatch({ if (!file.exists(vsl_file)) … })` on `08_vsl_results.rds`, and only populates `Economic_Value` if the file exists and its `scenario` values reconcile. It already builds fully formula-driven sheets (`Annual_Mortality`, `Annual_Cost`, `Budget_Impact`, `Cost_Effectiveness`, `QA_Checks`) off an editable `Calculation_Assumptions` sheet, using helper functions `frows()`, `idx_ce()`, `int2col()`, `style_sheet()` and style objects `st_hdr`, `st_rsrc` (grey R-source inputs), `st_formula` (live formulas), plus conditional-format styles `cf_pass`/`cf_fail`/`cf_rev`. Section **11.13 Economic_Value** currently writes pasted R VSL/VSLY values with only five supplementary formula columns (`incremental_cost`, `disc_incremental_cost`, `deaths_averted`, `benefit_cost_ratio_supp` computed as `economic_value_e1_2 / incremental_cost`, `net_benefit_supp_usd`). The public-health workbook's `Economic_Value` sheet is effectively a placeholder note. **Reuse these helpers, styles, and the `Calculation_Assumptions` anchor pattern for all new formulas.**
- The two input workbooks' `Assumptions` sheets use the schema `parameter_id, value, unit, base_case, lower_bound, upper_bound, description, source`. Match it exactly for any new row.

## Strict scope

- Refactor only Models **07, 08, and 09** and, only if additional analytic parameters are required, the existing `Assumptions` sheets of the two input workbooks.
- Do **not** modify Models 01–06 or 10, intervention effects, scenario construction, Markov transitions, costs, calibration, or unrelated report code.
- Do not modify Model 00 unless a minimal execution-only plumbing change is demonstrably necessary. Never place analytic assumptions in Model 00.
- Do not add new input sheets or columns if named rows in the existing `Assumptions` sheets suffice.
- Do not touch unnecessary lines. No broad renaming, formatting rewrites, or cleanup. Preserve existing output filenames, scenario IDs, styles, sheet order, and downstream contracts wherever possible.
- Before changing any existing Model 08 output filename or column, search the repository for downstream consumers (e.g. `scenarios/scenarios_aim1/aim1_report.Rmd`, `aim1_executive_slides.Rmd`). Preserve used fields through compatibility aliases rather than editing unrelated consumers.
- Do not create a parallel, competing valuation pipeline.

## Problems in the current implementation that must be fixed

- Model 07 hard-codes the intervention year, remaps scenario IDs to legacy labels, computes scenario DALYs but not a complete baseline-difference contract, and saves only `dt_output_dalys.rds`.
- Model 08 independently rereads Model 06, duplicates life-expectancy processing, hard-codes VSL and discount assumptions, and may not preserve the exact scenario IDs used by Models 04, 06, and 09.
- Model 09 explicitly states it does not depend on Model 07; reuses Model 08 only when scenario names happen to reconcile; computes one BCR against **undiscounted** incremental cost; and leaves the public-health `Economic_Value` sheet as a note.
- Model 08's label treating the 0.8/1.2 differential elasticity as the 2019 Reference Case primary estimate is **not** consistent with the Guidelines' preferred LMIC default (below).
- Benefits are GNI-per-capita at **PPP** while costs are labeled **USD**. A BCR must not silently divide PPP-based benefits by market-exchange-rate costs, or mix price years.

## 1. Refactor `07_output_dalys.R` into the health-outcome source of truth

Keep the existing purpose and outputs where possible, but make the contract explicit and sufficient for Models 08 and 09.

### Inputs and scenario handling

- Use the current Model 06 in-memory results when available, with the same safe on-disk fallback pattern Model 09 uses.
- Preserve the exact Model 06 `scenario` IDs. Remove the ad hoc relabeling (`b.a.u`, `Antihypertensive Therapy`, `Sodium Reduction`, …). Human-readable labels may be joined separately from the Model 04 catalogues into a `scenario_label` column.
- Use `baseline_scenario_id` from Model 00 / Model 04, not a hard-coded baseline name.
- Include every scenario produced for the active values of `run_clinical_interventions` and `run_public_health_interventions`, including joint parent-package and combined public-health scenarios.
- Derive analysis years from the active input-workbook assumptions / current-run catalogues; do not hard-code `int_year <- 2026`.

### Life expectancy and disability weights

- Load WPP life expectancy once in Model 07 and make Model 07 the downstream source of life expectancy. Save the resulting life-expectancy lookup (and, if useful, the disability-weight table) as documented objects so Model 08 consumes them instead of rebuilding them.
- Build a documented single-year-age LE lookup for all model ages, including the open-ended 95+ treatment. Use a transparent interpolation/mapping rule applied consistently; do not leave non-five-year ages unmatched.
- Retain the existing GBD disability-weight method (ratio of YLD rate to prevalence rate, by cause) unless a correction is essential; document it. Do not silently replace the epidemiologic method.
- Fail with a consolidated diagnostic if required cause mappings, disability weights, or LE values are missing after permitted interpolation/fallbacks.

### Required output grain and variables

Extend the existing `dt_output_dalys.rds` contract rather than creating redundant outcome files unless a separate lookup is essential. At minimum retain or add:

- keys: `location`, `scenario`, `scenario_label`, `intervention_family`, `year`, `age`, `sex`, `cause`;
- source outcomes: `population`, `well`, `sick`, `newcases`, `deaths`, `disability_weight`, `remaining_life_expectancy`, `yld`, `yll`, `daly`;
- correctly paired baseline values at the same location × year × age × sex × cause;
- incremental outcomes: `cases_averted`, `deaths_averted`, `yld_averted`, `yll_averted`, `dalys_averted`, `life_years_gained`;
- `life_years_gained` must be based on the age distribution of deaths averted and the same remaining-LE lookup used in the output.

Use the sign convention **baseline minus intervention**, so a beneficial intervention normally has positive averted outcomes. Do not truncate real negative incremental outcomes to zero; retain and flag them.

Add QA for unique keys, complete baseline pairing, missing LE/DW, `daly = yld + yll` reconciliation, and aggregation of age-specific deaths averted to scenario-year totals.

## 2. Refactor `08_economic_value_calculation.R` to consume Model 07

### Pipeline contract

- Model 08 must read the Model 07 health-outcome object and must **not** independently recompute deaths averted or reread Model 06 for valuation. Replace the `if (!exists("lt_interp")) { … read_excel(LT_FILE) … }` rebuild with a load of Model 07's LE table (raw-file rebuild only as an explicit, warned fallback).
- Model 08 must use Model 07's life-expectancy and life-years-gained variables. Remove duplicated, inconsistent LE calculations.
- Preserve exact scenario IDs and populate both clinical and public-health scenarios.
- Continue writing `output/08_vsl_results.rds` (Model 09 expects it). Preserve other Model 08 files/columns with downstream consumers (e.g. the `08_vsl_summary_table_e1_2_primary.rds` / `_e1_5_sensitivity` artefacts feeding the Aim-1 slides), adding compatibility aliases when necessary.
- The primary Model 08 → Model 09 contract should be annual, at least location × year × scenario, containing the source fields needed for Excel formulas: deaths averted, life-years gained, average adult age, remaining LE at average adult age, population, Indonesia GNI per capita, U.S. GNI per capita, total national GNI, currency/price basis, and scenario metadata.
- Model 08 may compute parallel R values for QA, but the final user-facing Excel calculations must be **formulas**, not pasted R results.

### Correct the VSL cases to match the 2019 Guidelines

Implement named, auditable valuation cases (source these numbers from workbook `Assumptions` rows per §3, not R constants):

1. **Preferred LMIC default** — transfer from a U.S. VSL/GNI-per-capita ratio of 160 using income elasticity **1.5**, with a floor of **20 × target-country GNI per capita**:
   `VSL_target = MAX(160 × GNIpc_US × (GNIpc_target / GNIpc_US)^1.5, 20 × GNIpc_target)`.
2. **Standardized sensitivity 1** — `VSL_target = 100 × GNIpc_target`.
3. **Standardized sensitivity 2** — `VSL_target = 160 × GNIpc_target`.

Use GNI per capita at PPP for the transfer and update VSL over time with projected real GNI-per-capita growth (the existing SSP2 projection). Label each case clearly. Do **not** describe the current 0.8/1.2 differential-elasticity case (`e1_2`) as the Guidelines' preferred default. If downstream work requires it, retain it only as a clearly labeled legacy/additional sensitivity and do not use it for the Reference Case headline BCR.

If a defensible Indonesia-specific willingness-to-pay estimate already exists in the repo/input workbook, allow it as an optional context-specific preferred case, but do not invent one.

### VSLY

- Treat constant VSLY as an age/LE sensitivity or alternative presentation, not automatically as a co-primary welfare estimate.
- For each VSL case: `VSLY = VSL / undiscounted remaining LE at the average age of the adult population`.
- Define the adult age range using named workbook assumptions; ideally reflect labor-force ages per the Guidelines. If unavailable, add explicit editable min/max adult ages rather than retaining the undocumented `age >= 20` rule.
- Apply VSLY to Model 07's age-specific `life_years_gained`. Do not create an algebraic identity by using the same average LE in both the VSLY denominator and the life-years-gained numerator.
- Keep within-lifetime treatment and calendar-year discounting conceptually separate; document the choice. Avoid double discounting.

### Discounting and monetary consistency

- Report undiscounted annual benefit streams.
- Use one explicit BCA base year and a context-specific primary discount rate for both benefits and costs.
- Include the Guidelines' standardized discount-rate sensitivities: **3%** and **twice projected near-term real GDP-per-capita growth**. Retain 1%/5% only as optional additional sensitivities if already needed downstream; do not substitute them for the specified Reference Case sensitivities.
- Express benefits and costs in the same real price year and monetary basis before computing BCR or net benefits. Add an explicit conversion factor/source if costs need conversion from market-exchange-rate USD to PPP international dollars. **Never silently assume the factor is 1.**

## 3. Put any new analytic parameters in the Excel input workbooks — reuse first, then ASK

First reuse equivalent existing assumption rows. If parameters are missing, **stop and ask before adding rows**; on approval, add only the required named rows to the existing `Assumptions` sheets in **both** `data/indonesia_model_inputs.xlsx` and `data/indonesia_model_inputs_public_health_updated_mortality.xlsx`, matching the exact schema `parameter_id, value, unit, base_case, lower_bound, upper_bound, description, source`.

Likely required parameters (stable snake_case IDs; do not duplicate an equivalent existing row):

- `bca_base_year`;
- `bca_discount_rate_primary`;
- `bca_discount_rate_sensitivity_3pct`;
- `bca_discount_rate_sensitivity_2x_gdp_pc_growth` (or the inputs to derive it);
- `vsl_us_gni_ratio` = 160;
- `vsl_income_elasticity_preferred` = 1.5;
- `vsl_floor_gni_multiple` = 20;
- `vsl_sensitivity_gni_multiple_100` = 100;
- `vsl_sensitivity_gni_multiple_160` = 160;
- `vsly_adult_min_age` and `vsly_adult_max_age`;
- `bca_currency_basis`, `bca_price_year`, and any necessary `cost_to_bca_currency_factor` with a traceable source;
- `bca_standing`/perspective and a scope label indicating whether this is a full societal BCA or a mortality-benefit/implementation-cost partial BCA.

Populate `unit`, `base_case`, bounds where meaningful, `description`, and `source` (cite the CHDS/Harvard Reference Case Guidelines and/or Robinson et al. 2019). Match existing workbook styling, input-cell colors, validation, named ranges, QA ranges, and README documentation; extend existing QA formulas/ranges to cover the new rows. Do **not** add hard-coded copies of these values to Models 00, 07, 08, or 09.

If the two workbooks carry conflicting BCA assumptions in a both-families run, stop with a clear consolidated error rather than choosing one silently.

## 4. Refactor `09_cost_value.R` to consume Models 07 and 08

Make Model 08 (and, for health outcomes, Model 07) a **required** input, not a best-effort `tryCatch`/`file.exists` reuse. When `run_cost_value` runs and the outputs are missing or unreconcilable, fail with a clear, actionable, consolidated diagnostic — not a silent `NULL` with a vague note. Update the Model 09 header comment (currently "Does NOT depend on Model 07… reused when reconcilable") to state the new required contract.

### User-facing Excel outputs

The authoritative user-facing decision workbooks are the clinical `indonesia_model_cost_value.xlsx` output and `indonesia_cost_value_public_health_formulae.xlsx`. Model 09 also creates a clinical `_formulae.xlsx` companion — search for downstream use and avoid leaving two divergent clinical workbooks; if the companion must be retained, keep the BCA/economic-value sheets synchronized in both, otherwise make the authoritative clinical output formula-driven without adding another duplicate.

In every user-facing workbook, input/source observations may be written as values (styled `st_rsrc`), but **every derived calculation must be an Excel formula** (`writeFormula`) built with the existing helpers (`frows()`, `idx_ce()`, `int2col()`, `style_sheet()`) and anchored to `Calculation_Assumptions`. This includes annual and present-value benefits, VSL, VSL/GNI ratios, VSLY, discount factors, total GNI, cost aggregation, net benefits, BCRs, GNI shares, reconciliation tests, and decision flags. Do not paste R-computed derived results into decision sheets.

### Add/refactor these sheets (in both workbooks)

1. **`Health_Outcomes`** (or closest existing equivalent): expose Model 07 source outcomes for the workbook's scenarios — annual deaths averted, life-years gained, YLLs/YLDs/DALYs averted, and source LE/DW metadata. Aggregate with no duplicate population or cause counting.

2. **`Economic_Value`** (replace the public-health placeholder; refactor the clinical sheet). One row per scenario × year (and valuation case if a long layout is cleaner). Source columns plus **formula-derived** columns for: deaths averted and life-years gained; Indonesia and U.S. GNI per capita; population and total national GNI; transferred VSL before floor, VSL floor, final VSL, and `VSL / GNIpc`; VSLY and its denominator LE; undiscounted VSL and VSLY benefits; discount factor and PV benefits; annual mortality benefit as a share of annual national GNI; cumulative PV mortality benefit as a share of cumulative PV GNI (here or on the BCA summary). Show the preferred and standardized sensitivity cases with unambiguous labels. Keep/extend the existing supplementary incremental-cost/net-benefit columns; cross-link rather than duplicate the `Benefit_Cost` sheet.

3. **`Benefit_Cost`** (new formula-driven summary sheet, both workbooks). At minimum, one row per scenario × valuation method/case with: scenario ID and label; intervention family/package level; valuation method (`VSL`/`VSLY`) and case; PV of benefits; PV of costs; PV net benefits `= PV benefits − PV costs`; benefit-cost ratio `= PV benefits / PV costs`; benefit/GNI comparison; decision/status and a concise scope note.
   The headline BCR must use the **preferred VSL case**, discounted benefits, and discounted costs on the **same base year, rate, currency basis, and price year**. Do **not** use the current `economic_value_e1_2 / undiscounted incremental_cost` calculation. If PV costs are zero/negative, leave BCR blank/not-meaningful and report the appropriate dominance/net-benefit status; never show a misleading negative or infinite ratio.

### BCA interpretation and reporting

- Keep `Cost_Effectiveness` conceptually separate: USD per death/DALY averted is not a BCR.
- Use consistent benefit/cost categorization across scenarios; count downstream cost offsets, tax transfers, or savings only if the selected perspective and input contract include them; avoid double-counting.
- If the workbook includes only mortality benefits and implementation/health-system costs, label the result a **partial mortality-benefit BCA**, list material omitted benefits/costs, and do not claim a complete societal BCA.
- Preserve the public-health package rule: parent-package health outcomes come from the joint scenario, never summed standalone children; shared costs counted once.
- Report the undiscounted time distribution of benefits and costs alongside PV totals, and report net benefits alongside BCRs (BCR hides scale and is sensitive to categorization).

### Documentation and QA

Update existing `README`, `Methods_and_Sources`, `Calculation_Assumptions`, `Calculation_Map`, and `QA_Checks` sheets (following the existing `qa_check`/`qa_expect`/`qa_actual`/`qa_status` vector + anchor-cell pattern) rather than creating competing documentation. Cite the CHDS/Harvard Reference Case Guidelines for the added formulas.

Add formula-driven QA for: scenario coverage and exact ID reconciliation across Models 06/07/08/09; baseline pairing; Model 07 deaths/life-years/DALY reconciliation; Model 08 annual benefit aggregation; identical benefit and cost base year, discount rate, price year, and monetary basis; no missing GNI/LE for valued rows; VSL floor application and valuation-case formulas; no double-counting of population, causes, shared costs, or package children; annual-to-cumulative benefit and cost reconciliation; Excel-vs-independent-R QA for at least one clinical, one public-health, one parent-package, and one combined scenario; and formula errors (`#REF!`, `#DIV/0!`, `#VALUE!`, `#NAME?`, unexpected `#N/A`) and broken external links.

## 5. Required implementation behavior

- Use named joins and explicit key validation; never rely on row order or partial scenario-name matching.
- Fail early with one consolidated, actionable error for missing required assumptions or incompatible currency bases.
- Use cross-sheet references with quoted sheet names and absolute/relative references appropriate for filling formulas.
- Use helper columns/cells where needed; formulas must be readable and auditable rather than monolithic.
- Preserve the current color convention (editable assumptions, R/source inputs `st_rsrc`, formulas `st_formula`, PASS/REVIEW/FAIL).
- Preserve full precision in source/helper sheets; apply display formats without rounding values used in calculations.
- Do not create external Excel links or formulas requiring unavailable add-ins.

## 6. Plan first, then validate — acceptance criteria

**Before writing code**, produce a short written plan stating the exact 07 → 08 → 09 data contracts (columns/keys 07 will export, columns 08 will require and produce, new/changed sheet layouts in 09) and confirm it matches this spec. Then implement.

Before finishing:

1. Parse all modified R files.
2. Run the pipeline from Model 00 with: clinical only; public health only; and both families enabled.
3. Confirm Model 07 contains all active nonbaseline scenario IDs and incremental outcomes reconcile with Model 06.
4. Confirm Model 08 contains the same scenario IDs and has no independent Model 06 health-outcome calculation and no independent LE rebuild.
5. Confirm both Excel outputs have populated `Health_Outcomes`, `Economic_Value`, and `Benefit_Cost` sheets for their scenarios.
6. Recalculate the workbooks with LibreOffice/Excel-compatible recalculation, reopen, and scan all formula cells for errors and broken references.
7. Programmatically verify every derived cell in those three sheets is a formula, not a hard-coded result.
8. Reconcile representative Excel results to independent R calculations within a documented tolerance.
9. Verify the preferred VSL formula exactly implements elasticity 1.5, the 160 U.S. ratio, and the 20×GNI floor; verify the 100× and 160×GNI sensitivities.
10. Verify BCR = PV benefits / PV costs on a consistent monetary basis, and net benefits = PV benefits − PV costs.

## Deliverables

- Modified `07_output_dalys.R`, `08_economic_value_calculation.R`, and `09_cost_value.R`.
- Only if required and approved, minimally updated `Assumptions` sheets in the two input workbooks.
- Regenerated clinical and public-health Excel outputs with formula-driven `Health_Outcomes`, `Economic_Value`, and `Benefit_Cost` sheets.
- A concise change summary listing: files and exact sections changed; the new 07 → 08 → 09 data contracts; any added workbook assumptions and sources; compatibility fields retained; tests run and results; and remaining limitations (especially incomplete societal costs/benefits or currency-conversion uncertainty).

Do not stop after proposing the plan. Confirm the contract, then implement, run, validate, and report the completed changes.
