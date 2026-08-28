# Claude Code prompt: refactor the Indonesia executive slides

You are working in the `uw-wb-indonesia-ncd` repository. Modify the existing executive slide deck so it is suitable for a senior policy audience while remaining fully reproducible and auditable.

## Strict file scope

You may modify **only**:

1. `reports/executive_slides.rmd`
2. `reports/references.bib`

First use `git ls-files` to confirm the exact case of the tracked Rmd filename. If the repository uses `executive_slides.Rmd`, edit that tracked file; do **not** create a second file that differs only by case. Do not modify any R scripts, workbooks, data files, preambles, themes, generated outputs, or other repository files. Render to a temporary directory outside the repository if necessary.

Before editing, inspect the full current Rmd and the relevant source files. Preserve its existing dynamic scenario discovery, validation gates, visual identity, formulas, references, and exclusion of benefit-cost analysis. Make the smallest coherent set of edits needed for this task; do not refactor unrelated code.

## Authoritative inputs and precedence

Use these as the source of truth for the new cascade analysis:

- `output/70_30_30_to_70_70_70/indonesia_70_30_30_to_70_70_70_cost_value_formulae.xlsx`
- `code/cvd-fair-choices/00_run_70_30_30_to_70_70_70.R`
- Every existing script sourced by that runner, especially Models 04, 06, 07, and 09

Use the workbook's literal stored values and formulas; do not manually transcribe results and do not require Excel COM/recalculation. The supplied `Figure1_HTN_cascade.png`, `Figure2_40q30_costs.png`, and `Indonesia_NCD_results_table.xlsx` are design and reconciliation references only. If any reference image/table conflicts with the current pipeline workbook or code, the pipeline workbook and code win. Do not embed the supplied PNGs as static results: rebuild the figures in R from the authoritative outputs.

Keep the ordinary clinical, public-health, and combined results linked to their existing current-run sources. Do not merge the isolated cascade run into the ordinary results contract or change any pipeline output.

Current cascade metadata to verify, not hard-code:

- Scenario: `S_70_30_30_TO_70_70_70`
- Model horizon: 2025–2050; 2025 is the measured baseline year and scale-up begins in 2026
- 70-30-30 reached in 2030; 70-70-70 reached in 2040; achieved levels maintained thereafter
- Cholesterol treatment coverage follows hypertension coverage
- Scale-up is piecewise linear and prevents coverage backsliding
- Cost perspective: health system; price year: 2023; cost discount rate: 3% to the 2025 base year
- The cascade workbook is isolated under `output/70_30_30_to_70_70_70/`

## 1. Make the deck executive and plain-language

### Use takeaway statements as slide titles

Rewrite every content-slide title as a short declarative takeaway, not a topic label. A title should tell the reader what the slide means, ideally in 8–14 words. Derive numerical titles dynamically from the current model outputs; do not hard-code a result into a title.

Examples of the intended style—not text that must be copied verbatim:

- “Indonesia can prevent many premature cardiovascular deaths with proven interventions”
- “The model follows how prevention and treatment change disease and survival”
- “Population policies and clinical care reinforce each other”
- “Costs depend on who needs care, how many receive it, and local prices”
- “A stronger care cascade could substantially reduce deaths by 2050”

Section dividers may be concise, but should still communicate the purpose of the next section.

### Replace technical terminology with plain language

Rewrite visible text for a senior non-technical policy audience. Keep exact technical definitions and equations in the appendix. In the main deck, prefer phrases such as:

- “new cases” instead of “incidence”
- “risk of dying after developing the disease” instead of “case fatality”
- “current course” or “baseline (current course)” instead of unexplained “status quo”
- “additional cost” instead of unexplained “incremental cost”
- “discounted total cost” instead of unexplained “present value”
- “people who need the service” instead of “population in need” where space allows
- “healthy years lost or gained” when DALYs are mentioned, with the formal term in parentheses only if needed

Do not remove necessary precision. Define unavoidable terms briefly and use them consistently.

## 2. Remove the term “40q30” from all rendered content

Replace every user-visible occurrence of “40q30” in slide titles, body text, chart titles, axes, legends, captions, tables, notes, and the technical appendix with:

**Probability of death from CVD before age 70**

Use concise variants only where space requires them, for example:

- Axis: `Probability of death from CVD before age 70 (%)`
- Table: `Probability of death from CVD before age 70, 2050`
- Reduction: `Reduction in the probability of death from CVD before age 70`

It is acceptable to retain internal R object names, workbook sheet names, and column names such as `CVD_40q30` or `cvd_40q30_r` in non-rendered code so the pipeline interface is not broken. No rendered text should contain `40q30`.

## 3. Make each analytical slide transparent and defensible

Audit every slide that contains a model result, quantitative claim, chart, table, comparison, or analytical interpretation. Add a short bottom-of-slide note containing both:

1. `Source:` the exact data/model source used for that slide; and
2. `Method:` a one-sentence explanation of how the displayed result was calculated.

Format this note consistently as a small italic footnote in a muted color distinct from body text—prefer the deck's existing muted grey/blue or define a local color inside the Rmd. Keep it legible and ensure it does not overlap or overflow. Implement a small reusable R/LaTeX helper if that is the least invasive way to ensure consistent formatting.

Source notes should name the relevant workbook/RDS and, where useful, the exact sheet or model stage. Examples:

- Ordinary health results: current-run results contract and the relevant clinical/public-health/combined workbook
- Probability before age 70: the literal annual probability sheet produced by Models 07 and 09
- Cascade results: cascade workbook sheets `Cascade_Assumptions`, `Cascade_Trajectory`, `Annual_Mortality`, `CVD_40q30`, `Annual_Cost`, `Budget_Impact`, and/or `Cost_Effectiveness`, as applicable
- Epidemiology/demography: GBD 2023 and UN World Population Prospects 2024, as used by the pipeline
- Cascade coverage: NCD-RisC-based hypertension and diabetes inputs, as documented by the cascade input workbook and model code

Do not write vague notes such as “Source: model output.” Make each note specific enough to reproduce the slide.

On **every slide that contains costing, budget impact, or cost-effectiveness results**, add this caveat in the same footnote area:

*Preliminary costing results: unit costs, service use, and delivery assumptions require further validation against BPJS and Ministry of Health data before policy use.*

Retain the existing limitations slide, but update it so this validation requirement is also stated clearly there.

## 4. Add a high-level costing-methods slide and technical equations

Add the following article to `reports/references.bib`, unless an entry with the same DOI already exists. If it exists, reuse and improve the existing entry rather than duplicating it.

```bibtex
@article{Watkins2020ResourceRequirements,
  author  = {Watkins, David A. and Qi, Jinyuan and Kawakatsu, Yoshito and Pickersgill, Sarah J. and Horton, Susan E. and Jamison, Dean T.},
  title   = {Resource requirements for essential universal health coverage: a modelling study based on findings from Disease Control Priorities, 3rd edition},
  journal = {The Lancet Global Health},
  year    = {2020},
  volume  = {8},
  number  = {6},
  pages   = {e829--e839},
  doi     = {10.1016/S2214-109X(20)30121-2},
  pmid    = {32446348},
  pmcid   = {PMC7248571},
  url     = {https://pubmed.ncbi.nlm.nih.gov/32446348/}
}
```

Add an in-text citation to the new slide and to the detailed appendix methods.

### Main-deck costing slide

Create one clean, schematic, high-level slide adapted from the DCP Cost Model in Watkins et al. (2020), but explicitly adapted to this Indonesia project. Do not claim that the current model exactly reproduces the paper. Show a simple left-to-right or top-to-bottom flow:

1. People who need each service
2. Current coverage and scale-up target
3. Services delivered per person per year
4. Indonesia-adjusted unit cost
5. Annual baseline and intervention costs
6. Additional cost, discounted cost, cost per person, and cost per death prevented

The slide should make clear that the current project uses a health-system perspective and annual projections, while the paper provides the conceptual intervention-based costing framework. Mention that the paper uses unit costs, demographic/epidemiological estimates of people in need, baseline coverage, and health-system delivery costs. Keep equations off the main slide.

Include the BPJS/MOH validation caveat on this slide.

### Technical appendix costing details

Add a compact appendix slide with the adapted equations and definitions. Use the pipeline's actual conventions:

\[
C_{i,s,t}=PIN_{i,s,t}\times cov_{i,s,t}\times f_i\times uc_i
\]

\[
\Delta C_{s,t}=C_{s,t}-C_{0,t}
\]

\[
PV(\Delta C_s)=\sum_{t=2026}^{2050}\frac{\Delta C_{s,t}}{(1+r)^{t-2025}}
\]

\[
\text{Cost per death prevented}_s=
\frac{PV(\Delta C_s)}{\sum_{t=2026}^{2050}(D_{0,t}-D_{s,t})}
\]

Define `PIN`, coverage, annual frequency, unit cost, baseline, scenario, discount rate, price year, perspective, and the exclusion of downstream cost offsets. Explain that costs are in market US dollars and deaths remain undiscounted in the existing cost-per-death calculation. Cite Watkins et al. (2020) and the relevant model workbook/methods sheet.

## 5. Add annual additional cost per capita to the three existing cost-effectiveness slides

For the **clinical**, **public-health**, and **combined** cost-effectiveness slides, add both:

- Average annual additional cost per capita, **undiscounted**
- Average annual additional cost per capita, **discounted**

Compute them dynamically for the relevant aggregate scenario from the appropriate current-run workbook. Use the `Budget_Impact`/`Annual_Cost` values rather than manually typed totals.

The team-requested calculation is:

\[
\text{Undiscounted average annual additional cost per capita}=
\frac{\sum_{t=2026}^{2050}\Delta C_t}{25\times Pop_{2026}}
\]

\[
\text{Discounted average annual additional cost per capita}=
\frac{\sum_{t=2026}^{2050}\Delta C_t/(1+r)^{t-2025}}{25\times Pop_{2026}}
\]

Requirements:

- Verify programmatically that `length(2026:2050) == 25`.
- Use the national Indonesia population in 2026 from the model's authoritative demographic source, aligned to UN World Population Prospects 2024.
- Count population once by location × year × age × sex; never sum a population repeated across causes, scenarios, interventions, or cost components.
- Do not use the eligible/treated population as the denominator.
- Show enough precision to be useful, generally two decimals in US dollars per capita per year.
- Label the metric accurately as a **simple average over the 25-year horizon using the 2026 population denominator**. It is not an equivalent annual annuity and not a person-year denominator.
- Reconcile the discounted numerator to the workbook's cumulative discounted additional cost and the undiscounted numerator to its cumulative undiscounted additional cost.
- If no single defensible 2026 national-population series can be identified, stop and report the issue rather than estimating or silently choosing a denominator.

Because the existing tables are already dense, redesign them carefully: shorten visible labels, use a compact callout below or beside each table, or adjust column widths. Do not make the slides unreadable.

## 6. Add the 70-30-30 to 70-70-70 cascade analysis

Add a short, coherent subsection after the main clinical/public-health/combined results and before the policy implications. It should contain four slides: one methods schematic, two figure slides, and one results-table slide.

### Slide A — the cascade scale-up is ambitious but staged

Create a plain-language methods schematic for a primary-prevention cascade based on NCD-RisC hypertension and diabetes data. Explain:

- Hypertension and type 2 diabetes are modeled with diagnosed → treated → controlled care cascades.
- Cholesterol treatment coverage follows hypertension treatment coverage.
- Reach 70% diagnosed, 30% of those diagnosed treated, and 30% of those treated controlled by 2030.
- Reach 70% diagnosed, 70% of those diagnosed treated, and 70% of those treated controlled by 2040.
- Hold the 2040 levels through 2050.
- Scale-up is linear from the observed baseline to 2030, then linear to 2040.
- If observed baseline coverage already exceeds an interim milestone, do not scale it down.
- The model gives the full treatment effect to controlled patients and half of the full effect to treated-but-uncontrolled patients, exactly as specified in the cascade code/workbook.

Show the nested arithmetic in plain language:

- In 2030: treated share of all people with the condition = `70% × 30% = 21%`; controlled share = `70% × 30% × 30% = 6.3%`.
- In 2040: treated share = `70% × 70% = 49%`; controlled share = `70% × 70% × 70% = 34.3%`.
- Effective modeled coverage is `controlled + 0.5 × treated-but-uncontrolled`, giving 13.65% in 2030 and 41.65% in 2040. Keep “effective modeled coverage” in the schematic/appendix, not as an unexplained headline.

Add a source/method footnote naming `Cascade_Assumptions`, `Cascade_Trajectory`, the isolated runner, and the NCD-RisC-based inputs.

### Slide B — hypertension cascade figure

Recreate a polished clustered bar chart, using `Figure1_HTN_cascade.png` as the layout reference but the pipeline inputs as the data source.

- Show diagnosed, treated, and controlled shares of all people with hypertension.
- Show the measured baseline year **2025**, plus 2030 and 2040. Note that scale-up begins in 2026.
- Use three related, color-blind-safe shades and direct percentage labels.
- Use 0–80% on the y-axis.
- At baseline, show only cascade stages actually measured/modelled. If diagnosis is not separately available, show no diagnosed bar and explain this explicitly; do not impute diagnosis.
- Derive the national baseline treatment/control percentages from the authoritative cascade inputs, using the correct population/sex weighting. Do not copy the rounded values from the PNG if they do not reconcile.
- Add a concise footnote that type 2 diabetes follows the same target trajectory, while its observed baseline differs. Use the current source values and do not copy the image's approximate diabetes values without verification.

Use a takeaway title, for example “Indonesia’s hypertension cascade can progress in two achievable stages,” improved as needed.

### Slide C — probability before age 70 and projected cost figure

Recreate a high-quality two-panel figure using `Figure2_40q30_costs.png` only as a layout reference.

Left panel:

- Annual baseline/current-course and intervention trajectories, 2025–2050
- Source: cascade workbook `CVD_40q30`; use the literal R-reconciled annual values
- Visible title/axis must say `Probability of death from CVD before age 70`, never `40q30`
- Make clear this probability covers the six modeled cardiovascular causes and exact ages 30–70

Right panel:

- Cumulative **discounted** health-system cost under baseline and intervention, 2025–2050
- Build from `Annual_Cost` by aggregating `disc_cost_baseline` and `disc_cost_scenario` once per year and then cumulatively summing
- Guard against double-counting shared cost components and verify the 2050 totals against the workbook
- Label the 3% discount rate, 2025 base year, 2023 US dollars, and health-system perspective

Use consistent baseline/intervention colors across both panels, direct end labels where legible, and a takeaway title. The footnote must distinguish the six-CVD-cause probability from the cost scope, which covers the CVD primary-care and type 2 diabetes treatment cascades. Include the BPJS/MOH validation caveat.

### Slide D — cumulative results at 2030, 2040, and 2050

Create an executive-quality table with columns `2030`, `2040`, and `2050`, where each column is cumulative from 2025 through year `t`. Use the cascade workbook as the source and calculate dynamically.

Rows:

1. Projected deaths from the modeled cardiovascular diseases and type 2 diabetes
   - Baseline (current course)
   - Intervention
2. Projected health-system cost
   - Baseline
   - Intervention
3. Additional cost
4. Deaths prevented

Use `Annual_Mortality` for deaths and `Annual_Cost` for costs. The table's cost basis must be explicit; use cumulative discounted costs in 2023 US dollars, consistent with the supplied reference table and Figure 2. Use sensible units—millions of deaths or rounded persons, and US$ billions or millions—and state them in labels. Do not mix discounted and undiscounted costs in one row without labeling.

The source/method note must state:

- cumulative through year `t` means summing annual values from 2025 through `t`;
- intervention scale-up begins in 2026;
- deaths prevented = baseline deaths minus intervention deaths;
- additional cost = intervention cost minus baseline cost;
- deaths include the six modeled cardiovascular causes plus type 2 diabetes;
- costs cover the CVD primary-care and type 2 diabetes treatment cascades;
- costs are discounted 3% to 2025 and reported in 2023 US dollars.

Include the BPJS/MOH validation caveat.

## 7. Preserve correctness and prevent double-counting

- Continue to derive scenario identities dynamically from the current `Scenario_Catalog`/results contract for ordinary runs.
- For the cascade subsection, validate that the workbook contains exactly the baseline and `S_70_30_30_TO_70_70_70` scenario expected by the isolated runner.
- Do not add the cascade results to the ordinary combined package; present it as a separate strategy analysis.
- Do not sum child interventions to reconstruct a package when the workbook provides the package/joint run.
- Do not sum population across causes.
- When aggregating `Annual_Cost`, respect `cost_join_key`, `cost_scope`, and the workbook's `shared_duplicate_count`/QA logic.
- Use workbook metadata for horizon, discounting, price year, currency, and perspective whenever available; avoid duplicate hard-coded constants.
- Fail clearly on missing sheets, columns, scenarios, years, or non-finite values. Do not silently show `NA`, zero, or stale values.

Current-run QA anchors from the supplied workbook may be used only as sanity checks, never as hard-coded slide values: the 2050 cascade result is approximately 1.130 million deaths prevented, US$28.669 billion cumulative discounted additional cost, and US$25,373 per death prevented. If the current repository workbook differs, the current workbook wins and the discrepancy should be reported.

## 8. Visual and narrative quality

- Preserve the existing theme and palette, but simplify dense text.
- Use consistent colors for clinical, public health, combined, baseline/current course, and cascade intervention.
- Use direct labels where they reduce legend lookups.
- Keep tables legible at presentation size; avoid tiny text and overcrowded headers.
- Use whitespace and short annotations to emphasize the takeaway.
- Ensure all footnotes fit and are readable.
- Keep the technical appendix complete but compact.
- Do not add decorative graphics that are not data- or method-relevant.

## 9. Validation and completion criteria

Before finishing:

1. Render `reports/executive_slides.rmd` successfully from the repository root, sending generated artifacts to a temporary directory.
2. Visually inspect every rendered slide for clipping, overflow, overlapping footnotes, unreadable tables, broken citations, and poor contrast.
3. Extract the rendered PDF text and confirm there is no user-visible occurrence of `40q30`.
4. Search rendered text for `NA`, `NaN`, `Inf`, `NULL`, unresolved citation keys, and raw variable/scenario IDs.
5. Confirm every analytical/results slide has a specific italic source-and-method note in the alternate muted color.
6. Confirm every cost/cost-effectiveness slide includes the BPJS/MOH validation caveat.
7. Reconcile the clinical, public-health, combined, and cascade cost calculations against their source workbooks.
8. Independently verify the two annual additional-cost-per-capita formulas, including 25 projection years and a de-duplicated 2026 national-population denominator.
9. Confirm the cascade milestones and nested percentages reconcile exactly to the workbook assumptions.
10. Run `git diff --check` and review `git diff --stat`/`git status --short`. The only tracked modifications must be the existing executive Rmd and `reports/references.bib`.

When done, report:

- the slide titles/sections changed or added;
- the exact source used for each new metric;
- the 2026 population value and source used in the per-capita denominator;
- reconciliation results for the cost and cascade calculations;
- render/visual QA status; and
- confirmation that no file outside the two-file scope was modified.
