# Prompt for Claude Code — Executive Slide Deck for the Indonesian MoH

Copy everything below into Claude Code as your task instruction.

---

You are working in the Indonesia CVD/FAIR Choices modeling repository. Create:

`Reports/executive_slides.RMD`

The file must render successfully to a polished, 16:9 LaTeX Beamer
presentation for a high-level briefing to the Indonesian Minister of Health.

## Objective and audience

Prepare a concise, decision-focused executive presentation describing the
Indonesia CVD modeling project, its methods, interventions, principal health
effects, cost-effectiveness, and benefit–cost findings.

The audience is senior government leadership, not technical modelers. Use
plain language, short conclusions, and policy-relevant interpretations.
Avoid equations and unnecessary implementation details. Emphasize:

* the scale of preventable CVD mortality;
* progress toward reducing premature CVD mortality;
* the comparative impact and affordability of the interventions;
* the additional value of combining clinical and public-health interventions;
* actionable implications for Indonesia.

Do not invent results, intervention definitions, assumptions, citations, or
data sources. Derive them from the repository and the specified input and
output files. If something can't be found, say so explicitly rather than
guessing (see "If a metric is unavailable" below).

## Files to review before writing anything

1. The complete modeling pipeline — read through it, don't just skim
   filenames — to understand disease-state definitions, scenario naming,
   and how clinical vs. public-health interventions act on the model:

   * `code/cvd-fair-choices/00*` through `09*`

2. The clinical intervention input workbook:

   * `indonesia_model_inputs(4).xlsx`, or the corresponding current
     repository input file (search for the current equivalent if renamed).

3. The public-health intervention input workbook:

   * `indonesia_model_inputs_public_health_updated_mortality(2).xlsx`, or
     the corresponding current repository input file.

4. The three results workbooks (source of truth for all reported numbers):

   * `output/indonesia_model_cost_value_formulae.xlsx` — baseline and
     clinical scenarios only
   * `output/indonesia_cost_value_public_health_formulae.xlsx` — baseline
     and public-health scenarios only
   * `output/indonesia_model_cost_value_clinical_public_health_formulae.xlsx`
     — baseline, all clinical scenarios, all public-health scenarios, and
     `all_clinical_public_health`

5. Comparator data:

   * `data/raw/03_asean_cvd_40q30_comparison.csv`

6. Existing report resources:

   * `reports/beamer_preamble.tex`
   * `reports/references.bib`
   * any existing report, slide, logo, color, or branding assets.

   **If `reports/beamer_preamble.tex` does not already exist in this repo**,
   it is meant to be adapted from a different project's preamble. Search
   sibling/other project directories on this machine for a
   `beamer_preamble.tex`, copy it into `reports/` here, and then rebrand it
   for a **World Bank – University of Washington** collaboration — strip
   any institution names, logos, or color branding from the project it came
   from. If no such file can be located anywhere, tell me before proceeding
   rather than fabricating a preamble from scratch.

Use `rg` or repository file searches to locate renamed or equivalent files.
**Do not modify the modeling pipeline, input workbooks, or output
workbooks** as part of this task.

## Data extraction and validation

Programmatically import all displayed results in the R Markdown file. Do
not manually transcribe model estimates.

The Excel workbooks contain formulas. Ensure imported values are current
and numerically evaluated. If the R packages used do not evaluate Excel
formulas, recalculate the workbooks safely with LibreOffice in a temporary
location, or use reliable cached formula values. **Do not overwrite the
source workbooks.**

Inspect workbook sheet names, column names, scenario identifiers, units,
time horizons, and formula definitions before constructing any table. Use
the output workbooks as the source of truth for reported model results.

Reconcile scenario names across files with an explicit internal crosswalk.
Do not silently match scenarios using partial-name matching if this could
produce ambiguity.

For each metric, first use the value already calculated in the output
workbook. Only derive a metric when it is not available directly, using the
model's established definitions. Unless the existing pipeline specifies
otherwise, use these as internal validation formulas (never displayed as
equations in the slides themselves):

* cumulative deaths averted = sum over the model's stated intervention
  horizon of (baseline deaths − scenario deaths) at each time t;
* % reduction in deaths in 2050 = 100 × (baseline deaths_2050 − scenario
  deaths_2050) / baseline deaths_2050;
* % reduction in CVD 40q30 in 2050 = 100 × (baseline CVD 40q30_2050 −
  scenario CVD 40q30_2050) / baseline CVD 40q30_2050.

Respect the pipeline's own definitions of: CVD deaths and modeled causes;
CVD 40q30; baseline and intervention horizons; incremental intervention
costs; annualization; population denominators; value of a statistical
life/life-year; benefits, net benefits, and benefit–cost ratios. Do not mix
cumulative costs with annual deaths, or annualized costs with cumulative
health effects.

**Add validation checks that stop rendering with an informative error if:**

* a required workbook or sheet is missing;
* baseline or required scenario rows are absent;
* required metrics are non-numeric or missing;
* scenario names are duplicated unexpectedly;
* 2050 is unavailable;
* the clinical and public-health workbooks produce conflicting baseline
  values beyond a clearly stated numerical tolerance.

## Required slide structure

Aim for approximately 16–22 substantive slides, excluding appendix slides.
Combine closely related material when that produces a clearer narrative.

**1. Title slide** — *"Reducing Cardiovascular Disease in Indonesia:
Health Impact, Costs, and Economic Returns"* (or similar), with a restrained
subtitle describing the 2025–2050 policy analysis, subject to the actual
model horizon. Present as a collaboration between the World Bank, the
University of Washington, and relevant Indonesian partners — but only when
supported by repository materials. Use local logo files only if they exist;
never download or fabricate logos.

**2. Why this matters** — minimum context: CVD burden in Indonesia, role of
premature CVD mortality, why clinical care and population-wide prevention
are complementary, why the model evaluates impact/cost/economics together.
Cite repository-supported facts.

**3. Project aims** — one general aim (evaluating health and economic
effects of scaling evidence-based clinical and public-health CVD
interventions in Indonesia through 2050) and three specific aims covering:
(1) projecting CVD burden and 40q30 under status quo and alternative
scenarios; (2) estimating deaths averted, mortality reductions, costs, and
cost-effectiveness; (3) estimating economic benefits, benefit–cost ratios,
and the added value of combining clinical + public-health implementation.
Refine wording from repository documentation; don't introduce unsupported
objectives.

**4. Executive summary** — 4–6 short headline findings from actual model
results (most impactful clinical intervention/package, most impactful
public-health intervention/package, combined result, progress on 40q30,
affordability, economic return). Each headline needs a number and a
timeframe; avoid false precision.

**5. Model overview** — plain-language description of population/
demographic projection, baseline epidemiology and calibration, annual
disease-state simulation, clinical and public-health scenarios, health
outcomes, intervention costs, cost-effectiveness and benefit–cost analysis.
**No equations.**

**6. TikZ state-transition figure** — one clean, native `tikzpicture`
(not a rasterized image) showing: Well → Sick; Well → Background death;
Sick → CVD death; Sick → Background death; a recovery transition only if it
actually exists in the code. Visually distinguish incidence effects
(Well→Sick), case-fatality/mortality effects (Sick→CVD death), and
background mortality. Must be understandable without technical notation and
fit cleanly on one slide.

**7. Clinical interventions** — from the clinical input workbook/scripts:
intervention/package name, target population, principal mechanism,
implementation/coverage scenario, disease transitions affected. Use
executive-friendly labels while retaining an internal mapping to exact
scenario codes. Separate individual interventions from combined packages.

**8. Public-health interventions** — from the PH input workbook/pipeline:
policy/intervention, exposure or risk factor addressed, affected CVD causes,
whether it affects incidence and/or post-disease mortality, implementation
scenario. List only interventions actually modeled in the current
workbooks.

**9. Data sources table** — columns: Model component | Primary source |
Years/version | Use in model. Include only sources verified in code,
spreadsheets, documentation, or the bibliography (check specifically for UN
World Population Prospects, GBD 2023, WHO Global Health Estimates, FAIR
Choices/DCP-related parameters, Indonesian national data, intervention
effect meta-analyses, intervention-cost sources, economic valuation inputs)
— don't assume all of these are actually used.

**10. Public-health impact table** — rows = PH interventions (+ combined PH
package if it exists); columns: intervention, cumulative deaths averted,
CVD 40q30 in 2050, % reduction in deaths in 2050, % reduction in CVD 40q30
in 2050. State the accumulation period in the title/subtitle.

**11. Clinical cost-effectiveness and impact** — rows = clinical
interventions; columns: intervention, cumulative deaths averted, %
reduction in deaths (2050), CVD 40q30 (2050), % reduction in CVD 40q30
(2050), cost per death averted, annualized incremental cost per capita. If
too wide, split into two aligned panels/slides rather than shrinking text.

**12. Clinical benefit–cost analysis** — rows = clinical interventions;
same impact/cost columns as #11 plus monetized benefit, benefit–cost ratio,
and net benefit if available. State valuation approach, currency year,
discount rate, and perspective in a short note based on actual model
assumptions. Use two slides/panels if needed.

**13. Public-health cost-effectiveness and impact** — same column structure
as #11, for PH interventions (include combined PH package where supported).

**14. Public-health benefit–cost analysis** — same column structure as #12,
for PH interventions. State currency, price year, discounting, valuation
method, and perspective.

**15. Combined package** — dedicated slide for `all_clinical_public_health`
vs. baseline, all-clinical, all-PH, and combined. Highlight health impact,
2050 CVD 40q30, total incremental costs, cost per death averted, annualized
cost per capita, and benefit–cost ratio where available. **Do not compute
the combined result by summing separate clinical + PH estimates** — use the
combined-workbook scenario directly, since effects may overlap.

**16. CVD 40q30 time series and international comparison** — year on x,
CVD 40q30 on y. Observed 1995–2023 as solid lines/points; projected
2025–2050 as dashed. Comparator countries from
`data/raw/03_asean_cvd_40q30_comparison.csv` as thin, muted background
lines. Indonesia lines layered/prominent on top: baseline/status quo,
all-clinical, all-public-health, and combined clinical+PH — each in a
distinct but restrained color, dashed for the projected segment. Use actual
year availability in the comparator file; do not interpolate/extrapolate
comparator countries beyond what the repo already documents. Add a vertical
marker + brief annotation ("Modeled scenarios") at the observed/projected
boundary; don't visually imply an observed value between 2023 and 2025.
Label lines directly at endpoints where feasible; avoid an oversized
legend. Confirm units before choosing a percent vs. rate axis format.

**17. Policy implications** — 3–5 evidence-based implications (largest
health gains, most affordable, strongest economic return, why PH + clinical
should be implemented together, implementation scale/sequencing supported
by results). Keep conclusions within what the model demonstrates; separate
model findings from policy judgment.

**18. Limitations** — short, transparent slide: dependence on modeled
epidemiological estimates; uncertainty in effect sizes, coverage, costs;
simplified disease states; sustained-implementation assumptions; limited
representation of implementation constraints; results are projections, not
guarantees.

**19. Closing slide** — three memorable messages, plus one central
quantified combined-package result if supported.

**20. Technical appendix** — compact slides: scenario crosswalk, metric
definitions and horizons, costing/valuation assumptions, model causes and
age range, data validation/reconciliation notes, references. Keep all
technical material out of the main narrative.

## Design requirements

* Adapt the sourced `reports/beamer_preamble.tex` for the World Bank–UW
  collaboration; 16:9 aspect ratio; clean title and section-divider slides.
* Restrained, color-blind-accessible palette, e.g.: dark navy/World Bank
  blue (primary), UW purple (secondary accent), teal/green (public-health
  scenarios), a contrasting blue/purple (clinical scenarios), a darker tone
  for the combined package, neutral gray (comparators/baseline context).
  Never rely on color alone — pair with line type, labels, or symbols.
* Large, readable fonts; minimal text; no dense paragraphs; no equations in
  the main deck; no screenshots of spreadsheets; no tables that extend
  outside slide margins; consistent number formatting (deaths rounded to
  thousands or sensible sig figs; costs with currency + scale; percentages
  to ≤1 decimal unless more precision is substantively needed); sources in
  small, readable footnotes; no orphaned titles, clipped figures,
  overlapping elements, or unreadable legends.
* Use `booktabs`, `tabularx`/`kableExtra`, `adjustbox`, `tikz`, etc. only as
  needed, and confirm they're declared in the preamble.

## Reproducibility

* Use project-relative paths, preferably via `here::here()`; no hard-coded
  absolute paths.
* All chunks: `echo=FALSE, message=FALSE, warning=FALSE`; keep data-prep
  code out of the rendered slides via appropriate chunk options.
* Load only packages actually required.
* Define reusable helper functions for importing sheets, formatting
  scenarios, validating results, and formatting numbers — don't duplicate
  logic per table/slide.
* No manually entered results.
* Set a reproducible ggplot2 theme consistent with the palette above.
* Comment non-obvious crosswalks or metric selections.
* Render from a clean R session.

### If a metric is genuinely unavailable

1. Search all relevant workbook sheets and pipeline outputs first.
2. Document exactly what is missing.
3. Use `NA` / "Not available" in the draft rather than a guess.
4. Add a code comment identifying the missing upstream field.
5. Report the issue in the completion summary.

## References

Use `reports/references.bib` where helpful. Add citations already supported
by the repository. If a required source is documented in the input
workbook/code but absent from the bibliography, add the minimum valid
BibTeX entry only if it can be verified from repository materials — never
invent citation details.

## Render and visual QA

1. Render `Reports/executive_slides.RMD` to PDF from a clean R session.
2. Fix all R, Pandoc, LaTeX, TikZ, bibliography, and missing-package errors.
3. Render every PDF page to an image/contact sheet and visually inspect for
   clipping, overflow, tiny text, bad page breaks, misalignment, unclear
   plots.
4. Revise until the deck is presentation-ready.
5. Confirm every reported number traces back to one of the three output
   workbooks.
6. Confirm the combined package comes from the combined-scenario workbook
   rather than a sum of separate estimates.

Do not modify unrelated files or refactor the model pipeline.

## Completion report

When finished, provide:

* path to the completed R Markdown and the rendered PDF;
* final slide count;
* a concise list of workbook sheets and scenario fields used;
* the definition and period used for cumulative deaths averted;
* the source and definition used for CVD 40q30;
* any unavailable metrics or unresolved discrepancies;
* confirmation that the `beamer_preamble.tex` used, and where it was
  sourced from if copied from another project;
* confirmation that the PDF was rendered and visually inspected.
