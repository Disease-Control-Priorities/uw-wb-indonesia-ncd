# Claude Code prompt: add the cascade + tobacco-tax interaction scenario

Please modify the Indonesia CVD model pipeline to add a genuinely jointly modelled interaction scenario:

```text
S_70_30_30_TO_70_70_70_I_PH_TOBACCO_TAX
```

This scenario must apply the `S_70_30_30_TO_70_70_70` cascade and `I_PH_TOBACCO_TAX` simultaneously in one Model 06 projection. It must not be calculated by adding, multiplying, averaging, or otherwise combining the two standalone output trajectories after simulation.

## Strict scope

Modify only these three source files:

1. `code/cvd-fair-choices/04_define_interventions_indonesia.R`
2. `code/cvd-fair-choices/09_cost_value.R`
3. `reports/executive_slides.Rmd`

Do not modify Model 05, Model 06, any other R script, any input workbook, or any unrelated part of the slide deck. Model 06 already iterates over all entries in `combined_scenarios`, so no Model 06 change should be necessary.

Do not create an additional Excel output workbook. The new scenario must be included in the existing workbook:

```text
output/indonesia_model_cost_value_70_30_30_to_70_70_70_public_health_formulae.xlsx
```

Preserve all existing scenarios, sheets, formulas, styles, column structures, ordering conventions, QA checks, and output paths unless the minimal changes below require an extension.

## 1. Model 04: build the targeted joint scenario

In the existing cascade + public-health joint-scenario branch, retain the current `all_cascade_public_health` scenario unchanged and add a second entry to `combined_scenarios` with:

```r
scenario_id    = "S_70_30_30_TO_70_70_70_I_PH_TOBACCO_TAX"
scenario_label = "70-30-30 -> 70-70-70 cascade + tobacco tax"
family         = "cascade_public_health"
scenario_level = "combined"
scenario_role  = "combined"
interventions  = c("fair_wb", "ph_wb")
```

Construct it from the already validated catalogue objects:

- Cascade arm: `fair_scenarios[[cascade_scenario_id]]`, using its `intervention_ids` and `fair_effect_rows`.
- Public-health arm: `public_health_scenarios[["I_PH_TOBACCO_TAX"]]`, using only its `intervention_ids` and `ph_effect_rows`.

The new entry must contain the same provenance fields used by `all_cascade_public_health`, including:

```r
intervention_ids
clinical_intervention_ids
public_health_intervention_ids
cascade_scenario_id
fair_effect_rows
ph_effect_rows
```

Add fail-loud validation that `I_PH_TOBACCO_TAX` exists and has non-empty validated `ph_effect_rows`. Add collision validation for the new scenario ID. Extend the existing cleanup `rm()` only for the new temporary variables introduced.

Do not replace or rename `all_cascade_public_health`; both joint scenarios must remain available.

## 2. Model 09: include multiple joint scenarios in the existing workbook

Update `source_combined_cost_value()` minimally so a single workbook build can include all current-run scenarios stored in `combined_scenarios`, while retaining its scalar `joint_id` argument as the primary QA/reconciliation anchor.

After obtaining `produced_ids`, define:

```r
joint_ids <- intersect(names(combined_scenarios), produced_ids)
```

Require the primary `joint_id` to be present in `joint_ids`, and construct the comparator vector as the unique union of:

```r
clin_comparators
ph_comparators
joint_ids
```

Then make the following targeted extensions:

1. Cost every scenario in `joint_ids` by calling both `add_clin()` and `add_ph()` with that scenario's own `clinical_intervention_ids` and `public_health_intervention_ids`.
2. In `catrow()`, recognize `scn %in% joint_ids` and retrieve `combined_scenarios[[scn]]`.
3. Classify every scenario in `joint_ids` as `scenario_level = "combined"`.
4. Highlight every joint row in `Scenario_Catalog` and `Summary` using `%in% joint_ids`, rather than highlighting only `scenario == joint_id`.
5. Keep `joint_id = "all_cascade_public_health"` as the primary reconciliation anchor for the existing QA formulas unless a change is strictly required for correctness.
6. Ensure the new scenario flows through all applicable existing sheets, including `Summary`, `Scenario_Catalog`, `Annual_Mortality`, `Health_Outcomes`, `CVD_40q30`, `CVD_40q30_Age`, `Annual_Cost`, `Budget_Impact`, `Cost_Effectiveness`, `Economic_Value`, and `Benefit_Cost`.

In the existing cascade + public-health export section, validate that both of these joint scenario IDs were built by Model 04:

```r
c(
  "all_cascade_public_health",
  "S_70_30_30_TO_70_70_70_I_PH_TOBACCO_TAX"
)
```

Call `source_combined_cost_value()` only once and retain the existing output path:

```text
output/indonesia_model_cost_value_70_30_30_to_70_70_70_public_health_formulae.xlsx
```

Do not add a second call that writes a tobacco-specific workbook, and do not introduce any new Excel filename.

## 3. Executive slides: plot the correct interaction scenario

In `reports/executive_slides.Rmd`, modify only the `fig-40q30-cascade` code and the comments/source text directly associated with that figure.

Replace this plotted specification:

```r
list(id = "all_cascade_public_health", wb = casc_ph_wb,
     label = "70-70-70 target achieved + tobacco tax",
     col = "#7B3294")
```

with:

```r
list(id = "S_70_30_30_TO_70_70_70_I_PH_TOBACCO_TAX", wb = casc_ph_wb,
     label = "70-70-70 target achieved + tobacco tax",
     col = "#7B3294")
```

Update only the figure's directly related inline documentation so it accurately states that this purple line is the cascade plus tobacco tax only—not `all_cascade_public_health` and not the broader tobacco regulatory package. This includes:

- The scenario list comments immediately above `scen_specs`.
- The source/method note immediately below `fig-40q30-cascade`.
- Any nearby comment that incorrectly says the plotted interaction contains all selected tobacco interventions.

Do not change the label or color unless needed to preserve the existing presentation. Do not alter any other figure, table, slide, plotting style, workbook path, or endpoint-label placement.

## Validation

After editing:

1. Confirm Model 04 creates both `all_cascade_public_health` and `S_70_30_30_TO_70_70_70_I_PH_TOBACCO_TAX`.
2. Confirm Model 06 requires no edit and automatically dispatches the new `combined_scenarios` entry.
3. Run the relevant pipeline from Model 04 through Model 09 using the existing cascade + public-health orchestrator if the required project data are available. Do not run Models 02 or 03.
4. Confirm the existing combined cascade/public-health workbook contains exactly one row for the new scenario in `Scenario_Catalog` and `Summary`, and contains its annual trajectory in `CVD_40q30` through 2050.
5. Confirm `Annual_Cost` for the new scenario contains only its cascade components plus `I_PH_TOBACCO_TAX`, not the clean-air, mass-media, or advertising-ban components.
6. Confirm no new Excel workbook was created.
7. Render or compile `reports/executive_slides.Rmd` if dependencies are available and confirm `fig-40q30-cascade` reads the new scenario from the existing cascade/public-health workbook without validation errors.
8. Report the exact files changed, tests run, and results. If execution is blocked by unavailable data or dependencies, perform syntax/static validation and report the blocker without editing additional files.

Keep the implementation narrowly scoped and avoid refactoring unrelated code.
