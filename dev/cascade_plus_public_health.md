# Claude Code task — Interact the 70‑30‑30 → 70‑70‑70 cascade with public‑health interventions

## Role & repository

You are working in the **`uw-wb-indonesia-ncd`** repository (Indonesia NCD decision‑science platform). All model code lives in `code/cvd-fair-choices/`. The pipeline is a chain of numbered scripts (`01_utils` … `10_validation`) sourced by a top‑level `00_run_*` orchestrator. Two orchestrators already exist and must stay **byte‑for‑byte unchanged in behaviour**:

- `code/cvd-fair-choices/00_run_model_cvd_fair.R` — the ordinary FAIR‑Choices run (clinical + public‑health families; writes the clinical, public‑health, and combined clinical+PH workbooks to `output/`).
- `code/cvd-fair-choices/00_run_70_30_30_to_70_70_70.R` — the **isolated cascade** run (hypertension/cholesterol + diabetes treatment cascade `S_70_30_30_TO_70_70_70`), which redirects every output under `output/70_30_30_to_70_70_70/` and runs with public health **off**.

Read both orchestrators, plus `04_define_interventions_indonesia.R`, `05_build_baseline_indonesia.R`, `06_run_scenarios_indonesia_fair.R`, and `09_cost_value.R`, before making any change. Do not assume — confirm the object and path names below against the actual code.

## Goal

Create a **third, standalone run** that executes the **70‑30‑30 → 70‑70‑70 cascade and the public‑health interventions together**, and produces one new cost/value workbook that presents both families side by side plus a combined ("join run") summary:

```
output/indonesia_model_cost_value_70_30_30_to_70_70_70_public_health_formulae.xlsx
```

This is analogous to how the ordinary run already builds a genuine joint `all_clinical_public_health` scenario (see the "JOINT CLINICAL + PUBLIC‑HEALTH SCENARIO" block near the end of `04_define_interventions_indonesia.R`) — but here the clinical arm is the **cascade** scenario instead of the clinical `all` package.

The combined summary must present the joint result **both ways, side by side** (this is a firm requirement):
1. the **genuine joint model run** — cascade effect rows + PH effect rows applied **once each** in a single Model 06 projection (effects compound multiplicatively on the rate/survival scale, exactly like `all_clinical_public_health`); and
2. the **multiplicative approximation** computed from the two families' separate reductions, i.e. `combined = 1 − (1 − R_cascade) × (1 − R_PH)`, shown as a clearly‑labelled cross‑check with the divergence between the two flagged.

---

## Part 1 — New orchestrator + Models 00/04/05/06

### 1a. Create a new orchestrator

Create `code/cvd-fair-choices/00_run_70_30_30_to_70_70_70_public_health.R`. Base it on `00_run_70_30_30_to_70_70_70.R` (same libraries, `wd`/`wd_code`/`wd_raw`/`wd_data` setup, same `rm(list=ls())` fresh‑session start, same `cause_map` / ages / config‑validation blocks), with these differences:

- Enable **both** families:
  ```r
  run_cascade_70_30_30_to_70_70_70 <- TRUE
  run_public_health_interventions  <- TRUE   # PH family ON — this is the join run
  run_clinical_interventions       <- FALSE  # the cascade carries the "clinical" arm; do NOT also run the FAIR clinical catalogue
  run_cost_value                   <- TRUE
  ```
  (Note: the "# PH family excluded from this run" comment copied from the cascade runner is **wrong** for this orchestrator — PH must be ON. Correct or remove it.)
- Keep the cascade namespace: `cascade_scenario_id <- "S_70_30_30_TO_70_70_70"`, `cascade_family <- "cascade_70_30_30_to_70_70_70"`, and point `model_inputs_file` at `data/indonesia_70_30_30_to_70_70_70_inputs.xlsx`.
- Point the **public‑health** input workbook at the same authoritative PH contract the ordinary runner resolves (`public_health_inputs_file` — reuse the `.ph_candidates` fallback logic from `00_run_model_cvd_fair.R`, preferring `data/indonesia_model_inputs_public_health_updated_mortality.xlsx`). Set `tobacco_timing_analysis <- "base"`.
- Set the output workbook paths so nothing collides with either existing run (see **Isolation** below):
  - `public_health_cost_value_formulae_file <- paste0(wd_outp, "indonesia_cost_value_public_health_formulae.xlsx")` **only if** you regenerate the PH workbook in this run; otherwise leave the canonical PH workbook to the ordinary run and **read** it as a source (preferred — see Part 2).
  - `combined_cost_value_70_30_30_public_health_formulae_file <- paste0(wd, "output/indonesia_model_cost_value_70_30_30_to_70_70_70_public_health_formulae.xlsx")`.

### 1b. Skip Models 02 and 03

**Do not source `02_load_inputs_indonesia.R` or `03_calibration_indonesia_nelder_mead.R`.** They are unnecessary because Model 05 loads the already‑calibrated baseline table `data/processed/b_rates_full_period_reconciled_2017_2050_national_current38.rds` directly (see `05_build_baseline_indonesia.R`). The new orchestrator sources: `01_utils` → `04` → `05` → `06` → `07` → `08` → `09`. Do **not** source `10_validation_indonesia.R` (it is a clinical‑run validation).

⚠️ Before removing the `02`/`03` sources, check what globals `04`/`05`/`06`/`09` expect from them. The cascade runner currently *does* source `02` and `03`; if any object those scripts create (e.g. `fair_inputs`, `public_health_inputs`, sodium/statin/TFA `.rds` files under `data/processed/`, `dt_gbd_rr` helpers, calibration handoff) is required downstream and is **not** already persisted to disk, either (a) load the persisted artifact directly, or (b) keep the minimal sourcing needed and clearly comment why. Prefer loading persisted `.rds`/`.xlsx` artifacts over re‑running calibration. Make the skip **explicit and guarded**: if a required artifact is missing, `stop()` with a message telling the user to run the calibration once, rather than silently producing NA results.

### 1c. Models 04 / 05 / 06 — build and run the joint cascade + PH scenario

The changes must be **flag‑guarded** so the two existing orchestrators are unaffected. Introduce a single new predicate, true only in the new orchestrator, e.g.:
```r
run_cascade_public_health_join <-
  isTRUE(get0("run_cascade_70_30_30_to_70_70_70", ifnotfound = FALSE)) &&
  isTRUE(get0("run_public_health_interventions",  ifnotfound = FALSE))
```

**Model 04 (`04_define_interventions_indonesia.R`):**
- The existing cascade branch (guarded by `run_cascade_70_30_30_to_70_70_70`, ~line 2311) already routes to `.build_cascade_catalogue()` and yields `fair_scenarios == {baseline, S_70_30_30_TO_70_70_70}`. Keep that.
- The existing PH branch (`if (isTRUE(run_public_health_interventions))`, ~line 3255) already builds `public_health_scenarios` (and `public_health_inputs`). Keep that — it will now fire in the cascade run too.
- The existing joint block (~line 3277 "JOINT CLINICAL + PUBLIC‑HEALTH SCENARIO") currently builds `combined_scenarios[["all_clinical_public_health"]]` only when `run_clinical_interventions && run_public_health_interventions`. **Add a parallel joint** for the cascade case: when `run_cascade_public_health_join` is TRUE, build
  ```r
  combined_scenarios[["all_cascade_public_health"]] <- list(
    scenario_id      = "all_cascade_public_health",
    scenario_label   = "70-30-30 -> 70-70-70 cascade + all public-health interventions (combined)",
    family           = "cascade_public_health",
    scenario_level   = "combined",
    scenario_role    = "combined",
    intervention_ids = c(<cascade intervention ids>, <ph intervention ids>),
    interventions    = c("fair_wb", "ph_wb"),      # BOTH engines run once each
    fair_effect_rows = fair_scenarios[["S_70_30_30_TO_70_70_70"]]$fair_effect_rows,  # cascade rows
    ph_effect_rows   = public_health_scenarios[["all_public_health"]]$ph_effect_rows
  )
  ```
  Mirror the field names, provenance tags, and collision‑safety check used by the existing `all_clinical_public_health` block. The cascade scenario already exposes its effects as `fair_scenarios[["S_70_30_30_TO_70_70_70"]]$fair_effect_rows` (same field a clinical `all` scenario uses) and `interventions = "fair_wb"`, so feeding it into the joint's `fair_effect_rows` is direct — but confirm this against the code before relying on it. Do **not** touch the existing `all_clinical_public_health` block.
- Preserve the cascade runner's post‑Model‑04 scope assertions, but **relax them for the join orchestrator only**: in a join run the catalogue is `{baseline, S_70_30_30_TO_70_70_70}` for `fair_scenarios`, a non‑NULL `public_health_scenarios`, and `combined_scenarios == {all_cascade_public_health}`. Keep the strict `{baseline, cascade}`‑only + `public_health_scenarios must be NULL` assertions active for the plain cascade runner (they are guarded by the absence of the PH flag today — make sure they don't fire when PH is on).

**Model 05 (`05_build_baseline_indonesia.R`):**
- No analytic change. Verify the generic sick→dead baseline guard (the `.sick_dead_target_codes` block) still behaves correctly when **both** `public_health_inputs$valid_links` and the cascade `fair_inputs$valid_links` are present — it already loops over both objects, so a joint run should simply union the required case‑fatality target causes. Confirm the cascade + PH links together don't demand a baseline stock the calibrated table lacks; if they do, fail loudly with a clear message.

**Model 06 (`06_run_scenarios_indonesia_fair.R`):**
- Model 06 must run baseline + the cascade scenario + each PH scenario + the joint `all_cascade_public_health` as a **single projection that applies `fair_wb` and `ph_wb` once each to the same baseline‑rate copy** — identical mechanism to `all_clinical_public_health`. The `combined_scenarios` dispatch loop (`for (nm in setdiff(names(combined_scenarios), baseline_id))`) already appears to be scenario‑id‑agnostic — it keys off each entry's `interventions` / `fair_effect_rows` / `ph_effect_rows`, not off the literal string `"all_clinical_public_health"` — so a correctly‑built `all_cascade_public_health` entry should flow through automatically. **Verify this**, and if any branch hard‑codes `all_clinical_public_health`, generalise it (key off the scenario's `family`/`interventions` fields).
- The joint run is where "combining multiplicatively" is realised: applying both engines once each compounds the cascade and PH effects multiplicatively on the rate scale. It must **never** be an arithmetic sum of the two families' separate outputs.

Everything analytic (effect sizes, affected fractions, coverage trajectories, costs, discount rate, analysis/price years, perspective) stays in the Excel input workbooks. **Do not duplicate or hard‑code any analytic assumption in R.**

---

## Part 2 — Model 09: the new combined workbook

Modify `code/cvd-fair-choices/09_cost_value.R` to write:

```
output/indonesia_model_cost_value_70_30_30_to_70_70_70_public_health_formulae.xlsx
```

Guard the new writer with `run_cascade_public_health_join` so it only fires in the new orchestrator and leaves the existing three writers (clinical `cost_value_formulae_file`, PH `public_health_cost_value_formulae_file`, combined `combined_cost_value_formulae_file`) untouched. Reuse the existing `openxlsx` helpers already in Model 09 (`createWorkbook`, `addWorksheet`, `writeData`, `writeFormula`, the header/joint styles, `int2col`, the Model‑07 40q30 sheet builders, the formula‑decision‑sheet builders) rather than inventing a new writer.

### 2a. Preserve the sheet structure of both source workbooks

The new workbook must **carry over the full sheet structure** of the two source workbooks:

- **Public‑health source:** `output/indonesia_cost_value_public_health_formulae.xlsx` (produced by the ordinary run / the PH writer in Model 09).
- **Cascade source:** `output/70_30_30_to_70_70_70/indonesia_70_30_30_to_70_70_70_cost_value_formulae.xlsx` (produced by the cascade runner; 22 sheets — `README, Run_Metadata, Scenario_Catalog, Cascade_Assumptions, Cascade_Trajectory, Cascade_QA, Selected_Interventions, Cost_Components, Annual_Mortality, Health_Outcomes, CVD_40q30, CVD_40q30_Age, Annual_Cost, Budget_Impact, Cost_Effectiveness, Economic_Value, Benefit_Cost, QA_Checks, Input_Diagnostic, Methods_and_Sources, Calculation_Assumptions, Calculation_Map`).

Do **not** hard‑code either sheet list — read whatever sheets each source workbook actually contains (`openxlsx::getSheetNames()`) and copy every one. To keep provenance unambiguous and avoid name collisions, **prefix** the copied sheets:
- public‑health sheets → `PH_<sheet>`
- cascade sheets → `CASC_<sheet>`

Copy the **values** of each source sheet (read with `readxl::read_excel()` / `openxlsx::read.xlsx()` and re‑`writeData` them). Preserving live Excel formulas across workbooks is not required — the source workbooks already carry the "formula edition"; the combined workbook is primarily a presentation/roll‑up. Note in the `README`/`Run_Metadata` that copied cells are static snapshots of the two source workbooks (with their file paths and mtimes recorded for traceability).

**Prerequisite check:** both source workbooks must exist before the combined writer runs. If either is missing, `stop()` with a message telling the user which prior run to execute first (the ordinary run for the PH workbook, the cascade runner for the cascade workbook) — unless you choose to (re)generate the PH workbook within this same run, in which case document that clearly.

### 2b. Add the combined `Summary` sheet (first sheet)

Add a new **`Summary`** sheet as the **first** sheet, styled like the `Summary` sheet in `indonesia_model_cost_value_clinical_public_health_formulae.xlsx` (header style, joint row highlighted). It presents, per row, the two families and the combined join result:

Rows: `baseline`, `S_70_30_30_TO_70_70_70` (cascade), `all_public_health` (PH), and the joint `all_cascade_public_health` — plus the two combined variants described below.

Columns — feature **all** of these metric groups (source them from the corresponding sheets of the two source workbooks and from the joint run's Model 07/08/09 outputs; label units and the discounting/price‑year basis):
- **Deaths averted + DALYs/YLL** — cumulative and annual deaths averted, DALYs and YLL/YLD averted vs baseline (from `Annual_Mortality` / `Health_Outcomes`).
- **CVD 40q30** — the period CVD 40q30 level and its reduction, reported at **2030 and 2040** (from `CVD_40q30` / `CVD_40q30_Age`); this ties to the cascade coverage milestones (the cascade runner already checks 2030 ≈ 0.1365 and 2040 ≈ 0.4165 effective coverage).
- **Costs + ICER + budget impact** — incremental cost, cost‑effectiveness (ICER / cost per DALY averted), and budget impact (from `Annual_Cost` / `Budget_Impact` / `Cost_Effectiveness`).
- **Economic value + benefit‑cost** — VSL‑based economic value and benefit‑cost ratio (from `Economic_Value` / `Benefit_Cost`).

**Show the combined result both ways, side by side** (firm requirement):
1. **Joint (model)** — the genuine `all_cascade_public_health` joint‑run values (from this run's Model 07/08/09 outputs). This is the headline.
2. **Multiplicative (approx.)** — computed on the family‑level proportional reductions from the two source workbooks:
   `R_combined = 1 − (1 − R_cascade) × (1 − R_PH)`
   applied to the relevant reduction metrics (e.g. deaths averted fraction, CVD‑40q30 reduction). Do the multiplication on the **survival / reduction scale** (not on levels), by year where the metric is annual.
3. A **`Δ (joint − multiplicative)`** column (or a small `Combination_Check` block) that reports the divergence between the two, so the user can see how far the simple multiplicative assumption sits from the genuine joint projection. Add a one‑line note explaining that independence (multiplicative) is only an approximation and the joint model run is authoritative.

Keep the `Summary` self‑documenting: a short legend distinguishing "R source value", "copied snapshot", "joint model", and "multiplicative approximation", mirroring the legend conventions already used in the combined clinical+PH workbook.

---

## Isolation & safety constraints (do not skip)

The cascade runner enforces strict output isolation. Preserve that discipline for the new run:

1. **Never modify the two existing orchestrators' behaviour.** All new logic is guarded by `run_cascade_public_health_join` (or the two underlying flags). Re‑running `00_run_model_cvd_fair.R` and `00_run_70_30_30_to_70_70_70.R` must produce identical outputs to before (byte‑for‑byte where they already do).
2. **Do not overwrite existing outputs.** The new combined workbook is a *new* path. If you (re)generate any intermediate under `output/`, write it to a dedicated location (e.g. `output/70_30_30_to_70_70_70_public_health/`) and **treat the two canonical source workbooks as read‑only inputs**. Adapt the cascade runner's fail‑fast collision guard and the pre/post‑run mtime snapshot so that: (a) the join run's *own* new outputs are asserted to live under its own directory, and (b) the two **source** workbooks it reads are asserted **unchanged** (snapshot mtimes before, compare after).
3. **Reconcile `wd_outp`.** The cascade runner sets `wd_outp <- output/70_30_30_to_70_70_70/`. Decide deliberately where the join run's Model 06–09 intermediates land, and make sure the PH/combined writers point at the intended absolute paths rather than accidentally inheriting the cascade subdirectory. Make every resolved output path explicit and assert it before any model runs.
4. **Analytic assumptions stay in the workbooks.** No effect sizes, coverages, costs, discount rates, or year ranges in R.
5. **Fail loud, never silent.** Any missing input, empty/NA joint effect rows, or absent source workbook must `stop()` with an actionable message — never emit all‑zero or NA results.

---

## Acceptance criteria / verification

Before declaring done, verify and report:

1. `source("code/cvd-fair-choices/00_run_model_cvd_fair.R")` and `source("code/cvd-fair-choices/00_run_70_30_30_to_70_70_70.R")` still run and produce unchanged outputs (spot‑check a couple of the existing workbooks' key cells / mtimes of untouched files).
2. `source("code/cvd-fair-choices/00_run_70_30_30_to_70_70_70_public_health.R")` runs end‑to‑end **without sourcing 02/03/10**, and writes `output/indonesia_model_cost_value_70_30_30_to_70_70_70_public_health_formulae.xlsx`.
3. The new workbook contains: a `Summary` first sheet; every `PH_<sheet>` from the PH source; every `CASC_<sheet>` from the cascade source; and the two source workbooks are byte‑identical afterward (mtime/hash check).
4. On the `Summary` sheet: the joint `all_cascade_public_health` row is present and highlighted; deaths‑averted/DALYs, CVD‑40q30 (2030 & 2040), cost/ICER/budget‑impact, and economic‑value/benefit‑cost columns are populated; and the multiplicative‑approx and `Δ(joint − multiplicative)` columns are present and sensible (joint deaths averted should be **less than** the naive sum of the two families and **close to** the multiplicative approximation, not equal to it).
5. Print a short reconciliation: for one headline metric (e.g. cumulative deaths averted 2026–2050), show cascade‑only, PH‑only, multiplicative combined, and joint‑model combined, and confirm ordering makes sense.
6. Confirm Model 04's post‑run scope assertions pass in the join run and still pass (unchanged) in the plain cascade run.

## Deliverables

- New file: `code/cvd-fair-choices/00_run_70_30_30_to_70_70_70_public_health.R`.
- Flag‑guarded edits to `04_define_interventions_indonesia.R`, `05_build_baseline_indonesia.R`, `06_run_scenarios_indonesia_fair.R`, `09_cost_value.R`.
- New output workbook `output/indonesia_model_cost_value_70_30_30_to_70_70_70_public_health_formulae.xlsx`.
- A brief summary of what changed in each file, the resolved output paths, and the verification results above.

Work in small, reviewable steps; show diffs for each modified file; and keep every new branch guarded so the two existing runs are provably unaffected.
