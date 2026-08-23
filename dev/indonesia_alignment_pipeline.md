# Claude Code prompt — align Indonesia transition probabilities to projected epidemiology

You are working in the repository `uw-wb-indonesia-ncd`, primarily in
`code/cvd-fair-choices/`. Build and execute an **isolated alignment/test
pipeline** that turns externally-projected epidemiology into model-ready
transition-probability inputs, calibrates the well–sick–dead Markov model so its
baseline reproduces the projection as closely as possible for **2023–2050**, and
proves the result stays schema-compatible with the unchanged downstream models —
**without touching the production pipeline**.

---

## 0. Hard constraints (read first)

1. **Do NOT modify, overwrite, rename, or delete any existing file** or any
   production artifact. In particular do not edit `00_run_model_cvd_fair.R`,
   `03_calibration_indonesia_nelder_mead.R`, `05_build_baseline_indonesia.R`,
   `06_run_scenarios_indonesia_fair.R`, `01_utils_indonesia.R`,
   `02_load_inputs_indonesia.R`, `04_define_interventions_indonesia.R`, or
   `07`–`10`, and do not overwrite the production `adjusted_searo_part*.rds`
   files or production `output/` results.
2. Create **only** these three new alignment scripts under `code/cvd-fair-choices/`:
   - `03_calibration_indonesia_alignment.R`
   - `05_build_baseline_indonesia_alignment.R`
   - `06_run_scenarios_indonesia_fair_alignment.R`

   Use the repository's normal **uppercase `.R`** extension even though the task
   was written with lowercase `.r`. Small testable helper files and a
   `code/cvd-fair-choices/alignment_tests/` folder are allowed; a fourth
   permanent **driver** script is discouraged — if one is truly necessary,
   explain why before creating it (see §9).
3. **Output contract is sacred.** What `06_run_scenarios_indonesia_fair_alignment.R`
   writes must be **identical in row grain, column names, column classes,
   scenario naming, and semantics** to production Model 06 output, so the
   unchanged `07_output_dalys.R`, `08_economic_value_calculation.R`, and
   `09_cost_value.R` run without modification. Write alignment outputs to a
   **separate location** (e.g. `output/out_model_alignment/`) so production
   outputs are never touched. Do not change 07–09 to accommodate an incompatible
   output — fix the alignment output instead.
4. **Read the existing scripts before writing anything.** Mirror their
   conventions: `data.table` idioms; the central `cause_map` / `dx_include` /
   `model_cause_codes` / `age_single` config in `00_run_model_cvd_fair.R`; the
   `gbd_band_label` / `gbd_age_bands` helpers in `01_utils_indonesia.R`; the
   `enforce_tp_constraints` probability logic and the well–sick–dead
   `project_combo` recursion in `03`. Reuse, don't reinvent.
5. If a real RDS differs from its sample CSV, **treat the RDS as authoritative**
   and report the difference. If attachments carry suffixes like `(2)`/`(7)`,
   resolve them to the repository filenames and target the repo versions.

---

## 1. Inputs

**Target projection (new external input).**
`data/processed/indonesia_epidemiology_baseline_alignment.rds`
(sample: `indonesia_epidemiology_baseline_alignment.csv`). Produced by
demographic methods **not** in this repo. For Indonesia, by `year` (2023–2050),
single `age` (0–99, with grouped metadata `age_group`/`age_mid` incl. `0-4` and
`95+`), `sex`, and `cause_id`, it carries: population, all-cause mortality, cause
fractions, incidence rate, prevalence rate, cause-specific mortality, and derived
counts. Columns:

```
year, age, sex, cause_id, age_group, age_mid, population, all_cause_mx,
all_cause_deaths, cause_fraction_raw, cause_fraction, selected_fraction_sum_raw,
fraction_adjustment_factor, incidence_rate, prevalence_rate, cause_name,
gbd_cause_name, cause_mx, cause_deaths, incident_cases, prevalent_cases,
epidemiology_source, scaling_source, incidence_available, incidence_data_status,
prevalence_available, prevalence_data_status
```

Facts from the sample you must handle (verify against the RDS):
- `cause_id` values present: `C_IHD, C_ICH, C_HHD, C_IS, C_RHD`. Our `cause_map`
  also defines `cmd` (Cardiomyopathy and myocarditis) and `dm2` (Diabetes
  mellitus type 2), which are **absent** from this projection → see the
  missing-cause rule (§3).
- Some rows have `incidence_available = FALSE` (e.g. HHD): `incidence_rate` is
  NA/blank while `prevalence_rate` is present → per-measure fallback (§3).
- `age` spans 0–99 with an open `95+` group; the model grid tops out at
  `max_model_age = 95` (open-ended 95+). Pool ages ≥ 95 into 95.

**Model-rate schema to emulate.**
`data/processed/adjusted_searo_part{1..10}.rds` — the calibrated
transition-probability files the model runs on (`b_rates`; sample
`b_rates_sample.csv`). Columns:

```
age, sex, cause, year, location, BG.mx.all, ALL.mx, BG.mx, PREVt0, DIS.mx.t0,
Nx, IR, CF, pop, check_well, check_sick, newcases, covid.mx
```

Semantics (confirm against `03`/`05`/`06`): `cause` = short code; `IR` = annual
well→sick probability; `CF` = annual sick→dead probability; `BG.mx` =
cause-specific background mortality; `BG.mx.all` = all-cause background mortality
of the pool; `PREVt0` = prevalence rate at t0; `DIS.mx.t0` = disease mortality at
t0; `Nx`/`pop` = population; `ALL.mx` = all-cause mortality; `covid.mx` is added
in `05` and enters the recursion. Production `adjusted_searo_*` carries observed
single-age data (calibration window 2009–2019); `05` then repeats 2019 forward
and applies trends.

**Files to inspect before planning:** `00`, `01`, `02`, `03_..._nelder_mead.R`,
`04`, `05`, `06_..._fair.R`, `07`, `08`, `09`; the alignment RDS; the production
`adjusted_searo*` inputs; and both sample CSVs.

---

## 2. Phase 1 — Inspect and plan (before editing)

Produce a concise written plan and **show it to me before writing or running
code**. State:

1. exact schema + key of the alignment RDS;
2. exact schema + key of the current `adjusted_searo*` objects;
3. the cause-name/code crosswalk (from central `cause_map`, not invented);
4. age and year coverage of each source;
5. how grouped/open-ended ages are expanded or pooled;
6. how each projection field maps into each model-rate field;
7. the calibration target grain and objective function;
8. where alignment-only inputs, calibrated rates, diagnostics, logs, and model
   outputs will be written; and
9. how output compatibility with Models 07–09 will be tested non-destructively.

Do not guess silently; surface any ambiguity you resolved and how.

---

## 3. Phase 2 — Construct the alignment transition-probability input

Implement preparation inside `03_calibration_indonesia_alignment.R` (or small
sourced helpers it calls) **before** calibration, using testable helper
functions.

### Required schema
The prepared/calibrated rate table must preserve the production `adjusted_searo*`
structure — at minimum: `age, sex, cause, year, location, BG.mx.all, ALL.mx,
BG.mx, PREVt0, DIS.mx.t0, Nx, IR, CF, pop, check_well, check_sick, newcases,
covid.mx` — plus any additional columns the current code needs. Match column
**classes, cause coding, sex labels, age convention, keys, and ordering** to the
production objects.

### Time coverage and cell-level source precedence
Deterministic precedence on the full key `location × year × age × sex × cause`:

1. **Years < 2023:** preserve production `adjusted_searo*` values **exactly**. Do
   not backcast the projection into these years.
2. **Years 2023–2050:** use the alignment RDS where a valid mapped value exists.
3. If an alignment year/cause/sex/age/measure is absent or invalid, **fall back
   to the production `adjusted_searo*` value** (for projected years, extend the
   observed series the same way production `05` carries a year forward).
4. Never replace a valid production value with `NA`/`NaN`/`Inf`, an impossible
   probability, or an unavailable alignment value.
5. **No Cartesian joins; no duplicate key rows.** Use explicit keyed joins with
   declared cardinality; **fail on duplicate keys after reporting examples.**
6. Keep a `source_*` audit field (or a separate provenance table) tagging each
   value, by field/key, as `alignment`, `production_fallback`, `interpolation`,
   or `derived`.

**Fallback is per-measure.** E.g. if projected prevalence exists but projected
incidence is unavailable for HHD, use projected prevalence **and retain the valid
production incidence/IR** — do not drop the row or set IR to 0.

### Cause mapping
Derive the final mapping from the central `cause_map` and the observed files,
reconciling at least: `C_IHD`/Ischaemic heart disease → `ihd`; `C_IS`/Ischaemic
stroke → `istroke`; `C_ICH`/Intracerebral haemorrhage → `hstroke`; `C_HHD` →
`hhd`; `C_RHD` → `rhd`. Inspect whether `cmd` and `dm2` appear; for any modeled
cause missing from the projection, **retain production data**. Do not invent a
mapping. Save/report a mapping audit listing unmatched and fallback causes.

### Age handling
- If every model single age already has its own independent record, **do not
  interpolate**.
- If only grouped ages carry independent observations, expand to the single-year
  grid with a documented, smooth, **nonnegative, shape-preserving/monotone**
  method (e.g. PCHIP); avoid unconstrained-spline overshoot.
- Preserve totals: interpolate **rates** (or suitable transformed rates), map
  population, and **re-derive counts** so aggregation back to the source group
  agrees within a documented tolerance.
- Use central ages `min_model_age:max_model_age`; keep the terminal age as the
  open-ended 95+ stock. Do **not** naïvely duplicate a 95+ rate/count across
  ages. Interpolate **separately by sex, cause, year, measure** — never across
  causes or sexes.

### Field derivation and identities
Inspect code semantics first. Maintain, to tolerance, where data support them:
`Nx`/`pop` = projected population; `ALL.mx` = all-cause mortality rate;
all-cause deaths = `Nx × ALL.mx`; `PREVt0` = prevalence rate and prevalent cases
= `Nx × PREVt0`; incident cases = `Nx × incidence_rate` when available; cause
deaths = `Nx × cause_mx`.

Do **not** mechanically equate an epidemiological rate to a transition
probability unless that identity holds in the discrete-time model. Inspect how
`IR`, `CF`, `DIS.mx.t0`, `BG.mx`, `BG.mx.all` are used in the recursion, then use
a defensible starting point and verify it against the code:
- `IR` initialized from projected incidence **only** when consistent with the
  well→sick transition and susceptible denominator;
- `CF` initialized from projected cause deaths ÷ projected prevalent cases where
  the denominator is positive (handle zero denominators explicitly);
- background mortality derived from the all-cause envelope after removing modeled
  cause mortality, following production logic.

Enforce the same constraints as the current model, **including covid**:
`0 ≤ IR, CF, BG.mx, BG.mx.all, covid.mx ≤ 1`; `IR + BG.mx + covid.mx ≤ 1`;
`CF + BG.mx + covid.mx ≤ 1`. Apply the production defensive clamps
(`CF≥1→0.99`, `IR≥1→0.99`, negatives→0) and `enforce_tp_constraints`: **prefer
preserving the all-cause/background envelope and cap/transform the disease
transition**; only fall back to proportional renormalisation when the envelope
alone leaves no headroom. Report every material constraint adjustment — never
silently clip large numbers.

### Alignment-only artifacts (never mix with production)
Write to an alignment-specific location / unmistakable prefix, e.g.:
- `data/processed/alignment/transition_probabilities_alignment_precalibration.rds`
- `data/processed/alignment/adjusted_searo_alignment_part*.rds`
- `data/processed/alignment/alignment_source_provenance.csv`
- `data/processed/alignment/alignment_cause_mapping.csv`

**Critical trap:** production `05_build_baseline_indonesia.R` ingests rates via
`list.files(path = wd_data, pattern = "adjusted", ...)`. A broad `"adjusted"`
pattern would silently mix alignment and production files. The alignment `05`
(§5) must load **only** alignment files by explicit path; never let a glob mix
the two.

---

## 4. Phase 3 — Calibrate to the projection (Nelder–Mead style)

Base `03_calibration_indonesia_alignment.R` on
`03_calibration_indonesia_nelder_mead.R` and reuse its robust features: bounded
parameter transforms (logistic squash) instead of optimizer-side clipping;
reproducible multi-start Nelder–Mead; a **baseline start** so calibration can
never knowingly return a worse objective than the uncalibrated candidate;
explicit probability constraints; optional safe parallelization; saved factors +
detailed diagnostics. Reuse the production functions verbatim where possible.

**The only substantive diffs from the production Nelder–Mead script should be:**
(a) the calibration **target source**, (b) the **year window**, and (c) the
input/output **paths**.

- **Targets = the projection's `prevalent_cases` and `cause_deaths`** (counts, by
  5-year age group × sex × cause × year), *not* the GBD
  `temp_1baseline_rates_gbd23.rds` extract. Aggregate model sick→prevalence and
  dead→deaths to the same bands. Use incidence/incident cases as an **additional**
  target only where consistent with the model and available; do not impute
  unavailable incidence just to add a term.
- **Window = 2023–2050** (set `CAL_YEAR_START`/`CAL_YEAR_END` for the alignment
  run). Do **not** multiply the 2023 rate once and repeat it to 2050 — targets
  vary by year; the TP inputs must **follow** the 2023–2050 path while respecting
  the cohort recursion and keeping smooth, plausible time paths.

### Objective
Scale-aware loss so large causes/ages don't dominate and tiny/zero cells don't
explode. Preserve the production emphasis on deaths (`W_DEATHS = 2`,
`W_PREV = 1`) unless inspection supports otherwise:
`loss = w_death·loss_deaths + w_prev·loss_prevalence [+ w_inc·loss_incidence]
[+ modest temporal smoothness penalty if year-specific params are optimized]`.
Define/centralize all weights; handle zero targets explicitly; report both the
optimization loss and interpretable diagnostics (MAE, RMSE, relative error where
defined, total-count error, max abs discrepancy) **overall and by year, cause,
sex, age group**; compare pre- vs post-calibration fit.

Avoid an unmanageable high-dimensional optimization: inspect the current
granularity and choose the **simplest identifiable parameterization that can
follow time-varying targets** (e.g. smooth year-specific or knot-based IR/CF
multipliers by sex/cause/age group). Explain the choice in comments/diagnostics.

### Output and acceptance
Emit the calibrated TP as the alignment `adjusted_searo_alignment_part*.rds`
(same chunked format/schema/ordering as production `03`), plus alignment-named
`calibration_factors_alignment.csv` and `calibration_diagnostics_alignment.csv`,
convergence status/optimizer messages, pre/post fit diagnostics, target-vs-modeled
tables, and a concise run summary. **Do not accept calibration merely because
`optim()` converged:** post-calibration overall loss must be **no worse** than
pre-calibration, and regressions by major cause/year must be flagged. If the
supplied incidence/prevalence/cause-mortality/all-cause mortality are internally
**incompatible** with a three-state Markov model, **quantify and report the
residual mismatch** — do not distort the data silently. Respect the production
`03` header guidance to set `run_adjustment_model <- FALSE` so `05` does not
re-apply `adjustments2023_age.csv` on top of the baked-in calibration.

---

## 5. Phase 4 — Alignment baseline builder

`05_build_baseline_indonesia_alignment.R`, an alignment-only counterpart of
Model 05, must:
1. explicitly load **only** the calibrated alignment files from the alignment 03;
2. **never** use `list.files(..., pattern = "adjusted")` in a way that could also
   ingest production files (see the trap in §3);
3. preserve pre-2023 production history;
4. **retain the calibrated 2023–2050 year-specific rates** — do **not** let the
   production block that repeats `year == 2019` forward (`for (i in 2020:2050)
   ... rep %>% mutate(year = i)`) overwrite projected years; adapt it so the
   projection's 2023–2050 values survive into `b_rates`;
5. not reapply production adjustments already embedded in the alignment
   calibration (`run_adjustment_model <- FALSE`);
6. preserve population, COVID/excess-mortality, background-mortality, age, cause,
   and coding conventions expected by Model 06;
7. prevent duplicate rows and unintended many-to-many joins; and
8. validate transition sums, missingness, uniqueness, and population/rate
   identities before returning `b_rates`.

The in-memory objects exposed at the end (especially `b_rates`) must have the
same names and compatible structures the production Model 06 consumes.

---

## 6. Phase 5 — Alignment scenario runner

`06_run_scenarios_indonesia_fair_alignment.R`, a minimal alignment-only
counterpart of Model 06, must:
1. preserve the intervention engines and workbook-driven scenario definitions
   from production Model 06; change only what's needed to consume the alignment
   baseline;
2. ensure the baseline scenario uses the aligned calibrated TPs for 2023–2050;
3. preserve scenario IDs, intervention-family fields, hierarchy fields, and
   execution switches;
4. write to an alignment-specific output dir/prefix (e.g.
   `output/out_model_alignment/`) so production results are never overwritten;
5. produce output with the **same row grain, column names, column classes,
   scenario naming, and semantics** as production Model 06 — the `out.df`
   columns `age, cause, sex, year, well, sick, newcases, dead, pop, all.mx,
   intervention, location, eff_ir, eff_cf` plus `scenario`,
   `htn_target_scenario`, `intervention_family`, `scenario_role`,
   `parent_package_id`;
6. not change Models 07–09 to accommodate an incompatible output.

If 07–09 locate Model 06 results only by a fixed production path/pattern, test
downstream compatibility via a **safe temporary/staged input path or a copied
test harness** — never overwrite production outputs just to run the test.

---

## 7. Execution entry point

Do not modify Model 00. At the top of each new script, document the exact
execution order and prerequisites, and provide a reproducible R invocation that:
1. loads the common configuration/inputs from Models 00–02 **without** triggering
   production Models 03–10;
2. runs `03_calibration_indonesia_alignment.R`;
3. sources the unchanged `04_define_interventions_indonesia.R`;
4. runs `05_build_baseline_indonesia_alignment.R`;
5. runs `06_run_scenarios_indonesia_fair_alignment.R`; and
6. stages/points Models 07–09 at the alignment Model 06 outputs for
   non-destructive compatibility tests.

Avoid copying large Model 00 config blocks that could drift; reuse/safely source
common definitions or add guarded initialization inside the three allowed
scripts. Only add a fourth permanent driver if absolutely necessary — explain why
first.

---

## 8. Plan, test, execute (work in this order)

1. **Plan.** Deliver the §2 plan and wait for nothing that blocks progress, but
   surface resolved ambiguities.
2. **Build + unit-test Phase 2.** Assert: schema/column order/classes match
   `adjusted_searo_part*.rds`; the full key is unique (no row-count expansion);
   no `NA`/`NaN`/`Inf` in required fields; `IR,CF ∈ [0,1]`;
   `IR+BG.mx+covid.mx ≤ 1`, `CF+BG.mx+covid.mx ≤ 1`; years 2023–2050 present for
   all expected (age,sex,cause); pre-2023 rows equal production exactly on shared
   keys/columns; `cmd`/`dm2` present via fallback; per-measure fallback correct
   (HHD prevalence projected, IR retained). Print the provenance summary and the
   age-aggregation reconciliation within tolerance.
3. **Run + validate calibration (Phase 3).** Fixed seeds → reproducible params;
   optimizer status recorded; post-calibration loss no worse than pre; target-vs-
   modeled prevalence and cause deaths compared for **every** year 2023–2050 and
   summarized by cause/sex/age group; large residuals and regressions surfaced.
4. **Run baseline + scenarios (Phases 4–5).** Run at least the baseline scenario
   end-to-end for Indonesia through 2050 (and, if runtime permits, one clinical +
   one public-health scenario as smoke tests). Verify no duplicate output keys
   and no negative/invalid `well/sick/newcases/dead/pop`. **Prove schema
   identity** vs a production Model 06 output via `setequal(names(...))` plus
   explicit class and key/grain checks. Then run Models 07/08/09 (or their
   read/init portions) against **safely staged** alignment output and show they
   don't fail on schema/naming/path assumptions — without modifying 07–09.
5. **Report.** Files created and output paths; calibration fit quality (how
   closely prevalence and cause deaths match the projection, pre vs post, overall
   and by cause/year); causes/years/cells served by fallback; internal-
   inconsistency residuals if any; and confirmation via `git status` /
   `git diff --stat` that the diff is limited to the new alignment files.

---

## 9. Coding requirements & guardrails

- Use `data.table` consistently where surrounding code does; explicit joins with
  declared keys and cardinality checks.
- Avoid hard-coded row counts, age ladders, and cause lists when central
  config/helpers exist.
- Centralize tolerances, calibration bounds, weights, seeds, optimizer settings,
  years, and artifact paths at the top of the alignment Model 03.
- Comment formulas and non-obvious choices; fail early with informative errors
  for missing files/columns, duplicate keys, unmapped required causes, or
  impossible transition inputs.
- Make parallel execution optional and **safe on Windows**; always stop clusters
  with `on.exit()`/equivalent cleanup. Keep runs seeded (reuse the production
  `SEED` scheme).
- Do not introduce new package dependencies unless essential (check availability,
  document install needs, do not install silently). Do not broadly
  `suppressWarnings`. No unrelated formatting/refactoring.
- If a required package or input the production scripts expect is missing in this
  environment, **say so and stop** rather than fabricating data. Don't paste huge
  tables — summarise, and save full diagnostics to the alignment CSVs.

---

## 10. Deliverables

1. the three new R scripts;
2. all alignment-specific input/calibration/output/diagnostic artifacts;
3. a concise change summary by file;
4. the exact commands used to execute the pipeline;
5. test results, including downstream Models 07–09 compatibility;
6. a table of pre- vs post-calibration fit for prevalence and cause deaths,
   overall and by cause/year;
7. warnings/unresolved limitations — especially missing causes/years,
   unavailable incidence, internal target inconsistencies, non-convergence, or
   material residual mismatch; and
8. `git diff --stat` plus a focused `git diff` of the three new scripts,
   confirming production files and artifacts were not modified or overwritten.

Do not report success until you have actually run the available pipeline/tests.
If execution is blocked by missing raw data, packages, memory, or runtime,
complete all feasible static and unit tests, show the exact failing command/error,
and state precisely what remains unverified.

Now: start with the **Plan** (§2) and show it to me before writing or executing code.
