# Claude Code task: refactor the Indonesia CVD/FAIR Choices production pipeline for ages 0–95+ and centrally configured causes

Work in this repository:

`C:\Users\wrgar\OneDrive - UW\02Work\WorldBank-Indonesia\uw-wb-indonesia-ncd\code\cvd-fair-choices`

## Objective

Modify the active production pipeline, scripts `00` through `06`, so that:

1. The model includes the full GBD age range from age 0 through the open-ended age group 95+.
2. Type 2 diabetes mellitus is included as a modeled cause.
3. The causes to run are declared only once in `00_run_model_cvd_fair.R`. Every downstream script must derive its cause filtering, mappings, loops, joins, validation, and outputs from that central configuration. A user must be able to add or remove a modeled cause by editing only script `00`, assuming the required input data exist.
4. The complete production pipeline runs successfully and tests demonstrate that these requirements are genuinely met.

Do not merely patch the final outputs. Trace and correct all age- and cause-dependent logic throughout the pipeline.

## Scope

Inspect and modify the active dependency chain invoked by:

- `00_run_model_cvd_fair.R`
- `01_utils_indonesia.R`
- `02_load_inputs_indonesia.R` (sources `021`, `022`, `023`)
- the production calibration script sourced by `00` (currently `03_calibration_indonesia_nelder_mead.R`; confirm this in the repository — `00` sources it directly, not `03_calibration_indonesia.R`)
- `04_define_interventions.R`
- `05_build_baseline_indonesia.R`
- `06_run_scenarios_indonesia_fair.R`

Also inspect helper scripts sourced by this chain, including as applicable:

- `021_get_base_rates_indonesia.R`
- `022_get_tps_indonesia.R`
- `023_get_tps_bgmx_indonesia.R`
- `031_calibration_indonesia.R`
- `032_adjustments_indonesia.R`
- the other calibration variants `03_calibration_indonesia_random_tp.R` and `03_calibration_indonesia_transparent.R` share the same age/cause ladders as the active Nelder-Mead script; if you centralize the age/cause helpers, update these too so they don't drift, but the Nelder-Mead script is the one that must run.

Modify helper scripts when required for the `00`–`06` production run. Do not refactor unrelated reporting or economic-analysis scripts unless a small compatibility change is essential to run and validate `00`–`06`.

Note: `00` also sources `07_output_dalys.R`, `08_economic_value_calculation.R`, and `09_validation_indonesia.R` (not in scope). Do not modify them, but be aware they consume `06` outputs — if your changes alter the output schema (e.g. new ages/causes), flag what will likely break downstream so it can be handled in a later pass.

## Known problems already identified in this repo (fix these, and search for more of the same pattern)

Treat this as a starting map, not an exhaustive list. Grep the full `00`–`06` chain for every age filter and every cause list and reconcile them all.

### Cause hard-coding / divergent declarations
- **`00_run_model_cvd_fair.R`** already defines the intended `dx_include`, `cause_map` (including `dm2 = "Diabetes mellitus type 2"`), and `cause_cols <- names(cause_map)`. These are the intended single source of truth — but downstream scripts currently ignore or shadow them.
- **`021_get_base_rates_indonesia.R` — `project.all()` has its OWN inner default `cause_map`** (roughly lines 108–120) that still lists `aod = "Alzheimer's disease and other dementias"` and **omits `dm2`**. Even though the loop calls `project.all(loc, yr, cause_map = cause_map)` with the global map, the divergent default is a trap and any code path relying on it is wrong. Remove the inner default; require the global map (or fail if absent).
- **`032_adjustments_indonesia.R`** contains a **second, divergent `dx_include`** (around lines 328–333) that still lists `"Alzheimer's disease and other dementias"` and **omits `dm2`**. Delete this local redefinition; consume the global one.
- **`05_build_baseline_indonesia.R`** uses repeated hand-written `fcase(cause == "Ischemic heart disease", "ihd", ...)` recoders and a `cause_lookup <- setNames(names(cause_map), cause_map)`. Replace the `fcase` recoders with a lookup/join derived from the central `cause_map` so new causes (dm2) are picked up automatically.
- **`06_run_scenarios_indonesia_fair.R`** contains many hard-coded cause vectors, e.g. `c("ihd", "istroke", "hstroke", "hhd")`, `c("ihd", "hhd", "istroke", "hstroke", "aod")`, and a long-name→short-code `recode` that lists ihd/istroke/hstroke/hhd/cmd/rhd but **not dm2**. **Critical distinction:** some of these vectors define *which causes an intervention/risk factor acts on* (e.g. a blood-pressure relative-risk table legitimately applies only to `ihd/istroke/hstroke/hhd`) — those are clinically correct and must stay cause-specific. Others mean *"all modeled causes"* and must include dm2. Do not blindly add dm2 to intervention-effect vectors; do carry dm2 through the state-transition model, baseline, and outputs. Where a given vector's intent is ambiguous, flag it in your summary rather than guessing.
- `022_get_tps_indonesia.R` has a dementia patch gated behind `run_aod_par` (currently `FALSE`). Leave dementia gated, but ensure it can never reintroduce a cause absent from the central `cause_map`.

### Age hard-coding (all assume adults 20–95)
- **`021_get_base_rates_indonesia.R`**: the `dt <- dt[age_name %in% c("20-24 years", ... "95+ years")]` filter starts at `20-24`; `interpolate.rate()` uses `ages_in <- c(seq(22, 92, 5), 95)` and `ages_out <- 20:95`; the loop sets `age := 20:95`; `pop.df` filters `age_group > 19`. All must extend down to age 0 with correct irregular low-age band midpoints (`<1`, `1-4`, `5-9`, …).
- **`022_get_tps_indonesia.R`**: two separate age-index schemes (`age2 = ifelse(age=="20-24 years", 1/0, ...)` around lines 109–142), `approx(x = c(seq(22,92,5), 95), xout = 20:95)` in `get.single.age.rates`, `over95` handling, and an `age2 < 96`-style terminal assumption. Extend to 0–95+ and treat 95 as open-ended (see terminal-group section).
- **`023_get_tps_bgmx_indonesia.R`**: header/logic states "ages 20–95+"; the `auto.arima`/`forecast` per-age background-mortality projection and its `keep_age`/NA handling must cover the full 0–95+ grid without silently dropping young ages.
- **`031_calibration_indonesia.R`** and **`03_calibration_indonesia_nelder_mead.R`** (and the `_random_tp` / `_transparent` variants): `age_match`/`am` built as `data.frame(age = 20:95)` with an `ifelse`/`fcase` band-label ladder starting at `20-24`. Extend the single-year vector and the label ladder to 0–95+.
- **`032_adjustments_indonesia.R`**: `gbd_breaks <- c(seq(20, 95, by = 5), Inf)` with matching labels starts at 20. Extend to include the younger bands.
- **`01_utils_indonesia.R`**: `create_age_groups()` uses `breaks <- c(20, seq(25, 85, by = 5), Inf)` and a top band labeled `"85plus"`.
- **Band-label inconsistency to unify:** the codebase emits `"95+ years"` (021/022), `"95+"` (032), and `"85plus"` (01_utils) for the top group(s). These mismatches make merges by `age_group` silently fail. Centralize one age-banding helper in `01_utils_indonesia.R`, driven by the `00` config, and have every script call it.
- **`06_run_scenarios_indonesia_fair.R`**: check age-conditional logic (`over90 <- dt[age == 89]`, `age >= 20 & age <= 24` risk ladders, `expand_age()`, and `aod & age < 60` restrictions) still behaves once ages below 20 exist. Where an intervention genuinely applies to adults only, keep an explicit documented age restriction rather than relying on the data not containing young ages.

## Required implementation

### 1. Make script `00` the single source of truth

In `00_run_model_cvd_fair.R`, define a clear central model configuration that includes at least:

- modeled ages `0:95`, where numeric age `95` represents the open-ended GBD `95+ years` group;
- minimum and maximum model ages, or equivalent named settings;
- the long GBD cause names and stable short codes;
- `All causes` separately if it is needed for background-mortality calculations rather than as a disease-state model (it currently is — background mortality is derived as all-cause minus modeled causes);
- `Diabetes mellitus type 2` with short code `dm2`.

Preserve the currently intended modeled causes unless the data prove one cannot be supported. At minimum the configured set should include the existing cardiovascular causes plus type 2 diabetes. Do not duplicate the authoritative cause vector or cause map in downstream scripts.

Consider centralizing the age grid the same way causes are centralized (e.g. `age_single <- 0:95` plus an explicit GBD-band definition in `00`), so ages have a single source of truth too.

Add early validation in `00` for duplicate codes/names, a missing all-cause entry when required, invalid age bounds, and missing required configuration fields.

### 2. Remove downstream hard-coding of causes

Search the full active dependency chain for hard-coded cause vectors and cause-specific mappings, including `dx_include`, local `cause_map` objects, `fcase()`/`case_when()` mappings, fixed column lists, and assumptions about a fixed number of causes.

Refactor downstream code to consume the configuration created in `00`. Use named lookup tables or joins derived from the central map instead of repeated `fcase()` mappings wherever practical. Ensure all filtering and iteration use only configured causes, while retaining `All causes` only where needed to derive background mortality.

In particular, eliminate the separate `dx_include` in `032_adjustments_indonesia.R`, the inner default `cause_map` in `021`'s `project.all()`, and the repeated long-name→short-code mappings in `05`/`06`. Fail early with an informative message if a configured cause is absent from an input required for that stage. Do not silently drop type 2 diabetes — or any other configured cause — in a join, reshape, calibration, forecast, baseline, or scenario step.

Intervention functions may legitimately target only relevant causes. Such cause-specific effect mappings must explicitly leave non-target causes unchanged; they must not remove those causes from the model.

### 3. Extend the model correctly from ages 0 through 95+

Replace all active assumptions tied to ages 20–95, including constructs such as:

- `20:95`;
- initialization or cohort-entry rules using `age == 20`;
- updates restricted to `age > 20`;
- hard-coded age matching that begins at 20;
- interpolation only over ages 20–95;
- projection logic such as `age2 < 96` that treats age 95 as an ordinary terminal single-year age.

Use the central age configuration from `00` throughout loading, interpolation, calibration, baseline construction, and scenario projection.

Implement age 95 as an open-ended 95+ stock, not a cohort that disappears after one year. Each annual transition must age survivors from 94 into the 95+ group and retain survivors already in 95+ within that same group. Ensure deaths, incident cases, prevalent cases, well population, and total population are handled consistently in the terminal group without double counting.

Initialize newborns/age 0 correctly in each projection year using the population inputs. Do not mechanically reuse the old age-20 entrant logic. Confirm how the UNWPP/GBD input data (`PopulationsSingleAge0050.rds` and the GBD extracts) represent ages under 5, and convert GBD grouped ages to single-year ages using an explicit, documented method. Preserve totals when disaggregating grouped ages; do not assign a full grouped-age value to every single year. If the available production input genuinely cannot support single-year ages 0–4, stop with a precise explanation and implement the safest transparent preprocessing consistent with the repository data rather than inventing values.

Update age-to-GBD-group mappings to cover, at minimum, the correct labels for `<1 year`, `1-4 years`, subsequent five-year groups, and `95+ years`, based on the actual input labels. Centralize this mapping in a reusable function (in `01_utils_indonesia.R`) rather than copying it across calibration variants.

### 4. Add type 2 diabetes as a complete modeled cause

Although `00` currently contains `Diabetes mellitus type 2 = dm2`, verify that `dm2` survives every production stage and is not lost to hard-coded mappings or CVD-only filters.

For type 2 diabetes, verify and document:

- availability of deaths and prevalence targets in the GBD extract;
- derivation or loading of incidence, case fatality, and background mortality inputs;
- calibration participation;
- baseline state construction;
- scenario propagation;
- behavior under CVD interventions (normally unchanged unless an intervention explicitly maps to diabetes).

Do not fabricate intervention effects for diabetes. If a required epidemiologic input is missing, fail clearly and identify the exact missing source, fields, ages, years, and locations. If the existing generic well–sick–dead model can validly process diabetes with the available inputs, use that generic path rather than adding a special-case duplicate pipeline.

## Data availability pre-check (do this before heavy refactoring)

Confirm the GBD raw extracts under `../../data/raw/GBD/gbd2023-indonesia-fair/` actually contain (a) the younger age bands (`<1 year`, `1-4 years`, … `15-19 years`) and (b) the `Diabetes mellitus type 2` cause with Deaths and Prevalence, and that the population input covers ages 0–4. GBD often does not report every cause for the youngest ages (e.g. IHD in `<1 year`) and rates can be zero or missing. If the raw CSVs lack under-20 ages or dm2, **stop and tell me** — the code change alone will not achieve the goal and a new GBD extract will be needed. Otherwise, handle NA/zero rates gracefully (no NA propagation, no divide-by-zero) and report any cause×age cells you zero-filled or imputed.

## Testing and execution

Before changing code, inspect `git status` and preserve all unrelated user changes. Keep edits focused and do not overwrite input data.

Add lightweight automated tests or reproducible validation assertions. At minimum, test that:

1. The configured model ages are exactly `0:95`, with 95 documented and treated as 95+.
2. Outputs contain ages 0 and 95 for every expected year, sex, location, scenario, and configured modeled cause, subject only to explicitly documented source-data limitations.
3. `dm2` / `Diabetes mellitus type 2` is present after input processing, calibration, baseline construction, and scenario execution.
4. No cause outside the central configuration leaks into disease-model outputs, and no configured cause is silently dropped. (Grep confirms no `dx_include`/`cause_map` redefinition remains outside `00`, except the intentional dementia block gated by `run_aod_par`.)
5. Removing one nonessential cause from the configuration in a temporary test run causes downstream stages to use the reduced set without editing another script. Restore the production configuration afterward.
6. Adding it back restores the cause without downstream edits.
7. Baseline/no-intervention scenarios leave rates unchanged relative to the corresponding baseline rates, within numerical tolerance; adult-CVD numbers stay close to the pre-change baseline (sanity, not exact).
8. Interventions that do not target diabetes leave diabetes IR and CF unchanged.
9. State variables and transition probabilities are finite and nonnegative; probabilities remain within `[0,1]`.
10. Population/state accounting is coherent by age, sex, year, location, and scenario. Include an explicit terminal-age test proving that 95+ survivors remain in 95+ and age-94 survivors enter it without double counting.
11. Joins used to attach cause and age parameters do not unexpectedly increase row counts or create missing required values.

Run the full active `00`–`06` pipeline from a clean R session using the repository's normal production command. Do not claim success based only on parsing or isolated functions. Because full calibration is computationally expensive (parallel calibration, large scenario expansion), first run a small deterministic smoke test (e.g. a single location/sex/year with reduced iterations), confirm the plumbing, then run the full configured production pipeline. If you add temporary test toggles, remove them afterward and say so. Report commands, run time, warnings, and errors. Fix errors caused by this refactor and rerun until successful. Do not suppress warnings without explaining and resolving their cause. Keep the existing flag switches in `00` working (`run_calibration_par`, `run_aod_par`, `run_bgmx_trend`, `run_CF_trend`, `run_CF_trend_80`, etc.); do not turn on `run_aod_par`.

## Documentation and deliverables

Update concise inline documentation or the relevant README to explain:

- where ages and causes are configured;
- that age 95 represents 95+;
- how ages below 5 are disaggregated, if needed;
- how the open-ended terminal group evolves;
- how to add or remove a cause safely;
- what input fields a newly configured cause must provide.

At completion, provide:

1. A concise summary of the changes by file.
2. A description of the age-0 entry logic and the 95+ terminal-group mathematics.
3. Confirmation of how type 2 diabetes flows through each stage.
4. The exact commands used to test and run the pipeline.
5. Test results and key validation counts, including causes and age range at each major stage, NA/zero-fill counts, and the adult-baseline sanity comparison.
6. Any of the `06` cause vectors you deliberately left CVD-only, with reasoning, so the clinical intent can be confirmed.
7. Any unresolved data limitations or modeling assumptions.
8. `git diff --stat` and a focused summary of the substantive diff.

Do not modify scripts `07` onward unless necessary for compatibility, do not commit or push changes, and do not declare the task complete until the production `00`–`06` run and the acceptance checks pass. Ask before making any change that would meaningfully alter existing adult-CVD results, and before deleting anything you are not certain is truly redundant.
