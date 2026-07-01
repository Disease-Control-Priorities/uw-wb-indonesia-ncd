# Integrated Demography + CVD Runner — Implementation Plan

> Status: **planning only — no integration code written yet.**
> Scope: an integrated runner that executes the demography module and the CVD
> Indonesia pipeline coherently, with the demography projection as the single,
> reproducible source of population.

---

## 0. Scope recommendation (read first)

There are two ways to read "integrate the demography module with the CVD model,"
and they lead to very different amounts of work:

- **(A) Population handoff** — demography becomes the single, reproducible source of
  the population projection; the existing CVD Markov pipeline (`*_indonesia.R`)
  consumes it in place of the externally-staged `Populations*` files. Both engines
  keep their own disease logic.
- **(B) Full unification** — replace the CVD `_indonesia` Markov pipeline with the
  demography module's own CVD engine (`code/utils/engine.R`).

The project framing ("added to **align the population projections**", "passes the
aligned population projections into the CVD model", "preserves existing standalone
functionality") points squarely at **(A)**. This plan is built for (A) and treats
(B) as explicitly out of scope. Note that the demography module *already contains a
second, parallel CVD engine* (`engine.R`) — that redundancy is the main reason to be
deliberate about scope.

---

## 1. Demography module — entry points, inputs, outputs, dependencies

**Entry point:** `code/demography/00_run_all.R` sources scripts 01 → 02 → 03 → 04 →
05 → 06 → 07. Note: `01b_prepare_sbp_rr.R` is **not** wired into the orchestrator and
must be run by hand.

**Path/runtime model:** uses `here::here()` rooted at the repo's `.Rproj`, and sources
code from `here("R", ...)` and `here("scripts", ...)`. Data lives under
`here("data","gbd")`, `here("data","wpp")`,
`here("data","model",{calibration,baseline,scenarios})`, outputs under
`here("outputs",{tables,figures,validation})`.

| Script | Role | Key inputs | Key outputs |
|---|---|---|---|
| `01_prepare_gbd_inputs.R` | Raw GBD CSV → clean tables | `data/gbd/raw/{disease,risks}/*.csv` | `gbd_cause_deaths.csv`, `gbd_measures_full.csv`, `gbd_cause_fractions.csv`, `gbd_risk_paf.csv` |
| `01b_prepare_sbp_rr.R` | SBP relative-risk lookup | BoP curves + Singh 2013 (embedded) | `data/gbd/gbd_rr_sbp.rds`, `gbd_rr_sbp_hhd_sens.rds` |
| `02_build_demography.R` | **WPP2024 demographic backbone** | `wpp2024` R package | `data/wpp/indonesia_ncd_demography.Rda` → `sf.wpp[["IDN"]]` with `$base.pop`[2×101 @2025], `$mx`/`$mig`[131×2×101], `$years`=2025:2155; plus `get.lt` |
| `03_build_cause_fractions.R` | Logit-trend cause fractions / log-trend incidence | `gbd_cause_fractions.csv`, `gbd_measures_full.csv` | `gbd_frac_annual_1yr.csv`, `gbd_incidence_annual_1yr.csv` (single-year, annual 2000–2100) |
| `04_calibrate_modules.R` | Fit CVD TPMs + cervical | `gbd_measures_full.csv`, demography `.Rda` | `data/model/calibration/cvd_calibration_v1.rds`, `cervical_tpm.Rda` |
| `05_run_baseline.R` | Baseline projection + **population projection** | calibration RDS, demography `.Rda`, fraction/incidence CSVs | **`data/model/baseline/pop_df.rds`** (long: `year, sex∈{Female,Male}, age 0–100, pop`), `pop_array.rds`[n_yr×2×101], `baseline_*`, `q4030` |
| `06_run_scenarios.R` | 12 intervention scenarios | all of 05's outputs | `data/model/scenarios/scenario_*.csv` |
| `07_make_outputs.R` | Tables/figures | 05+06 outputs | `outputs/tables/*`, `outputs/figures/*` |

**The population deliverable** is produced by
`project_population_exposure(p_extended, get.lt)` in `code/utils/engine.R` and
serialized in `05_run_baseline.R` as `pop_df.rds` / `pop_array.rds`.

**Shared parameters** (`code/utils/cause_registry.R`): `run_years = 2025:2100`,
`ages = 0:100`, `sexes = c("Female","Male")`, annual cycle, `calib_year = 2023`.

**Dependencies:** `here`, `dplyr/tidyr/readr/purrr/tibble`, `data.table`, `abind`,
`countrycode`, `ggplot2/scales/cowplot`, and `wpp2024` (script 02 only) — declared in
`code/utils/packages.R`.

---

## 2. CVD model (Indonesia pipeline) — entry points, inputs, outputs, dependencies

**Entry point:** `code/cvd/00_run_model_indonesia.R`. Hard-codes `wd` to an absolute
Windows path (line 20), derives `wd_raw/wd_data/wd_outp` and an **external** `wd_temp`
(sibling `../temp/`), then `setwd("code/cvd/")` and sources in order: `01_utils.R` →
`02_load_inputs_indonesia.R` → `03_calibration.R` → `04_define_interventions.R` →
`05_build_baseline_indonesia.R` → `06_run_scenarios_indonesia_fair.R` →
`07_output_dalys.R` → `08_economic_value_calculation.R` → `09_validation_indonesia.R`.

**Path/runtime model:** every path is `paste0(wd_*, "...")` against the hard-coded
`wd`. No `here()`.

| Stage | Role | Key inputs | Key outputs |
|---|---|---|---|
| `021_get_base_rates_indonesia.R` | GBD 2023 → base rates | `data/raw/GBD/GBD2023-Indonesia/*.csv`, `PopulationsSingleAge0050.rds` | `baseline_rates_part*.rds`, `locs.rds` |
| `022_get_tps_indonesia.R` | Transition probs (IR/CF) | `baseline_rates_part*.rds` | `tps_inpt_part*.rds` |
| `03_calibration*` | Calibrate IR/CF to GBD | `tps_inpt_part*.rds`, GBD targets | `adjusted_searo_part*.rds` (or `adjusted_part*`), `calibration_*` |
| `04_define_interventions.R` | Scenario inputs | risk-factor CSVs | `sodium/tfa/statin_*`, `covfxn2.csv`, `htn_*` |
| `05_build_baseline_indonesia.R` | **Assemble baseline `b_rates`** + **population merge** | `adjusted*` RDS, **`PopulationsSingleAge0050.rds`**, **`PopulationsAge20_2050.csv`**, `bp_data6.csv`, `covfxn2.csv`, bgmx/CF trend RDS | in-memory `b_rates` |
| `06_run_scenarios_indonesia_fair.R` | Markov state engine | `b_rates`, ETIHAD/GBD RRs, scenario files | `output/out_model/<scenario>.rds` |
| `07_output_dalys.R` | YLL/YLD/DALYs | `out_model/*`, GBD YLD, WPP life table | DALY summaries |
| `08_economic_value_calculation.R` | Costs / VSL / VSLY | DALY + econ inputs | `output/08_*` |
| `09_validation_indonesia.R` | vs GBD 2023 & UNWPP 2024 | `out_model/*`, GBD, UNWPP deaths | `output/09_*` plots |

**Cross-cutting:** single-year ages **20–95** (95+ collapsed;
`dt_pop_unwpp[age>=95, age:=95]`), sex strings **"Male"/"Female"**, simulation
initialized at **2017** and projected via a 41-step loop to **2058** (interventions
effectively to 2050), causes = **6** (`ihd, istroke, hstroke, hhd, rhd, cmd`),
multi-location-capable but filtered to "Indonesia".

**Dependencies:** `data.table`, `dplyr/tidyr`, `ggplot2/RColorBrewer`, `readxl`,
`countrycode`, `stringr`, `parallel/doParallel/foreach`, `gmodels`, `forecast`.

---

## 3. Where demography output feeds into CVD — the integration seam

There is exactly **one** clean seam. The CVD model's population enters in
`code/cvd/05_build_baseline_indonesia.R` (lines 48–77):

```r
# line 49: historical/recent population
dt_pop_unwpp <- as.data.table(readRDS(paste0(wd_data,"PopulationsSingleAge0050.rds")))
dt_pop_unwpp[age>=95, age:= 95]
... b_rates[, Nx := ifelse(is.na(Nx2), Nx, Nx2)]      # replace Nx
# line 72: ages 20+ projection to 2050
pop20 <- read.csv(paste0(wd_data,"PopulationsAge20_2050.csv"))
... mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop=Nx)  # final pop
```

**Critical fact (verified):** `PopulationsSingleAge0050.rds` and
`PopulationsAge20_2050.csv` are **not produced by any script in `code/`** (a grep for
any `saveRDS/write.*Populations` returns no matches). They are pre-staged external
artifacts. So demography is meant to **replace this external prep step** with a
reproducible projection.

The demography side already produces the equivalent object — `pop_df.rds`
(`year, sex, age, pop`). The handoff is therefore: **demography emits a CVD-schema
population table → `05_build_baseline_indonesia.R` reads it instead of the two
external files.** Everything downstream of `b_rates` (calibration denominators, the
Markov loop's `Nx`/`pop`, DALYs, validation rate denominators) inherits the aligned
population automatically.

---

## 4. Inconsistencies between the two modules

| # | Dimension | Demography | CVD | Impact / required reconciliation |
|---|---|---|---|---|
| 1 | **Path rooting** | `here()` + `R/`, `scripts/` | hard-coded `wd` + `code/cvd/`, `paste0` | **Blocker.** Pick one convention (recommend `here()`). |
| 2 | **Dir layout actually exists?** | expects `R/`, `scripts/`, `data/gbd`, `data/wpp`, `data/model`, `outputs/` — **none exist** | uses `code/`, `data/raw`, `data/processed`, `output/` (singular) — exist | Demography **cannot run as committed**. Must remap dirs or create them. |
| 3 | **Output dir name** | `outputs/` | `output/` | Trivial but real; unify. |
| 4 | **Population age range** | 0–100 (101 ages) | 20–95, 95+ collapsed | Filter to 20–95 and sum ages 95–100→95 at the seam. |
| 5 | **Population year coverage** | projection starts **2025** (`base.pop`@2025) | initializes at **2017**, needs 2017–2050 | **Gap 2017–2024.** Demography must also emit WPP historical estimates for those years (available in `wpp2024::popAge1dt`) or CVD's start year must move to 2025. |
| 6 | **Sex encoding** | array idx 1=Female/2=Male; long form `"Female"/"Male"` | string `"Male"/"Female"` | Use the **long-format** `pop_df` (string-keyed) for handoff; do not pass raw arrays. |
| 7 | **Population units** | scaled to persons (~275M @2025) | `Nx` assumed persons | **Verify** the external `Populations*` files aren't in thousands; assert totals match within tolerance at the seam. |
| 8 | **Source vintage / path** | `wpp2024` package directly | externally-derived UNWPP-2024 CSV/RDS | Same vintage in principle; values may differ slightly — the integration's purpose is to make CVD use the demography projection consistently. |
| 9 | **Location key** | single-country "IDN"/"Indonesia" | multi-location; joins on `location=="Indonesia"` | Export must carry `location = "Indonesia"` and (if downstream needs it) `iso3 = "IDN"`. |
| 10 | **Cause vocabulary** | `ischemic_stroke`, `ich`; 4 causes; **no** rhd/cmd | `istroke`, `hstroke`; 6 causes incl. rhd, cmd | Only matters under scope (B). Irrelevant to population handoff — flagged for completeness. |
| 11 | **Redundant CVD engine** | full 4-cause TPM CVD engine in `engine.R` | the maintained 6-cause Markov pipeline | Scope decision: keep them separate (A) vs. merge (B). Recommend A. |
| 12 | **Projection horizon** | 2025–2100 (backbone to 2155) | 2017–2058 (interventions to 2050) | Export population over the **union** (2017–2100) so either standalone horizon is satisfied. |

---

## 5. Proposed architecture for the integrated runner

```
code/
  config.R                      # NEW — single source of truth: paths (via here()), MODEL_VERSION, run-mode flags
  00_run_integrated.R           # NEW — master orchestrator (demography → population export → CVD)
  utils/
    packages.R                  # (existing) shared deps
    paths.R                     # NEW (optional) — path helpers mapping logical names → real dirs
    cause_registry.R, engine.R, interventions_cvd.R, validation.R
  demography/
    00_run_all.R                # MODIFIED — source config.R; fix R/→utils, scripts/→demography, dirs
    01..07 + 01b                # MODIFIED — paths via config; create dirs if missing
    08_export_population.R      # NEW — write data/processed/population_projection_idn.rds (CVD schema)
  cvd/
    00_run_model_indonesia.R    # MODIFIED — drop hard-coded wd; source config; add use_demography_population flag
    05_build_baseline_indonesia.R  # MODIFIED — read integrated population behind a flag (fallback = old files)
    ...unchanged
```

**Design principles, mapped to the requirements:**

1. **Demography runs first** — `00_run_integrated.R` runs demography data-prep +
   backbone + a new dedicated population-export step *before* touching CVD.
2. **Aligned population passed in** — the new `08_export_population.R` writes one
   canonical artifact, `data/processed/population_projection_idn.rds`, already shaped
   to the CVD schema (`location, iso3, year, sex∈{Male,Female}, age∈20..95, Nx`),
   covering 2017–2100. `05_build_baseline_indonesia.R` reads it behind
   `use_demography_population`.
3. **Standalone preserved** — both `demography/00_run_all.R` and
   `cvd/00_run_model_indonesia.R` keep working alone. The CVD flag defaults can be set
   so that with `use_demography_population = FALSE` the old `Populations*` files are
   still used; the integrated runner sets it `TRUE`.
4. **No hard-coded paths** — `config.R` defines every directory through `here()` (the
   repo `.Rproj` makes the root unambiguous). The CVD `wd <- "C:/Users/..."` block and
   demography's `R/`+`scripts/` assumptions are both replaced by `config.R` lookups.
5. **Versioned outputs** — `config.R` defines a `MODEL_VERSION` string (set manually,
   not from a clock — keeps runs reproducible) and a
   `RUN_DIR <- here("output","runs", MODEL_VERSION)`. The orchestrator writes a small
   `manifest` (version, timestamp passed in as an argument, input file hashes, flag
   settings) alongside outputs so every run is traceable.

**Why a dedicated `08_export_population.R` rather than reusing `pop_df.rds`:**
`pop_df.rds` is a *byproduct of running the full demography baseline disease model*
(script 05). Coupling CVD to it would force the entire demography disease run before
any CVD run. A small export step depends only on the WPP backbone (`02`'s `.Rda` +
`project_population_exposure()`), so the population handoff is decoupled from
demography's disease modelling — cleaner and faster.

---

## 6. Specific files/functions to create or modify

**Create**

- `code/config.R` — `here()`-based path constants for both pipelines; `MODEL_VERSION`;
  `RUN_DIR`; default run-mode flags; a `make_run_dirs()` helper that creates any
  missing dirs (`data/gbd`, `data/wpp`, `data/model/*`, `output/runs/<ver>`).
- `code/demography/08_export_population.R` — load
  `data/wpp/indonesia_ncd_demography.Rda`; extend `p` to 2017–2100 (prepend WPP
  historical 2017–2024 from `popAge1dt`); call `project_population_exposure()`;
  reshape to long; collapse ages 95–100→95, filter 20–95; tag `location="Indonesia"`,
  `iso3="IDN"`; write `data/processed/population_projection_idn.rds`; assert
  non-negative, total-pop sanity, and schema parity with the legacy files.
- `code/00_run_integrated.R` — master orchestrator (see §7 steps).
- `docs/integration_plan.md` — this document.

**Modify**

- `code/demography/00_run_all.R` — `source(here("code","utils","packages.R"))`; source
  `code/config.R`; source steps from `code/demography/...`; add
  `08_export_population.R`; create dirs via `make_run_dirs()`.
- `code/demography/01..07`, `01b` — replace `here("R",...)`/`here("scripts",...)` and
  the `data/{gbd,wpp,model}` / `outputs` literals with `config.R` constants (or a
  `paths.R` shim). Lowest-risk variant: keep the `here(...)` calls but **create the
  directories** they expect and source utils from their real location.
- `code/cvd/00_run_model_indonesia.R` — remove the hard-coded `wd` (lines 20–35);
  source `code/config.R`; introduce `use_demography_population` (default `FALSE` for
  standalone, `TRUE` when invoked by the integrated runner).
- `code/cvd/05_build_baseline_indonesia.R` (lines 48–77) — branch on
  `use_demography_population`: when `TRUE`, read `population_projection_idn.rds` and
  join it into `b_rates` as `Nx`/`pop`; when `FALSE`, keep current behavior unchanged.

**Functions to add (in `engine.R` or a new `code/utils/population_io.R`)**

- `build_cvd_population(p_extended, years, age_max = 95, collapse_open = TRUE)` →
  returns the CVD-schema long table.
- `validate_population_handoff(pop_cvd, legacy_rds = NULL)` → schema + units +
  total-pop assertions (reuse the `stopifnot` style already in the demography scripts
  and `032_adjustments_indonesia.R`).

---

## 7. Step-by-step implementation plan

**Phase 0 — De-risk (investigate before writing).**

1. Confirm units of `PopulationsSingleAge0050.rds` / `PopulationsAge20_2050.csv`
   (persons vs thousands) and total 2025 Indonesia population, to set the handoff
   assertion tolerance (resolves inconsistency #7).
2. Confirm whether `wpp2024::popAge1dt` covers 2017–2024 for Indonesia (resolves the
   2017–2024 gap, #5). If not, decide the fallback (carry-back or move CVD start to
   2025).
3. Confirm which `03_calibration*` variant is canonical for the integrated run (the
   entry script currently sources the generic `03_calibration.R`; CLAUDE.md flags the
   transparent variant + the double-calibration gotcha).

**Phase 1 — Path unification (foundation).**

4. Write `code/config.R` with `here()` paths + `MODEL_VERSION` + `make_run_dirs()`.
5. Repoint `cvd/00_run_model_indonesia.R` to `config.R`; verify the CVD pipeline still
   runs standalone (regression check: outputs identical to a pre-change run).
6. Repoint demography scripts to `config.R` / create expected dirs; verify
   `demography/00_run_all.R` runs standalone end-to-end (this is the first time it will
   actually run in this repo).

**Phase 2 — Population export.**

7. Implement `build_cvd_population()` + `08_export_population.R`; produce
   `population_projection_idn.rds`.
8. Run `validate_population_handoff()` comparing the new table against the legacy
   `Populations*` files (age coverage, sex labels, totals by year, units).

**Phase 3 — CVD consumption.**

9. Add `use_demography_population` branch in `05_build_baseline_indonesia.R`.
10. Run CVD with the flag `TRUE`; diff `b_rates` totals and `output/out_model/*` deaths
    against the legacy-population run to quantify the population-swap effect.

**Phase 4 — Orchestrator + versioning.**

11. Write `code/00_run_integrated.R`: source `config.R` → `make_run_dirs()` →
    demography prep (01,01b,02,03,04 with skip flags when artifacts exist) →
    `08_export_population.R` → CVD pipeline with `use_demography_population=TRUE` →
    write `manifest`.
12. Stamp outputs under `output/runs/<MODEL_VERSION>/` and emit the run manifest.

**Phase 5 — Validation & docs.**

13. Re-run `09_validation_indonesia.R` against GBD 2023 / UNWPP 2024 under the
    integrated population; confirm the population denominator is now internally
    consistent across CVD and demography.
14. Update `CLAUDE.md` (new entry point, flags, the handoff artifact) and keep this
    `docs/integration_plan.md` current.

---

## 8. Decisions to confirm before building

1. **Scope** — confirm **(A) population handoff** (recommended) vs. (B) merging
   engines. Everything above assumes (A).
2. **Path strategy** — adopt **`here()` everywhere** (recommended, removes the
   hard-coded `wd`) vs. a thinner adapter that leaves CVD's `wd` in place. The full
   `here()` route touches more files but is the durable fix.
3. **CVD initialization year** — keep CVD starting at **2017** (so demography must emit
   historical 2017–2024) vs. realign CVD to **2025** to match the demography backbone.
   This is the single biggest assumption-level choice.
4. **Calibration variant** for the integrated run — generic `03_calibration.R` (current
   default) vs. `03_calibration_indonesia_transparent.R` (note the
   `run_adjustment_model <- FALSE` gotcha).
