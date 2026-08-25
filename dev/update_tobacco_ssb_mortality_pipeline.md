# Claude Code prompt — implement tobacco→mortality (Jha) and SSB→diabetes-mortality effects in the Indonesia NCD R pipeline

> Paste everything below the line into Claude Code, run from the repo root
> (`.../uw-wb-indonesia-ncd/`). It updates **Models 00, 04, 05, 06, 09** so the
> pipeline actually ingests, validates, applies, costs and reports the new
> `sick → dead` (case-fatality / mortality) effects that were just added to the
> public-health input workbook. Today those rows exist in the workbook but the R
> code silently drops them.

---

## Role & goal

You are editing an R discrete-time Markov (state-transition) NCD model for
Indonesia — states **well → sick → dead**, by single-year age, sex, cause, year.
The **public-health input workbook was updated** so tobacco-control policies now
also act on **mortality** (a `sick → dead` case-fatality effect) based on
**Cho / Jha et al. 2024, *Smoking Cessation and Short- and Longer-Term
Mortality*, NEJM Evidence (DOI 10.1056/EVIDoa2300272)**, in addition to their
existing `well → sick` incidence effect. An **exploratory SSB-tax → type-2
diabetes mortality** (`sick → dead`) link was also added, **disabled by
default**.

The workbook is the authoritative contract and already carries every parameter.
Your job is to add the *mechanics* to consume the two new effect models and the
`sick → dead` transition — **without inventing effect sizes in code** and
**without breaking existing incidence-only behaviour**.

**Do not change the numbers.** Every effect size, hazard ratio, ERD timing
scalar, prevalence, exposure/coverage path, transition and cost is read from the
Excel workbook by **column name**. The only new constants in code are structural
(age-band edges; the M16/M17 algebra). **Do not add/rename workbook sheets or
columns, change column types, or restructure the Excel file** — it is an R-model
input; use its existing sheets and fields.

---

## Input files

- **New authoritative workbook (supersedes the previous one):**
  `data/indonesia_model_inputs_public_health_updated_mortality.xlsx`
  Point the pipeline at this file, keeping a graceful fallback chain
  (`_mortality` → previous `_updated` → non-suffixed).
- Sheets used: `Assumptions`, `Intervention_Cause_Map`, `Policy_Levers`,
  `Exposure_Targets`, `Effect_Parameters`, `Risk_Response`, `Model_Input_View`,
  `Cost_Components`, `Countdown_Methods`, `Scenario_Hierarchy`, `QA_Checks`.
- Evidence PDF (reference only, do **not** parse at runtime): Cho/Jha et al. 2024.

### Verified workbook diff — what changed (this is the spec)

**`Intervention_Cause_Map`** — 13 new links (`transition_from → transition_to`, `include_flag`):

- **12 tobacco `sick → dead` links** = **4 tobacco policies × 3 CVD causes**, all `include_flag = 1`:
  - policies: `I_PH_TOBACCO_TAX`, `I_PH_TOB_CLEAN_AIR`, `I_PH_TOB_MEDIA`, `I_PH_TOB_AD_BAN`
  - causes: `C_IHD` (ihd), `C_IS` (istroke), `C_ICH` (hstroke)
  - keys like `I_PH_TOBACCO_TAX__C_IHD__SICK_DEAD`; `cost_scope` unchanged from the incidence rows (`shared-count-once` / `component-count-once`).
- **1 SSB `sick → dead` link**: `I_PH_SSB_TAX__C_T2DM` (cause `dm2`), **`include_flag = 0` (disabled by default)**.
- The pre-existing tobacco `well → sick` incidence rows keep their transition but their **timing model changed** (below).

**`Effect_Parameters`** — two new `effect_model` values + a lag swap on existing rows:

- New `EFF_049…EFF_060` (tobacco `sick → dead`): `effect_model = tobacco_mortality_prevalence_shift_rr`, `response_key = HR_SMOKING_VASC_MORT_POOLED` (=3; **R must use sex-specific 2.9 M / 3.1 F**), `lag_model = jha_piecewise_shared_scalar`, `review_status = Proxy`, `qa_status = REVIEW`.
- New `EFF_061` (SSB `sick → dead`): `effect_model = direct_loglinear_rr_per_unit_reduction`, `response_key = RR_SSB_T2DM_ALLCAUSE_MORT_1SERV` (=1.08 HR per serving/day), `lag_model = immediate_after_full_implementation`, `review_status = Exploratory`.
- Existing tobacco incidence rows `EFF_001…EFF_020`: `lag_model` changed `delayed_exponential_remaining_effect → jha_piecewise_shared_scalar`; `lag_parameter` (0.0616) is now **sensitivity-only**; `effect_model` stays `direct_smoking_prevalence_shift_rr`.

**`Risk_Response`** — new selected rows to read:

- `HR_SMOKING_VASC_MORT_MALE = 2.9`, `HR_SMOKING_VASC_MORT_FEMALE = 3.1`, `HR_SMOKING_VASC_MORT_POOLED = 3.0` (pooled = backward-compatible fallback only).
- Full grid **`SCALAR_JHA_{M,F}_{20_39,40_49,50_59,60_79,80_95}_{LT3,Y3_9,GE10}`** = Jha excess-risk-difference (ERD) proportions by sex × age-band × years-since-cessation; ages 80–95 reuse the 60–79 scalars. `LAG_TOB_CVD` is now `Sensitivity` (normalized exponential, r = 0.0616).
- `RR_SSB_T2DM_ALLCAUSE_MORT_1SERV = 1.08` (`Sensitivity`, disabled by default).

**`Assumptions`** — new parameters (read by name, do not hard-code):
`tobacco_cvd_lag_model_base = jha_piecewise_shared_scalar`,
`tobacco_cvd_lag_model_sensitivity = normalized_exponential_lag`,
`tobacco_cvd_full_effect_year = 10`,
`tobacco_vascular_mortality_rr_male = 2.9` / `..._female = 3.1` / `..._pooled = 3.0`,
`tobacco_vascular_erd_ge10_pooled = 0.90125`,
`tobacco_age80_scalar_extrapolation = use_age_60_79`,
`transition_probability_rr_method = 1-(1-p0)^RR`.

**`Countdown_Methods`** — the formulas to implement: **M12** (tobacco lag: base = age-sex-duration Jha scalar on annual quitting cohorts; sensitivity = normalized exponential to full effect at year 10), **M16** (tobacco mortality full effect), **M17** (rate-ratio → annual probability), **M18** (SSB mortality sensitivity, disabled by default, all-cause mortality among adults with T2DM).

**`QA_Checks`** now expects: 13 `sick → dead` links (12 tobacco + 1 SSB), 12
tobacco-CVD `sick → dead`, **0 SSB mortality links enabled** in the base case,
and all tobacco-CVD links using `jha_piecewise_shared_scalar`. The workbook
README explicitly notes *"the attached R code still requires transition and
lag-model support."* — that is this task.

---

## Current code problems (confirmed — fix these exactly)

1. **`04_define_interventions_indonesia.R`**, in `.build_public_health_catalogue`:
   `translate_transition(from, to)` maps **only** `well → sick → "incidence"` and
   returns `NA` for everything else, so every new `sick → dead` link is
   dropped/invalidated. `reproduce_full_effect()` handles only
   `direct_smoking_prevalence_shift_rr`, `direct_loglinear_rr_per_unit_reduction`
   and `tfa_attributable_ihd_PAF*` — it does **not** know
   `tobacco_mortality_prevalence_shift_rr` and applies no age-sex-duration Jha
   timing. **Note:** this file may contain older/duplicate loader definitions;
   identify which is active at runtime and update *that* one — do not patch an
   obsolete block while the active one is unchanged.
2. **`06_run_scenarios_indonesia_fair.R`**: the public-health apply path
   (`interventions == "ph_wb"`) **errors/stops** on any `model_transition` other
   than `"incidence"` ("Public-health effects act on incidence only"). The
   clinical `fair_wb` path already dispatches both `"incidence"` (IR/`eff_ir`) and
   `"case_fatality"` (CF/`eff_cf`) — **mirror that logic** for `ph_wb`. The Jha
   piecewise timing and the M16/M17 mortality→CF conversion are not implemented.
3. **`00`, `05`, `09`** need the supporting changes described below.

---

## A. File-by-file change map

Keep the project's single-source-of-truth discipline: causes/ages declared only
in Model 00; analytic assumptions live only in the workbook; never duplicate or
hard-code effect sizes, HRs, scalars, prevalences, transitions or costs in R.

**`00_run_model_cvd_fair.R`**
- Repoint `public_health_inputs_file` to `..._updated_mortality.xlsx` with the fallback chain above.
- Add one clearly-named timing switch, default reproducible from the workbook:
  ```r
  tobacco_timing_analysis <- "base"  # "base" (jha_piecewise_shared_scalar) | "normalized_exponential_lag"
  ```
  `base` must use the workbook's Jha model; the sensitivity option overrides **only** tobacco timing and reads `tobacco_cvd_lag_rate = 0.0616` from the assumption row, not a scattered constant.
- Add `run_ssb_diabetes_mortality <- FALSE` — when `TRUE` (and only then) include the exploratory `I_PH_SSB_TAX__C_T2DM` link despite its `include_flag = 0`.
- Confirm `dm2` stays in `cause_map`.

**`04_define_interventions_indonesia.R`** — see requirements B1, B2, B4, B5, B6, B9.

**`05_build_baseline_indonesia.R`** — ensure the `dm2` cause carries a valid baseline `sick` stock and case-fatality (`CF`) through the same steps as CVD causes, so the diabetes `sick → dead` effect has something to act on when enabled. If dm2 is excluded from any secular/CF-trend join, extend it via the central `cause_lookup` or explicitly document that dm2 CF is held flat. **Never emit NA/zero `sick`/`CF` for dm2.** Make no other analytic change here.

**`06_run_scenarios_indonesia_fair.R`** — see requirements B6, B7, B8.

**`09_cost_value.R`** — see requirement B10.

---

## B. Required behaviour (with math)

### B1. Accept both public-health transition pathways
Extend the Model 04 contract/validation so `well → sick → incidence` **and**
`sick → dead → case_fatality` are both valid; reject/flag anything else. Carry the
original transition fields and `model_transition` through the catalogue and every
join. **Join effects on the complete key** to prevent cross-application:
```text
intervention_id, year, cause_id/cause_code, sex, age/age_group, transition_from, transition_to (or model_transition)
```
A `well → sick` effect must never be applied to `sick → dead` or vice-versa.
Respect `include_flag`; the exploratory SSB mortality row stays excluded unless
explicitly enabled.

### B2. Support the new effect and lag models
Add support for the workbook identifiers actually used:
```text
tobacco_mortality_prevalence_shift_rr   jha_piecewise_shared_scalar   normalized_exponential_lag
```
Retain `direct_smoking_prevalence_shift_rr`, `direct_loglinear_rr_per_unit_reduction`,
`delayed_exponential_remaining_effect`, `immediate_after_full_implementation` for
backward compatibility. Never silently drop a valid tobacco CVD row: an
unsupported *enabled* model or malformed parameter must raise a clear validation
failure. Run with **strict validation on during development**; preserve the
user-facing strict/non-strict switch afterward.

### B3. Preserve existing tobacco policy and exposure calculations
Continue deriving tobacco policy effects from Excel (do not hard-code
intervention-specific reductions): tobacco tax → implied price change from
baseline/target excise shares × price elasticity; smoke-free law / mass-media /
ad-ban → workbook regulatory implementation gap × effect parameter. Compute
target smoking prevalence from the workbook baseline, reduction method,
override/floor, start year, target year and scale-up shape. The exposure path
stays **annual**; for a relative reduction \(d_t\): \(p_t = p_0(1-d_t)\).

### B4. Full tobacco **incidence** effect
Per tobacco intervention × cause × year × age × sex, using the pathway-specific
current-smoker RR from the workbook:
\[
M_0 = 1 + p_0(RR_c - 1),\quad M_{t,\mathrm{inc}} = 1 + p_t(RR_c - 1),\quad
RR_{t,\mathrm{inc,full}} = \frac{M_{t,\mathrm{inc}}}{M_0}.
\]
Use the IHD, ischemic-stroke, ICH (and T2DM) response keys already in Excel. Do
not substitute a pooled value where a sex/age-specific one exists.

### B5. Full tobacco **mortality** effect (Countdown M16)
For enabled tobacco `sick → dead` rows, implement the workbook residual-risk
model. With \(ERD_{a,s,10+}\) the ≥10-year-cessation excess-risk reduction and
\(RR_{c,s}\) the sex-specific current-smoker vascular-mortality RR:
\[
RR_{q,a,s,10+} = 1 + (1 - ERD_{a,s,10+})(RR_{c,s} - 1),
\]
\[
RR_{t,a,s,c,\mathrm{mort,full}} =
\frac{1 + p_t(RR_{c,s}-1) + (p_0 - p_t)(RR_{q,a,s,10+}-1)}{1 + p_0(RR_{c,s}-1)}.
\]
Read sex-specific vascular-mortality RRs (2.9 M, 3.1 F), ERD values, age bands and
lookup keys from `Risk_Response`/`Assumptions`. Pooled 3.0 is a fallback only.
Use the documented 60–79 scalar for ages 80+ and **record that fallback in
diagnostics**. Keep this pathway **labelled in metadata/output as a
sensitivity/proxy for post-diagnosis case fatality** (evidence is vascular
mortality, not disease-specific case fatality among diagnosed patients).

### B6. Annual quitting cohorts + Jha base-case timing (M12)
**Separate magnitude from timing.** From the annual prevalence trajectory,
intervention-attributable quitting cohorts:
\[
q_u = \max(p_{u-1} - p_u,\, 0).
\]
For age \(a\), sex \(s\), cessation-duration category \(d \in \{<3, 3\text{–}9, 10+\}\):
\[
\lambda_{a,s,d} = \min\!\left(1,\ \frac{ERD_{a,s,d}}{ERD_{a,s,10+}}\right),\quad \lambda \in [0,1].
\]
Cohort-weighted scalar in calendar year \(t\) (return 0 when the denominator is 0):
\[
\bar\lambda_{a,s,t} = \frac{\sum_{u\le t} q_u\,\lambda_{a,s,t-u}}{\sum_{u\le t} q_u}.
\]
Use the **same** cohort-weighted timing scalar for tobacco incidence and
mortality while keeping their different full-effect RRs:
\[
RR_{\mathrm{effective}} = 1 - \bar\lambda\,(1 - RR_{\mathrm{full}}).
\]
Do **not** use only "years since policy start"; each annual cohort accumulates
cessation duration separately. In Model 06, add the `case_fatality` branch to the
`ph_wb` apply path mirroring `fair_wb` (mortality → `CF`/`eff_cf`; incidence →
`IR`/`eff_ir`), keeping effects multiplicative and stackable with existing
`eff_*_*` accumulation.

### B7. Normalized-exponential sensitivity timing
```text
normalized_exponential_lag ;  tobacco_cvd_lag_rate = 0.0616 ;  full_effect_year = 10
```
Implement a reusable function equivalent to:
```r
tobacco_lag_fraction <- function(years_since_cessation, lag_rate = 0.0616, full_effect_year = 10L) {
  years       <- pmax(years_since_cessation, 0)
  denominator <- 1 - (1 - lag_rate)^full_effect_year
  fraction    <- (1 - (1 - lag_rate)^years) / denominator
  pmin(pmax(fraction, 0), 1)
}
```
Validate `0 < lag_rate <= 1` and `full_effect_year > 0`. Apply it to annual
quitting cohorts with the **same cohort weighting** as the Jha model (do not treat
all quitters as quitting in the start year). This path is selected only by
`tobacco_timing_analysis = "normalized_exponential_lag"` and overrides only
tobacco timing. Keep the deprecated legacy lag parameter readable for
traceability but unused in the base case.

### B8. Apply rate ratios correctly to Markov transitions (M17)
Add a tested helper:
```r
rate_ratio_to_probability <- function(p0, rr) {   # validate p0 in [0,1], rr >= 0
  1 - (1 - p0)^rr
}
```
At the point annual transition probabilities are updated, apply
\(p_1 = 1 - (1 - p_0)^{RR_{\mathrm{effective}}}\). **Never** compute `p0 * RR` or
`p0 * (1 - effect)` for incidence or case fatality. **Determine whether the
upstream object stores rates or probabilities**: if a continuous rate, multiply
the rate by RR and convert **once**; if an annual probability, use the formula
above. Document the determination in a code comment; avoid double conversion.
When several interventions hit the same transition, combine rate ratios
multiplicatively and convert the baseline probability **once**; preserve the
package/scenario hierarchy and do not double-count child interventions in joint
scenarios. Keep probabilities finite and in `[0,1]` and preserve stock-flow
identities.

### B9. Exploratory SSB mortality row (M18) — safe handling
Workbook SSB–T2DM `sick → dead` relationship: all-cause mortality HR ≈ 1.08 per
additional serving/day; **exploratory, disabled by default**.
- The loader must validate and carry this row without affecting results while disabled.
- If enabled (`run_ssb_diabetes_mortality = TRUE`), apply its exposure-response model (`direct_loglinear_rr_per_unit_reduction`, effect \(= 1 - 1/RR^{(p_0 - p_t)}\)) to the **dm2** `sick → dead` transition using the workbook exposure change and response parameter.
- Label it clearly as an **all-cause mortality proxy, not diabetes-specific mortality**. Do not invent an intake-mediation parameter or silently reinterpret the HR.

### B10. Model 09 outputs & QA
Update `09_cost_value.R` only where necessary so it no longer claims all
public-health effects are incidence-only. It must: report both `incidence` and
`case_fatality` links; show transition-specific full and effective RRs/effects by
intervention, cause, age, sex, year where available; retain the original workbook
transition fields; distinguish base-case Jha timing from the exponential
sensitivity; **respect `shared-count-once` / `component-count-once` cost scopes**
so a policy's cost is counted once (not once per cause or per transition); ensure
the new `sick → dead` links flow through **deaths-averted** without
double-counting; and replace stale QA assertions ("all modeled public-health
effects map to incidence") with checks that both mappings are valid and no
cross-pathway application occurred. The exploratory SSB link must appear
**disabled** in the base-case audit. **Do not change deaths, cases or cost
formulas merely to make a QA check pass.**

---

## C. Reusable functions
Use small, testable functions rather than one large loop. Implement equivalents
(adapt names to repo style, but keep magnitude, timing and transition-application
separate):
```text
build_tobacco_scalar_matrix()   expand_tobacco_exposure()   build_effective_tobacco_scalars()
calculate_tobacco_transition_effects()   rate_ratio_to_probability()   apply_tobacco_effects_to_transitions()
```

---

## D. Guardrails
- **No magic numbers** — HRs, ERD scalars, prevalences, exposure/coverage paths, transitions and costs come from the workbook by column name; even band→scalar keys come from the `SCALAR_JHA_*` rows.
- **Backward compatibility** — with SSB off and timing at the workbook default, every existing scenario's results are numerically unchanged **except** the intended tobacco timing shift (exponential-lag → Jha piecewise).
- **Fail loud, not silent** — no `sick → dead` link is ever dropped without a logged, consolidated diagnostic.
- Respect the existing family switches (`run_public_health_interventions`, `run_clinical_interventions`) and `strict_model_input_validation`.
- Modify only the files/active code paths required; preserve unrelated logic and outputs; do not refactor unrelated code.

---

## E. Validation & tests
Add focused tests / executable QA covering at least:
1. `translate_transition("well","sick") == "incidence"` and `translate_transition("sick","dead") == "case_fatality"`.
2. `rate_ratio_to_probability(0.10, 0.927)` ≈ `0.0930`.
3. Normalized-exponential timing = 0 at year 0, monotonic, in `[0,1]`, = 1 at year 10 and after.
4. Jha scalars in `[0,1]`, correct age-sex band, documented age-80 fallback used.
5. A quitter entering 2030 has less accumulated effect in 2030 than one entering 2026.
6. With \(p_t = p_0\), tobacco full and effective RR = 1.
7. No annual quitters ⇒ cohort-weighted scalar 0 ⇒ no transition effect.
8. Sex-specific mortality RRs used instead of the pooled fallback.
9. Incidence effects modify only `well → sick`; mortality effects modify only `sick → dead`.
10. The disabled SSB mortality row does not alter outputs; enabling it changes only dm2 `sick → dead`.
11. All enabled tobacco CVD incidence and `sick → dead` rows pass Model 04 validation and reach Model 06; catalogue counts match the workbook QA (13 `sick → dead`; 12 tobacco-CVD; 0 SSB enabled base-case).
12. Baseline results unchanged; state counts nonnegative; probabilities valid; stock-flow identity passes; `eff_cf` in `[0,2]`.
13. Existing non-tobacco public-health and clinical scenarios remain reproducible.

Run the smallest representative end-to-end model that demonstrates **both** timing
modes, and produce a compact comparison for one tobacco intervention (preferably
tobacco tax), one cause (IHD), both sexes, selected years to 2050, showing:
```text
baseline smoking prevalence ; policy smoking prevalence ; quitting-cohort weight/timing scalar
full incidence RR ; effective incidence RR ; full mortality RR ; effective mortality RR
baseline & intervention incidence probability ; baseline & intervention case-fatality probability
```

---

## F. Acceptance criteria & final report
Complete only when: the base run uses `jha_piecewise_shared_scalar`; a config
change runs `normalized_exponential_lag` with the Excel `0.0616`; enabled tobacco
IHD / ischemic-stroke / ICH incidence **and** `sick → dead` rows are not dropped;
T2DM tobacco incidence remains supported; the disabled SSB mortality row stays
inert; rate ratios go through the correct rate/probability conversion; Model 09
documents and audits both pathways; and the full pipeline
(`00_run_model_cvd_fair.R`, public-health family on) completes without
formula/validation errors with unrelated scenarios unchanged.

Then report: (1) files and functions changed; (2) the active data flow Excel →
Model 04 → Model 06 → Model 09 (and which loader definition was the active one);
(3) how to run the base case and the sensitivity; (4) validation/test results and
the comparison table; (5) any remaining evidence or modeling limitations
(e.g. dm2 baseline CF handling in Model 05) flagged for the analyst to confirm.

### Work method
Read all five scripts and the new workbook first; confirm the current
`translate_transition` / `reproduce_full_effect` / `ph_wb` behaviour (and resolve
any duplicate loader definitions) before editing. Make minimal, well-commented,
targeted diffs; after each file, state what changed and why in one or two lines.
