# 🌍 Indonesia NCD Markov Simulation Model  
### Integrating Cardiovascular Disease and Cancer using State-Transition Models

## 📌 Overview

This repository contains a **population-based simulation framework** to evaluate the health and economic impact of **noncommunicable disease (NCD) policies in Indonesia**, focusing on:

- ❤️ Cardiovascular diseases (CVD)
- 🎗️ Cancer

Unlike models such as NCDSim, which rely on **system dynamics**, this framework is built using **discrete-time Markov (state-transition) models**.

---

## 🎯 Scientific Motivation

Indonesia faces a growing burden of NCDs driven by:

- Population aging  
- High smoking prevalence  
- Rising obesity and diabetes  
- Limited cancer screening access  

---

## 🧠 Modelling Framework

Population → Risk Factors → { CVD Markov Model + Cancer Markov Model } → Outcomes

---

## ⚙️ Repository Structure

/data  
/code  
/outputs  
/docs  

---

## ▶️ How to Run

```r
install.packages(c("data.table", "tidyverse", "openxlsx"))
source("code/06_run_simulation.R")
```

---

## 🧪 Policy Scenarios

- Hypertension control  
- Sodium reduction  
- Trans-fat elimination  
- Statins  
- Cancer screening  
- Treatment adherence  

---

## ⚠️ Assumptions

- Conditional independence across diseases  
- PAF-based incidence modeling  
- No comorbidity (v1)  

---

## 🔧 Model configuration — ages & causes (single source of truth)

Ages and modeled causes are declared **only** in `00_run_model_cvd_fair.R`. Every
downstream script (`021`–`06` and the calibration) derives its age grid, GBD
band mapping, cause filtering, loops, joins, validation and outputs from these
objects — do **not** re-declare them anywhere else.

**Where things are configured (`00_run_model_cvd_fair.R`):**

- `cause_map` — stable short code → long GBD cause name. Keep the all-cause
  envelope (`all = "All causes"`) **last**; it is not a disease-state model, it is
  used only to derive background mortality as *all-cause minus the sum of modeled
  causes*. Derived automatically: `dx_include` (GBD names to keep),
  `model_cause_codes` / `model_cause_names`, `all_cause_code` / `all_cause_name`.
- `min_model_age = 0`, `max_model_age = 95`, `age_single = 0:95`. **Numeric age 95
  represents the open-ended GBD `95+ years` group** (all survivors aged 95+ are
  pooled into this terminal stock).
- An early `local({ ... })` validation block fails fast on duplicate codes/names,
  a missing all-cause entry, invalid age bounds, or a band map that does not cover
  every model age.

**Central GBD age-band helper (`01_utils_indonesia.R`):** `gbd_age_bands()`,
`gbd_band_label()`, `gbd_band_midpoint()` provide the one authoritative single-age
↔ GBD-band mapping (labels match the raw GBD extract exactly, e.g. `<1 year`,
`12-23 months`, `2-4 years`, …, `95+ years`). Interpolation midpoints are band
centres (band lower + 2 for the regular 5-year bands; 0/1 for the single-year
infant/toddler bands; 95 for the open terminal).

**Ages below 5 / disaggregation:** GBD reports the youngest ages as `<1 year`
(= age 0), `12-23 months` (= age 1) and `2-4 years` (= ages 2–4). Band-level
rates are interpolated to single years of age across their band midpoints
(`021`, `022`); the two single-year infant bands are exact anchors, the 3-year
`2-4 years` band is spread smoothly by interpolation (rates, not counts, so no
value is copied verbatim to every year). Population comes from
`PopulationsSingleAge0050.rds`, whose `age` column is a **1-based index**
(index 1 = actual age 0); it is converted to actual age (`age - 1`) in `021` and
`05` and pooled at ages ≥ 95 into the terminal group.

**Age-0 entry & 95+ terminal (full-lifecycle model, `05`/`06` and calibration):**
Each projection year, newborns enter at **age 0** seeded from that year's
single-age population × observed prevalence (≈ 0 for CVD/dm2, so newborns are
almost entirely "well"); there is no age-20 boundary. Every cohort ages one year.
The **95+ group is open-ended**: each step, survivors aged from 94 enter it *and*
survivors already in 95+ are retained (both pooled and summed), so the terminal
stock accumulates rather than disappearing after one year.

**Type 2 diabetes (`dm2`)** is a fully modeled cause. GBD reports Deaths only from
age 15+ for dm2 (as for IHD/HHD); absent young-age Deaths are treated as 0 (no
burden). CVD interventions do not target dm2, so its IR/CF are left unchanged by
them (the diabetes-specific antihypertensive intervention acts on CVD outcomes in
the diabetic subgroup, not on dm2 incidence/case-fatality).

**To add or remove a modeled cause** (input data permitting): edit `cause_map`
(and nothing else). A newly configured cause must have, in the GBD extract under
`data/raw/GBD/gbd2023-indonesia-fair/`, **Deaths and Prevalence** by
`sex × age_name × year` for Indonesia across the calibration window (2009–2019);
young-age gaps are tolerated (zero-filled). Removing a cause automatically drops it
from every filter, loop, join, calibration combo, baseline and scenario output.
Intervention-effect maps (e.g. the blood-pressure RR table, statin/TFA/FAIR
targets) are intentionally cause-specific and leave non-target causes unchanged.

## 📜 License

MIT

---

## 📚 Citation

Forthcoming
