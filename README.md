# 🌍 Indonesia NCD Decision Science & Implementation Modeling Platform
### Integrating Cardiovascular Disease and Cancer Policy Modeling for Indonesia

## 📌 Overview

This repository contains a population-based simulation and decision science framework to evaluate the health, economic, and equity impacts of noncommunicable disease (NCD) policies in Indonesia, with a primary focus on:

- ❤️ Cardiovascular diseases (CVD)
- 🎗️ Cancer

The platform integrates:

- **Decision science**
- **Implementation science**
- **Economic evaluation**
- **Equity analysis**
- **Health systems optimization**

to support evidence-informed policymaking and health system strengthening in Indonesia.

Unlike models such as NCDSim, which rely primarily on system dynamics approaches, this framework is built using discrete-time Markov (state-transition) and microsimulation models for policy evaluation and prioritization.

The work supports ongoing collaborations involving the University of Washington, the World Bank, and the Indonesian Ministry of Health to improve cardiovascular disease prevention, treatment, and mortality reduction strategies.

---

## 🎯 Scientific Motivation

Indonesia faces a rapidly growing burden of NCDs driven by:

- Population aging
- High smoking prevalence
- Rising obesity and diabetes
- Limited cancer screening access
- Uneven access to high-quality chronic disease care
- Regional disparities in health system performance

Recent policy priorities from the Indonesian Ministry of Health include improving implementation of the CKG cardiovascular screening program, strengthening treatment adherence, improving supply chains, and scaling high-value cardiovascular interventions to reduce premature mortality.

---

## 🧠 Decision Science Framework

This repository applies **decision science** methods to inform policies and practices that improve population health by systematically integrating:

- Scientific evidence
- Population epidemiology
- Health system constraints
- Individual and societal values

across outcomes such as:

- Mortality
- Quality of life
- Costs
- Equity
- Health system efficiency

The framework evaluates alternative intervention strategies under realistic implementation and resource constraints to support priority setting and long-term policy planning.

---

## 🏥 Implementation Science

The platform incorporates **implementation science** approaches to evaluate how evidence-based interventions can be effectively delivered at scale within the Indonesian health system.

Key implementation strategies under consideration include:

- Strengthening medicines supply chains
- Improving treatment adherence
- Enhancing primary care delivery
- Expanding hypertension and diabetes control
- Improving quality of care in puskesmas
- Regional implementation targeting
- Health system performance monitoring

The modeling framework explicitly considers implementation barriers, scale-up feasibility, and regional heterogeneity in baseline capacity and program performance.

---

## 🎯 Intervention Goals

The overarching intervention goals are:

### Prioritization
Identify the highest-value NCD interventions capable of producing the largest population health gains under realistic resource constraints.

### Optimization
Optimize health system efficiency by improving implementation and effective coverage of interventions that generate the greatest reductions in mortality and morbidity.

The platform is designed to support:

- National policy planning
- Subnational targeting
- Resource allocation
- Strategic scale-up pathways
- Long-term mortality reduction goals

---

## ⚖️ Economic Evaluation

The repository includes economic evaluation modules to estimate:

- Intervention costs
- Cost-effectiveness
- Budget impact
- Long-term fiscal implications
- Value-for-money
- Health and economic returns on investment

Analyses are designed to support medium- and long-term planning for Indonesia’s health financing and NCD control strategies.

---

## 🌎 Equity Analysis

The framework includes explicit equity-oriented analyses through:

- Regional analysis by province and district
- Subnational comparisons of intervention performance
- Assessment of geographic disparities
- Differential implementation capacity evaluation
- Distributional analysis of health gains

The goal is to identify strategies that improve both overall population health and equity in access to effective NCD services.

---

## 🧩 Modelling Framework

```text
Population
   ↓
Risk Factors
   ↓
{ CVD Markov Model + Cancer Markov Model + Health Systems Modules }
   ↓
Implementation Strategies
   ↓
Health Outcomes + Economic Outcomes + Equity Outcomes
```

---

## 🔬 Current Technical Priorities

### Analysis 1 — Improving Implementation of CKG

The framework extends existing cardiovascular microsimulation models to:

1. Produce subnational projections by province and district
2. Evaluate realistic implementation targets (e.g., 70-30-30 → 70-70-70)
3. Assess implementation strategies such as supply chain strengthening
4. Improve estimation of treatment and control pathways

### Analysis 2 — Identifying High-Value CVD Interventions

The platform evaluates the potential impact of scaling effective coverage of additional cardiovascular interventions, including:

#### Primary Health Centers
- Secondary CVD prevention
- Chronic heart failure management
- Stroke rehabilitation
- Cardiac rehabilitation
- Secondary CKD prevention

#### First-Level Hospitals
- Acute heart failure management
- Basic acute coronary syndrome management
- Basic stroke management
- Renal replacement therapy

#### Referral Hospitals
- Primary PCI for acute coronary syndrome
- Mechanical thrombectomy

The analyses benchmark mortality reduction targets against high-performing ASEAN countries and evaluate realistic scale-up pathways over time.

---

## ⚙️ Repository Structure

```text
/data      -> Input datasets
/code      -> Simulation and analysis scripts
/outputs   -> Model outputs and figures
/docs      -> Documentation and technical notes
```

---

## ▶️ How to Run

```r
install.packages(c("data.table", "tidyverse", "openxlsx"))

source("code/cvd/06_run_simulation.R")
```

---

## 🧪 Policy Scenarios

Examples of evaluated policy and implementation scenarios include:

- Hypertension control
- Sodium reduction
- Trans-fat elimination
- Lipid-lowering therapy (statins)
- Cancer screening
- Treatment adherence strategies
- Supply chain strengthening
- Improved effective coverage
- Regional implementation targeting

---

## ⚠️ Core Assumptions

- Discrete-time state-transition framework
- Population-level risk factor modeling
- Conditional independence across diseases (baseline version)
- PAF-based incidence modeling
- No explicit multimorbidity interactions in v1
- Incremental scale-up relative to current effective coverage

---

## 🤝 Collaborators

- University of Washington
- World Bank
- Indonesian Ministry of Health
- Disease Control Priorities (DCP)

---

## 📜 License

MIT

---

## 📚 Citation

Forthcoming
