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

## 📜 License

MIT

---

## 📚 Citation

Forthcoming
