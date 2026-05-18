# Hybrid cancer modeling: Markov Model + Cohort Component Projection Model

## An R Model for Quantifying Costs and Impacts of Comprehensive Cancer-Control Strategies
This repository contains R code for simulating and analyzing the costs and health impacts of user-defined comprehensive cancer-control strategies.

## Prerequisites
Before running the code, please ensure you have the necessary dependencies installed.

``` r
# Install the tidyverse package for data manipulation and analysis
install.packages("tidyverse")

# Install the openxlsx package for exporting results to Excel files
install.packages("openxlsx")
```

## Instructions for Running the Code

The code is structured to follow the sequence shown in the cancer projections flowchart below.

![Cancer projections flowshart](Diagrams/Cancer projections flowchart.png)

## Code Execution

### 1. Process Input Data

To process inherent input parameters, execute the file `00_preprocess_data.R`. Pre-processed data are saved in a file named `all-baseline-data.RData`, which you can download from the following Google Drive [link](https://drive.google.com/file/d/1PdQrX5OgpwATnl1fwxLXybUslq9wIc5b/view?usp=drive_link).

### 2. Update Paramters with Adjustable Default Values

In the `intv_scen_inputs.xlsx` file, sheet `ui`, you can specify:

  1. Settings, where you can specify: **Target country**; **Intervention scale up start year**; and **Intervention scale up end year**
    
  2. Cancer prevention: *(not available in this version)*
  
  3. Cancer treatment, where you can specify: **Target cancer types**; **Baseline treatment adherence coverage**; and **Target treatment adherence coverage**
    
  4. Cancer palliation: *(not available in this version)*

### 3. Run the Cancer Model

To start the simulation, execute the file `01_main_sim_script.R`. This script contains all the functions required to run the cancer model and generates simulation outputs.

***Note:*** 

  1. The Markov model is set to start from 2000 in order to allow the model to stabilize before running population projections, and is set to end in 2050. You can update this value in lines #21 and #22 in the script, respectively.
  
  2. One of the key assumptions in the baseline scenarios is that TP values for 2019 would continue to be observed in all future years in the baseline scenario. You can update this value in line #23 in the script
  
  3. The population projections are set to start from 2020 and end at 2050. You can update those values in lines #176 and #177 in the script, respectively.

### 4. Run Analyses

Once the simulation has completed, run `02_analysis_outputs.R` to reproduce the results and export them as needed. This sequence will allow you to quantify both the costs and the health impacts of the cancer-control strategies as specified in the project.
