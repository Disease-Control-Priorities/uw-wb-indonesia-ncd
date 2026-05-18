#' @title Process Baseline-Scenario inputs for Cancer Modeling
#' @author Mohamed Albirair, MBBS, PhD; Renu Nargund, MPH; David Watkins, MD, MPH


# Load functions and settings
# source("R/library.R")

##-----Load scenario-defining input values-----------------------------------##
# load(z$raw_data_file)

# scen_input_param <- read_sim_param(path_input = z$scen_input_file)
# inputs           <- scen_input_param$input_param
# country_select   <- names(inputs)
# country_setting  <- pull(inputs)[1] # pull(income_setting[match(country_select, income_setting$location), "setting"])

# # Quick check
# identical(extract_RData(rdfile = paste0("R/inputs/RData/", country_select, "/", country_select, "_param.RData"),
#                         object = "country_select"),
#           country_select)
#
# # Load country-specific parameters
# load(paste0("R/inputs/RData/", country_select, "/", country_select, "_param.RData"))

#------------------------------------------------------------------------------#

## Baseline stage distribution
bsln_stg_dist     <- scen_input_param$bsln_cascade_stg_dist %>%
      select(cause, precancer, local, regional, distant)

## Initial Health State Distribution (in the Simulation Starting Year)
start_hstate_dist <- calc_start_hstate_dist_dt(markov_correct = FALSE)
# start_hstate_dist_no_markov <- calc_start_hstate_dist_dt(markov_correct = FALSE)

## TP Primers
tp_primers        <- extract_tps_primers_uptd()

## Baseline Scenario TPs
bsln_tps_raw      <- calc_bsln_tps_dt()

bsln_tps <- full_join(
      x  = bsln_tps_raw,
      y  = crossing(bsln_tps_raw %>%
                          filter(year == 2021) %>%
                          select(!year),
                    year = 2022:2050)
)

## Baseline Scenario Markov Trace
bsln_markov_trace <- project_markov_trace_dt(tps_inpt = bsln_tps)


##-----Save all---------------------------------------------------------------##

if (!dir.exists(paste0("R/inputs/RData/", country_select))) {
      dir.create(path = paste0("R/inputs/RData/", country_select, "/bsln/"),
                 recursive = TRUE)
}

## Save for country baseline scenario
save(country_select, country_setting,
     bsln_stg_dist,
     start_hstate_dist,
     bsln_tps,
     bsln_markov_trace,

     file = paste0("R/inputs/RData/",
                   country_select,
                   "/bsln/",
                   country_select, "_bsln.RData"))

# load("R/inputs/RData/Uganda/bsln/Uganda_bsln_input.RData")






