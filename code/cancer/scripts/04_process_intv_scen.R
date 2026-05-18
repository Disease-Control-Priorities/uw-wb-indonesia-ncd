#' @title Process Intervention-Scenario inputs for Cancer Modeling
#' @author Mohamed Albirair, MBBS, PhD; Renu Nargund, MPH; David Watkins, MD, MPH


# Load functions and settings
# source("R/library.R")

##-----Load scenario-defining input values-----------------------------------##
# load(z$raw_data_file)

scen_input_param   <- read_sim_param(path_input = z$scen_input_file)
inputs             <- scen_input_param$input_param
country_select     <- names(inputs)
country_setting    <- pull(inputs)[1] # pull(income_setting[match(country_select, income_setting$location), "setting"])
scaleup_start_year <- pull(inputs)[2]
scaleup_end_year   <- pull(inputs)[3]

## Identify target cancer types
target_cancers     <- scen_input_param$target_cancers
screenable_cancers <- c(cause2[1], cause3) # Breast, Cvx, CRC

# # Quick check
# identical(extract_RData(rdfile = paste0("R/inputs/RData/", country_select, "/", country_select, "_param.RData"),
#                         object = "country_select"),
#           country_select)
#
# # Load country-specific paramters
# load(paste0("R/inputs/RData/", country_select, "/", country_select, "_param.RData"))
#
# # Load country-specific paramters
# load(paste0("R/inputs/RData/",
#             country_select,
#             "/bsln/",
#             country_select, "_bsln.RData"))

#---------------------------------#

## Process Intervention Scenario-Defining Inputs
intv_modif_rr <- process_intv_scen_inputs(screen_scaleup = FALSE)

## Intervention Scenario TPs
intv_tps <- calc_intv_tps()

## Baseline Scenario Markov Trace
intv_markov_trace <- project_markov_trace_dt(tps_inpt = intv_tps)


##-----Save all---------------------------------------------------------------##

if (!dir.exists(paste0("R/inputs/RData/", country_select, "/intv/"))) {
      dir.create(path = paste0("R/inputs/RData/", country_select, "/intv/"),
                 recursive = TRUE)
}

## Save for country baseline scenario
save(country_select, country_setting,
     intv_modif_rr,
     intv_tps,
     intv_markov_trace,

     file = paste0("R/inputs/RData/",
                   country_select,
                   "/intv/",
                   country_select, "_intv.RData"))

