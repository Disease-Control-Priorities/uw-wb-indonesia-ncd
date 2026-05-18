#' @title Process Population Projections for Cancer Modeling
#' @author Mohamed Albirair, MBBS, PhDc; Renu Nargund, MPH; David Watkins, MD, MPH


# Load functions and settings
# source("R/library.R")

##-----Load scenario-defining input values-----------------------------------##
# load(z$raw_data_file)

# scen_input_param   <- read_sim_param(path_input = z$scen_input_file)
# inputs             <- scen_input_param$input_param
# country_select     <- names(inputs)

# Load universal parameters
# load(paste0("R/inputs/RData/", country_select, "/",
#             country_select, "_param.RData"))

# Load country-specific parameters
load(paste0("R/inputs/RData/", country_select , "/", country_select, "_param.RData"))

# Load baseline scenario data
load(paste0("R/inputs/RData/", country_select, "/bsln/",
            country_select, "_bsln.RData"))


##-----Population Projections-------------------------------------------------##

# Q: What year should be the source of Nx?

# Nx_Dx_mx_1y_ref_year <- merge(x   = n_mx_N_acm[, !"cause"],
#                               y   = subset(country_frt, year == sim_start_year),
#                               by  = c("age", "year", "sex", "location"),
#                               all = TRUE)

## Fertility data for future year projections
#---------------------------Allow user to modify fertility inputs/values/assumptions
fixed_fx <- country_frt %>%
      filter(year == reference_year) %>%
      arrange(sex, age) %>%
      select(age, sex, location, fx = asfr)

Nx_Dx_mx_fx_strt_year <- full_join(
      x  = country_acmx %>% filter(year == reference_year), # sim_start_year
      y  = fixed_fx, #----------------------------------------------------------Assumption!
      by = join_by(age, sex, location)
) %>% # view()
      # mutate(year = reference_year) %>% #----------------------------Q: double check this#
      select(location, sex, year, age, Nx, Dx = acDx, mx = acmx, fx)

## Extract and calculate total cause-specific mortality rates for target cancer types
#------------------------------Redundancy, remove once matched to where done before
#------------------------------Set the code so that it constantly produce output for all 16+ cancer types
# if (!exists("target_cancers")) {
#       target_cancers <- scen_input_param$target_cancers
# }

# tcmx_ref_year <- country_epi %>%
#       filter(year    == reference_year, # 2019
#              measure == "Deaths",
#              cause   %in% target_cancers) %>%
#       select(age, year, sex, location, cause, cmx = pred_rates) %>%
#
#       # Calculate total target-cancer mortality rates
#       summarise(tcmx = sum(cmx),
#                 .by = c(age, year, sex, location))

# tcmx_ref_year <- country_epi %>%
#       filter(year    == reference_year, # 2019
#              measure == "Deaths") %>%
#
#       # Calculate total target-cancer mortality rates
#       group_by(age, sex, location) %>%
#       reframe(tcmx = sum(pred_rates[cause %in% target_cancers]))

# tcmx_ref_year <- country_epi %>%
#       filter(year    == reference_year, #---------------------------------------Assumption
#              measure == "Deaths") %>%
#       select(location, sex, age, cause, cmx = pred_rates) %>%
#
#       # Calculate total target-cancer mortality rates
#       group_by(age, sex, location) %>%
#       mutate(tcmx = ifelse(test = cause %in% target_cancers,
#                            yes  = sum(cmx),
#                            no   = NA)) %>%
#       ungroup()

# Calculate "other" (non-target) mortality rates
# Nx_Dx_mx_omx_1y_ref_year <- full_join(
#       x  = Nx_Dx_mx_fx_1y_ref_year,
#       y  = tcmx_ref_year,
#       by = join_by(age, sex, location)) %>%
#       mutate(omx = mx - tcmx)

# Nx_Dx_mx_cmx_omx_strt_year <- full_join(
#       x  = Nx_Dx_mx_fx_strt_year,
#       y  = tcmx_ref_year,
#       by = join_by(age, sex, location)) %>%
#       mutate(omx = mx - tcmx)

# Calculate "other" (non-target) mortality rate
#-----------------------------split omx to omx_ca vs. omx_nca
#-----------------------------omx_nca = GBD lvl 0 - GBD Neoplasms (B1 or 1B)
# fixed_omx <- Nx_Dx_mx_omx_1y_ref_year %>%
#       select(age, sex, location, omx)
# fixed_omx <- Nx_Dx_mx_cmx_omx_strt_year %>%
#       select(age, sex, location, omx)

# Run population projections
# bsln_current_output <- intv_current_output <- Nx_Dx_mx_omx_1y_ref_year %>%
#       select(age, year, sex, location, Nx, fx, mx) %>%
#       mutate(Dx = mx * Nx)
bsln_current_output           <- Nx_Dx_mx_fx_strt_year
bsln_current_output$iteration <- 1
bsln_current_output$scen      <- "bsln"
bsln_pop_projection           <- bsln_current_output
strt_yr                       <- reference_year + 1 # sim_start_year + 1 #
end_yr                        <- sim_end_year

bsln_markov_trace <- bsln_markov_trace %>%
      mutate(cohort = year - age) %>%
      mutate(lplcl = plcl - lag(plcl, default = first(plcl)),
             lpcdx = pcdx - lag(pcdx, default = first(pcdx)),
             .by = c(cohort, sex, cause, location))

# bsln_markov_trace_rates <- bsln_markov_trace %>%
#       split(list(.$age, .$year, .$sex)) %>%
#       map(.x = .,
#           .f = ~ calc_markov_state_deltas_dt(markov_trace = .x)) %>%
#       list_rbind()

for (i in seq_along(strt_yr:end_yr) + 1) {

      # Baseline scenario
      bsln_current_output <- proj_ccpm_markov(pop_data     = bsln_current_output,
                                              markov_input = bsln_markov_trace, # bsln_markov_trace_rates %>% filter(year == reference_year), #
                                              # fx_input     = fixed_fx,
                                              # cmx_var      = "d_pcdx" # cdeath_rate
                                              ) %>%
            mutate(iteration = i,
                   scen      = "bsln",
                   Dx        = round(mx * Nx))

      bsln_pop_projection <- bind_rows(bsln_pop_projection,
                                       bsln_current_output)
}

# bsln_pop_proj_markov <- left_join(
#       x  = bsln_pop_projection,
#       y  = bsln_markov_trace,
#       by = join_by(age, year, sex, location)
# ) %>%
#       mutate(i_rate = plcl * Nx / ((plcl + pwell) * Nx),
#              d_rate = ifelse(test = cause %in% c(cause2, cause3),
#                              yes  = pcdx * Nx / ((pdst + pcdx) * Nx),
#                              no   = pcdx * Nx / ((plcl + pcdx) * Nx)))


## Save for country baseline scenario

if (!dir.exists(paste0("R/outputs/RData/", country_select, "/bsln/"))) {
      dir.create(path = paste0("R/outputs/RData/", country_select, "/bsln/"),
                 recursive = TRUE)
}

save(bsln_markov_trace,
     file = paste0("R/outputs/RData/",
                   country_select,
                   "/bsln/",
                   country_select, "_bsln.RData"))



