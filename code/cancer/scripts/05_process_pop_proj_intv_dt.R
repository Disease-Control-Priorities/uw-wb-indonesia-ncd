#' @title Process Population Projections for Cancer Modeling
#' @author Mohamed Albirair, MBBS, PhDc; Renu Nargund, MPH; David Watkins, MD, MPH


# Load functions and settings
# source("R/library.R")

##-----Load scenario-defining input values-----------------------------------##
# scen_input_param   <- read_sim_param(path_input = z$scen_input_file)
# inputs             <- scen_input_param$input_param
# country_select     <- names(inputs)

# Load country-specific paramters
load(paste0("R/inputs/RData/", country_select, "/",
            country_select, "_param.RData"))

# Load intervention scenario data
load(paste0("R/inputs/RData/", country_select, "/intv/",
            country_select, "_intv.RData"))



##-----Population Projections-------------------------------------------------##

# Run population projections
intv_current_output           <- Nx_Dx_mx_fx_strt_year %>%

# intv_current_output <- Nx_Dx_mx_omx_1y_ref_year %>%
      select(age, year, sex, location, Nx, fx, mx) %>%
      mutate(Dx = mx * Nx)
strt_yr                       <- reference_year + 1 # sim_start_year + 1 #
end_yr                        <- sim_end_year

intv_current_output$iteration <- 1
intv_current_output$scen      <- "intv"
intv_pop_projection           <- intv_current_output

#-----------------------------------------------------------------------Temp fix!
intv_markov_trace <- intv_markov_trace %>%
      mutate(cohort = year - age) %>%
      mutate(lplcl = plcl - lag(plcl, default = first(plcl)),
             lpcdx = pcdx - lag(pcdx, default = first(pcdx)),
             .by = c(cohort, sex, cause, location)) #%>%
      # mutate(i_rate = prob_to_rate(prob = lplcl, time = 1),
      #        d_rate = prob_to_rate(prob = lpcdx, time = 1))

# intv_markov_trace_rates <- intv_markov_trace %>%
#       split(list(.$age, .$year, .$sex)) %>%
#       map(.x = .,
#           .f = ~ calc_markov_state_deltas_dt(markov_trace = .x)) %>%
#       list_rbind()

for (i in seq_along(strt_yr:end_yr) + 1) {

      # Intervention scenario
      intv_current_output <- proj_ccpm_markov(pop_data     = intv_current_output,
                                              markov_input = intv_markov_trace,
                                              # fx_input,
                                              cmx_var = "lpcdx") %>%
            mutate(iteration = i,
                   scen      = "intv",
                   Dx        = round(mx * Nx))

      intv_pop_projection <- bind_rows(intv_pop_projection,
                                       intv_current_output)
      # by = join_by(age, year, sex, location,
      #              Nx, mx, fx, iteration, scen))
}

#------------------------------------------------------------------Temporary fix
# intv_pop_projection <- intv_pop_projection %>%
#       mutate(cohort = year - age) %>%
#       mutate(lplcl = plcl - lag(plcl, default = first(plcl)),
#              lpcdx = pcdx - lag(pcdx, default = first(pcdx)),
#              .by = c(cohort, sex, location))

## Save for country baseline scenario

# Save intervention scen
if (!dir.exists(paste0("R/outputs/RData/", country_select, "/intv/"))) {
      dir.create(path = paste0("R/outputs/RData/", country_select, "/intv/"),
                 recursive = TRUE)
}

save(intv_pop_projection,
     file = paste0("R/outputs/RData/",
                   country_select,
                   "/intv/",
                   country_select, "_intv.RData"))


