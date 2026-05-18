#' @title Process Country-Specific Parameters for Cancer Modeling
#' @author Mohamed Albirair, MBBS, MPH, PhD; Renu Nargund, MPH; Sarah Pickersgil, MPH; David Watkins, MD, MPH


# Load functions and settings
source("R/library.R")

##-----Load Country-Specific parameters---------------------------------------##
load("R/inputs/all-param.RData")
# load("R/inputs/gbd_dt.RData")

scen_input_param <- read_sim_param(path_input = z$scen_input_file)
inputs           <- scen_input_param$input_param
country_select   <- names(inputs)
country_setting  <- pull(inputs)[1] # pull(income_setting[match(country_select, income_setting$location), "setting"])

sim_start_year   <- z$sim_start_year
sim_end_year     <- z$sim_end_year
reference_year   <- z$reference_year
calib_years      <- sim_start_year:reference_year
all_sim_years    <- sim_start_year:sim_end_year


##-----Load country-specific data---------------------------------------------##
# country_epi <- load_country_dt(file_input    = "R/inputs/gbd_dt.RData",
#                                obj_input     = "gbd_epi_1yr_predict",
#                                country_input = country_select)

country_epi <- read.csv(
      paste0("Data/GBD 2021/16 cancer types/split/gbd_epi_",
             country_select,
             ".csv")
)

# country_acm <- load_country_dt(file_input    = "R/inputs/gbd_dt.RData",
#                                obj_input     = "gbd_acm_1yr_predict",
#                                country_input = country_select)[, Nx := pop]

country_acm <- read.csv(
      paste0("Data/GBD 2021/All causes/split/gbd_acm_",
             country_select,
             ".csv")
)

country_pop <- read.csv(
      paste0("Data/GBD 2021/Pop/split/gbd_pop_",
             country_select,
             ".csv")
)

# country_frt <- load_country_dt(file_input    = "R/inputs/gbd_dt.RData",
#                                obj_input     = "gbd_fert",
#                                country_input = country_select)

country_frt <- read.csv(
      paste0("Data/GBD 2023/Fertility/split/gbd_frt_",
             country_select,
             ".csv")
)

##----------------------------------------------------------------------------##

# Extract incident case counts and rates in the first simulation year
country_incd <- map(.x = calib_years,
                    .f = ~ calc_metric_dt(country_dt   = country_epi,
                                          measure_inpt = "Incidence",
                                          year_inpt    = .x)) %>%
      list_rbind()

# Extract prevalent case counts and rates in the first simulation year
country_prev <- map(.x = calib_years,
                    .f = ~ calc_metric_dt(country_dt   = country_epi,
                                          measure_inpt = "Prevalence",
                                          year_inpt    = .x)) %>%
      list_rbind()

# Extract cause-specific death counts and mortality rates for the init year
country_mort <- map(.x = calib_years,
                    .f = ~ calc_metric_dt(country_dt   = country_epi,
                                          measure_inpt = "Deaths",
                                          year_inpt    = .x)) %>%
      list_rbind()

# Extract all-cause mortality rates, counts and tot pop counts
country_acmx <- map(.x = calib_years,
                    .f = ~ calc_metric_dt(country_dt   = country_acm,
                                          measure_inpt = "Deaths",
                                          year_inpt    = .x)) %>%
      list_rbind()

# Extract total population counts
country_nx   <- map(.x = calib_years,
                    .f = ~ calc_metric_dt(country_dt   = country_pop,
                                          measure_inpt = "Population",
                                          year_inpt    = .x)) %>%
      list_rbind()


##-----Correct prevalence estimates and account for deaths in previous years----##
markov_correction <- correct_markov(acm_inpt = country_acmx,
                                    epi_inpt = country_mort,
                                    start_yr = sim_start_year)

#------------------------------------------------------------------------------#

if (!dir.exists(paste0("R/inputs/RData/", country_select))) {
      dir.create(path = paste0("R/inputs/RData/", country_select, "/"),
                 recursive = TRUE)
}

# # Quick check
# explore_rdata_obj(file_path = paste0("R/inputs/RData/", country_select , "/", country_select, "_param.RData"),
#                   obj_nm = "country_epi") %>% select(year) %>% pull() %>% unique()

## Save for performance testing
save(country_select, country_setting,
     sim_start_year, sim_end_year, reference_year, all_sim_years,
     country_epi, country_acm, # country_frt,
     country_incd, country_prev, country_mort, country_acmx,
     markov_correction,

     file = paste0("R/inputs/RData/",
                   country_select , "/",
                   country_select, "_param.RData"))

# cgwtools::resave(country_frt,
#                  file = paste0("R/inputs/RData/", country_select , "/", country_select, "_param.RData"))

