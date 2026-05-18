#' @title Read Data for Cancer Modeling
#' @author Mohamed Albirair, MBBS, PhDc


start_time <- Sys.time()

# Load functions and settings
source("R/library.R")

### Data Prep------------------------------------------------------------------#

# Discarded age categories (for now!)
discard_age <- c("0-6 days",
                 "7-27 days",
                 "<28 days",
                 "1-5 months",
                 "6-11 months",
                 "<1 year",
                 "12-23 months",
                 "2-4 years")

# Create location mapping vector for fast lookup
location_map <- c(
      "Bolivia (Plurinational State of)"      = "Bolivia",
      "Brunei Darussalam"                     = "Brunei",
      "Cabo Verde"                            = "Cape Verde",
      "Côte d'Ivoire"                         = "Cote d'Ivoire",
      "Czechia"                               = "Czech Republic",
      "Federated States of Micronesia"        = "Micronesia",               # 1
      "Republic of Moldova"                   = "Moldova",
      "Micronesia (Federated States of)"      = "Micronesia",               # 1
      "Iran (Islamic Republic of)"            = "Iran",
      "Lao People's Democratic Republic"      = "Laos",
      "North Macedonia"                       = "Macedonia",
      "Democratic People's Republic of Korea" = "North Korea",
      "Republic of Korea"                     = "South Korea",
      "Syrian Arab Republic"                  = "Syria",
      "Swaziland"                             = "Eswatini",
      "United Republic of Tanzania"           = "Tanzania",
      "Taiwan (Province of China)"            = "Taiwan",
      "Türkiye"                               = "Turkey",
      "The Bahamas"                           = "Bahamas",
      "The Gambia"                            = "Gambia",
      "Venezuela (Bolivarian Republic of)"    = "Venezuela",
      "Viet Nam"                              = "Vietnam",
      "Virgin Islands, U.S."                  = "Virgin Islands",           # 2
      "United States"                         = "United States of America",
      "United States Virgin Islands"          = "Virgin Islands"            # 2
)

# Countries not present in WB data:
wb_rm_loc <- c("American Samoa", "Brunei", "Cook Islands", "Dominica", "Niue",
               "Saint Kitts and Nevis", "Taiwan", "Tokelau")

## Cancer Models

#---- 1) Most basic model (well, sick, dead)
cause1 <- sort(c("Uterine cancer",
                 "Esophageal cancer",
                 "Thyroid cancer",
                 "Stomach cancer",
                 "Liver cancer",
                 "Pancreatic cancer",
                 "Ovarian cancer",
                 "Bladder cancer",
                 "Lip and oral cavity cancer",
                 "Nasopharynx cancer",
                 "Other pharynx cancer"))       # 11 cancer types

#---- 2) Staged model (well, local, regional, distant, dead)
# Screening-amenable cancers
cause2 <- sort(c("Tracheal, bronchus, and lung cancer",
                 "Prostate cancer",
                 "Breast cancer"))

#---- 3) Staged model with pre-cancerous lesions
cause3 <- sort(c("Cervical cancer",
                 "Colon and rectum cancer"))

# Label cancer types (shorter names)
cx_lab <- c("Bladder", "Breast", "Cervix", "CRC", "Esophagus", "Oral", "Liver", "Nasophx", "Pharynx",
            "Ovary", "Pancreas", "Prostate", "Stomach", "Thyroid", "Lung", "Uterus")
cx_vec <- sort(c(cause1, cause2, cause3))
names(cx_lab) <- cx_vec

# Female-only cancer types
female_cancers <- c("Cervical cancer", "Ovarian cancer", "Uterine cancer")

# Cancer whose age lm includes spines (15-39 & 40-79)
spline_cancers <- c(cause2[1], cause3[1])


### Pop Data-------------------------------------------------------------------#

## Population Counts Data
# https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/3931936f370fdaf18adfca2089680981

# 1) Read GBD pop data files
pop_zip_files <- list.files(path       = "Data/GBD 2021/Pop/",
                            pattern    = "\\.zip$",
                            full.names = TRUE)

gbd_pop_raw <- data.table::rbindlist(l         = lapply(X   = pop_zip_files,
                                                        FUN = read_csv_from_zip),
                                     fill      = TRUE,
                                     use.names = TRUE)

# 2) Process GBD pop data
gbd_pop <- data.table::copy(gbd_pop_raw)


gbd_pop <- gbd_pop[
      # Map locations
      , location := data.table::fcoalesce(location_map[location], location)
][
      # Remove locations not present in the WB data set
      location %!in% wb_rm_loc
][
      # Rename age to age_cat
      # , age_cat := age #------------------------------------------------------Q: ?#
      , `:=` (age_cat = age)
][
      # Convert age categories (much faster than case_when)
      , age := data.table::fcase(
            age_cat == "<28 days",     0.028,  # rm
            age_cat == "1-5 months",   0.5105, # rm
            age_cat == "6-11 months",  0.611,  # rm
            age_cat == "<1 year",      0.0,    # keep!
            age_cat == "12-23 months", 1.00,   # keep!
            age_cat == "2-4 years",    2.04,   # rm
            age_cat == "<5 years",     0.05,   # rm
            default = as.numeric(str_extract(age_cat, "\\d+"))
      )
][
      # Remove unwanted age groups (for now!)
      # (not needed, since GBD pop already includes single-year age groups values)
      age_cat %!in% c(
            "<28 days",    # rm
            "1-5 months",  # rm
            "6-11 months", # rm
            "2-4 years",   # rm
            "<5 years"     # rm
      ),
][
      # Sort efficiently
      order(location, year, age)
]

# # Export GBD pop .csv files
gbd_pop %>%
      split(.$location) %>%
      map2(.x = .,
           .y = names(.),
           .f = ~ write.csv(x         = .,
                            file      = paste0("Data/GBD 2021/Pop/split/gbd_pop_", .y, ".csv"),
                            row.names = FALSE))

# 3) Categorize GBD pop data into 5-year age groups
gbd_pop_5yr <- gbd_pop[
      , .(pop = sum(val)),
      by = .(
            location, year, sex,
            age_gp = cut(
                  age,
                  breaks = c(-Inf, seq(5, 95, 5), Inf),
                  labels = c("<5 years",
                             paste0(seq(5, 90, 5), "-", seq(9, 94, 5), " years"),
                             "95+ years"),
                  right = FALSE
            )
      )
][
      #-------------------------------------------------------------------------Q: Needed?#
      , age := data.table::fifelse(
            age_gp == "<5 years", 0, as.numeric(str_extract(age_gp, "\\d+"))
            # as.numeric(gsub("[-+].*", "", as.character(age_gp)))
      )
]

age_min <- min(gbd_pop$age)

age_max <- max(gbd_pop$age)

### EPI Data-------------------------------------------------------------------#

## Cancer-Specific Epidemiological Data

# Sep 2025: https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/df6164cc40d2ac450659826f45ba1661
# Additional <1 age groups: https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/2ed3b5e1505a7cfff44ad2730a065a97

# 1) Read GBD epi data files
zip_files <- list.files(path       = "Data/GBD 2021/16 cancer types/",
                        pattern    = "\\.zip$",
                        full.names = TRUE)

gbd_epi_raw <- data.table::rbindlist(l         = lapply(X   = zip_files,
                                                        FUN = read_csv_from_zip),
                                     fill      = TRUE,
                                     use.names = TRUE)

# Check:
# identical(unique(gbd_epi_raw$location), unique(gbd_epi_raw$location))

# 2) Process GBD epi data
gbd_epi_5yr <- data.table::copy(gbd_epi_raw) # Prep a separate copy of target dt

gbd_epi_5yr <- gbd_epi_5yr[
      # Apply sex-specific filtering first to reduce data size
      (cause %in% female_cancers & sex == "Female") |
            (cause == cause2[2] & sex == "Male") |
            (!(cause %in% female_cancers) & cause != cause2[2])
][
      # Map locations
      , location := data.table::fcoalesce(location_map[location], location)
][
      # Remove locations not present in the WB data set
      location %!in% wb_rm_loc
][
      # Rename age to age_cat
      , age_cat := age
][
      # Convert age categories (much faster than case_when)
      , age := data.table::fcase(
            age_cat == "<28 days",     0.028,  # rm
            age_cat == "1-5 months",   0.5105, # rm
            age_cat == "6-11 months",  0.611,  # rm
            age_cat == "<1 year",      0.01,   # rm
            age_cat == "12-23 months", 1.00,   # rm
            age_cat == "2-4 years",    2.04,   # rm
            age_cat == "<5 years",     0.00,   # keep!
            default = as.numeric(str_extract(age_cat, "\\d+"))
      )
][
      # Remove unwanted age groups (for now!)
      # (not needed, since GBD pop already includes single-year age groups values)
      age_cat %!in% c(
            "<28 days",     # rm
            "1-5 months",   # rm
            "6-11 months",  # rm
            "<1 year",      # rm
            "12-23 months", # rm
            "2-4 years"     # rm
      ),
][
      , age_mid := data.table::fcase(
            age < 95, age + 2.5,
            default = age
      )
][
      ,age_gp := cut(
            age,
            breaks = c(-Inf, seq(5, 95, 5), Inf),
            labels = c("<5 years",
                       paste0(seq(5, 90, 5), "-", seq(9, 94, 5), " years"),
                       "95+ years"),
            right = FALSE
      )
][
      , val := data.table::fifelse(is.na(val), 0, val)
][
      # Sort efficiently
      order(location, year, age, measure, metric)
]

# 3) Fit cancer-specific age-outcome regression model for 1-year interpolation
epi_age_models_dt <- gbd_epi_5yr[
      metric == "Rate" &
            year %in% z$sim_start_year:z$reference_year &
            ((cause %in% spline_cancers & between(age_mid, 15, 79)) |
                   (!cause %in% spline_cancers & between(age_mid, 30, 79))),
      .(location, sex, year, cause, measure, age = age_mid,
        log_val = log(data.table::fifelse(val == 0, 1e-16, val)))
][
      , .(model = {
            if (cause[1] %in% spline_cancers) {
                  # Spline model for cervical and colorectal cancer
                  # Split at age 40: linear spline with knot at 40
                  list(lm(log_val ~ age + I((age - 40) * (age > 40)), data = .SD))
            } else {
                  # Simple linear model for all other cancers
                  list(lm(log_val ~ age, data = .SD))
            }
      }),
      by = .(location, sex, year, cause, measure)
]

# Check:
# nrow(epi_age_models_dt) / 204 / (12 * 2 + 4) / length(c(2000, 2019)) / length(unique(epi_age_models_dt$measure))

# 4) Pull in corresponding age-specific population size from GBD Pop
epi_age_models_pop_dt <- gbd_pop[
      year %in% z$sim_start_year:z$reference_year,
      .(location, sex, year, age, pop = val) # year
][
      epi_age_models_dt,
      on = .(location, sex, year),
      allow.cartesian = TRUE  # Explicitly allow the cartesian product
]

# Check:
# nrow(epi_age_models_pop_dt) /
#       length(unique(epi_age_models_pop_dt$location)) / # 204
#       length(unique(epi_age_models_pop_dt$age)) /      # 96
#       length(unique(epi_age_models_pop_dt$year)) /     # 2
#       length(unique(epi_age_models_pop_dt$measure)) /  # 3
#       # nrow(count(epi_age_models_pop_dt, sex, cause))
#       (12 * 2 + 4)

# 5) Predict 1-year age group-specific values
# gbd_epi_1yr_predict <- data.table::copy(age_models_pop_dt) # (slow!)
gbd_epi_1yr_predict <- epi_age_models_pop_dt                 # Prep a separate copy

gbd_epi_1yr_predict[
      , log_fitted_val := {
            if (!is.null(model[[1]])) {
                  predict(model[[1]], newdata = .SD)
            } else NA_real_
      },
      by = .(location, sex, cause, year, measure)
][
      , pred_rates := exp(log_fitted_val) / 1e5  # Calculate predicted rates
][
      , pred_cases := pred_rates * pop           # Calculate predicted cases
][
      , model := NULL  # Remove model column
][
      , age_gp := cut(
            age,
            breaks = c(-Inf, seq(5, 95, 5), Inf),
            labels = c("<5 years",
                       paste0(seq(5, 90, 5), "-", seq(9, 94, 5), " years"),
                       "95+ years"),
            right = FALSE
      )
]

# Check:
# nrow(gbd_epi_1yr_predict) /
#       length(unique(gbd_epi_1yr_predict$year)) /
#       length(unique(gbd_epi_1yr_predict$location)) /
#       length(unique(gbd_epi_1yr_predict$age))/
#       nrow(count(gbd_epi_1yr_predict, sex, cause))/
#       length(unique(gbd_epi_1yr_predict$measure))

# Store as country-specific .csv files
gbd_epi_1yr_predict %>%
      split(.$location) %>%
      map2(.x = .,
           .y = names(.),
           .f = ~ write.csv(x         = .,
                            file      = paste0("Data/GBD 2021/16 cancer types/split/gbd_epi_", .y, ".csv"),
                            row.names = FALSE))

#---------------------------------------------------------------Flag: check mort <20 YO
#---------------------------------------------------------------Create a summary table, and check values across countires

#------------------------------------------------------------------------------#

## All-Cause Mortality
# Source:
# https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/676a9bc4225e49e4bffe5c9772aac247

acm_files <- list.files(path       = "Data/GBD 2021/All causes/",
                        pattern    = "\\.zip$",
                        full.names = TRUE)

gbd_acm_raw <- data.table::rbindlist(l         = lapply(X   = acm_files,
                                                        FUN = read_csv_from_zip),
                                     fill      = TRUE,
                                     use.names = TRUE)

# Prep a separate copy of target dt
gbd_acm_5yr <- data.table::copy(gbd_acm_raw)

gbd_acm_5yr <- gbd_acm_5yr[
      # Apply sex-specific filtering first to reduce data size
      (cause %in% female_cancers & sex == "Female") |
            (cause == cause2[2] & sex == "Male") |
            (!(cause %in% female_cancers) & cause != cause2[2])
][
      # Map locations
      , location := data.table::fcoalesce(location_map[location], location)
][
      # Remove locations not present in the WB data set
      location %!in% wb_rm_loc
][
      # Rename age to age_cat
      , age_cat := age
][
      # Convert age categories (much faster than case_when)
      , age := data.table::fcase(
            age_cat == "<28 days",     14 / 28 / 12,
            age_cat == "1-5 months",   3 / 12,
            age_cat == "6-11 months",  8.5 / 12,
            age_cat == "<1 year",      6 / 12,
            age_cat == "12-23 months", 1.50,
            age_cat == "2-4 years",    3.00,
            age_cat == "<5 years",     2.50,
            default = as.numeric(str_extract(age, "^\\d+"))
      )
][
      # Remove unwanted age groups#--------------------------------------------# TBU
      # age %!in% c(0.01, 0.05)
      # age %in% c("<28 days", "1-5 months", "6-11 months", "12-23 years", "2-4 years")
][
      , age_mid := data.table::fcase(
            # age > 1 & age <5, 3,
            age >= 5 & age < 95, age + 2.5,
            default = age
      )
][
      , age_gp := cut(
            age,
            breaks = c(-Inf, seq(5, 95, 5), Inf),
            labels = c("<5 years",
                       paste0(seq(5, 90, 5), "-", seq(9, 94, 5), " years"),
                       "95+ years"),
            right = FALSE
      )
][
      , val := data.table::fifelse(is.na(val), 0, val)
][
      # Sort efficiently
      order(location, year, age, measure, metric)
]


# Step 1: Create models using data.table (much faster)
acm_age_models_dt <- gbd_acm_5yr[
      metric == "Rate" &
            year %in% z$sim_start_year:z$reference_year,
      .(location, sex, year, cause, measure, age = age_mid,        # removed original age to run the model on age_mid
        log_val = log(data.table::fifelse(val == 0, 1e-16, val)))  # Converted log(val == 0)
][
      , .(model = list(lm(log_val ~ age +                          # <1
                                I((age - 01) * (age > 01)) +       # 1-4
                                I((age - 05) * (age > 05)) +       # 5-9
                                I((age - 10) * (age > 10)),        # 10+
                          data = .SD))),
      by = .(location, sex, year, cause, measure)
]

# Check:
# nrow(acm_age_models_dt) /
#       length(unique(acm_age_models_dt$location)) /                 # 204
#       length(unique(acm_age_models_dt$sex))/
#       length(unique(acm_age_models_dt$year))

# Step 2: Filter population data and join
acm_age_models_pop_dt <- gbd_pop[
      year %in% z$sim_start_year:z$reference_year,
      .(location, sex, year, age, pop = val) # year
][
      acm_age_models_dt,
      on = .(location, sex, year),
      allow.cartesian = TRUE  # Explicitly allow the cartesian product
]

# nrow(acm_age_models_pop_dt) /
#       length(unique(acm_age_models_pop_dt$location)) / # 204
#       length(unique(acm_age_models_pop_dt$age)) /      # 96
#       length(unique(acm_age_models_pop_dt$sex)) /      # 2
#       length(unique(acm_age_models_pop_dt$sex))        # 2

gbd_acm_1yr_predict <- acm_age_models_pop_dt

# Step 3: Predict using data.table (fastest method)
gbd_acm_1yr_predict[
      , log_fitted_val := {
            if (!is.null(model[[1]])) {
                  predict(model[[1]], newdata = .SD)
            } else NA_real_
      },
      by = .(location, sex, cause, year, measure)
][
      , pred_rates := exp(log_fitted_val) / 1e5  # Calculate predicted rates
][
      , pred_cases := pred_rates * pop           # Calculate predicted cases
][
      , model := NULL  # Remove model column
][
      , age_gp := cut(
            age,
            breaks = c(-Inf, seq(5, 95, 5), Inf),
            labels = c("<5 years",
                       paste0(seq(5, 90, 5), "-", seq(9, 94, 5), " years"),
                       "95+ years"),
            right = FALSE
      )
]

# Check:
# nrow(gbd_acm_1yr_predict) /
#       length(unique(gbd_acm_1yr_predict$year)) /
#       length(unique(gbd_acm_1yr_predict$location)) /
#       length(unique(gbd_acm_1yr_predict$age)) /
#       length(unique(gbd_acm_1yr_predict$sex))

# Store as country-specific .csv files
gbd_acm_1yr_predict %>%
      split(.$location) %>%
      map2(.x = .,
           .y = names(.),
           .f = ~ write.csv(x         = .,
                            file      = paste0("Data/GBD 2021/All causes/split/gbd_acm_", .y, ".csv"),
                            row.names = FALSE))

### Demography Data------------------------------------------------------------#

# Source (old):
# Part 1: https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/d57f16288821d1f25601074d218161eb
# Part 2: https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/9fa63f4c9f449fffe6a1e20ec54e71e5

# Source: https://vizhub.healthdata.org/gbd-results?params=gbd-api-2023-permalink/f172d3ede51ab5f3b24154eef8ebb54e

frt_files <- list.files(path       = "Data/GBD 2023/Fertility/",
                        pattern    = "\\.zip$",
                        full.names = TRUE)

gbd_frt_raw <- data.table::rbindlist(l         = lapply(X   = frt_files,
                                                        FUN = read_csv_from_zip),
                                     fill      = TRUE,
                                     use.names = TRUE)

# Prep a separate copy of target dt
gbd_frt_5yr <- data.table::copy(gbd_frt_raw)

gbd_frt_5yr <- gbd_frt_5yr[
      # Map locations
      , location := data.table::fcoalesce(location_map[location], location)
][
      # Remove locations not present in the WB data set
      location %!in% wb_rm_loc
][
      # Rename age to age_cat
      , age := as.numeric(str_extract(age, "^\\d+"))
][
      # Complete all sex combinations
      data.table::CJ(sex      = c("Male", "Female"),
                     age      = unique(age),
                     year     = unique(year),
                     location = unique(location)),
      on = .(sex, age, year, location)
]

gbd_frt_1yr_interpolate <- data.table::copy(gbd_frt_5yr)

gbd_frt_1yr_interpolate <- gbd_frt_1yr_interpolate[
      # Expand to single-year ages
      , {
            age_range <- seq(age_min, age_max, 1)
            .SD[data.table::CJ(age = age_range), on = .(age), allow.cartesian = TRUE]
      },
      by = .(year, sex, location)
][
      , asfr := data.table::fifelse(age < 15 | age > 49 | sex == "Male", 0, val)
][
      # Remove columns
      , c("measure", "metric", "val", "upper", "lower") := NULL
][
      # Interpolate ASFR
      , asfr := zoo::na.approx(asfr, na.rm = FALSE),
      by = .(year, sex, location)
][
      , age_gp := cut(
            age,
            breaks = c(-Inf, seq(5, 95, 5), Inf),
            labels = c("<5 years",
                       paste0(seq(5, 90, 5), "-", seq(9, 94, 5), " years"),
                       "95+ years"),
            right = FALSE
      )
][
      # Sort efficiently
      order(location, year, age)
]

#---------------------------------------------------------------Create a summary table, and check values across countires

# Store as country-specific .csv files
gbd_frt_1yr_interpolate %>%
      split(.$location) %>%
      map2(.x = .,
           .y = names(.),
           .f = ~ write.csv(x         = .,
                            file      = paste0("Data/GBD 2023/Fertility/split/gbd_frt_", .y, ".csv"),
                            row.names = FALSE))

### WB Income Classification---------------------------------------------------#
income_setting <- readxl::read_xlsx(path = "../../DCP/Cancer-commission/Local only/data/WB country classification/CLASS.xlsx", sheet = "List of economies", range = "A1:D219") %>%
      rename(location = Economy, setting = `Income group`) %>%
      mutate(location = recode_values(location,
                                      "American Samoa"                 ~ "Samoa",
                                      # "Virgin Islands" ~ "British Virgin Islands",
                                      "Bahamas, The"                   ~ "Bahamas",
                                      "Cabo Verde"                     ~ "Cape Verde",
                                      "Congo, Rep."                    ~ "Congo",
                                      "Côte d’Ivoire"                  ~ "Cote d'Ivoire",
                                      "Czechia"                        ~ "Czech Republic"  ,
                                      "Congo, Dem. Rep."               ~ "Democratic Republic of the Congo",
                                      "Dominica"                       ~ "Dominican Republic",
                                      "Egypt, Arab Rep."               ~ "Egypt",
                                      "Gambia, The"                    ~ "Gambia",
                                      "Iran, Islamic Rep."             ~ "Iran",
                                      "Kyrgyz Republic"                ~ "Kyrgyzstan",
                                      "Lao PDR"                        ~ "Laos",
                                      "Korea, Dem. People's Rep."      ~ "North Korea",
                                      "Korea, Rep."                    ~ "South Korea",
                                      "Micronesia, Fed. Sts."          ~ "Micronesia",
                                      "North Macedonia"                ~ "Macedonia",
                                      "St. Lucia"                      ~ "Saint Lucia",
                                      "São Tomé and Príncipe"          ~ "Sao Tome and Principe",
                                      "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                                      "Slovak Republic"                ~ "Slovakia",
                                      "Syrian Arab Republic"           ~ "Syria",
                                      "Türkiye"                        ~ "Turkey",
                                      "United States"                  ~ "United States of America",
                                      "Venezuela, RB"                  ~ "Venezuela",
                                      "Virgin Islands (U.S.)"          ~ "Virgin Islands",
                                      "West Bank and Gaza"             ~ "Palestine",
                                      "Yemen, Rep."                    ~ "Yemen",
                                      default = location),
             setting = recode_values(setting,
                                     "High income"         ~ "HIC",
                                     "Low income"          ~ "LIC",
                                     "Lower middle income" ~ "LMIC",
                                     "Upper middle income" ~ "UMIC",
                                     default = NA)) %>%
      filter(location %in% unique(gbd_epi_1yr_predict$location))


### Cascade and Stage Distribution Data----------------------------------------#

cascade_stg_dist_list <- map(.x = c("begin", "end"),
                             .f = ~ readxl::read_xlsx(path  = "R/inputs/sim_scen_inputs.xlsx",
                                                      sheet = .x) %>%
                                   janitor::clean_names()) %>%
      magrittr::set_names(c("bsln", "trgt"))


### Econ Data------------------------------------------------------------------#

## GNI per capita
gni_per_cap <- read.csv("R/inputs/mean_gni_per_cap.csv",
                        check.names = FALSE) %>%
      pivot_longer(cols      = `1960`:last_col(),
                   names_to  = "year",
                   values_to = "gni") %>%
      mutate(year = parse_number(year)) %>%
      select(country = 1, code = 2, year, gni_pc = gni) %>%

      filter(!is.na(gni_pc)) %>%
      group_by(country) %>%
      slice_max(year) %>%
      ungroup()

## GINI Coefficient
gini_coef_dat <- read.csv("R/inputs/gini_coef.csv",
                          check.names = FALSE) %>%
      pivot_longer(cols      = `1960`:last_col(),
                   names_to  = "year",
                   values_to = "gini_coef") %>%
      mutate(year      = parse_number(year),
             gini_coef = gini_coef / 100) %>%
      select(country = 1, code = 2, year, gini_coef) %>%

      filter(!is.na(gini_coef)) %>%
      group_by(country) %>%
      slice_max(year) %>%
      ungroup()

## Poverty ratio data
pov_ratio_dat <- read.csv("R/inputs/poverty_ratio.csv",
                          check.names = FALSE) %>%
      pivot_longer(cols      = `1960`:last_col(),
                   names_to  = "year",
                   values_to = "pov_ratio") %>%
      mutate(year      = parse_number(year),
             # Correct and divide by 100
             pov_ratio = pov_ratio / 100,
             # Convert from ratio to proportion
             pov_prop  = pov_ratio / (1 + pov_ratio)) %>%
      select(country = 1, code = 2, year, pov_prop) %>%

      # Subset to latest year reported for every country
      filter(!is.na(pov_prop)) %>%
      group_by(country) %>%
      slice_max(year) %>%
      ungroup()

####---------------------------------------------------------------Check WB how calculated


# Putting all Together---------------------------------------------------------#
save(cause1, cause2, cause3, female_cancers,
     age_min, age_max,
     income_setting,
     cascade_stg_dist_list,
     gni_per_cap,
     gini_coef_dat,
     pov_ratio_dat,

     file = "R/inputs/all-param.RData")

# cgwtools::resave(gbd_fert,
#                  file = "R/inputs/gbd_dt.RData")

save(
      # Pop data
      gbd_pop, gbd_pop_5yr,

      # Epi data
      gbd_epi_raw, gbd_epi_5yr,
      epi_age_models_dt, epi_age_models_pop_dt, gbd_epi_1yr_predict,

      # Acm data
      gbd_acm_raw, gbd_acm_5yr,
      acm_age_models_dt, acm_age_models_pop_dt, gbd_acm_1yr_predict,

      # Fert data
      gbd_frt_raw, gbd_frt_5yr, gbd_frt_1yr_interpolate,
      file = "R/inputs/gbd_dt.RData")

# load("R/inputs/gbd_dt.RData")

end_time <- Sys.time()
