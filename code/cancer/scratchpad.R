


process_nested_gbd <- function(nested_list, year_val, female_cancers) {
      # Initialize output list
      result <- list()

      # Process the specific location
      result[1] <- lapply(nested_list[1], function(cause_data) {
            lapply(cause_data, function(sex_data) {
                  # Check if the year exists for this sex
                  if (as.character(year_val) %in% names(sex_data)) {
                        year_data <- sex_data[[as.character(year_val)]]

                        # Apply the same transformations as your original code
                        year_data %>%
                              mutate(across(
                                    val,
                                    ~ case_when(
                                          # All female cancers among males have 0 prev and mortality
                                          sex == "Male" & cause %in% female_cancers ~ 0,
                                          # All male cancers among females have 0 prev and mortality
                                          sex == "Female" & cause == "Prostate cancer" ~ 0,
                                          # Liver cancer is the only type with values starting at age 0
                                          age == 0 & cause != "Liver cancer" ~ 0,
                                          TRUE ~ .
                                    )
                              ))
                  } else {
                        NULL  # Year not found for this sex-cause combination
                  }
            }) %>% discard(is.null)  # Remove NULL entries for missing years
      }) %>% discard(is.null)  # Remove NULL entries for missing years

      return(result)
}

# Example usage:
female_cancers <- c("Breast cancer", "Ovarian cancer", "Cervical cancer")
processed_data <- process_nested_gbd(gbd$Uganda$`Bladder cancer`$Female$`2000`,
                                     2019, female_cancers)




gbd_cases_n <- gbd %>%
      filter(measure  == "Prevalence",
             metric   == "Number",
             location == location_input,
             year     == year_val)

map(.x = gbd$Uganda$`Bladder cancer`,
    .f = ~ map(.x = .x,
               .f = ~ map(.x = .x,
                          .f = ~ head(1))))

map(.x = gbd$Uganda$`Bladder cancer`,
    .f = ~ map(.x = .x,
               # https://forum.posit.co/t/extract-single-list-element-as-part-of-a-pipeline/1095
               # .f = magrittr::extract2(1) #------------Check the 1st list item
               # .f = `[[`(1)
               map(.x = .x,
                   .f = `[[`(1))
                     # .x %>% filter(measure  == "Prevalence",
                     #            metric   == "Number",
                     #         location == location_input,
                     #         year     == year_val) %>%
                     # select(age, year, sex, location, cause, val) %>%
                     # mutate(across(
                     #       val,
                     #       ~ case_when(
                     #             # All female cancers among males have 0 prev and mortality
                     #             sex == "Male" &
                     #                   cause %in% female_cancers              ~ 0,
                     #             # All male cancers among females have 0 prev and mortality
                     #             sex == "Female" & cause == "Prostate cancer" ~ 0,
                     #             # Liver cancer is the only type with values starting at age 0
                     #             age == 0 & cause != "Liver cancer"           ~ 0,
                     #             TRUE                                         ~ .
                     #       )
                     # ))))
    ))



# gbd_cases_n <- map(.x = gbd$Uganda,
#                    .f = function(cause_lvl) {
#                          map(.x = cause_lvl,
#                              .f = function(sex_lvl) {
#                                    sex_lvl["2000"] %>%

# Get first year df for all causes and sexes
gbd_cases_n <- map_depth(.x = gbd,
                         .depth = 3,
                         function(df) {
                               # Extract data from year 2000, the starting year of simulation
                               df["2000"] %>%
                                     # Extract the 1st dataframe from a list that only has a single dataframe!
                                     magrittr::extract2(1) %>%

                                     filter(measure  == "Prevalence",
                                            metric   == "Number") %>%
                                     select(age, year, sex, location, cause, val) %>%
                                     mutate(across(
                                           val,
                                           ~ case_when(
                                                 # Liver cancer is the only type with values starting at age 0
                                                 age == 0 & cause != "Liver cancer"           ~ 0,
                                                 TRUE                                         ~ .)
                                     ))
                         }) %>%

      map_depth(.depth = 3,
                ~ expandinterpolate_counts(
                      dat_inpt = .x,
                      target   = "val",
                      new_name = "counts"))


# gbd_cases_n$Uganda$`Bladder cancer`$Female$



