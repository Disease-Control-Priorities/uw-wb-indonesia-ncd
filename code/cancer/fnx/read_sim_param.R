#' @title Process User-Defined Stage Distribution Data
#' @author Mohamed Albirair, MBBS, MPH, PhD
#' @param user_def_stg_dstrb User-defined stage distribution data
#'
#' @return An updated stage distribution data frame

read_sim_param <- function(path_input = "R/inputs/sim_scen_inputs.xlsx") {

      # Read main input values
      input_param <- readxl::read_excel(path  = path_input,
                                        sheet = "ui", # user interface sheet
                                        range = "B2:B5")

      # Identify country
      country_select <- names(input_param)

      # Identify the country setting, tot pop, and screenable cancer prevalence for future calculations
      # country_setting <- pull(income_setting[match(country_select, income_setting$location), "setting"])
      country_setting <- pull(input_param)[1]

      # Clean stage-distribution data------------------------------------------#
      setting_cascade_stg_dist <- map(
            .x = cascade_stg_dist_list,
            .f = ~ .x %>%
                  filter(setting == country_setting) %>% # set the income setting
                  select(!c(intervention, total))) # not needed in further analyses

      # Read the label for stage-distribution data source:
      # Assumed vs. User-defined-----------------------------------------------#
      stg_dist_source <- readxl::read_excel(path  = path_input,
                                            sheet = "ui",
                                            range = "B7:B7",
                                            col_names = FALSE) %>%
            as.character() # extract the single-Excel cell (B6) value

      # Read the stage distribution data according to the defined label
      if (stg_dist_source == "User-defined") {

            # Refer to user-defined stage distribution data
            bsln_cascade_stg_dist <- readxl::read_excel(path  = path_input,
                                                        sheet = "ui",
                                                        range = "A8:E24") %>%
                  rename_all(tolower) %>%
                  # rename_with(~ paste0("bsln_", .), !cause)

                  full_join(x  = .,
                            y  = setting_cascade_stg_dist$bsln %>%
                                  select(!precancer:distant),
                            by = join_by(cause))

            # Update the original (assumed values) list
            setting_cascade_stg_dist$bsln <- bsln_cascade_stg_dist

      } else if (stg_dist_source == "Assumed") {

            # Use assumed stage distribution
            bsln_cascade_stg_dist <- setting_cascade_stg_dist$bsln

      } else {

            stop("Error: Please specify an input value to the source of cancer-stage distribution (cell B6 on the Excel file):\neither 'Assumed' or 'User-defined'")

      }

      # Checks errors in inputted user-defined stage distribution
      if (stg_dist_source == "User-defined" &
          any(is.na(pull(bsln_cascade_stg_dist %>%
                         select(cause, precancer, local, regional, distant))))) {
            stop("Error: Please provide stage distribution values for all cancer types.\nIf no user-defined data available, then select ")

      } else if (stg_dist_source == "User-defined" &
                 any(rowSums(bsln_cascade_stg_dist %>%
                             select(precancer, local, regional, distant)) != 1)) {
            stop("Error: Probabilities for at least one cancer type do not sum to 1.")

      }

      # Identify target cancer types and treatment coverage values
      trt_cov <- readxl::read_xlsx(path  = path_input,
                                   sheet = "ui",
                                   range = "A36:F52") %>%
            magrittr::set_colnames(c("cause", "target",
                                     "bsln_init", "bsln_adh",
                                     "trgt_init", "trgt_adh"))

      target_cancers <- trt_cov %>%
            filter(target) %>%
            pull(cause)

      # Baseline treatment coverage
      bsln_trt_cov <- trt_cov %>%
            select(cause, starts_with("bsln_"))

      # Treatment coverage list
      trt_cov_list <- list(
            bsln = trt_cov %>% select(cause, starts_with("bsln_")),
            trgt = trt_cov %>%
                  filter(cause %in% target_cancers) %>%
                  select(cause, starts_with("trgt_")) %>%
                  full_join(x  = .,
                            y  = bsln_trt_cov %>%
                                  filter(cause %!in% target_cancers) %>%
                                  rename(trgt_init = bsln_init, trgt_adh = bsln_adh),
                            by = join_by(cause, trgt_init, trgt_adh))
      )

      # Update baseline cascade and stage distribution data
      bsln_cascade_stg_dist <- full_join(
            x  = bsln_cascade_stg_dist %>%
                  select(!(treated:completed)),
            y  = trt_cov_list$bsln %>%
                  rename(treated = bsln_init, completed = bsln_adh),
            by = join_by(cause)
      ) %>%
            select(cause, setting, scaleup,
                   screened, diagnosed, treated, completed,
                   precancer, local, regional, distant)

      # Update target cascade and stage distribution data
      # Assumption (25 Feb 2025): fixed stage distribution across baseline and target data
      trgt_cascade_stg_dist <- full_join(
            x  = bsln_cascade_stg_dist %>%
                  select(!(treated:completed)), # Everything else is identical to baseline
            y  = trt_cov_list$trgt %>%
                  rename(treated = trgt_init, completed = trgt_adh),
            by = join_by(cause)
      ) %>%
            mutate(scaleup = "end") %>%
            select(cause, setting, scaleup,
                   screened, diagnosed, treated, completed,
                   precancer, local, regional, distant)

      # All outputs
      list(input_param           = input_param,
           cascade_stg_dst       = setting_cascade_stg_dist,
           # trt_cov               = trt_cov,
           target_cancers        = target_cancers,
           # trt_cov_list          = trt_cov_list,
           bsln_cascade_stg_dist = bsln_cascade_stg_dist,
           trgt_cascade_stg_dist = trgt_cascade_stg_dist)
}
