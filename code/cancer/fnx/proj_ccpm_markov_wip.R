#' @title Overlay Cohort Component Projection Model with Markov Trace
#' @author Mohamed Albirair, MBBS, MPH, PhDc
#' @param pop_data A data frame that includes pop counts
#' @param markov_input Markov trace for scenario-specific, cause-specific mortality rate
#' @param fx_input A data frame of ASFR
#' @param target_input A vector listing target cancer types
#' @param omx_input A data frame of "other" (non-target) death rates

#' @return A data frame of year-, age-, sex-, country-, and scenario-specific cancer projections

proj_ccpm_markov <- function(pop_data, #     = c(bsln_current_output, intv_current_output),
                             markov_input,
                             fx_input     = fixed_fx,
                             cmx_var      = NULL) {

      current_year <- unique(pop_data$year)

      Nx_mx_df <- pop_data %>%
            select(age, year, sex, location, Nx, mx)

      ## Process scenarios-----------------------------------------------------#

      if (grepl(pattern = "bsln", x = deparse(substitute(pop_data)))) {

            scen         <- "baseline"
            cmx_var      <- NULL

            # Combine all CCPM ingredients
            all_ccpm_inputs <- purrr::reduce(
                  .x = list(Nx_mx_df,
                            fx_input),
                  .f = full_join,
                  by = join_by(age, sex, location)) %>%
                  select(age, year, sex, location, Nx, fx, mx)

      } else if (grepl(pattern = "intv", x = deparse(substitute(pop_data)))) {

            scen         <- "intervention"

            # Calculate total target cause-specific mortality rate
            #--------(Fixed in baseline scenario, but varies in intervention scenario)
            tcmx_markov <- markov_input %>%
                  filter(year  == reference_year,
                         cause %in% target_cancers) %>% #-----------------------target cancers
                  group_by(age, sex, location) %>%
                  reframe(tcmx = sum(get(cmx_var)))

            # Fixed omx
            omx_df <- full_join(
                  x  = Nx_mx_df,
                  y  = tcmx_markov,
                  by = join_by(age, sex, location)
            ) %>% mutate(omx = mx - tcmx) %>%
                  select(age, sex, location, tcmx, omx)

            # Combine all CCPM ingredients
            all_ccpm_inputs <- purrr::reduce(
                  .x = list(Nx_mx_df %>% select(!mx), # Nx: pop counts
                            fx_input,                 # fx: fertility
                            omx_df),                  # includes both tcmx: tot target mortality rate, and omx: tot non-target mortality rate
                  .f = full_join,
                  by = join_by(age, sex, location)) %>%
                  mutate(across(tcmx:omx, ~ . * Nx)) %>%
                  mutate(mx = (tcmx + omx) / Nx) %>%
                  select(age, year, sex, location, Nx, fx, mx)

      } else {

            stop("Input error: pop_data is could neither be defined as baseline nor intervention")

      }

      cat("Projecting", current_year + 1, scen, "\n")

      # Run the CCPM
      proj_Nx <- run_ccpm(pop_data = all_ccpm_inputs) %>%
            filter(year == current_year + 1) %>%
            left_join(., all_ccpm_inputs %>%
                            select(age, sex, location, fx, mx),
                      by = join_by(age, sex, location))

      return(proj_Nx)
}
