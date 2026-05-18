#' @title A Function to Calculate Incidence and Mortality Rates From a Markov Trace
#' @author Claudi.ai
#' @param markov_inpt Markov trace input
#' @returns An updated Markov trace with 2 columns of incidence and mortality rates

calc_epi_rates_markov <- function(markov_inpt) {

      markov_inpt %>%
            mutate(cohort = as.character(year - age)) %>%
            group_by(cohort, sex, cause, location) %>%
            arrange(year) %>% # or age
            mutate(well_next = lead(pwell),
                   sick_next = lead(plcl),
                   dead_next = lead(pcdx),

                   # Incidence ─────────────────────────────────────────────────#
                   new_sick = pmax(0, sick_next - plcl),

                   # Person-time denominator: average well population over the interval
                   well_person_time = (pwell + pmax(0, well_next)) / 2,

                   # Incidence rate per person-year
                   inc_rate = ifelse(test = well_person_time > 0,
                                     yes  = new_sick / well_person_time,
                                     no   = NA_real_),

                   # Mortality Rate ────────────────────────────────────────#
                   # Deaths attributable to the sick pool.
                   new_dead_from_sick = pmax(0, dead_next - pcdx),

                   # Person-time denominator: average sick population over the interval
                   sick_person_time = ((pwell + plcl) + pmax(0, well_next + sick_next)) / 2,

                   # Case fatality rate per person-year
                   mrt_rate = ifelse(test = sick_person_time > 0,
                                     yes  = new_dead_from_sick / sick_person_time,
                                     no   = NA_real_)
            ) %>%
            ungroup()
}
