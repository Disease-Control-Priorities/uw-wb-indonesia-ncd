#' @title Expand the TP Primers Data Set to Include all Target Simulation Years
#' @author Mohamed Albirair, MBBS, MPH, PhD; Renu Nargund, MPH; Sarah Pickersgill MPH; David Watkins, MD, MPH
#'
#' @param tps_primer_input Primer for calculating TPs values
#' @param stg_dist_input Stage distribution data
#'
#' @returns A data frame of all scenario-specific TPs values

expand_tps_years_dt <- function(tp_dt = bsln_tps,
                                start = sim_start_year,
                                end   = sim_end_year) {

      all_years <- start:end

      n_years   <- length(all_years)

      # For each row, duplicate across all years
      out <- tp_dt[, {
            .SD[rep(1L, n_years)][, year := all_years][]   # replicate row n_years times
      }, by = .(location, cause, sex, age)]

      return(out[])
}
