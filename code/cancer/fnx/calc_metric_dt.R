#' @title Calculate Metric Values from Country-Specific GBD Data
#' @author Mohamed Albirair, MBBS, MPH, PhDc; Renu Nargund, MPH
#' @param country_dt Country-specific data set, either EPI or ACM
#' @param measure_inpt A selected measure from GBD results data ("Incidence", "Prevalence", and/or "Deaths")
#' @param year_inpt A selected year for data, metric, and/or measure extraction--typically, the starting year of the simulation
#' @returns A dataframe that contains the user-defined metric

calc_metric_dt <- function(country_dt,
                           measure_inpt,
                           year_inpt) {

      # # Determine whether total pop Nx is calculated or not:
      # # Only calculate it if the inputted data is ACM data
      # if (identical(country_dt, country_epi)) {
      #       add_Nx <- FALSE
      #
      # } else if (identical(country_dt, country_acm)) {
      #       add_Nx <- TRUE
      #
      # }

      # Define counts and rate labels corresponding to "measure_inpt"
      if (identical(measure_inpt, "Incidence")) {
            counts_name <- "cIx"
            rate_name   <- "cix"

      } else if (identical(measure_inpt, "Prevalence")) {
            counts_name <- "cPx"
            rate_name   <- "cpx"

      } else if (identical(measure_inpt, "Deaths") & identical(country_dt, country_epi)) {
            counts_name <- "cDx"
            rate_name   <- "cmx"

      } else if (identical(measure_inpt, "Deaths") & identical(country_dt, country_acm)) {
            counts_name <- "acDx"
            rate_name   <- "acmx"

      } else if (identical(measure_inpt, "Population") & identical(country_dt, country_pop)) {
            counts_name <- "Nx"

      }

      # Rename columns
      data.table::setnames(x           = country_dt,
                           old         = c("pred_rates", "pred_cases", "val"),
                           new         = c("Rate", "Number", "Number"),
                           skip_absent = TRUE)

      # Set object as data.table object
      data.table::setDT(country_dt)

      # Check
      # Subset country-specific GBD epi data set to year, measure, and metric
      stack <- country_dt[year %in% year_inpt &
                                measure == measure_inpt,]

      if (!nrow(stack)) return(data.table::data.table())

      # # Grouping variables
      # cast_keys <- c("year", "sex", "location", "cause", "age")

      # Rename corresponding count and rate columns
      if ("Rate"   %in% names(stack)) data.table::setnames(stack, "Rate",   rate_name)
      if ("Number" %in% names(stack)) data.table::setnames(stack, "Number", counts_name)

      # # optional Nx
      # if (add_Nx && !is.null(counts_name)) {
      #       stack[, Nx := get(counts_name) / get(rate_name)]
      # }


      if (identical(country_dt, country_epi) | identical(country_dt, country_acm)) {
            data.table::setorder(stack, sex, cause, location, year, age)

      } else if (identical(country_dt, country_pop)) {
            data.table::setorder(stack, sex, location, year, age)

      }
      stack[]
}
