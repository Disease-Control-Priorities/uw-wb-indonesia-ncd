#' @title Load Country Data from Parent GBD List as a data.table Object
#' @author Mohamed Albirair, MBBS, MPH, PhD; Renu Nargund, MPH
#' @param file_input Parent file (.RData file)
#' @param obj_input Desired object (large list containing multiple country data)
#' @param country_input Desired country
#'
#' @returns A list containing the selected country data


load_country_dt <- function(file_input,
                            obj_input,
                            country_input) {

      # Build a temporary environment and load the large object data
      env <- new.env()
      load(file_input, envir = env)

      # Check
      if (!exists(obj_input, envir = env)) {
            stop("Object not found")
      }

      # Convert to data.table and filter efficiently
      dt <- data.table::as.data.table(get(obj_input, envir = env))
      dt[location == country_input]
}
