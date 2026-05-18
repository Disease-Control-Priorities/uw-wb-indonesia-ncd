#' @title Save Index data as *.RData file
#' @author Mohamed Albirair, MBBS, MPH, PhD
#' @param index_input Compiled index data
#' @details
#' The function assess whether target .RData file exists, and if so if it contains an identical data frame. If not, it creates the .RData file and/or updates it with the target index data frame.



save_index_rdata <- function(index_input) {

      # Set SDP category
      if (grepl(pattern = "phrm", x = deparse(substitute(index_input)))) {
            sdp_ctg <- "phrm"

      } else if (grepl(pattern = "fclty", x = deparse(substitute(index_input)))) {
            sdp_ctg <- "fclty"

      } else {
            stop("Cannot determine dataset type (pharmacy or facility). Please, revise inputted value to 'index_input'.")

      }

      # Set file path
      file_path <- paste0("R/outputs/uom_indices_", sdp_ctg, ".RData")

      # Get the name of the input object
      obj_name <- deparse(substitute(index_input))


      if (!file.exists(file_path)) {

            # File doesn't exist, so save the object
            save(list = deparse(substitute(index_input)), file = file_path)
            message("Created new uom_indices_fclty.RData")

      } else {
            # File exists, check if it already contains object
            # Load the file into a temporary environment to check contents
            temp_env <- new.env()
            loaded_objects <- load(file  = file_path,
                                   envir = temp_env)

            if (obj_name %in% loaded_objects) {

                  # Get the object from the temporary environment
                  existing_obj <- get(obj_name, envir = temp_env)

                  # Compare with the current object
                  if (identical(index_input, existing_obj)) {
                        # They are identical - skip saving
                        message("Object already exists in uom_indices_fclty.RData - skipping")

                  }

            } else {
                  # Object doesn't exist in file, so add it using resave
                  cgwtools::resave(list = deparse(substitute(index_input)), file = file_path)
                  message("Added object to existing uom_indices_fclty.RData")
            }

            # Clean up temporary environment
            rm(temp_env)
      }
}
