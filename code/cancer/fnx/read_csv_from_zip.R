#' @title Read *.CSV files from Zip files
#' @author DeepSeek
#' @param zip_loc A character vector of recorded filepaths to be extracted


# Function to read CSV using fread
read_csv_from_zip <- function(zip_loc) {

      # List files in zip
      zip_contents <- unzip(zipfile = zip_loc, list = TRUE)$Name

      # Find CSV file
      csv_file <- grep("\\.csv$", zip_contents, value = TRUE, ignore.case = TRUE)[1]

      if (is.na(csv_file)) {
            warning(paste("No CSV file found in", zip_loc))
            return(NULL)
      }

      # Create temporary file path for fread
      temp_file <- tempfile(fileext = ".csv")
      unzip(zip_loc, files = csv_file, exdir = tempdir())
      file.rename(file.path(tempdir(), csv_file), temp_file)

      # Read with fread
      data <- data.table::fread(temp_file)

      # Clean up temporary file
      file.remove(temp_file)

      return(data)
}
