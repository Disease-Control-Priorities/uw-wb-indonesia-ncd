#------------------------------------------------------------------------------#
# LCC&HS: Cancer Modeling
# Author: Mohamed Albirair
# See README.md for information on usage
#------------------------------------------------------------------------------#

### load packages
library(tidyverse)
# library(data.table)

## Load settings
z <- yaml::read_yaml("settings.yml")

# Home folder
z$home_dir <- "R/"

# R code, inputs, and outputs
if (!exists("root")){
      root <- z$home_dir
}

z$fnx_dir     <- paste0(root, "fnx")
z$inputs_dir  <- paste0(root, "inputs")
z$outputs_dir <- paste0(root, "outputs")
z$scripts_dir <- paste0(root, "scripts")

## Source manual functions
# list.files(path       = z$fnx_dir,
#            pattern    = "\\.R$",
#            full.names = TRUE) %>%
#       walk(.f        = safely(.f = ~ source(.x, echo = FALSE)),
#            .progress = TRUE)

# Fast sourcing
loc_fnx <- list.files(path       = z$fnx_dir,
                      pattern    = "\\.R$",
                      full.names = TRUE)

for (i in loc_fnx) {
      source(i, echo = FALSE)
}

## Unique ID vec
uid_vec <- c("age", "year", "sex", "cause", "location")


## Load functions/scripts from URLs
#-----check if connected to the internet:
# https://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r
if (curl::has_internet()) {
      # `not it`
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/misc/notin.R")

      # ggplot theme_caviz
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/viz_ggplot/theme_caviz.R")

      # Report missing
      # source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/misc/report_missing.R")
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/misc/report_missing.R")

      # Convert probability to rate
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/epi/prob_to_rate.R")

      # Convert rate to probability
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/epi/rate_to_prob.R")

      # Explore an .RData file
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/misc/explore_rdata_obj.R")

      # Save/Update an .RData file
      source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/refs/heads/main/R/misc/save_rdata.R")

} else {
      # `not it`
      source(z$notin)

      # ggplot theme_caviz
      source(z$caviz_theme)

      # Report missingness
      source(z$report_missing)

      # Convert probability to rate
      source(z$prob_to_rate)

      # Convert rate to probability
      source(z$rate_to_prob)

      # Explore an .RData file
      source(z$explore_rdata)

      # Save/update an .RData file
      source(z$save_rdata_file)
}
