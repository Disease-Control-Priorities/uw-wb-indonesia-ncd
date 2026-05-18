#' @title Run Cohort Component Projection Model
#' @author William T. Msemburi, PhD
#' @param pop_data description
#' @param srb sex ratio at birth

run_ccpm <- function(pop_data,
                     srb = 0.5) {

      if (length(unique(pop_data$year)) > 1) {
            stop("Population data set has more than a single year value")
      }

      # Data prep--------------------------------------------------------------#
      n_age       <- length(age_min:age_max)
      n_sex       <- 2
      pop         <- array(dim = c(2, n_sex, n_age))
      deaths      <- array(dim = c(1, n_sex, n_age))
      births      <- array(dim = c(1, n_sex))

      df          <- pop_data

      start_year  <- unique(df$year)

      pop_in      <- rbind(subset(df, sex == "Female")$Nx,
                           subset(df, sex == "Male")$Nx)

      mx_in       <- rbind(subset(df, sex == "Female")$mx,
                           subset(df, sex == "Male")$mx)

      fx_in       <- rbind(subset(df, sex == "Female")$fx,
                           subset(df, sex == "Male")$fx)

      pop[1, , ]  <- pop_in
      Sx          <- t(apply(mx_in, 1, gen_lifetable, "Sx"))

      # Cohort Component projection model--------------------------------------#

      # ages 1:(age_max - 1) -> 2:age_max
      deaths[1, , 2:n_age] = pop[1, , 1:(n_age - 1)] * (1.0 - Sx[, 2:n_age])
      pop[2, , 2:n_age]    = pop[1, , 1:(n_age - 1)] * Sx[, 2:n_age]

      # ages age_max -> age_max+ (beyond)
      deaths[1, , n_age]   = deaths[1, , n_age] + pop[1, , n_age] * (1.0 - Sx[, n_age])
      pop[2, , n_age]      = pop[2, , n_age] + pop[1, , n_age] * Sx[, n_age]

      # age 0
      tbirths              = sum(0.5 * (pop[2, 1, 11:55] + pop[1, 1, 11:55]) * fx_in[1, 11:55])
      births[1, 2]         = tbirths * srb / (1 + srb)
      births[1, 1]         = tbirths - births[1,2]
      deaths[1, , 1]       = births[1, ] * (1.0 - Sx[,1])
      pop[2, , 1]          = births[1, ] * Sx[, 1]

      pop_output <- data.table::as.data.table(ceiling(pop)) |>
            rename(year = V1, sex = V2, age = V3) |>
            as.data.frame() |>
            mutate(year     = year + start_year - 1,
                   sex      = factor(sex, labels = c("Female", "Male")),
                   age      = age - 1,
                   location = unique(df$location)) |>
            select(age, sex, year, location, Nx = value)
}
