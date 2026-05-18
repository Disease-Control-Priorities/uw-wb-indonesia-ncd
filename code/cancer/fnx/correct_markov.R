#' @title A function for Correcting Epi Metrics to Account for Previous Years in Markov Projections
#' @author Mohamed Albirair MBBS, MPH, PhD; Renu Nargund, MPH; David Watkins, MD, MPH
#' @param acm_inpt All-cause mortality data set
#' @param epi_inpt Cause-specific mortality data set

#' @return A data.table with corrected mortality data for Markov projections


correct_markov <- function(acm_inpt = country_acmx,
                           epi_inpt = country_mort,
                           start_yr = sim_start_year) {

      # Expand ACM ("All causes") across the causes present in cmx (cause-specific)
      acm_exp    <- data.table::as.data.table(acm_inpt)[, year == start_yr, .(location, sex, year, age, acmx, Nx)]
      cause_list <- unique(data.table::as.data.table(epi_inpt)$cause)
      acm_exp    <- acm_exp[, .(cause = cause_list), by = .(location, sex, year, age, acmx, Nx)]

      # Compute cumulative risks per (location, sex, year, cause) using ACM + CMX
      cmx <- data.table::as.data.table(epi_inpt)[, year == start_yr, .(location, sex, year, age, cause, cmx)]

      data.table::setkey(acm_exp, location, sex, year, cause, age)
      data.table::setkey(cmx,     location, sex, year, cause, age)

      both <- cmx[acm_exp, on = .(location, sex, year, cause, age), nomatch = 0L]

      both[, {
            data.table::setorder(.SD, age)
            ac <- acmx; ca <- cmx

            n_age <- length(ac)
            nx    <- rep(1, n_age)
            # Convert rates to probabilities
            acqx  <- 1 - exp(-nx * ac); acqx[n_age] <- 1
            cqx   <- 1 - exp(-nx * ca); cqx[n_age]  <- 1
            # acqx  <- rate_to_prob(rate = ac, time = nx); acqx[n_age] <- 0.6
            # cqx   <- rate_to_prob(rate = ca, time = nx); cqx[n_age]  <- 0.4

            # Ensure cause-specific deaths don't exceed all-cause deaths (Deepseek)
            cqx <- pmin(cqx, acqx)

            # (ax,lx,acdx) not required for the correction output, keep minimal here
            data.table::data.table(
                  age        = 0:(n_age - 1),
                  cumm_cqx   = 1 - cumprod(1 - cqx),
                  cumm_acqx  = 1 - cumprod(1 - acqx)
            )
      }, by = .(location, sex, year, cause)]

      # test_both <- both[, {
      #       data.table::setorder(.SD, age)
      #       ac <- acmx; ca <- cmx
      #
      #       n_age <- length(ac)
      #       nx    <- rep(1, n_age)
      #       # Convert rates to probabilities
      #       # acqx  <- 1 - exp(-nx * ac); acqx[n_age] <- 1
      #       # cqx   <- 1 - exp(-nx * ca); cqx[n_age]  <- 1
      #       acqx  <- rate_to_prob(rate = ac, time = nx); acqx[n_age] <- 1
      #       cqx   <- rate_to_prob(rate = ca, time = nx); cqx[n_age]  <- 1
      #
      #       # Ensure cause-specific deaths don't exceed all-cause deaths (Deepseek)
      #       cqx <- pmin(cqx, acqx)
      #
      #       # (ax,lx,acdx) not required for the correction output, keep minimal here
      #       data.table::data.table(
      #             age        = 0:(n_age - 1),
      #             cumm_cqx   = 1 - cumprod(1 - cqx),
      #             cumm_acqx  = 1 - cumprod(1 - acqx)
      #       )
      # }, by = .(location, sex, year, cause)]
}
