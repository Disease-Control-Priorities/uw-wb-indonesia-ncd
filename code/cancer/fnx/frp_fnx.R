

## A function to compute log-normal parameters from mean and Gini
lognormal_params <- function(mean, gini) {
      sigma <- 2 * sqrt(2) * qnorm((gini + 1) / 2)  # Inverse error function approximation
      mu    <- log(mean) - 0.5 * sigma ^ 2
      return(list(mu = mu, sigma = sigma))
}

## A function to simulate GNI PC and Gini coefficient using log-normal distribution parameters
sim_gni_gini <- function(dat,
                         row        = 1,
                         incom_inpt = "gni_pc",
                         gini_inpt  = "gini_coef") {

      # Call the function to compute log-normal parameters
      params <- lognormal_params(mean = dat[row, incom_inpt],
                                 gini = dat[row, gini_inpt])

      # Extract key distribution parameters
      mu     <- params$mu
      sigma  <- params$sigma

      # Simulate 10,000 individuals per country
      incomes <- rlnorm(1e4, meanlog = mu, sdlog = sigma)

      # approximated mean and Gini coefficient
      approx_mean <- mean(incomes)
      approx_gini <- Rfast::gini(incomes)

      # Store in a table
      data.frame(country         = dat[row, "country"],
                 mean_income     = dat[row, incom_inpt],
                 gini_coeff      = dat[row, gini_inpt],
                 lognorm_mu      = mu,
                 lognorm_sigma   = sigma,
                 mean_income_sim = approx_mean,
                 gini_coeff_sim  = approx_gini)
}

## Generate income distribution
gen_income_dist <- function(dat,
                            n_draw = 1e4) {

      # Checks
      if (length(unique(dat$lognorm_mu)) > 1 | length(unique(dat$lognorm_sigma)) > 1) {
            stop("Data set has more than a single mu or sigma value")
      }

      names(dat)[names(dat) == "location"] <- "country"

      rlnorm(n       = n_draw,
             meanlog = unique(dat$lognorm_mu),
             sdlog   = unique(dat$lognorm_sigma))
}


## Quantify distribution
extract_percentiles <- function(data) {
      # Define the percentiles of interest
      # percentiles <- c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99)
      percentiles <- c(1, 5, seq(10, 90, 10), 95, 99)
      # Calculate the percentiles
      result <- quantile(data, probs = percentiles / 100, na.rm = TRUE)
      # Return the result
      return(result)
}

## CDF of a log-normal distribution
lognorm_cdf <- function(dat,
                        country_inpt,
                        range_start = 0.1,
                        range_end   = 10,
                        range_length = 100) {
      mu    <- dat[dat$country == country_inpt,]$lognorm_mu
      sigma <- dat[dat$country == country_inpt,]$lognorm_sigma
      x_vec <- seq(range_start, range_end, length.out = range_length)
      y_cdf <- plnorm(q = x_vec, meanlog = mu, sdlog = sigma)
      cbind(x_vec, y_cdf)
}

### FRP Calculations-----------------------------------------------------------#

calc_pov <- function(bsln_oop,
                     trgt_oop_end,
                     trgt_oop_bgn      = bsln_oop,
                     scaleup_strt_inpt = scaleup_start_year,
                     scaleup_end_inpt  = scaleup_end_year,
                     pop_project       = cost_output,
                     pov_dat_inpt      = pov_line_dat) {

      # Check
      if (any(bsln_oop     < 0 | bsln_oop     > 1,
              trgt_oop_bgn < 0 | trgt_oop_bgn > 1,
              trgt_oop_end < 0 | trgt_oop_end > 1)) {
            stop("Inputted probability value is either < 0 or > 1. Please revise.")
      }

      ## Convert data set into long format
      cost_pov_hs_long <- left_join(
            x  = pop_project %>%
                  filter(cause %in% target_cancers) %>% #---------------------------#
                  pivot_longer(cols = c(pprc, plcl, prgn, pdst),
                               names_to = "state",
                               values_to = "prop") %>%
                  mutate(state = factor(state,
                                        levels = c("pprc", "plcl", "prgn", "pdst"),
                                        labels = c("prc", "lcl", "rgn", "dst"))),
            y  = pov_dat_inpt %>%
                  select(location = country, lognorm_mu, lognorm_sigma, pov_line),
            by = join_by(location)) %>%

            arrange(scen, year, state) %>%

            mutate(category = case_when(cause %in% cause1 ~ 1,
                                        cause %in% cause2 ~ 2,
                                        cause %in% cause3 ~ 3,
                                        TRUE              ~ NA),
                   oop      = case_when(scen == "bsln" | (scen == "intv" & cause %!in% target_cancers)         ~ bsln_oop,
                                        scen == "intv" & year <= scaleup_strt_inpt & cause %in% target_cancers ~ trgt_oop_bgn,
                                        scen == "intv" & year >= scaleup_end_inpt & cause %in% target_cancers  ~ trgt_oop_end,
                                        TRUE                                                                   ~ NA_real_)) %>%
            mutate(oop = zoo::na.approx(oop),
                   .by = c(cause, scen, state, age, sex)) %>%
            mutate(cost_oop  = cost * oop,
                   pop_count = round(prop * Nx * cov),
                   pop_cost  = prop * Nx * cov * cost)


      pov_input_groups <- cost_pov_hs_long %>%
            group_by(year, cause, scen, state, lognorm_mu, lognorm_sigma, cost_oop, pov_line) %>%
            reframe(sum_pop  = sum(pop_count),
                    sum_cost = sum(pop_cost)) %>%
            filter(state != "prc") %>% # Precancer is not a target for FRP calculations
            # since no intervention targets that health state, yet!
            arrange(year, cause, scen, state)

      # Calculate prop under poverty line
      pov_cases <- pov_input_groups %>%
            split(list(.$year, .$cause, .$scen, .$state)) %>%

            # Since filtered out non-plausible states --> empty list items
            # The split() above still creates empty lists of non-existing combinations
            # e.g., prostate cancer in females
            # https://search.r-project.org/CRAN/refmans/vctrs/html/list_drop_empty.html#:~:text=list_drop_empty()%20removes%20empty%20elements,0L)%20.
            vctrs::list_drop_empty() %>%

            map(.x = .,
                .f = function(dset) {

                      set.seed(123)

                      # Generate scen-sepcific isncome distribution, reading mu and sigma values
                      dstrbn_pre <- gen_income_dist(dat    = dset,
                                                    n_draw = dset$sum_pop)

                      # Shift income distribution after subtracting treatment/intervention costs
                      dstrbn_post <- dstrbn_pre - dset$cost_oop
                      # Implemented zero-floor for negative post-payment incomes, since Costs cannot be negative!
                      dstrbn_post <- ifelse(dstrbn_post < 0, 0, dstrbn_post)

                      # Tabulate costs: above and below the poverty line
                      # https://stackoverflow.com/questions/1617061/include-levels-of-zero-count-in-result-of-table
                      tbl_dstrbn_pre  <- table(factor(dstrbn_pre <= dset$pov_line,
                                                      levels = c(TRUE, FALSE)))

                      tbl_dstrbn_post <- table(factor(dstrbn_post <= dset$pov_line,
                                                      levels = c(TRUE, FALSE)))

                      # Append to original data set
                      dat_post_cost <- dset %>%
                            mutate(trgt_oop            = ifelse(scen == "bsln", bsln_oop, trgt_oop_end),
                                   under_pov_line_pre  = tbl_dstrbn_pre[names(tbl_dstrbn_pre) == "TRUE"],
                                   above_pov_line_pre  = tbl_dstrbn_pre[names(tbl_dstrbn_pre) == "FALSE"],
                                   under_pov_line_post = tbl_dstrbn_post[names(tbl_dstrbn_post) == "TRUE"],
                                   above_pov_line_post = tbl_dstrbn_post[names(tbl_dstrbn_post) == "FALSE"],
                                   delta_pov           = under_pov_line_post - under_pov_line_pre)

                      return(dat_post_cost)
                }
            ) %>%

            list_rbind()

      return(list(all_groups = cost_pov_hs_long,
                  pov_cases  = pov_cases))
}


## Poverty averted
calc_dth_pov_avrt <- function(bsln_oop,
                              trgt_oop_end,
                              trgt_oop_bgn = bsln_oop,
                              cum_metric   = c(TRUE, FALSE),
                              scaleup_year = scaleup_end_year,
                              trgt_year    = scaleup_end_year,
                              pop_project  = cost_output,
                              pov_dat_inpt = pov_line_dat) {

      pov_count <- calc_pov(bsln_oop,
                            trgt_oop_bgn,
                            trgt_oop_end,
                            scaleup_year,
                            pop_project,
                            pov_dat_inpt)

      oop_combn <- map(
            .x = trgt_oop_end, # was oop_vec, but I think that was wrong!
            # .y = cum_metric, # I don't think this is needed here!
            .f = ~ calc_pov(bsln_oop     = bsln_oop,
                            trgt_oop_end = .x,
                            scaleup_year = scaleup_year,
                            pop_project  = pop_project,
                            pov_dat_inpt = pov_dat_inpt) %>%
                  group_by(year, scen, cause) %>%
                  reframe(sum_pov = sum(delta_pov)) %>%
                  group_by(scen) %>% # had "oop" before, but dropped for now...
                  mutate(cum_pov = cumsum(sum_pov))) %>%

            list_rbind()

      scen_output <- full_join(
            x  = pop_project %>%
                  filter(cause %in% target_cancers,
                         year <= trgt_year) %>%
                  group_by(year, scen, cause) %>%
                  reframe(deaths = sum(pcdx * Nx)) %>%
                  pivot_wider(id_cols     = c(cause, year),
                              names_from  = scen,
                              values_from = deaths) %>%
                  group_by(cause, year) %>%
                  reframe(dth_avrt = bsln - intv),
            y  = pov_count %>%
                  filter(year <= trgt_year) %>%
                  group_by(year, scen, cause) %>%
                  reframe(n_pov = sum(delta_pov)) %>%
                  pivot_wider(id_cols     = c(cause, year),
                              names_from  = scen,
                              values_from = n_pov) %>%
                  group_by(cause, year) %>%
                  reframe(pov_avrt = bsln - intv),
            by = join_by(year, cause)
      ) %>%
            # group_by(scen) %>%
            mutate(cum_dth_avrt = cumsum(dth_avrt),
                   cum_pov_avrt = cumsum(pov_avrt))

      if (cum_metric) {
            dth_avrt <- round(scen_output$cum_dth_avrt[scen_output$year == trgt_year])
            pov_avrt <- round(scen_output$cum_pov_avrt[scen_output$year == trgt_year])
      } else {
            dth_avrt <- round(scen_output$dth_avrt[scen_output$year == trgt_year])
            pov_avrt <- round(scen_output$pov_avrt[scen_output$year == trgt_year])
      }

      output <- c(dth_avrt, pov_avrt)
      names(output) <- c("Deaths averted", "Poverty averted")

      # return(output)
      return(scen_output)
}
