#' @title Project Counts using Matrix Multiplication for a Single Year
#' @author Mohamed Albirair, MBBS, MPH, PhD; Sarah Pickersgill, MPH; Renu Nargund, MPH; David Watkins, MD, MPH
#' @param tps_inpt A nested list (location, cancer, sex, year, age) of the annual transition probability values between health states
#' @param hsd_inpt A data.table capturing the relative health state state distribution
#' @returns A data frame of age-, year-, sex-, location, and cancer-specific projected relative health state distribution

project_markov_trace_dt <- function(tps_inpt,
                                    hsd_inpt   = start_hstate_dist,
                                    start      = sim_start_year,
                                    end        = sim_end_year,
                                    age_old    = age_max,
                                    clamp_zero = TRUE) {

      # Define health states
      state_cols <- c("pwell", "pprc", "plcl", "prgn", "pdst", "pcdx", "pbgdx")

      # transition columns in the *exact* row-major order used by your original matricize_tps
      all_trans_cols <- c(
            # Well row (7)
            "w_w",   "w_prc",   "w_lcl",   "w_rgn",   "w_dst",   "w_d",   "w_do",
            # Precancer row (7)
            "prc_w", "prc_prc", "prc_lcl", "prc_rgn", "prc_dst", "prc_d", "prc_do",
            # Local row (7)
            "lcl_w", "lcl_prc", "lcl_lcl", "lcl_rgn", "lcl_dst", "lcl_d", "lcl_do",
            # Regional row (7)
            "rgn_w", "rgn_prc", "rgn_lcl", "rgn_rgn", "rgn_dst", "rgn_d", "rgn_do",
            # Distant row (7)
            "dst_w", "dst_prc", "dst_lcl", "dst_rgn", "dst_dst", "dst_d", "dst_do"
      )

      # Quick label-match check
      # if (all(all_trans_cols %in% names(tps_inpt))) {
      #       stop("tps_inpt column names don't match what is expected for this function!")
      # }

      # defensive copies + keys
      tps <- data.table::copy(tps_inpt); data.table::setkey(tps, location, sex, cause, year, age)
      hsd <- data.table::copy(hsd_inpt); data.table::setkey(hsd, location, sex, cause, year, age)

      # store results by year; include the baseline year as-is
      results <- list()
      results[[as.character(start)]] <- hsd[year == start]

      # Initialize progress bar
      pb <- cli::cli_progress_bar(
            name  = "Processing",
            total = length(start:(end - 1)),  # Number of iterations
            clear = FALSE
      )

      # loop years
      for (yr in seq(start, end - 1L)) {

            # Update progress bar
            cli::cli_progress_update()

            current <- results[[as.character(yr)]]

            # join TPMs for this year's rows
            dt <- tps[current, on = .(location, sex, cause, year = year, age = age), nomatch = 0L]
            # dt_check <- tps[current, on = .(location, sex, cause, year, age), nomatch = 0L] #----identical!

            # group apply per (loc,sex,cause,age,year): build TPM from w_w:dst_do, project prop_vec -> next_vec
            next_year <- dt[, {

                  ## 1) current state vector (length 7)
                  # prop_vec <- as.numeric(.SD[, ..state_cols])
                  prop_vec  <- as.numeric(.SD[, .SD, .SDcols = state_cols])

                  # check------------------------------------------------------#
                  # check_dt <- dt[, {prop_vec  <- as.numeric(.SD[, .SD, .SDcols = state_cols])},
                  #                by = .(location, sex, cause, age, year)]
                  #------------------------------------------------------------#

                  ## 2) build TPM vector in the same order as matricize_tps
                  # take available transitions; fill missing with 0 (because calc_bsln_tps_inpt sets many to 0 implicitly)
                  vals      <- mget(x          = all_trans_cols,
                                    ifnotfound = as.list(rep(x     = 0,
                                                             times = length(all_trans_cols))))

                  trans_vec <- as.numeric(unlist(vals, use.names = FALSE))

                  # check------------------------------------------------------#
                  # check_vals <- dt[, {mget(x          = all_trans_cols,
                  #                          ifnotfound = as.list(rep(x     = 0,
                  #                                                   times = length(all_trans_cols))))},
                  #                  by = .(location, sex, cause, age, year)]
                  # check_trans_vec <- dt[, {as.numeric(unlist(check_vals, use.names = FALSE))},
                  #                       by = .(location, sex, cause, age, year)]
                  #------------------------------------------------------------#

                  # append absorbing rows: pcdx row = [0,0,0,0,0,1,0], pbgdx row = [0,0,0,0,0,0,1]
                  tpm_vec   <- c(trans_vec, rep(x = 0, times = 5), 1, rep(x = 0, times = 7), 1)

                  ## 3) shape into 7x7 TPM (by row)
                  tpm       <- matrix(data     = tpm_vec,
                                      nrow     = length(state_cols),
                                      ncol     = length(state_cols),
                                      byrow    = TRUE,
                                      dimnames = list(state_cols, state_cols))

                  ## 4) project one year
                  next_vec  <- as.numeric(prop_vec %*% tpm)

                  ## return only the states; by-cols (incl. age/year) are auto-attached
                  setNames(as.list(next_vec), state_cols)

            }, by = .(location, sex, cause, age, year)]

            # update age/year AFTER the grouped calc (no duplicate columns)
            next_year[, `:=`(age = age + 1L, year = year + 1L)]

            # add newborn cohort at age 0 for next year (pwell = 1, others 0)
            new_births <- unique(current[, .(location, sex, cause)])
            new_births[, `:=`(age = 0L, year = yr + 1L)]
            for (s in state_cols) new_births[[s]] <- if (s == "pwell") 1 else 0

            # combine and cap age
            combined <- rbind(next_year, new_births, fill = TRUE)[age <= age_old]

            results[[as.character(yr + 1L)]] <- combined
      }

      # Close progress bar
      cli::cli_progress_done()

      out <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)


      # # For further processing
      # keys <- c("location", "cause", "sex")
      #
      # # ensure proper ordering by age within each (loc, cause, sex, year)
      # data.table::setkeyv(out, c(keys, "year", "age"))
      #
      # # compute within-year deltas age→age+1
      # out[, `:=`(
      #       inc_rate    = data.table::shift(plcl, type = "lead", fill = plcl[.N]) - plcl,
      #       cdeath_rate = data.table::shift(pcdx, type = "lead", fill = pcdx[.N]) - pcdx
      # ),
      # by = keys
      # ]

      #------------------------------------------------------------------------#
      # Deepseek
      # keys <- c("location", "cause", "sex")
      #
      # # Step 1: Ensure proper ordering
      # # Sort by location, cause, sex, year, age (ascending)
      # data.table::setorderv(out, c(keys, "year", "age"))
      #
      # # Step 2: Create shifted dataset with age+1 and year+1
      # out_shifted <- out[, .(
      #       location, cause, sex,
      #       year = year,        # Keep original year for joining
      #       age = age,          # Keep original age for joining
      #       # Create matching keys for the NEXT year and NEXT age
      #       year_match = year + 1,
      #       age_match = age + 1,
      #       plcl_next = plcl,
      #       pcdx_next = pcdx
      # )]
      #
      # # Step 3: Join back to original data using the match keys
      # # This matches: original (year, age) with shifted (year_match, age_match)
      # out <- merge(out,
      #              out_shifted[, .(location, cause, sex,
      #                              year = year_match,  # This becomes the join year
      #                              age = age_match,    # This becomes the join age
      #                              plcl_next, pcdx_next)],
      #              by = c(keys, "year", "age"),
      #              all.x = TRUE,
      #              sort = FALSE)  # Keep original order
      #
      # # Step 4: Calculate differences
      # # For a person age a in year y, plcl_next is the probability for age a+1 in year y+1
      # out[, `:=`(
      #       inc_rate = plcl_next - plcl,
      #       cdeath_rate = pcdx_next - pcdx
      # )]
      #
      # # Step 5: Clean up
      # out[, c("plcl_next", "pcdx_next") := NULL]
      #
      # # Step 6: Restore original order if needed
      # data.table::setorderv(out, c(keys, "year", "age"))
      #------------------------------------------------------------------------#

      # # clamp small negatives to zero (numeric noise / rounding)
      # if (clamp_zero) {
      #       out[inc_rate    < 0, inc_rate    := 0]
      #       out[cdeath_rate < 0, cdeath_rate := 0]
      # }

      # drop last age per (loc, cause, sex, year) (no lead value)
      # out <- out[!is.na(inc_rate) & !is.na(cdeath_rate),
      #           .(keys, age, inc_rate, cdeath_rate)]

      # data.table::setorder(out, cause, sex, year, age)
      out[]
}
