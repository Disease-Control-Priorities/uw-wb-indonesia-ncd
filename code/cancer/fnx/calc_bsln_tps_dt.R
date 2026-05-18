#' @title Calculate Scenario-Specific Transition Probabilities (TPs)
#' @author Mohamed Albirair, MBBS, MPH, PhD; Renu Nargund, MPH; Sarah Pickersgill MPH; David Watkins, MD, MPH
#'
#' @param tps_primer_input Primer for calculating TPs values
#' @param stg_dist_inpt Stage distribution data
#'
#' @returns A data.table with all baseline scenario, age-, year-, sex-, year, and location-specific TPs values

calc_bsln_tps_dt <- function(tps_inpt = tp_primers,
                             stg_inpt = bsln_stg_dist) {
                             # start          = sim_start_year,
                             # end            = sim_end_year) {

      stg <- data.table::setDT(stg_inpt)

      # Join stage distribution
      # tp_stg_dt <- merge(x     = tps_inpt,
      #                    y     = stg,
      #                    by    = "cause",
      #                    all.x = TRUE)

      tp_stg_dt <- stg[tps_inpt, on = .(cause)]

      # Add manual calibration params
      tp_stg_dt[, `:=`(
            input_tp0 = data.table::fcase(cause == cause3[1], 0.009,  # Cervix
                                          cause == cause3[2], 0.005,  # CRC
                                          default = 0),
            input_tp2 = data.table::fcase(cause == cause2[1], 0.3,    # Breast
                                          cause == cause3[1], 0.3,    # Cervix
                                          cause == cause3[2], 0.5,    # CRC
                                          cause == cause2[2], 0.24,   # Prostate
                                          cause == cause2[3], 0.3,    # Lung
                                          default = 0),
            input_tp3 = data.table::fcase(cause == cause2[1], 0.4,    # Breast
                                          cause == cause3[1], 0.4,    # Cervix
                                          cause == cause3[2], 0.6,    # CRC
                                          cause == cause2[2], 0.25,   # Prostate
                                          cause == cause2[3], 0.5,    # Lung
                                          default = 0)
      )]

      # Matching
      tp_lab <- c("w_w"     = "w_w",
                  "w_prc"   = "w_prc",
                  "w_lcl"   = "w_lcl",
                  "w_do"    = "w_do",
                  "prc_prc" = "prc_prc",
                  "prc_lcl" = "prc_lcl",
                  "prc_do"  = "prc_do",
                  "lcl_lcl" = "lcl_lcl",
                  "lcl_rgn" = "lcl_rgn",
                  "lcl_d"   = "lcl_d",
                  "lcl_do"  = "lcl_do",
                  "rgn_rgn" = "rgn_rgn",
                  "rgn_dst" = "rgn_dst",
                  "rgn_do"  = "rgn_do",
                  "dst_dst" = "dst_dst",
                  "dst_d"   = "dst_d",
                  "dst_do"  = "dst_do")

      # Compute TPs in one vectorized call
      tp_stg_dt[, names(tp_lab) :=
                  calc_indiv_tp_cat(cat       = category,
                                    s0        = precancer,
                                    s1        = local,
                                    s2        = regional,
                                    s3        = distant,
                                    w_sick    = cix,
                                    prev      = cpx,
                                    sick_d    = cmx,
                                    bgmx      = bgmx,
                                    solve_tp0 = input_tp0,
                                    solve_tp2 = input_tp2,
                                    solve_tp3 = input_tp3)[tp_lab], # mapped results and assign in correct order

            by = .(location, cause, sex, year, age)]

      # all_years <- start:end
      #
      # n_years   <- length(all_years)
      #
      # # For each row, duplicate across all years
      # out <- tp_dt[, {
      #       .SD[rep(1L, n_years)][, year := all_years][]   # replicate row n_years times
      # }, by = .(location, cause, sex, age)]
      #
      # return(out[])
      return(tp_stg_dt[])
}
