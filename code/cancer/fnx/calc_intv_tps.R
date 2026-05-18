#' @title Calculate Intervention Scenario-Specific Transition Probabilities (TPs)
#' @author Mohamed Albirair, MBBS, MPH, PhD; Renu Nargund, MPH; Sarah Pickersgill MPH; David Watkins, MD, MPH
#'
#' @param bsln_tps_inpt Baseline TPs
#' @param tp_modif_inpt Data set that captures how the different TPs are modified with intervention coverage
#'
#' @returns A data.table of all scenario-specific TPs values



calc_intv_tps <- function(bsln_tps_inpt = bsln_tps,
                          tp_modif_inpt = intv_modif_rr) {

      # Convert inputs to data.table if they aren't already
      bsln_dt   <- data.table::as.data.table(bsln_tps_inpt)
      modif_dt  <- data.table::as.data.table(tp_modif_inpt)

      # Perform the join
      result_dt <- merge(x   = bsln_dt,
                         y   = modif_dt,
                         by  = c("cause", "year", "precancer", "local", "regional", "distant"),
                         all = TRUE)

      # Update the transition probabilities using data.table syntax
      result_dt[, `:=`(
            # Well
            w_prc   = data.table::fifelse(category == 3, effect_on_tp0 * w_prc, 0),
            w_lcl   = data.table::fifelse(category != 3, effect_on_tp1 * w_lcl, 0),

            # Precancer
            prc_lcl = data.table::fifelse(category == 3, effect_on_tp1 * prc_lcl, 0),

            # Local
            lcl_rgn = effect_on_tp2 * lcl_rgn,
            lcl_d   = data.table::fifelse(category == 1, effect_on_cfr * lcl_d, 0),

            # Regional
            rgn_dst = effect_on_tp3 * rgn_dst,

            # Distant
            dst_d   = effect_on_cfr * dst_d
      )]

      # Update the "remain" transition probabilities
      result_dt[, `:=`(
            # Well
            w_w     = 1 - (w_prc + w_lcl + w_do),

            # Precancer
            prc_prc = data.table::fifelse(category == 3, 1 - (prc_lcl + prc_do), 0),

            # Local
            lcl_lcl = data.table::fifelse(category == 1, 1 - (lcl_d + lcl_do), 1 - (lcl_rgn + lcl_do)),

            # Regional
            rgn_rgn = data.table::fifelse(category == 1, 0, 1 - (rgn_dst + rgn_do)),

            # Distant
            dst_dst = data.table::fifelse(category == 1, 0, 1 - (dst_d + dst_do))
      )]

      return(result_dt)
}
