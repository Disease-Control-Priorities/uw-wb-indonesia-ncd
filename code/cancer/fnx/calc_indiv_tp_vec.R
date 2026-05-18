#' @title Calculate Individual Transition Probabilities (TPs)
#' @author Mohamed Albirair, MBBS, MPH, PhD; Renu Nargund, MPH; Sarah Pickersgill MPH; David Watkins, MD, MPH
#'
#' @param cat cancer category (1, 2, or 3)
#' @param s0 precancer stage
#' @param s1 local cancer stage
#' @param s2 regional cancer stage
#' @param s3 distant cancer state
#' @param w_sick probability of transitioning from well to sick (any stage)
#' @param prev probability of having the disease (any stage)
#' @param sick_d probability of transitioning distant cancer to death (case-fatality rate)
#' @param bgmx probability of transition to death from any other state (background mortality)
#' @param solve_tp0 placeholder for the probability of transitioning from well to precancer
#' @param solve_tp2 placeholder for the probability of transitioning from local to regional
#' @param solve_tp3 placeholder for the probability of transitioning from regional to distant
#'
#' @return A vector the different TP values
#' @export

calc_indiv_tp_vec <- function(cat,
                              s0,
                              s1,
                              s2,
                              s3,
                              w_sick,
                              prev,
                              sick_d,
                              bgmx,
                              solve_tp0,
                              solve_tp2,
                              solve_tp3) {

      # Prop local out of cancer
      prop_lcl <- s1 / (s1 + s2 + s3)

      # Neoplasia ratio
      neoplasia_cancer_ratio <- (s0 + s1 + s2 + s3) / (s1 + s2 + s3)
      neoplasia_prev         <- neoplasia_cancer_ratio * prev
      prc_prev               <- neoplasia_prev * s0

      # --- WELL
      w_prc <- data.table::fifelse(cat == 3, solve_tp0, 0)
      w_lcl <- data.table::fifelse(cat != 3, w_sick * prop_lcl, 0)
      w_do  <- bgmx
      w_w   <- 1 - (w_prc + w_lcl + w_do)

      # --- PRE-CANCER
      prc_lcl <- data.table::fifelse(cat == 3, (w_sick * prop_lcl) / pmax(prc_prev, 1e-12), 0)
      prc_do  <- data.table::fifelse(cat == 3, bgmx, 0)
      prc_prc <- data.table::fifelse(cat == 3, pmax(1 - (prc_lcl + prc_do), 0), 0)

      # prc_list$prc_prc <- ifelse(cat == 3, 1 - sum(unlist(prc_list)), 0)

      # --- LOCAL
      lcl_rgn <- data.table::fifelse(cat == 1, 0, solve_tp2)

      # print(c(sick_d, prev))
      lcl_d   <- data.table::fifelse(test = cat != 1,
                                     pmin(pmax(sick_d / pmax(prev, 1e-12), 0), 1 - bgmx),
                                     0)
      lcl_do  <- bgmx
      lcl_lcl <- pmax(1 - (lcl_rgn + lcl_d + lcl_do), 0)


      # --- REGIONAL
      rgn_dst <- data.table::fifelse(cat == 1, 0, solve_tp3)
      rgn_do  <- data.table::fifelse(cat == 1, 0, bgmx)
      rgn_rgn <- data.table::fifelse(cat == 1, 0, 1 - (rgn_dst + rgn_do))

      # --- DISTANT
      dst_d   <- data.table::fifelse(cat == 1, 0,
                                     pmin(pmax(sick_d / pmax(prev * s3, 1e-12), 0), 1 - bgmx))
      dst_do  <- data.table::fifelse(cat == 1, 0, bgmx)
      dst_dst <- data.table::fifelse(cat == 1, 0, 1 - (dst_d + dst_do))

      return(list(
            w_w     = w_w,
            w_prc   = w_prc,
            w_lcl   = w_lcl,
            w_do    = w_do,
            prc_prc = prc_prc,
            prc_lcl = prc_lcl,
            prc_do  = prc_do,
            lcl_lcl = lcl_lcl,
            lcl_rgn = lcl_rgn,
            lcl_d   = lcl_d,
            lcl_do  = lcl_do,
            rgn_rgn = rgn_rgn,
            rgn_dst = rgn_dst,
            rgn_do  = rgn_do,
            dst_dst = dst_dst,
            dst_d   = dst_d,
            dst_do  = dst_do
      ))
}
