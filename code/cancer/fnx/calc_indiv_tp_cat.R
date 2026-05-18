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

calc_indiv_tp_cat <- function(cat,
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

      if (cat == 1) {

            if (!all(s0 == 0 & s1 == 1 & s2 == 0 & s3 == 0)) {

                  cat("Rectify stage distribution for category 1 cancer = %100 local")

            }

            # Well-------------------------------------------------------------#
            w_prc   <- 0
            w_lcl   <- w_sick # * prop_lcl # (prop_lcl should be 1, %100)
            w_do    <- bgmx
            w_w     <- 1 - (w_prc + w_lcl + w_do)

            # Precancer--------------------------------------------------------#
            prc_lcl <- 0
            prc_do  <- 0
            prc_prc <- 0

            # Local------------------------------------------------------------#
            lcl_rgn <- 0
            lcl_d   <- pmin(pmax(sick_d / pmax(prev, 1e-12), 0), 1 - bgmx)
            lcl_do  <- bgmx
            lcl_lcl <- pmax(1 - (lcl_rgn + lcl_d + lcl_do), 0)

            # Regional---------------------------------------------------------#
            rgn_dst <- 0
            rgn_do  <- 0
            rgn_rgn <- 0

            # Distant----------------------------------------------------------#
            dst_d   <- 0
            dst_do  <- 0
            dst_dst <- 0

            #==================================================================#

      } else if (cat == 2) {

            # Prop local out of cancer
            prop_lcl <- s1 / (s1 + s2 + s3)

            # Well-------------------------------------------------------------#
            w_prc   <- 0
            w_lcl   <- w_sick * prop_lcl
            w_do    <- bgmx
            w_w     <- 1 - (w_prc + w_lcl + w_do)

            # Precancer--------------------------------------------------------#
            prc_lcl <- 0
            prc_do  <- 0
            prc_prc <- 0

            # Local------------------------------------------------------------#
            lcl_rgn <- solve_tp2
            lcl_d   <- 0
            lcl_do  <- bgmx
            lcl_lcl <- pmax(1 - (lcl_rgn + lcl_d + lcl_do), 0)


            # Regional---------------------------------------------------------#
            rgn_dst <- solve_tp3
            rgn_do  <- bgmx
            rgn_rgn <- 1 - (rgn_dst + rgn_do)

            # Distant----------------------------------------------------------#
            dst_d   <- pmin(pmax(sick_d / pmax(prev * s3, 1e-12), 0), 1 - bgmx)
            dst_do  <- bgmx
            dst_dst <- 1 - (dst_d + dst_do)

            #==================================================================#

      } else if (cat == 3) {

            # Prop local out of cancer
            prop_lcl <- s1 / (s1 + s2 + s3)

            # Neoplasia ratio
            neoplasia_cancer_ratio <- (s0 + s1 + s2 + s3) / (s1 + s2 + s3)
            neoplasia_prev         <- neoplasia_cancer_ratio * prev
            prc_prev               <- neoplasia_prev * s0

            # Well-------------------------------------------------------------#
            w_prc   <- solve_tp0
            w_lcl   <- 0
            w_do    <- bgmx
            w_w     <- 1 - (w_prc + w_lcl + w_do)

            # Precancer--------------------------------------------------------#
            prc_lcl <- (w_sick * prop_lcl) / pmax(prc_prev, 1e-12)
            prc_do  <- bgmx
            prc_prc <- pmax(1 - (prc_lcl + prc_do), 0)

            # Local------------------------------------------------------------#
            lcl_rgn <- solve_tp2
            lcl_d   <- 0
            lcl_do  <- bgmx
            lcl_lcl <- pmax(1 - (lcl_rgn + lcl_d + lcl_do), 0)

            # Regional---------------------------------------------------------#
            rgn_dst <- solve_tp3
            rgn_do  <- bgmx
            rgn_rgn <- 1 - (rgn_dst + rgn_do)

            # Distant----------------------------------------------------------#
            dst_d   <- pmin(pmax(sick_d / pmax(prev * s3, 1e-12), 0), 1 - bgmx)
            dst_do  <- bgmx
            dst_dst <- 1 - (dst_d + dst_do)

      }


      return(list(
            # The names below and their order have to match how they're called in
            # in `calc_bsln_tps_dt()`
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
