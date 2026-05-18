#' @title Extract well-to-sick, sick-to-dead, and background mortality TPs
#' @author Mohamed Albirair, MBBS, MPH, PhD; Renu Nargund, MPH; Sarah Pickersgill MPH; David Watkins, MD, MPH
#' @return A data.table with age-, sex-, cause- and country-specific crude epidemiologic probabilities

extract_tps_primers_uptd <- function(incid          = country_incd,
                                     prev           = country_prev,
                                     cmx            = country_mort,
                                     acmx           = country_acmx,
                                     # ref_year_input = reference_year, # removed, since we're no longer imposing
                                                                        # reference year's values on all calibration years
                                     age_young      = age_min,
                                     age_old        = age_max,
                                     cause_map      = list(cause1 = cause1,
                                                           cause2 = cause2,
                                                           cause3 = cause3)) {

      # 1) Incidence
      incid <- incid[
            , c(uid_vec, "cix")
            , with = FALSE
      ][
            order(location, cause, sex, year, age)
      ]

      # 2) Prevalence
      prev <- prev[
            , c(uid_vec, "cpx")
            , with = FALSE
      ][
            order(location, cause, sex, year, age)
      ]

      # 3) Cause-specific mortality (cmx)
      cmx <- cmx[
            , c(uid_vec, "cmx")
            , with = FALSE
      ][
            order(location, cause, sex, year, age)
      ]

      # 4) All-cause mortality (expand to all causes)
      all_causes <- unique(cmx$cause)

      acmx <- acmx[
      #       , cause := NULL
      # ][
            , c(uid_vec[!uid_vec == "cause"], "acmx")
            , with = FALSE
      ][
            order(location, sex, year, age)
      ]

      acmx <- acmx[, .(cause = all_causes),
                   by = .(location, sex, year, age, acmx)][
                         , .(location, cause, sex, year, age, acmx)
                   ][
                         # Apply sex-specific filtering first to reduce data size
                         (cause %in% female_cancers & sex == "Female") |
                               (cause == cause2[2] & sex == "Male") |
                               (!(cause %in% female_cancers) & cause != cause2[2])
                   ]

      # 5) Merge
      tp <- Reduce(
            f = function(x, y) {
                  merge(x, y, all = TRUE,
                        by = uid_vec)
                  },
            x = list(incid, prev, cmx, acmx))

      # 6) Drop "empty" groups (all incidence+prevalence are 0 or NA)
      #-------------------------------------------------------------------------# Not needed, I think!
      # tp <- tp[, {
      #       if ((all(is.na(incid)) || sum(incid, na.rm = TRUE) == 0) &&
      #           (all(is.na(prev))  || sum(prev,  na.rm = TRUE) == 0)) {
      #             .SD[0]  # drop group
      #       } else {
      #             .SD
      #       }
      # }, by = .(year, sex, cause, location)]

      # tp <- tp[
      #       # Apply sex-specific filtering first to reduce data size
      #       (cause %in% female_cancers & sex == "Female") |
      #             (cause == cause2[2] & sex == "Male") |
      #             (!(cause %in% female_cancers) & cause != cause2[2])
      # ]

      # 7) Derived
      tp[, bgmx := acmx - cmx]
      tp[, category := data.table::fcase(
            cause %in% cause_map$cause1, 1L,
            cause %in% cause_map$cause2, 2L,
            cause %in% cause_map$cause3, 3L,
            default = NA_integer_
      )]

      # 8) Edge correction
      tp[age == age_old, `:=`(
            cix = 0, cpx = 0, cmx = 0, acmx = 1, bgmx = 1
      )][]

      # # 9) Rename incid and prev columns
      # data.table::setnames(tp, c("cix", "cpx"), c("incid", "prev"))

      data.table::setorder(tp, cause, sex, year, age)
      tp[]
}
