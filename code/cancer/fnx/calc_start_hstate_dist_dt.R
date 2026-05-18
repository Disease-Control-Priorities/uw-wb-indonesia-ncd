#' @title Generate country-specific starting health state distribution
#' @author Mohamed Albirair, MBBS, PhD; Renu Nargund, MPH; Sarah Pickersgill, MPH; David Watkins, MD, MPH
#' @param markov_correct
#' @param stg_dist_inpt Stage distribution data

#' @returns A data.table that captures the health state distribution in the first year of the simulation

calc_start_hstate_dist_dt <- function(markov_correct  = c(TRUE, FALSE),  # markov_correction,
                                      stg_dist_inpt   = bsln_stg_dist,
                                      n_cases_inpt    = country_prev,    # prevalence counts: location,sex,year,age,cause,counts  (Nx not required here)
                                      n_cdeaths_inpt  = country_mort,    # cause-specific deaths: location,sex,year,age,cause,cDx
                                      n_acdeaths_inpt = country_acmx,    # ACM: location,sex,year,age,acmx,acDx,Nx,(cause="All causes")
                                      n_pop_inpt      = country_nx) {

      ## Extract counts of people in each health state from corresponding data sources
      cases  <- data.table::as.data.table(n_cases_inpt)[, .(location, sex, year, age, cause, cPx)][order(location, year, age, cause)]
      deaths <- data.table::as.data.table(n_cdeaths_inpt)[, .(location, sex, year, age, cause, cDx)][order(location, year, age, cause)]
      acm    <- data.table::as.data.table(n_acdeaths_inpt)[, .(location, sex, year, age, acDx)][order(location, year, age)]
      pop    <- data.table::as.data.table(n_pop_inpt)[, .(location, sex, year, age, Nx)][order(location, year, age)]
      # corr   <- data.table::as.data.table(markov_correct)[, .(location, sex, year, cause, age, cumm_cqx, cumm_acqx)][order(location, year, age, cause)]
      stg    <- data.table::as.data.table(stg_dist_inpt)  # cause, precancer, local, regional, distant

      ## Combine all 3 data sets, sequentially
      # Merge cases + cause deaths
      data.table::setkey(cases, location, sex, year, age, cause)
      data.table::setkey(deaths,location, sex, year, age, cause)
      hsd <- deaths[cases, on = .(location, sex, year, age, cause)]  # left join cases

      # Attach ACM (by location/sex/year/age only)
      data.table::setkey(acm, location, sex, year, age)
      data.table::setkey(hsd, location, sex, year, age)
      hsd <- acm[hsd]  # ACM on left to ensure Nx present; rows without Nx will be NA

      # Attach Nx (by location/sex/year/age only)
      data.table::setkey(pop, location, sex, year, age)
      data.table::setkey(hsd, location, sex, year, age)
      hsd <- pop[hsd]

      if (markov_correct) {

            # Prep correction data before combining
            corr <- data.table::as.data.table(markov_correction)[, .(location, sex, year, cause, age, cumm_cqx, cumm_acqx)][order(location, year, age, cause)]

            # Markov correction overwrites with cumulative risks
            data.table::setkey(corr, location, sex, year, cause, age)
            data.table::setkey(hsd,  location, sex, year, cause, age)
            hsd  <- corr[hsd]  # ensure cumm_* available

            hsd[, `:=`(
                  pacdx = cumm_acqx,
                  pcdx  = cumm_cqx
            )]

      } else {
            # Probabilities from cPx/Nx (bounded)
            hsd[, `:=`(
                  pcdx  = data.table::fifelse(!is.na(cDx) & !is.na(Nx), pmax(pmin(cDx / Nx, 1), 0), 0),
                  pacdx = data.table::fifelse(!is.na(acDx) & !is.na(Nx), pmax(pmin(acDx / Nx, 1), 0), 0)
            )]

      }

      # Probabilities from cPx/Nx (bounded)
      hsd[, `:=`(
            prev  = data.table::fifelse(!is.na(cPx) & !is.na(Nx), pmax(pmin(cPx / Nx, 1), 0), 0)
      )]

      hsd[, `:=`(
            prev  = prev * (1 - pacdx), # Prevalence proportion
            pwell = 1 - (prev + pacdx)  #
      )]

      # Stage proportions by cause
      data.table::setkey(stg, cause)
      data.table::setkey(hsd, cause)
      hsd <- stg[hsd]


      hsd[, `:=`(
            pprc = prev * precancer,
            plcl = prev * local,
            prgn = prev * regional,
            pdst = prev * distant
      )]

      # Final shape (matches your select/mutate in the original)
      hsd[, .(age, year, sex, location, cause,
              pwell, pprc, plcl, prgn, pdst,
              pcdx, pbgdx = pacdx - pcdx)]
}
