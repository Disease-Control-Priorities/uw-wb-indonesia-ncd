

# To work on this function to understand changes with data table
calc_rates <- function(trace_inpt,
                       state_inc    = "plcl",   # which stage’s delta defines “incidence”
                       state_cdeath = "pcdx",   # cancer-death absorbing state
                       keys         = c("location","cause","sex","year"),
                       age_col      = "age",
                       clamp_zero   = TRUE) {

      dt <- data.table::as.data.table(trace_inpt)

      # sanity checks
      stopifnot(all(c(keys, age_col, state_inc, state_cdeath) %in% names(dt)))

      # ensure proper ordering by age within each (loc, cause, sex, year)
      data.table::setkeyv(dt, c(keys, age_col))

      # compute within-year deltas age→age+1
      dt[, `:=`(
            inc_rate    = data.table::shift(get(state_inc), type = "lead") - get(state_inc),
            cdeath_rate = data.table::shift(get(state_cdeath), type = "lead") - get(state_cdeath)
      ),
      by = keys
      ]

      # clamp small negatives to zero (numeric noise / rounding)
      if (clamp_zero) {
            dt[inc_rate    < 0, inc_rate    := 0]
            dt[cdeath_rate < 0, cdeath_rate := 0]
      }

      # drop last age per (loc, cause, sex, year) (no lead value)
      out <- dt[!is.na(inc_rate) & !is.na(cdeath_rate),
                .(location, cause, sex, year,
                  age = get(age_col),
                  inc_rate,
                  cdeath_rate)]

      data.table::setorder(out, cause, sex, year, age)
      out[]
}
