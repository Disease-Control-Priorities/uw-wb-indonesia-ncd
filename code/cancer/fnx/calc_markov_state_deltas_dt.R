#' @title Calculate Incidence and Mortality Rates from Markov Trace
#' @author Renu Nargund, MS; Mohamed Albirair, MBBS, MPH, PhD
#' @param markov_trace Markov trace: baseline or intervention
#' @param

`%between%` <- data.table::`%between%`

calc_markov_state_deltas_dt <- function(
            markov_trace,
            cols = c("pwell", "pprc", "plcl", "prgn", "pdst", "pcdx", "pbgdx"),
            year_range = c(2000L, 2050L),
            age_range  = c(1L, 75L),
            year_col = "year",
            age_col  = "age",
            delta_prefix = "d_",
            check_unique_year_age = FALSE
) {
      dt <- data.table::as.data.table(markov_trace)

      miss_keys <- setdiff(c(year_col, age_col), names(dt))
      if (length(miss_keys)) stop("Missing key columns: ", paste(miss_keys, collapse = ", "))

      miss_cols <- setdiff(cols, names(dt))
      if (length(miss_cols)) stop("Missing state columns: ", paste(miss_cols, collapse = ", "))

      dt[, (year_col) := as.integer(get(year_col))]
      dt[, (age_col)  := as.integer(get(age_col))]

      y0 <- as.integer(year_range[1]); y1 <- as.integer(year_range[2])
      a0 <- as.integer(age_range[1]);  a1 <- as.integer(age_range[2])


      dt_sub <- dt[
            get(year_col) %between% c(y0, y1 + 1L) &
                  get(age_col)  %between% c(a0, a1 + 1L)
      ]

      if (check_unique_year_age) {
            dupN <- dt_sub[, .N, by = c(year_col, age_col)][N > 1L, .N]
            if (nrow(dupN) > 0) {
                  stop("Found duplicate rows for the same (year, age). ",
                       "Delta merge would expand rows. Fix upstream or aggregate first.")
            }
      }

      # shifted "next" table: align (year+1, age+1) back onto (year, age)
      next_dt <- dt_sub[, c(year_col, age_col, cols), with = FALSE]
      next_dt[, (year_col) := get(year_col) - 1L]
      next_dt[, (age_col)  := get(age_col)  - 1L]
      data.table::setnames(next_dt, cols, paste0("next_", cols))

      # merge by (year, age)
      data.table::setkeyv(dt_sub, c(year_col, age_col))
      data.table::setkeyv(next_dt, c(year_col, age_col))
      out <- next_dt[dt_sub]

      # compute deltas
      for (j in cols) {
            out[, (paste0(delta_prefix, j)) := get(paste0("next_", j)) - get(j)]
      }

      # drop helper next_*
      out[, (paste0("next_", cols)) := NULL]

      # restrict to requested output window
      out <- out[
            get(year_col) %between% c(y0, y1) &
                  get(age_col)  %between% c(a0, a1)
      ]

      out
}
