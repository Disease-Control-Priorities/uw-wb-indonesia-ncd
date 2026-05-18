#' @title Generate Abrdiged Life Tables
#' @author William T. Msemburi, PhD
#' @param mx A vector of age-specific, cumulative all-cause mortality rate values
#' @param parm description?

gen_lifetable <- function(mx, parm = NULL) {
      n_age     <- length(mx)
      nx        <- rep(1, n_age)
      px        <- exp(-nx * mx)
      qx        <- 1 - px
      qx[n_age] <- 1
      ax        <- (nx + 1 / mx - nx / qx)
      lx        <- c(1, cumprod(1 - qx)[1:(n_age - 1)])
      dx        <- c(rev(diff(rev(lx))), lx[1] - sum(rev(diff(rev(lx)))))
      nLx       <- nx * lx - (nx - ax) * dx
      Tx        <- rev(cumsum(rev(nLx)))
      ex        <- Tx / lx
      Sx        <- nLx / c(1, nLx)[1:n_age]
      Sx[n_age] <- nLx[n_age] / (nLx[n_age - 1] + nLx[n_age])
      df_lt     <- data.frame(age = c(0:(n_age - 1)),
                              ax, mx, lx, qx, dx, nLx, Tx, ex, Sx)
      if (is.null(parm)) {
            df_lt
      } else {
            df_lt %>% pull(parm)
      }
}
