################################################################################
# INDONESIA INTEGRATED NCD MODEL — MARKOV ENGINE
# R/engine.R
# ─────────────────────────────────────────────────────────────────────────────
# Sourced by all run scripts. Provides:
#   - Global constants: AGES, SEXES
#   - Interpolation helpers
#   - Demographic backbone projection
#   - Markov state machine (run_cause_module, run_direct_mortality_module)
#   - Module set runner (run_module_set)
#   - Output computation (compute_q4030, compute_deaths_averted)
#   - Scenario modifiers (coverage ramps, intervention TPM modifications)
#   - Module factory functions (make_cvd_module, make_direct_cvd_mortality_module,
#                               make_cervical_module)
#
# ── ARCHITECTURAL DESIGN ──────────────────────────────────────────────────────
# The engine separates the DEMOGRAPHIC BACKBONE from the DISEASE STATE LAYER.
#
#   Demographic backbone (project_population_exposure):
#     Builds a [year × sex × age] count array from sf.wpp by ageing survivors
#     forward each year and re-projecting births. Authoritative population
#     exposure for every projection year.
#
#   Disease state layer (run_cause_module):
#     Each module carries its own state distribution as COUNTS against the
#     demographic population. Within each year:
#       1. Apply TPM to current state counts (within-age transitions).
#       2. Extract cause-specific deaths as outflow to dead_cause.
#       3. Age surviving alive-state counts from age a to a+1.
#       4. Re-align total alive counts to WPP demographic target at t+1.
#
# ── MODULE INTERFACE ──────────────────────────────────────────────────────────
# A module spec is a named list with required fields:
#   $id               character string
#   $states           character vector (must include "well", dead_cause_state,
#                     dead_bg_state)
#   $dead_cause_state name of cause-specific dead state
#   $dead_bg_state    name of background dead state
#   $init_prob_fun    function(sex, ages) → [length(ages) × k] probability matrix
#   $tpm_fun          function(year, sex, ages, scenario) → named list of K×K TPMs
#   $eligible_fun     function(sex, ages, year) → logical vector (optional)
#
# ── TPM CONVENTION ────────────────────────────────────────────────────────────
# Rows = FROM state, columns = TO state. All rows sum to 1.
# Deaths extracted as outflow increment to dead_cause in one step:
#   deaths[a] = counts_post[a, dead_cause] - counts_pre[a, dead_cause]
#   mx_cause[a] = deaths[a] / pop[a]   (denominator from WPP backbone)
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

AGES  <- 0:100
SEXES <- c("Female", "Male")

###############################################################################
# SECTION 1 — VALIDATION HELPERS
###############################################################################

# Assert x is numeric in [0,1] with no NAs.
assert_prob <- function(x, tol = 1e-9, label = "object") {
  if (any(is.na(x)))        stop(label, " contains NA.")
  if (any(x < -tol))        stop(label, " contains values < 0. Min = ", min(x))
  if (any(x > 1 + tol))     stop(label, " contains values > 1. Max = ", max(x))
  invisible(TRUE)
}

# Validate a single K×K TPM: square, no NA, all in [0,1], rows sum to 1.
validate_tpm <- function(tpm, tol = 1e-8, label = "TPM") {
  if (!is.matrix(tpm))        stop(label, " must be a matrix.")
  if (nrow(tpm) != ncol(tpm)) stop(label, " must be square.")
  assert_prob(tpm, tol = tol, label = label)
  rs <- rowSums(tpm)
  if (any(abs(rs - 1) > tol)) {
    bad <- which(abs(rs - 1) > tol)
    stop(label, " row sums not 1 for rows: ",
         paste(bad, "(sum=", round(rs[bad], 8), ")", collapse = ", "))
  }
  invisible(TRUE)
}

# Validate a list of TPMs keyed by age character (keys must cover all of ages).
validate_tpm_list <- function(tpm_list, ages = AGES, tol = 1e-8, label = "TPM list") {
  miss <- setdiff(as.character(ages), names(tpm_list))
  if (length(miss) > 0)
    stop(label, " missing ages: ", paste(head(miss, 5), collapse = ", "))
  walk(ages, function(a)
    validate_tpm(tpm_list[[as.character(a)]], tol = tol,
                 label = paste0(label, "[age ", a, "]")))
  invisible(TRUE)
}


# Call a module TPM function while allowing newer modules to receive the current
# state counts and population exposure. Older TPM functions keep the original
# four-argument interface and are called unchanged.
call_tpm_fun <- function(tpm_fun, year, sex, ages, scenario,
                         counts = NULL, pop_exposure = NULL) {
  args <- list(year = year, sex = sex, ages = ages, scenario = scenario)
  fmls <- names(formals(tpm_fun))
  if ("counts" %in% fmls)       args$counts <- counts
  if ("pop_exposure" %in% fmls) args$pop_exposure <- pop_exposure
  do.call(tpm_fun, args)
}

# Validate a [n_ages × k] matrix of state probability vectors.
validate_state_probs <- function(prob_mat, tol = 1e-8, label = "state probs") {
  if (!is.matrix(prob_mat)) stop(label, " must be a matrix.")
  assert_prob(prob_mat, tol = tol, label = label)
  rs <- rowSums(prob_mat)
  if (any(abs(rs - 1) > tol)) {
    bad <- which(abs(rs - 1) > tol)
    stop(label, " rows don't sum to 1 for age indices: ",
         paste(head(bad - 1, 5), collapse = ", "),
         " (sums: ", paste(round(rs[bad], 8), collapse = ", "), ")")
  }
  invisible(TRUE)
}

###############################################################################
# SECTION 2 — INTERPOLATION HELPERS
###############################################################################

# Piecewise-constant (block) interpolation from 5-yr GBD age groups to
# single-year ages. Each single-year age inherits the value of its
# enclosing 5-yr group. Preferred over log-linear for non-monotone age
# patterns (cervical cancer, dementia).
interp_piecewise_constant <- function(age_mids, values, ages_out = 0:100) {
  breaks <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45,
              50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 101)
  vapply(ages_out, function(ag) {
    b <- findInterval(ag, breaks, rightmost.closed = TRUE)
    b <- min(max(b, 1L), length(values))
    values[b]
  }, numeric(1))
}

# Piecewise log-linear interpolation within each 5-yr GBD band.
# Interpolates between adjacent midpoints on the log scale. Falls back to
# piecewise-constant for bands with near-zero values.
interp_piecewise_loglinear <- function(age_mids, values, ages_out = 0:100,
                                       floor_value = 1e-12) {
  ord      <- order(age_mids)
  age_mids <- age_mids[ord]
  values   <- pmax(values[ord], floor_value)
  n        <- length(age_mids)

  vapply(ages_out, function(a) {
    if (a <= age_mids[1])  return(values[1])
    if (a >= age_mids[n])  return(values[n])
    j  <- max(which(age_mids <= a))
    if (j >= n) return(values[n])
    x0 <- age_mids[j];    x1 <- age_mids[j + 1]
    y0 <- log(values[j]); y1 <- log(values[j + 1])
    w  <- (a - x0) / (x1 - x0)
    exp((1 - w) * y0 + w * y1)
  }, numeric(1))
}

###############################################################################
# SECTION 3 — DEMOGRAPHIC BACKBONE
###############################################################################

# Project a [n_years × 2 × 101] population count array from an sf.wpp object.
#
# Reproduces the EXACT projection algorithm used by get.par() to back-solve
# migration residuals, so pop_array exactly recovers the WPP target surface:
#   1. Survive by Sx = nLx[a] / nLx[a-1] (life-table survival ratio, not exp(-mx)).
#   2. Births from average of current and previous year fertile-age female pop.
#   3. Age-0 population = births × Sx[0] (infant survival).
#   4. Apply net migration as multiplicative rate after all of the above.
#
# pop_array[ti, s, a] = population at START of year years_all[ti].

project_population_exposure <- function(p_country, get_lt_fn = get.lt) {
  yrs  <- p_country$years
  n_y  <- length(yrs)
  n_s  <- 2L
  n_a  <- 101L

  # Life-table survival ratios Sx for each year and sex
  Sx_arr <- array(NA_real_, dim = c(n_y, n_s, n_a))
  for (ti in seq_len(n_y)) {
    for (si in seq_len(n_s)) {
      lt <- get_lt_fn(p_country$mx[ti, si, ])
      Sx_arr[ti, si, ] <- lt$Sx
    }
  }

  pop <- array(0, dim = c(n_y, n_s, n_a),
               dimnames = list(year = yrs, sex = SEXES, age = AGES))
  pop[1, , ] <- p_country$base.pop

  for (ti in 2:n_y) {
    Sx_t   <- Sx_arr[ti, , ]
    asfr_t <- if (ti <= nrow(p_country$asfr)) p_country$asfr[ti, ] else
              p_country$asfr[nrow(p_country$asfr), ]
    srb_t  <- if (ti <= length(p_country$srb)) p_country$srb[ti] else
              p_country$srb[length(p_country$srb)]
    mig_t  <- if (ti <= dim(p_country$mig)[1]) p_country$mig[ti, , ] else
              p_country$mig[dim(p_country$mig)[1], , ]

    popin <- matrix(0, nrow = n_s, ncol = n_a)

    # Age survivors; open group 100+ absorbs both 99→100 and 100+→100+
    popin[, 2:n_a] <- pop[ti - 1, , 1:(n_a - 1)] * Sx_t[, 2:n_a]
    popin[, n_a]   <- popin[, n_a] + pop[ti - 1, , n_a] * Sx_t[, n_a]

    # Births: average of pre- and post-ageing female fertile-age population
    avg_fert_pop  <- 0.5 * (popin[1, 11:55] + pop[ti - 1, 1, 11:55])
    births_total  <- sum(avg_fert_pop * asfr_t, na.rm = TRUE)
    male_births   <- births_total * srb_t / (1 + srb_t)
    female_births <- births_total - male_births

    popin[1, 1] <- female_births * Sx_t[1, 1]
    popin[2, 1] <- male_births   * Sx_t[2, 1]

    pop[ti, , ] <- pmax(popin * (1 + mig_t), 0)
  }

  pop
}

# Convert the [year × sex × age] array to a long data frame.
population_array_to_df <- function(pop_array) {
  yrs <- as.integer(dimnames(pop_array)[[1]])
  sxs <- dimnames(pop_array)[[2]]
  ags <- as.integer(dimnames(pop_array)[[3]])
  expand_grid(year = yrs, sex = sxs, age = ags) |>
    mutate(pop = as.vector(aperm(pop_array, c(3, 2, 1))))
}

###############################################################################
# SECTION 4 — AGEING WITHIN THE DISEASE LAYER
###############################################################################

# Shift alive-state counts from age a to a+1 within one projection step.
# Dead states are NOT aged (they are cumulative absorbing accumulators).
age_forward_alive_counts <- function(counts_mat, alive_idx) {
  n_a  <- nrow(counts_mat)
  n_st <- ncol(counts_mat)
  out  <- matrix(0, nrow = n_a, ncol = n_st)
  out[2:n_a, alive_idx] <- counts_mat[1:(n_a - 1), alive_idx]
  # Open age group 100+: survivors of 99 AND existing 100+ stay in 100+
  out[n_a, alive_idx] <- out[n_a, alive_idx] + counts_mat[n_a, alive_idx]
  out
}

# Re-align alive state totals to the WPP demographic target, preserving
# relative state composition (well : prevalent : ...). Ages with zero
# alive counts are seeded entirely to the "well" state.
align_alive_to_target <- function(counts_mat, target_pop, alive_idx, well_idx) {
  alive_tot <- rowSums(counts_mat[, alive_idx, drop = FALSE])

  zero_idx <- which(alive_tot <= 0)
  if (length(zero_idx) > 0) {
    counts_mat[zero_idx, alive_idx] <- 0
    counts_mat[zero_idx, well_idx]  <- target_pop[zero_idx]
    alive_tot[zero_idx]              <- target_pop[zero_idx]
  }

  scale <- ifelse(alive_tot > 0, target_pop / alive_tot, 1)
  counts_mat[, alive_idx] <- counts_mat[, alive_idx, drop = FALSE] * scale
  counts_mat[counts_mat < 0] <- 0
  counts_mat
}

# Warm up a module's alive composition to a quasi-stationary baseline.
# Repeats the reference-year TPMs with population held fixed at start-year
# exposure. Dead state counts are reset each cycle (pseudo-history deaths
# are not real). Goal: stabilise alive composition before the real projection.
warmup_module_counts <- function(counts, module_spec, sex, n_cycles,
                                 target_pop, scenario = list(), year_ref) {
  if (n_cycles <= 0) return(counts)

  state_names    <- module_spec$states
  k              <- length(state_names)
  dead_cause_idx <- match(module_spec$dead_cause_state, state_names)
  dead_bg_idx    <- match(module_spec$dead_bg_state,    state_names)
  well_idx       <- match("well", state_names)
  alive_idx      <- setdiff(seq_len(k), c(dead_cause_idx, dead_bg_idx))

  for (cy in seq_len(n_cycles)) {
    eligible <- if (!is.null(module_spec$eligible_fun))
      module_spec$eligible_fun(sex = sex, ages = AGES, year = year_ref)
    else
      rep(TRUE, length(AGES))

    tpm_list <- call_tpm_fun(
      module_spec$tpm_fun,
      year         = year_ref,
      sex          = sex,
      ages         = AGES,
      scenario     = scenario,
      counts       = counts,
      pop_exposure = target_pop
    )
    tpm_list <- imap(tpm_list, function(tpm, age_chr) {
      a <- as.integer(age_chr)
      if (!eligible[a + 1L]) { m <- diag(1, k, k); return(m) }
      tpm
    })

    transitioned <- matrix(0, nrow = length(AGES), ncol = k)
    for (a in AGES)
      transitioned[a + 1L, ] <- as.numeric(
        counts[a + 1L, , drop = FALSE] %*% tpm_list[[as.character(a)]])

    counts_next <- matrix(0, nrow = length(AGES), ncol = k)
    aged <- age_forward_alive_counts(transitioned, alive_idx)
    counts_next[, alive_idx] <- aged[, alive_idx]
    counts_next <- align_alive_to_target(counts_next, target_pop,
                                         alive_idx, well_idx)

    eligible_next <- if (!is.null(module_spec$eligible_fun))
      module_spec$eligible_fun(sex = sex, ages = AGES, year = year_ref)
    else
      rep(TRUE, length(AGES))

    for (i in which(!eligible_next)) {
      counts_next[i, alive_idx] <- 0
      counts_next[i, well_idx]  <- target_pop[i]
    }

    counts <- counts_next
  }

  counts
}

###############################################################################
# SECTION 5 — CORE MARKOV RUNNER
###############################################################################

# Run a single disease module against the demographic population array.
#
# Arguments:
#   module_spec   module list (see interface at top of file)
#   pop_array     [n_years × 2 × 101] array from project_population_exposure()
#   proj_years    integer vector of years to store (NULL = all start-of-interval)
#   warmup_cycles  integer vector whose LENGTH gives the number of base-year
#                  stabilisation cycles. Year values are unused — the engine
#                  applies start-year TPMs for length(warmup_cycles) iterations.
#   scenario      named list of scenario parameters passed to tpm_fun
#   store_states  logical: save full state distribution?
#
# Returns list:
#   $mx      tibble: module, year, sex, age, mx_cause, deaths_cause
#   $states  tibble: state counts if store_states = TRUE, else NULL

run_cause_module <- function(module_spec,
                             pop_array,
                             proj_years   = NULL,
                             warmup_cycles = NULL,
                             scenario     = list(),
                             store_states = FALSE) {

  yrs_all <- as.integer(dimnames(pop_array)[[1]])
  if (length(yrs_all) < 2)
    stop("pop_array must contain at least two years.")
  if (any(diff(yrs_all) != 1L))
    stop("pop_array years must be consecutive integers.")

  if (is.null(proj_years))   proj_years   <- yrs_all[-length(yrs_all)]
  if (is.null(warmup_cycles)) warmup_cycles <- integer(0)
  years_store <- intersect(proj_years, yrs_all[-length(yrs_all)])

  if (length(years_store) == 0)
    stop("No projection years overlap valid start-of-interval years in pop_array.")

  state_names    <- module_spec$states
  k              <- length(state_names)
  dead_cause_idx <- match(module_spec$dead_cause_state, state_names)
  dead_bg_idx    <- match(module_spec$dead_bg_state,    state_names)
  well_idx       <- match("well",                       state_names)

  if (is.na(dead_cause_idx) || is.na(dead_bg_idx) || is.na(well_idx))
    stop("Module '", module_spec$id, "' states must include 'well', '",
         module_spec$dead_cause_state, "', '", module_spec$dead_bg_state, "'.")

  alive_idx  <- setdiff(seq_len(k), c(dead_cause_idx, dead_bg_idx))
  mx_rows    <- list()
  state_rows <- list()

  for (sx in SEXES) {
    s_idx <- match(sx, SEXES)

    init_probs <- module_spec$init_prob_fun(sex = sx, ages = AGES)
    validate_state_probs(init_probs,
                         label = paste0(module_spec$id, "/", sx, " initial probs"))

    counts <- init_probs * pop_array[1, s_idx, ]

    if (length(warmup_cycles) > 0)
      counts <- warmup_module_counts(
        counts      = counts,
        module_spec = module_spec,
        sex         = sx,
        n_cycles    = length(warmup_cycles),
        target_pop  = pop_array[1, s_idx, ],
        scenario    = scenario,
        year_ref    = yrs_all[1]
      )

    for (ti in seq_len(length(yrs_all) - 1L)) {
      yr      <- yrs_all[ti]
      yr_next <- yrs_all[ti + 1L]

      if (store_states && yr %in% years_store) {
        s_tbl <- as_tibble(counts)
        colnames(s_tbl) <- state_names
        state_rows[[length(state_rows) + 1L]] <- s_tbl |>
          mutate(module = module_spec$id, year = yr, sex = sx, age = AGES) |>
          relocate(module, year, sex, age)
      }

      eligible <- if (!is.null(module_spec$eligible_fun))
        module_spec$eligible_fun(sex = sx, ages = AGES, year = yr)
      else
        rep(TRUE, length(AGES))

      tpm_list <- call_tpm_fun(
        module_spec$tpm_fun,
        year         = yr,
        sex          = sx,
        ages         = AGES,
        scenario     = scenario,
        counts       = counts,
        pop_exposure = pop_array[ti, s_idx, ]
      )
      tpm_list <- imap(tpm_list, function(tpm, age_chr) {
        a <- as.integer(age_chr)
        if (!eligible[a + 1L]) { m <- diag(1, k, k); return(m) }
        tpm
      })
      validate_tpm_list(tpm_list, ages = AGES,
                        label = paste0(module_spec$id, "/", sx, "/", yr))

      transitioned <- matrix(0, nrow = length(AGES), ncol = k)
      for (a in AGES)
        transitioned[a + 1L, ] <- as.numeric(
          counts[a + 1L, , drop = FALSE] %*% tpm_list[[as.character(a)]])

      d_cause  <- pmax(transitioned[, dead_cause_idx] - counts[, dead_cause_idx], 0)
      exposure <- pmax(pop_array[ti, s_idx, ], 1e-12)
      mx_cause <- d_cause / exposure

      if (yr %in% years_store) {
        mx_rows[[length(mx_rows) + 1L]] <- tibble(
          module       = module_spec$id,
          year         = yr,
          sex          = sx,
          age          = AGES,
          mx_cause     = mx_cause,
          deaths_cause = d_cause
        )
      }

      counts_next <- matrix(0, nrow = length(AGES), ncol = k)
      counts_next[, dead_cause_idx] <- transitioned[, dead_cause_idx]
      counts_next[, dead_bg_idx]    <- transitioned[, dead_bg_idx]
      aged <- age_forward_alive_counts(transitioned, alive_idx)
      counts_next[, alive_idx] <- counts_next[, alive_idx] + aged[, alive_idx]

      target_next <- pop_array[ti + 1L, s_idx, ]
      counts_next <- align_alive_to_target(counts_next, target_next,
                                           alive_idx, well_idx)

      eligible_next <- if (!is.null(module_spec$eligible_fun))
        module_spec$eligible_fun(sex = sx, ages = AGES, year = yr_next)
      else
        rep(TRUE, length(AGES))

      for (i in which(!eligible_next)) {
        counts_next[i, alive_idx] <- 0
        counts_next[i, well_idx]  <- target_next[i]
      }

      counts <- counts_next
    }
  }

  list(mx     = bind_rows(mx_rows),
       states = if (length(state_rows) > 0) bind_rows(state_rows) else NULL)
}

###############################################################################
# SECTION 6 — OUTPUT HELPERS
###############################################################################

# Compute 40q30 from cause-specific mx and WPP all-cause mx.
# When baseline_mx_df is supplied, the life table is adjusted to reflect
# scenario mortality reductions:
#   mx_adj = mx_wpp - sum(mx_baseline) + sum(mx_scenario)
# Pass baseline_mx_df = NULL for the baseline scenario itself.
compute_q4030 <- function(mx_df, wpp_mx_df, baseline_mx_df = NULL) {

  if (!is.null(baseline_mx_df)) {
    bsln_sum <- baseline_mx_df |>
      group_by(year, sex, age) |>
      summarise(mx_bsln = sum(mx_cause, na.rm = TRUE), .groups = "drop")
    scen_sum <- mx_df |>
      group_by(year, sex, age) |>
      summarise(mx_scen = sum(mx_cause, na.rm = TRUE), .groups = "drop")
    wpp_mx_use <- wpp_mx_df |>
      left_join(bsln_sum, by = c("year", "sex", "age")) |>
      left_join(scen_sum,  by = c("year", "sex", "age")) |>
      mutate(
        mx_bsln = replace_na(mx_bsln, 0),
        mx_scen = replace_na(mx_scen, 0),
        mx_wpp  = pmax(mx_wpp - mx_bsln + mx_scen, 0)
      ) |>
      select(year, sex, age, mx_wpp)
  } else {
    wpp_mx_use <- wpp_mx_df
  }

  lx_df <- wpp_mx_use |>
    filter(age >= 30, age <= 69) |>
    arrange(year, sex, age) |>
    group_by(year, sex) |>
    mutate(px = exp(-mx_wpp),
           lx = cumprod(dplyr::lag(px, default = 1))) |>
    ungroup()

  q_all <- lx_df |>
    mutate(dx = lx * (1 - px)) |>
    group_by(year, sex) |>
    summarise(q4030 = sum(dx), .groups = "drop") |>
    mutate(cause = "All-cause")

  cause_detail <- mx_df |>
    filter(age >= 30, age <= 69) |>
    left_join(lx_df |> select(year, sex, age, lx, px, mx_wpp),
              by = c("year", "sex", "age"))

  if (any(is.na(cause_detail$mx_wpp))) {
    bad <- cause_detail |>
      filter(is.na(mx_wpp)) |>
      distinct(year, sex) |>
      arrange(year, sex)
    stop("compute_q4030: missing WPP mx rows. First missing: ",
         bad$year[1], "/", bad$sex[1])
  }

  cause_detail <- cause_detail |>
    mutate(
      cause_frac = if_else(mx_wpp > 0, pmin(mx_cause / mx_wpp, 1), 0),
      dx_cause   = lx * (1 - px) * cause_frac
    )

  frac_sum_check <- cause_detail |>
    group_by(year, sex, age) |>
    summarise(frac_sum = sum(cause_frac), .groups = "drop")
  n_over <- sum(frac_sum_check$frac_sum > 1.001, na.rm = TRUE)
  if (n_over > 0) {
    max_over <- max(frac_sum_check$frac_sum, na.rm = TRUE)
    warning("compute_q4030: ", n_over, " cells where sum(cause fractions) > 1.0",
            " (max = ", round(max_over, 4), "). Competing-risks guard fired. ",
            "Check combined calibrated mx vs WPP envelope.")
  }

  q_cause <- cause_detail |>
    group_by(year, sex, cause = module) |>
    summarise(q4030 = sum(dx_cause), .groups = "drop")

  bind_rows(q_all, q_cause) |>
    mutate(q4030_pct = round(q4030 * 100, 4))
}


# Build the authoritative module-level target mortality from the demographic
# backbone and the projected GBD cause-fraction layer.
#
# The intended V1 identity is:
#   mx_cause(year, sex, age) = mx_wpp(year, sex, age) * frac_cause(year, sex, age)
#
# expand_grid() is deliberately used to fail loudly if any expected
# module x year x sex x age target is missing.
make_module_target_mx <- function(cause_frac_df, wpp_mx_df, pop_df,
                                  module_cause_map,
                                  years = NULL,
                                  allow_missing_zero = tibble::tibble(
                                    module = "cervical_ca",
                                    sex    = "Male"
                                  )) {
  module_cause_map <- unlist(module_cause_map, use.names = TRUE)

  map_tbl <- tibble::tibble(
    module = names(module_cause_map),
    cause  = as.character(module_cause_map)
  )

  years_use <- if (is.null(years)) {
    sort(unique(wpp_mx_df$year))
  } else {
    as.integer(years)
  }

  needed_cf <- c("cause", "year", "sex", "age", "frac")
  missing_cf_cols <- setdiff(needed_cf, names(cause_frac_df))
  if (length(missing_cf_cols) > 0) {
    stop(
      "make_module_target_mx(): cause_frac_df missing required columns: ",
      paste(missing_cf_cols, collapse = ", "),
      call. = FALSE
    )
  }

  cf <- cause_frac_df |>
    dplyr::filter(year %in% years_use) |>
    dplyr::mutate(
      cause = as.character(cause),
      sex   = as.character(sex),
      year  = as.integer(year),
      age   = as.integer(age),
      frac  = as.numeric(frac)
    ) |>
    dplyr::group_by(cause, year, sex, age) |>
    dplyr::summarise(
      frac = dplyr::first(frac),
      n = dplyr::n(),
      .groups = "drop"
    )

  dup_cf <- cf |>
    dplyr::filter(n > 1)

  if (nrow(dup_cf) > 0) {
    stop(
      "make_module_target_mx(): duplicate cause-fraction rows. First rows:\n",
      paste(utils::capture.output(print(utils::head(dup_cf, 10))), collapse = "\n"),
      call. = FALSE
    )
  }

  # GBD inputs do not contain every cause at every age. For example, IHD/HHD
  # often start at older age groups, so the 1-year cause-fraction file has no
  # rows for ages below the observed cause-sex support. Those cells are true
  # structural zeroes for this V1 WPP x cause-fraction anchor. We still stop if
  # a row is missing inside the observed age support, because that indicates a
  # broken interpolation or an incomplete input file.
  cf_support <- cf |>
    dplyr::filter(cause %in% map_tbl$cause) |>
    dplyr::group_by(cause, sex) |>
    dplyr::summarise(
      .min_age = min(age, na.rm = TRUE),
      .max_age = max(age, na.rm = TRUE),
      .n_age   = dplyr::n_distinct(age),
      .groups  = "drop"
    )

  wpp <- wpp_mx_df |>
    dplyr::filter(year %in% years_use) |>
    dplyr::mutate(year = as.integer(year), age = as.integer(age)) |>
    dplyr::select(year, sex, age, mx_wpp)

  pop <- pop_df |>
    dplyr::filter(year %in% years_use) |>
    dplyr::mutate(year = as.integer(year), age = as.integer(age)) |>
    dplyr::select(year, sex, age, pop)

  allow_tbl <- allow_missing_zero |>
    dplyr::mutate(.allow_missing_zero = TRUE)

  out <- tidyr::expand_grid(
    module = map_tbl$module,
    year   = years_use,
    sex    = SEXES,
    age    = AGES
  ) |>
    dplyr::left_join(map_tbl, by = "module") |>
    dplyr::left_join(
      cf |> dplyr::select(cause, year, sex, age, frac),
      by = c("cause", "year", "sex", "age")
    ) |>
    dplyr::left_join(cf_support, by = c("cause", "sex")) |>
    dplyr::left_join(allow_tbl, by = c("module", "sex")) |>
    dplyr::mutate(
      .allow_missing_zero = tidyr::replace_na(.allow_missing_zero, FALSE),
      .outside_age_support = !is.na(.min_age) &
        (age < .min_age | age > .max_age),
      .zero_missing_frac = is.na(frac) &
        (.allow_missing_zero | .outside_age_support)
    )

  missing_frac <- out |>
    dplyr::filter(is.na(frac), !.zero_missing_frac) |>
    dplyr::distinct(module, cause, year, sex, age, .min_age, .max_age)

  if (nrow(missing_frac) > 0) {
    stop(
      "make_module_target_mx(): missing cause-fraction rows inside active age support. ",
      "Rows outside observed cause-sex age support are zero-filled; these rows are not. First rows:\n",
      paste(utils::capture.output(print(utils::head(missing_frac, 20))), collapse = "\n"),
      call. = FALSE
    )
  }

  out <- out |>
    dplyr::mutate(
      frac = dplyr::if_else(.zero_missing_frac, 0, frac),
      frac = pmin(pmax(frac, 0), 1)
    ) |>
    dplyr::left_join(wpp, by = c("year", "sex", "age")) |>
    dplyr::left_join(pop, by = c("year", "sex", "age"))

  missing_wpp_pop <- out |>
    dplyr::filter(is.na(mx_wpp) | is.na(pop)) |>
    dplyr::distinct(module, year, sex, age, mx_wpp, pop)

  if (nrow(missing_wpp_pop) > 0) {
    stop(
      "make_module_target_mx(): missing WPP mx or population rows. First rows:\n",
      paste(utils::capture.output(print(utils::head(missing_wpp_pop, 20))), collapse = "\n"),
      call. = FALSE
    )
  }

  out |>
    dplyr::mutate(
      mx_target     = pmax(mx_wpp * frac, 0),
      deaths_target = mx_target * pop
    ) |>
    dplyr::select(
      module, cause, year, sex, age,
      frac, mx_wpp, pop, mx_target, deaths_target
    )
}


anchor_baseline_mx_to_targets <- function(mx_df, target_mx_df,
                                           modules_to_anchor) {
  key <- c("module", "year", "sex", "age")

  target_small <- target_mx_df |>
    dplyr::select(dplyr::all_of(key), pop, mx_target, deaths_target)

  joined <- mx_df |>
    dplyr::left_join(target_small, by = key)

  missing_target <- joined |>
    dplyr::filter(
      module %in% modules_to_anchor,
      is.na(mx_target) | is.na(deaths_target)
    ) |>
    dplyr::distinct(module, year, sex, age)

  if (nrow(missing_target) > 0) {
    stop(
      "anchor_baseline_mx_to_targets(): missing target rows. First rows:\n",
      paste(utils::capture.output(print(utils::head(missing_target, 20))), collapse = "\n"),
      call. = FALSE
    )
  }

  joined |>
    dplyr::mutate(
      mx_cause = dplyr::if_else(
        module %in% modules_to_anchor,
        mx_target,
        mx_cause
      ),
      deaths_cause = dplyr::if_else(
        module %in% modules_to_anchor,
        deaths_target,
        deaths_cause
      )
    ) |>
    dplyr::select(-pop, -mx_target, -deaths_target)
}


anchor_scenario_mx_to_targets <- function(mx_df, baseline_raw_mx_df,
                                           target_mx_df,
                                           modules_to_anchor,
                                           raw_floor = 1e-12) {
  if (!"scenario" %in% names(mx_df)) {
    stop("anchor_scenario_mx_to_targets(): mx_df must contain a scenario column.",
         call. = FALSE)
  }

  key <- c("module", "year", "sex", "age")

  baseline_raw <- baseline_raw_mx_df |>
    dplyr::select(dplyr::all_of(key), mx_raw_baseline = mx_cause)

  target_small <- target_mx_df |>
    dplyr::select(dplyr::all_of(key), pop, mx_target)

  joined <- mx_df |>
    dplyr::left_join(baseline_raw, by = key) |>
    dplyr::left_join(target_small, by = key)

  missing_target <- joined |>
    dplyr::filter(
      module %in% modules_to_anchor,
      is.na(mx_target) | is.na(pop)
    ) |>
    dplyr::distinct(scenario, module, year, sex, age)

  if (nrow(missing_target) > 0) {
    stop(
      "anchor_scenario_mx_to_targets(): missing target rows. First rows:\n",
      paste(utils::capture.output(print(utils::head(missing_target, 20))), collapse = "\n"),
      call. = FALSE
    )
  }

  missing_raw_baseline <- joined |>
    dplyr::filter(
      module %in% modules_to_anchor,
      scenario != "baseline",
      is.na(mx_raw_baseline)
    ) |>
    dplyr::distinct(scenario, module, year, sex, age)

  if (nrow(missing_raw_baseline) > 0) {
    stop(
      "anchor_scenario_mx_to_targets(): missing raw baseline comparator rows. First rows:\n",
      paste(utils::capture.output(print(utils::head(missing_raw_baseline, 20))), collapse = "\n"),
      call. = FALSE
    )
  }

  joined |>
    dplyr::mutate(
      rel_effect = dplyr::case_when(
        !(module %in% modules_to_anchor) ~ 1,
        scenario == "baseline" ~ 1,
        !is.na(mx_raw_baseline) & mx_raw_baseline > raw_floor ~ mx_cause / mx_raw_baseline,
        TRUE ~ 1
      ),
      rel_effect = dplyr::if_else(is.finite(rel_effect), rel_effect, 1),
      rel_effect = pmax(rel_effect, 0),
      mx_cause = dplyr::if_else(
        module %in% modules_to_anchor,
        mx_target * rel_effect,
        mx_cause
      ),
      deaths_cause = dplyr::if_else(
        module %in% modules_to_anchor,
        mx_cause * pop,
        deaths_cause
      )
    ) |>
    dplyr::select(-mx_raw_baseline, -pop, -mx_target, -rel_effect)
}


validate_baseline_anchor <- function(mx_df, target_mx_df, modules,
                                     tol_abs = 1e-12,
                                     tol_rel = 1e-8) {
  chk <- mx_df |>
    dplyr::filter(module %in% modules) |>
    dplyr::left_join(
      target_mx_df |>
        dplyr::select(module, year, sex, age, mx_target, deaths_target),
      by = c("module", "year", "sex", "age")
    ) |>
    dplyr::mutate(
      rel_err_mx = abs(mx_cause - mx_target) / pmax(mx_target, tol_abs)
    )

  bad <- chk |>
    dplyr::filter(is.na(mx_target) | rel_err_mx > tol_rel) |>
    dplyr::arrange(dplyr::desc(rel_err_mx))

  if (nrow(bad) > 0) {
    stop(
      "validate_baseline_anchor(): baseline no longer equals WPP x cause fraction. First rows:\n",
      paste(utils::capture.output(print(utils::head(bad, 20))), collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


summarise_anchor_diagnostics <- function(raw_mx_df, anchored_mx_df, target_mx_df) {
  raw <- raw_mx_df |>
    dplyr::left_join(
      target_mx_df |> dplyr::select(module, year, sex, age, pop),
      by = c("module", "year", "sex", "age")
    ) |>
    dplyr::mutate(deaths_raw = mx_cause * pop) |>
    dplyr::group_by(module, year) |>
    dplyr::summarise(
      deaths_raw = sum(deaths_raw, na.rm = TRUE),
      .groups = "drop"
    )

  anchored <- anchored_mx_df |>
    dplyr::group_by(module, year) |>
    dplyr::summarise(
      deaths_anchored = sum(deaths_cause, na.rm = TRUE),
      .groups = "drop"
    )

  target <- target_mx_df |>
    dplyr::group_by(module, year) |>
    dplyr::summarise(
      deaths_target = sum(deaths_target, na.rm = TRUE),
      .groups = "drop"
    )

  raw |>
    dplyr::full_join(anchored, by = c("module", "year")) |>
    dplyr::full_join(target, by = c("module", "year")) |>
    dplyr::mutate(
      raw_to_target = deaths_raw / pmax(deaths_target, 1e-12),
      anchored_to_target = deaths_anchored / pmax(deaths_target, 1e-12)
    ) |>
    dplyr::arrange(module, year)
}

# Compute deaths averted vs baseline.
# mx_df must have columns: scenario, year, sex, age, module, mx_cause.
# Returns deaths averted by scenario x year x module.
#
# averted_raw is the SIGNED difference (deaths_base - deaths_scenario).
#   Positive = deaths prevented. Negative = deaths increased (diagnostic signal).
# averted_display clips to >= 0 for presentation use only.
compute_deaths_averted <- function(mx_df, pop_df) {
  deaths <- mx_df |>
    dplyr::left_join(pop_df, by = c("year", "sex", "age"))

  missing_pop <- deaths |>
    dplyr::filter(is.na(pop)) |>
    dplyr::distinct(scenario, year, sex, age, module)

  if (nrow(missing_pop) > 0) {
    stop(
      "compute_deaths_averted(): missing population rows. First rows:\n",
      paste(utils::capture.output(print(utils::head(missing_pop, 20))), collapse = "\n"),
      call. = FALSE
    )
  }

  deaths <- deaths |>
    dplyr::mutate(deaths = mx_cause * pop)

  deaths_base <- deaths |>
    dplyr::filter(scenario == "baseline") |>
    dplyr::select(year, sex, age, module, deaths_base = deaths)

  joined <- deaths |>
    dplyr::filter(scenario != "baseline") |>
    dplyr::left_join(deaths_base, by = c("year", "sex", "age", "module"))

  missing_base <- joined |>
    dplyr::filter(is.na(deaths_base)) |>
    dplyr::distinct(scenario, year, sex, age, module)

  if (nrow(missing_base) > 0) {
    stop(
      "compute_deaths_averted(): missing baseline comparator rows. First rows:\n",
      paste(utils::capture.output(print(utils::head(missing_base, 20))), collapse = "\n"),
      call. = FALSE
    )
  }

  joined |>
    dplyr::mutate(
      averted_raw     = deaths_base - deaths,
      averted_display = pmax(averted_raw, 0)
    ) |>
    dplyr::group_by(scenario, year, module) |>
    dplyr::summarise(
      deaths          = sum(deaths),
      deaths_base     = sum(deaths_base),
      averted_raw     = sum(averted_raw),
      averted_display = sum(averted_display),
      .groups         = "drop"
    )
}

###############################################################################
# SECTION 7 — SCENARIO MODIFIER FUNCTIONS
###############################################################################

# Linear coverage scale-up from bsln to target over 5 (fast) or 10 (slow) years.
coverage_at_year <- function(year, bsln, target, start = 2025L, speed = "fast") {
  end_yr <- if (speed == "fast") start + 5L else start + 10L
  if (year < start)   return(bsln)
  if (year >= end_yr) return(target)
  bsln + (target - bsln) * (year - start) / (end_yr - start)
}

# Modify IHD TPM list for antihypertensive coverage increase.
# Reduces well→incident (incidence) and →dead_cause (case fatality) outflows;
# recomputes complement and diagonal after each modification.
apply_hypertension <- function(tpm_list, year, speed,
                               bsln_cov = 0.40, trgt_cov = 0.70,
                               cf_red = 0.24, ir_red = 0.17) {
  cov_t     <- coverage_at_year(year, bsln_cov, trgt_cov, speed = speed)
  delta_cov <- max(cov_t - bsln_cov, 0)
  eff_cf    <- 1 - cf_red * delta_cov
  eff_ir    <- 1 - ir_red * delta_cov

  imap(tpm_list, function(tpm, age_chr) {
    tpm2 <- tpm
    tpm2[1, 2] <- tpm2[1, 2] * eff_ir   # well → incident
    tpm2[2, 4] <- tpm2[2, 4] * eff_cf   # incident → dead_cause
    tpm2[3, 4] <- tpm2[3, 4] * eff_cf   # prevalent → dead_cause
    tpm2[2, 3] <- max(1 - tpm2[2, 4] - tpm2[2, 5], 0)  # incident → prevalent (complement)
    tpm2[1, 1] <- max(1 - tpm2[1, 2] - tpm2[1, 5], 0)  # well diagonal
    tpm2[3, 3] <- max(1 - tpm2[3, 4] - tpm2[3, 5], 0)  # prevalent diagonal
    validate_tpm(tpm2, label = paste0("Hypertension-modified IHD TPM age ", age_chr))
    tpm2
  })
}

# Modify cervical TPM list for screening coverage increase.
# Reduces distant (row 5) and regional (row 4) CFR outflows; recomputes
# diagonals. tpm[4,5] (progression regional→distant) is NOT touched.
apply_screening <- function(tpm_list, year, speed,
                            bsln_cov = 0.15, trgt_cov = 0.70,
                            local_shift = 0.20) {
  cov_t     <- coverage_at_year(year, bsln_cov, trgt_cov, speed = speed)
  delta_cov <- max(cov_t - bsln_cov, 0)

  imap(tpm_list, function(tpm, age_chr) {
    tpm2    <- tpm
    red_dst <- pmin(delta_cov * local_shift,       1)
    red_rgn <- pmin(delta_cov * local_shift * 0.5, 1)
    tpm2[5, 6] <- tpm2[5, 6] * (1 - red_dst)   # distant → dead_cause
    tpm2[4, 6] <- tpm2[4, 6] * (1 - red_rgn)   # regional → dead_cause
    tpm2[5, 5] <- max(1 - tpm2[5, 6] - tpm2[5, 7], 0)
    tpm2[4, 4] <- max(1 - tpm2[4, 5] - tpm2[4, 6] - tpm2[4, 7], 0)
    validate_tpm(tpm2, label = paste0("Screening-modified cervical TPM age ", age_chr))
    tpm2
  })
}

# Modify cervical TPM list for treatment adherence improvement.
# Reduces stage-specific CFRs; recomputes diagonals last.
apply_treatment_adherence <- function(tpm_list, year, speed,
                                      bsln_adh = 0.35, trgt_adh = 0.70,
                                      rr_lcl = 0.60, rr_rgn = 0.55,
                                      rr_dst = 0.70) {
  adh_t     <- coverage_at_year(year, bsln_adh, trgt_adh, speed = speed)
  delta_adh <- max(adh_t - bsln_adh, 0) / max(1 - bsln_adh, 0.01)

  imap(tpm_list, function(tpm, age_chr) {
    tpm2 <- tpm
    tpm2[3, 6] <- tpm2[3, 6] * (1 - rr_lcl * delta_adh)
    tpm2[4, 6] <- tpm2[4, 6] * (1 - rr_rgn * delta_adh)
    tpm2[5, 6] <- tpm2[5, 6] * (1 - rr_dst * delta_adh)
    tpm2[3, 3] <- max(1 - tpm2[3, 4] - tpm2[3, 6] - tpm2[3, 7], 0)
    tpm2[4, 4] <- max(1 - tpm2[4, 5] - tpm2[4, 6] - tpm2[4, 7], 0)
    tpm2[5, 5] <- max(1 - tpm2[5, 6] - tpm2[5, 7], 0)
    validate_tpm(tpm2, label = paste0("Adherence-modified cervical TPM age ", age_chr))
    tpm2
  })
}

###############################################################################
# SECTION 8 — YEAR-VARYING TPM + GENERIC CVD MODULE FACTORIES
###############################################################################

# Scale a 5-state CVD TPM calibrated at base year to projection year t.
# Cause-specific transitions scale by mx_cause_t / mx_cause_base.
# Background transitions scale by bgmx_t / bgmx_base.
# Diagonals and complement transitions recomputed last.
scale_cvd_tpm_to_year <- function(base_tpm_list,
                                  mx_cause_base, bgmx_base,
                                  mx_cause_t,    bgmx_t) {
  imap(base_tpm_list, function(tpm, age_chr) {
    a   <- as.integer(age_chr)
    idx <- a + 1L
    sc  <- if (mx_cause_base[idx] > 1e-10) mx_cause_t[idx] / mx_cause_base[idx] else 1.0
    sbg <- if (bgmx_base[idx]    > 1e-10) bgmx_t[idx]     / bgmx_base[idx]     else 1.0

    tpm2 <- tpm
    # States: well(1) incident(2) prevalent(3) dead_cause(4) dead_bg(5)
    tpm2[1, 2] <- tpm[1, 2] * sc;   tpm2[2, 4] <- tpm[2, 4] * sc
    tpm2[3, 4] <- tpm[3, 4] * sc;   tpm2[1, 5] <- tpm[1, 5] * sbg
    tpm2[2, 5] <- tpm[2, 5] * sbg;  tpm2[3, 5] <- tpm[3, 5] * sbg

    # Row 2: incident — normalise if total outflows exceed 1
    out2 <- tpm2[2, 4] + tpm2[2, 5]
    if (out2 > 1) { tpm2[2, 4] <- tpm2[2, 4] / out2; tpm2[2, 5] <- tpm2[2, 5] / out2 }
    tpm2[2, 3] <- max(1 - tpm2[2, 4] - tpm2[2, 5], 0)

    out1 <- tpm2[1, 2] + tpm2[1, 5]
    if (out1 > 1) { tpm2[1, 2] <- tpm2[1, 2] / out1; tpm2[1, 5] <- tpm2[1, 5] / out1 }
    tpm2[1, 1] <- max(1 - tpm2[1, 2] - tpm2[1, 5], 0)

    out3 <- tpm2[3, 4] + tpm2[3, 5]
    if (out3 > 1) { tpm2[3, 4] <- tpm2[3, 4] / out3; tpm2[3, 5] <- tpm2[3, 5] / out3 }
    tpm2[3, 3] <- max(1 - tpm2[3, 4] - tpm2[3, 5], 0)

    validate_tpm(tpm2, label = paste0("Year-scaled CVD TPM age ", age_chr))
    tpm2
  })
}

# Apply combined eff_ir / eff_cf vectors to a 5-state CVD TPM list.
# eff_ir[age+1] multiplies well→incident. eff_cf[age+1] multiplies CFR exits.
apply_cvd_effects <- function(tpm_list, eff_ir, eff_cf) {
  imap(tpm_list, function(tpm, age_chr) {
    a    <- as.integer(age_chr) + 1L
    eir  <- eff_ir[a]
    ecf  <- eff_cf[a]
    tpm2 <- tpm

    # Rows = FROM states: 1 well, 2 incident, 3 prevalent, 4 dead_cause, 5 dead_bg.
    # In the V1 IR-CFR schedule, row 1 can contain a direct acute fatal-event
    # transition (well -> dead_cause). Treat total incident events from well as
    # p(well->incident survivor) + p(well->dead_cause acute fatality). IR effects
    # reduce total incident events; CF effects redistribute fatal events toward
    # non-fatal incident survivors.
    p_event0 <- tpm[1, 2] + tpm[1, 4]
    p_dead0  <- tpm[1, 4]
    p_event1 <- pmax(p_event0 * eir, 0)
    p_dead1  <- pmax(p_dead0  * eir * ecf, 0)
    p_inc1   <- pmax(p_event1 - p_dead1, 0)

    # Existing incident/prevalent fatality exits are CF-modified.
    tpm2[2, 4] <- tpm2[2, 4] * ecf
    tpm2[3, 4] <- tpm2[3, 4] * ecf

    # Rebuild row 1 with both non-fatal and fatal incident-event components.
    tpm2[1, 2] <- p_inc1
    tpm2[1, 4] <- p_dead1
    out1 <- tpm2[1, 2] + tpm2[1, 4] + tpm2[1, 5]
    if (out1 > 1) {
      tpm2[1, 2] <- tpm2[1, 2] / out1
      tpm2[1, 4] <- tpm2[1, 4] / out1
      tpm2[1, 5] <- tpm2[1, 5] / out1
    }
    tpm2[1, 1] <- max(1 - tpm2[1, 2] - tpm2[1, 4] - tpm2[1, 5], 0)

    out2 <- tpm2[2, 4] + tpm2[2, 5]
    if (out2 > 1) {
      tpm2[2, 4] <- tpm2[2, 4] / out2
      tpm2[2, 5] <- tpm2[2, 5] / out2
    }
    tpm2[2, 3] <- max(1 - tpm2[2, 4] - tpm2[2, 5], 0)

    out3 <- tpm2[3, 4] + tpm2[3, 5]
    if (out3 > 1) {
      tpm2[3, 4] <- tpm2[3, 4] / out3
      tpm2[3, 5] <- tpm2[3, 5] / out3
    }
    tpm2[3, 3] <- max(1 - tpm2[3, 4] - tpm2[3, 5], 0)

    validate_tpm(tpm2, label = paste0("Intervention CVD TPM age ", age_chr))
    tpm2
  })
}

# Internal: true if scenario has any CVD intervention active.
.has_cvd_intervention <- function(scenario) {
  any(c(scenario$bp_on, scenario$statins_on, scenario$sodium_on,
        scenario$tfa_on, scenario$diabetes_bp_on), na.rm = TRUE)
}

# Runner for direct-mortality modules (e.g. HHD).
# Bypasses all TPM/state machinery.
# Module must have: $id, $type = "direct_mortality", $mx_fun, $eligible_fun.
run_direct_mortality_module <- function(module_spec,
                                        pop_array,
                                        proj_years   = NULL,
                                        warmup_cycles = NULL,
                                        scenario     = list(),
                                        store_states = FALSE) {
  yrs_all <- as.integer(dimnames(pop_array)[[1]])
  if (length(yrs_all) < 2) stop("pop_array must contain at least two years.")

  if (is.null(proj_years))   proj_years   <- yrs_all[-length(yrs_all)]
  if (is.null(warmup_cycles)) warmup_cycles <- integer(0)
  years_store <- intersect(proj_years, yrs_all[-length(yrs_all)])
  if (length(years_store) == 0)
    stop("No projection years overlap valid start-of-interval years in pop_array.")

  mx_rows <- list()

  for (sx in SEXES) {
    s_idx <- match(sx, SEXES)
    for (ti in seq_len(length(yrs_all) - 1L)) {
      yr <- yrs_all[ti]
      if (!yr %in% years_store) next

      mx_cause <- module_spec$mx_fun(year = yr, sex = sx,
                                      ages = AGES, scenario = scenario)
      if (length(mx_cause) != length(AGES))
        stop("Module '", module_spec$id, "' mx_fun returned wrong length.")
      mx_cause <- pmax(mx_cause, 0)

      mx_rows[[length(mx_rows) + 1L]] <- tibble(
        module       = module_spec$id,
        year         = yr,
        sex          = sx,
        age          = AGES,
        mx_cause     = mx_cause,
        deaths_cause = mx_cause * pop_array[ti, s_idx, ]
      )
    }
  }

  list(mx = bind_rows(mx_rows), states = NULL)
}


# Build a CVD TPM list from a compact baseline schedule table.
# Schedule rows must contain module, year, sex, age and the six transition
# probabilities stored by derive_cvd_tpm_from_targets().
build_cvd_tpm_from_schedule <- function(schedule_df, ages = AGES) {
  needed <- c("age", "p_well_inc", "p_well_bgmx", "p_inc_dead",
              "p_inc_bgmx", "p_prev_dead", "p_prev_bgmx")
  miss <- setdiff(needed, names(schedule_df))
  if (length(miss) > 0) {
    stop("build_cvd_tpm_from_schedule(): missing columns: ",
         paste(miss, collapse = ", "), call. = FALSE)
  }
  if (!"p_well_dead" %in% names(schedule_df)) {
    schedule_df$p_well_dead <- 0
  }

  out <- lapply(ages, function(a) {
    row <- schedule_df[schedule_df$age == a, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop("build_cvd_tpm_from_schedule(): expected one row for age ", a,
           ", got ", nrow(row), call. = FALSE)
    }

    states <- c("well", "incident", "prevalent", "dead_cause", "dead_bg")
    tpm <- matrix(0, nrow = 5, ncol = 5, dimnames = list(states, states))

    p_well_inc  <- as.numeric(row$p_well_inc)
    p_well_dead <- as.numeric(row$p_well_dead)
    p_well_bgmx <- as.numeric(row$p_well_bgmx)
    p_inc_dead  <- as.numeric(row$p_inc_dead)
    p_inc_bgmx  <- as.numeric(row$p_inc_bgmx)
    p_prev_dead <- as.numeric(row$p_prev_dead)
    p_prev_bgmx <- as.numeric(row$p_prev_bgmx)

    tpm[1, 2] <- p_well_inc
    tpm[1, 4] <- p_well_dead
    tpm[1, 5] <- p_well_bgmx
    tpm[1, 1] <- pmax(1 - tpm[1, 2] - tpm[1, 4] - tpm[1, 5], 0)

    tpm[2, 4] <- p_inc_dead
    tpm[2, 5] <- p_inc_bgmx
    tpm[2, 3] <- pmax(1 - tpm[2, 4] - tpm[2, 5], 0)

    tpm[3, 4] <- p_prev_dead
    tpm[3, 5] <- p_prev_bgmx
    tpm[3, 3] <- pmax(1 - p_prev_dead - p_prev_bgmx, 0)

    tpm[4, 4] <- 1
    tpm[5, 5] <- 1

    validate_tpm(tpm, label = paste0("scheduled CVD TPM age ", a))
    tpm
  })
  names(out) <- as.character(ages)
  out
}

# Derive an annual CVD TPM from two targets:
#   1. projected incidence rate inc_target_t
#   2. hard mortality target mx_target_t = WPP mx x projected cause fraction
# The incident-death probability starts from the calibrated acute-CFR prior.
# The prevalent death probability is solved residually from the current state
# counts so deaths match the target. This makes the implied CFR visible and
# auditable rather than hidden inside a mortality-scaling multiplier.
derive_cvd_tpm_from_targets <- function(module_id, year, sex, ages,
                                        base_tpm_list,
                                        counts,
                                        pop_exposure,
                                        inc_target_t,
                                        mx_target_t,
                                        bgmx_target_t,
                                        acute_cfr_cap = 0.95,
                                        prevalent_cfr_cap = 0.95,
                                        tiny = 1e-12) {
  if (is.null(counts) || is.null(pop_exposure)) {
    stop("derive_cvd_tpm_from_targets(): counts and pop_exposure are required ",
         "for annual residual CFR derivation.", call. = FALSE)
  }
  if (!is.matrix(counts) || ncol(counts) < 5) {
    stop("derive_cvd_tpm_from_targets(): counts must be an age x state matrix.",
         call. = FALSE)
  }

  rows <- lapply(ages, function(a) {
    idx <- a + 1L
    base_tpm <- base_tpm_list[[as.character(a)]]
    if (is.null(base_tpm)) {
      stop("Missing base TPM for ", module_id, "/", sex, "/age ", a,
           call. = FALSE)
    }

    n_well <- pmax(counts[idx, 1], 0)
    n_inc  <- pmax(counts[idx, 2], 0)
    n_prev <- pmax(counts[idx, 3], 0)
    pop    <- pmax(pop_exposure[idx], tiny)

    inc_target <- pmax(inc_target_t[idx], 0)
    mx_target  <- pmax(mx_target_t[idx], 0)
    bgmx       <- pmax(bgmx_target_t[idx], 0)

    deaths_target <- mx_target * pop

    # Interpret projected IR as the total annual incident-event hazard. Some
    # fraction of those events can be fatal within the same model year. Without
    # this same-year fatal-event path, young ages and first projection years with
    # little prevalent stock can require impossible prevalent CFRs even when the
    # mortality/incidence pair is otherwise plausible.
    p_event_raw <- pmin(1 - exp(-inc_target), 0.999)

    # Background mortality competes with incident events in the well row. Keep
    # total non-background event probability feasible; if incidence is too small
    # to support the WPP x cause-fraction mortality target even at the CFR caps,
    # we apply an explicit compatibility floor to incidence and flag it.
    row_event_cap <- pmax(0, 0.999 - pmin(bgmx, 0.999))
    p_event_raw <- pmin(p_event_raw, row_event_cap)

    p_inc_dead_prior <- pmin(pmax(base_tpm[2, 4], 0), acute_cfr_cap)
    p_prev_bgmx <- pmin(bgmx, 0.95)
    prev_cap_effective <- pmin(prevalent_cfr_cap, pmax(1 - p_prev_bgmx, 0))

    # Capacity check under maximum acute and prevalent CFRs. If the projected
    # incidence target plus current disease stock cannot produce target deaths,
    # increase the incident-event probability just enough to make the target
    # feasible. This is not silent: flags are written to diagnostics.
    inc_mass_raw <- n_inc + n_well * p_event_raw
    max_deaths_raw <- inc_mass_raw * acute_cfr_cap + n_prev * prev_cap_effective

    required_event_for_capacity <- if (deaths_target > max_deaths_raw + tiny && n_well > tiny) {
      (deaths_target - n_inc * acute_cfr_cap - n_prev * prev_cap_effective) /
        pmax(n_well * acute_cfr_cap, tiny)
    } else {
      p_event_raw
    }

    p_event <- pmin(pmax(p_event_raw, required_event_for_capacity, 0), row_event_cap)
    flag_incidence_floor_applied <- p_event > p_event_raw + 1e-12

    inc_mass <- n_inc + n_well * p_event
    max_deaths_after_floor <- inc_mass * acute_cfr_cap + n_prev * prev_cap_effective
    flag_capacity_short <- deaths_target > max_deaths_after_floor + pmax(1e-8, 1e-8 * deaths_target)

    # Choose acute CFR. Use the calibrated prior where possible. If the prior
    # plus maximum prevalent CFR cannot match target deaths, raise acute CFR up
    # to the cap and flag it. If the prior alone exceeds target deaths, lower it
    # and flag the negative residual condition.
    deaths_at_prior <- inc_mass * p_inc_dead_prior
    flag_negative_residual <- deaths_at_prior > deaths_target + tiny

    p_inc_dead <- p_inc_dead_prior
    flag_acute_cfr_raised <- FALSE

    if (flag_negative_residual && inc_mass > tiny) {
      p_inc_dead <- pmin(p_inc_dead_prior, deaths_target / inc_mass, acute_cfr_cap)
    } else {
      residual_at_prior <- pmax(deaths_target - deaths_at_prior, 0)
      req_prev_at_prior <- if (n_prev > tiny) residual_at_prior / n_prev else if (residual_at_prior > tiny) Inf else 0

      if (req_prev_at_prior > prev_cap_effective + 1e-12 && inc_mass > tiny) {
        needed_inc_dead <- (deaths_target - n_prev * prev_cap_effective) / inc_mass
        p_inc_dead <- pmin(pmax(p_inc_dead_prior, needed_inc_dead), acute_cfr_cap)
        flag_acute_cfr_raised <- p_inc_dead > p_inc_dead_prior + 1e-12
      }
    }

    p_inc_dead <- pmin(pmax(p_inc_dead, 0), acute_cfr_cap)

    # Split well-row incident events into non-fatal incident survivors and
    # same-year fatal acute events. The same p_inc_dead applies to existing
    # incident-state counts for continuity with the V1 CVD state structure.
    p_well_dead <- pmin(p_event * p_inc_dead, row_event_cap)
    p_well_inc  <- pmax(p_event - p_well_dead, 0)
    p_well_bgmx <- pmin(bgmx, pmax(0, 0.999 - p_well_inc - p_well_dead))

    p_inc_bgmx <- pmin(bgmx, pmax(0, 0.999 - p_inc_dead))

    deaths_from_well_acute <- n_well * p_well_dead
    deaths_from_incident   <- n_inc  * p_inc_dead

    # Residual deaths to be allocated to the prevalent state. Because the
    # compatibility floor and same-year acute path often match deaths_target up
    # to floating-point tolerance, do not turn tiny residuals into Inf CFRs when
    # n_prevalent is zero or near zero. This matters especially at young ages and
    # age-support boundaries, where GBD/WPP mortality targets can be positive
    # before a prevalent stock has accumulated in the Markov states.
    death_match_tol <- pmax(1e-8, 1e-6 * pmax(deaths_target, 1))
    deaths_remaining_raw <- deaths_target - deaths_from_well_acute - deaths_from_incident
    deaths_remaining <- dplyr::if_else(
      deaths_remaining_raw > death_match_tol,
      deaths_remaining_raw,
      0
    )

    required_prev_cfr <- if (n_prev > tiny) {
      deaths_remaining / n_prev
    } else if (deaths_remaining > death_match_tol) {
      Inf
    } else {
      0
    }

    flag_required_prev_cfr_gt_cap <- deaths_remaining > death_match_tol &
      required_prev_cfr > prev_cap_effective + 1e-10
    p_prev_dead <- pmin(pmax(required_prev_cfr, 0), prev_cap_effective)
    p_prev_bgmx <- pmin(p_prev_bgmx, pmax(0, 0.999 - p_prev_dead))

    deaths_model <- n_well * p_well_dead + n_inc * p_inc_dead + n_prev * p_prev_dead
    death_abs_error <- deaths_model - deaths_target
    death_rel_error <- death_abs_error / pmax(deaths_target, 1)
    flag_death_target_miss <- abs(death_abs_error) > death_match_tol

    tibble::tibble(
      module = module_id,
      year = as.integer(year),
      sex = sex,
      age = as.integer(a),
      pop = pop,
      n_well = n_well,
      n_incident = n_inc,
      n_prevalent = n_prev,
      inc_target = inc_target,
      mx_target = mx_target,
      bgmx_target = bgmx,
      p_event_raw = p_event_raw,
      p_event = p_event,
      p_well_inc = p_well_inc,
      p_well_dead = p_well_dead,
      p_well_bgmx = p_well_bgmx,
      p_inc_dead_prior = p_inc_dead_prior,
      p_inc_dead = p_inc_dead,
      p_inc_bgmx = p_inc_bgmx,
      p_prev_dead_required = required_prev_cfr,
      p_prev_dead = p_prev_dead,
      p_prev_bgmx = p_prev_bgmx,
      deaths_target = deaths_target,
      deaths_from_well_acute = deaths_from_well_acute,
      deaths_from_incident = deaths_from_incident,
      deaths_remaining_raw = deaths_remaining_raw,
      deaths_remaining = deaths_remaining,
      death_match_tol = death_match_tol,
      deaths_model = deaths_model,
      death_abs_error = death_abs_error,
      death_rel_error = death_rel_error,
      row_event_cap = row_event_cap,
      max_deaths_raw = max_deaths_raw,
      max_deaths_after_floor = max_deaths_after_floor,
      flag_negative_residual = flag_negative_residual,
      flag_incidence_floor_applied = flag_incidence_floor_applied,
      flag_acute_cfr_raised = flag_acute_cfr_raised,
      flag_capacity_short = flag_capacity_short,
      flag_required_prev_cfr_gt_cap = flag_required_prev_cfr_gt_cap,
      flag_death_target_miss = flag_death_target_miss
    )
  })

  diag <- dplyr::bind_rows(rows)
  tpm_list <- build_cvd_tpm_from_schedule(diag, ages = ages)
  list(tpm_list = tpm_list, schedule = diag, diagnostics = diag)
}

# Validate the annual CVD residual-CFR schedule. This is intentionally strict:
# if projected incidence and the WPP x fraction mortality target require an
# impossible prevalent CFR, the model should stop rather than silently cap.
validate_cvd_annual_tpm_diagnostics <- function(diag,
                                                rel_tol = 1e-6,
                                                abs_tol = 1e-8,
                                                stop_on_negative_residual = FALSE,
                                                stop_on_incidence_floor = FALSE,
                                                stop_on_acute_cfr_raised = FALSE) {
  if (is.null(diag) || nrow(diag) == 0) {
    stop("validate_cvd_annual_tpm_diagnostics(): empty diagnostics.", call. = FALSE)
  }

  bad_target <- diag |>
    dplyr::filter(flag_death_target_miss | abs(death_abs_error) > pmax(abs_tol, rel_tol * pmax(deaths_target, 1)))

  bad_capacity <- diag |>
    dplyr::filter(flag_capacity_short | flag_required_prev_cfr_gt_cap |
                    (!is.finite(p_prev_dead_required) & flag_death_target_miss))

  bad_prob <- diag |>
    dplyr::filter(
      p_well_inc < -1e-10 | p_well_inc > 1 + 1e-10 |
        p_well_dead < -1e-10 | p_well_dead > 1 + 1e-10 |
        p_inc_dead < -1e-10 | p_inc_dead > 1 + 1e-10 |
        p_prev_dead < -1e-10 | p_prev_dead > 1 + 1e-10 |
        p_well_inc + p_well_dead + p_well_bgmx > 1 + 1e-8
    )

  if (nrow(bad_prob) > 0) {
    stop("Annual CVD TPM diagnostics contain invalid probabilities. First rows:\n",
         paste(utils::capture.output(print(utils::head(bad_prob, 20))), collapse = "\n"),
         call. = FALSE)
  }

  if (nrow(bad_capacity) > 0) {
    stop("Projected incidence and mortality target remain infeasible after same-year acute deaths and incidence compatibility floor. First rows:\n",
         paste(utils::capture.output(print(utils::head(bad_capacity, 20))), collapse = "\n"),
         call. = FALSE)
  }

  if (nrow(bad_target) > 0) {
    stop("Annual CVD TPM schedule fails to match mortality target. First rows:\n",
         paste(utils::capture.output(print(utils::head(bad_target, 20))), collapse = "\n"),
         call. = FALSE)
  }

  neg <- diag |> dplyr::filter(flag_negative_residual)
  if (nrow(neg) > 0) {
    msg <- paste0(
      "Annual CVD TPM diagnostics: ", nrow(neg),
      " cells where the acute-event prior exceeded target deaths; acute CFR was reduced and flagged."
    )
    if (isTRUE(stop_on_negative_residual)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  floor_rows <- diag |> dplyr::filter(flag_incidence_floor_applied)
  if (nrow(floor_rows) > 0) {
    msg <- paste0(
      "Annual CVD TPM diagnostics: ", nrow(floor_rows),
      " cells required an incidence compatibility floor so projected IR could support the WPP x fraction mortality target. See baseline_cvd_tpm_diagnostics.csv."
    )
    if (isTRUE(stop_on_incidence_floor)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  raised <- diag |> dplyr::filter(flag_acute_cfr_raised)
  if (nrow(raised) > 0) {
    msg <- paste0(
      "Annual CVD TPM diagnostics: ", nrow(raised),
      " cells required acute CFR above the calibrated prior, within cap, to match target mortality. See baseline_cvd_tpm_diagnostics.csv."
    )
    if (isTRUE(stop_on_acute_cfr_raised)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  invisible(TRUE)
}

# Factory: generic 5-state CVD module with year-varying TPM scaling.
#
# module_id       "ihd" | "ischemic_stroke" | "ich"
# gbd_cause_name  GBD cause label for gbd_full_df lookups
# cvd_tpm         list(Female = tpm_list, Male = tpm_list)  calibrated TPMs
# gbd_full_df     GBD measures data.frame at calibration year
# cause_frac_df   projected cause fractions: year, sex, age, cause, frac
# wpp_mx_df       WPP mortality rates: year, sex, age, mx_wpp
# base_mx_cause   list(Female = numeric[101], Male = numeric[101])
# base_bgmx       list(Female = numeric[101], Male = numeric[101])

make_cvd_module <- function(module_id, gbd_cause_name,
                            cvd_tpm, gbd_full_df,
                            cause_frac_df, wpp_mx_df,
                            base_mx_cause, base_bgmx,
                            incidence_rate_df = NULL,
                            baseline_tpm_schedule = NULL) {

  states <- c("well", "incident", "prevalent", "dead_cause", "dead_bg")

  cf_sub  <- cause_frac_df[cause_frac_df$cause == gbd_cause_name, ]
  cf_idx  <- split(cf_sub,  paste(cf_sub$year,    cf_sub$sex,    sep = "_"))
  wpp_idx <- split(wpp_mx_df, paste(wpp_mx_df$year, wpp_mx_df$sex, sep = "_"))

  inc_idx <- NULL
  if (!is.null(incidence_rate_df)) {
    inc_sub <- incidence_rate_df |>
      dplyr::filter(.data$module == module_id | .data$cause == gbd_cause_name) |>
      dplyr::mutate(year = as.integer(year), age = as.integer(age))
    if (nrow(inc_sub) > 0) {
      inc_idx <- split(inc_sub, paste(inc_sub$year, inc_sub$sex, sep = "_"))
    }
  }

  schedule_idx <- NULL
  if (!is.null(baseline_tpm_schedule)) {
    schedule_sub <- baseline_tpm_schedule |>
      dplyr::filter(.data$module == module_id) |>
      dplyr::mutate(year = as.integer(year), age = as.integer(age))
    if (nrow(schedule_sub) == 0) {
      stop("baseline_tpm_schedule has no rows for module '", module_id, "'.",
           call. = FALSE)
    }
    schedule_idx <- split(schedule_sub, paste(schedule_sub$year, schedule_sub$sex, sep = "_"))
  }

  schedule_env <- new.env(parent = emptyenv())
  schedule_env$schedule_rows <- list()
  schedule_env$diag_rows     <- list()

  list(
    id               = module_id,
    states           = states,
    dead_cause_state = "dead_cause",
    dead_bg_state    = "dead_bg",

    init_prob_fun = function(sex, ages) {
      prev_5yr <- gbd_full_df |>
        filter(sex == !!sex, cause == gbd_cause_name, measure == "Prevalence") |>
        select(age_mid, val = rate_per100k) |> mutate(val = val / 1e5)
      inc_5yr <- gbd_full_df |>
        filter(sex == !!sex, cause == gbd_cause_name, measure == "Incidence") |>
        select(age_mid, val = rate_per100k) |> mutate(val = val / 1e5)

      if (nrow(inc_5yr) == 0) {
        ACUTE_CFR_HHD <- 0.05
        inc_5yr <- gbd_full_df |>
          filter(sex == !!sex, cause == gbd_cause_name, measure == "Deaths") |>
          select(age_mid, val = rate_per100k) |>
          mutate(val = val / 1e5 / ACUTE_CFR_HHD)
      }

      prev_1yr <- interp_piecewise_constant(prev_5yr$age_mid, prev_5yr$val, ages)
      inc_1yr  <- interp_piecewise_constant(inc_5yr$age_mid,  inc_5yr$val,  ages)

      out <- t(sapply(ages, function(a) {
        i      <- a + 1L
        p_prev <- min(prev_1yr[i], 0.999)
        p_inc  <- min((1 - p_prev) * (1 - exp(-pmax(inc_1yr[i], 0))), 0.20)
        c(well       = max(1 - p_prev - p_inc, 0),
          incident   = p_inc,
          prevalent  = p_prev,
          dead_cause = 0,
          dead_bg    = 0)
      }))
      validate_state_probs(out, label = paste0(module_id, " init / ", sex))
      out
    },

    tpm_fun = function(year, sex, ages, scenario,
                       counts = NULL, pop_exposure = NULL) {
      key <- paste(year, sex, sep = "_")

      # Scenario and production baseline runs use the fixed baseline schedule.
      # This prevents scenario runs from re-solving CFR back to the baseline
      # mortality target after interventions have changed the state distribution.
      if (!is.null(schedule_idx)) {
        sched <- schedule_idx[[key]]
        if (is.null(sched) || nrow(sched) == 0) {
          stop("No fixed baseline TPM schedule for ", module_id, "/", sex, "/", year,
               call. = FALSE)
        }
        tl <- build_cvd_tpm_from_schedule(sched, ages = ages)
      } else if (isTRUE(scenario$derive_baseline_cfr_schedule)) {
        # Baseline derivation pass only: use projected IR plus WPP x fraction
        # mortality target to solve annual CFR from current state counts.
        cf_row <- cf_idx[[key]]
        wp_row <- wpp_idx[[key]]
        inc_row <- if (!is.null(inc_idx)) inc_idx[[key]] else NULL

        if (is.null(cf_row) || nrow(cf_row) == 0)
          stop("No cause-fraction rows for ", module_id, "/", sex, "/", year)
        if (is.null(wp_row) || nrow(wp_row) == 0)
          stop("No WPP mx rows for ", module_id, "/", sex, "/", year)
        if (is.null(inc_row) || nrow(inc_row) == 0)
          stop("No projected incidence rows for ", module_id, "/", sex, "/", year,
               ". Re-run scripts/03_build_cause_fractions.R to create gbd_incidence_annual_1yr.csv.",
               call. = FALSE)

        mx_wpp_t <- numeric(101); frac_t <- numeric(101); inc_t <- numeric(101)
        for (r in seq_len(nrow(wp_row)))  mx_wpp_t[wp_row$age[r] + 1L] <- wp_row$mx_wpp[r]
        for (r in seq_len(nrow(cf_row)))  frac_t  [cf_row$age[r] + 1L] <- cf_row$frac[r]
        for (r in seq_len(nrow(inc_row))) inc_t   [inc_row$age[r] + 1L] <- inc_row$inc_rate[r]

        mx_c_t  <- pmax(mx_wpp_t * frac_t, 0)
        bgmx_t  <- pmax(mx_wpp_t - mx_c_t, 0)

        derived <- derive_cvd_tpm_from_targets(
          module_id     = module_id,
          year          = year,
          sex           = sex,
          ages          = ages,
          base_tpm_list = cvd_tpm[[sex]],
          counts        = counts,
          pop_exposure  = pop_exposure,
          inc_target_t  = inc_t,
          mx_target_t   = mx_c_t,
          bgmx_target_t = bgmx_t
        )

        schedule_env$schedule_rows[[length(schedule_env$schedule_rows) + 1L]] <- derived$schedule
        schedule_env$diag_rows[[length(schedule_env$diag_rows) + 1L]] <- derived$diagnostics
        tl <- derived$tpm_list
      } else {
        # Legacy fallback: mortality-ratio scaling. Kept for tests and for
        # users who have not yet generated incidence targets. Primary V1 uses
        # derive_baseline_cfr_schedule in 05 and fixed schedules in 06.
        tl <- cvd_tpm[[sex]]
        cf_row <- cf_idx[[key]]
        wp_row <- wpp_idx[[key]]
        if (!is.null(cf_row) && nrow(cf_row) > 0 &&
            !is.null(wp_row) && nrow(wp_row) > 0) {
          mx_wpp_t <- numeric(101); frac_t <- numeric(101)
          for (r in seq_len(nrow(wp_row))) mx_wpp_t[wp_row$age[r] + 1L] <- wp_row$mx_wpp[r]
          for (r in seq_len(nrow(cf_row))) frac_t  [cf_row$age[r] + 1L] <- cf_row$frac[r]
          mx_c_t <- pmax(mx_wpp_t * frac_t, 0)
          bgmx_t <- pmax(mx_wpp_t - mx_c_t, 0)
          tl <- scale_cvd_tpm_to_year(tl, base_mx_cause[[sex]], base_bgmx[[sex]],
                                      mx_c_t, bgmx_t)
        }
      }

      if (exists("apply_cvd_interventions", mode = "function") &&
          .has_cvd_intervention(scenario)) {
        eff <- apply_cvd_interventions(year, sex, ages, module_id, scenario)
        tl  <- apply_cvd_effects(tl, eff$eff_ir, eff$eff_cf)
      }
      tl
    },

    eligible_fun = function(sex, ages, year) rep(TRUE, length(ages)),

    get_baseline_tpm_schedule = function() {
      if (length(schedule_env$schedule_rows) == 0) return(tibble::tibble())
      dplyr::bind_rows(schedule_env$schedule_rows) |>
        dplyr::select(module, year, sex, age,
                      p_event_raw, p_event,
                      p_well_inc, p_well_dead, p_well_bgmx,
                      p_inc_dead, p_inc_bgmx,
                      p_prev_dead, p_prev_bgmx,
                      inc_target, mx_target, bgmx_target,
                      p_inc_dead_prior, p_prev_dead_required,
                      flag_incidence_floor_applied, flag_acute_cfr_raised) |>
        dplyr::distinct()
    },

    get_baseline_tpm_diagnostics = function() {
      if (length(schedule_env$diag_rows) == 0) return(tibble::tibble())
      dplyr::bind_rows(schedule_env$diag_rows) |>
        dplyr::distinct()
    }
  )
}

# Factory: direct-mortality CVD module for causes without incident states (HHD).
# mx_cause(t) = mx_WPP(t) × frac_cause(t). Intervention applies multiplicative
# combined eff_ir × eff_cf to mx_cause.
make_direct_cvd_mortality_module <- function(module_id, gbd_cause_name,
                                             cause_frac_df, wpp_mx_df) {
  cf_sub  <- cause_frac_df[cause_frac_df$cause == gbd_cause_name, ]
  cf_idx  <- split(cf_sub,   paste(cf_sub$year,    cf_sub$sex,    sep = "_"))
  wpp_idx <- split(wpp_mx_df, paste(wpp_mx_df$year, wpp_mx_df$sex, sep = "_"))

  list(
    id   = module_id,
    type = "direct_mortality",

    mx_fun = function(year, sex, ages, scenario) {
      key    <- paste(year, sex, sep = "_")
      cf_row <- cf_idx[[key]]
      wp_row <- wpp_idx[[key]]

      if (is.null(cf_row) || nrow(cf_row) == 0)
        stop("No cause-fraction rows for ", module_id, "/", sex, "/", year)
      if (is.null(wp_row) || nrow(wp_row) == 0)
        stop("No WPP mx rows for ", module_id, "/", sex, "/", year)

      mx_wpp_t <- numeric(101); frac_t <- numeric(101)
      for (r in seq_len(nrow(wp_row))) mx_wpp_t[wp_row$age[r] + 1L] <- wp_row$mx_wpp[r]
      for (r in seq_len(nrow(cf_row))) frac_t  [cf_row$age[r] + 1L] <- cf_row$frac[r]

      mx_cause <- pmax(mx_wpp_t * frac_t, 0)

      if (exists("apply_cvd_interventions", mode = "function") &&
          .has_cvd_intervention(scenario)) {
        eff  <- apply_cvd_interventions(year, sex, ages, module_id, scenario)
        # HHD is a direct-mortality V1 module. In the primary scenario,
        # HHD SBP incidence RR is set to 1, so eff_cf is the operative BP
        # pathway and acts directly on mx_cause. In the HHD proxy sensitivity,
        # eff_ir can also be non-trivial; the product is interpreted as a
        # direct mortality multiplier, not a separated IR x CFR decomposition.
        mult <- pmax(eff$eff_ir * eff$eff_cf, 0.001)
        mx_cause[ages + 1L] <- mx_cause[ages + 1L] * mult
      }

      mx_cause[ages + 1L]
    },

    eligible_fun = function(sex, ages, year) rep(TRUE, length(ages))
  )
}

# Factory: 7-state cervical cancer module.
# Male eligibility is forced to FALSE: init_prob_fun returns all-well,
# tpm_fun returns identity TPMs. assert_zero_male_cervical() verifies this.
make_cervical_module <- function(cervical_tpm, gbd_full_df,
                                 stage_dist = list(local   = 0.22,
                                                   regional = 0.40,
                                                   distant  = 0.38),
                                 precancer_multiplier = 3) {
  states <- c("well", "precancer", "local", "regional", "distant",
              "dead_cause", "dead_bg")

  list(
    id               = "cervical_ca",
    states           = states,
    dead_cause_state = "dead_cause",
    dead_bg_state    = "dead_bg",

    init_prob_fun = function(sex, ages) {
      if (sex != "Female") {
        out <- matrix(0, nrow = length(ages), ncol = length(states))
        colnames(out) <- states; out[, 1] <- 1; return(out)
      }
      prev_5yr <- gbd_full_df |>
        filter(sex == "Female", cause == "Cervical cancer", measure == "Prevalence") |>
        select(age_mid, val = rate_per100k) |> mutate(val = val / 1e5)
      inc_5yr <- gbd_full_df |>
        filter(sex == "Female", cause == "Cervical cancer", measure == "Incidence") |>
        select(age_mid, val = rate_per100k) |> mutate(val = val / 1e5)

      prev_1yr <- interp_piecewise_constant(prev_5yr$age_mid, prev_5yr$val, ages)
      inc_1yr  <- interp_piecewise_constant(inc_5yr$age_mid,  inc_5yr$val,  ages)

      out <- t(sapply(ages, function(a) {
        i      <- a + 1L
        p_prev <- min(prev_1yr[i], 0.999)
        pprc   <- min(precancer_multiplier * pmax(inc_1yr[i], 0), max(1 - p_prev, 0))
        c(well       = max(1 - p_prev - pprc, 0),
          precancer  = pprc,
          local      = p_prev * stage_dist$local,
          regional   = p_prev * stage_dist$regional,
          distant    = p_prev * stage_dist$distant,
          dead_cause = 0,
          dead_bg    = 0)
      }))
      validate_state_probs(out, label = "Cervical init probs / Female")
      out
    },

    tpm_fun = function(year, sex, ages, scenario) {
      if (sex != "Female") {
        k  <- length(states)
        id <- lapply(ages, function(a) { m <- diag(1, k, k); colnames(m) <- rownames(m) <- states; m })
        names(id) <- as.character(ages)
        return(id)
      }
      tl <- cervical_tpm$Female
      if (isTRUE(scenario$cancer_on)) {
        tl <- apply_screening(tl, year, scenario$speed)
        tl <- apply_treatment_adherence(tl, year, scenario$speed)
      }
      tl
    },

    eligible_fun = function(sex, ages, year) {
      if (sex == "Female") rep(TRUE, length(ages)) else rep(FALSE, length(ages))
    }
  )
}

# Safety check: fail if any male cervical burden is non-zero.
assert_zero_male_cervical <- function(mx_df) {
  bad <- mx_df |> filter(module == "cervical_ca", sex == "Male", mx_cause > 0)
  if (nrow(bad) > 0)
    stop("Non-zero male cervical mx detected. Check module eligibility logic. ",
         nrow(bad), " rows affected.")
  invisible(TRUE)
}

###############################################################################
# SECTION 9 — MODULE SET RUNNER
###############################################################################

# Check that module IDs are unique within a set before running.
validate_module_set <- function(modules) {
  ids <- vapply(modules, function(m) m$id, character(1))
  dup <- unique(ids[duplicated(ids)])
  if (length(dup) > 0)
    stop("Duplicate module IDs: ", paste(dup, collapse = ", "))
  invisible(TRUE)
}

# Run all modules in a list against the shared demographic backbone and
# scenario, returning a combined mx tibble and optional states tibble.
# Routes direct-mortality modules to run_direct_mortality_module().
run_module_set <- function(modules,
                           pop_array,
                           proj_years,
                           warmup_cycles = integer(0),
                           scenario     = list(),
                           store_states = FALSE) {
  validate_module_set(modules)

  res <- map(modules, function(mod) {
    if (!is.null(mod$type) && identical(mod$type, "direct_mortality"))
      return(run_direct_mortality_module(mod, pop_array, proj_years,
                                          warmup_cycles, scenario, store_states))
    run_cause_module(mod, pop_array, proj_years, warmup_cycles, scenario, store_states)
  })

  list(
    mx        = bind_rows(map(res, "mx")),
    states    = if (store_states) bind_rows(map(res, "states")) else NULL,
    by_module = res
  )
}

# Diagnostic: summarise mean and max mx_cause per module and sex.
summarise_module_burden <- function(mx_df) {
  mx_df |>
    group_by(module, sex) |>
    summarise(
      mean_mx = mean(mx_cause, na.rm = TRUE),
      max_mx  = max(mx_cause,  na.rm = TRUE),
      years   = n_distinct(year),
      ages    = n_distinct(age),
      .groups = "drop"
    ) |>
    arrange(desc(max_mx))
}

# Diagnostic: identify cells where sum(mx_cause) approaches the all-cause envelope.
find_envelope_pressure <- function(mx_df, wpp_mx_df, threshold = 0.8) {
  mx_df |>
    group_by(year, sex, age) |>
    summarise(mx_sum = sum(mx_cause, na.rm = TRUE), .groups = "drop") |>
    left_join(wpp_mx_df, by = c("year", "sex", "age")) |>
    mutate(ratio = if_else(mx_wpp > 0, mx_sum / mx_wpp, NA_real_)) |>
    filter(!is.na(ratio), ratio >= threshold) |>
    arrange(desc(ratio), year, sex, age)
}

message("── R/engine.R loaded ────────────────────────────────────────────────────")
message("  project_population_exposure(), run_cause_module(), run_module_set()")
message("  compute_q4030(), compute_deaths_averted(), target anchoring helpers")
message("  make_cvd_module(), make_direct_cvd_mortality_module(), make_cervical_module()")
