################################################################################
# INDONESIA INTEGRATED NCD MODEL — CROSS-LAYER INVARIANT CHECKS
# R/validation.R
# ─────────────────────────────────────────────────────────────────────────────
# Sourced by scripts/05_run_baseline.R and scripts/06_run_scenarios.R.
# Exports one function: run_core_invariant_suite()
#
# DESIGN PHILOSOPHY:
#   These checks enforce structural correctness across the three model layers
#   (demographic backbone, WPP mortality envelope, disease module mx output).
#   Deliberately separate from the within-module TPM validation in engine.R,
#   which operates at the level of individual transition matrices. A model that
#   passes all TPM row-sum checks can still violate cross-layer invariants
#   (e.g. cause fractions summing > 1 across modules).
#
# SEVERITY LEVELS:
#   STOP  — mathematical impossibility or structural error that would silently
#           corrupt all downstream outputs.
#   WARN  — plausibility concern that does not corrupt the maths.
#   INFO  — diagnostic summaries always printed.
#
# INVARIANTS CHECKED:
#   IC-1  Backbone year alignment
#   IC-2  Backbone dimensions [year × sex × age], sex ∈ {1,2}, ages 0:100
#   IC-3  Population plausibility (Indonesia: 200–350M)
#   IC-4  pop_df / pop_array consistency (within 0.1%)
#   IC-5  mx_df non-negativity
#   IC-6  mx_df no NAs in key columns
#   IC-7  Mortality envelope: sum(mx_cause) ≤ mx_wpp
#   IC-8  Year range: mx_df years within p_wpp$years
#   IC-9  Male cervical burden = 0
################################################################################

.ic_header <- function(id, label) message(sprintf("  IC-%s  %s", id, label))
.ic_pass   <- function(id)         message(sprintf("        \u2713 PASS"))
.ic_warn   <- function(id, msg)  { warning(sprintf("IC-%s: %s", id, msg), call. = FALSE)
                                    message(sprintf("        \u26a0 WARN: %s", msg)) }
.ic_stop   <- function(id, msg)    stop(sprintf("IC-%s FAIL: %s", id, msg), call. = FALSE)
.ic_info   <- function(msg)        message(sprintf("        \u2139 %s", msg))

#' Run all cross-layer model invariant checks.
#'
#' @param pop_array  [year × sex × age] array from project_population_exposure()
#' @param pop_df     Tibble: year, sex, age, pop — pre-filtered to PROJ_YEARS
#' @param wpp_mx_df  Tibble: year, sex, age, mx_wpp — pre-filtered to PROJ_YEARS
#' @param mx_df      Tibble: year, sex, age, module, mx_cause — output of
#'                   run_cause_module or bind_rows thereof. May include scenario.
#' @param p_wpp      Extended WPP parameter list ($years vector required)
#' @param expected_start_year  First projection year (default 2025L)
#'
#' @return Invisibly returns named list of check results. Stops or warns on
#'         failures; all checks produce INFO messages on a clean run.

run_core_invariant_suite <- function(pop_array,
                                     pop_df,
                                     wpp_mx_df,
                                     mx_df,
                                     p_wpp,
                                     expected_start_year = 2025L) {

  results <- list()
  message("\n── Cross-layer invariant checks ────────────────────────────────────────")

  # ── IC-1: Backbone year alignment ───────────────────────────────────────────
  .ic_header("1", "Backbone year alignment")
  actual_start <- p_wpp$years[1]
  if (actual_start != expected_start_year)
    .ic_stop("1", sprintf("p_wpp$years[1] = %d, expected %d.",
                           actual_start, expected_start_year))
  n_wpp_years <- length(p_wpp$years)
  .ic_info(sprintf("p_wpp$years: %d to %d (%d years)",
                   p_wpp$years[1], p_wpp$years[n_wpp_years], n_wpp_years))
  .ic_pass("1"); results$ic1 <- "PASS"

  # ── IC-2: Backbone dimensions ───────────────────────────────────────────────
  .ic_header("2", "Backbone array dimensions")
  d <- dim(pop_array)
  if (length(d) != 3) .ic_stop("2", sprintf("pop_array must be 3-D; got %d dims", length(d)))
  if (d[2] != 2L)     .ic_stop("2", sprintf("pop_array dim[2] (sex) must be 2; got %d", d[2]))
  if (d[3] != 101L)   .ic_stop("2", sprintf("pop_array dim[3] (age) must be 101; got %d", d[3]))
  .ic_info(sprintf("pop_array: [%d years \u00d7 %d sexes \u00d7 %d ages]", d[1], d[2], d[3]))
  .ic_pass("2"); results$ic2 <- "PASS"

  # ── IC-3: Population plausibility ───────────────────────────────────────────
  .ic_header("3", "Population plausibility (Indonesia)")
  total_pop <- sum(pop_array[1L, , ])
  .ic_info(sprintf("Total population at %d: %s",
                   expected_start_year,
                   formatC(round(total_pop), format = "d", big.mark = ",")))
  if (total_pop < 2e8 || total_pop > 3.5e8) {
    .ic_warn("3", sprintf("Total IDN pop at %d = %.1fM; expected ~275M (200-350M).",
                           expected_start_year, total_pop / 1e6))
    results$ic3 <- "WARN"
  } else { .ic_pass("3"); results$ic3 <- "PASS" }

  # ── IC-4: pop_df / pop_array consistency ────────────────────────────────────
  .ic_header("4", "pop_df / pop_array total consistency")
  pop_df_total <- pop_df |>
    dplyr::filter(year == expected_start_year) |>
    dplyr::summarise(tot = sum(pop, na.rm = TRUE)) |>
    dplyr::pull(tot)

  if (length(pop_df_total) == 0 || is.na(pop_df_total)) {
    .ic_warn("4", sprintf("pop_df has no rows for year %d.", expected_start_year))
    results$ic4 <- "WARN"
  } else {
    rel_diff <- abs(pop_df_total - total_pop) / max(total_pop, 1)
    .ic_info(sprintf("pop_df total at %d: %s  |  rel diff: %.4f%%",
                     expected_start_year,
                     formatC(round(pop_df_total), format = "d", big.mark = ","),
                     rel_diff * 100))
    if (rel_diff > 0.001)
      .ic_stop("4", sprintf("pop_df and pop_array totals differ by %.3f%% at %d.",
                             rel_diff * 100, expected_start_year))
    .ic_pass("4"); results$ic4 <- "PASS"
  }

  # ── IC-5: mx_df non-negativity ───────────────────────────────────────────────
  .ic_header("5", "mx_df non-negativity")
  n_neg <- sum(mx_df$mx_cause < -1e-12, na.rm = TRUE)
  .ic_info(sprintf("Negative mx_cause rows: %d", n_neg))
  if (n_neg > 0) {
    worst <- min(mx_df$mx_cause, na.rm = TRUE)
    bad   <- mx_df |> dplyr::filter(mx_cause < -1e-12) |>
              dplyr::arrange(mx_cause) |> utils::head(5)
    .ic_info(sprintf("Most negative: %.6g", worst))
    .ic_info("Top offending rows:"); print(bad)
    .ic_warn("5", sprintf("%d rows have mx_cause < 0 (min = %.2e).", n_neg, worst))
    results$ic5 <- "WARN"
  } else { .ic_pass("5"); results$ic5 <- "PASS" }

  # ── IC-6: mx_df no NAs ───────────────────────────────────────────────────────
  .ic_header("6", "mx_df no NAs in key columns")
  key_cols <- intersect(c("year","sex","age","module","mx_cause","deaths_cause"),
                         names(mx_df))
  n_na     <- sapply(key_cols, function(col) sum(is.na(mx_df[[col]])))
  total_na <- sum(n_na)
  .ic_info(sprintf("NAs by column: %s",
                   paste(paste0(key_cols, "=", n_na), collapse = ", ")))
  if (total_na > 0)
    .ic_stop("6", sprintf("%d NAs in: %s", total_na,
                           paste(key_cols[n_na > 0], collapse = ", ")))
  .ic_pass("6"); results$ic6 <- "PASS"

  # ── IC-7: Mortality envelope ─────────────────────────────────────────────────
  .ic_header("7", "Mortality envelope: sum(mx_cause) \u2264 mx_wpp")

  check_envelope <- function(df, label = "all") {
    mx_sum <- df |>
      dplyr::group_by(year, sex, age) |>
      dplyr::summarise(mx_ncd = sum(mx_cause, na.rm = TRUE), .groups = "drop")

    envelope <- wpp_mx_df |>
      dplyr::inner_join(mx_sum, by = c("year", "sex", "age")) |>
      dplyr::mutate(
        violation = mx_ncd > mx_wpp + 1e-12,
        ratio     = dplyr::if_else(mx_wpp > 0, mx_ncd / mx_wpp, NA_real_)
      )

    n_cells     <- nrow(envelope)
    n_violate   <- sum(envelope$violation, na.rm = TRUE)
    pct_violate <- if (n_cells > 0) 100 * n_violate / n_cells else 0
    max_ratio   <- max(envelope$ratio, na.rm = TRUE)

    .ic_info(sprintf("[%s] Cells: %d | Violations: %d (%.2f%%) | Max ratio: %.4f",
                     label, n_cells, n_violate, pct_violate, max_ratio))

    if (n_violate > 0) {
      top_viols <- envelope |>
        dplyr::filter(violation) |>
        dplyr::arrange(dplyr::desc(ratio)) |>
        utils::head(10) |>
        dplyr::select(year, sex, age, mx_wpp, mx_ncd, ratio)
      .ic_info("Top WPP-envelope violations:"); print(top_viols)

      .ic_stop(
        "7",
        sprintf(
          "[%s] %d cells (%.2f%%) exceed WPP all-cause envelope; max ratio = %.4f.",
          label, n_violate, pct_violate, max_ratio
        )
      )
    }

    list(n_violate = n_violate, pct_violate = pct_violate, max_ratio = max_ratio)
  }

  has_scenario <- "scenario" %in% names(mx_df)
  if (has_scenario) {
    scen_ids   <- unique(mx_df$scenario)
    env_res    <- lapply(scen_ids, function(sid)
                    check_envelope(dplyr::filter(mx_df, scenario == sid), sid))
    names(env_res) <- scen_ids
    results$ic7 <- env_res
  } else {
    results$ic7 <- check_envelope(mx_df, "baseline")
  }
  .ic_pass("7")

  # ── IC-8: Year range ─────────────────────────────────────────────────────────
  .ic_header("8", "mx_df year range within p_wpp$years")
  mx_years     <- sort(unique(mx_df$year))
  out_of_range <- setdiff(mx_years, p_wpp$years)
  .ic_info(sprintf("mx_df year range: %d to %d (%d years)",
                   min(mx_years), max(mx_years), length(mx_years)))
  if (length(out_of_range) > 0)
    .ic_stop("8", sprintf("%d years outside p_wpp$years: %s",
                           length(out_of_range),
                           paste(head(out_of_range, 5), collapse = ", ")))
  .ic_pass("8"); results$ic8 <- "PASS"

  # ── IC-9: Male cervical burden ────────────────────────────────────────────────
  .ic_header("9", "Male cervical burden = 0")
  cerv_male <- mx_df |>
    dplyr::filter(module == "cervical_ca", sex == "Male", mx_cause > 1e-12)
  if (nrow(cerv_male) > 0)
    .ic_stop("9", sprintf("%d rows of non-zero male cervical mx. Check eligibility logic.",
                           nrow(cerv_male)))
  .ic_pass("9"); results$ic9 <- "PASS"

  # ── SUMMARY ─────────────────────────────────────────────────────────────────
  n_pass <- sum(unlist(lapply(results[paste0("ic", 1:9)],
                              function(x) if (is.character(x)) x == "PASS" else TRUE)),
                na.rm = TRUE)
  message(sprintf("\n  Invariant suite: %d / 9 checks PASS\n", n_pass))

  invisible(results)
}
