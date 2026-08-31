#===========================================================================
# 05_build_baseline_70_30_30_to_70_70_70_subnational.R
#   SUBNATIONAL baseline builder.
#---------------------------------------------------------------------------
# Adapted from code/cvd-fair-choices/05_build_baseline_indonesia.R. In the
# production script the entire upstream risk-factor / secular-trend preparation
# is DISCARDED at run time by the line
#     b_rates <- copy(dt_calibrated)          # the reconciled rate table
#     b_rates <- b_rates[location == "Indonesia", ]
# i.e. the final baseline IS the reconciled rate table, filtered to one location.
# The subnational build therefore reduces to: take the already-loaded reconciled
# table (loaded by the 00 runner), KEEP the validated 38 provinces (NOT filtered
# to Indonesia) plus the national row (retained only for the province-to-national
# reconciliation), de-duplicate on the analytical key, and run fail-fast data
# contract + sick->dead baseline guards. No calibration, no 01/02 preparation.
#===========================================================================

suppressPackageStartupMessages({ library(data.table) })

if (!exists("b_rates") || !is.data.table(b_rates))
  stop("Model 05 (subnational): `b_rates` (the reconciled province rate table) must be ",
       "loaded by the 00 runner before this script.", call. = FALSE)
if (!exists("cascade_provinces"))
  stop("Model 05 (subnational): `cascade_provinces` (from Model 04) not found.", call. = FALSE)

setDT(b_rates)

# Province analysis set (validated in Model 04) + the national reconciliation row.
province_locations <- cascade_provinces
national_location  <- "Indonesia"
sim_locations      <- c(province_locations, if (national_location %in% b_rates$location) national_location)

# Keep ONLY the locations we simulate; NEVER silently fall back to national rates.
b_rates <- b_rates[location %in% sim_locations]

# De-duplicate on the analytical key (matches production 05's unique() call).
key_cols <- c("age", "sex", "cause", "location", "year")
n_before <- nrow(b_rates)
b_rates  <- unique(b_rates, by = key_cols)
if (nrow(b_rates) != n_before)
  cat(sprintf("Model 05 (subnational): removed %d duplicate key row(s).\n", n_before - nrow(b_rates)))

#...........................................................
# Fail-fast data contract (before any simulation) ----
#...........................................................
.fail <- function(msg) stop("Model 05 (subnational) data-contract FAIL: ", msg, call. = FALSE)

# (1) Required columns present.
req <- c("location", "year", "age", "sex", "cause", "Nx", "IR", "CF", "BG.mx",
         "BG.mx.all", "PREVt0", "DIS.mx.t0", "covid.mx", "pop")
miss <- setdiff(req, names(b_rates))
if (length(miss)) .fail(paste0("missing column(s): ", paste(miss, collapse = ", ")))

# (2) No duplicate analytical keys.
ndup <- nrow(b_rates[, .N, by = key_cols][N > 1L])
if (ndup > 0L) .fail(sprintf("%d duplicate location x year x age x sex x cause row(s).", ndup))

# (3) Complete + uniform grid coverage per location (years, ages, sexes, causes).
yrs   <- sort(unique(b_rates$year));  ages <- sort(unique(b_rates$age))
sexes <- sort(unique(b_rates$sex));   causes <- sort(unique(b_rates$cause))
exp_n <- length(yrs) * length(ages) * length(sexes) * length(causes)
loc_n <- b_rates[, .N, by = location]
if (any(loc_n$N != exp_n))
  .fail(sprintf("incomplete grid: expected %d rows/location (%dyr x %dage x %dsex x %dcause) but found {%s}.",
                exp_n, length(yrs), length(ages), length(sexes), length(causes),
                paste(sort(unique(loc_n$N)), collapse = ", ")))
if (!all(c("Female", "Male") %in% sexes)) .fail("sex must include Female and Male.")

# (4) Analysis-horizon coverage: warm-up 2017- and policy years 2025-2050 present.
need_years <- 2025:2050
if (!all(need_years %in% yrs))
  .fail(paste0("missing analysis years: ", paste(setdiff(need_years, yrs), collapse = ", ")))
if (min(yrs) > 2017L) .fail("warm-up years (2017-) missing; the engine seeds from 2017.")

# (5) Finite, bounded transition probabilities; nonnegative population.
if (b_rates[, any(!is.finite(IR) | IR < 0 | IR > 1)]) .fail("IR non-finite or outside [0,1].")
if (b_rates[, any(!is.finite(CF) | CF < 0 | CF > 1)]) .fail("CF non-finite or outside [0,1].")
if (b_rates[, any(!is.finite(BG.mx) | BG.mx < 0)])    .fail("BG.mx non-finite or negative.")
if (b_rates[, any(!is.finite(Nx) | Nx < 0)])          .fail("Nx (population) non-finite or negative.")

# (6) Province Nx must be province-specific (never overwritten by national pop).
#     Sanity: the 38 provinces should NOT all share the national population.
if (national_location %in% b_rates$location) {
  natN <- b_rates[location == national_location & year == 2025 & cause == causes[1],
                  sum(Nx)]
  prvN <- b_rates[location != national_location & year == 2025 & cause == causes[1],
                  sum(Nx)]
  cat(sprintf("Model 05 (subnational): 2025 pop -- sum(38 provinces) = %s ; national = %s ; ratio = %.4f\n",
              format(round(prvN), big.mark = ","), format(round(natN), big.mark = ","),
              prvN / natN))
}

#...........................................................
# Sick -> dead baseline guard (case-fatality target causes) ----
#...........................................................
# Any selected sick->dead (case_fatality) link needs its target cause to carry a
# valid baseline sick stock (PREVt0) and case fatality (CF) so the effect has
# something to act on. Fail loud rather than emit NA/all-zero.
.cf_targets <- character(0)
if (exists("fair_inputs") && !is.null(fair_inputs$valid_links)) {
  vl <- as.data.table(fair_inputs$valid_links)
  if (all(c("model_transition", "cause_code") %in% names(vl)))
    .cf_targets <- vl[model_transition == "case_fatality", unique(as.character(cause_code))]
}
.cf_targets <- intersect(.cf_targets[!is.na(.cf_targets) & nzchar(.cf_targets)], unique(b_rates$cause))
if (length(.cf_targets)) {
  for (cc in .cf_targets) {
    sub <- b_rates[cause == cc & year >= 2025 & location %in% province_locations]
    if (anyNA(sub$CF) || all(sub$CF == 0) || anyNA(sub$PREVt0) || all(sub$PREVt0 == 0))
      .fail(sprintf("case-fatality target '%s' has NA/all-zero CF or PREVt0 in the province rows.", cc))
    cat(sprintf("Model 05 (subnational): sick->dead target '%s' baseline OK (CF mean %.4g; PREVt0 mean %.4g).\n",
                cc, mean(sub$CF), mean(sub$PREVt0)))
  }
} else cat("Model 05 (subnational): no selected sick->dead links; no case-fatality baseline guard needed.\n")

cat(sprintf("Model 05 (subnational): baseline ready -- %s rows, %d simulated location(s) (%d provinces%s), ",
            format(nrow(b_rates), big.mark = ","), length(sim_locations), length(province_locations),
            if (national_location %in% sim_locations) " + Indonesia for reconciliation" else ""),
    sprintf("years %d-%d, causes {%s}.\n", min(yrs), max(yrs), paste(causes, collapse = ", ")), sep = "")
