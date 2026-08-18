#===========================================================================
# tests_fair_cost_value.R  --  fast checks for the workbook-driven FAIR
# Choices cost/value extension (Models 04 / 06 / 09).
#
# These are STATIC / fixture-level tests: syntax, workbook schema & key
# cardinality, the FAIR coverage-adjusted-effect formula, coverage-trajectory
# behaviour, transition-probability bounds, multiplicative order-invariance,
# and (if present) a light check of the produced cost/value workbook. They do
# NOT run the full Markov model, so they are fast and have no heavy data deps.
#
# Run:  Rscript code/cvd-fair-choices/tests_fair_cost_value.R
#===========================================================================
suppressWarnings(suppressMessages({library(readxl); library(data.table)}))

.n_pass <- 0L; .n_fail <- 0L
chk <- function(desc, cond) {
  ok <- isTRUE(cond)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", desc))
  if (ok) .n_pass <<- .n_pass + 1L else .n_fail <<- .n_fail + 1L
  invisible(ok)
}
approx <- function(a, b, tol = 1e-4) is.finite(a) && is.finite(b) && abs(a - b) <= tol

# Locate repo root (portable): walk up until data/indonesia_model_inputs.xlsx.
find_repo <- function() {
  d <- if (exists("wd")) get("wd") else getwd()
  d <- normalizePath(d, winslash = "/", mustWork = FALSE)
  for (i in 1:6) {
    if (file.exists(file.path(d, "data", "indonesia_model_inputs.xlsx"))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate repo root (data/indonesia_model_inputs.xlsx).")
}
repo <- find_repo()
code_dir <- file.path(repo, "code", "cvd-fair-choices")
wb_in    <- file.path(repo, "data", "indonesia_model_inputs.xlsx")
wb_out   <- file.path(repo, "output", "indonesia_model_cost_value.xlsx")
wb_form  <- file.path(repo, "output", "indonesia_model_cost_value_formulae.xlsx")

cat("== 1. Syntax (parse) of changed R files ==\n")
for (f in c("00_run_model_cvd_fair.R", "04_define_interventions_indonesia.R",
            "06_run_scenarios_indonesia_fair.R", "09_cost_value.R")) {
  p <- file.path(code_dir, f)
  chk(paste("parse", f), tryCatch({ parse(p); TRUE }, error = function(e) { cat("   ", conditionMessage(e), "\n"); FALSE }))
}

cat("\n== 2. Input workbook schema & key cardinality ==\n")
sheets <- readxl::excel_sheets(wb_in)
req_sheets <- c("Assumptions", "Dictionaries", "Intervention_Cause_Map",
                "Effect_Sizes", "Coverage", "Cost_Components")
chk("required sheets present", all(req_sheets %in% sheets))
map <- as.data.table(read_excel(wb_in, "Intervention_Cause_Map"))
eff <- as.data.table(read_excel(wb_in, "Effect_Sizes"))
cov <- as.data.table(read_excel(wb_in, "Coverage"))
cst <- as.data.table(read_excel(wb_in, "Cost_Components"))
chk("map has intervention_cause_key / include_flag / cost_scope / cost_join_key",
    all(c("intervention_cause_key", "include_flag", "cost_scope", "cost_join_key") %in% names(map)))
chk("effect has transition_from/to, effect_value, affected_fraction",
    all(c("transition_from", "transition_to", "effect_value", "affected_fraction") %in% names(eff)))
chk("coverage has baseline_coverage / target_override",
    all(c("baseline_coverage", "target_override") %in% names(cov)))
sel <- map[as.integer(include_flag) == 1L]
chk("selected intervention_cause_key unique",
    nrow(sel[, .N, by = intervention_cause_key][N > 1]) == 0)
chk("one effect row per selected link",
    all(sel$intervention_cause_key %in% eff$intervention_cause_key) &&
      max(eff[intervention_cause_key %in% sel$intervention_cause_key, .N, by = intervention_cause_key]$N) == 1)
chk("one coverage row per selected link",
    all(sel$intervention_cause_key %in% cov$intervention_cause_key))
chk("cost_join_key values resolve into the map",
    all(cst$cost_join_key %in% map$cost_join_key))

cat("\n== 3. FAIR coverage-adjusted effect formula ==\n")
# apply_coverage_adjustment (as in Model 06) + affected fraction.
fair_te <- function(effect, cov_t, cov0, af) {
  e_adj <- if (cov0 == 0) effect * cov_t else effect * (cov_t - cov0) / (1 - effect * cov0)
  af * e_adj
}
chk("prompt fixture 0.58,0.31,0.80,0.80 -> 0.2772",
    approx(fair_te(0.58, 0.80, 0.31, 0.80), 0.2772, 1e-3))
chk("IHD secondary 0.5822,0.31,0.80,0.20 -> ~0.06962",
    approx(fair_te(0.5822, 0.80, 0.31, 0.20), 0.06962, 1e-3))
chk("RHD surgery 0.75,0.10,0.80,1.0 -> ~0.5676",
    approx(fair_te(0.75, 0.80, 0.10, 1.0), 0.5676, 1e-3))
chk("zero delta -> zero effect", approx(fair_te(0.58, 0.31, 0.31, 0.80), 0, 1e-9))

cat("\n== 4. Coverage trajectory (baseline -> target, linear) ==\n")
cov_path <- function(cb, ct, sy, ty, yrs) {
  span <- max(ty - sy + 1, 1); frac <- pmin(pmax((yrs - sy + 1) / span, 0), 1)
  cc <- cb + (ct - cb) * frac; cc[yrs < sy] <- cb; cc[yrs > ty] <- ct; pmin(pmax(cc, 0), 1)
}
yrs <- 2024:2051; p <- cov_path(0.31, 0.80, 2025, 2050, yrs)
chk("baseline before start", approx(p[yrs == 2024], 0.31) && approx(p[yrs == 2025 - 1], 0.31))
chk("target at target year", approx(p[yrs == 2050], 0.80))
chk("target after target year", approx(p[yrs == 2051], 0.80))
chk("monotonic non-decreasing", all(diff(p) >= -1e-12))
chk("within [0,1]", all(p >= 0 & p <= 1))

cat("\n== 5. Transition bounds & multiplicative order-invariance ==\n")
p_scn <- function(p_base, te) p_base * (1 - te)
chk("p_scenario in [0,1] for te in [0,1]",
    { g <- expand.grid(pb = c(0, .3, .99), te = c(0, .28, 1)); all({v <- p_scn(g$pb, g$te); v >= 0 & v <= 1}) })
te1 <- 0.3249; te2 <- 0.5676  # RHD secondary + surgery on the same transition
comb_a <- (1 - te1) * (1 - te2); comb_b <- (1 - te2) * (1 - te1)
chk("combined surviving fraction order-invariant", approx(comb_a, comb_b, 1e-12))
chk("combined survivor ~0.292 (RHD both)", approx(comb_a, 0.2919, 1e-3))

cat("\n== 6. Produced cost/value workbook (if present) ==\n")
if (file.exists(wb_out) && requireNamespace("openxlsx", quietly = TRUE)) {
  sn <- openxlsx::getSheetNames(wb_out)
  need <- c("README", "Run_Metadata", "Selected_Interventions", "Cost_Components",
            "Annual_Mortality", "Budget_Impact", "Cost_Effectiveness",
            "QA_Checks", "Model_State_Trace", "Background_Mortality", "Methods_and_Sources")
  chk("required information sheets present", all(need %in% sn))
  qa <- as.data.table(openxlsx::read.xlsx(wb_out, "QA_Checks"))
  chk("QA_Checks has no FAIL", sum(qa$status == "FAIL") == 0)
  if ("Annual_Cost" %in% sn && "Budget_Impact" %in% sn) {
    ac <- as.data.table(openxlsx::read.xlsx(wb_out, "Annual_Cost"))
    bi <- as.data.table(openxlsx::read.xlsx(wb_out, "Budget_Impact"))
    r1 <- ac[, .(c = sum(annual_cost_scenario)), by = .(scenario, year)]
    r2 <- bi[, .(scenario, year, scenario_cost)]
    m  <- merge(r1, r2, by = c("scenario", "year"))
    chk("Annual_Cost reconciles to Budget_Impact", max(abs(m$c - m$scenario_cost)) < 1e-2)
  }
  cea <- as.data.table(openxlsx::read.xlsx(wb_out, "Cost_Effectiveness"))
  chk("CEA ratio = disc incremental cost / deaths averted",
      { s <- cea[!is.na(cost_per_death_averted)]
        all(abs(s$cost_per_death_averted - s$disc_incremental_cost / s$deaths_averted) < 1e-3) })
} else {
  cat("   (cost/value workbook not found; run Model 09 first -- skipping section 6)\n")
}

cat("\n== 7. Formula-driven workbook (if present) ==\n")
# Portable structural checks (no Excel/COM needed): required sheets, live Excel
# formulas present in the decision tables, editable assumptions sheet, and a
# well-formed package (no dangling drawing parts).
if (file.exists(wb_form) && requireNamespace("openxlsx", quietly = TRUE)) {
  snf <- openxlsx::getSheetNames(wb_form)
  needf <- c("README", "Run_Metadata", "Selected_Interventions", "Cost_Components",
             "Annual_Mortality", "Annual_Cost", "Budget_Impact", "Cost_Effectiveness",
             "QA_Checks", "Calculation_Assumptions", "Calculation_Map", "Methods_and_Sources")
  chk("formula workbook required sheets present", all(needf %in% snf))
  ca <- as.data.table(openxlsx::read.xlsx(wb_form, "Calculation_Assumptions"))
  chk("Calculation_Assumptions exposes editable controls",
      all(c("analysis_start_year", "cost_discount_rate", "baseline_scenario_id") %in%
            ca$parameter_id))
  # Scan the sheet XML for <f> formula elements and for dangling drawing parts.
  tdir <- file.path(tempdir(), "fmlchk"); unlink(tdir, recursive = TRUE); dir.create(tdir)
  utils::unzip(wb_form, exdir = tdir)
  sx <- list.files(file.path(tdir, "xl", "worksheets"), pattern = "\\.xml$", full.names = TRUE)
  nf <- sum(vapply(sx, function(f) {
    x <- paste(readLines(f, warn = FALSE), collapse = "")
    length(gregexpr("<f>", x, fixed = TRUE)[[1]][gregexpr("<f>", x, fixed = TRUE)[[1]] > 0])
  }, integer(1)))
  chk("formula workbook contains live Excel formulas (>1000)", nf > 1000)
  ct <- paste(readLines(file.path(tdir, "[Content_Types].xml"), warn = FALSE), collapse = "")
  chk("formula workbook is well-formed (no dangling drawing parts)",
      !grepl("drawings/drawing", ct) && !dir.exists(file.path(tdir, "xl", "drawings")))
} else {
  cat("   (formula workbook not found; run Model 09 first -- skipping section 7)\n")
}

cat(sprintf("\n==== RESULT: %d passed, %d failed ====\n", .n_pass, .n_fail))
if (.n_fail > 0L) quit(status = 1L)
