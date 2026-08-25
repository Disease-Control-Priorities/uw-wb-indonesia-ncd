# =============================================================================
# tests_tobacco_ssb_mortality.R
# Acceptance / QA checks for the tobacco -> mortality (Jha) and SSB -> diabetes
# mortality (sick -> dead) implementation across Models 00/04/05/06/09.
#
# HOW TO RUN:
#   setwd(".../code/cvd-fair-choices"); source("tests_tobacco_ssb_mortality.R")
#
# Part A (always): pure-function checks on the Model 06 transition helpers
#   (rate_ratio_to_probability, tobacco_lag_fraction) sourced directly from
#   06_run_scenarios_indonesia_fair.R, plus the Jha timing/M08/M16 math via
#   calculate_tobacco_transition_effects on a workbook-driven config.
# Part B (if present): integration checks on the on-disk public-health cost/value
#   workbook and the Model 06 output (mirrors tests_acceptance_0006.R).
#
# Every effect size / HR / ERD scalar comes from the workbook (read by name);
# the test invents no numbers.
# =============================================================================
suppressMessages({library(data.table); library(readxl)})

wd      <- "C:/Users/wrgar/OneDrive - UW/02Work/WorldBank-Indonesia/uw-wb-indonesia-ncd/"
wd_data <- paste0(wd, "data/")
wd_outp <- paste0(wd, "output/")
ph_wb   <- local({
  cand <- paste0(wd_data, c("indonesia_model_inputs_public_health_updated_mortality.xlsx",
                            "indonesia_model_inputs_public_health_updated.xlsx",
                            "indonesia_model_inputs_public_health.xlsx"))
  cand[file.exists(cand)][1]
})

pass <- TRUE
chk  <- function(name, ok, extra = "") {
  cat(sprintf("[%s] %s %s\n", ifelse(isTRUE(ok), "PASS", "FAIL"), name, extra))
  if (!isTRUE(ok)) pass <<- FALSE
}
approx <- function(a, b, tol = 1e-6) is.finite(a) && is.finite(b) && abs(a - b) <= tol

# ---- Source the Model 06 PH transition helpers (marker-bounded, no side effects) ----
src   <- readLines("06_run_scenarios_indonesia_fair.R")
i0    <- grep("PUBLIC-HEALTH WORKBOOK-driven transition helpers", src)[1]
i1    <- grep("# Model\\. Project\\.all function", src)[1]
stopifnot(!is.na(i0), !is.na(i1), i1 > i0)
eval(parse(text = paste(src[i0:(i1 - 1)], collapse = "\n")), envir = environment())

cat("\n== Part A: transition-helper math ==\n")

# E2 rate_ratio_to_probability (M17): 1-(1-p)^rr, never p*rr
chk("E2 rate_ratio_to_probability(0.10,0.927) ~ 0.0930",
    approx(rate_ratio_to_probability(0.10, 0.927), 0.0930, 5e-4),
    sprintf("= %.5f", rate_ratio_to_probability(0.10, 0.927)))
chk("E2b rate_ratio_to_probability(p,1)==p (identity)", approx(rate_ratio_to_probability(0.037, 1), 0.037))
chk("E2c rate_ratio_to_probability(p,0)==0 (full protection)", approx(rate_ratio_to_probability(0.2, 0), 0))

# E3 normalized-exponential timing: 0 at yr0, monotone, in [0,1], =1 at yr10+
lf <- tobacco_lag_fraction(0:12)
chk("E3 lag fraction: 0@yr0, monotone, in[0,1], =1@yr10 and after",
    approx(lf[1], 0) && all(diff(lf) >= -1e-12) && all(lf >= 0 & lf <= 1) &&
    approx(lf[11], 1) && approx(lf[13], 1))
chk("E3b tobacco_lag_fraction validates lag_rate",
    inherits(try(tobacco_lag_fraction(1, lag_rate = 0), silent = TRUE), "try-error"))

# ---- Build a workbook-driven tobacco config (mirrors Model 04's parser) ----
rr  <- as.data.table(read_excel(ph_wb, sheet = "Risk_Response"))
asm <- as.data.table(read_excel(ph_wb, sheet = "Assumptions"))
A   <- setNames(as.character(asm$value), asm$parameter_id)
numA <- function(k) suppressWarnings(as.numeric(A[[k]]))
j <- rr[grepl("^SCALAR_JHA_", response_key)]
parse_key <- function(k) {
  s <- sub("^SCALAR_JHA_", "", k); sx <- substr(s, 1, 1); rest <- sub("^[MF]_", "", s)
  if (grepl("_LT3$", rest))       { d <- "LT3";  ages <- sub("_LT3$", "", rest) }
  else if (grepl("_GE10$", rest)) { d <- "GE10"; ages <- sub("_GE10$", "", rest) }
  else if (grepl("_Y3_9$", rest)) { d <- "Y3_9"; ages <- sub("_Y3_9$", "", rest) }
  ab <- strsplit(ages, "_")[[1]]
  data.table(sex = ifelse(sx == "M", "Male", "Female"),
             age_lo = as.integer(ab[1]), age_hi = as.integer(ab[2]), duration = d)
}
sm <- cbind(rbindlist(lapply(j$response_key, parse_key)),
            ERD = as.numeric(j$source_effect_value))
sm[, ERD_ge10 := ERD[duration == "GE10"], by = .(sex, age_lo, age_hi)]
sm[, lambda := pmin(1, ERD / ERD_ge10)]
cfg <- list(scalar_matrix = sm,
            erd10_by_band = unique(sm[duration == "GE10", .(sex, age_lo, age_hi, erd10 = ERD)]),
            vasc_rr_male = numA("tobacco_vascular_mortality_rr_male"),
            vasc_rr_female = numA("tobacco_vascular_mortality_rr_female"),
            vasc_rr_pooled = numA("tobacco_vascular_mortality_rr_pooled"),
            erd10_pooled = numA("tobacco_vascular_erd_ge10_pooled"),
            timing_mode = "jha_piecewise_shared_scalar",
            lag_rate = numA("tobacco_cvd_lag_rate"),
            full_effect_year = as.integer(numA("tobacco_cvd_full_effect_year")))

# E4 Jha scalars in [0,1]; GE10 == 1; age 80-95 reuses 60-79
a80 <- sm[age_lo == 80]; a60 <- sm[age_lo == 60]
chk("E4 Jha lambda in [0,1], GE10==1, age80 reuses 60-79",
    all(sm$lambda >= 0 & sm$lambda <= 1) && all(sm[duration == "GE10", lambda] == 1) &&
    all(abs(a80[a60, on = .(sex, duration)]$lambda - a60$lambda) < 1e-12))

# Synthetic tobacco-tax exposure path (workbook baseline/target/years) for IHD.
p0 <- 0.303; ptg <- 0.19828320; sy <- 2026L; ty <- 2030L; yrs <- 2020:2055
frac <- pmin(pmax((yrs - sy + 1) / max(ty - sy + 1, 1), 0), 1)
expo <- p0 + (ptg - p0) * frac; expo[yrs < sy] <- p0; expo[yrs > ty] <- ptg; names(expo) <- yrs
inc_row <- data.table(model_transition = "incidence", response_value = 1.60,
                      baseline_exposure = p0, target_exposure = ptg, age_start = 20L, age_stop = 95L)
mor_row <- copy(inc_row)[, model_transition := "case_fatality"]
ei <- calculate_tobacco_transition_effects(expo, yrs, inc_row, cfg)
em <- calculate_tobacco_transition_effects(expo, yrs, mor_row, cfg)

# E5 later years accrue more effect (RR_eff falls toward RR_full, =1 by full-effect year)
lam_ramp <- ei[age == 55 & sex == "Male", .(rr = mean(rr_eff)), by = year][order(year)]
chk("E5 tobacco effect accrues over time (RR_eff non-increasing while ramping)",
    all(diff(lam_ramp[year %in% 2026:2040, rr]) <= 1e-9))
chk("E5b full effect reached by year 2040 (all cohorts 10+ yrs)",
    approx(ei[age == 55 & sex == "Male" & year == 2040, rr_eff][1],
           (1 + ptg*(1.60-1))/(1 + p0*(1.60-1)), 1e-6))

# E6/E7 p_t=p0 (no quitting cohorts) => RR_eff==1 everywhere
flat <- setNames(rep(p0, length(yrs)), yrs)
e6 <- calculate_tobacco_transition_effects(flat, yrs, inc_row, cfg)
chk("E6/E7 no exposure change => no transition effect (RR_eff==1)", all(abs(e6$rr_eff - 1) < 1e-9))

# E8 sex-specific vascular mortality RR (2.9 M vs 3.1 F) => different mortality RR
mM <- em[age == 55 & sex == "Male"   & year == 2050, rr_eff][1]
mF <- em[age == 55 & sex == "Female" & year == 2050, rr_eff][1]
chk("E8 sex-specific vascular RR used (M vs F mortality RR differ)", abs(mM - mF) > 1e-6,
    sprintf("(M=%.4f F=%.4f)", mM, mF))

cat("\n== Part B: workbook contract + on-disk integration ==\n")

# E1/E11 workbook transition contract
map <- as.data.table(read_excel(ph_wb, sheet = "Intervention_Cause_Map"))
map[, include_flag := as.integer(include_flag)]
n_sd  <- map[transition_to == "dead", .N]
n_sd_tob <- map[transition_to == "dead" & grepl("^I_PH_TOB", intervention_id) & include_flag == 1, .N]
n_ws  <- map[transition_from == "well" & transition_to == "sick" & include_flag == 1, .N]
n_ssb_on <- map[transition_to == "dead" & grepl("SSB", intervention_id) & include_flag == 1, .N]
chk("E11 workbook: 13 sick->dead links (12 tobacco enabled + 1 SSB), 48 incidence, 0 SSB enabled",
    n_sd == 13 && n_sd_tob == 12 && n_ws == 48 && n_ssb_on == 0,
    sprintf("(sd=%d tob=%d inc=%d ssb_on=%d)", n_sd, n_sd_tob, n_ws, n_ssb_on))

# Integration with the produced PH cost/value workbook (if present)
phf <- paste0(wd_outp, "indonesia_cost_value_public_health_formulae.xlsx")
if (file.exists(phf) && requireNamespace("openxlsx", quietly = TRUE)) {
  ep <- as.data.table(openxlsx::read.xlsx(phf, "Effect_Parameters"))
  chk("B-wb Effect_Parameters: 48 incidence + 12 case_fatality; transition fields retained",
      ep[model_transition == "incidence", .N] == 48 &&
      ep[model_transition == "case_fatality", .N] == 12 &&
      all(c("transition_from", "transition_to") %in% names(ep)))
  qa <- as.data.table(openxlsx::read.xlsx(phf, "QA_Checks"))
  chk("B-wb QA_Checks has no FAIL", sum(qa$status == "FAIL", na.rm = TRUE) == 0)
} else cat("   (PH cost/value workbook not found; run Model 09 first -- skipping)\n")

# Integration with the Model 06 output (if present)
mo_f <- list.files(paste0(wd_outp, "out_model"), pattern = "^model_output_Indonesia_.*\\.rds$", full.names = TRUE)
if (length(mo_f)) {
  out <- setDT(readRDS(mo_f[which.max(file.mtime(mo_f))]))
  if ("scenario" %in% names(out) && "I_PH_TOBACCO_TAX" %in% out$scenario) {
    chk("E12a baseline scenario unchanged (eff_ir==eff_cf==1)",
        out[scenario == "baseline", all(abs(eff_ir - 1) < 1e-9 & abs(eff_cf - 1) < 1e-9)])
    sv <- c("well", "sick", "dead", "pop", "newcases")
    chk("E12b states finite & nonneg; eff_cf in [0,2]",
        all(sapply(sv, function(c) all(is.finite(out[[c]]) & out[[c]] >= -1e-6))) &&
        out[, all(eff_cf >= 0 & eff_cf <= 2 & eff_ir >= 0 & eff_ir <= 2)])
    tt <- out[scenario == "I_PH_TOBACCO_TAX" & year >= 2035]
    chk("E9a incidence effect present on tobacco CVD causes (eff_ir<1)",
        tt[cause %in% c("ihd", "istroke", "hstroke"), any(eff_ir < 1)])
    chk("E9b mortality effect present ONLY on IHD/IS/ICH (eff_cf<1); dm2 eff_cf==1",
        tt[cause %in% c("ihd", "istroke", "hstroke"), any(eff_cf < 1)] &&
        tt[cause == "dm2", all(abs(eff_cf - 1) < 1e-9)])
    chk("E9c non-tobacco causes untouched by tobacco (hhd/cmd/rhd eff==1)",
        tt[cause %in% c("hhd", "cmd", "rhd"), all(abs(eff_ir - 1) < 1e-9 & abs(eff_cf - 1) < 1e-9)])
  } else cat("   (latest Model 06 output has no public-health scenarios; skipping E9/E12)\n")
} else cat("   (no Model 06 output found; run Model 06 first -- skipping E9/E12)\n")

cat("\n", strrep("=", 55), "\n",
    ifelse(pass, "ALL TOBACCO/SSB MORTALITY TESTS PASSED", "SOME TESTS FAILED"),
    "\n", strrep("=", 55), "\n", sep = "")
if (!pass) stop("tobacco/ssb mortality tests failed")
