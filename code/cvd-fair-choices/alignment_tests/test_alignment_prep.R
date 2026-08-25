#===============================================================================
# test_alignment_prep.R  -- unit tests for the Phase-2 alignment TP builder.
# Run:  Rscript code/cvd-fair-choices/alignment_tests/test_alignment_prep.R
# Exits non-zero on the first failed assertion.
#===============================================================================
suppressWarnings(suppressMessages(library(data.table)))

wd_root <- "C:/Users/wrgar/OneDrive - UW/02Work/WorldBank-Indonesia/uw-wb-indonesia-ncd/"
source(file.path(wd_root, "code/cvd-fair-choices/alignment_tests/alignment_helpers.R"))

## lightweight bootstrap (loads central cause_map / age grid / paths)
align_bootstrap_config(wd_root)

npass <- 0L; nfail <- 0L
ok <- function(cond, msg) {
  if (isTRUE(cond)) { npass <<- npass + 1L; cat(sprintf("  PASS  %s\n", msg)) }
  else { nfail <<- nfail + 1L; cat(sprintf("  FAIL  %s\n", msg)) }
}

cat("\n== Load inputs ==\n")
align_check_inputs(wd_data)
proj_raw <- readRDS(file.path(wd_data, "indonesia_epidemiology_baseline_alignment.rds"))
setDT(proj_raw)
prod_adj <- align_load_production_adjusted(wd_data)

cat("\n== Crosswalk ==\n")
xw <- align_build_crosswalk(proj_raw, cause_map)
print(xw)
ok(setequal(xw$cause_short, c("ihd","istroke","hstroke","hhd","rhd")),
   "crosswalk maps the 5 projection causes to ihd/istroke/hstroke/hhd/rhd")
ok(!anyNA(xw$cause_short), "no unmapped projection cause")

cat("\n== Terminal-age pooling (>=95 -> 95) ==\n")
pooled <- align_pool_terminal(proj_raw, max_age = max_model_age)
ok(max(pooled$age) == max_model_age, "pooled max age == max_model_age (95)")
ok(uniqueN(pooled$age) == max_model_age + 1L, "single ages 0..95 present")
# reconciliation: pooled 95+ counts == sum of raw ages 95..100 counts
rawterm <- proj_raw[age >= max_model_age,
   .(pop = sum(population), pc = sum(prevalent_cases), cd = sum(cause_deaths)),
   by = .(year, sex, cause_id)]
poolterm <- pooled[age == max_model_age,
   .(pop = population, pc = prevalent_cases, cd = cause_deaths),
   by = .(year, sex, cause_id)]
recon <- merge(rawterm, poolterm, by = c("year","sex","cause_id"),
               suffixes = c("_raw","_pool"))
ok(recon[, max(abs(pop_raw - pop_pool))] < 1e-6, "95+ population reconciles (sum)")
ok(recon[, max(abs(pc_raw  - pc_pool ))] < 1e-6, "95+ prevalent_cases reconciles")
ok(recon[, max(abs(cd_raw  - cd_pool ))] < 1e-6, "95+ cause_deaths reconciles")

cat("\n== Fill missing projection years (2024) ==\n")
filled <- align_fill_missing_years(pooled, y_start = 2023L, y_end = 2050L)
ok(setequal(unique(filled$year), 2023:2050), "all 28 projection years present after fill")
ok(filled[interp_year == TRUE, all(year == 2024L)] &&
     filled[year == 2024L, all(interp_year)], "only 2024 is interpolated")
# 2024 == mean(2023,2025) on the rate scale (per age,sex,cause_id)
mid <- merge(
  filled[year == 2024, .(age, sex, cause_id, r24 = prevalence_rate, m24 = all_cause_mx)],
  merge(filled[year == 2023, .(age, sex, cause_id, r23 = prevalence_rate, m23 = all_cause_mx)],
        filled[year == 2025, .(age, sex, cause_id, r25 = prevalence_rate, m25 = all_cause_mx)],
        by = c("age","sex","cause_id")),
  by = c("age","sex","cause_id"))
ok(mid[, max(abs(r24 - 0.5*(r23+r25)))] < 1e-9, "2024 prevalence_rate == mean(2023,2025)")
ok(mid[, max(abs(m24 - 0.5*(m23+m25)))] < 1e-9, "2024 all_cause_mx == mean(2023,2025)")

cat("\n== Derive projection TPs ==\n")
built <- align_derive_projection_tps(filled, xw, prod_adj, cause_map,
                                     location = "Indonesia",
                                     max_age = max_model_age,
                                     y_start = 2023L, y_end = 2050L)
tps <- built$tps
cat("adjustment log:\n"); print(built$adj_log)

# schema/order/classes vs production adjusted_searo
ok(identical(names(tps), names(prod_adj)),
   "column names + ORDER identical to production adjusted_searo")
cls_t <- vapply(tps, function(x) class(x)[1], "")
cls_p <- vapply(prod_adj, function(x) class(x)[1], "")
ok(identical(cls_t, cls_p), "column CLASSES identical to production adjusted_searo")

# key uniqueness (no row expansion)
kc <- c("age","sex","cause","year","location")
ok(nrow(tps) == nrow(unique(tps[, ..kc])), "projection TP key unique (no expansion)")
ok(nrow(tps) == 28L*96L*2L*5L, "row count == 28yr(2023-2050) * 96age * 2sex * 5cause")

# no NA/NaN/Inf in required fields
bad <- sapply(tps, function(x) if (is.numeric(x)) sum(!is.finite(x)) else sum(is.na(x)))
ok(all(bad == 0), "no NA/NaN/Inf in any required field")

# probability ranges + row constraints (covid absent at this stage)
ok(tps[, all(IR >= 0 & IR <= 1)], "IR in [0,1]")
ok(tps[, all(CF >= 0 & CF <= 1)], "CF in [0,1]")
ok(tps[, all(IR + BG.mx <= 1 + 1e-9)], "IR + BG.mx <= 1")
ok(tps[, all(CF + BG.mx <= 1 + 1e-9)], "CF + BG.mx <= 1")
ok(tps[, all(BG.mx.all >= 0 & BG.mx.all <= 1)], "BG.mx.all in [0,1]")

# verified identity BG.mx == ALL.mx - DIS.mx.t0
ok(tps[, max(abs(BG.mx - (ALL.mx - DIS.mx.t0)))] < 1e-9,
   "BG.mx == ALL.mx - DIS.mx.t0 (production identity)")

# years 2023-2050 present for every (age,sex,cause)
yrs <- tps[, .(nyr = uniqueN(year)), by = .(age, sex, cause)]
ok(yrs[, all(nyr == 28L)], "all (age,sex,cause) have 28 years 2023-2050")

# per-measure fallback: HHD IR from production (2019 carry), HHD prev from projection
hhd_prov <- built$provenance[cause == cause_map[["hhd"]]]
ok(nrow(hhd_prov) > 0 && all(hhd_prov$IR_src == "production_fallback"),
   "HHD IR tagged production_fallback (incidence unavailable, all years incl 2024)")
ok(built$provenance[cause != cause_map[["hhd"]] & year != 2024, all(IR_src == "alignment")],
   "non-HHD, non-2024 IR tagged alignment")
ok(built$provenance[year == 2024, all(Nx_src == "interpolation" & ALL.mx_src == "interpolation")],
   "2024 (interpolated year) fields tagged interpolation")
# HHD IR value equals production HHD IR at 2019 (per age/sex), pre any clamp
hhd_long <- cause_map[["hhd"]]
hhd_check <- merge(
  tps[cause == hhd_long, .(age, sex, year, IR)],
  prod_adj[year == 2019 & cause == hhd_long, .(age, sex, IR2019 = IR)],
  by = c("age","sex"), all.x = TRUE)
ok(hhd_check[, max(abs(IR - IR2019))] < 1e-9,
   "HHD IR(2023-2050) == production HHD IR(2019) exactly")
# CF/PREVt0 for HHD come from projection (prevalence available)
ok(all(built$provenance[cause == hhd_long, CF_src] %in%
         c("alignment","derived_zero_denom","interpolation")),
   "HHD CF derived from projection prevalence/deaths (or interpolation in 2024)")

cat("\n== Assemble pre-calibration adjusted_searo ==\n")
precal <- align_assemble_precalib(prod_adj, tps, y_hist_end = 2019L)
ok(identical(names(precal), names(prod_adj)), "precalib schema identical to production")

# pre-2023 rows equal production EXACTLY on shared keys/columns
histp <- precal[year <= 2019]
setkeyv(histp, kc); pp <- copy(prod_adj); setkeyv(pp, kc)
merged_hist <- merge(histp, pp, by = kc, suffixes = c("",".prod"))
num_cols <- setdiff(names(prod_adj), kc)
maxdiff <- max(sapply(num_cols, function(c) max(abs(merged_hist[[c]] - merged_hist[[paste0(c,".prod")]]))))
ok(nrow(histp) == nrow(prod_adj) && maxdiff == 0,
   "pre-2023 rows equal production adjusted_searo EXACTLY")

# cmd/dm2 present (via production fallback) in the prepared table (<=2019)
ok(all(c(cause_map[["cmd"]], cause_map[["dm2"]]) %in% precal$cause),
   "cmd & dm2 present via production fallback")
ok(precal[cause %in% c(cause_map[["cmd"]], cause_map[["dm2"]]), all(year <= 2019)],
   "cmd/dm2 only <=2019 in precalib (2020-2050 carried forward by alignment 05)")

# projection causes span 2000-2050 (2000-2019 prod + 2023-2050 projection; gap 2020-2022)
ihd_yrs <- sort(unique(precal[cause == cause_map[["ihd"]], year]))
ok(all(2000:2019 %in% ihd_yrs) && all(2023:2050 %in% ihd_yrs) &&
     !any(2020:2022 %in% ihd_yrs),
   "projection cause year set = {2000-2019} U {2023-2050}, gap 2020-2022")

cat("\n== Provenance summary ==\n")
prov_summary <- melt(built$provenance,
                     id.vars = c("location","year","age","sex","cause"),
                     variable.name = "field", value.name = "source")
print(prov_summary[, .N, by = .(field, source)][order(field, source)])

cat(sprintf("\n==== %d PASS / %d FAIL ====\n", npass, nfail))
if (nfail > 0) quit(status = 1L)
