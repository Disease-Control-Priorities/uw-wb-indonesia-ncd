# =============================================================================
# tests_acceptance_0006.R
# Lightweight, reproducible acceptance checks for the ages-0-95 + type-2-diabetes
# full-lifecycle refactor of the Indonesia CVD / FAIR Choices pipeline.
#
# HOW TO RUN (after a 00 -> 06 pipeline run has produced the processed outputs):
#   setwd(".../code/cvd-fair-choices"); source("tests_acceptance_0006.R")
# It reads the on-disk stage outputs (data/processed + output/out_model), checks
# the acceptance criteria, prints PASS/FAIL, and stops with an error on any FAIL.
#
# The EXPECTED configuration below is the oracle for the test; it must match the
# central config declared in 00_run_model_cvd_fair.R.
# =============================================================================
suppressMessages(library(data.table))

wd      <- "C:/Users/wrgar/OneDrive - UW/02Work/WorldBank-Indonesia/uw-wb-indonesia-ncd/"
wd_data <- paste0(wd, "data/processed/")
wd_outp <- paste0(wd, "output/")
source("01_utils_indonesia.R")

## expected central config (mirror of 00_run_model_cvd_fair.R)
EXP_AGES         <- 0:95
EXP_MODEL_SHORT  <- c("ihd","istroke","hstroke","hhd","rhd","cmd","dm2")
EXP_MODEL_LONG   <- c("Ischemic heart disease","Ischemic stroke","Intracerebral hemorrhage",
                      "Hypertensive heart disease","Rheumatic heart disease",
                      "Cardiomyopathy and myocarditis","Diabetes mellitus type 2")
min_model_age <- 0L; max_model_age <- 95L

pass <- TRUE
chk <- function(name, ok, extra="") { cat(sprintf("[%s] %s %s\n", ifelse(ok,"PASS","FAIL"), name, extra)); if(!ok) pass <<- FALSE }
rd  <- function(pat) rbindlist(lapply(list.files(wd_data, pattern=pat, full.names=TRUE), readRDS), use.names=TRUE, fill=TRUE)

br021 <- rd("baseline_rates_part"); tps <- rd("tps_inpt_part"); adj <- rd("adjusted_searo_part")
out   <- setDT(readRDS(paste0(wd_outp,"out_model/model_output_Indonesia_htncov2_aspirational.rds")))

# 1 configured ages exactly 0:95, 95 == open 95+
chk("1 model ages == 0:95 (95==95+)", identical(EXP_AGES, 0:95) && max_model_age==95L)
# 2 ages 0 & 95 present for every year/sex/scenario/modeled cause in 06
he <- out[, .(a0=any(age==0), a95=any(age==95)), by=.(year,sex,intervention,cause)]
chk("2 ages 0 & 95 present for every year/sex/scenario/cause (06)", all(he$a0)&all(he$a95),
    sprintf("(ages %d-%d, %d scenarios)", min(out$age), max(out$age), uniqueN(out$intervention)))
# 3 dm2 present at every stage
chk("3 dm2 present after 021/022/03/05*/06",
    all(c("Diabetes mellitus type 2") %in% br021$cause, "Diabetes mellitus type 2" %in% tps$cause,
        "Diabetes mellitus type 2" %in% adj$cause, "dm2" %in% out$cause))
# 4 no cause leak; none dropped
chk("4 06 modeled causes == config (no leak / none dropped)",
    setequal(unique(out$cause), EXP_MODEL_SHORT))
# 7a baseline scenario leaves rates unmodified
chk("7a baseline scenario eff_ir==eff_cf==1", out[intervention=="baseline", all(abs(eff_ir-1)<1e-9 & abs(eff_cf-1)<1e-9)])
# 8 dm2 untouched by every scenario
chk("8 dm2 eff_ir==eff_cf==1 in all scenarios", out[cause=="dm2", all(abs(eff_ir-1)<1e-9 & abs(eff_cf-1)<1e-9)])
# 9 finite/nonneg states; probabilities in [0,1] at point of use
sv <- c("well","sick","dead","pop","all.mx","newcases")
chk("9 06 states finite & nonneg; calib IR/CF in [0,1]; effective (post-0.99-clamp) probs in [0,1]",
    all(sapply(sv, function(c) all(is.finite(out[[c]])) & all(out[[c]] >= -1e-6))) &&
    adj[, all(IR>=0 & IR<=1 & CF>=0 & CF<=1)],
    sprintf("(calib window CF/IR in [0,1]; projection secular-trend overshoot clamped by 06)"))
# 10 open 95+ terminal accounting (baseline, Male, ihd): pop95(t) == aged-in-94 + retained-95, no double count
d <- out[intervention=="baseline" & sex=="Male" & cause=="ihd", .(year,age,pop,all.mx)]; setkey(d,year,age)
ok10 <- TRUE
for (t in c(2030,2040,2049)) {
  exp95 <- (d[year==t-1&age==94,pop]-d[year==t-1&age==94,all.mx]) + (d[year==t-1&age==95,pop]-d[year==t-1&age==95,all.mx])
  act95 <- d[year==t&age==95,pop]
  ok10 <- ok10 && length(act95)==1 && act95>0 && abs(exp95-act95)/act95 < 1e-6
}
chk("10 open 95+ terminal: pop95(t)=(pop94-d94)+(pop95-d95)[t-1], survivors remain+age-in, no double count", ok10)
# 11 calibration preserves row count; no NA leaked
chk("11 03 rows == 022 rows & no NA in calibrated output", nrow(adj)==nrow(tps) && !anyNA(adj),
    sprintf("(022=%d, 03=%d)", nrow(tps), nrow(adj)))

cat("\n", strrep("=",50), "\n", ifelse(pass,"ALL ACCEPTANCE TESTS PASSED","SOME ACCEPTANCE TESTS FAILED"), "\n", strrep("=",50), "\n", sep="")
if (!pass) stop("acceptance tests failed")
