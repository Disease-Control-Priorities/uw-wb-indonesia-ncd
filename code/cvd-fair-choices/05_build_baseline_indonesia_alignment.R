# #############################################################################
# 05_build_baseline_indonesia_alignment.R
#
# ALIGNMENT-ONLY counterpart of 05_build_baseline_indonesia.R. Faithful copy of
# production Model 05 with the MINIMAL edits required to consume the alignment-
# calibrated transition probabilities (data/processed/alignment/
# adjusted_searo_alignment_part*.rds) instead of production adjusted_searo, so
# the projection's 2023-2050 epidemiology survives into `b_rates` for Model 06.
#
# Prerequisites (in memory): the Model-00 config (cause_map, wd*, min/max_model_age,
# gbd_* helpers, run_* flags) via align_bootstrap_config(), and the alignment
# Model 03 outputs on disk. Sourced AFTER 04_define_interventions_indonesia.R.
#
# EDITS vs production Model 05 (each marked  ## <ALIGNMENT EDIT>):
#   1. Load ONLY the alignment adjusted files by explicit path + specific
#      pattern -- never list.files(wd_data, "adjusted") which would also match
#      production adjusted_searo_part*.rds (the trap).
#   2. STASH the calibrated projection epidemiology (5 causes, 2023-2050) at load
#      and RESTORE it at the very end, so the year>2019 secular-trend blocks do
#      not overwrite the projection-derived / calibrated 2023-2050 rates.
#   3. Repeat-forward block fills ONLY the year cells MISSING per
#      (cause,age,sex,location) -- 2020-2022 for the 5 projection causes, and
#      2020-2050 for cmd/dm2 -- so the calibrated 2023-2050 rows are NOT
#      overwritten by a blanket "repeat 2019 into 2020:2050".
#   4. run_adjustment_model is FALSE (calibration already baked in).
# Everything else (covid/excess mortality, UNWPP population, BP data, HTN
# scale-up, background-mortality + case-fatality trends, cause coding) is
# preserved verbatim so `b_rates` is schema-identical to production Model 05.
# #############################################################################

run_adjustment_model <- FALSE                               ## <ALIGNMENT EDIT 4>
align_dir <- file.path(wd_data, "alignment")

#...........................................................
# add covid mx data ----
#...........................................................
setwd(wd_raw)
load(paste0(wd_data, "wpp.adj.Rda"))

wpp.adj <- wpp.adj %>%
  mutate(location_name = ifelse(location_name == "North Korea",
                                "Democratic People's Republic of Korea", location_name))
locs_wpp.adj <- unique(wpp.adj$location_name)

## <ALIGNMENT EDIT 1> load ONLY the alignment-calibrated rate files by explicit
## path + specific pattern (never a broad "adjusted" glob on wd_data).
files <- list.files(path = align_dir,
                    pattern = "^adjusted_searo_alignment_part[0-9]+\\.rds$",
                    full.names = TRUE)
if (!length(files))
  stop("alignment Model 05: no adjusted_searo_alignment_part*.rds in ", align_dir,
       " -- run 03_calibration_indonesia_alignment.R first.")
dt_list <- lapply(files, function(f) { dt <- readRDS(f); setDT(dt); dt })
b_rates <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
rm(dt_list, files)

## <ALIGNMENT EDIT 2> stash the calibrated projection epidemiology (5 projection
## causes, 2023-2050) with LONG names + PROJECTION Nx, before any Nx-overwriting
## merge or the secular-trend blocks. Restored at the very end.
proj_short  <- c("ihd", "istroke", "hstroke", "hhd", "rhd")
proj_long   <- unname(cause_map[proj_short])
cal_stash <- b_rates[year >= 2023 & year <= 2050 & cause %in% proj_long,
  .(location, year, age, sex, cause,
    IR_s = IR, CF_s = CF, BGmx_s = BG.mx, BGmxall_s = BG.mx.all,
    PREVt0_s = PREVt0, DISmxt0_s = DIS.mx.t0, ALLmx_s = ALL.mx, Nx_s = Nx)]

locs_b_rates <- unique(b_rates$location)
b_rates[location == "United States of America", location := "United States"]
b_rates[location == "Bolivia (Plurinational State of)", location := "Bolivia"]
b_rates[location == "United Republic of Tanzania", location := "Tanzania"]
b_rates <- b_rates[!is.na(location), ]
## drop percent_lag/percent_diff only if present (adjusted_searo has neither)
.pdrop <- intersect(c("percent_lag", "percent_diff"), names(b_rates))
if (length(.pdrop)) b_rates[, (.pdrop) := NULL]

b_rates <- left_join(b_rates, wpp.adj %>%
                       rename(location = location_name) %>%
                       select(-Nx, -mx, -iso3))

# Update to UNWPP 2024
dt_pop_unwpp <- as.data.table(readRDS(paste0(wd_data, "PopulationsSingleAge0050.rds")))
dt_pop_unwpp[, age := age - 1L]
dt_pop_unwpp[age >= max_model_age, age := max_model_age]
setnames(dt_pop_unwpp, c("year_id"), c("year"))
dt_pop_unwpp <- dt_pop_unwpp[age >= min_model_age, .(Nx = sum(Nx)), by = .(location, year, sex, age)]

b_rates <- merge(b_rates, dt_pop_unwpp[, .(location, year, age, sex, Nx2 = Nx)],
                 by = c("location", "year", "age", "sex"), all.x = TRUE)
b_rates <- b_rates[, Nx := ifelse(is.na(Nx2), Nx, Nx2)]
b_rates[, Nx2 := NULL]

locs <- unique(b_rates$location)

# Population age-20 from UNWPP
pop20 <- read.csv(paste0(wd_data, "PopulationsAge20_2050.csv"), stringsAsFactors = F)
b_rates <- left_join(b_rates, pop20 %>% rename(Nx2 = Nx, year = year_id) %>% filter(year >= 2017),
                     by = c("location", "year", "sex", "age")) %>%
  mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop = Nx) %>%
  select(-c(Nx2))

#...........................................................
# Blood Pressure data ----
#...........................................................
data.in <- fread(paste0(wd_data, "bp_data6.csv")) %>% rename(location = location_gbd) %>%
  select(-Year, -Country)

#...........................................................
# HTN add scale-up data ----
#...........................................................
inc <- read.csv(paste0(wd_data, "covfxn2.csv"), stringsAsFactors = F) %>%
  select(iso3, location, Year, aroc, p_change, a_change, refwsalt, aspwsalt, reach_base,
         aroc2, p_change2, a_change2, ideal)

bpcats <- c("<120", "120-129", "130-139", "140-149", "150-159", "160-169", "170-179", "180+")
data.in <- merge(bpcats, data.in) %>% rename(bp_cat = x)
data.in <- as.data.table(data.in)

name_map <- c(
  "Brunei" = "Brunei Darussalam", "Cape Verde" = "Cabo Verde",
  "Cote d'Ivoire" = "Ivory Coast", "Czech Republic" = "Czechia",
  "Federated States of Micronesia" = "Micronesia (Federated States of)",
  "Iran" = "Iran (Islamic Republic of)", "Laos" = "Lao People's Democratic Republic",
  "Macedonia" = "North Macedonia", "Moldova" = "Republic of Moldova",
  "South Korea" = "Republic of Korea", "Swaziland" = "Eswatini",
  "Syria" = "Syrian Arab Republic", "The Bahamas" = "Bahamas",
  "The Gambia" = "Gambia", "Venezuela" = "Venezuela (Bolivarian Republic of)",
  "Vietnam" = "Viet Nam", "North Korea" = "Democratic People's Republic of Korea")
data.in[, location := fcoalesce(name_map[location], location)]
inc <- as.data.table(inc)
inc[, location := fcoalesce(name_map[location], location)]
locs_data.in <- unique(data.in$location)

#rebalance TPs w/ covid such that they sum to less than 1
b_rates[, check_well := BG.mx + covid.mx + IR]
b_rates[, check_sick := BG.mx + covid.mx + CF]
b_rates[check_well > 1 | check_sick > 1, covid.mx := ifelse(1 - BG.mx < covid.mx, 1 - BG.mx, covid.mx)]
b_rates[check_well > 1, covid.mx := covid.mx - covid.mx * (check_well - 1) / (covid.mx + BG.mx + IR)]
b_rates[check_well > 1, BG.mx    := BG.mx    - BG.mx *    (check_well - 1) / (covid.mx + BG.mx + IR)]
b_rates[check_well > 1, IR       := IR       - IR *       (check_well - 1) / (covid.mx + BG.mx + IR)]
b_rates[, check_well := BG.mx + covid.mx + IR]
b_rates[check_sick > 1, covid.mx := covid.mx - covid.mx * (check_sick - 1) / (covid.mx + BG.mx + CF)]
b_rates[check_sick > 1, BG.mx    := BG.mx    - BG.mx *    (check_sick - 1) / (covid.mx + BG.mx + CF)]
b_rates[check_sick > 1, CF       := CF       - CF *       (check_sick - 1) / (covid.mx + BG.mx + CF)]
b_rates[, check_sick := BG.mx + covid.mx + CF]

#...........................................................
### fxn ----
#...........................................................
repYear <- function(row) { 2017 + floor((row - 1) / 224) }
data.in <- data.table(data.in %>% select(-age) %>% rename(age = Age.group))
b_rates[, newcases := 0]

## <ALIGNMENT EDIT 3> repeat 2019 forward only into MISSING year-cells per
## (cause,age,sex,location): 2020-2022 for the 5 projection causes (2023-2050
## already carry the calibrated rates), 2020-2050 for cmd/dm2. Replaces the
## production blanket `for (i in 2020:2050) bind rep(year==2019)`.
rep <- b_rates[year == 2019]
present_cells <- unique(b_rates[year >= 2020 & year <= 2050, .(cause, age, sex, location, year)])
fill_list <- vector("list", length(2020:2050)); k <- 0L
for (i in 2020:2050) {
  k <- k + 1L
  ri <- copy(rep)[, year := i]
  ri <- ri[!present_cells[year == i], on = .(cause, age, sex, location)]  # anti-join
  fill_list[[k]] <- ri
}
b_rates <- rbindlist(c(list(b_rates), fill_list), use.names = TRUE, fill = TRUE)

# rename causes to abbreviated codes (long GBD name -> short code)
cause_lookup <- setNames(names(cause_map), cause_map)
b_rates[, cause := fcoalesce(cause_lookup[cause], cause)]

#...........................................................
# Adjustments ---- (skipped: run_adjustment_model == FALSE for alignment)
#...........................................................
if (run_adjustment_model == TRUE) {
  adjustments <- fread(file = paste0(wd_data, "adjustments2023_age.csv"))
  adjustments <- adjustments[, c("location","sex","cause","age_group","IRadjust","CFadjust"), with = FALSE]
  b_rates[, age_group := as.character(gbd_band_label(age))]
  b_rates <- merge(b_rates, adjustments, by = c("location","sex","cause","age_group"), all.x = T)
  b_rates[, age_group := NULL]
  b_rates[!is.na(IRadjust), IR := IR * IRadjust]
  b_rates[!is.na(CFadjust), CF := CF * CFadjust]
  b_rates[, c("IRadjust", "CFadjust") := NULL]
}

# UNWPP 2024 Pop -- adjust pop 20 to unwpp
b_rates <- left_join(b_rates, pop20 %>% rename(Nx2 = Nx, year = year_id) %>% filter(year >= 2017),
                     by = c("location", "year", "sex", "age")) %>%
  mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop = Nx) %>%
  select(-c(Nx2))

# Covid 2020/2021 (refresh covid.mx by year after the carry-forward fill)
b_rates[, covid.mx := NULL]
b_rates <- merge(b_rates, wpp.adj[, c("location_name","year","sex","age","covid.mx"), with = F],
                 by.x = c("location","year","sex","age"),
                 by.y = c("location_name","year","sex","age"), all.x = T)
b_rates[is.na(covid.mx), covid.mx := 0]
b_rates[covid.mx >= 1, covid.mx := 0.9]

#...........................................................
# Mortality downward trends ----
#...........................................................
if (run_bgmx_trend == TRUE) {
  bgmx_fcst <- readRDS(file = paste0(wd_data, "tps_bgmx_forecasted.rds"))
  bgmx_fcst[, BG.mx.all := NULL]
  bgmx_fcst <- bgmx_fcst[year > 2019, ]
  bgmx_fcst <- unique(bgmx_fcst, by = c("age","sex","cause","year"))
  bgmx_fcst[, cause := fcoalesce(cause_lookup[cause], cause)]
  b_rates <- merge(b_rates, bgmx_fcst, , by = c("age","sex","cause","year"), all.x = T)
  b_rates[year > 2019 & !is.na(percent_diff), BG.mx := BG.mx * (1 + percent_diff)]
  b_rates[, c("percent_lag","percent_diff") := NULL]

  bgmx_fcst <- readRDS(file = paste0(wd_data, "tps_bgmx_all_forecasted.rds"))
  bgmx_fcst <- bgmx_fcst[year > 2019, ]
  bgmx_fcst[, BG.mx.all := NULL]
  bgmx_fcst <- unique(bgmx_fcst, by = c("age","sex","cause","year"))
  bgmx_fcst[, cause := fcoalesce(cause_lookup[cause], cause)]
  b_rates <- merge(b_rates, bgmx_fcst, by = c("age","sex","cause","year"), all.x = T)
  b_rates[year > 2019 & !is.na(percent_diff), BG.mx.all := BG.mx.all * (1 + percent_diff)]
  b_rates[, c("percent_lag","percent_diff") := NULL]
}

if (run_CF_trend == TRUE) {
  if (run_CF_trend_ihme == TRUE) {
    bgmx_fcst <- readRDS(file = paste0(wd_data, "tps_bgmx_cvd_ihme.rds"))
    bgmx_fcst <- bgmx_fcst[year > 2019, ]
    bgmx_fcst <- unique(bgmx_fcst, by = c("cause","year"))
    b_rates <- merge(b_rates, bgmx_fcst, by = c("cause","year"), all.x = T)
    b_rates[year > 2019 & !is.na(percent_diff), CF := CF * (1 + percent_diff)]
    b_rates[, c("percent_diff") := NULL]
  } else {
    bgmx_fcst <- readRDS(file = paste0(wd_data, "tps_bgmx_cvd_forecasted.rds"))
    bgmx_fcst <- bgmx_fcst[year > 2019, ]
    bgmx_fcst[, BG.mx.all := NULL]
    bgmx_fcst <- unique(bgmx_fcst, by = c("age","sex","cause","year"))
    bgmx_fcst[, cause := fcoalesce(cause_lookup[cause], cause)]
    b_rates <- merge(b_rates, bgmx_fcst, by = c("age","sex","cause","year"), all.x = T)
    if (run_CF_trend_80 == TRUE) {
      b_rates[year > 2019 & !is.na(percent_diff), CF := CF * (1 + percent_diff * 0.8)]
    } else {
      b_rates[year > 2019 & !is.na(percent_diff), CF := CF * (1 + percent_diff)]
    }
    b_rates[, c("percent_lag","percent_diff") := NULL]
  }
}

## <ALIGNMENT EDIT 2 (restore)> put the calibrated projection epidemiology +
## projection Nx back for the 5 projection causes, 2023-2050, so the secular
## trends above did NOT alter the projection-derived / calibrated values.
cal_stash[, cause := fcoalesce(cause_lookup[cause], cause)]   # long -> short
b_rates[cal_stash, on = .(location, year, age, sex, cause), `:=`(
  IR = i.IR_s, CF = i.CF_s, BG.mx = i.BGmx_s, BG.mx.all = i.BGmxall_s,
  PREVt0 = i.PREVt0_s, DIS.mx.t0 = i.DISmxt0_s, ALL.mx = i.ALLmx_s,
  Nx = i.Nx_s, pop = i.Nx_s)]
# re-enforce the covid-inclusive constraints on the restored rows (covid.mx==0
# for 2023+, so this is a safety re-check, not a material change)
b_rates[, check_well := BG.mx + covid.mx + IR]
b_rates[, check_sick := BG.mx + covid.mx + CF]

#...........................................................
# Validation before returning b_rates ----
#...........................................................
# Transition-sum policy: the PROJECTION rows (5 causes x 2023-2050) MUST satisfy
# the competing-risk constraint (the calibration guarantees IR/CF+BG.mx<=1 and
# covid==0 there) -- a violation there is a real bug and stops the run. Other
# rows (pre-2023 history, cmd/dm2, and 2020/2021 covid rows carried forward then
# secular-trended) can exceed 1 in a few old-age cells EXACTLY as production
# Model 05 produces them (production applies trends after the covid rebalance with
# no re-check); Model 06 tolerates this by clamping sick2<0 to 0. We therefore
# REPORT those production-inherited cells rather than fail on them.
b_rates[, .cw := IR + BG.mx + covid.mx]
b_rates[, .cs := CF + BG.mx + covid.mx]
proj_mask <- b_rates$year >= 2023 & b_rates$year <= 2050 & b_rates$cause %in% proj_short
proj_viol <- b_rates[proj_mask & (.cw > 1 + 1e-6 | .cs > 1 + 1e-6)]
other_viol <- b_rates[!proj_mask & (.cw > 1 + 1e-6 | .cs > 1 + 1e-6)]

if (nrow(other_viol)) {
  cat(sprintf("[alignment 05] NOTE: %d non-projection cells have TP-sum>1 (production-inherited; Model 06 clamps). By cause/year band:\n",
              nrow(other_viol)))
  print(other_viol[, .(n = .N, max_cw = round(max(.cw),3), max_cs = round(max(.cs),3)),
                   by = .(cause, covid = covid.mx > 0, yr_band = fifelse(year <= 2019, "<=2019", ">=2020"))][order(-n)])
}
stopifnot(
  "duplicate (age,sex,cause,year,location) keys" =
    nrow(b_rates) == nrow(unique(b_rates[, .(age, sex, cause, year, location)])),
  "NA in IR/CF/BG.mx/covid.mx" =
    !anyNA(b_rates$IR) && !anyNA(b_rates$CF) && !anyNA(b_rates$BG.mx) && !anyNA(b_rates$covid.mx),
  "PROJECTION-row TP-sum > 1 (real bug)" = nrow(proj_viol) == 0L,
  "all 7 causes present"  = length(intersect(unique(b_rates$cause),
                              c("ihd","istroke","hstroke","hhd","rhd","cmd","dm2"))) == 7L,
  "years 2000-2050 span"  = b_rates[, min(year) <= 2000 && max(year) >= 2050])
b_rates[, c(".cw", ".cs") := NULL]

cat(sprintf("[alignment 05] b_rates ready: %d rows, %d causes, years %d-%d, cols: %s\n",
            nrow(b_rates), uniqueN(b_rates$cause), min(b_rates$year), max(b_rates$year),
            paste(names(b_rates), collapse = ", ")))
cat(sprintf("[alignment 05] projection epidemiology restored on %d cells (5 causes x 2023-2050).\n",
            nrow(cal_stash)))

# Clean up (keep b_rates, data.in, inc, repYear for Model 06 -- matches production
# 05, which also removes pop20; Model 06 does not use pop20).
rm(list = intersect(c("adjustments","bgmx_fcst","dt_pop_unwpp","wpp.adj","rep","pop20",
                      "cal_stash","fill_list","present_cells","dt_list"), ls()))
