# Required inputs----

#...........................................................
#add covid mx data ----
#...........................................................

setwd(wd_raw)
load(paste0(wd_data,"wpp.adj.Rda"))

wpp.adj<-wpp.adj%>%
  mutate(location_name = ifelse(location_name=="North Korea", "Democratic People's Republic of Korea", location_name))
#Covid mx ~= excess mortality

# check for names as GBD and UWPP 2024
locs_wpp.adj <- unique(wpp.adj$location_name)

#baseline rates calculated in file calibration:

files <- list.files(
  path       = wd_data, 
  pattern    = "adjusted", 
  full.names = TRUE
)

dt_list <- lapply(files, function(f) {
  dt <- readRDS(f)
  setDT(dt)  # convert to data.table by reference if it isn't already
  dt
})

# Bind them all together, matching columns by name and filling missing ones
b_rates <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

rm(dt_list, files)

locs_b_rates <- unique(b_rates$location)
b_rates[location=="United States of America",location:="United States"]
b_rates[location=="Bolivia (Plurinational State of)",location:="Bolivia"]
b_rates[location=="United Republic of Tanzania",location:="Tanzania"]

b_rates <- b_rates[!is.na(location),]
b_rates[,c("percent_lag","percent_diff"):=NULL]

b_rates<-left_join(b_rates, wpp.adj%>%
                     rename(location = location_name)%>%
                     select( -Nx, -mx, -iso3))

# Update to UNWPP 2024
dt_pop_unwpp <- as.data.table(readRDS(paste0(wd_data,"PopulationsSingleAge0050.rds")))

## `age` in this file is a 1-based INDEX (index 1 == actual age 0, ...,
## index 101 == actual age 100+). Convert index -> actual age so it aligns with
## the GBD single-year ages used everywhere else, then pool all ages >=
## max_model_age into the open-ended terminal group (95+).
dt_pop_unwpp[, age := age - 1L]
dt_pop_unwpp[age >= max_model_age, age := max_model_age]

setnames(dt_pop_unwpp, c("year_id"), c("year"))

dt_pop_unwpp <- dt_pop_unwpp[age >= min_model_age, .(Nx = sum(Nx)), by = .(location, year, sex, age)]

b_rates <- merge(b_rates,
                 dt_pop_unwpp[, .(location,year,age,sex,Nx2=Nx)],
                 by = c("location", "year", "age","sex"),
                 all.x = TRUE
)

# replace Nx with Nx2
b_rates <- b_rates[, Nx := ifelse(is.na(Nx2), Nx, Nx2)]

b_rates[,Nx2 := NULL]

locs <- unique(b_rates$location)

#...........................................................
# Population data from UNWPP
pop20 <- read.csv(paste0(wd_data,"PopulationsAge20_2050.csv"), stringsAsFactors = F)

b_rates<-left_join(b_rates, pop20%>%rename(Nx2=Nx, year=year_id)%>%filter(year>=2017), 
                   by=c("location", "year", "sex", "age"))%>%
  mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop=Nx)%>%
  select(-c(Nx2))

#...........................................................
# Blood Pressure data ----
# blood pressure data calculated in file: "Blood pressure.R"
#...........................................................

data.in<-fread(paste0(wd_data,"bp_data6.csv"))%>%rename(location = location_gbd)%>%select(-Year, -Country)
# 
# data.in$salt[data.in$location=="China"]<-4.83*2.54
# length(unique(data.in$location))

#...........................................................
# HTN add scale-up data ----
#...........................................................

inc <- read.csv(paste0(wd_data,"covfxn2.csv"), stringsAsFactors = F)%>%
  select(iso3, location, Year, aroc, p_change, a_change, refwsalt, aspwsalt, reach_base,
         aroc2, p_change2, a_change2, ideal)

bpcats<-c("<120", "120-129", "130-139", 
          "140-149", "150-159", "160-169", 
          "170-179", "180+")

data.in<-merge(bpcats, data.in)%>%rename(bp_cat = x)

data.in <- as.data.table(data.in)
# Fixes location names

name_map <- c(
  "Brunei"                            = "Brunei Darussalam",
  "Cape Verde"                        = "Cabo Verde",
  "Cote d'Ivoire"                     = "Ivory Coast",
  "Czech Republic"                    = "Czechia",
  "Federated States of Micronesia"    = "Micronesia (Federated States of)",
  "Iran"                              = "Iran (Islamic Republic of)",
  "Laos"                              = "Lao People's Democratic Republic",
  "Macedonia"                         = "North Macedonia",
  "Moldova"                           = "Republic of Moldova",
  "South Korea"                       = "Republic of Korea",
  "Swaziland"                         = "Eswatini",
  "Syria"                             = "Syrian Arab Republic",
  "The Bahamas"                       = "Bahamas",
  "The Gambia"                        = "Gambia",
  "Venezuela"                         = "Venezuela (Bolivarian Republic of)",
  "Vietnam"                           = "Viet Nam",
  "North Korea"                       = "Democratic People's Republic of Korea"
)

# 3. update your data.in in place, using fcoalesce() so that
#    any location not in name_map stays unchanged
data.in[, location := fcoalesce(name_map[location], location)]

inc <- as.data.table(inc)
inc[, location := fcoalesce(name_map[location], location)]

unique(data.in$location)
any(is.na(data.in))

locs_data.in <- unique(data.in$location)

#? testing covid.x =0
#b_rates[covid.mx==0, covid.mx:=0]

#rebalance TPs w/ covid such that they sum to less than 1
#especially @ old ages where covid deaths are high
b_rates[,check_well := BG.mx+covid.mx+IR]
b_rates[,check_sick := BG.mx+covid.mx+CF]

#first ensure that background mortality + covid <1
b_rates[check_well>1 | check_sick>1, covid.mx:=ifelse(1-BG.mx<covid.mx, 1-BG.mx, covid.mx)]
#then proportionally reduce rates by check_well
b_rates[check_well>1, covid.mx:= covid.mx - covid.mx*(check_well-1)/(covid.mx+BG.mx+IR)]
b_rates[check_well>1, BG.mx   := BG.mx    - BG.mx*   (check_well-1)/(covid.mx+BG.mx+IR)]
b_rates[check_well>1, IR      := IR       - IR*      (check_well-1)/(covid.mx+BG.mx+IR)]

b_rates[,check_well := BG.mx+covid.mx+IR]
b_rates[check_well>1]

#same process for check_sick
b_rates[check_sick>1, covid.mx:= covid.mx - covid.mx*(check_sick-1)/(covid.mx+BG.mx+CF)]
b_rates[check_sick>1, BG.mx   := BG.mx    - BG.mx*   (check_sick-1)/(covid.mx+BG.mx+CF)]
b_rates[check_sick>1, CF      := CF       - CF*      (check_sick-1)/(covid.mx+BG.mx+CF)]

b_rates[,check_sick := BG.mx+covid.mx+CF]
b_rates[check_sick>1]

#check that no BG.mx.all+covid>1
b_rates[covid.mx+BG.mx.all>1]

#...........................................................
###fxn ----
#...........................................................

repYear<-function(row){
  2017+floor((row-1)/224)
}

data.in<-data.table(data.in%>%select(-age)%>%rename(age=Age.group))
b_rates[, newcases:=0]

##repeat rates for years 2020-2050
rep<-b_rates%>%filter(year==2019)

for (i in 2020:2050){
  b_rates<-bind_rows(b_rates, rep%>%mutate(year=i))
}

# # rename causes to match abbreviated names
# b_rates[,cause:=ifelse(cause=="Ischemic heart disease", "ihd",
#                        ifelse(cause=="Ischemic stroke", "istroke",
#                               ifelse(cause=="Intracerebral hemorrhage", "hstroke",
#                                      ifelse(cause=="Hypertensive heart disease", "hhd",
#                                             ifelse(cause=="Alzheimer's disease and other dementias", "aod",
#                                                    cause)))))]

# Build reverse lookup: full name -> abbreviation
cause_lookup <- setNames(names(cause_map), cause_map)

# Rename in place; unmapped causes keep their original name
b_rates[, cause := fcoalesce(cause_lookup[cause], cause)]

# #...........................................................
# # Adjustments ----
# #...........................................................
# ?? Adjustment of incidence rates and CF 

if(run_adjustment_model == TRUE) {
  
  adjustments <- fread(file = paste0(wd_data,"adjustments2023_age.csv"))
  
  adjustments <- adjustments[,c("location","sex","cause","age_group","IRadjust", "CFadjust"),with=FALSE]

  # Use the central GBD band helper (01_utils) so labels match everywhere and
  # cover the full 0-95 grid (was a local 20-95 "95+" ladder here).
  b_rates[,age_group := as.character(gbd_band_label(age))]
  # Adjustments for age group
  #b_rates <- merge(b_rates,adjustments,by=c("location","sex","cause"),all.x = T)
  b_rates <- merge(b_rates,adjustments,by=c("location","sex","cause","age_group"),all.x = T)
  
  b_rates[ , age_group:=NULL]
  
  b_rates[!is.na(IRadjust), IR:=IR * IRadjust]
  b_rates[!is.na(CFadjust), CF:=CF * CFadjust]
  
  b_rates[,c("IRadjust", "CFadjust"):=NULL]
  
}

# #...........................................................
# # UNWPP 2024 Pop ----
# #...........................................................
# Adjust pop 20 to unwpp

b_rates<-left_join(b_rates, pop20%>%rename(Nx2=Nx, year=year_id)%>%filter(year>=2017), 
                   by=c("location", "year", "sex", "age"))%>%
  mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop=Nx)%>%
  select(-c(Nx2))

# #...........................................................
# # Covid 2020/2021 ----
# #...........................................................

b_rates[,covid.mx:=NULL]
b_rates <- merge(b_rates,wpp.adj[,c("location_name","year","sex","age","covid.mx"),with=F],
                 by.x=c("location","year","sex","age"),
                 by.y=c("location_name","year","sex","age"),all.x=T)

b_rates[is.na(covid.mx), covid.mx:=0]
b_rates[covid.mx>=1, covid.mx:=0.9]


#...........................................................
# Mortality downward trends ----
#...........................................................

if(run_bgmx_trend == TRUE){
  
  bgmx_fcst <- readRDS(file = paste0(wd_data,"tps_bgmx_forecasted.rds"))
  
  bgmx_fcst[,BG.mx.all:=NULL]
  
  bgmx_fcst <- bgmx_fcst[year>2019,]
  
  bgmx_fcst <- unique(bgmx_fcst,by=c("age","sex","cause","year"))
  
  ## Long GBD name -> short code for ALL modeled causes via the central
  ## cause_lookup (defined above). Replaces a hand-written fcase that omitted
  ## rhd/cmd/dm2 and therefore silently applied NO secular trend to them. Per the
  ## model config, the secular background-mortality / case-fatality trends now
  ## apply to every modeled cause.
  bgmx_fcst[, cause := fcoalesce(cause_lookup[cause], cause)]
  
  summary(b_rates$BG.mx)
  
  b_rates <- merge(b_rates,bgmx_fcst,,by=c("age","sex","cause","year"),all.x = T)
  
  b_rates[year>2019 & !is.na(percent_diff),BG.mx:=BG.mx*(1+percent_diff)]
  b_rates[,c("percent_lag","percent_diff"):=NULL]
  
  summary(b_rates$BG.mx)
  
  # All dead envelope
  bgmx_fcst <- readRDS(file = paste0(wd_data,"tps_bgmx_all_forecasted.rds"))
  
  bgmx_fcst <- bgmx_fcst[year>2019,]
  
  bgmx_fcst[,BG.mx.all:=NULL]
  
  bgmx_fcst <- unique(bgmx_fcst,by=c("age","sex","cause","year"))
  
  ## Long GBD name -> short code for ALL modeled causes via the central
  ## cause_lookup (defined above). Replaces a hand-written fcase that omitted
  ## rhd/cmd/dm2 and therefore silently applied NO secular trend to them. Per the
  ## model config, the secular background-mortality / case-fatality trends now
  ## apply to every modeled cause.
  bgmx_fcst[, cause := fcoalesce(cause_lookup[cause], cause)]
  
  summary(b_rates$BG.mx.all)
  
  b_rates <- merge(b_rates,bgmx_fcst,by=c("age","sex","cause","year"),all.x = T)
  
  b_rates[year>2019 & !is.na(percent_diff),BG.mx.all:=BG.mx.all*(1+percent_diff)]
  b_rates[,c("percent_lag","percent_diff"):=NULL]
  
  summary(b_rates$BG.mx.all)
}

## Adjusting also CF with downward trend

if(run_CF_trend== TRUE){
  
  if(run_CF_trend_ihme== TRUE){
    
    bgmx_fcst <- readRDS(file = paste0(wd_data,"tps_bgmx_cvd_ihme.rds"))
    
    bgmx_fcst <- bgmx_fcst[year>2019,]
    
    bgmx_fcst <- unique(bgmx_fcst,by=c("cause","year"))
    
    b_rates <- merge(b_rates,bgmx_fcst,by=c("cause","year"),all.x = T)
    
    b_rates[year>2019 & !is.na(percent_diff),CF:=CF*(1+percent_diff)]
    b_rates[,c("percent_diff"):=NULL]
    
  }else{
    
    # All dead envelope
    #bgmx_fcst <- readRDS(file = paste0(wd_data,"tps_bgmx_all_forecasted.rds"))
    bgmx_fcst <- readRDS(file = paste0(wd_data,"tps_bgmx_cvd_forecasted.rds"))
    
    bgmx_fcst <- bgmx_fcst[year>2019,]
    
    bgmx_fcst[,BG.mx.all:=NULL]
    
    bgmx_fcst <- unique(bgmx_fcst,by=c("age","sex","cause","year"))
    
    ## Long GBD name -> short code for ALL modeled causes (see note above).
    bgmx_fcst[, cause := fcoalesce(cause_lookup[cause], cause)]
    
    
    b_rates <- merge(b_rates,bgmx_fcst,by=c("age","sex","cause","year"),all.x = T)
    
    if(run_CF_trend_80 == TRUE){
      b_rates[year>2019 & !is.na(percent_diff),CF:=CF*(1+percent_diff*0.8)]
    }else{
      b_rates[year>2019 & !is.na(percent_diff),CF:=CF*(1+percent_diff)]
    }
    
    b_rates[,c("percent_lag","percent_diff"):=NULL]
    
  }
  
}

#...........................................................
# Sick -> dead baseline sanity guard (generic, driven by selected links) ----
#...........................................................
# Any intervention that acts on a sick -> dead (case-fatality) transition needs
# the target cause to carry a valid baseline sick stock (PREVt0, seeded as
# sick = Nx*PREVt0 in Model 06) and case fatality (CF), so the effect has
# something to act on. The set of target causes is derived from the SELECTED,
# RUNNABLE case-fatality links in the Model 04 catalogues -- i.e. it honours the
# workbook include_flag. A cause is required here only if a currently active
# intervention actually targets its sick -> dead transition; an excluded
# intervention (e.g. an SSB -> diabetes mortality link flagged 0) imposes no
# requirement. Fail loud rather than emit NA/all-zero sick or CF. Non-analytic.
.sick_dead_target_codes <- character(0)
for (.obj in c("public_health_inputs", "fair_inputs")) {
  if (exists(.obj) && !is.null(get(.obj)) && !is.null(get(.obj)$valid_links)) {
    .vl <- as.data.table(get(.obj)$valid_links)
    if (all(c("model_transition", "cause_code") %in% names(.vl)))
      .sick_dead_target_codes <- c(
        .sick_dead_target_codes,
        .vl[model_transition == "case_fatality", unique(as.character(cause_code))])
  }
}
.sick_dead_target_codes <- unique(.sick_dead_target_codes[
  !is.na(.sick_dead_target_codes) & nzchar(.sick_dead_target_codes)])
# Restrict to causes that are actually modeled in the baseline.
.sick_dead_target_codes <- intersect(.sick_dead_target_codes, unique(b_rates$cause))
if (length(.sick_dead_target_codes)) {
  for (.cc in .sick_dead_target_codes) {
    .sub <- b_rates[cause == .cc & year >= 2025]
    .bad_cf   <- anyNA(.sub$CF)     || all(.sub$CF     == 0, na.rm = TRUE)
    .bad_sick <- anyNA(.sub$PREVt0) || all(.sub$PREVt0 == 0, na.rm = TRUE)
    if (.bad_cf || .bad_sick)
      stop("Model 05: cause '", .cc, "' has NA/all-zero ",
           if (.bad_cf) "CF " else "", if (.bad_sick) "PREVt0 " else "",
           "-- a selected sick->dead intervention effect would have nothing to ",
           "act on. Check the calibration output and the secular-trend join.",
           call. = FALSE)
    cat(sprintf(paste0("Model 05: sick->dead target '%s' baseline OK ",
                       "(CF mean %.4g, max %.4g; PREVt0 mean %.4g).\n"),
                .cc, mean(.sub$CF, na.rm = TRUE), max(.sub$CF, na.rm = TRUE),
                mean(.sub$PREVt0, na.rm = TRUE)))
  }
  rm(.sub, .bad_cf, .bad_sick, .cc)
} else {
  cat("Model 05: no selected sick->dead intervention links; ",
      "no case-fatality-target baseline guard required.\n", sep = "")
}
rm(.sick_dead_target_codes)
suppressWarnings(rm(.obj, .vl))

# Clean up environment
rm("adjustments","bgmx_fcst","dt_pop_unwpp","wpp.adj","rep","pop20")


## HERE I Include a patch to align to an aggregate projection for alignment with the UNWPP 2024 population projections. 
# This is a temporary fix to ensure that the model's population projections are consistent with the official UNWPP data.

# Upload the handoff file with probablities 2025-2050 to replace the model's projections for Indonesia. 
# This file should contain the necessary adjustments to align the model's outputs with the UNWPP 2024 projections

dt_calibrated <- as.data.table(readRDS(paste0(wd_data,"transition_probabilities_indonesia_handoff.rds")))

# # write 2% sample csv files b_rated and dt_calibrated
# sample_b_rates <- b_rates[sample(.N, .N * 0.02)]
# sample_dt_calibrated <- dt_calibrated[sample(.N, .N * 0.02)]
# fwrite(sample_b_rates, paste0(wd_data,"sample_b_rates.csv"), row.names = FALSE)
# fwrite(sample_dt_calibrated, paste0(wd_data,"sample_dt_calibrated.csv"), row.names = FALSE)

#replacing model parametesr in b_rates with the calibrated values from dt_calibrated for Indonesia from2025-2050

#..............................................................................
# Replace Indonesia baseline parameters with calibrated values, 2025–2050 ----
#..............................................................................

setDT(b_rates)
setDT(dt_calibrated)

# Translate calibration cause IDs to the model's internal cause codes.
calibration_cause_map <- c(
  C_IHD = "ihd",
  C_IS  = "istroke",
  C_ICH = "hstroke",
  C_HHD = "hhd",
  C_RHD = "rhd",
  C_CMD = "cmd",
  C_DM  = "dm2"
)

dt_calibrated[, cause := unname(calibration_cause_map[cause_id])]

# Fail if the handoff contains an unrecognized cause.
unmapped_causes <- unique(
  dt_calibrated[
    location_name == "Indonesia" &
      between(year, 2025L, 2050L) &
      is.na(cause),
    cause_id
  ]
)

if (length(unmapped_causes) > 0L) {
  stop(
    "Model 05: unmapped cause_id values in the calibration handoff: ",
    paste(unmapped_causes, collapse = ", "),
    call. = FALSE
  )
}

# Prepare one calibrated row per model key.
calibrated_update <- dt_calibrated[
  location_name == "Indonesia" &
    between(year, 2025L, 2050L),
  .(
    location  = location_name,
    year      = as.integer(year),
    age       = as.integer(age),
    sex       = as.character(sex),
    cause,
    BG.mx.all = background_mx_all_modelled_causes,
    ALL.mx    = all_cause_mx,
    BG.mx     = background_mx_for_cause,
    IR,
    CF,
    Nx        = population,
    pop       = population
  )
]

calibration_keys <- c("location", "year", "age", "sex", "cause")

# Each calibrated parameter set must be unique.
duplicate_calibration_rows <- calibrated_update[
  duplicated(calibrated_update, by = calibration_keys) |
    duplicated(calibrated_update, by = calibration_keys, fromLast = TRUE)
]

if (nrow(duplicate_calibration_rows) > 0L) {
  stop(
    "Model 05: the calibration handoff contains duplicate rows for ",
    "location-year-age-sex-cause.",
    call. = FALSE
  )
}

# Ensure every calibration row has a corresponding row in b_rates.
unmatched_calibration_rows <- calibrated_update[
  !b_rates,
  on = calibration_keys
]

if (nrow(unmatched_calibration_rows) > 0L) {
  stop(
    "Model 05: ", nrow(unmatched_calibration_rows),
    " calibrated Indonesia rows could not be matched to b_rates. ",
    "Check location, year, age, sex, and cause identifiers.",
    call. = FALSE
  )
}

# Update by reference. Because calibrated_update contains only 2025–2050,
# observations before 2025 cannot be modified.
b_rates[
  calibrated_update,
  on = calibration_keys,
  `:=`(
    BG.mx.all = i.BG.mx.all,
    ALL.mx    = i.ALL.mx,
    BG.mx     = i.BG.mx,
    IR        = i.IR,
    CF        = i.CF,
    Nx        = i.Nx,
    pop       = i.pop
  )
]

# Refresh diagnostic transition sums after replacing the parameters.
b_rates[
  location == "Indonesia" & between(year, 2025L, 2050L),
  `:=`(
    check_well = BG.mx + covid.mx + IR,
    check_sick = BG.mx + covid.mx + CF
  )
]

# Validate the updated transition probabilities.
invalid_calibrated_rows <- b_rates[
  location == "Indonesia" &
    between(year, 2025L, 2050L) &
    (
      is.na(BG.mx) | is.na(BG.mx.all) | is.na(IR) | is.na(CF) |
        IR < 0 | IR >= 1 |
        CF < 0 | CF >= 1 |
        check_well > 1 |
        check_sick > 1
    )
]

# if (nrow(invalid_calibrated_rows) > 0L) {
#   stop(
#     "Model 05: ", nrow(invalid_calibrated_rows),
#     " Indonesia rows have invalid calibrated transition probabilities.",
#     call. = FALSE
#   )
# }
# 
# cat(
#   sprintf(
#     paste0(
#       "Model 05: updated %,d Indonesia age-sex-cause-year rows ",
#       "with calibrated parameters for 2025–2050; pre-2025 data preserved.\n"
#     ),
#     nrow(calibrated_update)
#   )
# )

rm(
  calibration_cause_map,
  calibration_keys,
  calibrated_update,
  duplicate_calibration_rows,
  invalid_calibrated_rows,
  unmatched_calibration_rows,
  unmapped_causes
)

