

#...........................................................
## BP Control ----
#...........................................................


get.bp.prob<-function(DT, salteff, saltmet, saltyear1, saltyear2, rx, drugaroc){
  
  if(rx==1 & drugaroc =="baseline"){
    DT[,covinc:=aroc]
    DT[,covtrt:=aroc]
    #DT[,target_year:=ifelse(reach_base>2022, reach_base, 2022)]
  }
  
  if(rx==1 & drugaroc=="p75"){
    DT[,covinc:=htn_ctrl]
    DT[,covtrt:=htn_trt-htn_ctrl]
    #DT[,target_year:=ifelse(refwsalt>2022, refwsalt, 2022)]
  }
  
  if(rx==1 & drugaroc=="p975"){
    DT[,covinc:=htn_ctrl]
    DT[,covtrt:=htn_trt-htn_ctrl]
    #DT[,target_year:=ifelse(aspwsalt>2022, aspwsalt, 2022)]
  }
  
  if(rx==1 & drugaroc=="ideal"){
    DT[,covinc:=htn_ctrl]
    DT[,covtrt:=htn_trt-htn_ctrl]
    #DT[,target_year:=2030]
  }
  
  else{}
  
  
  #make salt variable represent salt gap
  if(saltmet=="percent"){
    DT[,salt_target:=salt*(1-salteff)]
    DT[salt_target<5.04, salt_target:=5.04]
    DT[salt<5.04, salt:=0]
    DT[salt>0,salt:=salt-salt_target]
    DT[salt<0, salt:=0]
  }
  
  if(saltmet=="target"){
    DT[,salt:=salt-salteff]
    DT[salt<0, salt:=0]
  }
  
  if(saltmet=="app"){
    DT[,salt:=salteff]
  }
  
  else{}
  
  if(salteff!=0){
    DT[Year>=saltyear1 & Year<=saltyear2, Mean:=Mean-(((1.12*raisedBP)+((1-raisedBP)*0.58))*salt*(Year-saltyear1+1)/(saltyear2-saltyear1+1))]
    DT[Year>saltyear2, Mean:=Mean-(((1.12*raisedBP)+((1-raisedBP)*0.58))*salt)]
  }
  
  else{}
  
  DT[bp_cat=="<120", prob:=pnorm(120,Mean,stdev)]
  DT[bp_cat=="120-129", prob:=pnorm(130,Mean,stdev)-pnorm(120,Mean,stdev)]
  DT[bp_cat=="130-139", prob:=pnorm(140,Mean,stdev)-pnorm(130,Mean,stdev)]
  DT[bp_cat=="140-149", prob:=pnorm(150,Mean,stdev)-pnorm(140,Mean,stdev)]
  DT[bp_cat=="150-159", prob:=pnorm(160,Mean,stdev)-pnorm(150,Mean,stdev)]
  DT[bp_cat=="160-169", prob:=pnorm(170,Mean,stdev)-pnorm(160,Mean,stdev)]
  DT[bp_cat=="170-179", prob:=pnorm(180,Mean,stdev)-pnorm(170,Mean,stdev)]
  DT[bp_cat=="180+", prob:=1-pnorm(180,Mean,stdev)]
  
  if(rx==1){
    
    #control
    DT[,shift:=prob*(covinc)] 
    DT[bp_cat=="<120" | bp_cat=="120-129" | bp_cat=="130-139", shift:=0]
    DT[, add130:=sum(shift*diabetes), by=.(age, sex, Year)]
    DT[, add140:=sum(shift*(1-diabetes)), by=.(age, sex, Year)]
    DT[,prob:=prob-shift]
    DT[bp_cat=="120-129", prob:=prob+add130]
    DT[bp_cat=="130-139", prob:=prob+add140]
    
    #treatment
    DT[,shift2:=ifelse(bp_cat=="<120" | bp_cat=="120-129", 0, prob*covtrt)]
    DT[,prob2:=prob+shift(shift2, type=c("lead")), by=.(age, sex, Year)]
    DT[bp_cat=="180+", prob2:=prob]
    DT[,prob2:=prob2-shift2]
    #DT[,check2:=sum(prob2), by=.(age, sex, Year)]
    DT[,prob:=prob2]
    
  }
  
  else{}
  
  DT[,c("age", "sex", "Year", "bp_cat" ,"prob", "location")]
  
}


#...........................................................
## TFA Policy ----
#...........................................................

# Parameters
RR_per_2_percent <- 1.28  # RR for 2% TFA increase
RR_per_1_percent <- RR_per_2_percent ^ 0.5  # RR for 1% TFA increase
target_tfa <- 0.5  # Target TFA intake (%E)
default_tfa <- 1.5  # Default TFA intake for "Unknown" values
default_mortality <- 5.0  # Default IHD mortality rate per 100,000

# Function to calculate mortality reduction
calc_mortality_reduction <- function(tfa_current, mortality_rate) {
  # Handle "Unknown" values
  if (tfa_current == "Unknown") {
    tfa_current <- default_tfa
  } else {
    # Handle ranges (e.g., "1.0-2.0") by taking the midpoint
    if (grepl("-", tfa_current)) {
      range_vals <- as.numeric(unlist(strsplit(tfa_current, "-")))
      tfa_current <- mean(range_vals)
    } else {
      # Handle cases like "0.5 (estimated)" or direct numbers
      tfa_current <- as.numeric(gsub("[^0-9.]", "", tfa_current))
    }
  }
  
  # Calculate change in TFA intake
  delta_tfa <- tfa_current - target_tfa
  if (delta_tfa <= 0) {
    return(0.0)  # No reduction if already below target
  }
  
  # Calculate adjusted relative risk and mortality reduction
  rr_adjusted <- RR_per_1_percent ^ delta_tfa
  rr_reduction <- 1 / rr_adjusted
  adjusted_mortality <- mortality_rate * rr_reduction
  reduction <- mortality_rate - adjusted_mortality
  return(reduction)
}

# Age Categories GBD-----

#...........................................................
## Central GBD age-band helper (SINGLE SOURCE for age banding) ----
#
# The GBD single-age <-> band mapping lives here ONLY and is driven by the age
# bounds declared in 00_run_model_cvd_fair.R (min_model_age / max_model_age).
# Every script that needs to (a) label a single-year age with its GBD band,
# (b) look up a band's interpolation midpoint, or (c) know a band's single-year
# members MUST call these helpers instead of hand-writing an age ladder. This
# removes the "20-24 years" / "95+" / "85plus" label mismatches that used to
# make merges on age_group silently fail.
#
# `label` strings match the raw GBD extract EXACTLY (so model output aggregated
# to bands joins the GBD targets). `midpt` is the single-age interpolation
# anchor (band centre; band lower+2 for the regular 5-year bands, 95 for the
# open 95+ group). If a future GBD extract uses different band labels, edit the
# `label` column here to match the new extract.
#...........................................................

gbd_age_bands <- function(min_age = 0L, max_age = 95L) {
  bands <- data.table::data.table(
    label  = c("<1 year", "12-23 months", "2-4 years",
               "5-9 years", "10-14 years", "15-19 years",
               "20-24 years","25-29 years","30-34 years","35-39 years",
               "40-44 years","45-49 years","50-54 years","55-59 years",
               "60-64 years","65-69 years","70-74 years","75-79 years",
               "80-84 years","85-89 years","90-94 years","95+ years"),
    age_lo = c(0L, 1L, 2L, 5L, 10L, 15L, seq(20L, 90L, by = 5L), 95L),
    age_hi = c(0L, 1L, 4L, 9L, 14L, 19L, seq(24L, 94L, by = 5L), Inf),
    midpt  = c(0, 1, 3, 7, 12, 17, seq(22, 92, by = 5), 95)
  )
  # keep only bands overlapping [min_age, max_age]; the terminal band is
  # open-ended, so cap its upper edge at max_age (which represents "max_age+").
  bands <- bands[age_hi >= min_age & age_lo <= max_age]
  bands[age_hi > max_age, age_hi := max_age]
  bands[age_lo < min_age, age_lo := min_age]
  bands[]
}

# Single-year age -> GBD band label (vectorised). Ages >= max_age map to the
# open-ended terminal band.
gbd_band_label <- function(age,
                           min_age = if (exists("min_model_age")) min_model_age else 0L,
                           max_age = if (exists("max_model_age")) max_model_age else 95L) {
  b   <- gbd_age_bands(min_age, max_age)
  idx <- findInterval(pmin(age, max_age), b$age_lo)  # b sorted ascending by age_lo
  b$label[idx]
}

# GBD band label -> interpolation midpoint (vectorised).
gbd_band_midpoint <- function(label,
                              min_age = if (exists("min_model_age")) min_model_age else 0L,
                              max_age = if (exists("max_model_age")) max_model_age else 95L) {
  b <- gbd_age_bands(min_age, max_age)
  b$midpt[match(label, b$label)]
}

# Backwards-compatible name kept for any caller of create_age_groups(): now
# returns the unified GBD band labels (covering the full 0:95 grid) instead of
# the old adult-only "20-24".."85plus" labels.
create_age_groups <- function(age) {
  factor(gbd_band_label(age), levels = gbd_age_bands()$label)
}