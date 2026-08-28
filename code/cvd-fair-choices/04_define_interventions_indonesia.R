
#...........................................................
# Sodium data ----
#...........................................................

## Salt data

# New IHME salt data

dt_sodium_ihme <- fread(paste0(wd_raw,"RiskFactors/","IHME_GBD_2023_DIET_RISK_1990_2024_SODIUM_Y2024M11D05.csv"))

dt_sodium_ihme <- dt_sodium_ihme[,c("year_id","location_name","sex_name","age_group_name","val","upper","lower","age_group_id"),with = F]

setnames(dt_sodium_ihme,c("year_id","location_name","sex_name","age_group_name","val","upper","lower"),
         c("year","location","sex","age","sodium_current","sodium_current_upper","sodium_current_lower"))

dt_sodium_ihme <- dt_sodium_ihme[location!="Global",]

name_map <- c(
  "United States of America"          = "United States",
  "Taiwan"                            = "Taiwan (Province of China)"
)

dt_sodium_ihme[, location := fcoalesce(name_map[location], location)]


# Only covers from 25 years and older, so imputing for 20-24 yearsas 25-29 years

dt_sodium_ihme_20 <- dt_sodium_ihme[age == "25 to 29",]
dt_sodium_ihme_20[, age := "20 to 24"]
dt_sodium_ihme <- rbind(dt_sodium_ihme, dt_sodium_ihme_20)

rm(dt_sodium_ihme_20)


dt_sodium_ihme <- dt_sodium_ihme[sex!="Both",]

# Fix countries names
dt_sodium_ihme[location == "Türkiye", location := "Turkey"]
dt_sodium_ihme[location == "Côte d'Ivoire", location := "Ivory Coast"]

# Filter to select Indonesia
dt_sodium_ihme <- dt_sodium_ihme[location=="Indonesia",]

# Save the data
saveRDS(dt_sodium_ihme, file = paste0(wd_data, "sodium_data.rds"))

# read the data
dt_sodium <- readRDS(file = paste0(wd_data,"sodium_data.rds"))

# Computing mean intake per location

dt_pop <- readRDS(file = paste0(wd_raw,"GBD/","totalpop_ihme.rds"))

# rename locations to match gbd locations (the baseline)
name_map <- c(
  "United States of America"          = "United States",
  "Ukraine (without Crimea & Sevastopol)" = "Turkey")

dt_pop[, location := fcoalesce(name_map[location], location)]

# Average population by age group
dt_pop <- dt_pop[,list(population=sum(val)),by=list(location,sex_name,year_id,age_group)]

dt_pop[,age_group_id:=age_group]

dt_pop[,sex:=sex_name]
dt_pop[,year:=year_id]

# Merge population data with sodium data
dt_sodium <- merge(dt_sodium,dt_pop,
                   by = c("location","sex","age_group_id","year"), all.x = TRUE)

# keep year >2000
dt_sodium <- dt_sodium[year >= 2000,]

dt_sodium <- dt_sodium[!is.na(population),]

# compute mean sodium intake per location
dt_sodium_mean <- dt_sodium[, .(sodium_current = weighted.mean(sodium_current,population, na.rm = TRUE),
                                salt = min(sodium_current*2.54,na.rm=T)),
                            by = list(year,location)]

dt_sodium_mean <- dt_sodium_mean[year >= 2019,]

# Create scenarios for 2025-2050

dt_sodium_mean_24 <- dt_sodium_mean[year==2024,]

years <- 2025:2050
dt_sodium_scenarios <- dt_sodium_mean_24[, {
  # create a fresh table of years
  yearly_data <- data.table(year = years)
  # assign the constant values of sodium_current & salt for this 'location' group
  yearly_data[, `:=`(
    sodium_current = sodium_current,
    salt           = salt
  )]
  yearly_data
}, by = location]

dt_sodium_scenarios <- rbind(dt_sodium_mean,dt_sodium_scenarios,fill=T)

# Scenarios: reduce 15% by 2030, 30% by 2030
# interpolate the target values for the scenarios linearly

interpolate_sodium_reduction <- function(dt,
                                         current_col       = "sodium_current",
                                         out_col           = "sodium_target_prog",
                                         transition_start  = 2025L,
                                         transition_end    = 2030L,
                                         target_fraction   = 0.85) {
  stopifnot(is.data.table(dt))
  
  # 1) Before the transition, keep at current
  dt[
    year < transition_start,
    (out_col) := get(current_col)
  ]
  
  # 2) Between transition_start and transition_end, ramp DOWN
  dt[
    year >= transition_start & year <= transition_end,
    (out_col) := get(current_col) +
      (get(current_col) * target_fraction - get(current_col)) *
      (year - transition_start) / (transition_end - transition_start)
  ]
  
  # 3) After transition_end, hold at the reduced level
  dt[
    year > transition_end,
    (out_col) := get(current_col) * target_fraction
  ]
  
  invisible(dt)
}

interpolate_sodium_reduction(
  dt = dt_sodium_scenarios,
  current_col      = "sodium_current",
  out_col          = "sodium_target_prog",
  transition_start = 2025L,
  transition_end   = 2030L,
  target_fraction  = 0.85
)

interpolate_sodium_reduction(
  dt = dt_sodium_scenarios,
  current_col      = "sodium_current",
  out_col          = "sodium_target_asp",
  transition_start = 2025L,
  transition_end   = 2030L,
  target_fraction  = 0.7
)

interpolate_sodium_reduction(
  dt = dt_sodium_scenarios,
  current_col      = "salt",
  out_col          = "salt_target_prog",
  transition_start = 2025L,
  transition_end   = 2030L,
  target_fraction  = 0.85
)

interpolate_sodium_reduction(
  dt = dt_sodium_scenarios,
  current_col      = "salt",
  out_col          = "salt_target_asp",
  transition_start = 2025L,
  transition_end   = 2030L,
  target_fraction  = 0.7
)

setnames(dt_sodium_scenarios,"salt","salt_current")

# remove missing salt current in not included territories
dt_sodium_scenarios <- dt_sodium_scenarios[!is.na(salt_current),]

dt_sodium_scenarios[location=="Bolivia (Plurinational State of)", location := "Bolivia"]
dt_sodium_scenarios[location=="United Republic of Tanzania", location := "Tanzania"]

# Save the data
saveRDS(dt_sodium_scenarios,file = paste0(wd_data,"sodium_policy_scenarios.rds"))

# remove objects that are no longer needed
rm(dt_sodium, dt_pop, dt_sodium_mean, dt_sodium_mean_24,
   years, dt_sodium_scenarios, interpolate_sodium_reduction,
   name_map, dt_check,age_match)

#...........................................................
# TFA Policy----
#...........................................................

# Input from IHME
dt_tfa <- fread(paste0(wd_raw,"RiskFactors/","IHME_GBD_2023_DIET_RISK_1990_2024_TRANSFAT_Y2024M11D05",".csv"))

dt_tfa <- dt_tfa[,c("year_id","location_name","sex_name","age_group_name","val","upper","lower","age_group_id"),with = F]

setnames(dt_tfa,c("year_id","location_name","sex_name","age_group_name","val","upper","lower"),
         c("year","location","sex","age","tfa_current","tfa_current_upper","tfa_current_lower"))

dt_tfa <- dt_tfa[location!="Global",]

# rename locations
dt_tfa[location == "Türkiye", location := "Turkey"]
dt_tfa[location == "Côte d'Ivoire", location := "Ivory Coast"]

# Only covers from 25 years and older, so imputing for 20-24 yearsas 25-29 years

dt_tfa_20 <- dt_tfa[age == "25 to 29",]
dt_tfa_20[, age := "20 to 24"]
dt_tfa <- rbind(dt_tfa, dt_tfa_20)

rm(dt_tfa_20)

# Countries with BPP

dt_tfa_bpp <- as.data.table(read_excel(paste0(wd_raw,"RiskFactors/", "List of Countries with Policies Passed.xlsx"), 
                                       sheet = "Sheet1", range = "A3:C53")) 

# rename locations to match gbd locations (the baseline)
name_map <- c(
  "Czech Republic"                    = "Czechia",
  "Macedonia"                         = "North Macedonia",
  "Moldova"                           = "Republic of Moldova",
  "Cyprus"                            = "Republic of Cyprus")

dt_tfa_bpp[, Country := fcoalesce(name_map[Country], Country)]

#? Leichtein has no tfa data but policy
dt_tfa <- merge(dt_tfa, dt_tfa_bpp, by.x = "location", by.y= "Country", all.x = TRUE,all.y = F)

# check locations
dt_check <- dt_tfa[is.na(sex),]

setnames(dt_tfa, c("Date passed", "Date in Effect"),
         c("tfa_bpp_date_passed", "tfa_bpp_date_in_effect"))

#dt_tfa <- dt_tfa[, .(location, tfa_current, tfa_bpp_date_passed, tfa_bpp_date_in_effect)]

dt_tfa <- dt_tfa[sex!="Both",]

# Keep Indonesia
dt_tfa <- dt_tfa[location=="Indonesia",]
# Save the data
saveRDS(dt_tfa, file = paste0(wd_data, "tfa_data.rds"))

# TFA data
# as of 2026, Indonesia has not yet fully implemented a WHO “best-practice” industrial
#trans fat ban (such as a national ban on partially hydrogenated oils or a mandatory ≤2% 
# trans fat limit in all foods). However, the country has been moving toward stronger regulation.

dt_tfa <- readRDS(file = paste0(wd_data,"tfa_data.rds"))

name_map <- c(
  "United States of America"          = "United States",
  "Taiwan"                            = "Taiwan (Province of China)",
  "Urkraine (without Crimea & Sevastopol)" = "Ukraine")

# P target
p_tfa_target <- 0.00
dt_tfa[, location := fcoalesce(name_map[location], location)]

# Computing mean intake per location

dt_pop <- readRDS(file = paste0(wd_raw,"GBD/","totalpop_ihme.rds"))

dt_pop <- dt_pop[, .(location, year_id, sex_name, age_group, val),with=T]

setnames(dt_pop, c("val", "year_id","sex_name"),
         c("population","year","sex"))

name_map <- c(
  "United States of America"          = "United States",
  "Taiwan"                            = "Taiwan (Province of China)",
  "Urkraine (without Crimea & Sevastopol)" = "Ukraine")

dt_pop[, location := fcoalesce(name_map[location], location)]

dt_pop[, age:= fifelse(
  age_group >= 95, "95 plus",
  paste0(
    floor(age_group / 5) * 5, 
    " to ", 
    floor(age_group / 5) * 5 + 4
  )
)]

# Keep only ages 20+
dt_pop <- dt_pop[age_group >= 20]

# Sum population
dt_pop <- dt_pop[,list(population=mean(population)),by=list(location,sex,age,year)]

# Merge population data with TFA data
dt_tfa <- merge(dt_tfa,dt_pop,
                by = c("location","sex","age","year"), all.x = TRUE)
# Keep 2000+
dt_tfa <- dt_tfa[year >= 2000,]
# compute mean TFA intake per location
dt_tfa_mean <- dt_tfa[, .(tfa_current = weighted.mean(tfa_current,population, na.rm = TRUE),
                          tfa_bpp_date_in_effect = min(tfa_bpp_date_in_effect, na.rm = TRUE),
                          tfa_bpp_date_passed = min(tfa_bpp_date_passed, na.rm = TRUE)),
                      by = list(year,location)]

#dt_tfa_mean_24 <- dt_tfa_mean[year==2024,]

#Most recent data from 2024:2019
dt_tfa_mean_24 <- dt_tfa_mean[year %in% c(2024),]
dt_tfa_mean_24 <- dt_tfa_mean_24[order(dt_tfa_mean_24$tfa_current,decreasing = T),]
dt_tfa_mean_24 <- unique(dt_tfa_mean_24,by="location")

years <- 2025:2050
dt_tfa_scenarios <- dt_tfa_mean_24[, {
  yearly_data <- data.table(year = years)
  yearly_data[, tfa_current := tfa_current]
  yearly_data
}, by = location]

dt_tfa_scenarios <- rbind(dt_tfa_mean,dt_tfa_scenarios,fill=T)

# Add the tfa_target to the main data table

dt_tfa_scenarios[,c("tfa_bpp_date_in_effect","tfa_bpp_date_passed"):=NULL]
dt_tfa_scenarios <- merge(dt_tfa_scenarios, 
                          dt_tfa_mean_24[, .(location, tfa_bpp_date_in_effect,tfa_bpp_date_passed)],
                          by = "location", all.x = TRUE)

# Fix bpp date for specific locations
dt_tfa_scenarios[ , tfa_target := tfa_current]
dt_tfa_scenarios[ tfa_bpp_date_passed<=2023 & year>2023 & year<2025 , tfa_target := ifelse(tfa_current<p_tfa_target,tfa_current,p_tfa_target)]
#dt_tfa_scenarios[ year>=2025 , tfa_target := 0.005]
#dt_tfa_scenarios[ year>=2027 , tfa_target := ifelse(tfa_current<p_tfa_target,tfa_current,p_tfa_target)]

# ensure that once a country reaches 0, its tfa_target remain 0 thereafter.

# Set initial targets to current values
dt_tfa_scenarios[, tfa_target := tfa_current]

# If TFA policy is passed before or in 2023, drop to target (0) by 2024
dt_tfa_scenarios[
  tfa_bpp_date_passed <= 2023 & year >= 2024, 
  tfa_target := p_tfa_target
]

# For all countries, once TFA reaches 0, it cannot go up again
dt_tfa_scenarios[, tfa_target := 
                   ifelse(
                     year >= min(year[tfa_target == p_tfa_target], na.rm = TRUE),
                     p_tfa_target,
                     tfa_target
                   ),
                 by = location
]

# Optional: also keep tfa_current at 0 after a country hits 0
dt_tfa_scenarios[, tfa_current := 
                   ifelse(
                     year >= min(year[tfa_target == p_tfa_target], na.rm = TRUE),
                     p_tfa_target,
                     tfa_current
                   ),
                 by = location
]

dt_tfa_scenarios[ year>=2027 , tfa_target := ifelse(tfa_current<p_tfa_target,tfa_current,p_tfa_target)]

dt_tfa_scenarios <- dt_tfa_scenarios[, .(location, year, tfa_current,tfa_target)]

# Taiwan, Ukraine and other territories are NA, so assigning the minimum value
dt_tfa_scenarios[is.na(tfa_target),  tfa_target := p_tfa_target]
dt_tfa_scenarios[is.na(tfa_current), tfa_current := p_tfa_target]

# Check country names
dt_tfa_scenarios[, location := gsub("United States of America", "United States", location)]
dt_tfa_scenarios[location=="Bolivia (Plurinational State of)", location := "Bolivia"]

dt_tfa_scenarios[, location := gsub("Taiwan", "Taiwan (Province of China)", location)]

dt_tfa_scenarios[, location := gsub("United Republic of Tanzania", "Tanzania", location)]

# Fix countries names
dt_tfa_scenarios[location == "Türkiye", location := "Turkey"]
dt_tfa_scenarios[location == "Côte d'Ivoire", location := "Ivory Coast"]

#print length(locs_statins)
locs_tfa <- unique(dt_tfa_scenarios$location)
print(paste0("Number of locations with tfa data: ", length(locs_tfa)))

# Save the data
saveRDS(dt_tfa_scenarios,file = paste0(wd_data, "tfa_policy_scenarios.rds"))

# clean up
rm(dt_tfa, dt_pop, dt_tfa_mean, dt_tfa_mean_24,
   years, dt_tfa_scenarios, name_map, dt_check, age_match)

#...........................................................
# Statins/Lipid data ----
#...........................................................

# Targets

# US 100% of elegible adults, i.e 32% 40-75
#https://www.acc.org/Latest-in-Cardiology/Articles/2022/10/04/13/38/Comparing-Guideline-Recommendations-of-Statin-Use-For-the-Primary-Prevention-of-ASCVD?utm_source=chatgpt.com
# WHO 50% of elegible adults 40+
#https://cdn.who.int/media/docs/default-source/inaugural-who-partners-forum/gmf_indicator_definitions_version_nov2014438a791b-16d3-46f3-a398-88dee12e796b.pdf?sfvrsn=4b337764_1&utm_source=chatgpt.com

# Input from Polypill paper

# Depreccated, for the file that blended multiple pills to the one with just statins prediction
# dt_sta.1 <- fread(paste0(wd_raw,"Statins/","coverage_statins",".csv"))
# dt_sta$location <- dt_sta$location_name
# data.in.statin <- dt_sta[cause=="ihd",c("location","pp_cov"),with=F]

data.in.statin <- fread(paste0(wd_raw,"RiskFactors/","FDC_coverage_data_statints_pp",".csv"))

data.in.statin[location == "Côte d'Ivoire", location := "Ivory Coast"]

# # Diabetes prevalence
# data.in.statin <- fread(paste0(wd,"bp_data6.csv"))%>%rename(location = location_gbd)%>%select(-Year, -Country)
# 
# data.in.statin <- unique(data.in.statin[,c("location","age","sex","diabetes"),with=F],by=c("location","age","sex"))
# 
# data.in.statin <- merge(data.in.statin, 
#                         dt_sta,
#                         by=c("location"), all.x=TRUE)

# ?? Subset for peoble 40+ 55-80 from SPC paper
#data.in.statin <- data.in.statin[age >= 40]

#setnames(data.in.statin,"age", "age_group")

# Age groups + merge on statin coverage info
#data.in.statin[, age_group := create_age_groups(age_group)]

# Current vs target coverage
data.in.statin[, statins_current := ifelse(is.na(pp_cov),0,pp_cov)]
data.in.statin[, statins_target  := 0.60]  # assuming progress scenario
#data.in.statin[diabetes > statins_target, statins_target  := diabetes]  # assuming diabetes defines the target
data.in.statin[statins_current > statins_target, statins_target  := statins_current]  # assuming diabetes defines the target

data.in.statin[,c("pp_cov","diabetes"):=NULL]

# Scaling up scenarios
# ? Scenario by age

# ── function to expand & interpolate ─────────────────────────────────────
interpolate_statins <- function(dt_raw,
                                start_year = 2025L,
                                end_year   = 2050L,
                                current_col = "statins_current",
                                target_col  = "statins_target",
                                out_col     = "statins_uptake") {
  
  stopifnot(is.data.table(dt_raw))
  yrs <- seq.int(start_year, end_year)               # 2025:2030
  span <- end_year - start_year                      # = 5
  
  dt_out <- dt_raw[, {
    delta <- get(target_col) - get(current_col)
    .(year = yrs,
      (get(current_col) + delta * (yrs - start_year) / span))
  }, by = .(location)]
  
  setnames(dt_out, old = "V2", new = out_col)
  setcolorder(dt_out, c("location", "year", out_col))
  
  dt_out[]
}

## ── function to expand & interpolate with a logistic curve ───────────────
interpolate_statins_logistic <- function(dt_raw,
                                         start_year   = 2025L,
                                         end_year     = 2050L,
                                         current_col  = "statins_current",
                                         target_col   = "statins_target",
                                         out_col      = "statins_uptake",
                                         k            = 6) {  # k controls steepness (approx 6 gives a classic S-shape)
  
  stopifnot(is.data.table(dt_raw))
  
  yrs  <- seq.int(start_year, end_year)
  span <- end_year - start_year
  
  ## pre-compute the normalising constants so that g(0)=0 and g(1)=1
  c1 <- 1 / (1 + exp(  k * 0.5))       # value of logistic at t = 0
  c2 <- 1 / (1 + exp(- k * 0.5))       # value of logistic at t = 1
  
  dt_out <- dt_raw[, {
    delta   <- get(target_col) - get(current_col)
    t_norm  <- (yrs - start_year) / span                     # 0 … 1
    g       <- (1 / (1 + exp(-k * (t_norm - 0.5))) - c1) /   # rescaled logistic
      (c2 - c1)
    .(year  = yrs,
      uptake = get(current_col) + delta * g)
  }, by = .(location)]
  
  setnames(dt_out, "uptake", out_col)
  setcolorder(dt_out, c("location", "year", out_col))
  dt_out[]
}


## ── scale up 2025-2030 ────────────────────────────────────────────────────────────────
#dt_statins_interp <- interpolate_statins_logistic(data.in.statin)
dt_statins_interp <- interpolate_statins(data.in.statin)

dt_statins_current <- c()

for(ii in 2019:2024){
  dt_temp <- dt_statins_interp[year==2025,]
  dt_temp[, year := ii]
  dt_statins_current <- rbind(dt_statins_current,
                              dt_temp)
}

# dt_statins_target <- c()
# 
# for(ii in 2031:2050){
#   dt_temp <- dt_statins_interp[year==2030,]
#   dt_temp[, year := ii]
#   dt_statins_target <- rbind(dt_statins_target, 
#                              dt_temp)
# }
# 
# dt_statins_scenarios <- rbind(dt_statins_current, dt_statins_interp,
#                                   dt_statins_target)

dt_statins_scenarios <- rbind(dt_statins_current, dt_statins_interp)

dt_statins_scenarios <- merge(dt_statins_scenarios,data.in.statin,all.x = T)

dt_statins_scenarios <- dt_statins_scenarios[order(dt_statins_scenarios$location,dt_statins_scenarios$year), ]
dt_statins_scenarios[, statins_uptake_lag := shift(statins_uptake, n = 1, type = "lag"), by = location]
dt_statins_scenarios[, statins_uptake_delta_lag := statins_uptake-statins_uptake_lag]

dt_statins_scenarios[, statins_uptake_delta := statins_uptake-statins_current]

dt_statins_scenarios[is.na(statins_uptake_lag), statins_uptake_lag := 0]
dt_statins_scenarios[is.na(statins_uptake_delta), statins_uptake_delta_lag := 0]
dt_statins_scenarios[is.na(statins_uptake_delta), statins_uptake_delta := 0]

# Check country names
dt_statins_scenarios[, location := gsub("United States of America", "United States", location)]


dt_statins_scenarios[location=="Bolivia (Plurinational State of)", location := "Bolivia"]
dt_statins_scenarios[location=="United Republic of Tanzania", location := "Tanzania"]

dt_statins_scenarios_samoa <- dt_statins_scenarios[location=="Samoa",]
dt_statins_scenarios_samoa[,location:="American Samoa"]

dt_statins_scenarios <- rbind(dt_statins_scenarios,dt_statins_scenarios_samoa)

# Check locations without statins data

# American Samoa
# Bermuda
# Bolivia
# Greenland
# Ivory Coast
# Tanzania

# Keep only Indonesia
dt_statins_scenarios <- dt_statins_scenarios[location=="Indonesia",]
# Save the data
saveRDS(dt_statins_scenarios, file = paste0(wd_data,"statin_data.rds"))

#print length(locs_statins)
locs_statins <- unique(dt_statins_scenarios$location)
print(paste0("Number of locations with statin data: ", length(locs_statins)))

#clean
rm(data.in.statin,dt_sta,dt_statins_interp,dt_statins_scenarios,dt_temp,
   dt_statins_current,dt_statins_target,interpolate_statins,dt_statins_scenarios_samoa)

# #...........................................................
# # Bp control----
# #...........................................................
# 
# #...........................................................
# ## Coverage----
# #...........................................................
# 
# # From coverage_newfig
# 
# #Using age-standardized rates of htn control from 2000-2019
# 
# ncdr <- read.csv(paste0(wd_raw,"NCD-RisC/","NCD-RisC_Lancet_2021_Hypertension_age_standardised_countries.csv"), stringsAsFactors = F)%>%
#   select(Country.Region.World, ISO, Year, Proportion.of.controlled.hypertension.among.all.hypertension)%>%
#   rename(control = Proportion.of.controlled.hypertension.among.all.hypertension,
#          Country = Country.Region.World)%>%
#   group_by(Country, ISO, Year)%>%
#   summarise(control = mean(control))%>%
#   group_by(Country, ISO)%>%
#   mutate(change = shift(control, type='lead')- control,
#          r_change = 100*change/control)
# 
# 
# #https://www.statology.org/quadratic-regression-r/
# 
# 
# ncdr$control2<-ncdr$control^2
# quadraticModel <- lm(change ~ control + control2, data=ncdr%>%filter(Country=="Canada"))
# summary(quadraticModel)
# 
# #create sequence of control values
# controlValues <- seq(0, 0.60, 0.01)
# #create list of predicted change using quadratic model
# changePredict <- predict(quadraticModel,list(control=controlValues, control2=controlValues^2))
# 
# data<-ncdr%>%filter(Country=="Canada")
# 
# fit<-data.frame(controlValues=controlValues, 
#                 changePredict=changePredict)
# 
# ggplot(data, aes(x=control, y=change))+
#   geom_point()+
#   geom_line(data = fit, aes(x=controlValues, y=changePredict))+
#   xlab("Baseline control")+
#   ylab("Change in coverage")+
#   ggtitle("Canada: 1990-2019")
# 
# #Add custom fxn line
# quadraticModel2<-quadraticModel
# quadraticModel2$coefficients[1]<-0
# quadraticModel2$coefficients[2]<-0.43*0.527
# quadraticModel2$coefficients[3]<- -0.43
# quadraticModel2
# 
# #create sequence of control values
# controlValues2 <- seq(0, 0.60, 0.01)
# #create list of predicted change using quadratic model
# changePredict2 <- predict(quadraticModel2,
#                           list(control=controlValues2, 
#                                control2=controlValues2^2))
# 
# fit2<-data.frame(controlValues2=controlValues2, 
#                  changePredict2=changePredict2)
# 
# ggplot(data, aes(x=control, y=change))+
#   geom_point()+
#   geom_line(data = fit, 
#             aes(x=controlValues, y=changePredict,color="Empirical"),
#             size=1)+
#   geom_line(data = fit2, 
#             aes(x=controlValues2, y=changePredict2,color="Modeled"),
#             size=1)+
#   xlab("Baseline control")+
#   ylab("Change in coverage")+
#   ggtitle("Canada: 1990-2019")+
#   scale_color_manual(name = "Models", 
#                      values = c("Empirical" = "darkblue", "Modeled" = "red"))
# 
# 
# #ggsave("Canada.png", height = 6, width =8)
# 
# #compare to other countries
# 
# more_data<-ncdr%>%filter(Country%in%c("Canada",
#                                       "South Korea",
#                                       "Germany",
#                                       "Finland",
#                                       "Iceland",
#                                       "China"))
# 
# ggplot(more_data, aes(x=control, y=change))+
#   geom_point(aes(colour=Country))+
#   geom_line(data = fit, 
#             aes(x=controlValues, y=changePredict),
#             size=1, color="darkblue")+
#   geom_line(data = fit2, 
#             aes(x=controlValues2, y=changePredict2),
#             size=1, color = "red")+
#   xlab("Baseline control")+
#   ylab("Change in coverage")+
#   ylim(0,0.035)
# 
# #ggsave("other_countries.png", height = 6, width = 8)
# 
# 
# 
# ## Ambitious----
# 
# quadraticModel3<-quadraticModel
# quadraticModel3$coefficients[1]<-0
# quadraticModel3$coefficients[2]<-0.285*0.75
# quadraticModel3$coefficients[3]<- -0.285
# quadraticModel3
# #create sequence of control values
# controlValues3 <- seq(0, 1, 0.01)
# #create list of predicted change using quadratic model
# changePredict3 <- predict(quadraticModel3,
#                           list(control=controlValues3, 
#                                control2=controlValues3^2))
# 
# fit3<-data.frame(controlValues3=controlValues3, 
#                  changePredict3=changePredict3)
# 
# 
# ggplot(data, aes(x=control, y=change))+
#   geom_point()+
#   geom_line(data = fit, 
#             aes(x=controlValues, y=changePredict,color="Empirical"),
#             size=1)+
#   geom_line(data = fit2, 
#             aes(x=controlValues2, y=changePredict2,color="Progress"),
#             size=1)+
#   geom_line(data = fit3, 
#             aes(x=controlValues3, y=changePredict3,color="Aspirational"),
#             size=1)+
#   xlab("Baseline control")+
#   xlim(0,1.1)+
#   ylab("Change in coverage")+
#   ggtitle("Canada: 1990-2019")+
#   scale_color_manual(name = "Models", 
#                      values = c("Empirical" = "darkblue", 
#                                 "Progress" = "red",
#                                 "Aspirational" = "darkgreen"))
# 
# #### Appendix plot####
# ggplot(ncdr, aes(x=100*control, y=100*change))+
#   geom_point()+
#   geom_line(data = fit2, 
#             aes(x=100*controlValues2, y=100*changePredict2,color="Progress"),
#             size=1)+
#   geom_line(data = fit3, 
#             aes(x=100*controlValues3, y=100*changePredict3,color="Aspirational"),
#             size=1)+
#   xlab("Proportion of population with blood pressure controlled in year t (%)")+
#   xlim(0,100)+
#   ylim(0,5)+
#   ylab("Additional proportion of population with blood pressure controlled in year t + 1 (%)")+
#   scale_color_manual(name = "Models", 
#                      values = c("Progress" = "red",
#                                 "Aspirational" = "blue"))
# 
# #ggsave("figures/scale-up.png",height = 6, width = 10)
# 
# ggplot(ncdr, aes(x=Year, y=control))+
#   geom_point()
# 
# mean(ncdr%>%filter(Year==2019)%>%arrange(control)%>%pull(control))
# df<-ncdr%>%filter(Year==2019)%>%arrange(desc(control))
# df<-df[1:5,]
# mean(df$control)
# 
# 
# ## add resolve data ----
# # 
# # rtsl<-read.csv(paste0(wd,"add_cov_data.csv"), stringsAsFactors = F)%>%
# #   filter(location%in%c("Bangladesh", "Colombia", "Ecuador", "Ethiopia", "Peru", "Vietnam", "India"))
# # 
# # rtsl[8,4]<-"Longest running HEARTS \nprogram (year 1)"
# # rtsl[9,4]<-"Longest running HEARTS \nprogram (year 2)"
# # 
# # rtsl<-rtsl%>%mutate(data = ifelse(data=="RTSL", "HEARTS program", data))
# # 
# # plot<-bind_rows(rtsl, ncdr%>%mutate(data="NCD-RisC"))%>%arrange(desc(data))
# # 
# # cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "grey", "#0072B2", "#D55E00", "#CC79A7")
# # 
# # mytheme <- theme_bw() + theme(legend.title = element_blank())
# # theme_set(mytheme)
# 
# # ggplot(plot%>%filter(change>=0),
# #        aes(x=100*control, y=100*change, color=data, fill=data,
# #            alpha=data, size=data, shape=data, linetype=data))+
# #   geom_point()+
# #   scale_fill_manual(values = c("#1E88E5", cbPalette[1],cbPalette[3],cbPalette[7],"black",'#D81B60'),
# #                     aesthetics = c("colour", "fill")) +
# #   scale_alpha_manual(values = c(1,1, 1,  1, 0.1, 1))+
# #   scale_size_manual(values = c(1,2,2,2,1, 1))+
# #   scale_shape_manual(values = c(NA, 24,23,15, 19, NA))+
# #   scale_linetype_manual(values = c("solid", NA,NA,NA,NA, "solid"))+
# #   guides(color = guide_legend(override.aes = list(alpha = 1)))+
# #   xlab("Proportion of population with blood pressure controlled in year t (%)")+
# #   xlim(0,100)+
# #   coord_cartesian(ylim=c(0,9))+
# #   ylab("Additional proportion of population with blood pressure controlled in year t+1 (%)")+
# #   #theme(legend.title=element_blank())+
# #   #theme_bw()+
# #   geom_line(data = fit2%>%filter(changePredict2>=0),
# #             aes(x=100*controlValues2, y=100*changePredict2,
# #                 color=   "Progress scenario \nscale-up function",
# #                 fill=    "Progress scenario \nscale-up function",
# #                 alpha=   "Progress scenario \nscale-up function",
# #                 size=    "Progress scenario \nscale-up function",
# #                 shape =  "Progress scenario \nscale-up function",
# #                 linetype="Progress scenario \nscale-up function"))+
# #   geom_line(data = fit3%>%filter(changePredict3>=0),
# #             aes(x=100*controlValues3, y=100*changePredict3,
# #                 color=    "Aspirational scenario \nscale-up function",
# #                 fill=     "Aspirational scenario \nscale-up function",
# #                 alpha=    "Aspirational scenario \nscale-up function",
# #                 size=     "Aspirational scenario \nscale-up function",
# #                 shape =   "Aspirational scenario \nscale-up function",
# #                 linetype= "Aspirational scenario \nscale-up function"))
# 
# 
# # ggsave("../../output/fig_A5.pdf", height = 6, width = 8, dpi=600)
# 
# 
# 
# #########bau############
# #https://www.mathepower.com/en/quadraticfunctions.php
# 
# quadraticModel_1b<-quadraticModel
# quadraticModel_1b$coefficients[1]<-0
# quadraticModel_1b$coefficients[2]<-0.45*0.45
# quadraticModel_1b$coefficients[3]<- -0.45
# 
# quadraticModel_2b<-quadraticModel
# quadraticModel_2b$coefficients[1]<-0
# quadraticModel_2b$coefficients[2]<-0.4*0.4
# quadraticModel_2b$coefficients[3]<- -0.4
# 
# quadraticModel_3b<-quadraticModel
# quadraticModel_3b$coefficients[1]<-0
# quadraticModel_3b$coefficients[2]<-0.467*0.35
# quadraticModel_3b$coefficients[3]<- -0.467
# 
# quadraticModel_4b<-quadraticModel
# quadraticModel_4b$coefficients[1]<-0
# quadraticModel_4b$coefficients[2]<-0.7*0.3
# quadraticModel_4b$coefficients[3]<- -0.7
# 
# quadraticModel_5b<-quadraticModel
# quadraticModel_5b$coefficients[1]<-0
# quadraticModel_5b$coefficients[2]<-0.7*0.25
# quadraticModel_5b$coefficients[3]<- -0.7
# 
# quadraticModel_6b<-quadraticModel
# quadraticModel_6b$coefficients[1]<-0
# quadraticModel_6b$coefficients[2]<-0.6*0.25
# quadraticModel_6b$coefficients[3]<- -0.6
# 
# quadraticModel_7b<-quadraticModel
# quadraticModel_7b$coefficients[1]<-0
# quadraticModel_7b$coefficients[2]<-0.85*0.2
# quadraticModel_7b$coefficients[3]<- -0.85
# 
# quadraticModel_8b<-quadraticModel
# quadraticModel_8b$coefficients[1]<-0
# quadraticModel_8b$coefficients[2]<-0.8*0.2
# quadraticModel_8b$coefficients[3]<- -0.8
# 
# quadraticModel_9b<-quadraticModel
# quadraticModel_9b$coefficients[1]<-0
# quadraticModel_9b$coefficients[2]<-0.6*0.2
# quadraticModel_9b$coefficients[3]<- -0.6
# 
# 
# #create sequence of control values
# controlValues <- seq(0, 0.6, 0.01)
# #create list of predicted change using quadratic model
# changePredict_1 <- predict(quadraticModel_1b,
#                            list(control=controlValues, 
#                                 control2=controlValues^2))
# changePredict_2 <- predict(quadraticModel_2b,
#                            list(control=controlValues, 
#                                 control2=controlValues^2))
# changePredict_3 <- predict(quadraticModel_3b,
#                            list(control=controlValues, 
#                                 control2=controlValues^2))
# changePredict_4 <- predict(quadraticModel_4b,
#                            list(control=controlValues, 
#                                 control2=controlValues^2))
# changePredict_5 <- predict(quadraticModel_5b,
#                            list(control=controlValues, 
#                                 control2=controlValues^2))
# changePredict_6 <- predict(quadraticModel_6b,
#                            list(control=controlValues, 
#                                 control2=controlValues^2))
# changePredict_7 <- predict(quadraticModel_7b,
#                            list(control=controlValues, 
#                                 control2=controlValues^2))
# changePredict_8 <- predict(quadraticModel_8b,
#                            list(control=controlValues, 
#                                 control2=controlValues^2))
# changePredict_9 <- predict(quadraticModel_9b,
#                            list(control=controlValues, 
#                                 control2=controlValues^2))
# 
# 
# fit_1<-data.frame(controlValues=controlValues, 
#                   changePredict=changePredict_1)
# fit_2<-data.frame(controlValues=controlValues, 
#                   changePredict=changePredict_2)
# fit_3<-data.frame(controlValues=controlValues, 
#                   changePredict=changePredict_3)
# fit_4<-data.frame(controlValues=controlValues, 
#                   changePredict=changePredict_4)
# fit_5<-data.frame(controlValues=controlValues, 
#                   changePredict=changePredict_5)
# fit_6<-data.frame(controlValues=controlValues, 
#                   changePredict=changePredict_6)
# fit_7<-data.frame(controlValues=controlValues, 
#                   changePredict=changePredict_7)
# fit_8<-data.frame(controlValues=controlValues, 
#                   changePredict=changePredict_8)
# fit_9<-data.frame(controlValues=controlValues, 
#                   changePredict=changePredict_9)
# 
# #....................................................
# #fit each country to its BAU fxn - minimize errors
# #....................................................
# 
# ncdr1<-ncdr%>%mutate(fit1=predict(quadraticModel_1b,
#                                   list(control=control, 
#                                        control2=control^2)),
#                      fit2=predict(quadraticModel_2b,
#                                   list(control=control, 
#                                        control2=control^2)),
#                      fit3=predict(quadraticModel_3b,
#                                   list(control=control, 
#                                        control2=control^2)),
#                      fit4=predict(quadraticModel_4b,
#                                   list(control=control, 
#                                        control2=control^2)),
#                      fit5=predict(quadraticModel_5b,
#                                   list(control=control, 
#                                        control2=control^2)),
#                      fit6=predict(quadraticModel_6b,
#                                   list(control=control, 
#                                        control2=control^2)),
#                      fit7=predict(quadraticModel_7b,
#                                   list(control=control, 
#                                        control2=control^2)),
#                      fit8=predict(quadraticModel_8b,
#                                   list(control=control, 
#                                        control2=control^2)),
#                      fit9=predict(quadraticModel_9b,
#                                   list(control=control, 
#                                        control2=control^2)),
#                      error1 = sqrt((change-fit1)^2),
#                      error2 = sqrt((change-fit2)^2),
#                      error3 = sqrt((change-fit3)^2),
#                      error4 = sqrt((change-fit4)^2),
#                      error5 = sqrt((change-fit5)^2),
#                      error6 = sqrt((change-fit6)^2),
#                      error7 = sqrt((change-fit7)^2),
#                      error8 = sqrt((change-fit8)^2),
#                      error9 = sqrt((change-fit9)^2)
# )
# 
# 
# ncdr1<-ncdr1%>%select(Country, ISO, Year, error1, error2, error3, error4, 
#                       error5, error6, error7, error8, error9)%>%
#   filter(Year>=2010)%>%
#   gather(model, error, -ISO, -Year, -Country)%>%
#   group_by(ISO,Country, model)%>%
#   summarise(error = sum(error, na.rm = T))%>%
#   ungroup()%>%
#   group_by(ISO, Country)%>%
#   filter(error==min(error))%>%
#   mutate(model = substr(model, 6,6))
# 
# 
# ncdr<-left_join(ncdr, ncdr1%>%select(ISO, model))
# 
# ggplot(ncdr, aes(x=control, y=change))+
#   geom_point(alpha=0.2)+
#   geom_line(data = fit2, 
#             aes(x=controlValues2, y=changePredict2,color="Progress"),
#             size=1)+
#   geom_line(data = fit3, 
#             aes(x=controlValues3, y=changePredict3,color="Aspirational"),
#             size=1)+
#   geom_line(data = fit_1%>%mutate(model = 1), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_2%>%mutate(model = 2), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_3%>%mutate(model = 3), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_4%>%mutate(model = 4), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_5%>%mutate(model = 5), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_6%>%mutate(model = 6), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_7%>%mutate(model = 7), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_8%>%mutate(model = 8), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_9%>%mutate(model = 9), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   facet_wrap(~model)+
#   xlab("Baseline control rate")+
#   xlim(0,1)+
#   ylim(0,0.05)+
#   ylab("Change in HTN control coverage")+
#   ggtitle("1990-2019")+
#   scale_color_manual(name = "Models", 
#                      values = c("Progress" = "#D81B60",
#                                 "Aspirational" = "#1E88E5",
#                                 "BAU" = "#E4AB00"))+
#   theme_bw()
# 
# 
# #ggsave("baseline_fxns.png", height=8, width=12)
# #take out countries already at 0.512 and make a 'progress=bau' bin
# 
# #....................................................#
# #library(stringr)
# 
# ggplot(ncdr%>%filter(model%in%c(1,5,9))%>%mutate(model = factor(model, levels=c(1,5,9),
#                                                                 labels=c("Best-performing quantile", "Middle-performing quantile", "Worst-performing quantile"))), 
#        aes(x=control, y=change))+
#   geom_point(alpha=0.2)+
#   geom_line(data = fit2, 
#             aes(x=controlValues2, y=changePredict2,color="Progress scenario \nscale-up function"),
#             size=1)+
#   geom_line(data = fit3, 
#             aes(x=controlValues3, y=changePredict3,color="Aspirational scenario \nscale-up function"),
#             size=1)+
#   geom_line(data = fit_1%>%mutate(model = "Best-performing quantile"), 
#             aes(x=controlValues, y=changePredict,color="Business as usual scenario \nscale-up function"),
#             size=1)+
#   geom_line(data = fit_5%>%mutate(model = "Middle-performing quantile"), 
#             aes(x=controlValues, y=changePredict,color="Business as usual scenario \nscale-up function"),
#             size=1)+
#   geom_line(data = fit_9%>%mutate(model = "Worst-performing quantile"), 
#             aes(x=controlValues, y=changePredict,color="Business as usual scenario \nscale-up function"),
#             size=1)+
#   facet_wrap(~model, ncol=1)+
#   xlab("Proportion of population with blood pressure controlled in year t (%)")+
#   xlim(0,1)+
#   ylim(0,0.05)+
#   ylab(str_wrap("Additional proportion of population with blood pressure controlled in year t+1 (%)",45))+
#   ggtitle("1990-2019")+
#   scale_color_manual(name = "Models", 
#                      values = c("Progress scenario \nscale-up function" = "#D81B60",
#                                 "Aspirational scenario \nscale-up function" = "#1E88E5",
#                                 "Business as usual scenario \nscale-up function" = "#E4AB00"))+
#   theme_bw()
# 
# 
# #ggsave("../../output/fig_A6_alt.pdf", height=10, width=8, dpi=600)
# 
# #....................................................#
# 
# 
# ggplot(ncdr, aes(x=control, y=change))+
#   geom_point(alpha=0.2)+
#   geom_line(data = fit2, 
#             aes(x=controlValues2, y=changePredict2,color="Progress"),
#             size=1)+
#   geom_line(data = fit3, 
#             aes(x=controlValues3, y=changePredict3,color="Aspirational"),
#             size=1)+
#   geom_line(data = fit_1%>%mutate(model = 1), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_2%>%mutate(model = 2), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_3%>%mutate(model = 3), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_4%>%mutate(model = 4), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_5%>%mutate(model = 5), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_6%>%mutate(model = 6), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_7%>%mutate(model = 7), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_8%>%mutate(model = 8), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   geom_line(data = fit_9%>%mutate(model = 9), 
#             aes(x=controlValues, y=changePredict,color="BAU"),
#             size=1)+
#   xlab("Baseline control rate")+
#   xlim(0,1)+
#   ylim(0,0.05)+
#   ylab("Change in HTN control coverage")+
#   ggtitle("1990-2019")+
#   scale_color_manual(name = "Models", 
#                      values = c("Progress" = "#D81B60",
#                                 "Aspirational" = "#1E88E5",
#                                 "BAU" = "#E4AB00"))+
#   theme_bw()
# 
# #ggsave("baseline_fxns_1.png", height=8, width=12)
# 
# #new fig
# 
# 
# df<- read.csv(paste0(wd_raw,"NCD-RisC/","NCD-RisC_Lancet_2021_Hypertension_age_standardised_countries.csv"), stringsAsFactors = F)%>%
#   select(Country.Region.World, ISO, Year, Proportion.of.controlled.hypertension.among.all.hypertension)%>%
#   #filter(Year>=2000)%>%
#   rename(control = Proportion.of.controlled.hypertension.among.all.hypertension,
#          Country = Country.Region.World)%>%
#   group_by(Country, ISO, Year)%>%
#   summarise(control = mean(control))%>%
#   mutate(Progress = control,
#          Aspirational = control,
#          `Business as usual` = control,
#          p_change = NA,
#          a_change = NA,
#          aroc = NA)
# 
# #for those already above say 49%, we assume the progress ~ bau
# plocs<-df%>%filter(control>0.49)%>%pull(Country)%>%unique()
# 
# df<-left_join(df, ncdr1%>%select(Country, ISO, model))
# 
# for(i in 1:31){
#   
#   if(i>5){
#     temp<-df%>%filter(Year==2018+i)%>%
#       mutate(Year= Year+1,
#              p_change = Progress*0.43*0.527 + (-0.43)*(Progress^2),
#              a_change = Aspirational*0.285*0.75 + (-0.285)*(Aspirational^2),
#              aroc = ifelse(model==1, `Business as usual`*0.45*0.45 + (-0.45)*(`Business as usual`)^2,
#                            ifelse(model==2, `Business as usual`*0.4*0.4 + (-0.4)*(`Business as usual`)^2,
#                                   ifelse(model==3, `Business as usual`*0.467*0.35 + (-0.467)*(`Business as usual`)^2,
#                                          ifelse(model==4, `Business as usual`*0.7*0.3 + (-0.7)*(`Business as usual`)^2,
#                                                 ifelse(model==5, `Business as usual`*0.7*0.25 + (-0.7)*(`Business as usual`)^2,
#                                                        ifelse(model==6, `Business as usual`*0.6*0.25 + (-0.6)*(`Business as usual`)^2,
#                                                               ifelse(model==7, `Business as usual`*0.85*0.2 + (-0.85)*(`Business as usual`)^2,
#                                                                      ifelse(model==8, `Business as usual`*0.8*0.2 + (-0.8)*(`Business as usual`)^2,
#                                                                             `Business as usual`*0.6*0.2 + (-0.6)*(`Business as usual`)^2)
#                                                               ))))))),
#              aroc = ifelse(Country%in%plocs, p_change, aroc),
#              p_change = ifelse(p_change<0,0,p_change),
#              a_change = ifelse(a_change<0,0,a_change),
#              aroc     = ifelse(aroc<0,0,aroc),
#              Progress = Progress + p_change,
#              Aspirational = Aspirational + a_change,
#              `Business as usual` = `Business as usual` + aroc)
#   }
#   else{
#     temp<-df%>%filter(Year==2018+i)%>%
#       mutate(Year= Year+1,
#              Progress = Progress,
#              Aspirational = Aspirational,
#              `Business as usual` = `Business as usual`,
#              p_change = 0,
#              a_change = 0,
#              aroc     = 0)
#   }
#   df<-bind_rows(df, temp)
# }
# 
# regions<-read.csv(paste0(wd,"Country_groupings_extended.csv"), stringsAsFactors = F)%>%
#   rename(ISO = iso3)%>%
#   select(ISO, wb2021)
# 
# df<-left_join(df, regions)%>%
#   mutate(`Business as usual` = ifelse(`Business as usual`>Progress, Progress, `Business as usual`))
# 
# plot<-df%>%select(Country, ISO, Year, wb2021, Progress, Aspirational, `Business as usual`)%>%
#   gather(Scenario, control, - Country, - Year, - wb2021, -ISO)%>%ungroup()
# 
# plot$wb2021<-factor(plot$wb2021, levels = c("LIC", "LMIC", "UMIC", "HIC"))
# plot$Scenario<-factor(plot$Scenario, levels = c("Business as usual", 
#                                                 "Progress",
#                                                 "Aspirational"))
# 
# ##graph it
# ggplot(na.omit(plot), 
#        aes(x=Year, y=100*control, group = Country))+
#   geom_line(size = 0.5)+
#   facet_grid(wb2021~Scenario)+
#   ylab("Hypertension control rate (%)")+
#   geom_line(y=51.2, color="red", linetype='dotted')
# 
# #ggsave("../../output/fig_A7.pdf", height = 8, width = 12, dpi=600)
# 
# #write.csv(plot, "../../output/scale_up_data_2022.csv", row.names = F)
# 
# covfxn<-df%>%filter(Year>=2017)
# 
# covfxn$aroc[covfxn$Year<=2019]<-0
# covfxn$p_change[covfxn$Year<=2019]<-0
# covfxn$a_change[covfxn$Year<=2019]<-0
# 
# #write.csv(covfxn, "model/covfxn.csv", row.names = F)
# 
# #plus salt ---
# 
# #add salt impacts
# #add salt impacts
# 
# data.in<-fread(paste0(wd,"bp_data5.csv"))%>%
#   select(-Year, -Country)%>%
#   rename(location = location_gbd,
#          age = Age.group)
# 
# #data.in<-fread(paste0(wd,"bp_data6.csv"))
# 
# data.in$salt[data.in$location=="China"]<-4.83*2.52
# 
# names<-read.csv(paste0(wd,"Country_groupings_extended.csv"), stringsAsFactors = F)%>%
#   select(location_gbd, iso3)%>%
#   rename(location = location_gbd)
# 
# data.in<-left_join(data.in, names)
# 
# bpcats<-c("<120", "120-129", "130-139", 
#           "140-149", "150-159", "160-169", 
#           "170-179", "180+")
# 
# data.in<-merge(bpcats, data.in)%>%rename(bp_cat = x)
# 
# #b_rates<-fread("../base_rates_2019.csv")%>%
# b_rates<-fread(paste0("C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/","100MLives/data/","base_rates_2022.csv"))%>%
#   filter(cause=="ihd")%>%
#   select(year, location, sex, age, Nx)%>%
#   rename(Year=year)
# 
# source(paste0(wd_code,"functions_review_6_100.R"))
# 
# repYear<-function(row){
#   2017+floor((row-1)/224)
# }
# 
# bp_out<-data.frame(Year=numeric(),
#                    ref=numeric(),
#                    asp=numeric(),
#                    location=character()
# )
# 
# data.in<-data.table(data.in)
# 
# for (i in unique(data.in$location)){
#   DT<-unique(data.in[location==i][,Year:=2017][,-c("Lower95", "Upper95")])
#   DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
#   bp_prob_salt<-get.bp.prob(DT.in, 0.15, 'percent', 2023, 2030, 0, "baseline")
#   DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
#   bp_prob_salt2<-get.bp.prob(DT.in, 0.3, 'percent', 2023, 2027, 0, "baseline")
#   DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
#   bp_prob_base<-get.bp.prob(DT.in, 0, 'percent', 2023, 2025, 0, "baseline")
#   setnames(bp_prob_base, "prob", "prob_0")
#   setnames(bp_prob_salt2, "prob", "prob_2")
#   
#   bp_probs<-merge(bp_prob_salt, bp_prob_base, 
#                   by=c("age","sex", "bp_cat", "Year", "location")) 
#   bp_probs<-merge(bp_prob_salt2, bp_probs, 
#                   by=c("age","sex", "bp_cat", "Year", "location")) 
#   
#   #duplicating data to be age-specific
#   bp_probs[, age:=as.numeric(substr(age, 1,2))]
#   bp_probs<-bp_probs[rep(seq_len(nrow(bp_probs)), each=5)]
#   bp_probs[,age2:=rep(1:5, nrow(bp_probs)/5)][,age:=age+age2-1]
#   
#   over90<-bp_probs[age==89]
#   
#   over90<-over90[rep(seq_len(nrow(over90)), each=6)]
#   over90[,age2:=rep(1:6, nrow(over90)/6)][,age:=age+age2]
#   
#   #bind  
#   bp_probs<-rbindlist(list(bp_probs, over90))
#   
#   bps<-left_join(bp_probs, b_rates%>%filter(location==i), 
#                  by=c("age", "sex", "location", "Year"))
#   
#   normo<-bps%>%filter(bp_cat=="<120" | bp_cat=="120-129" | bp_cat=="130-139")%>%
#     group_by(Year, age, sex, location)%>%
#     summarise(propbase = sum(prob_0), 
#               propref=sum(prob), 
#               propasp=sum(prob_2),
#               Nx=sum(Nx)/2)%>%
#     ungroup()%>%group_by(Year, location)%>%
#     summarise(base=weighted.mean(propbase, Nx),
#               ref=weighted.mean(propref, Nx),
#               asp=weighted.mean(propasp, Nx))%>%
#     mutate(ref=ref-base, asp=asp-base)%>%
#     select(Year, location, ref, asp)
#   
#   bp_out<-bind_rows(normo,bp_out)
#   
# }
# 
# bp_out<-left_join(bp_out, names)
# #newdf<-left_join(df, bp_out%>%rename(ISO=iso3))
# 
# add<-merge(2041:2050, bp_out%>%filter(Year==2040)%>%
#              ungroup()%>%select(-Year))%>%rename(Year=x)
# 
# newdf<-bind_rows(bp_out, add)%>%filter(Year<=2050)
# 
# 
# covfxn<-left_join(covfxn%>%rename(iso3 = ISO), newdf)
# 
# covfxn<-covfxn%>%
#   mutate(reach_base = ifelse(0.512<=`Business as usual`, Year, NA),
#          refwsalt = ifelse(0.512<=Progress+ref, Year, NA),
#          aspwsalt = ifelse(0.512<=Aspirational+asp, Year, NA),
#          reach_75 = ifelse(0.512<=Progress, Year, NA),
#          reach_975 = ifelse(0.512<=Aspirational, Year, NA))%>%
#   group_by(iso3)%>%
#   mutate(reach_base = min(reach_base, na.rm=T),
#          refwsalt = min(refwsalt, na.rm=T),
#          aspwsalt = min(aspwsalt, na.rm=T),
#          reach_75 = min(reach_75, na.rm=T),
#          reach_975 = min(reach_975, na.rm=T))%>%
#   ungroup()
# 
# 
# # 
# # covfxn<-covfxn%>%filter(location!="Global", 
# #                         location!="American Samoa",
# #                         location!="Andorra",
# #                         location!= "Bermuda",
# #                         location!= "Dominica",
# #                         location!="Greenland",
# #                         location!="Marshall Islands",
# #                         location!="Northern Mariana Islands",
# #                         location!="Palestine",
# #                         location!="Taiwan (Province of China)",
# #                         location!="Guam",
# #                         location!="Puerto Rico",
# #                         location!="South Sudan",
# #                         location!="Virgin Islands, U.S.")
# 
# mean(covfxn$reach_75-covfxn$refwsalt)
# mean(covfxn$reach_975-covfxn$aspwsalt)
# 
# 
# ##cumulative increases
# covfxn<-covfxn%>%
#   group_by(iso3)%>%
#   mutate(p_change = Progress - control,
#          p_change = ifelse(p_change<0,0, p_change),
#          a_change = Aspirational - control,
#          a_change = ifelse(a_change<0,0,a_change),
#          aroc     = `Business as usual` - control,
#          aroc     = ifelse(aroc<0,0,aroc))
# 
# # covfxn$p_change[covfxn$Year<=2022]<-0
# # covfxn$a_change[covfxn$Year<=2022]<-0
# # covfxn$aroc[covfxn$Year<=2022]<-0
# 
# covfxn$p_change[covfxn$Year<=2025]<-0
# covfxn$a_change[covfxn$Year<=2025]<-0
# covfxn$aroc[covfxn$Year<=2025]<-0
# 
# #update Jan 18 - scale-up relative to baseline
# covfxn<-covfxn%>%
#   group_by(iso3)%>%
#   mutate(aroc2 = aroc/(1-control),
#          p_change2 = p_change/(1-control),
#          a_change2 = a_change/(1-control))
# 
# 
# #add ideal - treat all htn in 2023
# #covfxn<-covfxn%>%mutate(ideal = ifelse(Year>=2023, (1-control), 0))
# covfxn<-covfxn%>%mutate(ideal = ifelse(Year>=2025, (1-control), 0))
# 
# # Fix location names
# covfxn$location[covfxn$location=="Cote d'Ivoire"]<-"Ivory Coast"
# 
# write.csv(covfxn, paste0(wd_data,"covfxn2.csv"), row.names = F)
# 
# 
# # Compute linear intervention scale up-----
# 
# inc <- fread(paste0(wd_data,"covfxn2.csv"))
# 
# # Fixes location names
# 
# name_map <- c(
#   "Brunei"                            = "Brunei Darussalam",
#   "Cape Verde"                        = "Cabo Verde",
#   "Cote d'Ivoire"                     = "Ivory Coast",
#   "Czech Republic"                    = "Czechia",
#   "Federated States of Micronesia"    = "Micronesia (Federated States of)",
#   "Iran"                              = "Iran (Islamic Republic of)",
#   "Laos"                              = "Lao People's Democratic Republic",
#   "Macedonia"                         = "North Macedonia",
#   "Moldova"                           = "Republic of Moldova",
#   "South Korea"                       = "Republic of Korea",
#   "Swaziland"                         = "Eswatini",
#   "Syria"                             = "Syrian Arab Republic",
#   "The Bahamas"                       = "Bahamas",
#   "The Gambia"                        = "Gambia",
#   "Venezuela"                         = "Venezuela (Bolivarian Republic of)",
#   "Vietnam"                           = "Viet Nam",
#   "North Korea"                       = "Democratic People's Republic of Korea"
# )
# 
# 
# inc <- as.data.table(inc)
# inc[, location := fcoalesce(name_map[location], location)]
# 
# ## keep location, year, control for 2024
# inc <- inc[Year == 2024, .(location, iso3, Year, control)]
# 
# #rename location, year
# setnames(inc, c("location", "Year"), c("location", "year"))
# 
# inc[, control_target := 0.5]
# 
# interpolate_control <- function(dt_raw,
#                                 start_year  = 2024L,
#                                 end_year    = 2050L,
#                                 current_col = "control",
#                                 target_col  = "control_target",
#                                 out_col     = "control_scaled") {
#   
#   stopifnot(is.data.table(dt_raw))
#   
#   yrs  <- seq.int(start_year, end_year)
#   span <- end_year - start_year
#   
#   dt_out <- dt_raw[, {
#     
#     curr <- get(current_col)[1]     # baseline control
#     tgt  <- get(target_col)[1]      # baseline target
#     
#     if (curr > tgt) {
#       scaled_vals <- rep(0, length(yrs))
#     } else {
#       delta <- tgt - curr
#       scaled_vals <- curr + delta * (yrs - start_year) / span
#     }
#     
#     .(
#       year          = yrs,
#       baseline_ctrl = curr,          # preserve baseline value
#       scaled        = scaled_vals
#     )
#     
#   }, by = .(location)]
#   
#   # rename scaled column
#   setnames(dt_out, "scaled", out_col)
#   
#   # reorder columns
#   setcolorder(dt_out, c("location", "year", "baseline_ctrl", out_col))
#   
#   dt_out[]
# }
# 
# 
# dt_hbp_control <- interpolate_control(
#   inc,  
#   start_year  = 2026L,
#   end_year    = 2040L,
#   current_col = "control",
#   target_col  = "control_target",
#   out_col     = "control_scaled"
# )
# 
# # take only 2024 rows (baseline year)
# dt_hbp_control_2024 <- dt_hbp_control[year == 2026]
# dt_hbp_control_2040 <- dt_hbp_control[year == 2040]
# 
# # ----- 1. Create 2020–2023 block
# dt_hbp_control_2020_2023 <- dt_hbp_control_2024[
#   , .(year = 2020:2025,
#       baseline_ctrl = baseline_ctrl,
#       control_scaled = control_scaled),
#   by = .(location)
# ]
# 
# # ----- 2. Create 2041–2050 block
# dt_hbp_control_2041_2050 <- dt_hbp_control_2040[
#   , .(year = 2041:2050,
#       baseline_ctrl = baseline_ctrl,
#       control_scaled = control_scaled),
#   by = .(location)
# ]
# 
# # ----- 3. Combine all
# dt_hbp_control <- rbindlist(
#   list(
#     dt_hbp_control_2020_2023,
#     dt_hbp_control,
#     dt_hbp_control_2041_2050
#   ),
#   use.names = TRUE
# )[order(location, year)]
# 
# 
# 
# # coverage scale up
# dt_hbp_control[, coverage_scaleup := control_scaled - baseline_ctrl]
# 
# # Save the data
# saveRDS(dt_hbp_control, file = paste0(wd_data,"BloodPressure/", "hbp_control_data.rds"))
# 
# #print length(locs_statins)
# locs_statins <- unique(dt_hbp_control$location)
# print(paste0("Number of locations with data: ", length(locs_statins)))
# 
# #clean
# rm(dt_hbp_control, dt_hbp_control_2020_2023, inc, interpolate_control, name_map)




#===========================================================================
# FAIR CHOICES WORKBOOK-DRIVEN CATALOGUE  (feeds Models 06 and 09) ----
#---------------------------------------------------------------------------
# Everything below makes the FAIR Choices interventions, intervention-cause
# links, transition targets, effect sizes, coverage trajectories and cost
# components WORKBOOK-DRIVEN. All analytic inputs are read from the user's
# selections in `indonesia_model_inputs.xlsx` (path supplied by Model 00 as
# `model_inputs_file`); raw input columns are read and every derived field is
# reproduced in R (Excel cached formula results are NOT trusted).
#
# Two objects are exported:
#   * fair_scenarios : named list consumed by Model 06 -- baseline + one
#                      scenario per selected+valid intervention + a combined
#                      "all" scenario (when >= 2 interventions are runnable).
#   * fair_inputs    : list of validated catalogues consumed by Model 09
#                      (links, costs, assumptions, validation report, blocked
#                      links, cause/transition translations).
# Both are LISTS on purpose: Model 06 ends by removing every data.frame from
# the global environment, and lists survive that cleanup.
#
# FAIR Choices coverage-adjusted effect (FairChoices_Methods sheet and
# https://fairchoices.w.uib.no/documentation/fairchoices-methods/):
#   delta_cov(t)       = coverage(t) - baseline_coverage
#   adjusted_effect(t) = effect_value * delta_cov(t) / (1 - effect_value*baseline_coverage)
#   transition_effect  = adjusted_effect(t) * affected_fraction
#   p_scenario(t)      = p_baseline(t) * (1 - transition_effect(t))
# Prevention (well->sick) modifies incidence (IR / eff_ir); disease management
# (sick->death) modifies case fatality (CF / eff_cf). The workbook HF / severe
# labels (sick_hf, sick_severe) are collapsed onto the model's single "sick"
# state through affected_fraction ONLY -- NO new Markov states are created
# (explicit translate_transition() table below).
#===========================================================================

# --- Execution metaparameters: single source of truth is Model 00. Provide
#     safe fallbacks so this script can also be sourced stand-alone. ----------
if (!exists("model_inputs_file"))
  model_inputs_file <- paste0(wd, "data/indonesia_model_inputs.xlsx")
if (!exists("strict_model_input_validation"))
  strict_model_input_validation <- FALSE
if (!exists("baseline_scenario_id"))
  baseline_scenario_id <- "baseline"

# ---------------------------------------------------------------------------
# Binding intervention-inclusion contract (single source of truth).
#
# `Intervention_Cause_Map$include_flag` is the authoritative, row-level
# decision on whether a link enters the analysis. This helper normalizes and
# VALIDATES the column for BOTH the clinical and public-health catalogues so
# the rule is enforced identically everywhere:
#   * accept only unambiguous 0 / 1 (numeric, integer, or safely coercible
#     text such as "0"/"1"); anything else -- blank, NA, "TRUE", "yes", 2,
#     0.5 -- is a hard error, never silently treated as included;
#   * return an integer 0/1 vector the callers filter with `== 1L`.
# No execution switch may override a value produced here: inclusion is decided
# by the workbook alone. To (de)activate a link, edit its workbook include_flag.
# ---------------------------------------------------------------------------
.normalize_include_flag <- function(x, context = "Intervention_Cause_Map") {
  raw <- trimws(as.character(x))
  num <- suppressWarnings(as.numeric(raw))
  bad <- is.na(num) | !(num %in% c(0, 1))
  if (any(bad)) {
    idx  <- which(bad)
    shown <- ifelse(is.na(raw[idx]) | !nzchar(raw[idx]), "<blank>", raw[idx])
    stop(sprintf(
      "%s: include_flag must be exactly 0 or 1 for every row; found %d invalid value(s) at row(s) %s (value(s): %s). Blank/NA/other values are not permitted -- set the flag explicitly.",
      context, length(idx), paste(utils::head(idx, 25L), collapse = ", "),
      paste(utils::head(shown, 25L), collapse = ", ")), call. = FALSE)
  }
  as.integer(round(num))
}

.build_fair_catalogue <- function(inputs_path,
                                  cause_map,
                                  strict      = FALSE,
                                  baseline_id = "baseline") {

  if (!file.exists(inputs_path))
    stop("FAIR: input workbook not found: ", inputs_path)

  req_sheets <- c("Assumptions", "Dictionaries", "Intervention_Cause_Map",
                  "Effect_Sizes", "Coverage", "Cost_Components")
  have <- readxl::excel_sheets(inputs_path)
  miss <- setdiff(req_sheets, have)
  if (length(miss))
    stop("FAIR: workbook missing required sheet(s): ", paste(miss, collapse = ", "))

  rd   <- function(sheet) as.data.table(readxl::read_excel(inputs_path, sheet = sheet))
  numv <- function(x) suppressWarnings(as.numeric(x))
  chrv <- function(x) trimws(as.character(x))

  # Consolidated validation issue log -----------------------------------------
  issues <- data.table(scope = character(), item_key = character(), field = character(),
                       problem = character(), severity = character())
  add_issue <- function(scope, item_key, field, problem, severity = "FAIL")
    issues <<- rbind(issues, data.table(scope = scope, item_key = as.character(item_key),
                                        field = field, problem = problem,
                                        severity = severity))

  ## -- Assumptions (parameter_id -> value) ------------------------------------
  asmp <- rd("Assumptions")
  A    <- setNames(as.character(asmp$value), as.character(asmp$parameter_id))
  getA <- function(id, default = NA) if (id %in% names(A)) A[[id]] else default

  analysis_start_year     <- as.integer(numv(getA("analysis_start_year", 2025)))
  analysis_end_year       <- as.integer(numv(getA("analysis_end_year", 2050)))
  intervention_start_year <- as.integer(numv(getA("intervention_start_year", analysis_start_year)))
  coverage_target_year    <- as.integer(numv(getA("coverage_target_year", analysis_end_year)))
  target_coverage_default <- numv(getA("target_coverage_default", 0.8))
  cost_discount_rate      <- numv(getA("cost_discount_rate", 0.03))
  health_discount_rate    <- numv(getA("health_discount_rate", 0.03))
  cost_price_year         <- as.integer(numv(getA("cost_price_year", 2023)))

  ## -- cause_id -> model cause short code (explicit translation table) --------
  cause_id2code <- c(C_RHD = "rhd", C_IHD = "ihd", C_IS = "istroke",
                     C_ICH = "hstroke", C_HHD = "hhd", C_CMP = "cmd", C_T2D = "dm2")
  # C_SHARED intentionally absent (shared-cost bucket; not a modeled cause).
  bad_codes <- setdiff(unname(cause_id2code), names(cause_map))
  if (length(bad_codes))
    stop("FAIR: cause translation maps to code(s) absent from Model 00 cause_map: ",
         paste(bad_codes, collapse = ", "))

  ## -- transition translation (workbook from/to -> model transition) ----------
  translate_transition <- function(from, to) {
    from <- tolower(chrv(from)); to <- tolower(chrv(to))
    out <- rep(NA_character_, length(from))
    out[from == "well" & to == "sick"] <- "incidence"
    out[from %in% c("sick", "sick_severe", "sick_hf") & grepl("^dead", to)] <- "case_fatality"
    out
  }

  ## -- Read the four contract sheets ------------------------------------------
  map <- rd("Intervention_Cause_Map")
  eff <- rd("Effect_Sizes")
  cov <- rd("Coverage")
  cst <- rd("Cost_Components")

  # Binding inclusion contract: validate 0/1 then keep only include_flag == 1.
  map[, include_flag := .normalize_include_flag(
        include_flag, sprintf("Intervention_Cause_Map (%s)", basename(inputs_path)))]
  map_sel <- map[include_flag == 1L]

  # Duplicate / missing intervention_cause_key (selected links) ----------------
  dupk <- map_sel[, .N, by = intervention_cause_key][N > 1L]
  if (nrow(dupk))
    for (k in dupk$intervention_cause_key)
      add_issue("health_link", k, "intervention_cause_key", "duplicate selected link key", "FAIL")
  if (any(is.na(map_sel$intervention_cause_key) | !nzchar(chrv(map_sel$intervention_cause_key))))
    add_issue("health_link", "<NA>", "intervention_cause_key", "missing link key", "FAIL")

  # Effect / coverage match cardinality ---------------------------------------
  eff_n <- eff[, .(n_eff = .N), by = intervention_cause_key]
  cov_n <- cov[, .(n_cov = .N), by = intervention_cause_key]

  eff_k <- eff[, .(intervention_cause_key,
                   transition_from, transition_to, effect_measure,
                   effect_value      = numv(effect_value),
                   affected_fraction = numv(affected_fraction),
                   e_age_start       = numv(age_start),
                   e_age_stop        = numv(age_stop),
                   e_sex             = chrv(sex),
                   effect_review     = chrv(review_status))]

  cov_k <- cov[, .(intervention_cause_key,
                   baseline_coverage = numv(baseline_coverage),
                   target_override   = numv(target_override),
                   coverage_review   = chrv(review_status))]
  cov_k[, target_coverage := ifelse(is.na(target_override), target_coverage_default, target_override)]
  cov_k[, start_year  := intervention_start_year]
  cov_k[, target_year := coverage_target_year]

  ## -- Assemble the selected-link table ---------------------------------------
  L <- merge(map_sel[, .(intervention_cause_key, intervention_id, intervention_name,
                         cause_id, model_name, cost_join_key, cost_scope)],
             eff_k, by = "intervention_cause_key", all.x = TRUE)
  L <- merge(L, cov_k, by = "intervention_cause_key", all.x = TRUE)
  L <- merge(L, eff_n, by = "intervention_cause_key", all.x = TRUE)
  L <- merge(L, cov_n, by = "intervention_cause_key", all.x = TRUE)
  L[is.na(n_eff), n_eff := 0L]
  L[is.na(n_cov), n_cov := 0L]

  L[, model_transition := translate_transition(transition_from, transition_to)]
  L[, cause_code       := cause_id2code[cause_id]]

  ## -- Per-link validation -----------------------------------------------------
  in01 <- function(x) !is.na(x) & x >= 0 & x <= 1
  L[, problem := ""]
  padd <- function(cond, msg) {
    cond[is.na(cond)] <- FALSE
    L[cond, problem := paste0(problem, ifelse(nchar(problem) > 0L, "; ", ""), msg)]
  }
  padd(L$n_eff != 1L,                          "effect match != 1")
  padd(L$n_cov != 1L,                          "coverage match != 1")
  padd(!in01(L$effect_value),                  "effect_value missing/out of [0,1]")
  padd(!in01(L$affected_fraction),             "affected_fraction missing/out of [0,1]")
  padd(is.na(L$baseline_coverage),             "baseline coverage missing")
  padd(!is.na(L$baseline_coverage) & (L$baseline_coverage < 0 | L$baseline_coverage > 1),
                                               "baseline coverage out of [0,1]")
  padd(!in01(L$target_coverage),               "target coverage missing/out of [0,1]")
  padd(!is.na(L$baseline_coverage) & !is.na(L$target_coverage) &
         L$target_coverage < L$baseline_coverage, "target coverage < baseline coverage")
  padd(is.na(L$model_transition),              "transition label not mapped to model")
  padd(is.na(L$cause_code),                    "cause_id absent from Model 00 cause_map")
  padd(is.na(L$start_year) | is.na(L$target_year) | L$start_year > L$target_year,
                                               "invalid start/target year")
  L[, valid := problem == ""]

  for (i in which(!L$valid))
    add_issue("health_link", L$intervention_cause_key[i], "effect/coverage",
              L$problem[i], "FAIL")

  ## -- Cost components ---------------------------------------------------------
  sel_int <- unique(map_sel$intervention_id)
  C <- cst[, .(cost_record_id, cost_component_key, cost_option,
               selected_for_base_case      = as.integer(numv(selected_for_base_case)),
               intervention_id, cause_id, cost_join_key, cost_component,
               population_in_need_measure  = tolower(chrv(population_in_need_measure)),
               population_in_need_fraction = numv(population_in_need_fraction),
               frequency_per_year          = numv(frequency_per_year),
               c_age_start = numv(age_start), c_age_stop = numv(age_stop),
               c_sex = chrv(sex),
               unit_cost_usd = numv(unit_cost_usd),
               price_year    = as.integer(numv(price_year)),
               indonesia_adjusted_flag = as.integer(numv(indonesia_adjusted_flag)),
               cost_review   = chrv(review_status))]
  C <- C[intervention_id %in% sel_int]

  # Exactly one selected base-case row per in-scope cost_component_key ----------
  C[, n_sel := sum(selected_for_base_case == 1L, na.rm = TRUE), by = cost_component_key]
  sel_counts <- unique(C[, .(cost_component_key, n_sel)])
  for (i in seq_len(nrow(sel_counts))) {
    if (isTRUE(sel_counts$n_sel[i] > 1L))
      add_issue("cost", sel_counts$cost_component_key[i], "selected_for_base_case",
                "more than one base-case option selected", "FAIL")
    if (isTRUE(sel_counts$n_sel[i] == 0L))
      add_issue("cost", sel_counts$cost_component_key[i], "selected_for_base_case",
                "no base-case cost option selected (component omitted from costing)", "REVIEW")
  }

  Cbase <- C[selected_for_base_case == 1L]
  valid_cjk <- unique(chrv(map$cost_join_key))
  Cbase[, cause_code := cause_id2code[cause_id]]  # NA for C_SHARED

  padc <- function(dt, cond, field, msg, severity) {
    cond[is.na(cond)] <- FALSE
    if (any(cond))
      for (i in which(cond))
        add_issue("cost", dt$cost_record_id[i], field, msg, severity)
  }
  padc(Cbase, is.na(Cbase$unit_cost_usd) | Cbase$unit_cost_usd < 0, "unit_cost_usd",
       "missing or negative unit cost on a selected base-case row", "FAIL")
  padc(Cbase, is.na(Cbase$frequency_per_year) | Cbase$frequency_per_year < 0, "frequency_per_year",
       "missing or negative frequency", "FAIL")
  padc(Cbase, is.na(Cbase$population_in_need_fraction) |
         Cbase$population_in_need_fraction < 0 | Cbase$population_in_need_fraction > 1,
       "population_in_need_fraction", "PIN fraction missing/out of [0,1]", "FAIL")
  padc(Cbase, !(chrv(Cbase$cost_join_key) %in% valid_cjk), "cost_join_key",
       "cost_join_key not present in Intervention_Cause_Map", "FAIL")
  padc(Cbase, !(Cbase$population_in_need_measure %in% c("all", "prevalence", "incidence")),
       "population_in_need_measure", "unsupported PIN measure", "FAIL")
  padc(Cbase, Cbase$indonesia_adjusted_flag == 0L, "indonesia_adjusted_flag",
       "cost not Indonesia-adjusted (flagged; not treated as ready)", "REVIEW")
  padc(Cbase, !is.na(Cbase$price_year) & Cbase$price_year != cost_price_year, "price_year",
       paste0("cost price year != reporting price year (", cost_price_year, ")"), "REVIEW")

  # cost_scope per cost_join_key (from the map) --------------------------------
  scope_by_cjk <- unique(map[, .(cost_join_key = chrv(cost_join_key), cost_scope = chrv(cost_scope))])
  scope_by_cjk <- scope_by_cjk[, .(cost_scope = cost_scope[1]), by = cost_join_key]
  Cbase[, cost_join_key := chrv(cost_join_key)]
  Cbase <- merge(Cbase, scope_by_cjk, by = "cost_join_key", all.x = TRUE)

  # Coverage spec for each cost record (link-level, else intervention-level) ---
  cov_by_key <- cov_k[, .(cost_join_key = intervention_cause_key,
                          cb = baseline_coverage, ct = target_coverage,
                          cs = start_year, cty = target_year)]
  cov_int <- merge(map_sel[, .(intervention_cause_key, intervention_id)],
                   cov_k[, .(intervention_cause_key, baseline_coverage, target_coverage,
                             start_year, target_year)],
                   by = "intervention_cause_key")
  cov_int_u <- cov_int[, .(ib = mean(baseline_coverage, na.rm = TRUE),
                           it = mean(target_coverage, na.rm = TRUE),
                           is = suppressWarnings(min(start_year, na.rm = TRUE)),
                           ity = suppressWarnings(max(target_year, na.rm = TRUE)),
                           n_traj = uniqueN(paste(baseline_coverage, target_coverage,
                                                  start_year, target_year))),
                       by = intervention_id]
  Cbase <- merge(Cbase, cov_by_key,  by = "cost_join_key",  all.x = TRUE)
  Cbase <- merge(Cbase, cov_int_u,   by = "intervention_id", all.x = TRUE)
  Cbase[is.na(cb), `:=`(cb = ib, ct = it, cs = is, cty = ity)]
  # Shared cost that fell back to an ambiguous intervention-level trajectory
  padc(Cbase, is.na(Cbase$cb), "coverage",
       "no coverage trajectory found for cost record", "FAIL")
  setnames(Cbase, c("cb", "ct", "cs", "cty"),
           c("cov_baseline", "cov_target", "cov_start_year", "cov_target_year"))
  Cbase[, c("ib", "it", "is", "ity") := NULL]

  ## -- Runnable interventions & scenario catalogue ----------------------------
  valid_links   <- L[valid == TRUE]
  runnable_ints <- unique(valid_links$intervention_id)
  blocked_ints  <- setdiff(sel_int, runnable_ints)

  to_engine <- function(dd)
    dd[, .(intervention_id, intervention_cause_key, cause_code, model_transition,
           effect_value, affected_fraction, baseline_coverage, target_coverage,
           start_year, target_year, age_start = e_age_start, age_stop = e_age_stop,
           sex = e_sex)]

  scen <- list()
  scen[[baseline_id]] <- list(scenario_id = baseline_id,
                              scenario_label = "Baseline (no new intervention)",
                              intervention_ids = character(0),
                              interventions = character(0),
                              fair_effect_rows = NULL)
  for (iid in runnable_ints) {
    nm <- unique(map_sel[intervention_id == iid, intervention_name])[1]
    scen[[iid]] <- list(scenario_id = iid, scenario_label = nm,
                        intervention_ids = iid, interventions = "fair_wb",
                        fair_effect_rows = to_engine(valid_links[intervention_id == iid]))
  }
  if (length(runnable_ints) >= 2L)
    scen[["all"]] <- list(scenario_id = "all",
                          scenario_label = "All selected interventions (combined)",
                          intervention_ids = runnable_ints,
                          interventions = "fair_wb",
                          fair_effect_rows = to_engine(valid_links))

  ## -- Assemble fair_inputs (consumed by Model 09) ----------------------------
  fair_inputs <- list(
    links          = L,
    valid_links    = valid_links,
    blocked_links  = L[valid == FALSE],
    costs          = Cbase,
    cost_all       = C,
    validation     = issues,
    cause_translation = data.table(cause_id = names(cause_id2code),
                                   cause_code = unname(cause_id2code)),
    runnable_interventions = runnable_ints,
    blocked_interventions  = blocked_ints,
    inputs_path    = inputs_path,
    baseline_scenario_id = baseline_id,
    assumptions    = list(
      analysis_start_year     = analysis_start_year,
      analysis_end_year       = analysis_end_year,
      intervention_start_year = intervention_start_year,
      coverage_target_year    = coverage_target_year,
      target_coverage_default = target_coverage_default,
      cost_discount_rate      = cost_discount_rate,
      health_discount_rate    = health_discount_rate,
      cost_price_year         = cost_price_year,
      currency                = getA("currency", "USD"),
      scale_up_shape          = getA("scale_up_shape", "linear"),
      downstream_cost_offsets = as.integer(numv(getA("downstream_cost_offsets", 0))),
      rhd_surgery_frequency   = numv(getA("rhd_surgery_frequency", 1)),
      economic_perspective    = getA("economic_perspective", "health_system")))

  ## -- Report ------------------------------------------------------------------
  n_fail <- sum(issues$severity == "FAIL")
  n_rev  <- sum(issues$severity == "REVIEW")
  cat("\n--- FAIR Choices workbook catalogue ---------------------------------\n")
  cat(sprintf("Workbook: %s\n", inputs_path))
  cat(sprintf("Selected links: %d | valid: %d | invalid: %d\n",
              nrow(map_sel), nrow(valid_links), nrow(L[valid == FALSE])))
  cat(sprintf("Runnable interventions (%d): %s\n",
              length(runnable_ints), paste(runnable_ints, collapse = ", ")))
  if (length(blocked_ints))
    cat(sprintf("BLOCKED interventions (%d): %s\n",
                length(blocked_ints), paste(blocked_ints, collapse = ", ")))
  cat(sprintf("Selected base-case cost rows: %d\n", nrow(Cbase)))
  cat(sprintf("Validation issues: %d FAIL, %d REVIEW\n", n_fail, n_rev))
  if (nrow(issues)) {
    cat("Consolidated validation diagnostic:\n")
    print(issues)
  }
  cat(sprintf("Scenarios built (%d): %s\n",
              length(scen), paste(names(scen), collapse = ", ")))
  cat("---------------------------------------------------------------------\n\n")

  if (strict && n_fail > 0L)
    stop("FAIR: strict_model_input_validation = TRUE and ", n_fail,
         " FAIL-level workbook issue(s) present (see diagnostic above). ",
         "Resolve them or set strict_model_input_validation = FALSE to run only ",
         "the valid scenarios.", call. = FALSE)

  list(scenarios = scen, inputs = fair_inputs)
}

#===========================================================================
# 70-30-30 -> 70-70-70 CASCADE CATALOGUE  (opt-in; feeds Models 06 and 09) ----
#---------------------------------------------------------------------------
# Dedicated reader for the BESPOKE cascade workbook
# (indonesia_70_30_30_to_70_70_70_inputs.xlsx). It emits fair_scenarios and
# fair_inputs in EXACTLY the shapes Models 06 and 09 already consume, but for a
# SINGLE scenario (baseline + S_70_30_30_TO_70_70_70). The two component
# interventions (I_CVD_PRIMARY, I_T2D_TREATMENT) are retained only as traceable
# effect-row components of that one scenario -- never emitted as separate
# scenarios. It is called ONLY when run_cascade_70_30_30_to_70_70_70 is TRUE; the
# ordinary run path (.build_fair_catalogue above) is untouched.
#
# Why a dedicated reader (not the FAIR reader): the cascade workbook's Coverage /
# Coverage_Trajectory sheets are a milestone-cascade format (diagnosis x treatment
# x control with a half-effect for treated-but-uncontrolled), NOT the FAIR
# baseline_coverage/target_override contract. Effect_Sizes, Intervention_Cause_Map
# and Cost_Components ARE in the standard FAIR schema, so their parsing/validation
# mirror .build_fair_catalogue.
#
# EXACTNESS: the per-year effective-coverage path is taken VERBATIM from the
# workbook's Coverage_Trajectory$scenario_effective_coverage (which already bakes
# in the piecewise 2030/2040 milestones, the 0.5 treated-uncontrolled half effect,
# the cholesterol-follows-hypertension rule and the no-backsliding rule). It is
# attached as a per-row `coverage_path` list-column that Model 06's
# calculate_fair_workbook_impact() consumes verbatim, so R reproduces the
# workbook's own Model_Input_View$transition_multiplier to machine precision (an
# in-function reconciliation asserts this). Never re-derives or rounds cascade %.
.build_cascade_catalogue <- function(inputs_path,
                                     cause_map,
                                     strict              = FALSE,
                                     baseline_id         = "baseline",
                                     cascade_scenario_id = "S_70_30_30_TO_70_70_70",
                                     cascade_family      = "cascade_70_30_30_to_70_70_70") {

  if (!file.exists(inputs_path))
    stop("Cascade: input workbook not found: ", inputs_path)

  req_sheets <- c("Assumptions", "Dictionaries", "Intervention_Cause_Map",
                  "Effect_Sizes", "Coverage", "Coverage_Trajectory",
                  "Cost_Components", "Model_Input_View", "QA_Checks")
  have <- readxl::excel_sheets(inputs_path)
  miss <- setdiff(req_sheets, have)
  if (length(miss))
    stop("Cascade: workbook missing required sheet(s): ", paste(miss, collapse = ", "))

  rd   <- function(sheet) as.data.table(readxl::read_excel(inputs_path, sheet = sheet))
  numv <- function(x) suppressWarnings(as.numeric(x))
  chrv <- function(x) trimws(as.character(x))

  issues <- data.table(scope = character(), item_key = character(), field = character(),
                       problem = character(), severity = character())
  add_issue <- function(scope, item_key, field, problem, severity = "FAIL")
    issues <<- rbind(issues, data.table(scope = scope, item_key = as.character(item_key),
                                        field = field, problem = problem, severity = severity))

  ## -- Assumptions (parameter_id -> value) ------------------------------------
  asmp <- rd("Assumptions")
  A    <- setNames(as.character(asmp$value), as.character(asmp$parameter_id))
  getA <- function(id, default = NA) if (id %in% names(A)) A[[id]] else default

  analysis_start_year <- as.integer(numv(getA("analysis_start_year", 2025)))
  analysis_end_year   <- as.integer(numv(getA("analysis_end_year",   2050)))
  first_target_year   <- as.integer(numv(getA("first_target_year",   2030)))
  final_target_year   <- as.integer(numv(getA("final_target_year",   2040)))
  cost_discount_rate  <- numv(getA("cost_discount_rate",   0.03))
  health_discount_rate<- numv(getA("health_discount_rate", 0.03))
  cost_price_year     <- as.integer(numv(getA("cost_price_year", 2023)))
  eff_2030 <- numv(getA("first_target_effective_coverage_exact", NA_real_))
  eff_2040 <- numv(getA("final_target_effective_coverage_exact", NA_real_))
  partial_effect_fraction <- numv(getA("treated_uncontrolled_effect_fraction", 0.5))
  prevent_backsliding <- as.integer(numv(getA("prevent_coverage_backsliding", 1)))
  scen_id_wb  <- chrv(getA("scenario_id", cascade_scenario_id))
  if (!is.na(scen_id_wb) && nzchar(scen_id_wb) && !identical(scen_id_wb, cascade_scenario_id))
    add_issue("assumptions", "scenario_id", "scenario_id",
              sprintf("workbook scenario_id '%s' != expected '%s'", scen_id_wb, cascade_scenario_id),
              "REVIEW")
  if (is.na(eff_2030) || is.na(eff_2040))
    add_issue("assumptions", "effective_coverage", "value",
              "first/final_target_effective_coverage_exact missing or non-numeric", "FAIL")

  ## -- cause_id -> model cause short code (same table as the FAIR reader) ------
  cause_id2code <- c(C_RHD = "rhd", C_IHD = "ihd", C_IS = "istroke",
                     C_ICH = "hstroke", C_HHD = "hhd", C_CMP = "cmd", C_T2D = "dm2")
  bad_codes <- setdiff(unname(cause_id2code), names(cause_map))
  if (length(bad_codes))
    stop("Cascade: cause translation maps to code(s) absent from Model 00 cause_map: ",
         paste(bad_codes, collapse = ", "))
  translate_transition <- function(from, to) {
    from <- tolower(chrv(from)); to <- tolower(chrv(to))
    out <- rep(NA_character_, length(from))
    out[from == "well" & to == "sick"] <- "incidence"
    out[from %in% c("sick", "sick_severe", "sick_hf") & grepl("^dead", to)] <- "case_fatality"
    out
  }
  # Workbook sex labels ("Men"/"Women") -> model rate-table labels ("Male"/"Female").
  sex_wb2model <- c(Men = "Male", Women = "Female",
                    Male = "Male", Female = "Female", Both = "Both")

  ## -- Read the contract sheets -----------------------------------------------
  map <- rd("Intervention_Cause_Map")
  eff <- rd("Effect_Sizes")
  cst <- rd("Cost_Components")
  miv <- rd("Model_Input_View")
  ctr <- rd("Coverage_Trajectory")

  map[, include_flag := .normalize_include_flag(
        include_flag, sprintf("Intervention_Cause_Map (%s)", basename(inputs_path)))]
  map_sel <- map[include_flag == 1L]

  ## -- Effect parameters per link ---------------------------------------------
  eff_k <- eff[, .(intervention_cause_key,
                   effect_value      = numv(effect_value),
                   affected_fraction = numv(affected_fraction),
                   e_age_start       = numv(age_start),
                   e_age_stop        = numv(age_stop),
                   e_sex             = chrv(sex),
                   effect_review     = chrv(review_status))]
  eff_n <- eff[, .(n_eff = .N), by = intervention_cause_key]

  ## -- Per-year effective-coverage trajectory (authoritative; verbatim) -------
  ctr_k <- ctr[, .(intervention_id = chrv(intervention_id),
                   risk_factor_id  = chrv(risk_factor_id),
                   sex             = chrv(sex),
                   year            = as.integer(year),
                   baseline_effective_coverage = numv(baseline_effective_coverage),
                   scenario_effective_coverage = numv(scenario_effective_coverage))]
  # Monotonicity / range guard on the supplied trajectory (mirrors workbook QA06).
  setorder(ctr_k, intervention_id, sex, year)
  ctr_k[, d := scenario_effective_coverage - shift(scenario_effective_coverage),
        by = .(intervention_id, sex)]
  if (nrow(ctr_k[!is.na(d) & d < -1e-9]))
    add_issue("coverage", "Coverage_Trajectory", "scenario_effective_coverage",
              "coverage decreases in some year (violates no-backsliding)", "FAIL")
  if (nrow(ctr_k[scenario_effective_coverage < -1e-9 | scenario_effective_coverage > 1 + 1e-9]))
    add_issue("coverage", "Coverage_Trajectory", "scenario_effective_coverage",
              "coverage outside [0,1]", "FAIL")
  ctr_k[, d := NULL]

  ## -- Assemble link table + per-link validation ------------------------------
  L <- merge(map_sel[, .(intervention_cause_key, intervention_id, intervention_name,
                         cause_id, model_name, cost_join_key, cost_scope,
                         transition_from, transition_to)],
             eff_k, by = "intervention_cause_key", all.x = TRUE)
  L <- merge(L, eff_n, by = "intervention_cause_key", all.x = TRUE)
  L[is.na(n_eff), n_eff := 0L]
  L[, model_transition := translate_transition(transition_from, transition_to)]
  L[, cause_code       := cause_id2code[cause_id]]
  # Every included link must have a coverage trajectory for its intervention_id.
  ints_with_traj <- unique(ctr_k$intervention_id)
  L[, has_traj := intervention_id %in% ints_with_traj]
  # Single-value coverage SUMMARY per link, for Model 09's Selected_Interventions
  # display and its adjusted-effect-at-target summary column. The engine itself
  # uses the exact per-year `coverage_path`; baseline_coverage is the sex-averaged
  # 2025 effective coverage and target_coverage the 2040 milestone (full scale-up).
  base_by_int <- ctr_k[year == min(year), .(bl = mean(baseline_effective_coverage)),
                       by = intervention_id]
  L <- merge(L, base_by_int, by = "intervention_id", all.x = TRUE)
  L[, `:=`(baseline_coverage = bl,
           target_coverage   = eff_2040,
           start_year        = analysis_start_year,
           target_year       = final_target_year,
           coverage_review   = "OK (cascade trajectory)")]
  L[, bl := NULL]

  in01 <- function(x) !is.na(x) & x >= 0 & x <= 1
  L[, problem := ""]
  padd <- function(cond, msg) {
    cond[is.na(cond)] <- FALSE
    L[cond, problem := paste0(problem, ifelse(nchar(problem) > 0L, "; ", ""), msg)]
  }
  padd(L$n_eff != 1L,               "effect match != 1")
  padd(!in01(L$effect_value),       "effect_value missing/out of [0,1]")
  padd(!in01(L$affected_fraction),  "affected_fraction missing/out of [0,1]")
  padd(is.na(L$model_transition),   "transition label not mapped to model")
  padd(is.na(L$cause_code),         "cause_id absent from Model 00 cause_map")
  padd(!L$has_traj,                 "no coverage trajectory for intervention_id")
  L[, valid := problem == ""]
  for (i in which(!L$valid))
    add_issue("health_link", L$intervention_cause_key[i], "effect/coverage", L$problem[i], "FAIL")

  ## -- Build the sex-split engine effect rows with exact coverage paths --------
  # One row per (valid link x sex). Each carries the row's baseline effective
  # coverage (FAIR anchor) and a `coverage_path` list-column = the exact per-year
  # scenario_effective_coverage. Model 06 masks by sex (Male/Female) and applies
  # the path verbatim.
  build_engine_rows <- function(links) {
    if (!nrow(links)) return(NULL)
    out <- vector("list", 0L)
    for (i in seq_len(nrow(links))) {
      lk <- links[i]
      for (sx_wb in c("Men", "Women")) {
        cp <- ctr_k[intervention_id == lk$intervention_id & sex == sx_wb,
                    .(year, coverage_t = scenario_effective_coverage)][order(year)]
        if (!nrow(cp)) {
          add_issue("health_link", lk$intervention_cause_key, "coverage_path",
                    sprintf("no Coverage_Trajectory rows for %s/%s", lk$intervention_id, sx_wb), "FAIL")
          next
        }
        base_cov <- ctr_k[intervention_id == lk$intervention_id & sex == sx_wb,
                          baseline_effective_coverage][1]
        out[[length(out) + 1L]] <- data.table(
          intervention_id        = lk$intervention_id,
          intervention_cause_key = lk$intervention_cause_key,
          cause_code             = lk$cause_code,
          model_transition       = lk$model_transition,
          effect_value           = lk$effect_value,
          affected_fraction      = lk$affected_fraction,
          baseline_coverage      = base_cov,
          target_coverage        = eff_2040,
          start_year             = analysis_start_year,
          target_year            = final_target_year,
          age_start              = lk$e_age_start,
          age_stop               = lk$e_age_stop,
          sex                    = unname(sex_wb2model[[sx_wb]]),
          coverage_path          = list(cp))     # per-row per-year trajectory
      }
    }
    if (length(out)) rbindlist(out) else NULL
  }

  valid_links   <- L[valid == TRUE]
  runnable_ints <- unique(valid_links$intervention_id)
  blocked_ints  <- setdiff(unique(map_sel$intervention_id), runnable_ints)
  engine_rows   <- build_engine_rows(valid_links)

  ## -- Reconcile against the workbook's own Model_Input_View -------------------
  # For each (link, sex, year) recompute the surviving transition multiplier with
  # the SAME FAIR formula Model 06 uses and confirm it matches the workbook's
  # transition_multiplier. Any mismatch is a hard FAIL (adapter would not
  # reproduce the workbook).
  if (nrow(miv)) {
    mivx <- miv[, .(intervention_cause_key = chrv(intervention_cause_key),
                    sex = chrv(sex), year = as.integer(year),
                    effect_value = numv(effect_value),
                    affected_fraction = numv(affected_fraction),
                    base = numv(baseline_effective_coverage),
                    cov  = numv(scenario_effective_coverage),
                    tm_wb = numv(transition_multiplier))]
    mivx[, e_adj := effect_value * (cov - base) / (1 - effect_value * base)]
    mivx[, tm_me := 1 - affected_fraction * e_adj]
    mivx[, dabs  := abs(tm_me - tm_wb)]
    max_recon <- suppressWarnings(max(mivx$dabs, na.rm = TRUE))
    if (is.finite(max_recon) && max_recon > 1e-6)
      add_issue("reconciliation", "Model_Input_View", "transition_multiplier",
                sprintf("adapter multiplier differs from workbook by up to %.3e", max_recon), "FAIL")
  } else max_recon <- NA_real_

  ## -- Cost components (standard FAIR schema; cascade coverage path) -----------
  sel_int <- unique(map_sel$intervention_id)
  C <- cst[, .(cost_record_id, cost_component_key, cost_option,
               selected_for_base_case      = as.integer(numv(selected_for_base_case)),
               intervention_id, cause_id, cost_join_key, cost_component,
               population_in_need_measure  = tolower(chrv(population_in_need_measure)),
               population_in_need_fraction = numv(population_in_need_fraction),
               frequency_per_year          = numv(frequency_per_year),
               c_age_start = numv(age_start), c_age_stop = numv(age_stop),
               c_sex = chrv(sex),
               unit_cost_usd = numv(unit_cost_usd),
               price_year    = as.integer(numv(price_year)),
               indonesia_adjusted_flag = as.integer(numv(indonesia_adjusted_flag)),
               cost_review   = chrv(review_status))]
  C <- C[intervention_id %in% sel_int]
  C[, n_sel := sum(selected_for_base_case == 1L, na.rm = TRUE), by = cost_component_key]
  sel_counts <- unique(C[, .(cost_component_key, n_sel)])
  for (i in seq_len(nrow(sel_counts))) {
    if (isTRUE(sel_counts$n_sel[i] > 1L))
      add_issue("cost", sel_counts$cost_component_key[i], "selected_for_base_case",
                "more than one base-case option selected", "FAIL")
    if (isTRUE(sel_counts$n_sel[i] == 0L))
      add_issue("cost", sel_counts$cost_component_key[i], "selected_for_base_case",
                "no base-case cost option selected (component omitted from costing)", "REVIEW")
  }
  Cbase <- C[selected_for_base_case == 1L]
  valid_cjk <- unique(chrv(map$cost_join_key))
  Cbase[, cause_code := cause_id2code[cause_id]]

  padc <- function(dt, cond, field, msg, severity) {
    cond[is.na(cond)] <- FALSE
    if (any(cond)) for (i in which(cond)) add_issue("cost", dt$cost_record_id[i], field, msg, severity)
  }
  padc(Cbase, is.na(Cbase$unit_cost_usd) | Cbase$unit_cost_usd < 0, "unit_cost_usd",
       "missing or negative unit cost on a selected base-case row", "FAIL")
  padc(Cbase, is.na(Cbase$frequency_per_year) | Cbase$frequency_per_year < 0, "frequency_per_year",
       "missing or negative frequency", "FAIL")
  padc(Cbase, is.na(Cbase$population_in_need_fraction) |
         Cbase$population_in_need_fraction < 0 | Cbase$population_in_need_fraction > 1,
       "population_in_need_fraction", "PIN fraction missing/out of [0,1]", "FAIL")
  padc(Cbase, !(chrv(Cbase$cost_join_key) %in% valid_cjk), "cost_join_key",
       "cost_join_key not present in Intervention_Cause_Map", "FAIL")
  padc(Cbase, !(Cbase$population_in_need_measure %in% c("all", "prevalence", "incidence")),
       "population_in_need_measure", "unsupported PIN measure", "FAIL")
  padc(Cbase, Cbase$indonesia_adjusted_flag == 0L, "indonesia_adjusted_flag",
       "cost not Indonesia-adjusted (flagged; not treated as ready)", "REVIEW")
  padc(Cbase, !is.na(Cbase$price_year) & Cbase$price_year != cost_price_year, "price_year",
       paste0("cost price year != reporting price year (", cost_price_year, ")"), "REVIEW")

  scope_by_cjk <- unique(map[, .(cost_join_key = chrv(cost_join_key), cost_scope = chrv(cost_scope))])
  scope_by_cjk <- scope_by_cjk[, .(cost_scope = cost_scope[1]), by = cost_join_key]
  Cbase[, cost_join_key := chrv(cost_join_key)]
  Cbase <- merge(Cbase, scope_by_cjk, by = "cost_join_key", all.x = TRUE)

  # Cost coverage = the SAME cascade effective-coverage path (sex-averaged, since
  # cost records are sex="Both"), so costs and health effects share one coverage
  # concept. Baseline/target endpoints exposed for display; the per-year path is
  # attached as a `coverage_path` list-column consumed by Model 09's cost loop.
  ctr_avg <- ctr_k[, .(coverage_t = mean(scenario_effective_coverage),
                       base_avg   = mean(baseline_effective_coverage)),
                   by = .(intervention_id, year)][order(intervention_id, year)]
  cov_int_u <- ctr_avg[, .(cov_baseline = base_avg[which.min(year)],
                           cov_target   = eff_2040,
                           cov_start_year = analysis_start_year,
                           cov_target_year = final_target_year),
                       by = intervention_id]
  Cbase <- merge(Cbase, cov_int_u, by = "intervention_id", all.x = TRUE)
  padc(Cbase, is.na(Cbase$cov_baseline), "coverage",
       "no cascade coverage trajectory found for cost record", "FAIL")
  cost_cov_path <- lapply(Cbase$intervention_id, function(iid)
    ctr_avg[intervention_id == iid, .(year, coverage_t)][order(year)])
  Cbase[, coverage_path := cost_cov_path]

  ## -- Scenario catalogue (baseline + the single cascade scenario) ------------
  scen <- list()
  scen[[baseline_id]] <- list(scenario_id = baseline_id,
                              scenario_label = "Baseline (no new intervention)",
                              intervention_ids = character(0),
                              interventions = character(0),
                              fair_effect_rows = NULL,
                              family = "baseline")
  scen[[cascade_scenario_id]] <- list(
    scenario_id      = cascade_scenario_id,
    scenario_label   = "70-30-30 -> 70-70-70 hypertension/cholesterol + diabetes cascade",
    intervention_ids = runnable_ints,
    interventions    = "fair_wb",
    fair_effect_rows = engine_rows,
    family           = cascade_family,
    scenario_role    = "combined",
    scenario_level   = "combined",
    parent_package_id = NA_character_,
    component_intervention_ids = runnable_ints)

  ## -- Assemble fair_inputs (consumed by Model 09) ----------------------------
  fair_inputs <- list(
    links          = L,
    valid_links    = valid_links,
    blocked_links  = L[valid == FALSE],
    costs          = Cbase,
    cost_all       = C,
    validation     = issues,
    cause_translation = data.table(cause_id = names(cause_id2code),
                                   cause_code = unname(cause_id2code)),
    runnable_interventions = runnable_ints,
    blocked_interventions  = blocked_ints,
    inputs_path    = inputs_path,
    baseline_scenario_id = baseline_id,
    # Cascade-specific bundle for Model 09's Cascade_* sheets (only present here).
    cascade = list(
      scenario_id = cascade_scenario_id, family = cascade_family,
      analysis_start_year = analysis_start_year, analysis_end_year = analysis_end_year,
      first_target_year = first_target_year, final_target_year = final_target_year,
      eff_2030 = eff_2030, eff_2040 = eff_2040,
      partial_effect_fraction = partial_effect_fraction,
      prevent_coverage_backsliding = prevent_backsliding,
      coverage_trajectory = ctr_k, coverage = rd("Coverage"),
      model_input_view = miv, qa_checks = rd("QA_Checks"),
      assumptions_sheet = asmp, recon_max_abs = max_recon),
    assumptions    = list(
      analysis_start_year     = analysis_start_year,
      analysis_end_year       = analysis_end_year,
      intervention_start_year = analysis_start_year,
      coverage_target_year    = final_target_year,
      target_coverage_default = eff_2040,
      cost_discount_rate      = cost_discount_rate,
      health_discount_rate    = health_discount_rate,
      cost_price_year         = cost_price_year,
      currency                = getA("currency", "USD"),
      scale_up_shape          = getA("scale_up_shape", "piecewise_linear"),
      downstream_cost_offsets = as.integer(numv(getA("downstream_cost_offsets", 0))),
      rhd_surgery_frequency   = numv(getA("rhd_surgery_frequency", 1)),
      economic_perspective    = getA("economic_perspective", "societal")))

  ## -- Report + scope assertion ------------------------------------------------
  n_fail <- sum(issues$severity == "FAIL")
  n_rev  <- sum(issues$severity == "REVIEW")
  cat("\n--- 70-30-30 -> 70-70-70 CASCADE catalogue --------------------------\n")
  cat(sprintf("Workbook: %s\n", inputs_path))
  cat(sprintf("Included links: %d | valid: %d | invalid: %d\n",
              nrow(map_sel), nrow(valid_links), nrow(L[valid == FALSE])))
  cat(sprintf("Component interventions (%d): %s\n",
              length(runnable_ints), paste(runnable_ints, collapse = ", ")))
  cat(sprintf("Engine effect rows (link x sex): %d\n",
              if (is.null(engine_rows)) 0L else nrow(engine_rows)))
  cat(sprintf("Effective coverage milestones: 2030 = %.10g | 2040 = %.10g\n", eff_2030, eff_2040))
  cat(sprintf("Model_Input_View reconciliation max |diff|: %.3e\n", max_recon))
  cat(sprintf("Selected base-case cost rows: %d\n", nrow(Cbase)))
  cat(sprintf("Validation issues: %d FAIL, %d REVIEW\n", n_fail, n_rev))
  if (nrow(issues)) { cat("Consolidated validation diagnostic:\n"); print(issues) }
  cat(sprintf("Scenarios built (%d): %s\n", length(scen), paste(names(scen), collapse = ", ")))
  cat("---------------------------------------------------------------------\n\n")

  # After Model 04: the catalogue must contain ONLY baseline + the cascade scenario.
  if (!setequal(names(scen), c(baseline_id, cascade_scenario_id)))
    stop("Cascade: scenario catalogue must be exactly {", baseline_id, ", ",
         cascade_scenario_id, "}; got {", paste(names(scen), collapse = ", "), "}.",
         call. = FALSE)

  if (strict && n_fail > 0L)
    stop("Cascade: strict_model_input_validation = TRUE and ", n_fail,
         " FAIL-level workbook issue(s) present (see diagnostic above).", call. = FALSE)
  if (n_fail > 0L)
    stop("Cascade: ", n_fail, " FAIL-level issue(s) in the cascade workbook (see ",
         "diagnostic above); the cascade run requires a clean catalogue.", call. = FALSE)

  list(scenarios = scen, inputs = fair_inputs)
}

# --- Choose the catalogue builder: cascade (opt-in) or ordinary FAIR Choices ---
if (isTRUE(get0("run_cascade_70_30_30_to_70_70_70", ifnotfound = FALSE))) {
  .fair_built  <- .build_cascade_catalogue(
    model_inputs_file, cause_map,
    strict              = strict_model_input_validation,
    baseline_id         = baseline_scenario_id,
    cascade_scenario_id = if (exists("cascade_scenario_id")) cascade_scenario_id else "S_70_30_30_TO_70_70_70",
    cascade_family      = if (exists("cascade_family")) cascade_family else "cascade_70_30_30_to_70_70_70")
} else {
  .fair_built  <- .build_fair_catalogue(model_inputs_file, cause_map,
                                        strict      = strict_model_input_validation,
                                        baseline_id = baseline_scenario_id)
}
fair_scenarios <- .fair_built$scenarios
fair_inputs    <- .fair_built$inputs
rm(.fair_built)


#===========================================================================
# PUBLIC-HEALTH WORKBOOK-DRIVEN CATALOGUE  (feeds Models 06 and 09) ----
#---------------------------------------------------------------------------
# The public-health (fiscal / regulatory policy) family is a SEPARATE catalogue
# from the clinical FAIR Choices one above. It is driven entirely by the
# public-health input workbook (path supplied by Model 00 as
# `public_health_inputs_file`, e.g. indonesia_model_inputs_ public_health.xlsx).
# Every relevant column is read by NAME (never by fixed cell), and every derived
# value (exposure target, exposure path, full effect at target) is REPRODUCED in
# R -- cached Excel formula results are not trusted and are only used for a QA
# cross-check against the R reproduction.
#
# Two objects are exported (mirroring the clinical fair_* objects but never
# overwriting them):
#   * public_health_scenarios : named list consumed by Model 06 -- baseline +
#       one scenario per runnable public-health intervention + a combined
#       "all_public_health" scenario (when >= 2 interventions are runnable).
#       Each non-baseline entry carries interventions = "ph_wb" and the
#       validated per-link `ph_effect_rows`.
#   * public_health_inputs    : list of validated catalogues consumed by Model 09
#       (links, exposure paths, effect parameters, policy levers, costs,
#       assumptions, validation report, blocked links, cause/transition maps and
#       the raw workbook views used to build the audit sheets).
#
# KEY DIFFERENCES from the clinical workbook (absorbed here, not forced onto the
# clinical schema):
#   * policy_start_year / exposure_target_year (NOT coverage-year fields);
#   * Effect_Parameters (NOT Effect_Sizes); Exposure_Targets (NOT Coverage);
#   * `population` PIN measure (public-wide policy) -> the model's all-population
#     quantity (deduplicated, once per age/sex/year -- never summed over causes);
#   * public-health cause ids C_T2DM -> dm2 and C_CMYO -> cmd (plus the shared
#     C_IHD/C_IS/C_ICH/C_HHD/C_RHD mappings);
#   * shared policy costs keyed by C_SHARED with cost_scope "shared-count-once".
#
# Effect models implemented (exposure-based; NOT the clinical coverage formula):
#   1. direct_smoking_prevalence_shift_rr
#        effect = 1 - (1 + pt*(RR-1)) / (1 + p0*(RR-1))
#   2. direct_loglinear_rr_per_unit_reduction
#        effect = 1 - 1 / (RR ^ (p0 - pt))          # delta = p0 - pt
#        This is the DEFAULT industrial-TFA path (RR per 1 percentage-point energy,
#        e.g. RR_TFA_IHD_1PCT = 1.10); it needs no PAF.
#   3. tfa_attributable_ihd_PAF_x_regulatory_gap (optional; only when
#        Assumptions$tfa_effect_method = "PAF")
#        effect = optional_PAF * implementation_gap
# The exposure path pt = pt(t) (baseline -> target over start..target year,
# floored) and the lag model are applied per YEAR in Model 06; here we reproduce
# only the target-exposure full effect (illustrative_full_effect) for validation
# and for the Model 09 audit sheets. Public-health effects map to well -> sick
# incidence ONLY (no case-fatality transition in this contract).
#===========================================================================

# Execution metaparameters: single source of truth is Model 00. Provide safe
# fallbacks so this script can also be sourced stand-alone.
if (!exists("run_public_health_interventions"))
  run_public_health_interventions <- TRUE
if (!exists("public_health_inputs_file")) {
  public_health_inputs_file <- paste0(wd, "data/indonesia_model_inputs_public_health_updated.xlsx")
  if (!file.exists(public_health_inputs_file))
    public_health_inputs_file <- paste0(wd, "data/indonesia_model_inputs_public_health.xlsx")
}

.build_public_health_catalogue <- function(inputs_path,
                                           cause_map,
                                           strict      = FALSE,
                                           baseline_id = "baseline",
                                           # Timing model for the tobacco-CVD effect (both incidence
                                           # and the sick -> dead mortality proxy). "base" uses the
                                           # workbook Jha age-sex-duration scalars; the sensitivity
                                           # option overrides only tobacco timing. Read by name; no
                                           # numeric assumption is set here.
                                           tobacco_timing_analysis = "base") {

  if (!file.exists(inputs_path))
    stop("Public-health: input workbook not found: ", inputs_path)

  req_sheets <- c("Assumptions", "Intervention_Cause_Map", "Policy_Levers",
                  "Exposure_Targets", "Effect_Parameters", "Risk_Response",
                  "Model_Input_View", "Cost_Components", "Scenario_Hierarchy")
  have <- readxl::excel_sheets(inputs_path)
  miss <- setdiff(req_sheets, have)
  if (length(miss))
    stop("Public-health: workbook missing required sheet(s): ",
         paste(miss, collapse = ", "))

  rd   <- function(sheet) as.data.table(readxl::read_excel(inputs_path, sheet = sheet))
  numv <- function(x) suppressWarnings(as.numeric(as.character(x)))
  chrv <- function(x) trimws(as.character(x))

  # Optional (audit-only) sheets -- read if present, else NULL.
  rd_opt <- function(sheet) if (sheet %in% have) rd(sheet) else NULL

  ## -- Consolidated validation issue log --------------------------------------
  issues <- data.table(scope = character(), item_key = character(), field = character(),
                       problem = character(), severity = character())
  add_issue <- function(scope, item_key, field, problem, severity = "FAIL")
    issues <<- rbind(issues, data.table(scope = scope, item_key = as.character(item_key),
                                        field = field, problem = problem,
                                        severity = severity))

  ## -- Assumptions (parameter_id -> value) ------------------------------------
  asmp <- rd("Assumptions")
  A    <- setNames(as.character(asmp$value), as.character(asmp$parameter_id))
  getA <- function(id, default = NA) if (id %in% names(A)) A[[id]] else default

  analysis_start_year   <- as.integer(numv(getA("analysis_start_year", 2025)))
  analysis_end_year     <- as.integer(numv(getA("analysis_end_year", 2050)))
  policy_start_year     <- as.integer(numv(getA("policy_start_year", analysis_start_year + 1L)))
  exposure_target_year  <- as.integer(numv(getA("exposure_target_year", 2030)))
  policy_cost_ramp_years <- numv(getA("policy_cost_ramp_years", 3))
  cost_discount_rate    <- numv(getA("cost_discount_rate", 0.03))
  health_discount_rate  <- numv(getA("health_discount_rate", 0.03))
  reporting_price_year  <- as.integer(numv(getA("reporting_price_year", 2023)))
  source_cost_price_year <- as.integer(numv(getA("source_cost_price_year", 2017)))
  scale_up_shape        <- chrv(getA("scale_up_shape", "linear"))
  # TFA effect method switch (RR default; PAF optional). Regulatory scores map the
  # none/partial/full implementation categories (M03). All read by name.
  tfa_effect_method     <- toupper(chrv(getA("tfa_effect_method", "RR")))
  if (!(tfa_effect_method %in% c("RR", "PAF"))) tfa_effect_method <- "RR"
  tfa_optional_ihd_paf  <- numv(getA("tfa_optional_ihd_paf", NA))
  reg_none_score        <- numv(getA("regulatory_none_score", 0))
  reg_partial_score     <- numv(getA("regulatory_partial_score", 0.5))
  reg_full_score        <- numv(getA("regulatory_full_score", 1))

  # Tobacco-CVD timing + vascular-mortality parameters (all read by name; used to
  # build the Jha timing/RR config consumed by Model 06 and the pooled M16
  # illustrative here). Sex-specific vascular RRs (2.9 M / 3.1 F) are the applied
  # values; the pooled 3.0 / pooled ERD are illustrative fallbacks only.
  tob_lag_model_base   <- chrv(getA("tobacco_cvd_lag_model_base", "jha_piecewise_shared_scalar"))
  tob_lag_model_sens   <- chrv(getA("tobacco_cvd_lag_model_sensitivity", "normalized_exponential_lag"))
  tob_full_effect_year <- as.integer(numv(getA("tobacco_cvd_full_effect_year", 10)))
  tob_lag_rate         <- numv(getA("tobacco_cvd_lag_rate", 0.0616))
  tob_rr_mort_male     <- numv(getA("tobacco_vascular_mortality_rr_male",   2.9))
  tob_rr_mort_female   <- numv(getA("tobacco_vascular_mortality_rr_female", 3.1))
  tob_rr_mort_pooled   <- numv(getA("tobacco_vascular_mortality_rr_pooled", 3.0))
  tob_erd10_pooled     <- numv(getA("tobacco_vascular_erd_ge10_pooled",     0.90125))
  tob_age80_extrap     <- chrv(getA("tobacco_age80_scalar_extrapolation", "use_age_60_79"))
  # Selected timing model for THIS run (Model 00 switch): base Jha or the
  # normalized-exponential sensitivity. Only these two are valid.
  tob_timing_selected  <- if (identical(chrv(tobacco_timing_analysis), "normalized_exponential_lag"))
                            tob_lag_model_sens else tob_lag_model_base
  if (!tob_timing_selected %in% c("jha_piecewise_shared_scalar", "normalized_exponential_lag"))
    tob_timing_selected <- "jha_piecewise_shared_scalar"

  ## -- cause_id -> model cause short code (explicit translation table) --------
  # C_CMYO -> cmd and C_T2DM -> dm2 as required, plus the shared CVD mappings.
  # C_SHARED intentionally absent (shared-cost bucket; not a modeled cause).
  cause_id2code <- c(C_IHD = "ihd", C_IS = "istroke", C_ICH = "hstroke",
                     C_HHD = "hhd", C_RHD = "rhd", C_CMYO = "cmd", C_T2DM = "dm2")
  bad_codes <- setdiff(unname(cause_id2code), names(cause_map))
  if (length(bad_codes))
    stop("Public-health: cause translation maps to code(s) absent from Model 00 ",
         "cause_map: ", paste(bad_codes, collapse = ", "))

  ## -- transition translation (workbook from/to -> model transition) ----------
  # Public-health effects now map to BOTH modeled transitions: well -> sick
  # (incidence, IR/eff_ir) and sick -> dead (case fatality, CF/eff_cf -- the
  # tobacco vascular-mortality proxy and the exploratory SSB mortality link).
  # Anything outside this allowed pair returns NA and is rejected/flagged. This
  # mirrors the clinical .build_fair_catalogue translate_transition().
  translate_transition <- function(from, to) {
    from <- tolower(chrv(from)); to <- tolower(chrv(to))
    out <- rep(NA_character_, length(from))
    out[from == "well" & to == "sick"] <- "incidence"
    out[from %in% c("sick", "sick_severe", "sick_hf") & grepl("^dead", to)] <- "case_fatality"
    out
  }

  ## -- Exposure-path reproduction helpers (R, not Excel-cached) ---------------
  # Reproduce the Countdown target-exposure identity (M01/M02/M03).
  reproduce_target <- function(baseline, method, red_or_tgt, floor) {
    method <- tolower(chrv(method))
    out <- rep(NA_real_, length(baseline))
    rel <- method == "relative"
    abso <- method == "absolute"
    lvl <- method %in% c("target", "level")
    out[rel]  <- baseline[rel]  * (1 - red_or_tgt[rel])
    out[abso] <- baseline[abso] - red_or_tgt[abso]
    out[lvl]  <- red_or_tgt[lvl]
    pmax(floor, out)
  }
  # Target-exposure full proportional effect (ILLUSTRATIVE, single scalar per
  # link). This reproduces the workbook Model_Input_View illustrative_full_effect
  # for the QA cross-check ONLY; the applied, age-sex-year-specific effect (and,
  # for the mortality model, the sex-specific vascular RR and Jha timing) is
  # computed in Model 06. `erd10_pooled` is the pooled 10+ year excess-risk
  # reduction used by the workbook's illustrative pooled mortality formula (M16).
  reproduce_full_effect <- function(model, p0, pt, RR, paf, erd10_pooled = NA_real_) {
    model <- chrv(model)
    n <- length(model); e <- rep(NA_real_, n)
    sm <- model == "direct_smoking_prevalence_shift_rr"      # M08 tobacco incidence
    ll <- model == "direct_loglinear_rr_per_unit_reduction"  # M09 log-linear (alcohol/salt/TFA/SSB)
    mo <- model == "tobacco_mortality_prevalence_shift_rr"   # M16 tobacco sick -> dead
    tf <- grepl("^tfa_attributable_ihd_PAF", model)          # optional PAF path (any variant)
    e[sm] <- 1 - (1 + pt[sm] * (RR[sm] - 1)) / (1 + p0[sm] * (RR[sm] - 1))
    e[ll] <- 1 - 1 / (RR[ll] ^ (p0[ll] - pt[ll]))
    # M16 (illustrative, pooled): residual-risk model. RR[] carries the pooled
    # current-smoker vascular-mortality HR; erd10_pooled is the pooled 10+ year
    # ERD. Model 06 replaces both with sex/age-specific workbook values.
    if (any(mo)) {
      RRq <- 1 + (1 - erd10_pooled) * (RR[mo] - 1)
      e[mo] <- 1 - (1 + pt[mo] * (RR[mo] - 1) + (p0[mo] - pt[mo]) * (RRq - 1)) /
                   (1 + p0[mo] * (RR[mo] - 1))
    }
    # Optional TFA PAF path only: effect = PAF * implementation gap (RR[] carries the
    # gap/score). Used solely when Assumptions$tfa_effect_method = "PAF"; the RR base
    # case runs through the log-linear branch above and needs no PAF.
    e[tf] <- ifelse(is.na(paf[tf]), 0, paf[tf]) * ifelse(is.na(RR[tf]), 0, RR[tf])
    e
  }

  ## -- Read the contract sheets -----------------------------------------------
  map <- rd("Intervention_Cause_Map")
  exp <- rd("Exposure_Targets")
  eff <- rd("Effect_Parameters")
  lev <- rd("Policy_Levers")
  cst <- rd("Cost_Components")
  rr  <- rd("Risk_Response")
  miv <- rd("Model_Input_View")
  sh  <- rd("Scenario_Hierarchy")

  ## -- Scenario hierarchy (parent packages <-> child interventions) -----------
  # Read every field by name; classify individual interventions vs nested
  # tobacco/salt packages. Never hard-code the membership lists here.
  SH <- sh[, .(parent_scenario_id       = chrv(parent_scenario_id),
               parent_scenario_name     = chrv(parent_scenario_name),
               intervention_id          = chrv(intervention_id),
               intervention_name        = chrv(intervention_name),
               package_group            = chrv(package_group),
               scenario_role            = tolower(chrv(scenario_role)),
               include_in_parent_scenario = as.integer(numv(include_in_parent_scenario)),
               standalone_scenario_id   = chrv(standalone_scenario_id),
               parent_aggregation_rule  = chrv(parent_aggregation_rule),
               outcome_reporting_rule   = chrv(outcome_reporting_rule),
               cost_reporting_rule      = chrv(cost_reporting_rule),
               component_order          = as.integer(numv(component_order)),
               source_note              = chrv(source_note))]
  SH[is.na(include_in_parent_scenario), include_in_parent_scenario := 1L]
  # Parent packages = parent_scenario_id that own >= 1 child row.
  package_ids  <- unique(SH[scenario_role == "child", parent_scenario_id])
  children_of  <- function(pid) SH[scenario_role == "child" &
                                     parent_scenario_id == pid &
                                     include_in_parent_scenario == 1L, intervention_id]
  # Hierarchy QA (data-driven): a parent package must own at least one child in
  # the workbook hierarchy. The number of children is whatever the workbook
  # declares -- not a fixed count -- and the RUNNABLE children (hierarchy
  # children intersected with include_flag == 1) are computed later at scenario
  # assembly, where a package with no runnable child is simply omitted.
  for (pid in package_ids)
    if (!length(children_of(pid)))
      add_issue("ph_hierarchy", pid, "children",
                "parent package has no children in Scenario_Hierarchy", "FAIL")
  child_ids_all <- unique(SH[scenario_role == "child", intervention_id])
  for (cid in child_ids_all)
    if (!nzchar(unique(SH[intervention_id == cid, parent_scenario_id])[1]))
      add_issue("ph_hierarchy", cid, "parent_scenario_id", "child without a parent package", "FAIL")

  ## -- Policy levers: reproduce fiscal / regulatory quantities in R -----------
  # Read all fiscal, regulatory and hierarchy lever fields by name. Reproduce the
  # tax-share / price-change / regulatory-gap identities (Countdown M03-M06) and
  # keep the workbook's cached values only as QA comparators.
  level2score <- function(x) {
    x <- tolower(trimws(as.character(x)))
    out <- rep(NA_real_, length(x))
    out[x == "none"]    <- reg_none_score
    out[x == "partial"] <- reg_partial_score
    out[x %in% c("full", "best practice", "best-practice")] <- reg_full_score
    out
  }
  Lv <- lev[, .(
    lever_id = chrv(lever_id), intervention_id = chrv(intervention_id),
    intervention_name = chrv(intervention_name), component = chrv(component),
    lever_method = chrv(lever_method), baseline_value = numv(baseline_value),
    target_value = numv(target_value), lever_unit = chrv(lever_unit),
    effect_parameter = numv(effect_parameter),
    effect_parameter_unit = chrv(effect_parameter_unit),
    estimated_risk_reduction_wb = numv(estimated_risk_reduction),
    parent_package_id = chrv(parent_package_id),
    parent_package_name = chrv(parent_package_name),
    intervention_role = chrv(intervention_role),
    fiscal_baseline_tax_level = numv(fiscal_baseline_tax_level),
    fiscal_target_tax_level = numv(fiscal_target_tax_level),
    fiscal_tax_level_unit = chrv(fiscal_tax_level_unit),
    regulatory_baseline_level = chrv(regulatory_baseline_level),
    regulatory_target_level = chrv(regulatory_target_level),
    regulatory_baseline_score = numv(regulatory_baseline_score),
    regulatory_target_score = numv(regulatory_target_score),
    implementation_gap_wb = numv(implementation_gap),
    implied_price_change_wb = numv(implied_price_change),
    fiscal_tax_delta_wb = numv(fiscal_tax_delta),
    lever_review = chrv(review_status), lever_qa = chrv(qa_status))]
  Lv[, method := tolower(lever_method)]
  # Regulatory implementation scores: prefer the none/partial/full level strings
  # (M03); fall back to the explicit numeric score columns when a level is blank.
  Lv[, reg_baseline_score := fcoalesce(level2score(regulatory_baseline_level), regulatory_baseline_score)]
  Lv[, reg_target_score   := fcoalesce(level2score(regulatory_target_level),   regulatory_target_score)]
  Lv[, implementation_gap := NA_real_]
  Lv[method == "regulatory_gap_multiplicative",
     implementation_gap := pmax(0, reg_target_score - reg_baseline_score)]          # M04
  Lv[, fiscal_tax_delta := NA_real_]
  Lv[method == "price_elasticity",
     fiscal_tax_delta := pmax(0, fiscal_target_tax_level - fiscal_baseline_tax_level)]  # M05
  Lv[, implied_price_change := NA_real_]
  Lv[method == "tax_share_to_price_elasticity",
     implied_price_change := (1 - fiscal_baseline_tax_level) /
       (1 - fiscal_target_tax_level) - 1]                                           # M06
  Lv[method == "price_elasticity", implied_price_change := fiscal_tax_delta]
  # Reproduced relative exposure reduction implied by the lever.
  Lv[, policy_reduction := NA_real_]
  Lv[method == "regulatory_gap_multiplicative",
     policy_reduction := effect_parameter * implementation_gap]
  Lv[method %in% c("price_elasticity", "tax_share_to_price_elasticity"),
     policy_reduction := abs(effect_parameter) * implied_price_change]
  Lv[, policy_reduction := pmax(0, policy_reduction)]
  # QA: reproduced levers vs workbook-cached values (REVIEW on drift only).
  qa_tol <- function(a, b) is.finite(a) & is.finite(b) &
    abs(a - b) > 1e-6 + 1e-4 * pmax(abs(b), 1)
  for (i in which(qa_tol(Lv$policy_reduction, Lv$estimated_risk_reduction_wb)))
    add_issue("ph_lever", Lv$intervention_id[i], "estimated_risk_reduction",
              sprintf("reproduced reduction %.6g != workbook %.6g",
                      Lv$policy_reduction[i], Lv$estimated_risk_reduction_wb[i]), "REVIEW")
  for (i in which(qa_tol(Lv$implied_price_change, Lv$implied_price_change_wb)))
    add_issue("ph_lever", Lv$intervention_id[i], "implied_price_change",
              sprintf("reproduced %.6g != workbook %.6g",
                      Lv$implied_price_change[i], Lv$implied_price_change_wb[i]), "REVIEW")
  for (i in which(qa_tol(Lv$implementation_gap, Lv$implementation_gap_wb)))
    add_issue("ph_lever", Lv$intervention_id[i], "implementation_gap",
              sprintf("reproduced %.6g != workbook %.6g",
                      Lv$implementation_gap[i], Lv$implementation_gap_wb[i]), "REVIEW")
  # QA: tax rows must carry fiscal baseline/target; regulatory rows valid scores.
  bad_fiscal <- Lv[method %in% c("price_elasticity", "tax_share_to_price_elasticity") &
                     (is.na(fiscal_baseline_tax_level) | is.na(fiscal_target_tax_level))]
  for (i in seq_len(nrow(bad_fiscal)))
    add_issue("ph_lever", bad_fiscal$intervention_id[i], "fiscal_tax_level",
              "fiscal lever missing baseline/target tax level", "FAIL")
  bad_reg <- Lv[method == "regulatory_gap_multiplicative" &
                  (!(reg_baseline_score %in% c(0, 0.5, 1)) | !(reg_target_score %in% c(0, 0.5, 1)))]
  for (i in seq_len(nrow(bad_reg)))
    add_issue("ph_lever", bad_reg$intervention_id[i], "regulatory_level",
              "regulatory score outside none/partial/full", "FAIL")

  # Binding inclusion contract: validate 0/1 then keep only include_flag == 1.
  # No execution switch may override the workbook. Any link that ships with
  # include_flag = 0 (e.g. the exploratory SSB -> T2DM sick -> dead mortality
  # link) stays excluded; to enable such a link, set its workbook flag to 1.
  map[, include_flag := .normalize_include_flag(
        include_flag, sprintf("Intervention_Cause_Map (%s)", basename(inputs_path)))]
  map_sel <- map[include_flag == 1L]
  # Label the mapped transition on the selected rows so the structural
  # acceptance counts below can be split by incidence vs sick->dead.
  map_sel[, model_transition := translate_transition(transition_from, transition_to)]

  # Duplicate / missing intervention_cause_key (selected links) ----------------
  dupk <- map_sel[, .N, by = intervention_cause_key][N > 1L]
  if (nrow(dupk))
    for (k in dupk$intervention_cause_key)
      add_issue("ph_link", k, "intervention_cause_key", "duplicate selected link key", "FAIL")
  if (any(is.na(map_sel$intervention_cause_key) | !nzchar(chrv(map_sel$intervention_cause_key))))
    add_issue("ph_link", "<NA>", "intervention_cause_key", "missing link key", "FAIL")

  ## -- Per-intervention exposure table (reproduce derived values) -------------
  E <- exp[, .(intervention_id = chrv(intervention_id), risk_id = chrv(risk_id),
               risk_exposure_measure = chrv(risk_exposure_measure),
               exposure_unit = chrv(exposure_unit),
               baseline_exposure = numv(baseline_exposure),
               reduction_method  = chrv(reduction_method),
               desired_reduction_override = numv(desired_reduction_override),
               applied_reduction_or_target = numv(applied_reduction_or_target),
               recommended_reduction_or_target = numv(recommended_reduction_or_target),
               exposure_floor = numv(exposure_floor),
               target_exposure_wb = numv(target_exposure),
               absolute_reduction_wb = numv(absolute_reduction),
               relative_reduction_wb = numv(relative_reduction),
               start_year  = as.integer(numv(start_year)),
               target_year = as.integer(numv(target_year)),
               scale_up_shape = chrv(scale_up_shape),
               exposure_review = chrv(review_status))]
  # Reproduced relative reduction from the policy lever (fiscal/regulatory) takes
  # precedence for "relative" rows; the cached workbook values are the QA fallback.
  E <- merge(E, Lv[, .(intervention_id, lever_policy_reduction = policy_reduction)],
             by = "intervention_id", all.x = TRUE)
  E[, red_or_target := fcoalesce(
      desired_reduction_override,
      ifelse(tolower(reduction_method) == "relative" & is.finite(lever_policy_reduction),
             lever_policy_reduction, NA_real_),
      applied_reduction_or_target, recommended_reduction_or_target)]
  for (i in which(qa_tol(E$red_or_target, E$applied_reduction_or_target)))
    add_issue("ph_exposure", E$intervention_id[i], "applied_reduction_or_target",
              sprintf("reproduced reduction %.6g != workbook %.6g",
                      E$red_or_target[i], E$applied_reduction_or_target[i]), "REVIEW")
  E[is.na(exposure_floor), exposure_floor := 0]
  E[, target_exposure := reproduce_target(baseline_exposure, reduction_method,
                                          red_or_target, exposure_floor)]
  E[, absolute_reduction := baseline_exposure - target_exposure]
  E[, relative_reduction := ifelse(baseline_exposure > 0,
                                   (baseline_exposure - target_exposure) / baseline_exposure, 0)]
  # Fall back to workbook start/target year when the exposure row omits them.
  E[is.na(start_year),  start_year  := policy_start_year]
  E[is.na(target_year), target_year := exposure_target_year]
  E[is.na(scale_up_shape) | !nzchar(scale_up_shape), scale_up_shape := scale_up_shape]
  # QA: reproduced target vs workbook cached target
  E[, target_reproduction_ok :=
      is.na(target_exposure_wb) | abs(target_exposure - target_exposure_wb) <=
        1e-6 + 1e-4 * pmax(abs(target_exposure_wb), 1)]
  for (i in which(!E$target_reproduction_ok))
    add_issue("ph_exposure", E$intervention_id[i], "target_exposure",
              sprintf("reproduced target %.6g != workbook %.6g",
                      E$target_exposure[i], E$target_exposure_wb[i]), "REVIEW")
  # Exposure-input validity
  bad_exp <- E[is.na(baseline_exposure) | baseline_exposure < 0 |
                 is.na(target_exposure) | target_exposure < 0 |
                 is.na(start_year) | is.na(target_year) | start_year > target_year |
                 !(tolower(reduction_method) %in% c("relative", "absolute", "target", "level"))]
  if (nrow(bad_exp))
    for (i in seq_len(nrow(bad_exp)))
      add_issue("ph_exposure", bad_exp$intervention_id[i], "exposure",
                "invalid baseline/target exposure, dates, or reduction_method", "FAIL")
  # TFA RR is expressed per percentage-point of dietary energy; the exposure must
  # be in % energy before the log-linear RR formula is applied.
  tfa_expo <- E[risk_id == "R_TFA"]
  if (nrow(tfa_expo) && !all(grepl("energy", tolower(tfa_expo$exposure_unit))))
    for (i in which(!grepl("energy", tolower(tfa_expo$exposure_unit))))
      add_issue("ph_exposure", tfa_expo$intervention_id[i], "exposure_unit",
                "TFA exposure not in % energy; RR per percentage-point not applicable", "FAIL")

  ## -- Per-link effect table (reproduce full effect at target) ----------------
  supported_effect_models <- c("direct_smoking_prevalence_shift_rr",     # M08 tobacco incidence
                               "tobacco_mortality_prevalence_shift_rr",  # M16 tobacco sick -> dead
                               "direct_loglinear_rr_per_unit_reduction", # M09 alcohol/salt/TFA/SSB
                               "tfa_attributable_ihd_PAF_x_regulatory_gap",
                               "tfa_attributable_ihd_PAF_x_policy_score")
  supported_lag_models <- c("delayed_exponential_remaining_effect",  # tobacco-T2DM proxy (legacy)
                            "immediate_after_full_implementation",   # log-linear risks
                            "jha_piecewise_shared_scalar",           # tobacco-CVD base timing (M12)
                            "normalized_exponential_lag")            # tobacco-CVD sensitivity timing
  F <- eff[, .(intervention_cause_key = chrv(intervention_cause_key),
               intervention_id = chrv(intervention_id), risk_id = chrv(risk_id),
               cause_id = chrv(cause_id), effect_model = chrv(effect_model),
               response_key = chrv(response_key),
               response_parameter = chrv(response_parameter),
               response_value = numv(response_value),
               paf_value = numv(paf_value),
               paf_required_flag = as.integer(numv(paf_required_flag)),
               lag_model = chrv(lag_model), lag_parameter = numv(lag_parameter),
               transition_from = chrv(transition_from), transition_to = chrv(transition_to),
               effect_review = chrv(review_status))]
  # one effect row per selected link
  eff_n <- F[, .(n_eff = .N), by = intervention_cause_key]

  # Age band / sex of applicability from Risk_Response (by response_key). Public
  # policies otherwise apply population-wide; defaults 0-95, Both if unmatched.
  rr_age <- rr[, .(response_key = chrv(response_key), rr_age_start = numv(age_start),
                   rr_age_stop = numv(age_stop), rr_sex = chrv(sex))]
  rr_age <- unique(rr_age, by = "response_key")
  F <- merge(F, rr_age, by = "response_key", all.x = TRUE)
  F[is.na(rr_age_start), rr_age_start := 0]
  F[is.na(rr_age_stop),  rr_age_stop  := 95]
  F[is.na(rr_sex) | !nzchar(rr_sex), rr_sex := "Both"]

  ## -- Assemble the selected-link table ---------------------------------------
  L <- merge(map_sel[, .(intervention_cause_key = chrv(intervention_cause_key),
                         intervention_id = chrv(intervention_id),
                         intervention_name = chrv(intervention_name),
                         risk_id = chrv(risk_id), cause_id = chrv(cause_id),
                         cause_name = chrv(cause_name),
                         effect_key = chrv(effect_key), exposure_key = chrv(exposure_key),
                         cost_join_key = chrv(cost_join_key), cost_scope = chrv(cost_scope),
                         transition_from, transition_to,
                         requires_cause_expansion = as.integer(numv(requires_cause_expansion)))],
             F[, .(intervention_cause_key, effect_model, response_key, response_parameter,
                   response_value, paf_value, paf_required_flag, lag_model, lag_parameter,
                   effect_review, rr_age_start, rr_age_stop, rr_sex)],
             by = "intervention_cause_key", all.x = TRUE)
  L <- merge(L, eff_n, by = "intervention_cause_key", all.x = TRUE)
  L[is.na(n_eff), n_eff := 0L]
  L[, model_transition := translate_transition(transition_from, transition_to)]
  L[, cause_code := cause_id2code[cause_id]]
  # bring exposure-path parameters onto each link (by intervention)
  L <- merge(L, E[, .(intervention_id, baseline_exposure, target_exposure, exposure_floor,
                      exposure_start_year = start_year, exposure_target_year = target_year,
                      exposure_scale_up_shape = scale_up_shape, reduction_method,
                      exposure_review)],
             by = "intervention_id", all.x = TRUE)
  # bring the lever trace/reproduction (fiscal, regulatory, hierarchy) onto each link
  L <- merge(L, Lv[, .(intervention_id, parent_package_id, parent_package_name,
                       intervention_role, implied_price_change, fiscal_tax_delta,
                       implementation_gap, lever_policy_reduction = policy_reduction)],
             by = "intervention_id", all.x = TRUE)
  L[, tfa_effect_method := tfa_effect_method]
  # Optional TFA PAF method: only when Assumptions$tfa_effect_method = "PAF". The RR
  # base case leaves the log-linear effect model untouched (no PAF required).
  tfa_paf_optional <- tfa_optional_ihd_paf
  if (is.na(tfa_paf_optional)) {
    .prv <- rr[chrv(response_key) == "PAF_TFA_IHD_OPTIONAL", numv(model_response_value)]
    if (length(.prv)) tfa_paf_optional <- .prv[1]
  }
  if (identical(tfa_effect_method, "PAF")) {
    L[risk_id == "R_TFA", `:=`(
      effect_model  = "tfa_attributable_ihd_PAF_x_regulatory_gap",
      response_value = implementation_gap,     # RR slot carries the implementation gap
      paf_value     = tfa_paf_optional,
      lag_model     = "immediate_after_full_implementation")]
    if (nrow(L[risk_id == "R_TFA"]) && (is.na(tfa_paf_optional) || tfa_paf_optional == 0))
      add_issue("ph_effect", "I_PH_TFA_POLICY", "paf_value",
                "TFA PAF mode selected but optional PAF missing/zero (effect provisional)", "REVIEW")
  }
  L[, full_effect_at_target := reproduce_full_effect(effect_model, baseline_exposure,
                                                     target_exposure, response_value, paf_value,
                                                     erd10_pooled = tob_erd10_pooled)]
  # QA: reproduced full effect vs Model_Input_View illustrative_full_effect
  miv_e <- miv[, .(intervention_cause_key = chrv(intervention_cause_key),
                   illustrative_full_effect = numv(illustrative_full_effect))]
  L <- merge(L, miv_e, by = "intervention_cause_key", all.x = TRUE)

  ## -- Per-link validation -----------------------------------------------------
  in01 <- function(x) !is.na(x) & x >= 0 & x <= 1
  L[, problem := ""]
  padd <- function(cond, msg) {
    cond[is.na(cond)] <- FALSE
    L[cond, problem := paste0(problem, ifelse(nchar(problem) > 0L, "; ", ""), msg)]
  }
  padd(L$n_eff != 1L,                              "effect match != 1")
  padd(is.na(L$cause_code),                        "cause_id absent from Model 00 cause_map")
  padd(is.na(L$model_transition),                  "transition outside allowed {well->sick incidence, sick->dead case_fatality}")
  padd(!(L$effect_model %in% supported_effect_models), "unsupported effect_model")
  padd(!(L$lag_model %in% supported_lag_models),   "unsupported lag_model")
  padd(is.na(L$lag_parameter) | L$lag_parameter < 0, "missing/negative lag_parameter")
  padd(is.na(L$response_value),                    "missing response parameter")
  padd(is.na(L$baseline_exposure) | is.na(L$target_exposure), "missing exposure row for link")
  padd(!in01(L$full_effect_at_target),             "full effect at target out of [0,1]")
  padd(L$requires_cause_expansion == 1L,           "link requires unresolved cause expansion")
  # PAF-required links must carry a PAF (TFA); flag but keep (REVIEW), not FAIL.
  L[, valid := problem == ""]
  # PAF review (numerically usable at effect 0, but flagged for review)
  paf_missing <- L$paf_required_flag == 1L & (is.na(L$paf_value) | L$paf_value == 0)
  paf_missing[is.na(paf_missing)] <- FALSE
  for (i in which(paf_missing))
    add_issue("ph_effect", L$intervention_cause_key[i], "paf_value",
              "PAF-dependent effect missing/zero PAF (kept; effect provisional)", "REVIEW")
  # full-effect reproduction vs workbook illustrative value (REVIEW on drift)
  fe_drift <- !is.na(L$illustrative_full_effect) & !is.na(L$full_effect_at_target) &
    abs(L$full_effect_at_target - L$illustrative_full_effect) > 1e-4 + 1e-3 * abs(L$illustrative_full_effect)
  fe_drift[is.na(fe_drift)] <- FALSE
  for (i in which(fe_drift))
    add_issue("ph_effect", L$intervention_cause_key[i], "full_effect_at_target",
              sprintf("reproduced full effect %.6g != workbook %.6g",
                      L$full_effect_at_target[i], L$illustrative_full_effect[i]), "REVIEW")
  for (i in which(!L$valid))
    add_issue("ph_link", L$intervention_cause_key[i], "effect/exposure",
              L$problem[i], "FAIL")

  ## -- Cost components ---------------------------------------------------------
  sel_int <- unique(map_sel$intervention_id)
  C <- cst[, .(cost_record_id = chrv(cost_record_id),
               cost_component_key = chrv(cost_component_key), cost_option = chrv(cost_option),
               selected_for_base_case = as.integer(numv(selected_for_base_case)),
               intervention_id = chrv(intervention_id), cause_id = chrv(cause_id),
               cost_join_key = chrv(cost_join_key), cost_component = chrv(cost_component),
               population_in_need_measure  = tolower(chrv(population_in_need_measure)),
               population_in_need_fraction = numv(population_in_need_fraction),
               frequency_per_year = numv(frequency_per_year),
               c_age_start = numv(age_start), c_age_stop = numv(age_stop),
               c_sex = chrv(sex), platform = chrv(platform),
               unit_cost_usd = numv(unit_cost_usd),
               price_year = as.integer(numv(price_year)),
               indonesia_adjusted_flag = as.integer(numv(indonesia_adjusted_flag)),
               source_country = chrv(source_country), source_file = chrv(source_file),
               cost_review = chrv(review_status), notes = chrv(notes),
               # NEW allocation fields (read by name): parent package, child share,
               # package total and the allocation method / scenario role.
               parent_package_id = chrv(parent_package_id),
               cost_allocation_share = numv(cost_allocation_share),
               package_total_cost_usd_per_capita = numv(package_total_cost_usd_per_capita),
               allocation_method = chrv(allocation_method),
               cost_scenario_role = chrv(scenario_role))]
  # Retain ALL cost rows (incl. parent_reference rows) for the allocation audit;
  # cost only the in-scope executable intervention rows.
  Call <- copy(C)
  C <- C[intervention_id %in% sel_int]

  # Exactly one selected base-case row per in-scope cost_component_key -----------
  C[, n_sel := sum(selected_for_base_case == 1L, na.rm = TRUE), by = cost_component_key]
  sel_counts <- unique(C[, .(cost_component_key, n_sel)])
  for (i in seq_len(nrow(sel_counts))) {
    if (isTRUE(sel_counts$n_sel[i] > 1L))
      add_issue("ph_cost", sel_counts$cost_component_key[i], "selected_for_base_case",
                "more than one base-case cost option selected", "FAIL")
    if (isTRUE(sel_counts$n_sel[i] == 0L))
      add_issue("ph_cost", sel_counts$cost_component_key[i], "selected_for_base_case",
                "no base-case cost option selected (component omitted from costing)", "REVIEW")
  }

  Cbase <- C[selected_for_base_case == 1L]
  valid_cjk <- unique(chrv(map$cost_join_key))
  # Normalize the public-wide `population` PIN measure to the model's all-population
  # quantity so Model 09 can reuse the shared "all" costing path (deduplicated
  # population once per age/sex/year, never summed over causes).
  Cbase[, population_in_need_measure := ifelse(population_in_need_measure == "population",
                                               "all", population_in_need_measure)]

  padc <- function(dt, cond, field, msg, severity) {
    cond[is.na(cond)] <- FALSE
    if (any(cond))
      for (i in which(cond))
        add_issue("ph_cost", dt$cost_record_id[i], field, msg, severity)
  }
  padc(Cbase, is.na(Cbase$unit_cost_usd) | Cbase$unit_cost_usd < 0, "unit_cost_usd",
       "missing or negative unit cost on a selected base-case row", "FAIL")
  padc(Cbase, is.na(Cbase$frequency_per_year) | Cbase$frequency_per_year < 0, "frequency_per_year",
       "missing or negative frequency", "FAIL")
  padc(Cbase, is.na(Cbase$population_in_need_fraction) |
         Cbase$population_in_need_fraction < 0 | Cbase$population_in_need_fraction > 1,
       "population_in_need_fraction", "PIN fraction missing/out of [0,1]", "FAIL")
  padc(Cbase, !(chrv(Cbase$cost_join_key) %in% valid_cjk), "cost_join_key",
       "cost_join_key not present in Intervention_Cause_Map", "FAIL")
  padc(Cbase, !(Cbase$population_in_need_measure %in% c("all", "prevalence", "incidence")),
       "population_in_need_measure", "unsupported PIN measure", "FAIL")
  padc(Cbase, Cbase$indonesia_adjusted_flag == 0L, "indonesia_adjusted_flag",
       "cost not Indonesia-adjusted (proxy/review estimate; kept but flagged)", "REVIEW")
  padc(Cbase, !is.na(Cbase$price_year) & Cbase$price_year != reporting_price_year, "price_year",
       paste0("cost price year != reporting price year (", reporting_price_year, ")"), "REVIEW")

  # cost_scope per cost_join_key (from the map) --------------------------------
  scope_by_cjk <- unique(map[, .(cost_join_key = chrv(cost_join_key), cost_scope = chrv(cost_scope))])
  scope_by_cjk <- scope_by_cjk[, .(cost_scope = cost_scope[1]), by = cost_join_key]
  Cbase[, cost_join_key := chrv(cost_join_key)]
  Cbase <- merge(Cbase, scope_by_cjk, by = "cost_join_key", all.x = TRUE)
  Cbase[, cost_ready := !is.na(unit_cost_usd) & unit_cost_usd >= 0 &
          !is.na(frequency_per_year) & frequency_per_year >= 0 &
          !is.na(population_in_need_fraction) &
          population_in_need_measure %in% c("all", "prevalence", "incidence") &
          chrv(cost_join_key) %in% valid_cjk]

  ## -- Package cost allocation QA (M13/M14): child shares sum to 1 within each
  ## package and package cost equals the sum of its selected child costs. The
  ## parent_reference rows (selected_for_base_case = 0) are never charged.
  cost_reference <- Call[selected_for_base_case == 0L]
  for (pid in package_ids) {
    kids <- Call[selected_for_base_case == 1L & parent_package_id == pid]
    if (!nrow(kids)) next
    sh_sum <- sum(kids$cost_allocation_share, na.rm = TRUE)
    if (abs(sh_sum - 1) > 1e-6)
      add_issue("ph_cost", pid, "cost_allocation_share",
                sprintf("child cost-allocation shares sum to %.6g (expected 1)", sh_sum), "FAIL")
    child_cost <- sum(kids$unit_cost_usd, na.rm = TRUE)
    ref <- cost_reference[intervention_id == pid]
    pkg_total <- if (nrow(ref)) ref$unit_cost_usd[1] else
      suppressWarnings(unique(kids$package_total_cost_usd_per_capita)[1])
    if (is.finite(pkg_total) && abs(child_cost - pkg_total) > 1e-6 + 1e-4 * abs(pkg_total))
      add_issue("ph_cost", pid, "package_total_cost_usd_per_capita",
                sprintf("sum of child costs %.6g != package total %.6g", child_cost, pkg_total), "REVIEW")
  }

  ## -- Runnable interventions & scenario catalogue ----------------------------
  valid_links   <- L[valid == TRUE]
  runnable_ints <- unique(valid_links$intervention_id)
  blocked_ints  <- setdiff(sel_int, runnable_ints)

  to_engine <- function(dd)
    dd[, .(intervention_id, intervention_cause_key, effect_key, exposure_key, cost_join_key,
           cause_id, cause_code, model_transition,
           # Raw workbook transition fields carried through so every downstream
           # join can key on the COMPLETE transition (prevents a well->sick effect
           # ever being applied to sick->dead or vice-versa) and Model 09 can
           # report the original transition.
           transition_from = chrv(transition_from), transition_to = chrv(transition_to),
           effect_model, response_key,
           baseline_exposure, target_exposure, exposure_floor,
           start_year = exposure_start_year, target_year = exposure_target_year,
           scale_up_shape = exposure_scale_up_shape,
           response_value, paf_value, lag_model, lag_parameter,
           full_effect_at_target,
           # fiscal / regulatory reproduction + hierarchy trace carried per link
           parent_package_id, intervention_role, implied_price_change, fiscal_tax_delta,
           implementation_gap, tfa_effect_method,
           # age band / sex of applicability from Risk_Response (else population-wide)
           age_start = rr_age_start, age_stop = rr_age_stop, sex = rr_sex)]

  # Scenario labels: individual interventions and parent packages (from the hierarchy).
  int_label <- function(iid) {
    v <- unique(SH[intervention_id == iid, intervention_name]); v <- v[nzchar(v)]
    if (length(v)) v[1] else iid
  }
  pkg_label <- function(pid) {
    v <- unique(SH[parent_scenario_id == pid, parent_scenario_name]); v <- v[nzchar(v)]
    if (length(v)) v[1] else pid
  }

  scen <- list()
  scen[[baseline_id]] <- list(scenario_id = baseline_id,
                              scenario_label = "Baseline (no new intervention)",
                              family = "baseline", scenario_level = "baseline",
                              scenario_role = "baseline",
                              parent_package_id = NA_character_,
                              parent_package_name = NA_character_,
                              intervention_id = NA_character_,
                              intervention_ids = character(0),
                              component_order = NA_integer_,
                              interventions = character(0),
                              ph_effect_rows = NULL)

  # (2) one standalone scenario per runnable intervention (uses standalone_scenario_id).
  SH_ind <- unique(SH[, .(intervention_id, standalone_scenario_id, scenario_role,
                          parent_scenario_id, component_order)])
  for (i in seq_len(nrow(SH_ind))) {
    iid <- SH_ind$intervention_id[i]
    if (!(iid %in% runnable_ints)) next
    sid  <- if (nzchar(SH_ind$standalone_scenario_id[i])) SH_ind$standalone_scenario_id[i] else iid
    role <- SH_ind$scenario_role[i]
    ppid <- if (identical(role, "child")) SH_ind$parent_scenario_id[i] else NA_character_
    scen[[sid]] <- list(scenario_id = sid, scenario_label = int_label(iid),
                        family = "public_health", scenario_level = "standalone",
                        scenario_role = role,
                        parent_package_id = ppid,
                        parent_package_name = if (is.na(ppid)) NA_character_ else pkg_label(ppid),
                        intervention_id = iid, intervention_ids = iid,
                        component_order = SH_ind$component_order[i],
                        interventions = "ph_wb",
                        ph_effect_rows = to_engine(valid_links[intervention_id == iid]))
  }

  # (3) one JOINT scenario per parent package: all runnable children applied
  # together in a single model run (never sum standalone child outcomes).
  for (pid in package_ids) {
    kids <- intersect(children_of(pid), runnable_ints)
    if (!length(kids)) next
    scen[[pid]] <- list(scenario_id = pid, scenario_label = pkg_label(pid),
                        family = "public_health", scenario_level = "package",
                        scenario_role = "package",
                        parent_package_id = pid, parent_package_name = pkg_label(pid),
                        intervention_id = NA_character_, intervention_ids = kids,
                        component_order = NA_integer_,
                        interventions = "ph_wb",
                        ph_effect_rows = to_engine(valid_links[intervention_id %in% kids]))
  }

  # (4) combined all-public-health scenario (>= 2 runnable interventions).
  if (length(runnable_ints) >= 2L)
    scen[["all_public_health"]] <- list(scenario_id = "all_public_health",
                        scenario_label = "All selected public-health interventions (combined)",
                        family = "public_health", scenario_level = "combined",
                        scenario_role = "combined",
                        parent_package_id = NA_character_, parent_package_name = NA_character_,
                        intervention_id = NA_character_, intervention_ids = runnable_ints,
                        component_order = NA_integer_,
                        interventions = "ph_wb",
                        ph_effect_rows = to_engine(valid_links))

  ## -- Acceptance-criteria checks (data-driven; add issues only on failure) ----
  # Inclusion is decided by the workbook include_flag, so the number of runnable
  # interventions and selected links is whatever the flags leave -- NOT a fixed
  # historical count. A valid exclusion (all links flagged 0) must never trip a
  # warning. We therefore assert only structural invariants that must hold for
  # every runnable intervention, and log the resulting counts for the audit
  # trail without comparing them to a hard-coded expectation.
  n_exec   <- uniqueN(map_sel$intervention_id)
  n_inc    <- map_sel[model_transition == "incidence",     uniqueN(intervention_cause_key)]
  n_cf     <- map_sel[model_transition == "case_fatality", uniqueN(intervention_cause_key)]
  n_cf_tob <- map_sel[model_transition == "case_fatality" & grepl("^I_PH_TOB", intervention_id),
                      uniqueN(intervention_cause_key)]
  cat(sprintf(paste0("Public-health inclusion contract (from workbook include_flag): ",
                     "%d runnable intervention(s), %d incidence link(s), %d sick->dead link(s) ",
                     "(%d tobacco-CVD).\n"),
              n_exec, n_inc, n_cf, n_cf_tob))
  # A family must have at least one runnable intervention to be analyzable.
  if (n_exec < 1L)
    add_issue("ph_contract", "interventions", "count",
              "no runnable public-health interventions after applying include_flag", "FAIL")
  # Per-runnable-intervention structural invariants (these are the binding ones).
  for (iid in runnable_ints) {
    if (nrow(E[intervention_id == iid]) != 1L)
      add_issue("ph_contract", iid, "exposure", "not exactly one exposure row", "FAIL")
    if (!nrow(valid_links[intervention_id == iid]))
      add_issue("ph_contract", iid, "effect", "no valid effect rows", "FAIL")
    if (!nrow(Cbase[intervention_id == iid & cost_ready == TRUE]))
      add_issue("ph_contract", iid, "cost_join", "no ready child-specific cost row", "REVIEW")
  }

  ## -- Processed hierarchy + scenario catalogue (exported for Models 06/09) ----
  hierarchy_dt <- copy(SH)
  hierarchy_dt[, is_runnable := intervention_id %in% runnable_ints]
  hierarchy_dt[, is_parent_package := intervention_id %in% package_ids]
  scenario_catalogue <- rbindlist(lapply(scen, function(s) data.table(
    scenario_id = s$scenario_id, scenario_label = s$scenario_label, family = s$family,
    scenario_level = s$scenario_level, scenario_role = s$scenario_role,
    parent_package_id = s$parent_package_id, parent_package_name = s$parent_package_name,
    intervention_ids = paste(s$intervention_ids, collapse = "; "),
    n_interventions = length(s$intervention_ids),
    component_order = s$component_order)), fill = TRUE)

  ## -- Tobacco Jha timing / vascular-mortality config (parsed once; consumed by
  ## Model 06). Parses the SCALAR_JHA_* grid into (sex, age band, cessation
  ## duration) rows carrying the ERD and the shared timing scalar
  ## lambda = min(1, ERD_duration / ERD_10plus) (M12), plus ERD_10plus per band
  ## and the sex-specific vascular-mortality HRs (M16). No numbers are invented:
  ## every value is a Risk_Response / Assumptions cell read by name.
  build_tobacco_scalar_matrix <- function(rr_dt) {
    j <- rr_dt[grepl("^SCALAR_JHA_", chrv(response_key))]
    if (!nrow(j)) return(NULL)
    parse_key <- function(k) {
      s   <- sub("^SCALAR_JHA_", "", k)                # e.g. M_20_39_LT3 / F_40_49_Y3_9
      sx  <- substr(s, 1, 1)
      rest <- sub("^[MF]_", "", s)
      if      (grepl("_LT3$",  rest)) { d <- "LT3";  ages <- sub("_LT3$",  "", rest) }
      else if (grepl("_GE10$", rest)) { d <- "GE10"; ages <- sub("_GE10$", "", rest) }
      else if (grepl("_Y3_9$", rest)) { d <- "Y3_9"; ages <- sub("_Y3_9$", "", rest) }
      else stop("Public-health: unparseable SCALAR_JHA key '", k, "'")
      ab <- strsplit(ages, "_")[[1]]
      data.table(sex = ifelse(sx == "M", "Male", "Female"),
                 age_lo = as.integer(ab[1]), age_hi = as.integer(ab[2]),
                 duration = d)
    }
    meta <- rbindlist(lapply(chrv(j$response_key), parse_key))
    sm <- cbind(meta, ERD = numv(j$source_effect_value),
                lambda_wb = numv(j$model_response_value))
    # lambda = min(1, ERD_duration / ERD_10plus) within each sex x age band (M12).
    sm[, ERD_ge10 := ERD[duration == "GE10"], by = .(sex, age_lo, age_hi)]
    sm[, lambda   := pmin(1, ERD / ERD_ge10)]
    # QA: reproduced lambda must match the workbook cached model_response_value.
    for (ii in which(is.finite(sm$lambda_wb) & abs(sm$lambda - sm$lambda_wb) > 1e-6))
      add_issue("ph_tobacco", paste0(sm$sex[ii], "_", sm$age_lo[ii], "_", sm$duration[ii]),
                "SCALAR_JHA", sprintf("reproduced lambda %.6g != workbook %.6g",
                                      sm$lambda[ii], sm$lambda_wb[ii]), "REVIEW")
    sm[]
  }
  tob_scalar_matrix <- build_tobacco_scalar_matrix(rr)
  # ERD_10plus per (sex, age band) drives the M16 residual mortality RR.
  tob_erd10_by_band <- if (!is.null(tob_scalar_matrix))
    unique(tob_scalar_matrix[duration == "GE10", .(sex, age_lo, age_hi, erd10 = ERD)]) else NULL
  tobacco_effect_config <- list(
    scalar_matrix   = tob_scalar_matrix,     # sex x age band x duration -> lambda
    erd10_by_band   = tob_erd10_by_band,     # sex x age band -> ERD_10plus (M16)
    vasc_rr_male    = tob_rr_mort_male,       # 2.9 (applied)
    vasc_rr_female  = tob_rr_mort_female,     # 3.1 (applied)
    vasc_rr_pooled  = tob_rr_mort_pooled,     # 3.0 (illustrative fallback only)
    erd10_pooled    = tob_erd10_pooled,       # 0.90125 (illustrative fallback only)
    timing_mode     = tob_timing_selected,    # jha_piecewise_shared_scalar | normalized_exponential_lag
    lag_rate        = tob_lag_rate,           # 0.0616 (sensitivity timing only)
    full_effect_year = tob_full_effect_year,  # 10
    age80_extrapolation = tob_age80_extrap,   # use_age_60_79 (documented fallback)
    # Age band 80-95 reuses the 60-79 scalars per Assumptions; recorded for the
    # Model 06 diagnostic so the extrapolation is auditable, not silent.
    age80_reuses_60_79 = identical(tob_age80_extrap, "use_age_60_79"))
  cat(sprintf("Public-health: tobacco timing model = %s (full effect year %d; sens. lag rate %.4g).\n",
              tob_timing_selected, tob_full_effect_year, tob_lag_rate))

  ## -- Assemble public_health_inputs (consumed by Model 09) -------------------
  public_health_inputs <- list(
    links          = L,
    valid_links    = valid_links,
    blocked_links  = L[valid == FALSE],
    effect_rows_engine = to_engine(valid_links),
    tobacco_effect_config = tobacco_effect_config,
    exposure       = E,
    policy_levers  = lev,
    policy_levers_processed = Lv,
    scenario_hierarchy = SH,
    hierarchy      = hierarchy_dt,
    scenario_catalogue = scenario_catalogue,
    package_ids    = package_ids,
    effect_parameters = eff,
    risk_response  = rr,
    model_input_view = miv,
    countdown_methods = rd_opt("Countdown_Methods"),
    scope_sources  = rd_opt("Scope_and_Sources"),
    qa_workbook    = rd_opt("QA_Checks"),
    costs          = Cbase,
    cost_all       = C,
    cost_reference = cost_reference,
    validation     = issues,
    cause_translation = data.table(cause_id = names(cause_id2code),
                                   cause_code = unname(cause_id2code)),
    runnable_interventions = runnable_ints,
    blocked_interventions  = blocked_ints,
    inputs_path    = inputs_path,
    baseline_scenario_id = baseline_id,
    assumptions    = list(
      analysis_start_year    = analysis_start_year,
      analysis_end_year      = analysis_end_year,
      policy_start_year      = policy_start_year,
      exposure_target_year   = exposure_target_year,
      policy_cost_ramp_years  = policy_cost_ramp_years,
      cost_discount_rate     = cost_discount_rate,
      health_discount_rate   = health_discount_rate,
      cost_price_year        = reporting_price_year,
      source_cost_price_year = source_cost_price_year,
      currency               = getA("currency", "USD"),
      scale_up_shape         = scale_up_shape,
      economic_perspective   = getA("economic_perspective", "health_system"),
      tfa_effect_method      = tfa_effect_method,
      tfa_optional_ihd_paf   = tfa_paf_optional,
      regulatory_none_score    = reg_none_score,
      regulatory_partial_score = reg_partial_score,
      regulatory_full_score    = reg_full_score))

  ## -- Report ------------------------------------------------------------------
  n_fail <- sum(issues$severity == "FAIL")
  n_rev  <- sum(issues$severity == "REVIEW")
  cat("\n--- Public-health workbook catalogue --------------------------------\n")
  cat(sprintf("Workbook: %s\n", inputs_path))
  cat(sprintf("Selected links: %d | valid: %d | invalid: %d\n",
              nrow(map_sel), nrow(valid_links), nrow(L[valid == FALSE])))
  cat(sprintf("Runnable interventions (%d): %s\n",
              length(runnable_ints), paste(runnable_ints, collapse = ", ")))
  cat(sprintf("TFA effect method: %s\n", tfa_effect_method))
  for (pid in package_ids)
    cat(sprintf("Package %s -> children: %s\n", pid,
                paste(intersect(children_of(pid), runnable_ints), collapse = ", ")))
  if (length(blocked_ints))
    cat(sprintf("BLOCKED interventions (%d): %s\n",
                length(blocked_ints), paste(blocked_ints, collapse = ", ")))
  cat(sprintf("Selected base-case cost rows: %d (ready: %d)\n",
              nrow(Cbase), sum(Cbase$cost_ready)))
  cat(sprintf("Validation issues: %d FAIL, %d REVIEW\n", n_fail, n_rev))
  if (nrow(issues)) { cat("Consolidated validation diagnostic:\n"); print(issues) }
  cat(sprintf("Scenarios built (%d): %s\n",
              length(scen), paste(names(scen), collapse = ", ")))
  cat("---------------------------------------------------------------------\n\n")

  if (strict && n_fail > 0L)
    stop("Public-health: strict_model_input_validation = TRUE and ", n_fail,
         " FAIL-level workbook issue(s) present (see diagnostic above). ",
         "Resolve them or set strict_model_input_validation = FALSE to run only ",
         "the valid scenarios.", call. = FALSE)

  list(scenarios = scen, inputs = public_health_inputs)
}

if (isTRUE(run_public_health_interventions)) {
  # Timing model is an execution-level analytic choice from Model 00; default to
  # the reproducible base case if Model 00 did not declare it. Row-level
  # inclusion is decided solely by the workbook include_flag (see
  # .normalize_include_flag); no execution switch can force an excluded link on.
  .tob_timing <- if (exists("tobacco_timing_analysis")) tobacco_timing_analysis else "base"
  .ph_built <- .build_public_health_catalogue(public_health_inputs_file, cause_map,
                                              strict      = strict_model_input_validation,
                                              baseline_id = baseline_scenario_id,
                                              tobacco_timing_analysis = .tob_timing)
  public_health_scenarios <- .ph_built$scenarios
  public_health_inputs    <- .ph_built$inputs
  rm(.ph_built)
} else {
  # Public-health family disabled: define empty catalogues so downstream models
  # can detect "PH not requested" without erroring.
  public_health_scenarios <- NULL
  public_health_inputs    <- NULL
  cat("\nPublic-health interventions disabled (run_public_health_interventions = FALSE); ",
      "PH catalogue not built.\n\n", sep = "")
}

#===========================================================================
# JOINT CLINICAL + PUBLIC-HEALTH SCENARIO  (feeds Models 06, 07 and 09) ----
#---------------------------------------------------------------------------
# When BOTH intervention families are enabled we build ONE genuine joint
# scenario, `all_clinical_public_health`, that carries the complete validated
# clinical effect rows (from the clinical `all` scenario) AND the complete
# validated public-health effect rows (from `all_public_health`). Model 06 runs
# it as a SINGLE projection that applies `fair_wb` and `ph_wb` exactly once each
# to the same baseline-rate copy -- it is NEVER an arithmetic combination of the
# separate `all` / `all_public_health` outputs. The two family catalogues above
# are left completely untouched; this only ADDS a third catalogue object.
#===========================================================================
.run_cl <- if (exists("run_clinical_interventions"))      isTRUE(run_clinical_interventions)      else TRUE
.run_ph <- if (exists("run_public_health_interventions")) isTRUE(run_public_health_interventions) else TRUE
if (.run_cl && .run_ph) {
  # Complete validated effect rows of each family's combined scenario.
  .cl_all  <- if (exists("fair_scenarios") && !is.null(fair_scenarios))
    fair_scenarios[["all"]] else NULL
  .ph_all  <- if (exists("public_health_scenarios") && !is.null(public_health_scenarios))
    public_health_scenarios[["all_public_health"]] else NULL
  .cl_rows <- if (!is.null(.cl_all)) .cl_all$fair_effect_rows else NULL
  .ph_rows <- if (!is.null(.ph_all)) .ph_all$ph_effect_rows   else NULL

  if (is.null(.cl_rows) || !nrow(.cl_rows) || is.null(.ph_rows) || !nrow(.ph_rows))
    stop("Model 04: run_clinical_interventions and run_public_health_interventions are ",
         "both TRUE but the validated combined effect rows are unavailable. The joint ",
         "scenario requires the clinical 'all' scenario (>= 2 runnable clinical ",
         "interventions) and the public-health 'all_public_health' scenario (>= 2 ",
         "runnable public-health interventions). Clinical rows: ",
         if (is.null(.cl_rows)) "MISSING" else nrow(.cl_rows), "; public-health rows: ",
         if (is.null(.ph_rows)) "MISSING" else nrow(.ph_rows), ".", call. = FALSE)

  .cl_ids <- .cl_all$intervention_ids
  .ph_ids <- .ph_all$intervention_ids
  .joint_id <- "all_clinical_public_health"
  # Collision-safety: the joint id must not clash with either family catalogue.
  if (.joint_id %in% c(names(fair_scenarios), names(public_health_scenarios)))
    stop("Model 04: joint scenario id '", .joint_id, "' collides with an existing ",
         "family scenario id.", call. = FALSE)

  combined_scenarios <- list()
  combined_scenarios[[.joint_id]] <- list(
    scenario_id      = .joint_id,
    scenario_label   = "All clinical + public-health interventions (combined)",
    family           = "clinical_public_health",
    scenario_level   = "combined",
    scenario_role    = "combined",
    parent_package_id   = NA_character_,
    parent_package_name = NA_character_,
    intervention_id  = NA_character_,
    # Union of all runnable intervention IDs, with family provenance retained.
    intervention_ids = c(.cl_ids, .ph_ids),
    clinical_intervention_ids      = .cl_ids,
    public_health_intervention_ids = .ph_ids,
    component_order  = NA_integer_,
    # BOTH engines run once each in a single projection (fair_wb + ph_wb).
    interventions    = c("fair_wb", "ph_wb"),
    fair_effect_rows = .cl_rows,          # complete validated clinical rows
    ph_effect_rows   = .ph_rows)          # complete validated public-health rows

  cat(sprintf(paste0("\nJoint scenario built: '%s' -> %d clinical + %d public-health ",
                     "runnable interventions; %d clinical + %d public-health effect rows ",
                     "(single joint run, fair_wb + ph_wb).\n\n"),
              .joint_id, length(.cl_ids), length(.ph_ids), nrow(.cl_rows), nrow(.ph_rows)))
} else {
  # Not a both-families run: no joint scenario (single-family runs unchanged).
  combined_scenarios <- NULL
}
rm(list = intersect(ls(), c(".run_cl", ".run_ph", ".cl_all", ".ph_all", ".cl_rows",
                            ".ph_rows", ".cl_ids", ".ph_ids", ".joint_id")))
