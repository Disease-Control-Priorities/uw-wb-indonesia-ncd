#...........................................................
# Documentation ----
#...........................................................

# The input of this file is the GBD 2019 data and the GBD 2019 population estimates.
# The output of this file is a data.csv file with the baseline rates for each country and year.
# The output data set includes the following columns: sex	age	location	year	ALL.mx	BG.mx.all	cause	BG.mx	PREVt0	DIS.mx.t0	Nx

#...........................................................
# GBD 2023 Data ----
#...........................................................

#...........................................................
# Documentation ----
#...........................................................

# The input of this file is the GBD 2023 data and the GBD 2019 population estimates.
# The output of this file is a data.csv file with the baseline rates for each country and year.
# The output data set includes the following columns: sex	age	location	year	ALL.mx	BG.mx.all	cause	BG.mx	PREVt0	DIS.mx.t0	Nx

#...........................................................
# GBD 2023 Data ----
#...........................................................

# Indonesia Level 2

# https://collab2023.healthdata.org/gbd-results?params=gbd-api-2023-permalink/e23ae880499d3ae1d643dca738057425

# load 2020- 2023

# List all CSV files
files <- list.files(paste0(wd_raw,"GBD/GBD2023/2020-2023/"), pattern = "\\.csv$", full.names = TRUE)

# Read and combine using rbindlist
dt_23 <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)

## Now complete the series
setwd(wd_raw)

path <- paste0(wd_raw,"GBD/GBD2023/")

# List all CSV files
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Read and combine using rbindlist
dt <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)

dt<-data.table(dt)

dt[, upper:=NULL]
dt[, lower:=NULL]

unique(dt$year)
unique(dt$location_name)
unique(dt$cause_name)
unique(dt$age_name)

dt <- dt[age_name!="75-84 years",]

# rbind 2020-2023
dt <- rbind(dt[year<2020,], dt_23, use.names = TRUE, fill = TRUE)

# clean
rm(dt_23)

# Fix countries names
# dt[location_name == "Türkiye", location_name := "Turkey"]
# dt[location_name == "Côte d'Ivoire", location_name := "Ivory Coast"]
dt[location_name == "Republic of the Congo", location_name := "Congo"]

# Step 1: Clean some known patterns (optional but helps)
dt[, location_clean := gsub("^(Republic|Kingdom|Commonwealth|Democratic Republic|Principality|Federated States|State|United States|Islamic Republic|Portuguese|Union) of ", "", location_name)]
dt[, location_clean := gsub(" of the", "", location_clean)]
dt[, location_clean := trimws(location_clean)]

# # Remove unnecessary dx
dx_include <- c("All causes","Ischemic heart disease","Alzheimer's disease and other dementias",
                "Ischemic stroke","Intracerebral hemorrhage","Hypertensive heart disease")

cause_map <- c(
  ihd      = "Ischemic heart disease",
  istroke  = "Ischemic stroke",
  hstroke  = "Intracerebral hemorrhage",
  hhd      = "Hypertensive heart disease",
  aod      = "Alzheimer's disease and other dementias",
  all      = "All causes"
)

# AFTER  – define the vector once, reuse it
cause_cols <- names(cause_map)

# Filter the data to include only the specified causes
dt <- dt[cause_name %in% dx_include,]


rename_map <- c(
  "Global" = "Global",          # No match in locs; set to NA or remove
  "Moldova" = "Republic of Moldova",
  "Korea" = "Republic of Korea",
  "America" = "United States",
  "the Congo" = "Democratic Republic of the Congo",
  "Côte d'Ivoire" = "Ivory Coast",
  "the Niger" = "Niger",
  "People's Republic of Bangladesh" = "Bangladesh",
  "Independent State of Samoa" = "Samoa",
  "Portuguese Republic" = "Portugal",
  "Argentine Republic" = "Argentina",
  "Micronesia" = "Micronesia (Federated States of)",
  "Hellenic Republic" = "Greece",
  "the Philippines" = "Philippines",
  "People's Republic of China" = "China",
  "Federal Republic of Nigeria" = "Nigeria",
  "the Gambia" = "Gambia",
  "French Republic" = "France",
  "the Marshall Islands" = "Marshall Islands",
  "Togolese Republic" = "Togo",
  "Federal Democratic Republic of Ethiopia" = "Ethiopia",
  "Lebanese Republic" = "Lebanon",
  "Swiss Confederation" = "Switzerland",
  "Hashemite Kingdom of Jordan" = "Jordan",
  "Federal Democratic Republic of Nepal" = "Nepal",
  "Sultanate of Oman" = "Oman",
  "Plurinational State of Bolivia" = "Bolivia",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Bolivarian Republic of Venezuela" = "Venezuela (Bolivarian Republic of)",
  "Eastern Republic of Uruguay" = "Uruguay",
  "Socialist Republic of Viet Nam" = "Viet Nam",
  "Federal Republic of Somalia" = "Somalia",
  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
  "the Bahamas" = "Bahamas",
  "Federal Republic of Germany" = "Germany",
  "Democratic Socialist Republic of Sri Lanka" = "Sri Lanka",
  "People's Democratic Republic of Algeria" = "Algeria",
  "Gabonese Republic" = "Gabon",
  "Slovak Republic" = "Slovakia",
  "United Mexican States" = "Mexico",
  "Independent State of Papua New Guinea" = "Papua New Guinea",
  "Czech Republic" = "Czechia",
  "Federative Republic of Brazil" = "Brazil",
  "the Union of Myanmar" = "Myanmar",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Arab Republic of Egypt" = "Egypt",
  "the Comoros" = "Comoros",
  "Iran" = "Iran (Islamic Republic of)",
  "Grand Duchy of Luxembourg" = "Luxembourg",
  "the Netherlands" = "Netherlands",
  "Türkiye" = "Turkey",
  "Taiwan" = "Taiwan (Province of China)",
  "United Republic of Tanzania" = "Tanzania"
)

# Apply the renaming
dt[, location_clean := fifelse(location_clean %in% names(rename_map),
                                rename_map[location_clean],
                                location_clean)]

dt[, location_name := location_clean]

dt[, c("location_clean") := NULL]


# Filter only Indonesia
dt <- dt[location_name == "Indonesia",]

# save temp baseline rates from gbd 2023
saveRDS(dt, file = paste0(wd_raw,"GBD/","temp_1baseline_rates_gbd23.rds"))


# UNWPP instead
years <- 2000:2023

gbdpop <- readRDS(file = paste0(wd_data,"PopulationsSingleAge0050.rds"))


# # Create/modify age_group column from age_group_name
# gbdpop[, age_group := as.numeric(age_group_name)]
# gbdpop[age_group_name == "<1 year", age_group := 0]
# gbdpop[age_group_name == "95 plus", age_group := 95]
# 
# # Rename the second column to "location" (using positional renaming)
# setnames(gbdpop, 2, "location")

setnames(gbdpop, c("sex", "age", "Nx"),
                 c("sex_name", "age_group", "val"))

# Create totalpop by adding the iso3 code using countrycode
totalpop <- copy(gbdpop)
totalpop[, iso3 := countrycode(location, "country.name", "iso3c")]

# Standardize sex names to title case
totalpop[sex_name == "male", sex_name := "Male"]
totalpop[sex_name == "female", sex_name := "Female"]

# Fix countries names
totalpop[location == "Türkiye", location := "Turkey"]
totalpop[location == "Côte d'Ivoire", location := "Ivory Coast"]

# Filer Indonesia
totalpop <- totalpop[location == "Indonesia",]

saveRDS(totalpop, file = paste0(wd_raw,"GBD/","totalpop_ihme.rds"))


### 2. Define the projection function using data.table

setnames(dt,c("sex_name","age_name","cause_name","measure_name","metric_name","location_name")
         ,c("sex","age","cause","measure","metric","location"))


# remove AOD
#dt <- dt[cause != "Alzheimer's disease and other dementias",]

# Calibration starts

project.all <- function(Country,
                        yr,
                        ## short code  = long GBD cause name
                        cause_map = c(
                          ihd     = "Ischemic heart disease",
                          istroke = "Ischemic stroke",
                          hstroke = "Intracerebral hemorrhage",
                          hhd     = "Hypertensive heart disease",
                          aod     = "Alzheimer's disease and other dementias",
                          all     = "All causes")  # keep “all” last for readability
) {
  
  ## .....................................................................
  ##  Helpers 
  ## .....................................................................
  all_long  <- unname(cause_map["all"])        # “All causes”
  short_all <- "all"
  short_vec <- setdiff(names(cause_map), short_all)  # everything except “all”
  
  interpolate.rate <- function(y) {
    ages_in  <- c(seq(22, 92, 5), 95)
    ages_out <- 20:95
    if (sum(!is.na(y)) < 2)
      return(rep(NA_real_, length(ages_out)))
    approx(x = ages_in, y = y, xout = ages_out,
           rule = 2, method = "linear")$y
  }
  
  ## .....................................................................
  ##  Data for the chosen year 
  ## .....................................................................
  gbd_data  <- dt[year == yr]
  pop.df    <- totalpop[year_id == yr &
                          location == Country & age_group > 19,
                        .(location, sex = sex_name, age = age_group, Nx = val)]
  
  ## .....................................................................
  ##  Generic rate extractor 
  ## .....................................................................
  other.rates <- function(met, meas, colname, sel) {
    df <- gbd_data[metric == met & measure == meas & location == Country]
    df[, midptage := as.numeric(substr(age, 1, 2)) + 2]
    setorder(df, sex, cause, midptage)
    
    ## wide: one column per original cause name
    df <- dcast(df, sex + midptage ~ cause, value.var = "val")
    
    ## build the required short-name columns 
    ## if sel == 0 → background (All causes minus each cause)
    ## if sel == 1 → original cause values
    for (sc in short_vec) {
      long_nm <- cause_map[sc]
      if (sel == 1) {
        df[, (sc) := get(long_nm)]
      } else {
        df[, (sc) := get(all_long) - get(long_nm)]
      }
    }
    ## explicit “all” column
    df[, (short_all) := get(all_long)]
    
    ## keep only the needed columns, tidy up
    keep_cols <- c("sex", "midptage", names(cause_map))
    df <- df[, ..keep_cols]
    setorder(df, sex, midptage)
    
    ## interpolate to single-year ages 20:95 for each sex
    rates_sex <- rbindlist(lapply(unique(df$sex), function(sx) {
      mat  <- as.matrix(df[sex == sx, ..cause_cols])
      res  <- apply(mat, 2, interpolate.rate)
      out  <- as.data.table(res)
      out[, sex := sx]
      out[, age := 20:95]
      out[]
    }))
    
    ## long format + housekeeping
    rates_long <- melt(rates_sex,
                       id.vars      = c("sex", "age"),
                       variable.name = "cause",
                       value.name    = colname)
    rates_long[, (colname) := get(colname)/1e5]   # per-person units
    rates_long[, `:=`(location = Country, year = yr)]
    rates_long[]
  }
  
  ## .....................................................................
  ##  Prevalence and death rates 
  ## .....................................................................
  prev.rates  <- other.rates("Rate", "Prevalence", "PREVt0", 1)
  death.rates <- other.rates("Rate", "Deaths",      "DIS.mx.t0", 1)
  
  ## .....................................................................
  ##  Background mortality 
  ## .....................................................................
  bg.rates <- dcast(death.rates,
                    age + sex + location + year ~ cause,
                    value.var = "DIS.mx.t0")
  
  ## BG.mx.all = all minus *sum* of each specific cause
  bg.rates[, BG.mx.all :=
             get(short_all) - rowSums(.SD), .SDcols = short_vec]
  
  ## BG.mx.<cause> = all minus specific cause (vectorised)
  for (sc in short_vec)
    bg.rates[, paste0("BG.mx.", sc) := get(short_all) - get(sc)]
  
  ## reshape: wide → long, then strip prefix to recover short code
  bg.melt <- melt(
    bg.rates,
    id.vars      = c("BG.mx.all", "age", "sex", "location", "year", short_all),
    measure.vars = patterns("^BG\\.mx\\."),
    variable.name = "cause",
    value.name    = "BG.mx"
  )
  bg.melt[, cause := sub("^BG\\.mx\\.", "", cause)]
  setnames(bg.melt, short_all, "ALL.mx")
  
  ## .....................................................................
  ##  Merge everything 
  ## .....................................................................
  jvars <- c("age", "sex", "location", "year")
  baseline <- merge(bg.melt, prev.rates,  by = c(jvars, "cause"))
  baseline <- merge(baseline, death.rates, by = c(jvars, "cause"))
  baseline <- merge(baseline, pop.df,      by = c("location", "sex", "age"))
  
  setorder(baseline, sex, cause, age)
  baseline[, location := Country][]
}

#...........................................................
# Loop over locations and years, passing cause_map to project.all() ----
#...........................................................

### 3. Export results for each location

# Clean the temp folder

# folder <- "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/100MLives/data/processed/baseline_rates"

# Create a temporary directory for the processing data change to wd in final version
folder <- paste0(wd_temp, "baseline_rates")

if (!dir.exists(folder)) {
  dir.create(folder, recursive = TRUE)
}

# List all files (not directories) in there
files_to_delete <- list.files(
  path       = folder,
  full.names = TRUE,
  recursive  = FALSE
)

# Then delete them
success <- file.remove(files_to_delete)

# Get all locations from dt except "Global"

# Fix countries names
# dt[location == "Türkiye", location := "Turkey"]
# dt[location == "Côte d'Ivoire", location := "Ivory Coast"]
# dt[location == "United States of America", location := "United States"]
# dt[location == "Bolivia (Plurinational State of)", location := "Bolivia"]
# dt[location == "United Republic of Tanzania", location := "Tanzania"]



locs <- unique(dt[location != "Global", location])


for (loc in locs) {
  
  cat("Processing location:", loc, "\n")
  
  ## build one big data.table for 2000-2023
  data.out <- rbindlist(lapply(2000:2023, function(yr) {
    cat("year", yr, "\n")
    project.all(loc, yr, cause_map = cause_map)
  }))
  
  ## optional QC
  print(anyNA(data.out))
  print(unique(data.out$cause))   # still the short codes here
  
  ## 3. Replace short codes with full names using the map
  ##    – fast vectorised lookup, no fcase needed
  
  data.out[, cause := cause_map[cause]]
  
  
  ## 4. Save
  
  saveRDS(data.out,
          file = file.path(wd_temp, "baseline_rates",
                           paste0("baseline_rates_", loc, ".rds")))
}

#...........................................................
# Saving processed files ----
#...........................................................


# 1. List all .rds files in the folder
files <- list.files(
  #path       = "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/100MLives/data/processed/baseline_rates/", 
  path       = folder,
  pattern    = "\\.rds$", 
  full.names = TRUE
)

# 2. Read each one (assuming each .rds is a data.frame or data.table) and coerce to data.table
dt_list <- lapply(files, function(f) {
  dt <- readRDS(f)
  setDT(dt)  # convert to data.table by reference if it isn't already
  dt
})

# 3. Bind them all together, matching columns by name and filling missing ones
baseline_rates <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

# compute number of rows and chunk size
n     <- nrow(baseline_rates)
chunk <- ceiling(n / 5)

# loop over the three parts
for (i in 1:5) {
  start <- (i - 1) * chunk + 1
  end   <- min(i * chunk, n)
  
  part <- baseline_rates[start:end]
  
  saveRDS(
    part,
    file = paste0(wd_data, "baseline_rates_part", i, ".rds")
  )
}

rm(dt_list)


#saveRDS(baseline_rates, file = paste0(wd_data,"baseline_rates.rds"))

# Convert locs to a data.frame
locs <- unique(baseline_rates[, .(location)])
locs <- data.frame(locs = unique(locs))

# Save as .rds
saveRDS(locs, file = paste0(wd,"locs.rds"))

#...........................................................
# Cleaning up the workspace ----
#...........................................................

rm(list = ls()[sapply(ls(), function(x) is.data.frame(get(x)))])
#rm(is,j,locs)



