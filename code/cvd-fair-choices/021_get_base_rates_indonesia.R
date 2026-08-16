#...........................................................
# Documentation ----
#...........................................................

# The input of this file is the GBD 2023 data at subnational level.
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

# Indonesia Level 2 DM

# https://collab2023.healthdata.org/gbd-results?params=gbd-api-2023-permalink/d82f6c1f52b4905e2fcfcf571df01abe

# load 2020- 2023

# List all CSV files
files <- list.files(paste0(wd_raw,"GBD/gbd2023-indonesia-fair/"), pattern = "\\.csv$", full.names = TRUE)

# Read and combine using rbindlist
dt_23 <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)

dt<-data.table(dt_23)

dt[, upper:=NULL]
dt[, lower:=NULL]

unique(dt$year)
unique(dt$location_name)
unique(dt$cause_name)
unique(dt$age_name)


# AFTER  – define the vector once, reuse it
cause_cols <- names(cause_map)

# Filter the data to include only the specified causes
dt <- dt[cause_name %in% dx_include,]

# Filter only Indonesia
dt <- dt[location_name == "Indonesia",]

# Filter to the modeled age bands (age 0 through the open-ended 95+ group),
# taken from the central GBD band definition in 01_utils (driven by the
# min/max_model_age set in 00). Previously this hard-coded the adult 20-95 bands.
dt <- dt[age_name %in% gbd_age_bands(min_model_age, max_model_age)$label, ]

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
                        cause_map) {

  ## `cause_map` MUST be supplied by the caller from the central config in 00.
  ## No local default: a divergent inner map (which used to list "aod" and omit
  ## "dm2") would silently drift from the single source of truth.
  if (missing(cause_map) || is.null(cause_map))
    stop("project.all(): cause_map not supplied; pass the central cause_map from 00.")

  ## .....................................................................
  ##  Helpers
  ## .....................................................................
  all_long  <- unname(cause_map[all_cause_code])   # "All causes"
  short_all <- all_cause_code
  short_vec <- setdiff(names(cause_map), short_all)  # everything except "all"

  ## Interpolate GBD band-level rates (anchored at band midpoints, ages_in) to
  ## single years of age across the full model grid (age_single = 0:95). ages_in
  ## is the set of band midpoints actually present, so this is robust to which
  ## bands the extract contains and treats age 95 as the open-ended 95+ anchor.
  interpolate.rate <- function(y, ages_in) {
    ages_out <- age_single
    if (sum(!is.na(y)) < 2)
      return(rep(NA_real_, length(ages_out)))
    approx(x = ages_in, y = y, xout = ages_out,
           rule = 2, method = "linear")$y
  }
  
  ## .....................................................................
  ##  Data for the chosen year 
  ## .....................................................................
  gbd_data  <- dt[year == yr]
  ## Population by single year of age (UNWPP single-age file). Its `age` column
  ## (renamed `age_group`) is a 1-based INDEX: index 1 == actual age 0, ...,
  ## index 101 == actual age 100+. Convert index -> actual age, then pool all
  ## ages >= max_model_age into the open-ended terminal group (95+).
  pop.df    <- totalpop[year_id == yr & location == Country,
                        .(location, sex = sex_name, age = age_group - 1L, Nx = val)]
  pop.df[age >= max_model_age, age := max_model_age]
  pop.df    <- pop.df[age >= min_model_age,
                      .(Nx = sum(Nx)), by = .(location, sex, age)]
  
  ## .....................................................................
  ##  Generic rate extractor 
  ## .....................................................................
  other.rates <- function(met, meas, colname, sel) {
    df <- gbd_data[metric == met & measure == meas & location == Country]
    ## GBD band label -> single-age interpolation midpoint (central helper).
    ## Replaces substr(age,1,2)+2, which produced NA for the irregular young
    ## bands ("<1 year", "12-23 months", "2-4 years", ...).
    df[, midptage := gbd_band_midpoint(age)]
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

    ## Young ages: GBD does not report Deaths for some diseases below ~15y
    ## (rows simply absent -> NA after dcast). Treat an absent DISEASE rate as
    ## 0 (no burden), never NA -- NA would make approx() flat-extrapolate an
    ## adult rate down onto children. The all-cause column is never zero-filled
    ## (a missing all-cause value is a real error we must not mask).
    for (sc in short_vec) df[is.na(get(sc)), (sc) := 0]

    ## interpolate to single-year ages (age_single = 0:95) for each sex, using
    ## the band midpoints actually present as the interpolation anchors.
    rates_sex <- rbindlist(lapply(unique(df$sex), function(sx) {
      dsx     <- df[sex == sx][order(midptage)]
      ages_in <- dsx$midptage
      mat     <- as.matrix(dsx[, ..cause_cols])
      res     <- apply(mat, 2, interpolate.rate, ages_in = ages_in)
      out     <- as.data.table(res)
      out[, sex := sx]
      out[, age := age_single]
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


##  Incident rates from GBD

## https://collab2023.healthdata.org/gbd-results?params=gbd-api-2023-permalink/6e31df864b55737aa223f47834aedda1


