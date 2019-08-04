##############################################################################
# impact_data_prep.R
#
# Red Cross Fire Alarm Phase 2
# J. Zlotoff
#
# This script preps a combined data set from  NFIRS data and ARC Home Visits
# to analyze, in another script, the (potential) impact of these visits on future
# fires.
#
# This script assumes that all input files listed below are stored in a
# ./data subfolder.
#
# Inputs:
#     NFIRS 2009 - 2016.txt
#         Raw NFIRS data with incident date, other fields
#
#     NFIRS_2009_2016_geocoded_with_tract.csv
#         Geocoded NFIRS data from previous phase
#
#     HomeFire_SmokeAlarmInstalls_NEW.csv
#         ARC Home Visit data
#
#    state_FIPs_codes.txt
#         https://www.census.gov/geo/reference/ansi_statetables.html
#         under "National FIPS and GNIS Codes File" tab
#         Manually downloaded and saved as a TXT file
#
#     shapefiles_tract_2018Census/tl_2018_*_tract.shp
#     shapefiles_tract_2010Census/tl_2010_*_tract10.shp
#     State-based TIGER/LINE shapefile, state census tract 2018/2010
#         Link to download found here: https://www.census.gov/geo/maps-data/data/tiger-line.html
#
#    pdb2018trv4_us.csv
#         2018 Planning Database from Census
#         https://www.census.gov/research/data/planning_database/2018/
#         Manually downloaded
#
# Outputs:
#   impact_data.rdata
#       Fires and home visits per year, grouped by Census tract
#
#   The intermediate output files are listed below:
#       nfirs_prepped.rdata: pre-processed
#       nfirs_w_tract.rdata: with Census tract
#       nfirs_w_tract_total.rdata: summarized by Census tract (final for merge)
#
#       home_visits_w_tract.rdata: home visits summarized by Census tract
#
#############################################################################

# Clear workspace
#rm(list = ls())

# Load libraries
library(tictoc)
library(xml2)
library(XML)
library(sf)
library(raster)
library(rgdal)
library(tidyverse)

# Set directories/inputs
data_folder <- "data"
input_nfirs <- "NFIRS 2009 - 2016.txt"
input_nfirs_geo <- "NFIRS_2009_2016_geocoded_with_tract.csv"
input_hvisits <- "HomeFire_SmokeAlarmInstalls_NEW.csv"
input_census <- "pdb2018trv4_us.csv"
input_model_p1 <- "tracts_74k_weighted_linear_preds_upsampled.csv"

####################

# Add tract to nfirs data with all fields, by state
#   Pre-process raw file

nfirs_prep <- tic()

nfirs <- read.csv(paste0(data_folder, '//', input_nfirs), stringsAsFactors = F, sep = ",")

nfirs <- nfirs %>%
    mutate(
           inc_date = as.character(inc_date),
           inc_year = as.numeric(sub("^[^/]+/[^/]+/([0-9]+).*", "\\1", inc_date)))

nfirs <- nfirs %>%
    rowwise() %>%
    mutate(ORIGINAL_ADDRESS = str_to_lower(str_c(street, city, state, zip5, sep=", "))) %>%
    select(ORIGINAL_ADDRESS, state, inc_year)

nfirs <- nfirs %>%
    group_by(ORIGINAL_ADDRESS, state, inc_year) %>%
    mutate(inc_count = n()) %>%
    unique() %>%
    ungroup()

nfirs <- nfirs %>%
    spread(key=inc_year, val=inc_count) %>%
    rename_at(vars(`2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`), list(~paste0("fires_",.)))

save(nfirs, file = "nfirs_prepped.rdata")
toc(nfirs_prep)

####################

load("nfirs_prepped.rdata")

# Add tract to nfirs data with all fields, by state
#   Add tract by state
#   ~30 minutes
nfirs_loop <- tic()
nfirs$TRACTCE <- NA
nfirs$COUNTYFP <- NA
nfirs$GEOID <- NA
state_list <- sort(unique(nfirs$state))

for(st in state_list) {
    print(paste("processing state:", st))

    idx <- which((nfirs$state == st)
                 & (!is.na(nfirs$ORIGINAL_ADDRESS)),
                 arr.ind = TRUE)

    # read in tract data for current state
    geo <- read.csv(paste0(data_folder, '//',input_nfirs_geo),stringsAsFactors = F)
    geo <- geo[geo$STATE==st,]
    geo <- geo[which(!is.na(geo$COUNTYFP)),]
    geo <- geo %>%
        mutate(ORIGINAL_ADDRESS = str_to_lower(ORIGINAL_ADDRESS)) %>%
        select(ORIGINAL_ADDRESS, STATE, TRACTCE, COUNTYFP, GEOID)
    geo <- geo[!duplicated(geo$ORIGINAL_ADDRESS),] # remove duplicate address

    # pull out current state for join/update
    temp_df <- nfirs[idx, c("ORIGINAL_ADDRESS") ]
    joined <- temp_df %>%
        left_join(geo, by="ORIGINAL_ADDRESS")

    nfirs$TRACTCE[idx] <- joined$TRACTCE
    nfirs$COUNTYFP[idx] <- joined$COUNTYFP
    nfirs$GEOID[idx] <- joined$GEOID

}
toc(nfirs_loop)

save(nfirs, file = "nfirs_w_tract.rdata")

rm(geo, temp_df, joined)

####################

# Summarize nfirs data by Census tract
load("nfirs_w_tract.rdata")

nfirs <- nfirs %>%
    select(-ORIGINAL_ADDRESS) %>%
    rename(STATE=state) %>%
    group_by(STATE, COUNTYFP, TRACTCE) %>%
    mutate_at(vars(starts_with("fires")), list(~replace_na(.,0)),
              vars(starts_with("fires")), list(~sum(.))) %>%
    unique() %>%
    ungroup()

nfirs <- nfirs %>%
    filter(!is.na(TRACTCE))

save(nfirs, file = "nfirs_w_tract_total.rdata")

####################

# Prep home visits data for adding tract
hvisits <- read.csv(paste0(data_folder, '//',input_hvisits),stringsAsFactors = F)
hvisits <- hvisits[which(!is.na(hvisits$Census.Block.Group.X)),] # remove those missing census blocks

state_fips <- read.table(paste(data_folder,"/state_FIPs_codes.txt",sep = ''), header = TRUE, sep = "|", stringsAsFactors = F)

hvisits <- hvisits %>%
    mutate(visit_year = as.numeric(sub("^[0-9]+/[0-9]+/(....)$", "\\1", `In.Home.Visit.Date`)))

df <- hvisits %>%
    group_by(`Census.Block.Group.X`, `Census.Block.Group.Y`, visit_year) %>%
    mutate(people_seen = sum(`People.Served`),
           homes_visited = n()) %>%
    select(`Census.Block.Group.X`, `Census.Block.Group.Y`, visit_year, people_seen, homes_visited, State) %>%
    unique() %>%
    ungroup()

df <- df %>%
    rename(STUSAB = State) %>%
    left_join(state_fips) %>%
    select(-STATENS) %>%
    rename(X = `Census.Block.Group.X`,
           Y = `Census.Block.Group.Y`) %>%
    mutate(state_fips = STATE)

rm(hvisits)

####################

# Find census tract for home visits via intersection of spatial dataframes, blocked by state
# ~3 minutes (from local shapefiles)
start_time <- tic()
df$COUNTYFP <- NA
df$TRACTCE <- NA
df$GEOID <- NA
df$NAME <- NA
df$NAMELSAD <- NA
df$MTFCC <- NA
df$FUNCSTAT <- NA
fips_list <- unique(df$state_fips)
fips_list <- sort(fips_list[which(!is.na(fips_list))])

# loop through states
for (fips in fips_list){
    print(paste("processing state_fips:", fips))
    idx <- which((df$state_fips == fips)
                 & (!is.na(df$X))
                 & (!is.na(df$Y)),
                 arr.ind = TRUE)
    if (length(idx) > 0){
        # Load shapefile ... MN and MO shapefiles are corrupted. will use 2010 shapefiles instead
        if (fips==27 | fips==29) {
            filename_ <- paste("tl_2010_",sprintf("%02d", fips),"_tract10.shp", sep = '')
            s_temp <- shapefile(paste(data_folder,'/shapefiles_tract_2010Census/',filename_,sep=''))
            names(s_temp@data) <- c("STATEFP","COUNTYFP","TRACTCE","GEOID","NAME","NAMELSAD","MTFCC",
                                    "FUNCSTAT","ALAND","AWATER","INTPTLAT","INTPTLON")
        }
        else {
            filename_ <- paste("tl_2018_",sprintf("%02d", fips),"_tract.shp", sep = '')
            s_temp <- shapefile(paste(data_folder,'/shapefiles_tract_2018Census/',filename_,sep=''))
        }

        # Make lat-long coords into sf object/ spatial dataframe
        temp_df <- df[idx,c('X','Y','STATE')]
        temp_df$index <- idx
        coordinates(temp_df) <- ~X+Y
        proj4string(temp_df) <- proj4string(s_temp)

        # Join spatial dataframes to find point & polygon intersections
        joined_df <- over(temp_df,s_temp)

        # Add census data to RC dataframe
        df$COUNTYFP[idx] <- joined_df$COUNTYFP
        df$TRACTCE[idx] <- joined_df$TRACTCE
        df$GEOID[idx] <- joined_df$GEOID
        df$NAME[idx] <- joined_df$NAME
        df$NAMELSAD[idx] <- joined_df$NAMELSAD
        df$MTFCC[idx] <- joined_df$MTFCC
        df$FUNCSTAT[idx] <- joined_df$FUNCSTAT
    }
}
toc(start_time)

df <- df %>%
    select(-STATE)

# QC
sort(unique(df$STUSAB[!is.na(df$TRACTCE)]))

save(df, file='home_visits_w_tract.rdata')

rm(joined_df, s_temp, temp_df)

####################

load('home_visits_w_tract.rdata')

# home visit post-processing
home_visits <- df %>%
    rename(STATE = STUSAB) %>%
    select(STATE, COUNTYFP, TRACTCE, visit_year, homes_visited, people_seen) %>%
    mutate(TRACTCE = as.numeric((TRACTCE)),
           COUNTYFP = as.numeric(COUNTYFP))

# roll up by tract and year
home_visits <- home_visits %>%
    group_by(STATE, COUNTYFP, TRACTCE, visit_year) %>%
    mutate(homes_visited = sum(homes_visited),
           people_seen = sum(people_seen)) %>%
    unique() %>%
    arrange(STATE, COUNTYFP, TRACTCE, visit_year)

# reshape year to columns
home_visits <- home_visits %>%
    select(-people_seen) %>%
    spread(key=visit_year, val=homes_visited) %>%
    rename_at(vars(`2014`, `2015`, `2016`, `2017`, `2018`), list(~paste0("visits_",.)))

save(home_visits, file='home_visits_w_tract.rdata')
rm(df)

####################

# Add select Census data at the tract level
#   Pre-process raw file

census_prep <- tic()

census <- read.csv(paste0(data_folder, '//', input_census), stringsAsFactors = F, sep = ",")

census <- census %>%
    select(GIDTR,State,County,Tract,Tot_Population_CEN_2010,pct_RURAL_POP_CEN_2010,
           pct_Pop_65plus_CEN_2010,pct_Tot_Occp_Units_CEN_2010,avg_Agg_House_Value_ACS_12_16)

census <- census %>%
    rename(STATE = State)

census <- census %>%
    left_join(state_fips) %>%
    select(-STATE, -STATE_NAME, -STATENS) %>%
    rename(STATE = STUSAB,
           COUNTYFP = County,
           TRACTCE = Tract) %>%
    select(STATE, COUNTYFP, TRACTCE, everything())

census <- census %>%
    group_by(STATE, COUNTYFP, TRACTCE) %>%
    mutate(rown = row_number()) %>%
    filter(rown==1) %>%
    select(-rown) %>%
    ungroup()

save(census, file='census_tracts.rdata')
toc(census_prep)

####################

# Merge NFIRS and Home Visit (df) data on Census tract
load("nfirs_w_tract_total.rdata")
load("home_visits_w_tract.rdata")

impact_data <- nfirs %>%
    left_join(home_visits) %>%
    mutate_at(vars(starts_with("visits")), list(~replace_na(., 0))) %>%
    select(STATE, COUNTYFP, TRACTCE, GEOID, everything())

# Merge on Census tract characteristics
load('census_tracts.rdata')

impact_data <- impact_data %>%
    left_join(census)

save(impact_data, file="impact_data.rdata")

rm(census, home_visits, nfirs, state_fips)
