##############################################################################
# GET_2018_CENSUS_TRACTS_B.R
#
# Red Cross Fire Alarm Phase 2, 2019
# S. Sylvester
#
# This script is an exact copy of get_2018_census_tracts.R, but it was
# altered so that the inputs are J. Janecek's geocoding via software.
#
# This script reverse geocodes lat-long coordinates to find census tracts.
# The overall approach finds the tract polygons for each lat-long coordinate
# and extracts census tract and block information from state tract shapefiles.
#
# Inputs:
#     State-based TIGER/LINE shapefile, state census tract 2018:
#         Link to download found here: https://www.census.gov/geo/maps-data/data/tiger-line.html
#         List of FTP links (downloaded by code below): "ftp://ftp2.census.gov/geo/tiger/TIGER2018/TRACT/"
#         **WARNING** Connection to server often times out. Add a try/catch to attempt download again if
#               download fails
#
#     State FIPs code, state_FIPs_codes.txt:
#         https://www.census.gov/geo/reference/ansi_statetables.html
#         under "National FIPS and GNIS Codes File" tab
#         Manually downloaded and saved as a TXT file
#
#     FIXED NFIRS Geocode.csv
#         Lat-long coordinates of addresses by J. Janecek via a program.
#
# Outputs:
#     NFIRS_2009_2016_FIXED_geocodes_census_tracts.csv
#############################################################################

# Clear workspace
rm(list = ls())

# Load libraries
library(tictoc)
library(xml2)
library(XML)
library(sf)
library(raster)

# Build "NOT IN" function
'%!in%' <- function(x,y)!('%in%'(x,y))

# Set directories
data_folder <- "~/Documents/DKDC_RC/data"

# Load in data sets
state_fips <- read.table(paste(data_folder,"/state_FIPs_codes.txt",sep = ''), header = TRUE, sep = "|", stringsAsFactors = F)
df <- read.csv("~/Documents/DKDC_RC/data/FIXED NFIRS Geocode.csv",stringsAsFactors = F)
df$X.latitude. <- as.numeric(df$X.latitude.)
df$X.longitude. <- as.numeric(df$X.longitude.)
df <- df[which(unlist(lapply(df$X.latitude.,is.numeric)) & df$X.latitude. != 0),]
df$ID <- paste0('rcp2_id',df$ROWNUM)

dataset_state_list <- state_fips$STUSAB

# Build URLs for census.gov FTPing
base_url <- "ftp://ftp2.census.gov/geo/tiger/TIGER2018/TRACT/"
sf_names <- c()
url <- c()
counter <- 1
for (abbrev in dataset_state_list){
  if (abbrev %in% state_fips$STUSAB){
    idx <- which(state_fips$STUSAB == abbrev)
    fips_temp <- state_fips$STATE[idx]
    sf_names[counter] <- paste("tl_2018_",sprintf("%02d", fips_temp),"_tract.zip", sep = '')
    url[counter] <- paste(base_url,sf_names[counter], sep = '')
    counter <- counter + 1
  }
}

# Check to see if shapefiles folder exists, create if missing
if ('shapefiles_tract_2018Census' %!in% dir(data_folder)){
    dir.create(file.path(data_folder, 'shapefiles_tract_2018Census'))
}

# Download shapefiles for each state. There is some hang time/ timeouts
# occassionally. May download manually since that is faster
for (i in 1:length(url)){
  temp_sf_names <- gsub(".zip",".shp",sf_names[i])
    if (temp_sf_names %!in% dir(paste(data_folder,'/shapefiles_tract_2018Census',sep=''))){
      download.file(url[i],paste(data_folder,'/shapefiles_tract_2018Census/',sf_names[i],sep = ''),mode = "wb")
      unzip(paste(data_folder,'/shapefiles_tract_2018Census/',sf_names[i],sep=''),
            exdir = paste(data_folder,'/shapefiles_tract_2018Census',sep=''))
    }
}

# Append fips code to df
df$state_fips <- NA
for (abbrev in dataset_state_list){
    idx_rc <- which(df$STATE == abbrev)
    idx_fips <- which(state_fips$STUSAB == abbrev)
    if ((length(idx_rc > 0)) & (length(idx_fips) > 0)){
        fips_temp <- state_fips$STATE[idx_fips]
        df$state_fips[idx_rc] <- fips_temp
    }
}

# Find tract via intersection of spatial dataframes, blocked by state
# 8 minutes
start_time <- tic()
df$COUNTYFP <- NA
df$TRACTCE <- NA
df$GEOID <- NA
df$NAME <- NA
df$NAMELSAD <- NA
df$MTFCC <- NA
df$FUNCSTAT <- NA
fips_list <- unique(df$state_fips)
fips_list <- fips_list[which(!is.na(fips_list), arr.ind = TRUE)]

df$Y <- df$X.latitude.
df$X <- df$X.longitude.

# MN and MO shapefiles are corrupted. will use 2010 shapefiles instead
#for (fips in fips_list[!fips_list %in% c(27,29)]){
for (fips in fips_list){
  print(fips)
    idx <- which((df$state_fips == fips)
                 & (!is.na(df$X))
                 & (!is.na(df$Y)),
                 arr.ind = TRUE)
    if (length(idx) > 0){
        # Load shapefile
        filename_ <- paste("tl_2018_",sprintf("%02d", fips),"_tract.shp", sep = '')
        s_temp <- shapefile(paste(data_folder,'/shapefiles_tract_2018Census/',filename_,sep=''))
        
        # Make lat-long coords into sf object/ spatial dataframe
        temp_df <- df[idx,c('X','Y','STATE')]
        #if(fips == 27){
        #  idx <- idx[1:25100]
        #  temp_df <- df[idx,c('X','Y','STATE')]
        #}
        #if(fips == 29){
        #  idx <- idx[1:37000]
        #  temp_df <- df[idx,c('X','Y','STATE')]
        #}
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

write.csv(df,"~/Documents/DKDC_RC/data/NFIRS_2009_2016_FIXED_geocodes_census_tracts.csv",row.names = F)
