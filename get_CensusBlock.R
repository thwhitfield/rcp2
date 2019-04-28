##############################################################################
# GET_2018_CENSUS_TRACTS_B.R
#
# Red Cross Fire Alarm Phase 2, 2019
# S. Sylvester
#
# This script downloads Census block shapefiles for each state.
#
# Inputs:
#     State-based TIGER/LINE shapefile, state Census Block 2018:
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
#     NFIRS_2009_2016_geocoded_with_tract.csv
#         Geocoded NFIRS data.
#
# Outputs:
#     Blockl group shapefiles: tl_2018_[state FIPS]_bg
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
#df = read.csv("~/Documents/DKDC_RC/data/NFIRS_2009_2016_geocoded_with_tract.csv",stringsAsFactors = F)
#final_save_name = "NFIRS_2009_2016_Geocodes_CensusBlock.csv"
df = read.csv("~/Documents/DKDC_RC/data/NFIRS_2009_2016_FIXED_unique_geocodes_census_tracts.csv",stringsAsFactors = F)
final_save_name = "NFIRS_2009_2016_FIXED_Geocodes_CensusBlock.csv"

state_fips = read.table(paste(data_folder,"/state_FIPs_codes.txt",sep = ''), header = TRUE, sep = "|", stringsAsFactors = F)
state_fips$padded_state_fips = sprintf("%0.2d",state_fips$STATE)

bg_url_list = paste0("https://www2.census.gov/geo/tiger/TIGER2018/BG/tl_2018_",state_fips$padded_state_fips,"_bg.zip")

# Check to see if shapefiles folder exists, create if missing
if ('shapefiles_block_2018Census' %!in% dir(data_folder)){
  dir.create(file.path(data_folder, 'shapefiles_block_2018Census'))
}

bg_folder = file.path(data_folder, 'shapefiles_block_2018Census')

# Download shapefiles for each state. There is some hang time/ timeouts
# occassionally. May download manually since that is faster
for (i in 1:length(bg_url_list)){
  save_name = gsub('https://www2.census.gov/geo/tiger/TIGER2018/BG/','',bg_url_list[i])
  destination_file = paste0(bg_folder,'/',save_name)
  if (save_name %!in% dir(bg_folder)){
    download.file(bg_url_list[i],destination_file,mode = "wb")
    unzip(destination_file,exdir = paste(data_folder,'/shapefiles_block_2018Census/',gsub('.zip','',save_name),sep=''))
  }
}


# Append fips code to df
dataset_state_list = unique(df$STATE)
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

df$STATEFP = NA
df$COUNTYFP = NA
df$TRACTCE = NA
df$BLKGRPCE = NA
df$GEOID = NA
df$NAMELSAD = NA
df$MTFCC = NA
df$FUNCSTAT = NA
df$ALAND = NA
df$AWATER = NA
df$INTPTLAT = NA
df$INTPTLON = NA

fips_list <- unique(df$state_fips)
fips_list <- fips_list[which(!is.na(fips_list), arr.ind = TRUE)]

tic_ = Sys.time()
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
    filename_ <- paste("tl_2018_",sprintf("%02d", fips),"_bg.shp", sep = '')
    s_temp <- shapefile(paste(bg_folder,gsub('.shp','',filename_),filename_,sep='/'))
    
    # Make lat-long coords into sf object/ spatial dataframe
    temp_df <- df[idx,c('X','Y','STATE')]
    
    # 27 & 29 are corrupt here too?? so cut
    if (!grepl('FIXED',final_save_name)){
      if(fips == 27){
        idx <- idx[1:25100]
        temp_df <- df[idx,c('X','Y','STATE')]
      }
      if(fips == 29){
        idx <- idx[1:37000]
        temp_df <- df[idx,c('X','Y','STATE')]
      }
    }
    
    temp_df$index <- idx
    coordinates(temp_df) <- ~X+Y
    proj4string(temp_df) <- proj4string(s_temp)
    
    # Join spatial dataframes to find point & polygon intersections
    joined_df <- over(temp_df,s_temp)
    
    # Add census data to RC dataframe
    df$COUNTYFP[idx] = joined_df$COUNTYFP
    df$TRACTCE[idx] = joined_df$TRACTCE
    df$GEOID[idx] = joined_df$GEOID
    df$NAME[idx] = joined_df$NAME
    df$NAMELSAD[idx] = joined_df$NAMELSAD
    df$MTFCC[idx] = joined_df$MTFCC
    df$FUNCSTAT[idx] = joined_df$FUNCSTAT
    df$STATEFP[idx] = joined_df$STATEFP
    df$BLKGRPCE[idx] = joined_df$BLKGRPCE
    df$ALAND[idx] = joined_df$ALAND
    df$AWATER[idx] = joined_df$AWATER
    df$INTPTLAT[idx] = joined_df$INTPTLAT
    df$INTPTLON[idx] = joined_df$INTPTLON
    
  }
}

write.csv(df,paste0("~/Documents/DKDC_RC/data/",final_save_name),row.names = F)
