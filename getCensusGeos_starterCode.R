##############################################################################
# getCensusGeos_starterCode.R
#
# Red Cross Fire Alarm Phase 2, Data Dive Prep, April 2019
# S. Sylvester (ssylvest00@gmail.com)
#
# This code demonstrates how to extract Census data for a collection of
# lat-long coordinates of a location using only Census shapefiles. This code
# works by intersecting a lat-long point with polygons in a state's shapefile.
# For this to work properly, the state must be known and included in the
# dataframe that contains the lat-long coordinates.
#
# Shapefies can be found in the RCP2 input folder. Original files were
# downloaded from Census.
#
# Inputs:
#     Latitude-longitude of a place
#     State shapefiles
#     State FIPs
#
# Outputs:
#     Census tract
#
#############################################################################

# Clear workspace
rm(list = ls())

# Load packages
library(sf)
library(raster)

# Load state FIPs file
data_folder = '~/Documents/DKDC_RC/DataDive_Spring2019_prep'
state_fips = read.table(paste(data_folder,"/state_FIPs_codes.txt",sep = ''),
                        header = TRUE, sep = "|", stringsAsFactors = F)

# Create a dataframe that contains the lat-long and state of each location
whiteHouse_df = data.frame(id = 'rcp2_1',
                           Y = 38.8977, #latitude
                           X = -77.0365, #longitude
                           state = 'DC')

# Find the state FIPs for state in dataframe
idx = which(state_fips$STUSAB == whiteHouse_df$state)
fips = state_fips[idx,'STATE']

# Load matching shapefile
filename_ = paste("tl_2018_",sprintf("%02d", fips),"_tract.shp", sep = '')
sf_temp = shapefile(paste(data_folder,'/',filename_,sep=''))

# Convert dataframe to a spatial points dataframe
temp_df = whiteHouse_df
coordinates(temp_df) = ~X+Y
proj4string(temp_df) = proj4string(sf_temp)

# Join spatial dataframe to find point & polygon intersections
joined_df = over(temp_df,sf_temp)

# Join dataframes
df_final = merge(data.frame(whiteHouse_df),joined_df)
df_final