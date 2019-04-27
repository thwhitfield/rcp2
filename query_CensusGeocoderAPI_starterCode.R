##############################################################################
# QUERY_CENSUSGEOCODERAPI_STARTER_CODE.R
#
# Red Cross Phase 2, Census Geocoder API Starter Code, 2019
# S. Sylvester
#
# This code deomonstrates how to call the Census Geocoder API to
# extract the latitude and longitude of addresses from Census.
#
# Inputs:
#     sample_dataset_CensusGeocoderAPI.csv
#
# Outputs:
#     sample_geocoded_CensusGeocoderAPI_results.csv
#
#############################################################################

# Clear workspace
rm(list = ls())

# Load libraries
library(httr)
library(jsonlite)

# Disable scientific notation
options(scipen=999)

# Make sample datafile
df = read.csv("~/Documents/DKDC_RC/data/chunked_NFIRS_addresses_idx/NFIRS_addresses_chunk1_idx_1_10000.csv",
              stringsAsFactors = FALSE, header = FALSE)
state_list = unique(df[,4])
idx = c()
for(state in state_list){
  idx = c(idx,which(df[,4] == state)[1:10])
}

df = df[idx,]
write.table(df,"sample_dataset_CensusGeocoderAPI.csv", sep = ",", row.names = F, col.names = FALSE)


#################################################################################################
# Get Census data via Census Geocoder API
#   For this script, the geocoder API endpoint extracts geographial information as well as
#   approximated latitude & longitude coordinates for addresses. Change the API URL if
#   only coordinates are needed.
#       apiurl <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"
#
#   General API information: https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf
#   Vintages for each benchmark: https://geocoding.geo.census.gov/geocoder/vintages?form
#
#   Benchmark "Public_AR_Current" was used to extract information using the most current
#   Census files.
#################################################################################################

# Store API address
#apiurl <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch" # returns Census info & lat-long
apiurl <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch" # returns only lat-long

convert_to_raw <- function(raw_content) {
  out <- tryCatch(
    {
      rawToChar(raw_content)
    },
    error=function(cond) {
      message("Unable to convert the results from to char")
      message("Original error message:")
      message(cond)
      return(NA)
    },
    warning=function(cond) {
      message("Original warning message:")
      message(cond)
    },
    finally={
      message("Converted raw results to char")
    }
  )    
  return(out)
}


# Call Census Geocoder API
tic = Sys.time()
req <- POST(apiurl,
            body = list(addressFile = upload_file("sample_dataset_CensusGeocoderAPI.csv"),
                        benchmark = "Public_AR_Current")) # current Census
#req <- POST(apiurl,
#            body = list(addressFile = upload_file("sample_dataset_CensusGeocoderAPI.csv"),
#                        benchmark = "Public_AR_Census2010")) # 2010 Census
Sys.time() - tic

# Convert API results to raw text data
temp0 <- convert_to_raw(req$content)

# Seperate raw text data into different columns
if(!is.na(temp0)){
  temp <- strsplit(temp0,"\n")[[1]]
  temp <- strsplit(temp,",")
  temp <- plyr::ldply(temp, rbind)
  temp <- as.data.frame(gsub('"', "", as.matrix(temp)), stringsAsFactors = F)
  
  # Column for extra stuff if address line is split into 2
  colnames(temp) <- c("ID","ORIGINAL_STREET","ORIGINAL_CITY","ORIGINAL_STATE","ORIGINAL_ZIPCODE",
                      "MATCH","MATCH_TYPE",
                      "MATCHED_STREET","MATCHED_CITY","MATCHED_STATE","MATCHED_ZIPCODE",
                      "X","Y","TIGER_LINE_ID","TIGER_LINE_SIDE","EXTRA")[1:dim(temp)[2]]

  # Save dataframe as a CSV-file
  write.csv(temp, file = "sample_geocoded_CensusGeocoderAPI_results.csv", row.names = F)
}else{
  print("API failed to find lat-longs for address chunk!!")
}