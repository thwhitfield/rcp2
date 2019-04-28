#########
#INITIAL RUN OF MASTER DATASET COMPILING
## BY MATT BARGER (Hi, I'm Matt.)
##
##
#Notes: 
# Summary: I'm using a simple bind_rows function to put all the datasets together, but you want to make sure that
# two unifiers (GEOID and date_year) are aligned. Also, to keep things tidy, I'm adding a data identifier (data_id)
# 0. Obviously this isn't perfect. Changes welcome. Specifically:
# 0a. add date and time increments for NFIRS and ARC_Preparedness database
# 0b. add process to build on ACS/AHS/SVI data
# 0c. add instructions for non-time-based data (i.e., ARC_saved_lives data)
# 0d. add ARC Boundary as Geographies.
#
# 1. All data will come from the RCP2 data drive: Here's the shareable link to the Drive itself: "https://drive.google.com/open?id=1-qw4vqGA9EbphXZFcaQuJlJvwWD_0v9w"
# 2. The files are big. Prepare for that. You will need more than 4 GB of RAM.

rm(list = ls())

library(tidyverse)
library(tidycensus)
library(readr)

##NFIRS DATA COMBINED BY CENSUS BLOCK GROUP (obv find latest file)... don't worry about the error message (it's for the time variable)
NFIRS_2009_2016_Combined_Census_Tract <- read_csv("NFIRS_2009_2016_Combined_Census_Tract.csv")
nfirs <- NFIRS_2009_2016_Combined_Census_Tract
nfirs <- nfirs %>% 
  mutate(data_id = "NFIRS") %>%
  separate(inc_date,
           into = c("inc_date", "inc_time"),
           sep = " ") %>%
  mutate(date_year = substr(inc_date, -4, -1),
         GEOID = as.character.numeric_version(GEOID))
  
##ARC RESPONSE DATA
ARC_RESPONSE <- read_csv("ARC Response Data (Phase 2).csv", 
                         col_types = cols(FIPS = col_character(), 
                                          Zip = col_character(),
                                          Year = col_character()))
arc_r <- ARC_RESPONSE %>% 
  mutate(data_id = "ARC-RESPONSE")
  select(GEOID = FIPS, date_year = Year, everything())

##ARC SAVED LIVES DATA, No date or time
ARC_SavedLives <- read_csv("ARC Lives Saved Locations (Phase 2).csv", 
                           col_types = cols(FIPS = col_character(), 
                                            Zip = col_character()))
arc_sl <- ARC_SavedLives %>% 
  mutate(data_id = "ARC-SAVEDLIVES") %>%
  select(GEOID = FIPS, everything())

##ARC PREPAREDNESS DATA, All date and time
ARC_Prep <- read_csv("ARC Preparedness Data (Phase 2).csv", 
                     col_types = cols(FIPS = col_character(), 
                                      Zip = col_character()))
arc_p <- ARC_Prep %>% 
  mutate(data_id = "ARC-PREP", 
         date_year = substr(`In-Home Visit Date`, -4, -1))

#JOIN DATA WITH BIND_ROWS
nfirs_arc <- bind_rows(arc_r, arc_sl, arc_p, nfirs)

### THIS IS WHERE I RAN OUT OF RAM :( :( :(, so, points moving forward:

# GATHER name-of-region data from tidycensus
# GET NAME OF REGION DATA
all_states <- unique(fips_codes$state)[1:51]

#To extract names of each data point, use the get_acs() function as follows 
names_of_regions <- get_acs(geography = "tract",
                                     variables = "B01003_001",
                                     state = all_states) %>% 
  select(GEOID, NAME) %>%
  separate(NAME, into = c("tract", "county", "state"), sep = ", ")

#... there's probably a much easier way to do this.

## We... did a lot of stuff by this point. You might want to write to csv HERE.
write.csv(nfirs_arc, "nfirs_arc.csv", row.names = F)


#THEN, FOR SAMPLE ANALYSIS:
## 1. GROUP BY date_year and STATE, COUNTY, TRACT
## 2. SUMMARIZE by counts (i.e., 'n()') or by sums for variables within the NFIRS data