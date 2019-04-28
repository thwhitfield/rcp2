##############################################################################
# JOIN_NFIRS_ARC_BOUNDARIES.R
#
# DataDive, spring 2019
#
#############################################################################

library(stringr)

df = read.csv("~/Documents/DKDC_RC/data/NFIRS_2009_2016_Geocodes_CensusBlock.csv",stringsAsFactors = FALSE)
df_fixed = read.csv("~/Documents/DKDC_RC/data/NFIRS_2009_2016_FIXED_Geocodes_CensusBlock.csv",stringsAsFactors = FALSE)
arc_boundary = read.csv("~/Documents/DKDC_RC/data/ARC_data/Zip to ARC Geographies (as of 7.1.2018).csv",stringsAsFactors = FALSE)
original_df = read.csv("~/Documents/DKDC_RC/data/NFIRS_2009_2016_with_IDs.csv",stringsAsFactors = FALSE)

# check that the fixed addresses are ALL in the regular file
sum(!(df_fixed$ID %in% df$ID)) #0, so all in

# remove rows with df_fixed IDs from df
df_final = df[which(!df$ID %in% df_fixed$ID),]

column_list = c('ID','COUNTYFP','TRACTCE','GEOID','NAME','NAMELSAD','MTFCC',
                'FUNCSTAT','X','Y','STATEFP','BLKGRPCE','ALAND','AWATER',
                'INTPTLAT','INTPTLON')

df_fixed = df_fixed[c(column_list,'X.full_zipcode.')]
df_fixed = df_fixed[!is.na(df_fixed$GEOID),]
colnames(df_fixed) = c(column_list,'ZIP_new')
df_fixed$ZIP_new = str_sub(df_fixed$ZIP_new,1,5)


df = df[c(column_list,'ORIGINAL_ADDRESS','MATCHED_ADDRESS')]
df = df[!is.na(df$GEOID),]

sum(!(df_fixed$ID %in% df$ID)) #should be almost 72k

df_fixed$ORIGINAL_ADDRESS = NA
df_fixed$MATCHED_ADDRESS = NA
df$ZIP_new = NA

df_final = rbind(df,df_fixed)
df_final = df_final[,names(df_final) != 'X']
#write.csv(df_final,"~/Documents/DKDC_RC/data/NFIRS_2009_2016_CensusInfo_Combined.csv",row.names = F)

original_df = original_df[,names(original_df) != 'X']

# Merge original NFIRS with geocoded stuff
df_merge = merge(original_df,df_final,by = 'ID',all.x = TRUE)
original_temp = trimws(str_split_fixed(df_merge$ORIGINAL_ADDRESS, ",", 4))
matched_temp = trimws(str_split_fixed(df_merge$MATCHED_ADDRESS, ",", 4))
df_merge$original_zip = original_temp[,4]
df_merge$matched_zip = matched_temp[,4]

# fill Zip_new column with edited or replaced zipcodes from original NFIRS data or Census Geocoder matched addresses
df_merge$matched_zipcode = as.numeric(gsub("\\D", " ", matched_temp[,4]))
df_merge$matched_zipcode = sprintf("%0.2d",df_merge$matched_zipcode)
which(nchar(df_merge$matched_zipcode) > 5)
df_merge$matched_zipcode[which(df_merge$matched_zipcode == 'NA')] = NA

#df_merge[which(is.na(df_merge$ZIP_new) & !(is.na(df_merge$matched_zipcode)))[1:10],]
idx = which(is.na(df_merge$ZIP_new) & !(is.na(df_merge$matched_zipcode)))
df_merge$ZIP_new[idx] = df_merge$matched_zipcode[idx]

which(is.na(df_merge$ZIP_new) & !is.na(df_merge$GEOID))
which(is.na(df_merge$ZIP_new) & (df_merge$GEOID == 'NA'))


idx = which(is.na(df_merge$ZIP_new) & !(is.na(df_merge$zip5)) & (nchar(gsub("\\D","",df_merge$zip5)) == 5))
df_merge$ZIP_new[idx] = df_merge$zip5[idx]
df_merge$ZIP_new = gsub("\\D","",df_merge$ZIP_new)
df_merge$ZIP_new = sprintf("%05s",df_merge$ZIP_new)
df_merge$ZIP_new[df_merge$ZIP_new == '000NA'] = NA
  
# if true, can continue since these don't have GEOIDs any way
# maybe pass these addresses to JJ for geocoding?
if ((length(which(is.na(df_merge$ZIP_new) & is.na(df_merge$GEOID)))) == (length(which(is.na(df_merge$ZIP_new))))){
  #write.csv(df_merge,"~/Documents/DKDC_RC/data/NFIRS_2009_2016_Census_FINAL.csv",row.names = F)
}

# Merge ARC Boundary
df_merge_2 = merge(df_merge,arc_boundary,by.x = 'ZIP_new',by.y = 'ZIP', all.x = TRUE, incomparables = NA)
df_merge_2 = df_merge_2[,!names(df_merge_2) %in% c('original_zip','matched_zip','matched_zipcode','ORIGINAL_ADDRESS')]
#write.csv(df_merge_2,"~/Documents/DKDC_RC/data/NFIRS_2009_2016_Census_ARCBoundary.csv",row.names = F)
