# Testing script before I fuck up GitHub code
# CH_reach = need to:
# 1.fill in missing Rms for fish/trips that need it
# 2.remove fish that have no RMs associated with it
#   Basically no "NAs" should exist
#   What to do for "NAs" that exist for antenna data?

# CH_length = need to:
# 3.for fish that multiple CH, if fish >325, fill in NAs with TL>325
# 4.for those fish with 1CH and no info = remove completely

########################################################################
#grab mechanical removal stations from Big Boy to work on fixing reach data
library(tidyverse)

#load station.csv files that have station information needed
file.names <- list.files(
  path ="C:/Users/ltennant/Desktop/FMS_mark_recap/QAQC")
FMS <- read.csv("C:/Users/ltennant/Desktop/FMS_mark_recap/flannelmouth/data/all_PIT_tagged_flannelmouth.csv") #all FMS
MRS <- read.csv("C:/Users/ltennant/Desktop/FMS_mark_recap/QAQC/BB_Mechanical_removal_stations.csv",
                   stringsAsFactors = FALSE) #mechanical removal stations
LFS <- read.csv("C:/Users/ltennant/Desktop/FMS_mark_recap/QAQC/BB_LF_Stations.csv",
                stringsAsFactors = FALSE) #Lee's Ferry stations

View(FMS)
View(MRS)
#####################################################################
# work on correcting reach information first

FMS <- FMS %>%
  mutate(START_RM = case_when(TRIP_ID == "GC20090430" & is.na(START_RM) ~ 108.6,



# ------------------------------------------------------#
colnames(MRS)[2] <- "MRS_START_RM"

#remove columns not applicable
MRS <- MRS %>%
  select (-c(END_RM,DB_LINK_ID,STATION_TYPE,RIVER_CODE,
             SIDE_CODE,START_RKM,END_RKM,STATION_ID))

FMS <- FMS %>%
  bind_rows(MRS)



