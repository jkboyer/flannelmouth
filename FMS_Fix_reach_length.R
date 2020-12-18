# Testing script before I fuck up GitHub code
# # CH_reach = need to:
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
fms <- read.csv("./data/all_PIT_tagged_flannelmouth.csv") #all FMS ############ Need to update - only has 30K tags, needs 90K!!!!!!!!!!!!!!!!!!!!!!!!!
mrs <- read.csv("./data/BB_Mechanical_removal_stations.csv",
                stringsAsFactors = FALSE) #mechanical removal stations
lfs <- read.csv("./data/BB_LF_Stations.csv",
                stringsAsFactors = FALSE) #Lee's Ferry stations

View(fms)
View(mrs)
View(lfs)
#####################################################################
#Correcting for missing reach information
#merge station data based on unpadded station id
#then fill in mile if mile is missing

#column names need to match (for column we are joining on)
colnames(fms)

#subset to just data missing mile to see missing data
fms.missing <- fms %>%
  filter(is.na(START_RM) & RIVER_CODE == "COR")

#when joining, subset table to be joined to only necessary columns
mrs <- mrs %>% #need to associate mechanical removal station IDs to get RM
  #column names must match for joining column
  transmute(STATION_ID = UNPADDED_STATION_ID,
            #and not match for other columns
            mile = START_RM)

#join data from mechanical removal station table on to fms
fms <- fms %>%
  left_join(mrs) #left join keeps all FMS columns, only MRS cols that match

# if else statement to add mile from station only if start_RM is missing
fms$START_RM <- ifelse(!is.na(fms$START_RM), fms$START_RM, fms$mile)

# join data from the Lee's Ferry station table on to fms
lfs <- lfs %>% #need to associate Lee's Ferry station IDs to get RM
  transmute(STATION_ID = UNPADDED_STATION_ID,
            mile2 = DOWN_RM)

fms <- fms %>%
  left_join(lfs)

# if else statement to add mile from station only if start_RM is missing
fms$START_RM <- ifelse(!is.na(fms$START_RM), fms$START_RM, fms$mile2)

# remove mile columns from fms
fms <- fms %>%
  select(-mile, -mile2)

# what else is missing for RMs and RKMs (inlcudes LCR, HAV, etc.)
fms.missing <- fms %>%
  filter(is.na(START_RM) & is.na(START_RKM))

# remove all RM = NA for where we couldn't find the RM information for COR (only 27 records) -
# did it for RKMs for tribs as well (18 records) = 45 records
fms <- fms %>%
  filter(!is.na(START_RM) | !is.na(START_RKM))

# double checking all records were removed
fms.missing <- fms %>%
  filter(is.na(START_RM) & is.na(START_RKM))

# other weird shit happening with tribs
trib <- fms %>%
  filter(RIVER_CODE !="COR" & !is.na(START_RM))
view(trib)

#LCR only trib with weird RMs; remove records where LCR has START_RM weirdness (n=20)




# ------------------------------------------------------#


















