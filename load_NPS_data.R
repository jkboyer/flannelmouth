#load NPS flannelmouth recapture data
#incorporate into load file for big boy data later

#need to ask Brian
#location info - what do station IDs mean, for fish without RM
#are datetimes available, or only dates?
#on shinumo trips, were hoop nets in mainstem or shinumo, and how do I tell?
#BAC river mile numbering in station_id - where is it from?
#                          what are negative RMs?
#data we still need: anything location related, disposition code

require(tidyverse)
fms <- read.csv("./data/all_PIT_tagged_flannelmouth.csv", stringsAsFactors = FALSE)
colnames(fms)

#replace with filepath for work
nps.filepath <- "C:/Users/Jan Boyer/Documents/"
nps.filename <- "NPS_FMS_data_forCharles03March2020.csv"

#load NPS flannelmouth PIT tag data
nps <- read.csv(paste0(nps.filepath, nps.filename), stringsAsFactors = FALSE)
glimpse(nps)
colnames(nps)

#Fix location issues
#rules: All COR captures on shinumo trips between rM 108 and 109.2
#river mile may be in station notes or sample id

#what columns will get deleted?
colnames(nps)[(colnames(nps) %in% colnames(fms)) == FALSE]
colnames(nps)[colnames(nps) %in% colnames(fms)]

#what columns are missing from NPS data?
colnames(fms)[colnames(fms) %in% colnames(nps) == FALSE]

nps <- nps %>% #format dates
  mutate(START_DATE = as.Date(START_DATE, format = "%m/%d/%Y"),
         #convert to start datetime to match big boy data
         START_DATETIME = as.POSIXct(paste(START_DATE, "12:00:00"))) %>%
  select(c(colnames(nps)[colnames(nps) %in% colnames(fms)], STATION_ID)) %>%
  filter(PITTAG != "") #remove fish that are not PIT tagged
glimpse(nps)


