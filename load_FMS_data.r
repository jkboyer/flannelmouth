#add code to extract effort info from antennas (if not present in sites data)
#LOAD BRIAN EFFORT DATA

#loads data
#1. all PIT tagged FMS records in GCMRC database for mark-recapture analysis
#   and NPS FMS records that are not in big boy
#2. all flannelmouth to look at size structure, maturity, etc.
#Authors: Jan Boyer, AGFD, jboyer@azgfd.gov
#         Laura Tennant, USGS, ltennant@usgs.gov
#Inputs: most recent version of big boy (access database)
#Outputs: "all_PIT_tagged_flannelmouth.csv"
#         "all_flannelmouth.csv"
#Dependencies:
#Note: MUST USE 32-bit R (and version 1.1.x or older of Rstudio, newer versions
#      of Rstudio only run 64-bit R) to use RODBC to connect to access database
#      in Rstudio Tools/Global Options/General, click change button by R version

#setup: load packages, data, define subsetting criteria ######
library(RODBC) #database interface
library(tidyverse)
library(lubridate) #need this for date transformation

theme_set(theme_minimal()) #override ugly default ggplot theme

#load some metric/english conversion fuctions I wrote
source("./functions/conversion_functions.r")

#define cutoffs to use for subsetting data
start.year <- 2004
#season cutoffs - %j is day of year and will be same day regardless of year
start.spring <- strftime(as.POSIXct("2020-03-01"), format = "%j")
end.spring <- strftime(as.POSIXct("2020-06-20"), format = "%j")
start.fall <- strftime(as.POSIXct("2020-08-01"), format = "%j")
end.fall <- strftime(as.POSIXct("2020-10-31"), format = "%j")

#gear types to keep
#these are aggregated (e.g. baited hoop net includes HB, MHB, and more, see
#                      gear_types.csv for grouping)
gear.types.keep <- c("boat_electrofishing", "baited_hoop_net",
                     "unbaited_hoop_net", "antenna_temporary")

#length of reach used to bin data for cauchy distribution
reach.km = 8

# load data from big boy database ######
# database file name: UPDATE to most recent version here
db.GCMRC <- "FISH_SAMPLE_SPECIMEN_HISTORY_20200318_1541.mdb"

# specify file location of GCMRC database
#Laura's working file path
gcmrc.file.path <- "M:/Lovich/Laura Tennant work folder/GCMRC/FMS_mark_recap/"
#Jan's work file path
#gcmrc.file.path <- "\\\\flag-server/Office/GCMRC_master_database/"
#Jan's coronavirus work from home filepath
gcmrc.file.path <- "C:/Users/jboyer/Documents/big_boy/"

#connect to database
db <- odbcConnectAccess(paste(gcmrc.file.path, db.GCMRC, sep = ""))

#see tables and column names
sqlTables(db)
sqlColumns(db, "SAMPLE_SPECIMEN_ALL")$COLUMN_NAME

# query desired FMS data from specimen table
# this will take a while to run
fms <- sqlQuery(db,
              paste("SELECT SAMPLE_TYPE, FISH_T_SAMPLE.TRIP_ID, GEAR_CODE,",
                        "FISH_T_SAMPLE.START_DATETIME, RIVER_CODE, START_RM,",
                        "END_RM, START_RKM, END_RKM, GPS_START_WAYPOINT,",
                        "GPS_END_WAYPOINT, GIS_X, GIS_Y, LAT, LON,",
                        "SPECIES_CODE, TOTAL_LENGTH, FORK_LENGTH,",
                        "WEIGHT, PITTAG, PITTAG_RECAP, PITTAG2, PITTAG2_RECAP,",
                        "PITTAG3, PITTAG3_RECAP, DISPOSITION_CODE,",
                        "SEX_CODE, SEX_COND_CODE, SEX_CHAR_CODE",
                    "FROM SAMPLE_SPECIMEN_ALL",
                    "WHERE SPECIES_CODE = 'FMS'"),
              stringsAsFactors = FALSE, #import strings as character, not factor
              na.strings = c(NA, "", " ", 999, -999, "#N/A")) #these values are all NA

glimpse(fms) #overview of dataframe imported from access database

# query desired antenna data from specimen table
# this will take a while to run
antenna <- sqlQuery(db,
                    paste("SELECT FISH_T_SAMPLE.SAMPLE_ID,",
                          "SAMPLE_TYPE, FISH_T_SAMPLE.TRIP_ID, GEAR_CODE,",
                          "RIVER_CODE, FISH_T_SAMPLE.START_DATETIME,",
                          "END_DATETIME, START_RKM, START_RM,",
                          "PITTAG",
                          "FROM SAMPLE_SPECIMEN_ALL",
                          "WHERE GEAR_CODE = 'CUPS_BAITED'
                          OR GEAR_CODE = 'BK'
                          OR GEAR_CODE = 'BK_BAITED'
                          OR GEAR_CODE = 'BK_UNBAITED'
                          OR GEAR_CODE = 'FS_B_24_HR'
                          OR GEAR_CODE = 'MUX2009'
                          OR GEAR_CODE = 'MUX2011'
                          OR GEAR_CODE = 'MUX2012'
                          OR GEAR_CODE = 'MUX2013'"),
                    stringsAsFactors = FALSE, #import strings as character, not factor
                    na.strings = c(NA, "", " ", 999, -999, "#N/A")) #these values are all NA

glimpse(antenna) #overview of dataframe imported from access database

#get sample data too - will need to calculate effort days for each gear
sqlColumns(db, "FISH_T_SAMPLE")$COLUMN_NAME

#see what trips we need effort data for
trip.ids <- unique(c(unique(fms$TRIP_ID), unique(antenna$TRIP_ID)))
trip.ids
#reformat trip ids as a string with quotes and commas to put in SQL query
trip.list <- paste("'", as.character(trip.ids),"'",collapse=", ",sep="")

#query all sample data from those trips from big boy
samples <- sqlQuery(db,
                    paste("SELECT TRIP_ID, SAMPLE_TYPE, GEAR_CODE, RIVER_CODE,",
                          "START_RM, END_RM,",
                          "START_DATETIME, END_DATETIME,",
                          "EF_TOTAL_SECONDS, SAMPLE_ID",
                          "FROM FISH_T_SAMPLE",
                          "WHERE TRIP_ID IN (", trip.list, ")"),
                    stringsAsFactors = FALSE, #import strings as character, not factor
                    na.strings = c(NA, "", " ", 999, -999, "#N/A")) #these values are all NA

odbcClose(db) #close database connection

# load additional data from NPS and FWS ######
# capture data mostly from bright angel and shinumo (mainstem sampling) trips

#load NPS flannelmouth data
#nps.filepath <- "\\\\FLAG-SERVER/Office/Grand Canyon Downstream/Databases/NPS_data/"
nps.filepath <- "C:/Users/jboyer/Documents/data/"
nps.filename <- "NPS_FMS_data_captures_forJan17March2020.csv"

#load NPS flannelmouth PIT tag data
nps <- read.csv(paste0(nps.filepath, nps.filename), stringsAsFactors = FALSE)
glimpse(nps)

nps <- nps %>% #format dates
  mutate(START_DATE = as.Date(START_DATE, format = "%m/%d/%Y"),
         #convert to start datetime to match big boy data
         START_DATETIME = as.POSIXct(paste(START_DATE, "12:00:00"))) %>%
  select(c(colnames(nps)[colnames(nps) %in% colnames(fms)], STATION_ID)) %>%
  filter(PITTAG != "") #remove fish that are not PIT tagged
glimpse(nps)

#Fix location issues (missing RMs
#rules: All COR captures on shinumo trips between rM 108 and 109.2
#stations labeled -1 in tribs are close to mouth
# (below weir and within 200m of COR at BAC, below first barrier at HAV)
#positive number station ids (1-5, 1-3) go from downstream to upstream
#if station id is SHI, it should be station ID 1
#assign RM to locations lacking that
nps %>%
  filter(is.na(START_RM)) %>%
  group_by(RIVER_CODE, STATION_ID) %>%
  summarize(n = n()) %>%
  print(n = Inf)

nps <- nps %>%
  #if no RM on COR sites from SHI trips, assign 108.6
  #Brian said all captures between RM108-109.2, 108.6 is middle
  mutate(START_RM = case_when(RIVER_CODE == "COR" & is.na(START_RM) ~ 108.6,
                              RIVER_CODE == "BAC" & STATION_ID == -1 ~ 88.3,
                              RIVER_CODE == "HAV" ~ 157.3,
                              # I think all SHI captures are below waterfall
                              # but check with Brian
                              RIVER_CODE == "SHI" ~ 109.3,
                              TRUE ~ START_RM),
         #for all trib fish near mouth, recode river to Colorado
         RIVER_CODE = case_when(RIVER_CODE == "BAC" & STATION_ID == -1 ~ "COR",
                                RIVER_CODE == "HAV" ~ "COR",
                                # I think all SHI captures are below waterfall
                                # but check with Brian
                                RIVER_CODE == "SHI" ~ "COR",
                                TRUE ~ RIVER_CODE)) %>%
  select(-STATION_ID) #no longer needed, remove

# join NPS data to big boy data
fms <- fms %>%
  bind_rows(nps)

rm(nps) #no longer needed, remove

#NPS data is from BAC antenna, and shinumo area mainstem hoopnetting trips
#FWS data is submersible antenna data, will eventually be in big boy, but is
#not yet in big boy

#NPS antenna data
#Adding NPS antenna BAC data in (Laura's file path)
NPS.ant <- read.csv("C:/Users/ltennant/Desktop/FMS_mark_recap/FMS_NPScaptures_on_BAC_antenna.csv")

#Jan's filepaths
NPSdataantenna <- "FMS_NPScaptures_on_BAC_antenna.csv"
NPS.ant <- read.csv(paste0(nps.filepath, NPSdataantenna),
                    stringsAsFactors = FALSE)

NPS.ant <- NPS.ant %>% #format date as date
  mutate(detected_at = as.Date(detected_at, format = "%m/%d/%Y"))

#subset to only antenna columns (others were initial capture data joined on)
NPS.ant <- NPS.ant %>%
  transmute(PITTAG = tag,
            START_DATETIME = as.POSIXct(detected_at))

#Add year to "year" column, PITTAG_RECAP = "Y" column, GEAR_CODE = "NPS_BIOMARK_ANTENNA"
#Brian Healy said the antenna detections of FMS are ONLY at the the BAC antenna
#which is about 200 m upstream of the Colorado River in BAC, just below the lower campground bridge.
#I am reassigning RIVER_CODE = "BAC" and as a mainstem antenna (START_RM = 88.3).
NPS.ant <- NPS.ant %>%
  mutate(year = as.numeric(substr(as.character(START_DATETIME), 1, 4)))

#add columns that big boy antenna data has and NPS data needs
#these are the same for all NPS antenna data
NPS.ant["PITTAG_RECAP"] <- "Y"
NPS.ant["GEAR_CODE"] <- "NPS_BIOMARK_ANTENNA"
NPS.ant["RIVER_CODE"] <- "COR"
NPS.ant["START_RM"] <- 88.3
NPS.ant["TRIP_ID"] <- "BACantenna"

#Clean up tags so there is only one unique PIT tag detection/day instead of multiples.
NPS.ant <- NPS.ant %>%
  arrange(PITTAG, START_DATETIME) %>% #order by pittag and date
  distinct() #keep only unique rows
#since only PITTAG and START_DATETIME vary in this dataset, distinct() will subset
#our data to one record per fish per day
#SIDE NOTE: we'd need to modify code if we had datetimes instead of dates

#join NPS antenna data to big boy antenna data
antenna <- antenna %>%
  bind_rows(NPS.ant)

rm(NPS.ant) #no longer needed, remove

#FWS antenna data
#NOTE ABOUT THIS DATASET: FWS cleans up the data already so there's only one unique PITTAG/day
#so we don't need to do that in our code. There are two antenna types: submersible and
#shore-based. Shore Based antennas were assigned to SAMPLE_TYPE 127 and submersible antennas
#to SAMPLE_TYPE 128.GEAR_CODE is assigned as BK_BAITED or BK_UNBAITED. BK_UNBAITED
#is true when SAMPLE_TYPE = 128 (when FWS forgot to bait antennas; n = 12 cases) OR
#is a permanent shore-based antenna.

#Adding FWS antenna data in (Laura's file path)
#FWS.ant <- read.csv("C:/Users/ltennant/Desktop/FMS_mark_recap/Antennas_COR_USFWS_update.csv",
 #                   stringsAsFactors = FALSE,
 #                   header = TRUE)
FWS.ant <- read.csv("./data/Antennas_COR_USFWS_update.csv",
                   stringsAsFactors = FALSE, header = TRUE)
#filter FMS only
#FWS.ant <- FWS.ant %>%
 # filter(Species == "FMS")

#format date and time
FWS.ant$newSTART_DATETIME <- gsub("/","-", FWS.ant$Datetime)
FWS.ant$formattedSTART_DATETIME <- parse_date_time(FWS.ant$newSTART_DATETIME, orders="mdy_H!M!")

#rename columns
colnames(FWS.ant)[4] <- "PITTAG"
colnames(FWS.ant)[6] <- "RIVER_CODE"
colnames(FWS.ant)[7] <- "START_RM"
colnames(FWS.ant)[10] <- "Antenna_ID"
colnames(FWS.ant)[16] <- "START_DATETIME"

#add columns (year and PITTAG_RECAP added in later code)
FWS.ant <- FWS.ant %>%
  mutate(year = substr(as.character(START_DATETIME), 1, 4))
FWS.ant["PITTAG_RECAP"] <- "Y"
FWS.ant["SAMPLE_TYPE"] <- "128"
FWS.ant["GEAR_CODE"] <- "BK_BAITED"

#format data to be numeric
FWS.ant$START_RM <- as.numeric(FWS.ant$START_RM)
FWS.ant$year <- as.numeric(FWS.ant$year)
FWS.ant$SAMPLE_TYPE <- as.numeric(FWS.ant$SAMPLE_TYPE)

#Havasu needs a START_RM
FWS.ant <- FWS.ant %>%
  mutate(START_RM = case_when(RIVER_CODE == "HAV" ~ 157.3,
                              TRUE ~ START_RM))
#two types of antennas: shore-based antennas need to be different sample type
FWS.ant <- FWS.ant %>%
  mutate(SAMPLE_TYPE = case_when(Antenna.Type == "Shore Based" ~ 127,
                                 TRUE ~ SAMPLE_TYPE))

#Designate gear type based on if baited antennas or not
FWS.ant <- FWS.ant %>%
  mutate(GEAR_CODE = case_when(Baited == "N" ~ "BK_UNBAITED",
                               TRUE ~ GEAR_CODE))

#remove columns
FWS.ant <- FWS.ant %>%
  select (-c(Date, Time, Datetime, Species, Side,
             X, X.1, newSTART_DATETIME, Baited, Antenna.Type))

#DOES NOT HAVE SAMPLE_id
#create one from trip id+ antenna ID
FWS.ant <- FWS.ant %>%
  mutate(SAMPLE_ID = paste(TRIP_ID, Antenna_ID, START_RM, sep = "_"))

#how many antennas per trip - do numbers make sense?
FWS.ant %>%
  group_by(TRIP_ID, SAMPLE_TYPE) %>%
  summarise(n = n())

#checking that SAMPLE_TYPE and GEAR_CODE parsed correctly
FWS.ant %>%
  group_by(GEAR_CODE, SAMPLE_TYPE) %>%
  summarize(n = n()) %>%
  arrange(-n)
FWS.ant <- FWS.ant %>%
  mutate(year = as.numeric(substr(as.character(START_DATETIME), 1, 4)))
glimpse(FWS.ant)
glimpse(antenna)
#join FWS antenna data to big boy and NPS antenna data
antenna <- antenna %>%
  mutate(SAMPLE_ID = as.character(SAMPLE_ID)) %>%
  bind_rows(FWS.ant)

rm(FWS.ant) #no longer needed, remove

# Fix various errors in data ######
fms <- fms %>% #don't need species column since they are all flannelmouth
  select(-SPECIES_CODE) #remove column

# people used 999 as an NA value!?
fms$START_RM <- ifelse(fms$START_RM == 999.00, NA, fms$START_RM)

fms$TOTAL_LENGTH <- ifelse(fms$TOTAL_LENGTH <= 1, NA, fms$TOTAL_LENGTH)

fms$PITTAG <- ifelse(fms$PITTAG %in% c("N", "SCANNER KAPUT"), NA, fms$PITTAG)

fms <- fms %>%
  mutate(length.tag = nchar(PITTAG), #count length of pit tag code
         PITTAG = toupper(PITTAG)) #capitalize pittag

fms %>%
  group_by(length.tag) %>%
  summarize(n = n())

#Incorrect PIT tag codes - FIX THEM
# . was omitted in some 3DD.003... entries (recorded as 3dd003...)
#for lengths of 13, if 4th character is not . add .
fms <- fms %>%
  mutate(PITTAG = case_when(nchar(PITTAG) == 13 & #one less than normal 14
                              #and there is no . in PITTAG
                              str_detect(PITTAG, "\\.") == FALSE ~
                              paste0(substr(PITTAG,1,3), ".", substr(PITTAG,4,13)),
                            TRUE ~ PITTAG))

fms <- fms %>%
  mutate(length.tag = nchar(PITTAG))

fms %>%
  group_by(length.tag) %>%
  summarize(n = n())
#tags with 11 digits are old tags
#tags with 14 digits are new tags
#others are errors or missing digits - low enough numbers, just drop them.

# get year from datetime
fms <- fms %>%
  mutate(year = substr(as.character(START_DATETIME), 1, 4))

# If missing total length, calculate from fork length, and vice versa #####
# calculate coefficients
fms.lengths <- fms %>% #subset to fish with both lengths
  filter(!is.na(TOTAL_LENGTH) & !is.na(FORK_LENGTH))

#look at length data: TL vs. FL
fms.lengths %>%
  ggplot(aes(x = TOTAL_LENGTH, y = FORK_LENGTH)) +
  geom_point(alpha = 0.1) +
  stat_smooth(method = "lm")
#apparently there are some measurement errors

#fit linear models
#predict fork length from total length
lm.TL.to.FL <- lm(FORK_LENGTH ~ TOTAL_LENGTH, data = fms.lengths)
summary(lm.TL.to.FL)
lm.FL.to.TL <- lm(TOTAL_LENGTH ~ FORK_LENGTH, data = fms.lengths)
summary(lm.FL.to.TL)

#extract coefficients
TLtoFL.intercept <- summary(lm.TL.to.FL)$coef["(Intercept)", "Estimate"]
TLtoFL.slope <- summary(lm.TL.to.FL)$coef["TOTAL_LENGTH", "Estimate"]
FLtoTL.intercept <- summary(lm.FL.to.TL)$coef["(Intercept)", "Estimate"]
FLtoTL.slope <- summary(lm.FL.to.TL)$coef["FORK_LENGTH", "Estimate"]

#calculate predicted fork length
fms.lengths <- fms.lengths %>%
  mutate(predicted.TL = FLtoTL.intercept + FLtoTL.slope*FORK_LENGTH,
         TL.difference = abs(TOTAL_LENGTH - predicted.TL),
         predicted.FL = TLtoFL.intercept + TLtoFL.slope*TOTAL_LENGTH,
         FL.difference = abs(FORK_LENGTH - predicted.FL))

#remove obviously incorrect values
#use graph below and adjust values until it seems measurement errors or species
#code errors have been removed

fms.lengths <- fms.lengths %>%
  filter(TL.difference < 25 & FL.difference < 25)

#graph, see if I picked the right value above to exclude measurement/species
#errors but keep all real flannelmouths
fms.lengths %>%
  ggplot(aes(x = TOTAL_LENGTH, y = FORK_LENGTH, color = FL.difference)) +
  geom_point(size = 0.75) +
  scale_color_viridis_c()

#recalculate coeeficients with subsetted data
#fit linear models
#predict fork length from total lenght
lm.TL.to.FL <- lm(FORK_LENGTH ~ TOTAL_LENGTH, data = fms.lengths)
summary(lm.TL.to.FL)
lm.FL.to.TL <- lm(TOTAL_LENGTH ~ FORK_LENGTH, data = fms.lengths)
summary(lm.FL.to.TL)

#extract coefficients
TLtoFL.intercept <- summary(lm.TL.to.FL)$coef["(Intercept)", "Estimate"]
TLtoFL.slope <- summary(lm.TL.to.FL)$coef["TOTAL_LENGTH", "Estimate"]
FLtoTL.intercept <- summary(lm.FL.to.TL)$coef["(Intercept)", "Estimate"]
FLtoTL.slope <- summary(lm.FL.to.TL)$coef["FORK_LENGTH", "Estimate"]

#calculate predicted lengths on all data
fms <- fms %>%
  mutate(FORK_LENGTH = as.numeric(FORK_LENGTH),
         TOTAL_LENGTH = as.numeric(TOTAL_LENGTH)) %>%
  #if length is missing, calculate from other length, if length exists keep it
  mutate(TL = case_when(is.na(TOTAL_LENGTH) ~
                          FLtoTL.intercept + FLtoTL.slope*FORK_LENGTH,
                        TRUE ~ TOTAL_LENGTH),
         FL = case_when(is.na(FORK_LENGTH) ~
                          TLtoFL.intercept + TLtoFL.slope*TOTAL_LENGTH,
                        TRUE ~ FORK_LENGTH))

#save all FMS (including not tagged) to examine size structure, maturity
write.csv(fms, "./data/all_flannelmouth.csv", row.names = FALSE)

#subset to only PIT tagged fish captured since start year (2004) ######
fms <- fms %>%
  filter(!is.na(PITTAG) & #remove non tagged fish
          length.tag == 14) %>% #new tags have 14 digits
  filter(year >= start.year) %>%
  select(-c(SEX_CODE, SEX_COND_CODE, SEX_CHAR_CODE, length.tag,
            PITTAG2, PITTAG2_RECAP, PITTAG3, PITTAG3_RECAP))

#also subset the sample dataframe
samples <- samples  %>%
  filter(START_DATETIME >= as.POSIXct(paste0(start.year, "-01-01 00:0:01")))

#don't need to subset antennas by year, they were not used until 2009

# fish captured in mouths of tributaries - recode as mainstem fish #####
#see what fish are in tributaries
tribs <- fms %>%
  filter(RIVER_CODE != "COR")

# some fish have tributary river codes, but a mainstem river mile recorded
# (e.g. HAV 157.26, SHI 108.6), and a low (<0.2) or no kilometer record
# these fish can be reclassified as COR (mainstem) fish
trib.cutoff <- 0.2 #how many miles up a tributary we consider mainstem
#if start km < 0.2*1.609, or if start km is missing and start RM < 0.2,
#reclassify as COR and assign start RM based on tributary

unique(fms$RIVER_CODE) # see what river codes there are

#dataframe of confluence locations in miles
confluences <- data.frame(RIVER_CODE = c("COR", "HAV", "LCR", "SHI"),
                          confluence_RM = c(NA, 157.3, 61.8,  109.3))

fms <- fms %>% #join confluence miles to fms data
  left_join(confluences)

# replace tributary code (LCR, HAV, SHI) with COR, and trib RM with confluence
#RM for fish caught near mouth of tributary
fms <- fms %>%
  mutate(START_RM = case_when((RIVER_CODE != "COR" &
            #trib rkm is very close to confluence
            (START_RKM <= trib.cutoff*1.609 |
               (is.na(START_RKM) & START_RM <= trib.cutoff))) ~ confluence_RM,
             TRUE ~ START_RM),
    RIVER_CODE = case_when( #reclassify as COR for tributary captures if:
      #rkm or rm is < 0.2 miles (close to confluence)
      RIVER_CODE != "COR" & (START_RKM <= trib.cutoff*1.609 |
      (is.na(START_RKM) & START_RM <= trib.cutoff) |
      #or river mile matches the appropriate tributary confluence
      (RIVER_CODE == "LCR" & START_RM >= 61.3 & START_RM <= 61.4) |
      (RIVER_CODE == "SHI" & START_RM >= 108.6 & START_RM <= 109.3) |
      (RIVER_CODE == "HAV" & START_RM >= 156.7 & START_RM <= 157.3)) ~ "COR",
    #otherwise, keep river code unchanged
    TRUE ~ RIVER_CODE))

#all the 2017 agg trip shinumo nets were below waterfall,
#and the Havasu nets were right in mouth - recode as COR
fms <- fms %>%
  mutate(START_RM = case_when(RIVER_CODE %in% c("HAV", "SHI") & TRIP_ID == "GC20170819" ~ confluence_RM,
                              TRUE ~ START_RM),
         RIVER_CODE = case_when(RIVER_CODE %in% c("HAV", "SHI") & TRIP_ID == "GC20170819" ~ "COR",
                               TRUE ~ RIVER_CODE)) %>%
  select(-confluence_RM) #no longer needed, remove column

#Questions still remaining:
#   in LCR, what is mile 197?
#   lots of zeros (especially in Shinumo). Confluence, or missing data?
#   should I give each trib a confluence river mile so we have that location
#       for modelling movement?

# Have filters successfully removed mainstem/confluence fish from tribs?
tribs <- fms %>%
  filter(RIVER_CODE != "COR")

#counts of fms in each tributary
tribs %>%
  group_by(RIVER_CODE) %>%
  summarize(n = n()) %>%
  arrange(-n)
#now only LCR has actual tributary fish
#makes sense, most FMS from SHI and HAV are right in mouth

rm(confluences, fms.lengths, tribs, lm.FL.to.TL, lm.TL.to.FL) # no longer needed, remove

# calculate sampling effort ############

#data has T on end of trip code cause was uploaded separately -
#but is same trip, so remove T
samples <- samples %>%
  mutate(TRIP_ID = str_remove(TRIP_ID, "[[:upper:]]$"))
fms <- fms %>%
  mutate(TRIP_ID = str_remove(TRIP_ID, "[[:upper:]]$"))
antenna <- antenna %>%
  mutate(TRIP_ID = str_remove(TRIP_ID, "[[:upper:]]$"))

#check that it worked
unique(samples$TRIP_ID)
unique(fms$TRIP_ID)
unique(antenna$TRIP_ID)

# perform final subsetting of data #####
#based on year, season, and gear type (see top of script for exact criteria)
#load gear type table
gear <- read.csv("./data/gear_types.csv", stringsAsFactors = FALSE)

gear <- gear %>%
  select(-n)
#join generalized gear type to fish and sample data

fms <- fms %>%
  left_join(gear) %>%
#one gear type is confusingly permanent and temporary antennas
#BK_UNBAITED is permanent if 141 and 127. Otherwise temporary
  mutate(gear = case_when(GEAR_CODE == "BK_UNBAITED" &
                            SAMPLE_TYPE %in% c(127, 141) ~ "antenna_permanent",
                          TRUE ~ gear)) %>%
  mutate(day = strftime(START_DATETIME, format = "%j")) %>%
  filter(gear %in% gear.types.keep) %>% #subset to gear types to analyze
  filter((day >= start.spring & day <= end.spring) | #spring or fall only
           (day >= start.fall & day <= end.fall)) %>%
  #add variable for season
  mutate(season = case_when(day >= start.spring & day <= end.spring ~ "spring",
                            day >= start.fall & day <= end.fall ~ "fall"))

antenna <- antenna %>%
  left_join(gear) %>%
  mutate(gear = case_when(GEAR_CODE == "BK_UNBAITED" &
                            SAMPLE_TYPE %in% c(127, 141) ~ "antenna_permanent",
                          TRUE ~ gear)) %>%
  mutate(day = strftime(START_DATETIME, format = "%j")) %>%
  filter(gear %in% gear.types.keep) %>% #gear type - temporary antennas only
  filter((day >= start.spring & day <= end.spring) | #spring or fall only
           (day >= start.fall & day <= end.fall)) %>%
  #add variable for season
  mutate(season = case_when(day >= start.spring & day <= end.spring ~ "spring",
                            day >= start.fall & day <= end.fall ~ "fall"))

samples <- samples %>%
  left_join(gear) %>%
  mutate(day = strftime(START_DATETIME, format = "%j")) %>%
  mutate(gear = case_when(GEAR_CODE == "BK_UNBAITED" &
                            SAMPLE_TYPE %in% c(127, 141) ~ "antenna_permanent",
                          TRUE ~ gear)) %>%
  filter(gear %in% gear.types.keep) %>% #filter by gear type
  filter((day >= start.spring & day <= end.spring) | #spring or fall only
           (day >= start.fall & day <= end.fall)) %>%
  #add variable for season
  mutate(season = case_when(day >= start.spring & day <= end.spring ~ "spring",
                            day >= start.fall & day <= end.fall ~ "fall"))

#for each trip, bin into 5 miles (counting from dam) ######
#define length of reach to bin samples in to

#convert river mile to kilometer
samples <- samples %>% #calculate km from mile, or insert NA if tributary
  mutate(start_rkm = case_when(RIVER_CODE == "COR" ~ MileToKmCOR(START_RM)),
         reach = cut(start_rkm, seq(0, 504, by = reach.km)), #bin into reaches
         reach_no = as.numeric(reach), #number each reach
         reach_start = (reach_no - 1)*reach.km, #start point
         reach_mid = reach_start + reach.km/2, #mid point
         reach_end = reach_no*reach.km) %>% #end point
  arrange(reach)

samples %>% #plot to see spatial distribution of samples
  ggplot(aes(x = reach_mid)) +
  geom_histogram()
#yep, theres a lot of sampling at the LCR

fms <- fms %>%#calculate km from mile, or insert NA if tributary
  mutate(start_rkm = case_when(RIVER_CODE == "COR" ~ MileToKmCOR(START_RM)),
         reach = cut(start_rkm, seq(0, 504, by = reach.km)), #bin into reaches
         reach_no = as.numeric(reach), #number each reach
         reach_start = (reach_no - 1)*reach.km, #start point
         reach_mid = reach_start + reach.km/2, #mid point
         reach_end = reach_no*reach.km)

antenna <- antenna %>%#calculate km from mile, or insert NA if tributary
  mutate(start_rkm = case_when(RIVER_CODE == "COR" ~ MileToKmCOR(START_RM)),
         reach = cut(start_rkm, seq(0, 504, by = reach.km)), #bin into reaches
         reach_no = as.numeric(reach), #number each reach
         reach_start = (reach_no - 1)*reach.km, #start point
         reach_mid = reach_start + reach.km/2, #mid point
         reach_end = reach_no*reach.km)
# calculate days of effort for antennas ########
#trip id, reach_start, year + season

#MAY NEED TO ADJUST NAMES TO MERGE TO MAIN EFFORT TABLE
#antenna effort by trip, season, 8km reach, year
antenna.effort <- antenna %>%
  filter(gear == "antenna_temporary") %>%
  group_by(TRIP_ID, reach_start, year, season) %>%
  summarize(antenna.effort.calc = length(unique(SAMPLE_ID)))

#antenna effort by trip, season, year (NOT split by reach)
antenna.effort.trip <- antenna %>%
  filter(gear == "antenna_temporary") %>%
  group_by(TRIP_ID, year, season) %>%
  summarize(antenna.effort.calc = length(unique(SAMPLE_ID)))

# finalize subsetting ########
#antennas - only keep fish also in fms dataframe
#this will remove other species, FMS tagged before 2004 or in wrong season, etc.
antenna <- antenna %>%
  filter(PITTAG %in% unique(fms$PITTAG)) #only keep tags that match tag in fms

#samples - only keep data from trips we are using data from
#this will remove trips in wrong season
samples <- samples %>%
#keep only trips that we are using FMS data from
  filter(TRIP_ID %in% c(unique(fms$TRIP_ID), unique(antenna$TRIP_ID)))

#merge antenna data onto fms data ########
fms <- fms %>%
  mutate(year = as.numeric(year)) %>%
  bind_rows(antenna)

rm(antenna) #no longer need separate file, now it is on fish data

#antennas calculated from sample and antennas calculated from fms
#ccheck match, replace 0s from sample with fms

#calculate effort per 5 miles per time block for each gear type #######

samples <- samples %>%
  mutate(year = as.numeric(substr(START_DATETIME, 1, 4)))

#effort by trip, year, season, 8km reach
n.samples <- samples %>%
  group_by(TRIP_ID, gear, season, year, reach_start) %>%
  summarize(n = n())

n.samples <- n.samples %>%
  pivot_wider(names_from = gear, values_from = n,
              values_fill = list(n = 0)) #fill with zero if is NA

#effort by trip, year, season (no reach grouping)
n.samples.trip <- samples %>%
  group_by(TRIP_ID, gear, season, year) %>%
  summarize(n = n())

n.samples.trip <- n.samples.trip %>%
  pivot_wider(names_from = gear, values_from = n,
              values_fill = list(n = 0)) #fill with zero if is NA

#add on calculated anntenna effort if antenna effort is missing
n.samples <- n.samples %>%
  left_join(antenna.effort)

n.samples.trip <- n.samples.trip %>%
  left_join(antenna.effort.trip)

#check that there is no overlap (number in antenna temporary or n, not both)
#move n (calculated antenna effort) into antenna temporary colulmn
n.samples <- n.samples %>%
  mutate(antenna_temporary = case_when( #if no value for antenna temporary and
    #there is a calculated value, replace with calculated value
    antenna_temporary == 0 & !is.na(antenna.effort.calc) ~ antenna.effort.calc,
    #otherwise, keep antenna_temporary effort count
    TRUE ~ antenna_temporary)) %>%
  select(-antenna.effort.calc) #no longer needed, remove

n.samples.trip <- n.samples.trip %>%
  mutate(antenna_temporary = case_when( #if no value for antenna temporary and
    #there is a calculated value, replace with calculated value
    antenna_temporary == 0 & !is.na(antenna.effort.calc) ~ antenna.effort.calc,
    #otherwise, keep antenna_temporary effort count
    TRUE ~ antenna_temporary)) %>%
  select(-antenna.effort.calc) #no longer needed, remove

#when done, check that every fish trip/location/time has a corresponding
#sample/effort record




# antenna data - keep only if matching record in FMS #######

#Making sure all antenna types are present and pulled from Big Boy
unique(antenna$GEAR_CODE)

#assign season and river mile to antenna dataframe
#load gear type table
gear <- read.csv("./data/gear_types.csv", stringsAsFactors = FALSE)

gear <- gear %>%
  select(-n)

antenna <- antenna %>% #join generalized gear type to antenna data
  left_join(gear)

#one gear type is confusingly permanent and temporary antennas
#BK_UNBAITED is permanent if 141 and 127. Otherwise temporary
unique(antenna$SAMPLE_TYPE[antenna$GEAR_CODE == "BK_UNBAITED"])
antenna <- antenna %>%
  mutate(gear = case_when(GEAR_CODE == "BK_UNBAITED" &
                            SAMPLE_TYPE %in% c(127, 141) ~ "antenna_permanent",
                          TRUE ~ gear))

#remove permanent antennas
antenna <- antenna %>%
  filter(gear == "antenna temporary")

#assign season and reach

#first, calculate antenna-nights of effort
antenna.effort <- antenna %>%
  group_by(TRIP_ID, gear) %>%
  summarize(n = n())




#for FWS data (missing sample ID, but has antenna ID)
#make antenna id the sa
#filter FMS only
#FWS.ant <- FWS.ant %>%
# filter(Species == "FMS")
#Pull out matching FMS PIT tags from antenna observations
add <- antenna[which(antenna$PITTAG %in% fms$PITTAG),]

add <- add %>% #format date as date
  mutate(START_DATETIME = as.POSIXct(START_DATETIME),
         END_DATETIME = as.POSIXct(END_DATETIME))
glimpse(add)

#Add year to "year" column and PITTAG_RECAP = "Y" column
add <- add %>%
  mutate(year = substr(as.character(START_DATETIME), 1, 4))

add["PITTAG_RECAP"] <- "Y"

#bind rows by column from antenna data to fms.pit data
fms.pit.ant <- bind_rows(fms, add)

#consolidate similar gear types ########
gear <- read.csv("./data/gear_types.csv", stringsAsFactors = FALSE)

gear <- gear %>%
  select(-n)

fms.pit.ant <- fms.pit.ant %>%
  left_join(gear)

#counts of gear types with gear codes simplified/consolidated
gear.totals <- fms.pit.ant %>%
  group_by(gear) %>%
  summarize(n = n(),
            percent = 100*(n/nrow(fms.pit.ant))) %>%
  arrange(-n)
gear.totals

gear.totals <- gear.totals %>%
  mutate(cumulative.percent = 100*(cumsum(n)/sum(n)))
gear.totals

#counts of unique fish by gear types
gear.totals.unique <- fms.pit.ant %>%
  group_by(gear) %>%
  summarize(n = length(unique(PITTAG)),
            percent = 100*(n/nrow(fms.pit.ant))) %>%
  arrange(-n)
gear.totals.unique

gear.totals.unique <- gear.totals.unique %>%
  mutate(cumulative.percent = 100*(cumsum(n)/sum(n)))
gear.totals.unique

gear.totals <- bind_rows(gear.totals, gear.totals.unique)

#save gear totals in output
write.csv(gear.totals, "./output/tables/gear_totals.csv", row.names = FALSE)

fms.pit.ant %>% #see n for each antenna type
  filter(gear %in% c("antenna_temporary", "antenna_permanent")) %>%
  group_by(gear, GEAR_CODE, SAMPLE_TYPE) %>%
  summarize(n = n(),
            unique.fish = length(unique(PITTAG)),
            ratio = unique.fish/n) %>%
  arrange(-ratio)

#subset to only gear types to use #######
#these gear codes will keep 97% of fish captures
#removing things like trammels, angling, seines will simplify calculation of
#effort, but have little impact on sample size
fms.pit.ant <- fms.pit.ant %>%
  filter(gear %in% c("boat_electrofishing", "baited_hoop_net",
                     "unbaited_hoop_net", "antenna_temporary"))

#simplify disposition codes to alive or dead
unique(fms.pit.ant$DISPOSITION_CODE)

fms.pit.ant <- fms.pit.ant %>%
  mutate(disposition = case_when(
    #NAs are either antenna records or missing data, reasonable to presume alive
    DISPOSITION_CODE %in% c("RA", NA) ~ "alive",
    #These codes mean fish was removed from population
    DISPOSITION_CODE %in% c("DR", "DC", "DP", "TA") ~"dead"))

unique(fms.pit.ant$TRIP_ID)

#remove trailing letters in trip codes
#some trips have multiple codes (GC20190722, GC20190722T) because antenna data
#was entered separately. consolidate to one code.
fms.pit.ant <- fms.pit.ant %>%
  mutate(TRIP_ID = str_remove(TRIP_ID, "[[:alpha:]]$"))

#save new csv file with all gear types  #######
write.csv(fms.pit.ant, "./data/all_PIT_tagged_flannelmouth.csv",
          row.names = FALSE)

