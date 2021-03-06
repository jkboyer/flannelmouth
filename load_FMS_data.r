
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

#setup: load packages, data, define subsetting criteria #######################
library(RODBC) #database interface
library(tidyverse)
library(lubridate) #need this for date transformation

theme_set(theme_minimal()) #override ugly default ggplot theme

#load some metric/english conversion fuctions I wrote
source("./functions/conversion_functions.r")

#define cutoffs to use for subsetting data
start.year <- 2004

#season cutoffs - will make pretend 2000-mm-dd datetime to subset
start.spring <- strftime(as.POSIXct("2000-03-01"))
end.spring <- strftime(as.POSIXct("2000-06-20"))
start.fall <- strftime(as.POSIXct("2000-08-01"))
end.fall <- strftime(as.POSIXct("2000-10-31"))

#gear types to keep
#these are aggregated (e.g. baited hoop net includes HB, MHB, and more, see
#                      gear_types.csv for grouping)
gear.types.keep <- c("boat_electrofishing", "baited_hoop_net",
                     "unbaited_hoop_net", "antenna_temporary")

#length of reach used to bin data for cauchy distribution
reach.km = 8

#size break - dividing point between adult and subadult (in mm)
size.break <- 325

# load data from big boy database #############################################
# database file name: UPDATE to most recent version here
db.GCMRC <- "FISH_SAMPLE_SPECIMEN_HISTORY_20201210_1415.mdb"

# specify file location of GCMRC database
#Laura's working file path
# gcmrc.file.path <- "M:/Lovich/Laura Tennant work folder/GCMRC/FMS_mark_recap/"
# Laura's coronavirus work from home filepath
gcmrc.file.path <- "C:/Users/ltennant/Desktop/FMS_mark_recap/"
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
                        "END_RM, START_RKM, END_RKM, STATION_ID, GPS_START_WAYPOINT,",
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
                          "END_DATETIME, START_RKM, START_RM, STATION_ID,",
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
                          "START_RM, END_RM, START_RKM, STATION_ID,",
                          "START_DATETIME, END_DATETIME,",
                          "EF_TOTAL_SECONDS, SAMPLE_ID",
                          "FROM FISH_T_SAMPLE",
                          "WHERE TRIP_ID IN (", trip.list, ")"),
                    stringsAsFactors = FALSE, #import strings as character, not factor
                    na.strings = c(NA, "", " ", 999, -999, "#N/A")) #these values are all NA

odbcClose(db) #close database connection

# load additional data from NPS and FWS ######################################
# capture data mostly from bright angel and shinumo (mainstem sampling) trips

#load NPS flannelmouth PIT tag data
nps <- read.csv("./data/NPS_FMS_data_captures_forJan17March2020.csv",
                       stringsAsFactors = FALSE)
glimpse(nps)

#nps.sample data
nps.sample <- read.csv("./data/NPS_shinumo_nets_sample.csv",
                       stringsAsFactors = FALSE)

nps <- nps %>% #format dates
  mutate(START_DATE = as.Date(START_DATE, format = "%m/%d/%Y"),
         #convert to start datetime to match big boy data
         START_DATETIME = as.POSIXct(paste(START_DATE, "12:00:00")))

#Fix location issues (missing RMs
#rules: All COR captures on shinumo trips were between RM 108 and 109.2
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
                              # all SHI captures are below waterfall
                              RIVER_CODE == "SHI" ~ 109.3,
                              TRUE ~ START_RM),
         #for all trib fish near mouth, recode river to Colorado
         RIVER_CODE = case_when(RIVER_CODE == "BAC" & STATION_ID == -1 ~ "COR",
                                RIVER_CODE == "HAV" ~ "COR",
                                # I think all SHI captures are below waterfall
                                # but check with Brian
                                RIVER_CODE == "SHI" ~ "COR",
                                TRUE ~ RIVER_CODE),
        #for further upstream Bright Angel sites, assign them rkm 1.0
        #km 1.0 is arbitrary, but actually having a rkm will ensure these fish
        #are not dropped for missing location info, and all BAC fish will
        #be binned into one 8km BAC reach for analysis anyway
         START_RKM = case_when(RIVER_CODE == "BAC" & STATION_ID > -1 ~ 1.0,
                               TRUE ~ as.numeric(NA))) %>%
  select(-STATION_ID) #no longer needed, remove

nps <- nps %>%
  select(c(colnames(nps)[colnames(nps) %in% colnames(fms)])) %>%
  filter(PITTAG != "") #remove fish that are not PIT tagged
glimpse(nps)

# join NPS data to big boy data
fms <- fms %>%
  bind_rows(nps)

rm(nps) #no longer needed, remove

nps.sample <- nps.sample %>%
  mutate(SAMPLE_ID = paste0("NPS", ACCESS_SAMPLE_ID)) %>%
  select(-c(ACCESS_SAMPLE_ID, DATASHEET_SAMPLE_ID)) %>%
  #the dates and times are formatted wrong! fix them. datetimes are frustrating!
  mutate(START_DATE = as.Date(START_DATE, format = "%m/%d/%Y"),
         END_DATE = as.Date(END_DATE, format = "%m/%d/%Y"),
         START_DATETIME = as.POSIXct(paste0(as.character(START_DATE), " ",
                                 START_TIME, ":00")),
         END_DATETIME = as.POSIXct(paste0(as.character(END_DATE), " ",
                                 END_TIME, ":00")),
         #NPS data did not have river mile, but they sample between
         #bass rapid and shinumo, so this is halfway point
         START_RM = 108.9,
         RIVER_CODE = "COR") %>%
  select(-c(START_DATE, START_TIME, END_DATE, END_TIME))
colnames(nps.sample) %in% colnames(samples)

samples <- samples %>%
  mutate(SAMPLE_ID = as.character(SAMPLE_ID)) %>%
  bind_rows(nps.sample)

#NPS data is from BAC antenna, and shinumo area mainstem hoopnetting trips
#FWS data is submersible antenna data, will eventually be in big boy, but is
#not yet in big boy

#NPS antenna data
#Adding NPS antenna BAC data
NPS.ant <- read.csv("./data/FMS_NPScaptures_on_BAC_antenna.csv",
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
FWS.ant$START_DATETIME <- gsub("/","-", FWS.ant$Datetime)
FWS.ant$START_DATETIME <- parse_date_time(FWS.ant$START_DATETIME, orders="mdy_H!M!")

#rename columns
colnames(FWS.ant)[4] <- "PITTAG"
colnames(FWS.ant)[6] <- "RIVER_CODE"
colnames(FWS.ant)[7] <- "START_RM"
colnames(FWS.ant)[10] <- "Antenna_ID"


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

#Havasu needs a START_RM and name reassigned because antennas placed at mouth of HAV in COR
FWS.ant <- FWS.ant %>%
  mutate(START_RM = case_when(RIVER_CODE == "HAV" ~ 157.3,
                              TRUE ~ START_RM),
        RIVER_CODE = case_when(RIVER_CODE == "HAV" ~ "COR",
                                 TRUE ~ RIVER_CODE))

#need to assign RM manually (Pillow sent "PA20180521.csv" file with RMs on 12/18/20)
FWS.ant<- FWS.ant %>%
  mutate(START_RM = case_when(TRIP_ID == "GC20180521" & is.na(START_RM) & Date == "5/21/2018" ~ 236.2,
                              #5/22/2018 antennas were placed between RM 236.2 and 249.8 (just assigned RM to b/t these two)
                              TRIP_ID == "GC20180521" & is.na(START_RM) & Date == "5/22/2018" ~ 243,
                              #5/23/2018 antennas were placed between RM 247.1 and 268 (just assigned RM to b/t these two)
                              TRIP_ID == "GC20180521" & is.na(START_RM) & Date == "5/23/2018" ~ 257.55,
                              TRIP_ID == "GC20180521" & is.na(START_RM) & Date == "5/24/2018" ~ 268,
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
             X, X.1, Baited, Antenna.Type))

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

# Fix various errors in data ##################################################
fms <- fms %>% #don't need species column since they are all flannelmouth
  select(-SPECIES_CODE) #remove column

# people used 999 as an NA value!?
fms$START_RM <- ifelse(fms$START_RM == 999.00, NA, fms$START_RM)
samples$START_RM <- ifelse(samples$START_RM == 999.00, NA, samples$START_RM)
fms$TOTAL_LENGTH <- ifelse(fms$TOTAL_LENGTH <= 1, NA, fms$TOTAL_LENGTH)

fms$PITTAG <- ifelse(fms$PITTAG %in% c("N", "SCANNER KAPUT"), NA, fms$PITTAG)

fms <- fms %>%
  mutate(length.tag = nchar(PITTAG), #count length of pit tag code
         PITTAG = toupper(PITTAG)) #capitalize pittag

fms %>%
  group_by(length.tag) %>%
  summarize(n = n())

#Incorrect PIT tag codes - FIX THEM
# . was omitted in some 3DD.003... entries (recorded as 3DD003...)
#for lengths of 13, if 4th character is not . add .
fms <- fms %>%
  mutate(PITTAG = case_when(nchar(PITTAG) == 13 & #one less than normal 14 chars
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

#extract coefficients for fork-total length equations
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

#for all fish - if total length is missing, calculate from fork
#               if fork length missing, calculate from total
fms <- fms %>%
  mutate(FORK_LENGTH = as.numeric(FORK_LENGTH),
         TOTAL_LENGTH = as.numeric(TOTAL_LENGTH)) %>%
  #if length is missing, calculate from other length, if length exists keep it
  mutate(TL = case_when(is.na(TOTAL_LENGTH) ~
                          round(FLtoTL.intercept + FLtoTL.slope*FORK_LENGTH, 0),
                        TRUE ~ TOTAL_LENGTH),
         FL = case_when(is.na(FORK_LENGTH) ~
                          round(TLtoFL.intercept + TLtoFL.slope*TOTAL_LENGTH, 0),
                        TRUE ~ FORK_LENGTH))

#save all FMS (including not tagged) to examine size structure, size at maturity
write.csv(fms, "./data/all_flannelmouth.csv", row.names = FALSE)

#subset to only PIT tagged fish captured since start year (2004) ######
fms <- fms %>%
  filter(!is.na(PITTAG) & #remove non tagged fish
          length.tag == 14) %>% #new tags have 14 digits
  filter(year >= start.year) %>%
  select(-c(SEX_CODE, SEX_COND_CODE, SEX_CHAR_CODE, length.tag,
            PITTAG2, PITTAG2_RECAP, PITTAG3, PITTAG3_RECAP))

#also subset the sample dataframe to since start year (2004)
samples <- samples  %>%
  filter(START_DATETIME >= as.POSIXct(paste0(start.year, "-01-01 00:0:01")))
#don't need to subset antennas by year, they were not used until 2009

# fish captured in mouths of tributaries - recode as mainstem fish #####
#see what fish are in tributaries
tribs <- fms %>%
  filter(RIVER_CODE != "COR")

# some fish have tributary river codes, but a mainstem river mile recorded
# (e.g. HAV 157.26, SHI 108.6), and a low (<0.2) or no kilometer record
# these fish were captured near a confluence and can be reclassified as
# COR (mainstem) fish
trib.cutoff <- 0.2 #how many miles up a tributary we consider mainstem
#if start km < 0.2*1.609, or if start km is missing and start RM < 0.2,
#reclassify as COR and assign start RM based on tributary

unique(fms$RIVER_CODE) # see what river codes there are

#dataframe of confluence locations in miles
#BAC not included because NPS did not supply rm or rkm data
confluences <- data.frame(RIVER_CODE = c("COR", "HAV", "LCR", "SHI"),
                          confluence_RM = c(NA, 157.3, 61.8,  109.3 ))

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
      (RIVER_CODE == "BAC" & START_RM >= 87.5 & START_RM <= 88.8) |
      (RIVER_CODE == "SHI" & START_RM >= 108.6 & START_RM <= 109.3) |
      (RIVER_CODE == "HAV" & START_RM >= 156.7 & START_RM <= 157.3)) ~ "COR",
    #otherwise, keep river code unchanged
    TRUE ~ RIVER_CODE))

#Do the same for antenna data - a few antennas in mouth of HAV should be COR
antenna <- antenna %>% #join confluence miles to fms data
  left_join(confluences)

antenna <- antenna %>%
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
               (RIVER_CODE == "BAC" & START_RM >= 87.5 & START_RM <= 88.8) |
               (RIVER_CODE == "SHI" & START_RM >= 108.6 & START_RM <= 109.3) |
               (RIVER_CODE == "HAV" & START_RM >= 156.7 & START_RM <= 157.3)) ~ "COR",
           #otherwise, keep river code unchanged
           TRUE ~ RIVER_CODE))

#do the same for samples
samples <- samples %>% #join confluence miles to fms data
  left_join(confluences)

samples <- samples %>%
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
                                    (RIVER_CODE == "BAC" & START_RM >= 87.5 & START_RM <= 88.8) |
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

#do the same for samples
samples <- samples %>%
  mutate(START_RM = case_when(RIVER_CODE %in% c("HAV", "SHI") & TRIP_ID == "GC20170819" ~ confluence_RM,
                              TRUE ~ START_RM),
         RIVER_CODE = case_when(RIVER_CODE %in% c("HAV", "SHI") & TRIP_ID == "GC20170819" ~ "COR",
                                TRUE ~ RIVER_CODE)) %>%
  select(-confluence_RM) #no longer needed, remove column

#Questions still remaining:
#   in LCR, what is mile 197? it was an agg trip recode as COR
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
#now only LCR and BAC has actual tributary fish
#makes sense, most FMS from SHI and HAV are right in mouth

rm(confluences, fms.lengths, tribs, lm.FL.to.TL, lm.TL.to.FL) # no longer needed, remove

# a mechanical removal trip with many records is missing river mile ########
# load table with river miles of station and merge
#grab mechanical removal stations from Big Boy to work on fixing reach data

#load station.csv files that have station information needed
mrs <- read.csv("./data/BB_Mechanical_removal_stations.csv",
                stringsAsFactors = FALSE) #mechanical removal stations
lfs <- read.csv("./data/BB_LF_Stations.csv",
                stringsAsFactors = FALSE) #Lee's Ferry stations

#other lees ferry file does not have NO/TRGD stations
lfs2 <- read.csv("./data/LF_Stations.csv", #this file used for AGFD site selection,
                 stringsAsFactors = FALSE) #NO/TRGD Ids should be in here

head(mrs)
head(lfs)

#merge station data based on station id
#then fill in mile if mile is missing

#column names need to match (for column we are joining on)
colnames(fms)

#subset to just data missing mile to see missing data
fms.missing.mile <- fms %>%
  filter(is.na(START_RM) & RIVER_CODE == "COR")

#station IDs are in multiple places
#UNPADDED_STATION_ID or STATION_ID for mechanical removal
#  coding of station ID was not consistent so have to join on both to get miles
#NO ids are in Lees Ferry file, ___ column

#when joining, subset table to be joined to only necessary columns
mrs1 <- mrs %>% #need to associate mechanical removal station IDs to get RM
  #column names must match for joining column
  transmute(STATION_ID = STATION_ID,
            #and not match for other columns
            mile = START_RM)

mrs2 <- mrs %>%
  transmute(STATION_ID = UNPADDED_STATION_ID,
            mile = START_RM)

lfs1 <- lfs %>% #need to associate Lee's Ferry station IDs to get RM
  transmute(STATION_ID = UNPADDED_STATION_ID,
            mile = DOWN_RM)

lfs2 <- lfs2 %>% #need to associate Lee's Ferry station IDs to get RM
  transmute(STATION_ID = NO_start,
            mile = rm_start)

#rbind them all together so I only have to merge once)
stations <- bind_rows(mrs1, mrs2, lfs1, lfs2) %>%
  distinct() #unique rows only, remove duplicates

#check that columns match
glimpse(fms)
glimpse(mrs1)
glimpse(lfs1)

#join data from mechanical removal station table on to fms
fms <- fms %>%
  left_join(stations)

# if else statement to add mile from station only if start_RM is missing
fms$START_RM <- ifelse(!is.na(fms$START_RM), fms$START_RM, fms$mile)

#and repeat for samples
samples <- samples %>%
  left_join(stations)

# if else statement to add mile from station only if start_RM is missing
samples$START_RM <- ifelse(!is.na(samples$START_RM), samples$START_RM, samples$mile)

#how many are still missing?
fms.missing.mile <- fms %>%
  filter(is.na(START_RM) & RIVER_CODE == "COR")

#still some NO sites missing from a GC January 2015 trip, but winter data will
# be subset out anyway so not worth fixing, just drop that data
#also ~ 50 fish from various trips missing river mile due to incomplete data
# recording, just drop, 50 fish out of 90k records is nothing.

fms <- fms %>% # remove mile columns from fms
  select(-mile)

samples <- samples %>% # remove mile columns from fms
  select(-mile)
rm(lfs, mrs, lfs1, lfs2, mrs1, mrs2, stations) #no longer needed, remove

# what else is missing for RMs and RKMs (inlcudes LCR, HAV, etc.)
missing.mile.tribs <- fms %>%
  filter(RIVER_CODE != "COR" & is.na(START_RM) & is.na(START_RKM))
# Missing location info:
#  LCR NSE data (129): drop, Charles said NSE LCR data is weird anyway
# BAC records - should figure out cause we don't have much BAC data

# remove all RM = NA for where we couldn't find the RM information for COR (only 27 records) -
# did it for RKMs for tribs as well (18 records) = 45 records
fms <- fms %>%  #only keep mainstem records if they have river mile
  filter((!is.na(START_RM) & RIVER_CODE == "COR") |
           #keep trip records if they have kilometer OR mile
           RIVER_CODE %in% c("BAC", "HAV", "LCR", "SHI") &
              (!is.na(START_RM) | !is.na(START_RKM)))

# double checking all records were removed
fms.missing.mile <- fms %>%
  filter(is.na(START_RM) & is.na(START_RKM))

#Also remove fish that are missing length
fms.missing.length <- fms %>%
  filter(is.na(TL))
#only 174 fish and they are missing fork lengths too - just drop the data
fms <- fms %>%
  filter(!is.na(TL))


# calculate sampling effort ###################################################

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

#join generalized gear type to fms and antenna and sample data
fms <- fms %>%
  left_join(gear) %>%
#one gear type is confusingly permanent and temporary antennas
#BK_UNBAITED is permanent if 141 and 127. Otherwise temporary
  mutate(gear = case_when(GEAR_CODE == "BK_UNBAITED" &
                            SAMPLE_TYPE %in% c(127, 141) ~ "antenna_permanent",
                          TRUE ~ gear)) %>%
  mutate(day = as.POSIXct(paste0("2000", #assign same year to all
                      #keep the month-day-time part
                      substr(as.character(START_DATETIME), 5, 19)))) %>%
  filter(gear %in% gear.types.keep) %>% #subset to gear types to analyze
  #remove AGFD lower 1200 LCR samplign - enough LCR data from FWS
  filter(SAMPLE_TYPE %in% c(94, 95) == FALSE) %>%
  filter((day >= start.spring & day <= end.spring) | #spring or fall only
           (day >= start.fall & day <= end.fall)) %>%
  #add variable for season
  mutate(season = case_when(day >= start.spring & day <= end.spring ~ "spring",
                            day >= start.fall & day <= end.fall ~ "fall")) %>%
  #remove day column - would not want people to think it is actual date
  select(-day)

antenna <- antenna %>%
  left_join(gear) %>%
  mutate(gear = case_when(GEAR_CODE == "BK_UNBAITED" &
                            SAMPLE_TYPE %in% c(127, 141) ~ "antenna_permanent",
                          TRUE ~ gear)) %>%
  mutate(day = as.POSIXct(paste0("2000", #assign same year to all
                                 #keep the month-day-time part
                                 substr(as.character(START_DATETIME), 5, 19)))) %>%
  filter(gear %in% gear.types.keep) %>% #gear type - temporary antennas only
  filter((day >= start.spring & day <= end.spring) | #spring or fall only
           (day >= start.fall & day <= end.fall)) %>%
  #add variable for season
  mutate(season = case_when(day >= start.spring & day <= end.spring ~ "spring",
                            day >= start.fall & day <= end.fall ~ "fall")) %>%
  #remove day column - would not want people to think it is actual date
  select(-day)

samples <- samples %>%
  left_join(gear) %>%
  mutate(day = as.POSIXct(paste0("2000", #assign same year to all
                                 #keep the month-day-time part
                                 substr(as.character(START_DATETIME), 5, 19)))) %>%
  mutate(gear = case_when(GEAR_CODE == "BK_UNBAITED" &
                            SAMPLE_TYPE %in% c(127, 141) ~ "antenna_permanent",
                          TRUE ~ gear)) %>%
  filter(gear %in% gear.types.keep) %>% #filter by gear type
  #remove AGFD lower 1200 LCR samplign - enough LCR data from FWS
  filter(SAMPLE_TYPE %in% c(94, 95) == FALSE) %>%
  filter((day >= start.spring & day <= end.spring) | #spring or fall only
           (day >= start.fall & day <= end.fall)) %>%
  #add variable for season
  mutate(season = case_when(day >= start.spring & day <= end.spring ~ "spring",
                            day >= start.fall & day <= end.fall ~ "fall")) %>%
  #remove day column - would not want people to think it is actual date
  select(-day)

#for each trip, bin into 5 miles (counting from dam) ######
#define length of reach to bin samples in to
max.rkm <- max(MileToKmCOR(samples$START_RM[samples$RIVER_CODE == "COR"]),
               na.rm = TRUE)

#convert river mile to kilometer
samples <- samples %>% #calculate km from mile, or insert NA if tributary
  mutate(start_rkm = case_when(RIVER_CODE == "COR" ~ MileToKmCOR(START_RM),
                               #LCR will be next number after max Colorado
                               RIVER_CODE == "LCR" ~ max.rkm + 8,
                               RIVER_CODE == "BAC" ~ max.rkm + 16),
         #for COR, split into 8 km reaches
         #for LCR, reach = (max COR reach) + 1
         reach = cut(start_rkm, seq(0, 520, by = reach.km)),
         reach_no = as.numeric(reach), #number for each reach
         #get start point - a numeric field is useful for graphing
         #for now, LCR and BAC has confluence rkm - not sure if this is best approach
         reach_start = case_when(RIVER_CODE == "COR" ~ (reach_no - 1)*reach.km,
                                 RIVER_CODE == "LCR" ~ MileToKmCOR(61.4),
                                 RIVER_CODE == "BAC" ~ MileToKmCOR(88.3))) %>%
arrange(reach)

 #remove the rkm 512s for LCR and 520s for BAC - not actual Rkm, just what I used to create
  #reach numbering without messing up factors
samples$start_rkm <- ifelse(samples$RIVER_CODE %in% c( "LCR", "BAC"),
                            NA, samples$start_rkm)

samples %>% #plot to see spatial distribution of samples
  ggplot(aes(x = reach_start)) +
  geom_histogram()
#yep, theres a lot of sampling at the LCR

fms <- fms %>% #calculate km from mile, or insert NA if tributary
  mutate(start_rkm = case_when(RIVER_CODE == "COR" ~ MileToKmCOR(START_RM),
                               #LCR will be next number after max Colorado
                               RIVER_CODE == "LCR" ~ max.rkm + 8,
                               RIVER_CODE == "BAC" ~ max.rkm + 16),
         #for COR, split into 8 km reaches
         #for LCR, reach = (max COR reach) + 1
         reach = cut(start_rkm, seq(0, 520, by = reach.km)),
         reach_no = as.numeric(reach), #number for each reach
         #get start point - a numeric field is useful for graphing
         #for now, LCR has confluence rkm - not sure if this is best approach
         reach_start = case_when(RIVER_CODE == "COR" ~ (reach_no - 1)*reach.km,
                                 RIVER_CODE == "LCR" ~ MileToKmCOR(61.4),
                                 RIVER_CODE == "BAC" ~MileToKmCOR(88.3))) %>%
  arrange(reach)

#remove the rkm 512s for LCR - not actual Rkm, just what I used to create
#reach numbering without messing up factors
fms$start_rkm <- ifelse(fms$RIVER_CODE %in% c("LCR", "BAC"), NA, fms$start_rkm)

antenna <- antenna %>%#calculate km from mile, or insert NA if tributary
  mutate(start_rkm = case_when(RIVER_CODE == "COR" ~ MileToKmCOR(START_RM),
                               #LCR will be next number after max Colorado
                               RIVER_CODE == "LCR" ~ max.rkm + 8,
                               RIVER_CODE == "BAC" ~ max.rkm + 16),
         #for COR, split into 8 km reaches
         #for LCR, reach = (max COR reach) + 1
         reach = cut(start_rkm, seq(0, 520, by = reach.km)),
         reach_no = as.numeric(reach), #number for each reach
         #get start point - a numeric field is useful for graphing
         #for now, LCR has confluence rkm - not sure if this is best approach
         reach_start = case_when(RIVER_CODE == "COR" ~ (reach_no - 1)*reach.km,
                                 RIVER_CODE == "LCR" ~ MileToKmCOR(61.4),
                                 RIVER_CODE == "BAC" ~MileToKmCOR(88.3))) %>%
  arrange(reach)

#remove the rkm 512s for LCR - not actual Rkm, just what I used to create
#reach numbering without messing up factors
antenna$start_rkm <- ifelse(antenna$RIVER_CODE %in% c("LCR", "BAC"),
                            NA, antenna$start_rkm)

# calculate days of effort for antennas ########
#trip id, reach_start, year + season
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

antenna <- antenna %>%
  mutate(STATION_ID = as.character(STATION_ID))

#merge antenna data onto fms data ########
fms <- fms %>%
  mutate(year = as.numeric(year)) %>%
  bind_rows(antenna)

rm(antenna) #no longer need separate file, now it is on fish data

#calculate n times each fish captured
n_captures <- fms %>%
  group_by(PITTAG) %>%
  summarize(n_captures = n(),
            recapture = case_when(n_captures > 1 ~ "Y",
                                  n_captures == 1 ~ "N"))

fms <- fms %>%
  left_join(n_captures)

#how many fish were captured a certiain number of times?
fms %>%
  group_by(n_captures) %>%
  summarize(n = n()) %>%
  print(n = 30) #print all rows

#antennas calculated from sample and antennas calculated from fms
#ccheck match, replace 0s from sample with fms

#calculate effort per 5 miles per time block for each gear type #######

samples <- samples %>% #add year column
  mutate(year = as.numeric(substr(START_DATETIME, 1, 4)))

#fix any effort discrepancies before calculating effort
#check that hoops and antennas were set for 1 day, not more
#if set for 2 days, effort = 2

#Effort rules:
# Electrofishing: 1 250m site = 1 effort
#                 NSE (129) sites are 50m, so they are 0.2 effort each
#Hoop nets: 1 day (e.g., overnight set) of effort = 1 effort
#Unbaited hoops: usually set for 1 day, 2 or 3 day sets happen occasionally
#Baited hoops: should always be set for 1 day, other values are most likely 1 day
#              sets with date entry errors
#Antennas: like baited hoop nets, all temp antennas should be 1 day sets


samples <- samples %>% #get date from datetime
  mutate(start_date = as.Date(substr(as.character(START_DATETIME), 1, 10)),
         end_date = as.Date(substr(as.character(END_DATETIME), 1, 10)),
         effort_days = round(as.numeric(difftime(end_date,
                                                 start_date, units = "days"))))
#look at variation in set times for each gear
set_times <- samples %>%
  group_by(gear, effort_days) %>%
  summarize(n = n())

#Create n.samples column using effort rules above
samples <- samples %>%
  mutate(n.samples = case_when(
    #electrofishing: each sample = 1, except NSE samples (129) which = 0.2
    gear == "boat_electrofishing" & SAMPLE_TYPE == 129 ~ 0.2,
    gear == "boat_electrofishing" ~ 1,
    #unbaited hoops - may be 1, 2, or 3 (days)
    #assume any negative or very large numbers are data entry errors
    #and are 1 day sets (1 day sets are most common)
    gear == "unbaited_hoop_net" &  effort_days %in% c(1,2,3) ~ effort_days,
    gear == "unbaited_hoop_net" ~ 1,
    #baited hoops - always set for 1 night
    gear == "baited_hoop_net" ~ 1,
    #temporary antennas - should always set for 1 night
    gear == "antenna_temporary" ~ 1))

#summary of sample effort
samples %>%
  group_by(gear, n.samples) %>%
  summarize(n = n())

# add effort data from trips we don't have sample data for
#there were 30 hoops set in BAC (location unknown, NPS does not record that)
#on BAC20180917
nps.hoops.BAC <-
  data.frame(TRIP_ID = "BAC20180917", gear = "baited_hoop_net",
             gear_code_simple = "HB", year = 2018, reach_no = 65,
             reach = "(512,520]", reach_start = 167.53,
             season = "fall", reach_start = NA, RIVER_CODE = "BAC",
             n.samples = 30)

samples <- bind_rows(samples, nps.hoops.BAC)

#All the samples coded SHI are actually mainstem samples near shinumo
#switch to COR and add appropriate reach start km
MileToKmCOR(108.6) #SHI
MileToKmCOR(157.3) #HAV

samples <- samples %>%
  #assign reach where sampling occurred
  mutate(reach_start = case_when(RIVER_CODE == "SHI" ~ 200,
                                 RIVER_CODE == "HAV" ~ 272,
                                 TRUE ~ reach_start),
         #switch river code to COR
         RIVER_CODE = case_when(RIVER_CODE %in% c("SHI", "HAV") ~ "COR",
                                TRUE ~ RIVER_CODE))
#make combined trip/gear variable
samples <- samples %>%
  mutate(trip.gear = paste(TRIP_ID, gear_code_simplified, sep = "_"))

#effort by trip, year, season, 8km reach
n.samples <- samples %>%
  group_by(TRIP_ID,  gear, season, year, reach_start, RIVER_CODE) %>%
  summarize(n = sum(n.samples))

n.samples <- n.samples %>%
  pivot_wider(names_from = gear, values_from = n,
              values_fill = list(n = 0)) #fill with zero if is NA

#effort by trip, year, season (no reach grouping)
n.samples.trip <- samples %>%
  group_by(TRIP_ID, gear, season, year, RIVER_CODE) %>%
  summarize(n = sum(n.samples))

n.samples.trip <- n.samples.trip %>%
  pivot_wider(names_from = gear, values_from = n,
              values_fill = list(n = 0)) #fill with zero if is NA

#add on calculated anntenna effort if antenna effort is missing
n.samples <- n.samples %>%
  left_join(antenna.effort) %>%
  mutate(antenna.effort.calc = as.numeric(antenna.effort.calc))

n.samples.trip <- n.samples.trip %>%
  left_join(antenna.effort.trip) %>%
  mutate(antenna.effort.calc = as.numeric(antenna.effort.calc))

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

#do all trips have effort data?
missing.trip.effort <- fms %>%
  filter((TRIP_ID %in% n.samples.trip$TRIP_ID) == FALSE)
trips.missing.effort <- unique(missing.trip.effort$TRIP_ID)
trips.missing.effort
#no effort for SHI20150830 trip
#    or GC20180521 (128) baited antennas
#    GC20180326 (128) baited antennas
#    BAC20190521 (we just don't have this data, brian only gave me 2018)
# 362 records of 77k, drop them
fms <- fms %>% #remove fms with no sampling effort recorded for that trip
  filter((TRIP_ID %in% trips.missing.effort) == FALSE)

#make a combined variable for tripid, season, reach start
fms <- fms %>%
  mutate(id = paste(TRIP_ID, season, reach_start))
n.samples <- n.samples %>%
  mutate(id = paste(TRIP_ID, season, reach_start))

#check if all fms records have corresponding efort for that trip/reach/season
missing.reach.effort <- fms %>%
  filter((id %in% n.samples$id) == FALSE)
#only 42 missing! We did so good! I thought the data would be worse
#42 is barely any fish, drop them

reaches.missing.effort <- unique(missing.reach.effort$id)
reaches.missing.effort
fms <- fms %>% #remove fms with no sampling effort recorded for that reach
  filter((id %in% reaches.missing.effort) == FALSE)

# a few sample size summaries for gears ###########
#counts of gear types with gear codes simplified/consolidated
gear.totals <- fms %>%
  group_by(gear) %>%
  summarize(n = n(),
            percent = 100*(n/nrow(fms))) %>%
  arrange(-n)
gear.totals

gear.totals <- gear.totals %>%
  mutate(cumulative.percent = 100*(cumsum(n)/sum(n)))
gear.totals

#counts of unique fish by gear types
gear.totals.unique <- fms %>%
  group_by(gear) %>%
  summarize(n = length(unique(PITTAG)),
            percent = 100*(n/nrow(fms))) %>%
  arrange(-n)
gear.totals.unique

gear.totals.unique <- gear.totals.unique %>%
  mutate(cumulative.percent = 100*(cumsum(n)/sum(n)))
gear.totals.unique

gear.totals <- bind_rows(gear.totals, gear.totals.unique)

#save gear totals in output
write.csv(gear.totals, "./output/tables/gear_totals.csv", row.names = FALSE)

#simplify disposition codes to alive or dead #######
unique(fms$DISPOSITION_CODE)

fms <- fms %>%
  mutate(disposition = case_when(
    #NAs are either antenna records or missing data, reasonable to presume alive
    DISPOSITION_CODE %in% c("RA", NA) ~ "alive",
    #These codes mean fish was removed from population
    DISPOSITION_CODE %in% c("DR", "DC", "DP", "TA") ~"dead"),
    size.class = case_when(TL < size.break ~ 1,
                     TL >= size.break ~ 2))

#add a variable for trip and gear combined
#this is the metric we will make capture histories on

fms <- fms %>%
  mutate(trip.gear = paste(TRIP_ID, gear_code_simplified, sep = "_"))


#save new csv file with all gear types  #######
write.csv(fms, "./data/all_PIT_tagged_flannelmouth.csv",
          row.names = FALSE)

write.csv(n.samples, "./data/effort_by_trip_reach.csv",
          row.names = FALSE)

write.csv(n.samples.trip, "./data/effort_by_trip.csv",
          row.names = FALSE)

