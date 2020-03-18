#loads data
#1. all PIT tagged FMS in GCMRC database for mark-recapture analysis
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

library(RODBC) #database interface
library(tidyverse)

theme_set(theme_minimal()) #override ugly default ggplot theme

#load data ######

# define database file name as most recent version here
db.GCMRC <- "FISH_SAMPLE_SPECIMEN_HISTORY_20200212_1711.mdb"

# specify file location of GCMRC database
#Laura's working file path
gcmrc.file.path <- "M:/Lovich/Laura Tennant work folder/GCMRC/FMS_mark_recap/"
#Jan's working file path
gcmrc.file.path <- "\\\\flag-server/Office/GCMRC_master_database/"

#connect to database
db <- odbcConnectAccess(paste(gcmrc.file.path, db.GCMRC, sep = ""))

#see tables and column names
sqlTables(db)
sqlColumns(db, "SAMPLE_SPECIMEN_ALL")$COLUMN_NAME

# query desired data from specimen table
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
odbcClose(db) #close database connection

# load NPS flannelmouth recapture data #######
# mostly from bright angel and shinumo trips

#need to ask Brian
#location info - what do station IDs mean, for fish without RM
#are datetimes available, or only dates?
#on shinumo trips, were hoop nets in mainstem or shinumo, and how do I tell?
#BAC river mile numbering in station_id - where is it from?
#                          what are negative RMs?
#data we still need: anything location related, disposition code


#load NPS flannelmouth data
nps.filepath <- "\\\\FLAG-SERVER/Office/Grand Canyon Downstream/Databases/NPS_data/"
nps.filename <- "NPS_FMS_data_captures_forJan17March2020.csv"

#load NPS flannelmouth PIT tag data
nps <- read.csv(paste0(nps.filepath, nps.filename), stringsAsFactors = FALSE)
glimpse(nps)
colnames(nps)



#what columns will get deleted?
colnames(nps)[(colnames(nps) %in% colnames(fms)) == FALSE]
#what columns will get kept?
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
                                TRUE ~ RIVER_CODE))

nps <- nps %>%
  select(-STATION_ID) #no longer needed, remove

# join NPS data to big boy data
fms <- fms %>%
  bind_rows(nps)

rm(nps) #no longer needed, remove

# format and subset data ######
fms <- fms %>% #don't need species column since they are all flannelmouth
  select(-SPECIES_CODE) %>% #remove column
  filter(!is.na(PITTAG)) #remove non tagged fish

# Fix frustrating things people did, like using 999 as NA, and writing comments
# in the PITTAG field
fms$START_RM <- ifelse(fms$START_RM == 999.00, NA, fms$START_RM)

fms$TOTAL_LENGTH <- ifelse(fms$TOTAL_LENGTH <= 1, NA, fms$TOTAL_LENGTH)

fms$PITTAG <- ifelse(fms$PITTAG %in% c("N", "SCANNER KAPUT"), NA, fms$PITTAG)

#Incorrect PIT tag codes
# . was omitted in some 3DD. entries
fms <- fms %>%
  mutate(length.tag = nchar(PITTAG),
         PITTAG = toupper(PITTAG)) #capitalize pittag

fms %>%
  group_by(length.tag) %>%
  summarize(n = n())

#for lentgths of 13, if 4th character is not . add .
fms %>%
  filter(nchar(PITTAG) == 13 & str_detect(PITTAG, "\\.") == FALSE)

fms <- fms %>%
  mutate(PITTAG = case_when(nchar(PITTAG) == 13 & #one less than normal 14
                              #and there is no . in PITTAG
                              str_detect(PITTAG, "\\.") == FALSE ~
                              paste0(substr(PITTAG, 1, 3), ".",
                                     substr(PITTAG, 4, 13)),
                            TRUE ~ PITTAG))

fms <- fms %>%
  mutate(length.tag = nchar(PITTAG))

fms %>%
  group_by(length.tag) %>%
  summarize(n = n())
#tags with 11 digits are old tags
#tags with 14 digits are new tags
#others are errors or missing digits - low enough numbers, just drop them.


#make sure all values make sense for that column, replace with NA if not
unique(fms$SAMPLE_TYPE)
unique(fms$TRIP_ID)
unique(fms$GEAR_CODE)
unique(fms$RIVER_CODE)
unique(fms$SEX_CODE)
unique(fms$SEX_COND_CODE)
unique(fms$SEX_CHAR_CODE)
unique(fms$DISPOSITION_CODE)

# get year from datetime
fms <- fms %>%
  mutate(year = substr( as.character(START_DATETIME), 1, 4))

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

# fish captured in mouths of tributaries - recode as mainstem fish #####

#see what fish are in tributaries
tribs <- fms %>%
  filter(RIVER_CODE != "COR")

# some fish have tributary river codes, but a mainstem river mile recorded
# (e.g. HAV 157.26, SHI 108.6), and a low (<0.2) or no kilometer record
# these fish can be reclassified as COR (mainstem) fish
upstream.cutoff <- 0.2 #how many miles up a tributary we consider mainstem
#ASK LAURA AND CHARLES HOW FAR UPSTREAM CUTOFF SHOULD BE


fms <- fms %>%
  mutate(RIVER_CODE = case_when( #reclassify as COR if:
    #if kilometer is missing or small (i.e. close to confluence)
    (is.na(START_RKM) | START_RKM <= upstream.cutoff*1.609) &
    #and river mile matches the appropriate tributary confluence
    ((RIVER_CODE == "PAR" & START_RM >= 0.70 & START_RM <= 0.80) |
    (RIVER_CODE == "LCR" & START_RM >= 61.3 & START_RM <= 61.4) |
    (RIVER_CODE == "CLC" & START_RM == 84.10) |
    (RIVER_CODE == "BAC" & START_RM == 87.70) |
    (RIVER_CODE == "SHI" & START_RM == 108.6) |
    (RIVER_CODE == "DRC" & START_RM == 136.3) |
    (RIVER_CODE == "KAN" & START_RM >= 143.5 & START_RM <= 146.5) |
    (RIVER_CODE == "MAT" & START_RM == 147.8) |
    (RIVER_CODE == "HAV" & START_RM >= 156.7 & START_RM <= 157.3)) ~ "COR",
    #some of the paria records had confluence location in km and miles
    RIVER_CODE == "PAR" & START_RM >= 0.70 & START_RM <= 0.80 &
      START_RKM == 26.7 ~ "COR",
    #otherwise, keep river code unchanged
    TRUE ~ RIVER_CODE))

#most trib records are recorded in km, but some are in RM

#if start km < 0.2*1.609, or if start km is missing and start RM < 0.2,
#reclassify as COR and assign start RM based on tributary

#join confluence rkms to fms
rivers <- sort(c(unique(fms$RIVER_CODE)))

#dataframe of confluence locations in miles
confluences <- data.frame(RIVER_CODE = sort(c(unique(fms$RIVER_CODE))),
                          confluence_RM = c(88.3, #BAC
                                            84.7, #CLR
                                            NA, #COR
                                            136.9, #DRC
                                            157.3, #HAV
                                            144.0, #KAN
                                            61.8, #LCR
                                            52.5, #NKW
                                            0.9, #PAR
                                            260.3, #QUA
                                            109.3, #SHI
                                            NA, #SHM does not exist, typo?
                                            246.3, #SPE
                                            248.7, #SUR
                                            134.3)) #TAP

fms <- fms %>% #join confluence miles to fms data
  left_join(confluences)

# replace RM with confluence RM for fish caught near mouth of tributary
fms <- fms %>%
  mutate(START_RM = case_when(
           RIVER_CODE != "COR" & (START_RKM <= upstream.cutoff*1.609 |
           (is.na(START_RKM) & START_RM <= upstream.cutoff)) ~ confluence_RM,
           TRUE ~ START_RM),
         RIVER_CODE = case_when(
           RIVER_CODE != "COR" & (START_RKM <= upstream.cutoff*1.609 |
           (is.na(START_RKM) & START_RM <= upstream.cutoff)) ~ "COR",
           TRUE ~ RIVER_CODE)) %>%
  select(-confluence_RM) #no longer needed, remove column

#Questions still remaining:
#   how far upstream do we consider mainstem?
#   in LCR, what do negative kilometers mean?
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
  summarize(n = n(),
            n.rkm = sum(!is.na(START_RKM)),
            n.rm = sum(!is.na(START_RM))) %>%
  arrange(-n)
#most trib records are recorded in km, but some are in RM
#some Havasu records with RM 157.2 and some Paria records with 0.7 are clearly COR

# subset to PIT tagged fish only for mark recap analysis #####
fms.pit <- fms %>%
  filter(!is.na(PITTAG)) %>%
  select(-c(SEX_CODE, SEX_COND_CODE, SEX_CHAR_CODE,
            PITTAG2, PITTAG2_RECAP, PITTAG3, PITTAG3_RECAP))

#save data locally in R project data folder #######
write.csv(fms, "./data/all_flannelmouth.csv", row.names = FALSE)
write.csv(fms.pit, "./data/all_PIT_tagged_flannelmouth.csv",
          row.names = FALSE)

