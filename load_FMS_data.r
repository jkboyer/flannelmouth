#loads data
#1. all PIT tagged FMS in GCMRC database for mark-recapture analysis
#2. all flannelmouth to look at size structure, maturity, etc.
#Author: Jan Boyer, AGFD, jboyer@azgfd.gov
#Inputs: most recent version of big boy (access database)
#Outputs: "all_PIT_tagged_flannelmouth.csv"
#         "all_flannelmouth.csv"
#Dependencies:
#Note: MUST USE 32-bit R (and version 1.1.x or older of Rstudio, newer versions
#      of Rstudio only run 64-bit R) to use RODBC to connect to access database
#      in Rstudio Tools/Global Options/General, click change button by R version

library(RODBC) #database interface
library(tidyverse)

#load data ######

# define database file name as most recent version here
db.GCMRC <- "FISH_SAMPLE_SPECIMEN_HISTORY_20200212_1711.mdb"

# specify file location of GCMRC database

gcmrc.file.path <- "M:/Lovich/Laura Tennant work folder/GCMRC/FMS_mark_recap/" #Laura's working file path
gcmrc.file.path <- "\\\\flag-server/Office/GCMRC_master_database/"    #Jan's working file path

#connect to database
db <- odbcConnectAccess(paste(gcmrc.file.path, db.GCMRC, sep = ""))

#see tables and column names
sqlTables(db)
sqlColumns(db, "SAMPLE_SPECIMEN_ALL")$COLUMN_NAME


# define columns wanted from database
#specimen.columns <- paste(
  # SAMPLE_TYPE,",
 # "FISH_T_SAMPLE.START_DATETIME, RIVER_CODE, START_RM, START_RKM,",
 # "SPECIES_CODE, TOTAL_LENGTH, FORK_LENGTH, WEIGHT, DISP,",
#  "SEX_CODE, SEX_COND_CODE, SEX_CHAR_CODE")

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

# format and subset data ######

# Fix frustrating things people did, like using 999 as NA, and writing comments
# in the PITTAG field
fms$START_RM <- ifelse(fms$START_RM == 999.00, NA, fms$START_RM)

fms$TOTAL_LENGTH <- ifelse(fms$TOTAL_LENGTH <= 1, NA, fms$TOTAL_LENGTH)

fms$PITTAG <- ifelse(fms$PITTAG %in% c("N", "SCANNER KAPUT"), NA, fms$PITTAG)

fms <- fms %>% #don't need species column since they are all flannelmouth
  select(-SPECIES_CODE) #remove column

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

theme_set(theme_minimal()) #override ugly default ggplot theme

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

# subset to PIT tagged fish only for mark recap analysis
fms.pit <- fms %>%
  filter(!is.na(PITTAG)) %>%
  select(-c(SEX_CODE, SEX_COND_CODE, SEX_CHAR_CODE,
            PITTAG2, PITTAG2_RECAP, PITTAG3, PITTAG3_RECAP))


#save data locally in R project data folder #######
write.csv(fms, "./data/all_flannelmouth.csv", row.names = FALSE)
write.csv(fms.pit, "./data/all_PIT_tagged_flannelmouth.csv",
          row.names = FALSE)

