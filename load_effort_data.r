#ADD: NPS data
#     antenna calculations

#remove trailing letters (T, B, etc. from trip id)

#Will group by:
#1. Trip
#2. time period: spring (Mar 1 - June 30) and
#                fall (Aug 1 - Oct 31) each year
#3. 8 km reach (measured from dam)

#Load effort data
library(tidyverse)
library(RODBC)

#load some metric/english conversion fuctions I wrote
source("./functions/conversion_functions.r")

#load fish data to see what trips we need effort data for #####
fms <- read.csv("./data/all_PIT_tagged_flannelmouth.csv", stringsAsFactors = FALSE)

trip.ids <- unique(fms$TRIP_ID)
trip.ids

rm(fms) #no longer needed, remove

#####get sample data from big boy #########
# define database file name as most recent version here
db.GCMRC <- "FISH_SAMPLE_SPECIMEN_HISTORY_20200318_1541.mdb"

# specify file location of GCMRC database
#Laura's working file path
gcmrc.file.path <- "M:/Lovich/Laura Tennant work folder/GCMRC/FMS_mark_recap/"
#Jan's working file path
gcmrc.file.path <- "\\\\flag-server/Office/GCMRC_master_database/"
#Jan's coronavirus work from home filepath
gcmrc.file.path <- "C:/Users/jboyer/Documents/big_boy/"

#connect to database
db <- odbcConnectAccess(paste(gcmrc.file.path, db.GCMRC, sep = ""))

#see tables and column names
sqlTables(db)
sqlColumns(db, "FISH_T_SAMPLE")$COLUMN_NAME

#reformat trip ids as a string with quotes and commas to put in SQL query
trip.list <- paste("'", as.character(trip.ids),"'",collapse=", ",sep="")
#query all sample data from those trips from big boy
samples <- sqlQuery(db,
                paste("SELECT TRIP_ID, SAMPLE_TYPE, GEAR_CODE, RIVER_CODE,",
                      "START_RM, END_RM,",
                      "START_DATETIME, END_DATETIME,",
                      "EF_TOTAL_SECONDS",
                      "FROM FISH_T_SAMPLE",
                      "WHERE TRIP_ID IN (", trip.list, ")"),
                stringsAsFactors = FALSE, #import strings as character, not factor
                na.strings = c(NA, "", " ", 999, -999, "#N/A")) #these values are all NA

#antenna data has T on end of trip code cause was uploaded separately -
#but is same trip, so remove T
samples <- samples %>%
  mutate(TRIP_ID = str_remove(TRIP_ID, "[[:alpha:]]$"))


##### subset to only gear types we will analyze #####
#load gear type table
gear <- read.csv("./data/gear_types.csv", stringsAsFactors = FALSE)

gear <- gear %>%
  select(-n)

samples <- samples %>% #join gear type to sample data
  left_join(gear)

#one gear type is confusingly permanent and temporary antennas
#BK_UNBAITED is permanent if 141 and 127. Otherwise temporary
unique(samples$SAMPLE_TYPE[samples$GEAR_CODE == "BK_UNBAITED"])
samples <- samples %>%
  mutate(gear = case_when(GEAR_CODE == "BK_UNBAITED" &
                            SAMPLE_TYPE %in% c(127, 141) ~ "antenna permanent",
                          TRUE ~ gear))

#subset to only the gear types we are analyzing
samples <- samples %>%
  filter(gear %in% c("boat electrofishing", "baited hoop net",
                     "unbaited hoop net", "antenna temporary"))

### recode tributary samples near mouth as mainstem ######

#for each trip, bin into 5 miles (counting from dam) ######
#define length of reach to bin samples in to
reach.km = 8
#convert river mile to kilometer
samples <- samples %>%
  mutate(start_rkm = MileToKmCOR(START_RM), #calculate km from mile
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

#subset to spring and fall only seasons #####
# Spring: March to June
# fall: August to October
#create day variable (year is always the same, set to 2019)
samples <- samples %>%
  mutate(day = as.Date(paste0("2020",
                                 substr(as.character(START_DATETIME), 5, 10))))

samples <- samples %>%
  mutate(season = case_when(day >= as.POSIXct("2020-03-01") &
                              day <= as.POSIXct("2020-06-20") ~ "spring",
                            day >= as.POSIXct("2020-08-01") &
                              day <= as.POSIXct("2020-10-31") ~ "fall"))

samples <- samples %>%
  filter(season %in% c("spring", "fall"))


# calculate days of effort for antennas ########

#load all fish antenna data (maybe save an antenna copy from other script)

#classify as permanent/temporary

#collapse by Sample id or sample day (may n

#calculate effort per 5 miles per time block for each gear type #######
#    el sites
#    baited hoops
#    unbaited hoops
#    overnight antennas

n.samples <- samples %>%
  group_by(TRIP_ID, gear, season) %>%
  summarize(n = n())

n.samples <- n.samples %>%
  pivot_wider(names_from = gear, values_from = n,
              values_fill = list(n = 0)) #fill with zero if is NA

#add season and year
n.samples <- n.samples %>%
  mutate(year = as.numeric(substr(TRIP_ID, 3, 6)))

#when done, check that every fish trip/location/time has a corresponding
#sample/effort record


