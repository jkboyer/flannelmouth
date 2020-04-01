#Need to email brian and ask for shinumo sample/effort data

#Load effort data
library(tidyverse)
library(RODBC)

#load some metric/english conversion fuctions I wrote
source("./functions/conversion_functions.r")


#load fish data to see what trips we need effort data for #####
fms <- read.csv("./data/all_PIT_tagged_flannelmouth.csv", stringsAsFactors = FALSE)

trip.ids = unique(fms$TRIP_ID)
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

##### subset to only gear types we will analyze #####
#load gear type table
gear <- read.csv("./data/gear_types.csv", stringsAsFactors = FALSE)

gear <- gear %>%
  select(-n)

samples <- samples %>% #join gear type to sample data
  left_join(gear)  %>%
  #subset to only the gear types we are analyzing
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
         reach_no = as.numeric(reach_8km), #number each reach
         reach_start = (reach_8km_no-1)*reach.km, #start point
         reach_mid = reach_8km_start + reach.km/2, #mid point
         reach_end = reach_8km_no*reach.km) %>% #end point

  arrange(reach)

samples %>% #plot to see spatial distribution of samples
  ggplot(aes(x = reach_mid)) +
  geom_histogram()
#yep, theres a lot of sampling at the LCR


#calculate effort per 5 miles per time block for each gear type
#    el sites
#    baited hoops
#    unbaited hoops
#    overnight antennas

n.samples <- samples %>%
  group_by(TRIP_ID, gear) %>%
  summarize(n = n(),
            n.type = length(unique(SAMPLE_TYPE)))

n.samples.wide <- n.samples %>%
  pivot_wider(names_from = gear, values_from = n,
              values_fill = list(n = 0)) #fill with zero if is NA
