#Turbidity - find what adjustment is needed for siltbank areas
#  no gauges - need to look at AGFD measurements

#setup: load packages, data, define subsetting criteria ######
library(RODBC) #database interface
library(tidyverse)
library(lubridate) #need this for date transformation

theme_set(theme_minimal()) #override ugly default ggplot theme

#load some metric/english conversion fuctions I wrote
source("./functions/conversion_functions.r")

#year we started measuring NTUs
start.year <- 2014

# load data from big boy database ######
# database file name: UPDATE to most recent version here
db.GCMRC <- "FISH_SAMPLE_SPECIMEN_HISTORY_20200519_1537.mdb"

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
sqlColumns(db, "FISH_T_SAMPLE")$COLUMN_NAME

# query desired FMS data from specimen table
# this will take a while to run
ntu <- sqlQuery(db,
                paste("SELECT SAMPLE_TYPE, TRIP_ID,",
                      "START_DATETIME, RIVER_CODE, START_RM, TURBIDITY_NTU",
                      "FROM FISH_T_SAMPLE",
                      "WHERE (SAMPLE_TYPE = 99 OR SAMPLE_TYPE = 115)"),
                stringsAsFactors = FALSE, #import strings as character, not factor
                na.strings = c(NA, "", " ", 999, -999, "#N/A")) #these values are all NA

glimpse(ntu)

ntu <- ntu %>%
  filter(!is.na(TURBIDITY_NTU) &
           #remove zero values - impossible in Colorado, must be typos
           TURBIDITY_NTU > 0.001 &
           START_DATETIME >= as.POSIXct("2014-01-01 01:01:01")) %>%
  mutate(date = as.Date(substr(as.character(START_DATETIME), 1, 10)))
#collapse to unique dates and turbidity
#RM will be a mean (so, middle of that reach)
ntu <- ntu %>%
  group_by(date, TURBIDITY_NTU, TRIP_ID) %>%
  summarize(top_RM = min(START_RM),
            bottom_RM = max(START_RM),
            river_mile = round(median(START_RM), 1), #use median, this avoids errors in min or max
            length = bottom_RM - top_RM)

#figure - turbidity by rm, grouped by trip
ntu %>%
  ggplot(aes(x = river_mile, y = TURBIDITY_NTU, group = TRIP_ID)) +
  geom_point() +
  geom_line() +
  scale_y_log10()


ntu %>%
  ggplot(aes(x = river_mile, y = TURBIDITY_NTU, group = TRIP_ID)) +
  geom_point() +
  geom_line()


#lots of noise - is there some way to simplify?

#see what a loess curve looks like
ntu %>%
  ggplot(aes(x = river_mile, y = TURBIDITY_NTU)) +
  geom_point() +
  stat_smooth() +
  scale_y_log10()

ntu %>%
  ggplot(aes(x = river_mile, y = TURBIDITY_NTU)) +
  geom_point() +
  stat_smooth()

#bin by every 10 miles, take mean over multiple years?
#or maybe median would be better?

#look at how much more turbid it is at 255-280 compared to spencer(2

#when the Paria/LCR are going it won't matter (2000 vs. 2200 not important)
#so what I should really look at are comparisons when base turbidity is < 100
#RM will be a mean (so, middle of that reach)

ntu %>%
  ggplot(aes(x = river_mile, y = TURBIDITY_NTU, group = TRIP_ID)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 280))

#for turbidity purposes, the meaningfull reach breaks are:
#     Paria - LCR - start of glurp
#just calculate summary stats for each reach, and see how glurp reach
#   compares to LCR-spencer reach?
