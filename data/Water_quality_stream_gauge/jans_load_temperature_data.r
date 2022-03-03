#TESTING prediction on smaller subset of data
#go back and replace with full dataset once done

#loads temperature and discharge data

library(tidyverse)
theme_set(theme_minimal())

#load data ######
#Dam:        RM -16
#Lees Ferry: RM
#30 Mile:    RM
#Above LCR:  RM
#Below LCR:  RM 66
#Phantom:    RM
#127:        RM
#National:   RM
#Diamond:    RM 225  09404200
#spencer:    RM 246  09404220

#
file.names <- list.files(path="./data/water_quality_stream_gauge/temperature/")

#Create list of data frame names without the ".csv" part
gauge.names <- gsub(".tsv", "", file.names)


#Load all files
for(i in gauge.names){
  col.names <- c("datetime", "temp.c")
  filepath <- file.path("./data/water_quality_stream_gauge/temperature/", paste0(i, ".tsv"))
  assign(i, read.delim(filepath,
                       col.names = c("datetime", "temp.c"),
                       stringsAsFactors = FALSE))
}

#give data names
aboveLCR$site.name <- "aboveLCR"
belowLCR$site.name <- "belowLCR"
diamond$site.name <- "diamond"
glencanyondam$site.name <- "glencanyondam"
leesferry$site.name <- "leesferry"
national$site.name <- "national"
phantom$site.name <- "phantom"
rm127$site.name <- "rm127"
rm30$site.name <- "rm30"
spencer$site.name <- "spencer"
LCR$site.name <- "LCR"

#join data from all gauges
temp.actual <- bind_rows(aboveLCR, belowLCR, diamond, glencanyondam,
                     leesferry, national, phantom, rm127, rm30, spencer, LCR)
#remove turbidity
temp.actual <- temp.actual %>%
  select(datetime, site.name, temp.c)

#give data river miles
rm(aboveLCR, belowLCR, diamond, glencanyondam,
      leesferry, national, phantom, rm127, rm30, spencer, LCR)

miles <- data.frame(site.name = c("glencanyondam", "leesferry", "rm30",
                             "aboveLCR", "belowLCR", "phantom", "rm127",
                             "national", "diamond", "spencer"),
                    rivermile = c(-16, 0, 30,
                                  61, 66, 88, 128,
                                  167, 225, 246))

temp.actual <- temp.actual %>%
  left_join(miles)

#USGS gauges use -999 for NA - WHY?!
#fix that terribleness
temp.actual$temp.c <- ifelse(temp.actual$temp.c == -999, NA, temp.actual$temp.c)
temp.actual$temp.c <- ifelse(temp.actual$temp.c == 999, NA, temp.actual$temp.c)

#aggregate to daily means
temp.daily <- temp.actual %>%
  mutate(datetime = as.POSIXct(datetime),
         date = as.Date(datetime)) %>%
  group_by(date, site.name, rivermile) %>%
  summarize(temp.c.daily = mean(temp.c, na.rm = TRUE))

temp.daily$temp.c.daily <- ifelse(temp.daily$temp.c.daily == "NaN", NA,
                                  temp.daily$temp.c.daily)

rm(temp.actual)

#graph to look at completeness
temp.daily %>%
  ggplot(aes(x = date, y = temp.c.daily, color = rivermile)) +
  geom_line(alpha = 0.3)

temp.daily %>%
  ggplot(aes(x = date, y = temp.c.daily)) +
  geom_line() +
  facet_wrap(~rivermile)
#looks very complete - yay!

#for all locations between gauges, simply graph
temp.daily %>%
  filter(date == as.Date("2019-08-04")) %>%
  ggplot(aes(x = rivermile, y = temp.c.daily)) +
  geom_line() +
  geom_point() +
  coord_cartesian(xlim = c(-16, 281), ylim = c(0, 22))

#find last date to subset data
temp.daily %>%
  group_by(rivermile) %>%
  summarize(last.date = max(date)) %>%
  arrange(last.date)
#just remove 2021-22
#NOTE: NA values are fine, they are LCR which does not have a COR river mile

temp.daily <- temp.daily %>%
  filter(date < as.Date("2021-01-01"))

#to estimate temperature at pearce, assume same slope as diamond to spencer
#This is actually below Pearce, to make sure all is included
pearce <- temp.daily %>%
  filter(rivermile %in% c(167, 225, 246)) %>%
  ungroup() %>%
  select(-rivermile) %>%
  pivot_wider(names_from = site.name, values_from = temp.c.daily) %>%
#estimate temp at pearce from spencer and diamond gauges
  mutate(pearce = case_when(!is.na(spencer) ~
        spencer + ((spencer - diamond)/(246 - 225))*(281 - 246),
#spencer gauge was offline for some of 2018.
#for those months, estimate pearce from diamond
          is.na(spencer) ~
            diamond + ((diamond - national)/(225 - 167))*(281 - 226))) %>%
  select(date, pearce) %>%
  transmute(date = date,
            temp.c.daily = pearce,
            rivermile = 281,
            site.name = "pearceferry")

below.pearce <- temp.daily %>%
  filter(rivermile %in% c(167, 225, 246)) %>%
  ungroup() %>%
  select(-rivermile) %>%
  pivot_wider(names_from = site.name, values_from = temp.c.daily) %>%
  #estimate temp at pearce from spencer and diamond gauges
  mutate(belowpearce = case_when(!is.na(spencer) ~
                              spencer + ((spencer - diamond)/(246 - 225))*(295 - 246),
                            #spencer gauge was offline for some of 2018.
                            #for those months, estimate pearce from diamond
                            is.na(spencer) ~
                              diamond + ((diamond - national)/(225 - 167))*(295 - 226))) %>%
  select(date, belowpearce) %>%
  transmute(date = date,
            temp.c.daily = belowpearce,
            rivermile = 295,
            site.name = "belowpearce")

#join pearce ferry estimates to other data
temp.daily <- temp.daily %>%
  bind_rows(pearce)

temp.daily <- temp.daily %>%
  bind_rows(below.pearce)

#convert rm to rkm
library(grandcanyonfish)
temp.daily <- temp.daily %>%
  mutate(rkm = mile_to_km_COR(rivermile)) %>%
  select(-rivermile)


write.csv(temp.daily, "./data/temperature.csv", row.names = FALSE)


