#Loads temperature data from files for each GCMRC gauge
#binds into one file, saves
#author: Jan Boyer, jboyer@azgfd.gov
#inputs: 2016-2021 data (water temperature and datetime) from the glen canyon
#        dam, lees ferry, 30 mile, above LCR, above Lava Chuar, Phantom, 127 mile,
#        national, diamond, and spencer gauges maintained by USGS-GCMRC
#        https://www.gcmrc.gov/discharge_qw_sediment/stations/GCDAMP
#outputs: ./data/clean/gcmrc_gauge_temperature.csv
#        all locations aggregated into one file.


library(tidyverse)
library(directlabels)
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

#load all files in .data/raw/temperature folder
file.names <- list.files(path = "./data/raw/temperature")

#Create list of data frame names without the ".csv" part
gauge.names <- gsub(".tsv", "", file.names)

#Load all files
for(i in gauge.names){
  col.names <- c("datetime", "temp.c")
  filepath <- file.path("./data/raw/temperature", paste0(i, ".tsv"))
  assign(i, read.delim(filepath,
                       col.names = c("datetime", "temp.c"),
                       stringsAsFactors = FALSE))
}

#add column with site name
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

#join data from all gauges
temp.actual <- bind_rows(aboveLCR, belowLCR, diamond, glencanyondam,
                         leesferry, national, phantom, rm127, rm30, spencer)

rm(aboveLCR, belowLCR, diamond, glencanyondam, #no longer needed, remove
   leesferry, national, phantom, rm127, rm30, spencer)

#give data river miles
miles <- data.frame(site.name = c("glencanyondam", "leesferry", "rm30",
                                  "aboveLCR", "belowLCR", "phantom", "rm127",
                                  "national", "diamond", "spencer"),
                    rivermile = c(-16, 0, 30,
                                  61, 66, 88, 128,
                                  167, 225, 246))

temp.actual <- right_join(temp.actual, miles)

#USGS gauges use -999 for NA - WHY?!
#fix that terribleness
temp.actual$temp.c <- ifelse(temp.actual$temp.c == -999, NA, temp.actual$temp.c)

#aggregate to daily mean temperature
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
  ggplot(aes(x = date, y = temp.c.daily, color = site.name)) +
  geom_line()

#look at 2021 completeness
temp.daily %>%
  filter(date >= as.Date("2021-01-01")) %>%
  ggplot(aes(x = date, y = temp.c.daily, color = site.name)) +
  geom_line() +
  geom_dl(aes(label = site.name), method = "last.points") +
  theme(legend.position = "none")
# 3 sites were last downloaded in march or feb 2021, no data for april 2021
#should be ok to extrapolate from nearest gauges when one is missing.

#to estimate temperature at pearce, assume same slope as diamond to spencer
pearce <- temp.daily %>%
  filter(rivermile %in% c(167, 225, 246)) %>%
  ungroup() %>%
  select(-rivermile) %>%
  pivot_wider(names_from = site.name, values_from = temp.c.daily) %>%
  #estimate temp at pearce from spencer and diamond gauges
  mutate(pearce = case_when(!is.na(spencer) ~
                              spencer + ((spencer - diamond)/(246 - 225))*(282 - 246),
                            #spencer gauge was offline for some of 2018.
                            #for those months, estimate pearce from diamond
                            is.na(spencer) ~
                              diamond + ((diamond - national)/(225 - 167))*(282 - 226))) %>%
  select(date, pearce) %>%
  transmute(date = date,
            temp.c.daily = pearce,
            rivermile = 282,
            site.name = "pearceferry")

#join pearce ferry estimates to other data
temp.daily <- temp.daily %>%
  bind_rows(pearce) %>%
  mutate(data.type = case_when(rivermile == 282 ~ "Estimated",
                               TRUE ~ "Measured"))

#add year, month, day of year
temp.daily <- temp.daily %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m")),
         day.of.year = as.Date(paste0("2000-", format(date, "%m-%d"))),
         id = paste(site.name, date))

#save temperature data
write.csv(temp.daily, "./data/clean/gcmrc_gauge_temperature.csv",
          row.names = FALSE)


################################################



#predict from river mile and date.
hoops <- read_csv("./data/clean/hoop_locations.csv")

hoops <- hoops %>%
  mutate(mile.date = paste(START_DATE, START_RM)) %>%
  unique()

str(hoops)
str(temp.daily)

#temp tiny dataframe to test loop on
#hoops <- hoops[seq(1, 1050, by = 50),]

#loop to calculate temperature
#vector of unique nets and dates
net.miles <- hoops$START_RM
net.dates <- hoops$START_DATE

#blank df to store values created by loop
net.temperatures <- data.frame(START_RM = as.numeric(),
                               START_DATE = as.Date(as.character()),
                               water.temp = as.numeric())

#must arrange temperature df by river mile to use findInterval() correctly
temp.daily <- temp.daily %>%
  arrange(rivermile)


for (i in seq_along(net.miles)) {

  print(net.miles[[i]]) #print net location/date
  print(net.dates[[i]])
  #1.find closest upstream and closest downstream gauge on given date
  #make temp df subset to that date
  temperature <- temp.daily %>%
    filter(date == net.dates[i])

  #make temp upstream gauge df
  gauges <- data.frame(up = temperature$rivermile,
                       down = lead(temperature$rivermile))
  upstream.gauge <- gauges$up[findInterval(net.miles[i], gauges$up)]
  upstream <- temperature %>%
    filter(rivermile == upstream.gauge)
  #make temp downstream gauge df
  downstream.gauge <- gauges$down[findInterval(net.miles[i], gauges$up)]
  downstream <- temperature %>%
    filter(rivermile == downstream.gauge)

  #2. calculate temperature
  #temp = temp.us.gauge +
  #            ((temp.ds.gauge - temp.us.gauge)/
  #             (rm.ds.gauge - rm.us.gauge)*(hoop.rm - rm.us.gauge)
  predict.temp <- upstream$temp.c.daily +
    ((downstream$temp.c.daily - upstream$temp.c.daily)/
       (downstream$rivermile -
          upstream$rivermile))*(net.miles[i] - upstream$rivermile)

  START_RM = net.miles[i]
  START_DATE = net.dates[i]


  temp.i <- data.frame(START_RM,
                       START_DATE,
                       water.temp = round(predict.temp, 2))

  net.temperatures <- bind_rows(net.temperatures, temp.i) #bind onto one dataframe

}
