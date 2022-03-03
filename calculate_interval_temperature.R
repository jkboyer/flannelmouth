# Calculate mean temperature in each interval and reach
library(tidyverse)
library(grandcanyonfish)
library(data.table) #for day of year function


#define list of reaches
#define list of time intervals
#   and dates within each interval
#calculate temperature on each date and reach (using existing loop)
#calculate mean by summarizing by reach/interval
#Also will split by LCR/COR like other temperature calculations.
reaches <- read_csv("./data/reaches_8km.csv")

all.dates <- seq(as.Date("2004-05-02"), as.Date("2020-09-23"), by = 1)

#fms <- read.csv("./data/all_PIT_tagged_flannelmouth.csv")
#n.samples <- read_csv("./data/effort_by_trip_reach.csv")

#calculate reach midpoint
#use this to calculate temperature
reaches <- reaches %>%
  mutate(reach_middle = if_else(RIVER_CODE == "COR", reach_start + 4, NA_real_))

temp.daily <- read_csv("./data/temperature.csv")

#split into COR and LCR
reaches.LCR <- reaches %>%
  filter(RIVER_CODE == "LCR")
reaches.BAC <- reaches %>%
  filter(RIVER_CODE == "BAC")
reaches <- reaches %>%
  filter(RIVER_CODE == "COR")

reaches <- reaches %>%
  arrange(reach_start)

#temp tiny dataframe to test loop on
#n.samples <- n.samples[seq(2, 1051, by = 50),]

#loop to calculate temperature
#vector of unique nets and dates
reach.midpoints <- reaches$reach_middle
all.dates <- all.dates

#dataframe with all possibilities
all.df <- data.frame(reach.midpoints = rep(reach.midpoints, each = length(all.dates)),
  date = rep(all.dates, length(reach.midpoints)))

reach.midpoints <- all.df$reach.midpoints
all.dates <- all.df$date


#temp tiny vectors to test loop on
#reach.midpoints <- reach.midpoints[seq(2, 300001, by = 1000)]
#all.dates <- all.dates[seq(seq(2, 300001, by = 1000))]

#sample.dates = all.dates
#sample.kms = reach.midpoints
#sample.temperatures = midpoint.temperatures

#blank df to store values created by loop
midpoint.temperatures <- data.frame(reach_middle = as.numeric(),
                                  sampling.date = as.Date(as.character()),
                                  water.temp = as.numeric())

#must arrange temperature df by river mile to use findInterval() correctly
temp.daily <- temp.daily %>%
  arrange(rkm)

#I think now it is set up to get just one date, need to do all dates
for (i in seq_along(reach.midpoints)) {

  print(reach.midpoints[[i]]) #print net location/date
  #1.find closest upstream and closest downstream gauge on given date
  #make temp df subset to that date
  temperature <- temp.daily %>%
    filter(date == all.dates[i] & site.name != "LCR")

  #make temp upstream gauge df
  gauges <- data.frame(up = temperature$rkm,
                       down = lead(temperature$rkm))
  upstream.gauge <- gauges$up[findInterval(reach.midpoints[i], gauges$up)]
  upstream <- temperature %>%
    filter(rkm == upstream.gauge)

  #make temp downstream gauge df
  downstream.gauge <- gauges$down[findInterval(reach.midpoints[i], gauges$up)]
  downstream <- temperature %>%
    filter(rkm == downstream.gauge)

  #2. calculate temperature
  #temp = temp.us.gauge +
  #            ((temp.ds.gauge - temp.us.gauge)/
  #             (rm.ds.gauge - rm.us.gauge)*(hoop.rm - rm.us.gauge)
  predict.temp <- upstream$temp.c.daily +
    ((downstream$temp.c.daily - upstream$temp.c.daily)/
       (downstream$rkm -
          upstream$rkm))*(reach.midpoints[i] - upstream$rkm)

  reach_middle = reach.midpoints[i]
  sampling.date = as.Date(all.dates[i])


  temp.i <- data.frame(reach_middle,
                       sampling.date,
                       water.temp = round(predict.temp, 2))

  midpoint.temperatures <- bind_rows(midpoint.temperatures, temp.i) #bind onto one dataframe

}

#write.csv(midpoint.temperatures, "./data/midpoint_temperatures.csv",
 #         row.names = FALSE)
midpoint.temperatures <- read_csv("./data/midpoint_temperatures.csv")

midpoint.temperatures <- midpoint.temperatures %>%
  left_join(reaches)

#join on LCR data
lcr.temp <- temp.daily %>%
  filter(site.name == "LCR") %>%
  transmute(sampling.date = date,
         water.temp = temp.c.daily,
         RIVER_CODE = site.name) %>%
  filter(sampling.date %in% all.dates) %>%
  left_join(reaches.LCR)

midpoint.temperatures <- midpoint.temperatures %>%
  bind_rows(lcr.temp)


#Assign interval variable
midpoint.temperatures <- midpoint.temperatures %>%
  mutate(doy = yday(sampling.date),
         year = as.numeric(format(sampling.date, format = "%Y")))

midpoint.temperatures <- midpoint.temperatures %>%
  mutate(interval_start = case_when(
    doy < 122 ~ paste0((year - 1), "-09-23"),
    doy >= 122 & doy < 266 ~ paste0((year), "-05-02"),
    doy >= 266 ~ paste0((year), "-09-23")))

unique(midpoint.temperatures$interval_start)

midpoint.temperatures %>%
  filter(reach_middle == 4 & year > 2015) %>%
ggplot( aes(x = sampling.date, y = interval_start)) +
  geom_point()


#group by reach, interval and calculate mean
interval.temperatures <- midpoint.temperatures %>%
  group_by(RIVER_CODE, reach_no, reach, reach_start, interval_start) %>%
  summarise(temp_interval = mean(water.temp, na.rm = TRUE))

write.csv(interval.temperatures, "./data/mean_temperature_by_reach_and_time_interval.csv",
          row.names = FALSE)




