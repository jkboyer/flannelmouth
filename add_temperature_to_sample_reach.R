# Add temperature data to FMS data
#calculate Mean spring and fall date
library(tidyverse)

#fms <- read.csv("./data/all_PIT_tagged_flannelmouth.csv")

n.samples <- read_csv("./data/effort_by_trip_reach.csv")

#n.samples.trip <- read.csv("./data/effort_by_trip.csv")

temp.daily <- read_csv("./data/temperature.csv")

#split into COR and LCR
n.samples.LCR <- n.samples %>%
  filter(RIVER_CODE == "LCR")
n.samples.BAC <- n.samples %>%
  filter(RIVER_CODE == "BAC")
n.samples <- n.samples %>%
  filter(RIVER_CODE == "COR")

n.samples <- n.samples %>%
  arrange(reach_start)


#temp tiny dataframe to test loop on
#n.samples <- n.samples[seq(2, 1051, by = 50),]

#loop to calculate temperature
#vector of unique nets and dates
sample.kms <- n.samples$reach_start
sample.dates <- n.samples$sampling.date

#blank df to store values created by loop
sample.temperatures <- data.frame(reach_start = as.numeric(),
                               sampling.date = as.Date(as.character()),
                               water.temp = as.numeric())

#must arrange temperature df by river mile to use findInterval() correctly
temp.daily <- temp.daily %>%
  arrange(rkm)


for (i in seq_along(sample.kms)) {

  print(sample.kms[[i]]) #print net location/date
  print(sample.kms[[i]])
  #1.find closest upstream and closest downstream gauge on given date
  #make temp df subset to that date
  temperature <- temp.daily %>%
    filter(date == sample.dates[i] & site.name != "LCR")

  #make temp upstream gauge df
  gauges <- data.frame(up = temperature$rkm,
                       down = lead(temperature$rkm))
  upstream.gauge <- gauges$up[findInterval(sample.kms[i], gauges$up)]
  upstream <- temperature %>%
    filter(rkm == upstream.gauge)

  #make temp downstream gauge df
  downstream.gauge <- gauges$down[findInterval(sample.kms[i], gauges$up)]
  downstream <- temperature %>%
    filter(rkm == downstream.gauge)

  #2. calculate temperature
  #temp = temp.us.gauge +
  #            ((temp.ds.gauge - temp.us.gauge)/
  #             (rm.ds.gauge - rm.us.gauge)*(hoop.rm - rm.us.gauge)
  predict.temp <- upstream$temp.c.daily +
    ((downstream$temp.c.daily - upstream$temp.c.daily)/
       (downstream$rkm -
          upstream$rkm))*(sample.kms[i] - upstream$rkm)

  reach_start = sample.kms[i]
  sampling.date = as.Date(sample.dates[i])


  temp.i <- data.frame(reach_start,
                       sampling.date,
                       water.temp = round(predict.temp, 2))

  sample.temperatures <- bind_rows(sample.temperatures, temp.i) #bind onto one dataframe

}

#bind temperatures to colorado data
n.samples <- n.samples %>%
  left_join(sample.temperatures)

#LCR - just assign temp from LCR gauge by date
lcr.temp <- temp.daily %>%
  filter(site.name == "LCR") %>%
  mutate(sampling.date = date,
         water.temp = temp.c.daily) %>%
  select(sampling.date, water.temp)

n.samples.LCR <- n.samples.LCR %>%
  left_join(lcr.temp)

#join LCR, COR, BAC back together
n.samples <- bind_rows(n.samples, n.samples.LCR, n.samples.BAC)

write.csv(n.samples, "./data/effort_by_trip_reach_with_temperature.csv",
          row.names = FALSE)




