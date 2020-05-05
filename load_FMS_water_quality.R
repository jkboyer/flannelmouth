################################
# Temperature and turbidity data from USGS stream gauges in Grand Canyon
# used as covariates in the FMS Multi-state model. Subset to 01/01/2004 to present (5/2020)
# with raw data.Further subset in code to defined spring
# (March 1 - June 20) and fall (August 1 - Oct 31) data for each year.
# Note that some stream gauges were not operational in 2004, so may be missing
# from the data.Earliest turbidity data for some gauges starts in 2005.
#
# Stream gauge data from:
#Dam:        RM -16  09379901
#Lees Ferry: RM 0    09380000
#30 Mile:    RM 30   09383050
#Above LCR:  RM 61   09383100
#LCR        RKM 1    09402300 No turbidity data
#Below LCR:  RM 66   09402352 No turbidity data
#Phantom:    RM 88   09402500
#127:        RM 128  09403270 Temp data begins in 2006; No turbidity data
#National:   RM 167  09404120
#Diamond:    RM 225  09404200
#spencer:    RM 246  09404220 No turbidity data
#
# Questions: what do we want to do about LCR turbidity?
#
# Output: Temperature_Turbidity.csv
########################################################################

library(lubridate)
library(tidyverse)
theme_set(theme_minimal())

#load file
file.names <- list.files(
  path ="./data/Water_quality_stream_gauge/temperature")

#Create list of data frame names without the ".csv" part
gauge.names <- gsub(".tsv", "", file.names)


#Load all files
for(i in gauge.names){
  col.names <- c("DateTime", "Temperature.C", "Turbidity.FNU")
  filepath <- file.path(
    "./data/Water_quality_stream_gauge/temperature",paste0(i, ".tsv"))
  assign(i, read.delim(filepath,
                       col.names = c("DateTime", "Temperature.C", "Turbidity.FNU"),
                       stringsAsFactors = FALSE))
}

#give data names
AboveLCR$Site.Name <- "Above_LCR"
BelowLCR$Site.Name <- "Below_LCR"
Diamond$Site.Name <- "Diamond"
GlenCanyonDam$Site.Name <- "Glen_Canyon_Dam"
LF$Site.Name <- "Lee's_Ferry"
National$Site.Name <- "National"
Phantom$Site.Name <- "Phantom"
rm127$Site.Name <- "127-mile"
rm30$Site.Name <- "30-mile"
Spencer$Site.Name <- "Spencer"
LCR$Site.Name <- "LCR"

#join data from all gauges
temp.actual <- bind_rows(AboveLCR, BelowLCR, Diamond, GlenCanyonDam,
                         LF, National, Phantom, rm127, rm30, Spencer, LCR)

#give data river miles - except LCR!
rm(AboveLCR, BelowLCR, Diamond, GlenCanyonDam,
   LF, National, Phantom, rm127, rm30, Spencer, LCR)


miles <- data.frame(Site.Name = c("Glen_Canyon_Dam", "Lee's_Ferry", "30-mile",
                                  "Above_LCR", "Below_LCR", "Phantom", "127-mile",
                                  "National", "Diamond", "Spencer", "LCR"),
                    RiverMile = c(-16, 0, 30,
                                  61, 66, 88, 128,
                                  167, 225, 246, NA))

temp.actual <- right_join(temp.actual, miles)

#add rkm column and give rkm to LCR
temp.actual["RiverKM"] <- "NA"
temp.actual$RiverKM <- as.numeric(temp.actual$RiverKM)
temp.actual <- temp.actual %>%
  mutate(RiverKM = case_when(Site.Name == "LCR" ~ 1,
                              TRUE ~ RiverKM))

#USGS gauges use -999 for NA - WHY?!
#fix that terribleness
temp.actual$Temperature.C <- ifelse(temp.actual$Temperature.C == -999, NA, temp.actual$Temperature.C)
temp.actual$Turbidity.FNU <- ifelse(temp.actual$Turbidity.FNU == -999, NA, temp.actual$Turbidity.FNU)

#aggregate to daily temperature and turbidity means
temp.turb.daily <- temp.actual %>%
  mutate(DateTime = as.POSIXct(DateTime),
         Date = as.Date(DateTime)) %>%
  group_by(Date, Site.Name, RiverMile, RiverKM) %>%
  summarize(Temperature.C.Daily = mean(Temperature.C, na.rm = TRUE),
            Turbidity.FNU.Daily = mean(Turbidity.FNU, na.rm = TRUE))

temp.turb.daily$Temperature.C.Daily <- ifelse(temp.turb.daily$Temperature.C.Daily == "NaN", NA,
                                              temp.turb.daily$Temperature.C.Daily)

temp.turb.daily$Turbidity.FNU.Daily <- ifelse(temp.turb.daily$Turbidity.FNU.Daily == "NaN", NA,
                                         temp.turb.daily$Turbidity.FNU.Daily)

rm(temp.actual)

#graphs to look at completeness (commented out so if run code - it doesn't take forever)
# temp1 <- temp.turb.daily %>%
#   ggplot(aes(x = Date, y = Temperature.C.Daily, color = RiverMile)) +
#   geom_line(alpha = 0.3)
#
# temp1
#
# temp2 <- temp.turb.daily %>%
#   ggplot(aes(x = Date, y = Temperature.C.Daily)) +
#   geom_line() +
#   facet_wrap(~RiverMile)
#
# temp2
#
# turb1 <- temp.turb.daily %>%
#   ggplot(aes(x = Date, y = Turbidity.FNU.Daily, color = RiverMile)) +
#   geom_line(alpha = 0.3)
#
# turb1
#
# turb2 <- temp.turb.daily %>%
#   ggplot(aes(x = Date, y = Turbidity.FNU.Daily)) +
#   geom_line() +
#   facet_wrap(~RiverMile)
#
# turb2

#looks complete - I can't zoom in on graphs because my computer is slow.

#no summer or winter data needed: subset to spring (March 1 - June 20) and fall (August 1 - Oct 31)
#define season cutoffs - %j is day of year and will be same day regardless of year
#year is defined as 2000 since we were having problems with date cutoffs
start.spring <- strftime(as.POSIXct("2000-03-01"))
end.spring <- strftime(as.POSIXct("2000-06-20"))
start.fall <- strftime(as.POSIXct("2000-08-01"))
end.fall <- strftime(as.POSIXct("2000-10-31"))

#filter out for spring or fall data only; create season and year column
temp.turb.daily <- temp.turb.daily %>%
  mutate(Day = as.POSIXct(paste0("2000", #assign same year to all
                               #keep the month-day-time part
                               substr(as.character(Date), 5, 19)))) %>%
  filter((Day >= start.spring & Day <= end.spring) | #spring or fall only
           (Day >= start.fall & Day <= end.fall))%>%
  #add variable for season
  mutate(Season = case_when(Day >= start.spring & Day <= end.spring ~ "spring",
                            Day >= start.fall & Day <= end.fall ~ "fall")) %>%
  #add variable for year
  mutate(Year = substr(as.character(Date), 1, 4))

# Remove day column
temp.turb.daily <- temp.turb.daily %>%
    select (-c(Day))

#write csv
write.csv(temp.turb.daily,"./data/Temperature_Turbidity.csv",
          row.names = FALSE)



