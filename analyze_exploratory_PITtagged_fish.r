# summaries of sample size of PIT tagged flannelmouth sucker, grouped
# by year, river, project, tag type, gear type, etc.
# use to inform decisions about what data we want to use in model
#Author: Jan Boyer, AGFD, jboyer@azgfd.gov
#Inputs: ./data/all_PIT_tagged_flannelmouth.csv
#Outputs: none
#Dependencies: none

require(tidyverse)
theme_set(theme_minimal(base_size = 14)) #prettier plot defaults


#load data
fms <- read.csv("./data/all_PIT_tagged_flannelmouth.csv", stringsAsFactors = FALSE)
colnames(fms)

#classify each fish as a recapture (multiple records) or not (only seen once)
n_captures <- fms %>%
  group_by(PITTAG) %>%
  summarize(n_captures = n(),
            recapture = case_when(n_captures > 1 ~ "Y",
                                  n_captures == 1 ~ "N"))

fms <- fms %>%
  left_join(n_captures)

# classify tags as old or new
#old tags have 10 characters, new tags have 14
#any other number is a typo, or a tag missing some characters
fms <- fms %>%
  mutate(tag_length = nchar(PITTAG),
         tag_type = case_when(tag_length == 10 ~ "old tag",
                              tag_length == 14 ~ "new tag")) %>%
  select(-tag_length)

#NEED TO FIX: about 20 new tags missing . (3DD003BFCE6CD)
#             go back to load file and correct

fms %>% #what size fish were they tagging?
  group_by(year) %>%
  summarize(min = min(TL, na.rm = TRUE),
            mean = mean(TL, na.rm = TRUE),
            max = max(TL, na.rm = TRUE)) %>%
  print(n = Inf)

#subset to recaptured fish only
fms <- fms %>%
  filter(recapture == "Y") %>%
  #subset to since 2004
  filter(year >= 2004)

# missing data ############

#How many records are missing river mile data
missing.rm <- fms %>%
  filter(is.na(START_RM))

missing.rm %>%
  group_by(RIVER_CODE) %>%
  summarize(n = n())
#only 366 in mainstem
#LCR records are ok because they probably have a kilometer

#how many records are missing length data
missing.length <- fms %>%
  filter(is.na(TL))

missing.length %>%
  group_by(RIVER_CODE) %>%
  summarize(n = n())
#good, only 25 don't have length data

# summaries - n fish by tributary, year, type of tag, gear, project, etc. ######

#how many old tags vs. new tags
fms %>%
  group_by(tag_type) %>%
  summarize(n = n())

#how many in each river
fms %>%
  group_by(RIVER_CODE) %>%
  summarize(n = n()) %>%
  arrange(-n)

fms %>% #by year
  group_by(year) %>%
  summarise(n = n()) %>%
  print(n = Inf)

fms %>% #plot, by year
  ggplot(aes(x = year)) +
  geom_bar()

fms %>% #plot, by year and project
  ggplot(aes(x = year, fill = factor(SAMPLE_TYPE))) +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_bar()

fms %>% #plot, by year and tag type
  ggplot(aes(x = year, fill = tag_type)) +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_bar()

fms %>% #HOW LONG were trammel nets used
  filter(GEAR_CODE %in% c("TK", "T50", "T75", "TL", "TM", "TN", "TP", "TRA",
                          "TS", "TY", "TZ")) %>%
  ggplot(aes(x = year)) +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_bar()
#until 2014, which means we have to deal with them and calculating effort

#by project
fms %>%
  group_by(SAMPLE_TYPE) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  print(n = Inf)

#by gear type
fms %>%
  group_by(GEAR_CODE) %>%
  summarise(n = n(),
            percent = 100*(n/nrow(fms))) %>%
  arrange(-n) %>%
  print(n = Inf)

#by river mile (Colorado only, excludes tributaries)
fms %>%
  filter(RIVER_CODE == "COR") %>%
  ggplot(aes(x = START_RM, fill = factor(SAMPLE_TYPE))) +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_histogram()
#obviously lots more sampling effort at LCR, JCM-west, and bridge city
#USFWS aggregation (128) and AGFD (99) have most even catch geographically

fms %>%
  filter(RIVER_CODE == "COR") %>%
  ggplot(aes(x = START_RM, fill = factor(year))) +
  scale_fill_viridis_d() +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_histogram()
#western canyon data mostly from recent years
#most older data is near LCR or RM 150
#what is at RM 150?
#subsetting to recent years would be a good way to ensure more even sampling
#geographically, avoid excessive LCR sampling from skewing results

fms %>%
  filter(RIVER_CODE == "COR") %>%
  ggplot(aes(x = START_RM, fill = tag_type)) +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_histogram()


#by time of year
fms <- fms %>%
  mutate(day = as.POSIXct(paste0("2019",
                                substr(as.character(START_DATETIME), 5, 10))),
         day)

fms %>%
  ggplot(aes(x = day)) +
  geom_histogram(bins = 52) +
  scale_x_datetime(date_breaks = "15 days", date_labels = "%b %d") +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 90))

fms %>%
  ggplot(aes(x = day)) +
  geom_bar() +
  scale_x_datetime(date_breaks = "3 days",
                   date_labels = "%b %d",
                   limits = c(as.POSIXct("2019-10-01 00:00:00"),
                              as.POSIXct("2019-11-15 00:00:00")))+
  theme(axis.text.x = element_text(angle = 90))

