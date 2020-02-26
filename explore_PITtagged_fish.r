#explore pit tag data
require(tidyverse)

fms <- read.csv("./data/FMS_MarkRecap_20200214.csv", stringsAsFactors = FALSE)

colnames(fms)

tribs <- fms %>%
  group_by(river_code) %>%
  summarize(n = n())

fms %>%
  filter(river_code != "" & !is.na(river_code) & river_code != "COR") %>%
  ggplot(aes(x = river_code)) +
  geom_bar() +
  facet_wrap(~year)

missing.rm <- fms %>%
  filter(!is.na(pittag) & pittag != "" & is.na(start_rm))

missing.rm %>%
  group_by(river_code) %>%
  summarize(n = n())

fms.year <- fms %>%
  filter(!is.na(pittag) & pittag != "") %>%
  group_by(year) %>%
  summarise(n = n())

ggplot(fms.year, aes(x = year, y = n)) +
  geom_col()
