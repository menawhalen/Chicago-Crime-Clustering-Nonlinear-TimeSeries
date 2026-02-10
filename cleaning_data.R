library(bigrquery)
library(wk)
library(tidyverse)
library(lubridate)
library(sf)
library(rgdal)
library(janitor)
library(tidygeocoder)
# bq_auth()

#####################################################################################################################################################################################################
### district level run 2/13/24 
### re run old code but collected all data to end of 2023

############################################################

## cleaned_guns_beat_full.rds and monthly_guns_rate.rds added to data folder on 2/13/24 going up till the end of 2023

homic_vic_name <- bq_table("n3-main", "cpd_anon", "homic_vics")
non_shoot_name <- bq_table("n3-main","cpd_anon","nonfat_shoot")

homicide <- bq_table_download(homic_vic_name)
nonfatal_shooting <- bq_table_download(non_shoot_name)

homic_vic <- homicide %>% 
  mutate(death_date = ymd(death_date),
         injury_date = ymd(injury_date),
         zip_code = factor(zip_code),
         district = as.numeric(district),
         beat = factor(beat))
non_shoot <- nonfatal_shooting %>% 
  mutate(date = ymd_hms(date, tz = "UTC")) %>% 
  #removed those without a date and long and lat (was 120)
  filter(!is.na(date) & !is.na(o_geometry))
## homicide goes from 1961 to end of 2021
# summary(homic_vic$death_date)
## nonshooting goes from 2010-2-2 to 12/29/2021
# summary(non_shoot$date)

## district was redraw in 2012 need to convert to modern districting
dist_map <- st_read(dsn="data/district_map/Police_District.shp") %>% 
  clean_names() %>% 
  mutate(district = str_sub(district, start = 2, end = 3)) %>% 
  select(district, geometry)
# ggplot(dist_map) + geom_sf(aes(fill = district)) + geom_sf_label(aes(label = district))
## has 22 districts but without beats
beat_map <- read_sf("data/PoliceBeatDec2012.csv", wkt = "the_geom") %>% 
  clean_names() %>% 
  st_as_sf(wkt = "the_geom")
# ggplot(beat_map) + geom_sf(aes(fill = district)) + geom_sf_label(aes(label = district))
## has 23 districts with 31 being the hole in ohare so can remove 31 district

## district 23 is completely 19 now 
## district 13 is completely 12 now
homic_vic_remap <- homic_vic %>% 
  filter(death_date >= ymd("2011-01-01")) %>% 
  mutate(district_remap = ifelse(district == 23, 19,
                                 ifelse(district == 13, 12, 
                                        ifelse(district == 21, 2, district))))
#### district 21 is split into 3 but is mainly district 2
## since cant use long and lat just put into 2
## only 22 instances
## look at zip codes but the changes are so small zip codes don't help


non_fatal_remap <- non_shoot %>% 
  filter(date >= ymd("2011-01-01")) %>% 
  st_as_sf(wkt = "o_geometry", crs = st_crs(beat_map)) %>% 
  st_join(beat_map, join = st_intersects) %>% 
  mutate(district_remap = as.numeric(ifelse(district.y == 23, 19,
                                            ifelse(district.y == 13, 12, district.y))))
#### there is no shooting in district 21


homicide_simple <- homic_vic_remap %>% 
  rename(date = death_date) %>% 
  select(rd_no, date, district_remap, beat) %>% 
  mutate(type = "homicide")

non_fatal_simple <- non_fatal_remap %>% 
  as_tibble() %>% 
  select(rd_no, date, district_remap, beat_num) %>% 
  rename(beat = beat_num) %>% 
  mutate(type = "non-fatal") 

full_guns <- rbind(homicide_simple, non_fatal_simple) %>% 
  mutate(type = factor(type)) %>% 
  rename(district = district_remap) %>% 
  #got rid of 32 NA's and one crime in side ohare
  filter(!is.na(district) & district != 31) 
table(full_guns$district)

write_rds(full_guns, "data/cleaned_guns_beat_full.rds")

## now need to aggregrate into levels of time and change to rates
## get population data
district_population_spatial <- readRDS("data/police_district_demo_2020.rds")

month_guns <- full_guns %>% 
  mutate(type = factor(type),
         district = factor(district)) %>% 
  group_by(month = floor_date(date, "month"), district) %>% 
  tally() %>% 
  complete(district, fill = list(n = 0)) %>% 
  left_join(district_population_spatial) %>% 
  mutate(rate = n/total *100000) %>% 
  select(month, district, n, rate)

write_rds(month_guns, "data/monthly_guns_rate.rds")

#####################################################################################################################################################################################################
### community level run 3/15/2024 
### updating old code to be run back in time from 2015- 2023

############################################################

### clean up raw crimes dataset
crimes_raw <- read_csv("data/chicago_crimes_raw_full.csv") %>% 
  clean_names() %>% 
  mutate(
    date = mdy_hms(date),
    iucr = as.factor(iucr),
    primary_type = as.factor(primary_type)
  )

crimes_raw %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  tally() %>% 
  ggplot(aes(year, n)) +
  geom_line()

## only a few collected for 2024 so why the large drop
## platue and drip around 2015

## collect from 2015-2023

crimes_raw_time <- crimes_raw %>% 
  filter(year(date) > 2014 & year(date) < 2024)

## get long and lat that are missing
long_lat_miss <- crimes_raw_time %>% 
  ## missing long and lat variables 
  filter(is.na(latitude) == T | is.na(longitude) == T) %>% 
  ## only looking at index crime
  filter(substr(iucr, start = 1, stop = 2) == "02"|
           substr(iucr, start = 1, stop = 2) == "03"|
           substr(iucr, start = 1, stop = 2) == "04"|
           substr(iucr, start = 1, stop = 2) == "05"|
           substr(iucr, start = 1, stop = 2) == "06"|
           substr(iucr, start = 1, stop = 2) == "08"|
           substr(iucr, start = 1, stop = 2) == "09"|
           substr(iucr, start = 1, stop = 2) == "10") %>% 
  mutate(address = paste0(block, ", CHICAGO, IL"))

replace_long_lat <- long_lat_miss %>%
  tidygeocoder::geocode(address = address, method = "arcgis")
## 7190 addresses looked up (only 0.3% missing so not to big a deal)
crimes_raw_time[crimes_raw_time$id %in% replace_long_lat$id, "longitude"] <- replace_long_lat$long
crimes_raw_time[crimes_raw_time$id %in% replace_long_lat$id, "latitude"] <- replace_long_lat$lat

write.csv(crimes_raw_time, "data/crimes_cleaned_full.csv")
###########################################################################################
cleaned_raw <- read_csv("data/crimes_cleaned_full.csv")[-1] 
comm_ar <- here::here("../data/comm_areas_shapefiles/chi_comm_areas.shp") %>%
  st_read(quiet = TRUE) %>%
  mutate(community = factor(tolower(community)),
         area_numbe = as.numeric(area_numbe)) %>%
  select(community, area_numbe, geometry) %>%
  arrange(community)


chi_comm_area_data <- read_rds("data/community_area_demo_2020.rds") %>%  
  data.frame() %>% 
  mutate(white_perc = white/total,
         black_perc = black/total)

index_crimes <- cleaned_raw %>% 
  ## look at index crimes
  filter(substr(iucr, start = 1, stop = 2) == "02"|
           substr(iucr, start = 1, stop = 2) == "03"|
           substr(iucr, start = 1, stop = 2) == "04"|
           substr(iucr, start = 1, stop = 2) == "05"|
           substr(iucr, start = 1, stop = 2) == "06"|
           substr(iucr, start = 1, stop = 2) == "08"|
           substr(iucr, start = 1, stop = 2) == "09"|
           substr(iucr, start = 1, stop = 2) == "10") %>% 
  filter(!is.na(community_area)) %>% 
  left_join(comm_ar, by = c("community_area" = "area_numbe"))

weekly_index <- index_crimes %>% 
  group_by(week = floor_date(date, "week"), community) %>% 
  tally() %>% 
  complete(community, fill = list(n = 0)) %>%  
  left_join( chi_comm_area_data) %>% 
  mutate(rate = n/total*100000,
         n = as.numeric(n)) %>% 
  select(week, community, n, rate)

weekly_index %>% 
  group_by(community) %>% 
  summarise(zeros = sum(n ==0)) %>% 
  filter(zeros != 0)
#### edison park is the largest number of zeros with 42 but not too many zeros
weekly_index %>% 
  filter(community == "edison park") %>% 
  ggplot(aes(week, rate)) +
  geom_line()
## fairly evenly spread out so just a small community

write_rds(weekly_index, "data/weekly_index_rate.rds")
