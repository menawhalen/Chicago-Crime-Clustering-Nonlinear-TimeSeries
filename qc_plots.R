#################################################
#### paper plots and tables
##################################################

######################### packages
library(tidyverse)
library(viridis)
library(lubridate)
library(sf)
library(jmv)
library(ggrepel)
##############################################################
######## load data
##############################################################
comm_ar <- here::here("../data/comm_areas_shapefiles/chi_comm_areas.shp") %>%
  st_read(quiet = TRUE) %>%
  mutate(community = tolower(community)) %>%
  select(community, geometry) %>%
  arrange(community)

chi_comm_area_data <- readRDS("data/community_area_demo_2020.rds") %>%
  tibble() %>%
  mutate(White_perc = white/total,
         Black_perc = black/total) %>%
  select(community, White_perc, Black_perc) %>%
  arrange(community) %>%
  left_join(comm_ar) %>%
  pivot_longer(cols = c(White_perc, Black_perc), names_to = "demo", values_to = "Percent") %>% 
  mutate(demo = str_replace(demo, "_perc", ""),
         Percent = 100*Percent) %>% 
  st_as_sf()
district_demo_2020 <- readRDS("data/police_district_demo_2020.rds")

dist_measure <- read_rds("data/dist_measure_index_list.rds")
clust_index <- read_rds("data/cluster_assignments_index.rds")

dist_measure_firearm <- read_rds("data/dist_measure_firearm_list.rds")
clust_firearm <- read_rds("data/cluster_assignments_firearm.rds")
### index data cleaning
index_week <- readRDS("data/weekly_index_rate.rds") %>% 
  mutate(week = ymd(week)) %>% 
  arrange(community)
index_data <- map(dist_measure, \(x) x$dat %>% 
      pivot_longer(cols = -week, names_to = "community", values_to = "rate")) %>% 
  bind_rows(.id = "time") %>% 
  left_join(map(dist_measure, \(x) x$trend_dat %>% 
                  pivot_longer(cols = -week, names_to = "community", values_to = "trend")) %>% 
              bind_rows(.id = "time"))

## index cluster cleaning
## using 3 clusters before, 2020 and 2 clusters after
## everytime using dtw1

index_data_cluster <- index_data %>% 
  left_join(clust_index %>% 
              filter(diss == "dtw1") %>% 
              filter((cluster_size == 3 & time == "before") | (cluster_size == 3 & time == "after20") | (cluster_size == 2 & time == "after23")) ) %>% 
  select(-diss)

##################
guns_month <- readRDS("data/monthly_guns_rate.rds")
firearm_data <- map(dist_measure_firearm, \(x) x$dat %>% 
                      pivot_longer(cols = -month, names_to = "district", values_to = "rate")) %>% 
  bind_rows(.id = "time") %>% 
  bind_cols(map(dist_measure_firearm, \(x) x$trend_dat %>% 
                  pivot_longer(cols = everything(), names_to = "district", values_to = "trend") %>% select(trend)) %>% bind_rows())

firearm_data_cluster <- firearm_data %>% 
  left_join(clust_firearm %>% 
              filter(diss == "dtw1") %>% 
              filter((cluster_size == 2 & time == "before") | (cluster_size == 3 & time == "after20") | (cluster_size == 3 & time == "after23")) ) %>% 
  select(-diss)

###################################################################
######### Figure 1 Data Demographics Map
##################################################################
ggplot(chi_comm_area_data, aes(fill = Percent)) +
  geom_sf() +
  scale_fill_viridis(limits = c(0,100)) + 
  facet_wrap(~ demo) +
  theme_void() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        legend.position="bottom") 
###
## save as eps

###################################################################
######### Table 1 median/average for both crime types
##################################################################


index_data_cluster %>% 
  group_by(time, community) %>% 
  summarise(mean = mean(rate)) %>% 
  group_by(time) %>% 
  summarise(all_med = median(mean),
            all_mean = mean(mean),
            n = n(),
            sd = sd(mean),
            se = sd/n) %>% 
  bind_rows(firearm_data_cluster %>%
              group_by(time, district) %>%
              summarise(mean = mean(rate),
                        sd = sd(rate)) %>% 
              group_by(time) %>% 
              summarise(all_med = median(mean),
                        all_mean = mean(mean),
                        n = n(),
                        sd = sd(mean),
                        se = sd/n), .id = "crime_type") %>%
  mutate(time = factor(time, levels = c("before", "after20", "after23"), labels = c("2011(15)-2019", "2020", "2020-2023")),
         crime_type = factor(crime_type, labels = c("Index", "Firearm"))) %>%
  arrange(crime_type, time) %>% view()

###################################################################
######### Table 2 Cluster Assignment Index with mean/median by years
##################################################################
index_data_cluster %>% 
  group_by(time, community) %>% 
  summarise(mean = mean(rate)) %>% 
  left_join(clust_index %>% 
              filter(diss == "dtw1") %>% 
              filter((cluster_size == 3 & time == "before") | (cluster_size == 3 & time == "after20") | (cluster_size == 2 & time == "after23")) ) %>% 
  group_by(time, cluster) %>% 
  summarise(all_mean = mean(mean),
            all_median = median(mean),
            n = n(),
            sd = sd(mean),
            se = sd/n) %>% 
  mutate(time = factor(time, levels = c("before", "after20", "after23"), labels = c("2015-2019", "2020", "2020-2023"))) %>% 
  arrange(time, cluster) %>% view(
  )

###################################################################
######### Figure 2 Cluster Assignment Index with median rate
##################################################################
plotting_index_data_comm <- index_data_cluster %>% 
  group_by(time, cluster, community) %>% 
  summarise(mean_rate = mean(rate)) %>%
  left_join(comm_ar) %>% 
  mutate(time = factor(time, levels = c("before", "after20", "after23"),
                       label = c("2015-2019", "2020", "2020-2023"))) %>% 
  st_as_sf() 

ggplot() +
  geom_sf(data = filter(plotting_index_data_comm, cluster == 3), aes(fill = mean_rate)) +
  scale_fill_gradient(low = alpha("red", 0.1), high = "red",
                      name = "Cluster 3 \nMean Rate",
                      guide = guide_colourbar(order = 3)) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = filter(plotting_index_data_comm, cluster == 2), aes(fill = mean_rate)) +
  scale_fill_gradient(low = alpha("orange", 0.1), high = "orange",
                      name = "Cluster 2 \nMean Rate",
                      guide = guide_colourbar(order = 2))  +
  ggnewscale::new_scale_fill() +
  geom_sf(data = filter(plotting_index_data_comm, cluster == 1), aes(fill = mean_rate)) +
  scale_fill_gradient(low = alpha("blue", 0.1), high = "blue",
                      name = "Cluster 1 \nMean Rate",
                      guide = guide_colourbar(order = 1)) +
  facet_wrap(~time) +
  theme_void() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom") 

###################################################################
######### Table 3 firearm cluster medians
##################################################################
firearm_data_cluster %>%
  group_by(time, district, cluster) %>% 
  summarise(mean = mean(rate)) %>% 
  group_by(time, cluster) %>% 
  summarise(all_mean = mean(mean),
            all_median = median(mean),
            n = n(),
            sd = sd(mean),
            se = sd/n) %>% 
  mutate(time = factor(time, levels = c("before", "after20", "after23"), labels = c("2011-2019", "2020", "2020-2024"))) %>% 
  arrange(time, cluster) %>% view(
  )
###################################################################
######### Figure 4 Firearm Cluster Assignment
##################################################################
plot_guns_district <- firearm_data_cluster %>% 
  group_by(time, cluster, district) %>% 
  summarise(mean_rate = mean(rate)) %>%
  left_join(district_demo_2020) %>% 
  mutate(time = factor(time, levels = c("before", "after20", "after23"),
                       label = c("2011-2019", "2020", "2020-2023"))) %>% 
  st_as_sf()


ggplot() +
  geom_sf(data = filter(plot_guns_district, cluster == 2), aes(fill = mean_rate)) +
  scale_fill_gradient(low = alpha("orange", 0.1), high = "orange",
                      name = "Cluster 2 \nMean Crime Rate",
                      guide = guide_colourbar(order = 2))  +
  ggnewscale::new_scale_fill() +
  geom_sf(data = filter(plot_guns_district, cluster == 3), aes(fill = mean_rate)) +
  scale_fill_gradient(low = alpha("red", 0.1), high = "red",
                      name = "Cluster 3 \nMean Crime Rate",
                      guide = guide_colourbar(order = 3)) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = filter(plot_guns_district, cluster == 1), aes(fill = mean_rate)) +
  scale_fill_gradient(low = alpha("blue", 0.1), high = "blue",
                      name = "Cluster 1 \nMean Crime Rate",
                      guide = guide_colourbar(order = 1)) +
  facet_wrap(~time) +
  theme_void() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom") 


###################################################################
######### SUPP: Figure 5 Time Series of Index
##################################################################
index_data_cluster %>% 
  mutate(time = factor(time, levels = c("before", "after20", "after23"), 
                       labels = c("2015-2019", "2020", "2020-2023"))) %>% 
  ggplot() +
  geom_line(aes(week, trend, color = community), alpha = 0.6) +  
  facet_grid(cluster~time, scales = "free",space = "free") +
  scale_x_date(date_breaks = "3 months", date_labels = "%m %Y") +
  labs(x = "Time",
       y = "Estimated Trend of Time Series",
       color = "Community") +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        axis.text.y=element_text(angle=20, hjust=1))


###################################################################
######### SUPP: Figure 6 Time Series of Firearm
##################################################################
firearm_data_cluster %>% 
  mutate(time = factor(time, levels = c("before", "after20", "after23"), 
                       labels = c("2011-2019", "2020", "2020-2023"))) %>% 
  ggplot() +
  geom_line(aes(month, trend, color = district), alpha = 0.6) +  
  facet_grid(cluster~time, scales = "free",space = "free") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m %Y") +
  labs(x = "Time",
       y = "Estimated Trend of Time Series",
       color = "District") +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        axis.text.y=element_text(angle=20, hjust=1))

###################################################################
######### Table 4/5 Demographics for cluster and crime type
##################################################################

index_data_cluster %>% 
  select(time, community, cluster) %>% 
  distinct() %>% 
  left_join(chi_comm_area_data) %>% 
  group_by(time, cluster, demo) %>% 
  summarise(mean = mean(Percent), 
            sd = sd(Percent),
            n = n(),
            se = sd/n) %>% 
  mutate(time = factor(time, levels = c("before", "after20", "after23"), 
                       labels = c("2015-2019", "2020", "2020-2023"))) %>% 
  arrange(cluster, time)

firearm_data_cluster %>% 
  select(time, district, cluster) %>% 
  distinct() %>% 
  left_join(district_demo_2020 %>% 
    tibble() %>% 
    mutate(white = white/total,
           black = black/total) %>%
    select(district, white, black) %>%
    pivot_longer(cols = c(white, black), names_to = "demo", values_to = "percent")) %>%
  mutate(percent = 100*percent) %>% 
  group_by(time, cluster, demo) %>%
  summarise(mean = mean(percent), 
            sd = sd(percent),
            n = n(),
            se = sd/n) %>%
  mutate(time = factor(time, levels = c("before", "after20", "after23"), labels = c("2011-2019", "2020", "2020-2023"))) %>%
  arrange(cluster, time)

###################################################################
######### Figure 6 boxplots comparisons index
##################################################################

index_data_cluster %>%
  group_by(time, cluster, community) %>% 
  summarise(m = mean(rate)) %>% 
  mutate(time = factor(time, levels = c("before", "after20", "after23"), labels = c("2015-2019", "2020", "2020-2023")),
         cluster = factor(cluster)) %>% 
  ggplot(aes(cluster, m, fill = time)) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  labs(x = "Cluster",
       y = "Index Crime Rate - per 100,000",
       fill = "Time Period")

###################################################################
######### Table 7 boxplots comparisons firearm
##################################################################

firearm_data_cluster %>%
  group_by(time, cluster, district) %>% 
  summarise(m = mean(rate)) %>% 
  mutate(time = factor(time, levels = c("before", "after20", "after23"), labels = c("2011-2019", "2020", "2020-2023")),
         cluster = factor(cluster)) %>% 
  ggplot(aes(cluster, m, fill = time)) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  labs(x = "Cluster",
       y = "Firearm-Related Crime Rate - per 100,000",
       fill = "Time Period")
###################################################################
######### Analysis of Cluster changes over time Index
##################################################################

index_data_cluster %>% 
  group_by(time, cluster, community) %>% 
  summarise(m = mean(rate)) %>% 
  group_by(cluster) %>% 
  group_split() %>% 
  map(\(x) kruskal.test(m ~ time, data = x))

index_data_cluster %>% 
  
  group_by(time, cluster, community) %>% 
  summarise(m = mean(rate)) %>% 
  group_by(cluster) %>% 
  group_split() %>% 
  map(\(x) anovaNP(x, deps = "m", group = "time", pairs = TRUE))
###################################################################
######### Analysis of Cluster changes over time Firearm
##################################################################

firearm_data_cluster %>% 
  group_by(time, cluster, district) %>% 
  summarise(m = mean(rate)) %>% 
  group_by(cluster) %>% 
  group_split() %>% 
  map(\(x) kruskal.test(m ~ time, data = x))

firearm_data_cluster %>%
  group_by(time, cluster, district) %>% 
  summarise(m = mean(rate)) %>% 
  group_by(cluster) %>% 
  group_split() %>% 
  map(\(x) anovaNP(x, deps = "m", group = "time", pairs = TRUE))

###################################################################
######### Communities that changes over time index
##################################################################

index_data_cluster %>% 
  select(time, community, cluster) %>% 
  distinct() %>% 
  pivot_wider(id_cols = community, names_from = time, values_from = cluster) %>% 
  mutate(changes = paste(before, after20, after23, sep = "_")) %>% 
  group_by(changes) %>% count()


point_labs <- index_data_cluster %>% 
  select(time, community, cluster) %>% 
  distinct() %>% 
  pivot_wider(id_cols = community, names_from = time, values_from = cluster) %>% 
  mutate(changes = paste(before, after20, after23, sep = "_")) %>%
  filter(changes == "1_1_2" | changes == "2_2_1" | changes == "3_1_2" | changes == "3_3_2") %>% 
  mutate(labels = case_when(
    changes == "3_3_2" ~ "Decreasing \n Very High - High",
    changes == "2_2_1" ~ "Decreasing \nHigh-Low",
    changes == "3_1_2" ~ "Drop in 2020",
    changes == "1_1_2" ~ "Increasing \nLow-High"
  )) %>% 
  left_join(comm_ar) %>%
  mutate(community = str_to_title(community)) %>% 
  st_as_sf(crs = st_crs(comm_ar)) %>%
  st_point_on_surface() 

index_data_cluster %>% 
  select(time, community, cluster) %>% 
  distinct() %>% 
  pivot_wider(id_cols = community, names_from = time, values_from = cluster) %>% 
  mutate(changes = paste(before, after20, after23, sep = "_")) %>%
  filter(changes == "1_1_2" | changes == "2_2_1" | changes == "3_1_2" | changes == "3_3_2") %>% 
  mutate(labels = case_when(
    changes == "3_3_2" ~ "Decreasing \n Very High - High",
    changes == "2_2_1" ~ "Decreasing \nHigh-Low",
    changes == "3_1_2" ~ "Drop in 2020",
    changes == "1_1_2" ~ "Increasing \nLow-High"
  )) %>% 
  left_join(comm_ar) %>% 
  mutate(community = str_to_title(community)) %>% 
  st_as_sf(crs = st_crs(comm_ar)) %>%
  ggplot() +
  geom_sf(data = comm_ar, fill = NA) +
  geom_sf(aes(fill = labels)) +
  geom_text_repel(data = point_labs, aes(label = community, geometry = geometry), stat = "sf_coordinates", min.segment.length = 0.1, force = 2.5) +
  theme_void() +
  labs(fill = "Community Pattern") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

#### near west side need to explore

index_data %>% 
  filter(community == "near west side") %>% 
  ggplot(aes(week, rate)) +
  geom_line() +
  geom_line(aes(week, trend), color = "red")

########################## firearms
firearm_data_cluster %>% 
  select(time, district, cluster) %>% 
  distinct() %>% 
  pivot_wider(id_cols = district, names_from = time, values_from = cluster) %>% 
  mutate(changes = paste(before, after20, after23, sep = "_")) %>% 
  group_by(changes) %>% 
  


firearm_data_cluster %>% 
  select(time, district, cluster) %>% 
  distinct() %>% 
  pivot_wider(id_cols = district, names_from = time, values_from = cluster) %>% 
  mutate(changes = paste(before, after20, after23, sep = "_")) %>% 
  filter(changes == "2_3_2" | changes == "2_3_3")

firearm_data %>% 
  filter(district == "11" | district == "15"| district == "7") %>% 
  ggplot() +
  geom_line(aes(x = month, y = rate, color = district)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

touches_comm <- district_demo_2020 %>% 
  filter(district == "11" | district == "15"| district == "7") %>% 
  st_transform(crs = st_crs(comm_ar)) %>% 
  st_join(comm_ar, join = st_intersects) %>% 
  filter(!(community %in% c("ashburn", "auburn gresham", "chicago lawn", "gage park", "west town", "new city", "fuller park"))) %>% 
  select(district, community) %>% 
  mutate(change = ifelse(district == "7", "Spike 2020", "Increasing"))

tibble(touches_comm) %>% 
  select(-geometry) %>% 
  left_join(comm_ar) %>% 
  mutate(community = str_to_title(community)) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = community)) +
  geom_sf(data = comm_ar, fill = NA) +
  geom_sf(data = touches_comm, aes(color = change, linetype = change), fill =  NA, size = 5) +
  scale_color_manual(values = c("darkred","red")) +
  labs(fill = "Community",
       color = "District Pattern",
       linetype = "District Pattern") +
  theme_void() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 


## dont like all together
index_data_cluster %>% 
  select(time, community, cluster) %>% 
  distinct() %>% 
  pivot_wider(id_cols = community, names_from = time, values_from = cluster) %>% 
  mutate(changes = paste(before, after20, after23, sep = "_")) %>%
  filter(changes == "1_1_2" | changes == "2_2_1" | changes == "3_1_2" | changes == "3_3_2") %>% 
  mutate(labels = case_when(
    changes == "3_3_2" ~ "Decreasing \n Very High - High",
    changes == "2_2_1" ~ "Decreasing \nHigh-Low",
    changes == "3_1_2" ~ "Drop in 2020",
    changes == "1_1_2" ~ "Increasing \nLow-High"
  )) %>% 
  left_join(comm_ar) %>% 
  mutate(community = str_to_title(community)) %>% 
  st_as_sf(crs = st_crs(comm_ar)) %>%
  ggplot() +
  geom_sf(data = comm_ar, fill = NA) +
  geom_sf(aes(fill = labels)) +
  geom_text_repel(data = point_labs, aes(label = community, geometry = geometry), stat = "sf_coordinates", min.segment.length = 0.1, force = 2.5) +
  geom_sf(data = touches_comm, aes(color = change, linetype = change), fill =  NA, size = 5) +
  scale_color_manual(values = c("darkblue","darkslateblue")) +
  theme_void() +
  labs(fill = "Community Pattern") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


### other demo data
library(labelled)
health_atlas <- read_csv("data/CHA_commar.csv") %>% 
  janitor::clean_names()
var_label(health_atlas) <- health_atlas[1,]

HA_info <- health_atlas[1:4,]

health_info <- health_atlas[-c(1:4),] %>% 
  select(!contains("se")) %>% 
  mutate(name = str_to_lower(name)) %>% 
  select(-layer)

names(health_info) <- c("community", "geoid", "population", "longitude", "latitude", 
                        "uninsured", "eviction", "vacancy", "svi", "adi", "food_access", 
                        "hs_grad", "unemployed", "child_score", "econ_diversity", 
                        "hardship", "median_income", "poverty", "public_assistance", "snap")

# all_plots <- health_info %>% 
#   left_join(touches_comm) %>% 
#   mutate(changing = ifelse(!is.na(district), 1, 0)) %>% 
#   select(-geometry) %>% 
#   pivot_longer(cols = uninsured:snap, names_to = "variable", values_to = "value") %>% 
#   left_join(comm_ar) %>%
#   st_as_sf() %>% 
#   ggplot() +
#   geom_sf(aes(fill = value)) +
#   facet_wrap_paginate(~ variable, ncol = 1, nrow = 1)

pdf("other_demographics.pdf", width = 8, height = 11)
for(i in c("uninsured", "eviction", "vacancy", "svi", "adi", "food_access", 
           "hs_grad", "unemployed", "child_score", "econ_diversity", 
           "hardship", "median_income", "poverty", "public_assistance", "snap")){
  print(health_info %>% 
    left_join(touches_comm) %>% 
    mutate(changing = ifelse(!is.na(district), 1, 0)) %>% 
    select(-geometry) %>% 
    pivot_longer(cols = uninsured:snap, names_to = "variable", values_to = "value") %>% 
    mutate(value = as.numeric(value)) %>% 
    left_join(comm_ar) %>%
    filter(variable == !!i) %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(aes(fill = value)) +
    scale_fill_viridis(discrete = F) +
    ggtitle(i) )
}
dev.off()

## fix ohare
health_info[which(health_info$community == "o'hare"),]$community <- "ohare"

##### Table for changes in clusters

health_info %>% 
  select(community, hs_grad, public_assistance, median_income, svi) %>% 
  pivot_longer(cols = -community, names_to = "variable", values_to = "value") %>% 
  mutate(value = as.numeric(value)) %>% 
  left_join(distinct(select(index_data_cluster, time, community, cluster))) %>% 
  group_by(time, cluster, variable) %>% 
  summarise(m = mean(value),
            n = n(),
            se = sd(value)/n) %>% view()




#### now doing it for districts but at district level

health_atlas_census <- read_csv("data/CHA_censustract.csv") %>% 
  janitor::clean_names()
var_label(health_atlas_census) <- health_atlas_census[1,]

census<-st_read("census_2020/census_tract/tl_2020_17_tract.shp") %>% janitor::clean_names()


health_atlas_census <- health_atlas_census[-c(1:4),] %>% 
  select(!contains("se")) %>% 
  mutate(name = str_to_lower(name)) %>% 
  select(-layer)
names(health_atlas_census) <- c("tract", "geoid", "population", "longitude", "latitude", 
                        "uninsured", "eviction", "vacancy", "svi", "adi", "food_access", 
                        "hs_grad", "unemployed", "child_score", "econ_diversity", 
                        "hardship", "median_income", "poverty", "public_assistance", "snap")



vars_to_interpolate <- c("population", "uninsured", "eviction", "vacancy", "svi", 
                         "adi", "food_access", "hs_grad", "unemployed", "child_score", 
                         "econ_diversity", "hardship", "median_income", "poverty", 
                         "public_assistance", "snap")
health_sf <- left_join(health_atlas_census, census, by = "geoid") %>% 
  st_as_sf(crs = st_crs(district_demo_2020)) %>% 
  mutate(across(all_of(vars_to_interpolate), as.numeric))

ggplot(district_demo_2020) +
  geom_sf() +
  geom_sf(data = health_sf, color = "blue", fill = NA)

interpolated_vars <- map_dfc(vars_to_interpolate, function(var) {
  interpolated <- st_interpolate_aw(x = health_sf[var],
                                    to = select(district_demo_2020, district, geometry),
                                    extensive = FALSE, keep_NA = TRUE, na.rm = TRUE)
  setNames(interpolated[[var]], var)
 })
names(interpolated_vars) <- vars_to_interpolate
joined <- bind_cols(select(district_demo_2020, district, geometry), interpolated_vars)

st_as_sf(joined) %>% 
  ggplot(aes(fill = hs_grad)) +
  geom_sf()


tibble(joined) %>% 
  select(-geometry) %>% 
  select(district, hs_grad, public_assistance, median_income, svi) %>% 
  pivot_longer(cols = -district, names_to = "variable", values_to = "value") %>% 
  left_join(distinct(select(firearm_data_cluster, time, district, cluster))) %>% 
  group_by(time, cluster, variable) %>% 
  summarise(m = mean(value),
            n = n(),
            se = sd(value)/n) %>% view()

#### Map of demos with areas that change 
library(biscale)

bi <- health_info %>% 
  select(community, econ_diversity, unemployed) %>% 
  mutate(econ_diversity = as.numeric(econ_diversity),
         unemployed = as.numeric(unemployed)) %>% 
  left_join(comm_ar) %>% 
  st_as_sf()
  # pivot_longer(cols = -community, names_to = "variable", values_to = "value") %>% 
  # mutate(value = as.numeric(value)) %>% 
  # left_join(comm_ar) 

## example 1
labels1 <- bi_class_breaks(bi, x = econ_diversity, y = unemployed, style = "quantile", 
                           dim = 4, dig_lab = 2, split = FALSE)

## example 2
breaks2 <- bi_class_breaks(bi, x = econ_diversity, y = unemployed, style = "quantile", 
                           dim = 4, dig_lab = c(x = 2, y = 5), split = TRUE)
legend1 <- bi_legend(pal = "GrPink2",
                     xlab = "Economic Diversity (Probability)",
                     ylab = "% Unemployed",
                     size = 12,
                     breaks = labels1,
                     arrows = FALSE)

# community labels 
label_base <- touches_comm %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    label_x = st_coordinates(centroid)[, 1],
    label_y = st_coordinates(centroid)[, 2]
  ) %>%
  select(community, label_x, label_y)
custom_labels <- label_base %>%
  mutate(
    label_x = case_when(
      community == "west garfield park"     ~ label_x - 0.2,
      community == "west englewood"         ~ label_x + 0.025,
      community == "north lawndale"         ~ label_x - 0.025,
      community == "near west side"         ~ label_x + 0.03,
      community == "humboldt park"          ~ label_x - 0.035,
      community == "greater grand crossing" ~ label_x + 0.025,
      community == "englewood"              ~ label_x + 0.03,
      community == "east garfield park"     ~ label_x - 0.02,
      community == "austin"                 ~ label_x - 0.04,
      TRUE ~ label_x
    ),
    label_y = case_when(
      community == "west garfield park"     ~ label_y + 0.01,
      community == "west englewood"         ~ label_y - 0.015,
      community == "north lawndale"         ~ label_y + 0.015,
      community == "near west side"         ~ label_y + 0.01,
      community == "humboldt park"          ~ label_y + 0.02,
      community == "greater grand crossing" ~ label_y - 0.02,
      community == "englewood"              ~ label_y - 0.025,
      community == "east garfield park"     ~ label_y + 0.01,
      community == "austin"                 ~ label_y + 0.025,
      TRUE ~ label_y
    )
  ) %>%
  st_as_sf(coords = c("label_x", "label_y"), crs = st_crs(touches_comm))

library(ggsflabel)
pl <- ggplot() +
  geom_sf(data = dat, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink2", dim = 4) +
  # geom_sf(data = st_as_sf(left_join(tibble(touches_comm),comm_ar)), color = "black", fill = NA) +
  #geom_sf_text(data = custom_labels, aes(label = community), size = 3) +
  geom_sf(data = touches_comm, color = "yellow", fill = NA) +
  labs(x = NULL,
         y = NULL) +
  bi_theme() 
  
library(cowplot)
ggdraw() +
  draw_plot(pl, 0, 0, 1, 1) +
  draw_plot(legend1, 0.002, 0.008, 0.5, .5) 

