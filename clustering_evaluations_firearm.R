library(tidyverse)
library(lubridate)
library(sf)
library(janitor)
library(viridis)
library(bfast)
library(trendsegmentR)
library(spdep)
library(janitor)
library(ClustGeo)
library(TSclust)
library(dtw)
library(ggpmisc)

month_guns <- readRDS("data/monthly_guns_rate.rds")
months <- unique(month_guns$month)
## have to get spatial data with demographic info
district_population_spatial <- readRDS("data/police_district_demo_2020.rds") %>% 
  mutate(perc_white = white/total,
         perc_black = black/total)

### get deseasoned data
stl_district <- data.frame(month_guns) %>% 
  select(month, district, rate) %>% 
  group_by(district) %>% 
  group_map(~ stl(ts(.x$rate, start = decimal_date(.x$month[1]), frequency = 12), "periodic"))

names(stl_district) <- levels(month_guns$district)


### join seasonal est and data to get deseasoned = data - seasonal
deseasoned_month_guns <- bind_rows(lapply(stl_district, function(x) pluck(x, "time.series")[,"seasonal"])) %>% 
  mutate(month = months) %>% 
  pivot_longer(cols = -month, names_to = "district", values_to = "seasonal_effect") %>% 
  left_join(month_guns) %>% 
  mutate(deseasoned = rate - seasonal_effect,
         district = factor(district, levels = district_population_spatial$district))


######################################################
## geo spatial
demo_neigh <- read.gal("neigh_guns.GAL")

### now get into correct matrix form
neigh_mat <- nb2mat(demo_neigh, style = "W", zero.policy = T)
diag(neigh_mat) <- 1
colnames(neigh_mat) <- rownames(neigh_mat) <- district_population_spatial$district
D1 <- 1-neigh_mat
D_geo <- as.dist(D1)



#####################################################
## distance measure
guns_wide <- pivot_wider(deseasoned_month_guns[c("month", "district", "deseasoned")], names_from = district, values_from = deseasoned)

dist_measure <- list(before = list(dat = guns_wide %>% dplyr::filter(month < ymd("2020-01-01")),
                                   geo = D_geo),
                     after20  = list(dat = guns_wide %>% dplyr::filter(month > ymd("2019-12-31") & month < ymd("2021-01-01")),
                                     geo = D_geo),
                     after23  = list(dat = guns_wide %>% dplyr::filter(month > ymd("2019-12-31") & month < ymd("2024-01-01")),
                                     geo = D_geo))

###
get_trend <- function(data, thres){
  ts_dat <- ts(data$dat[2:23], start = data$dat$month[1], frequency = 12)
  months <- data$dat$month
  trend_out <- apply(ts_dat, 2, trendsegment, th.const = thres)
  trend_df <- bind_cols(month = months, bind_rows(lapply(trend_out, function(x) ts = pluck(x, 2)), .id = "district"))
  no_cpt <- bind_cols(bind_rows(lapply(trend_out, function(x) ts = pluck(x, 3)), .id = "district")) %>% mutate(thres = thres)
  return(list(trend_out = trend_out, trend_data = trend_df, cpt_total = no_cpt))
}

#### find right parameter choices
for (i in 1:length(dist_measure)) {
  for (j in 1:length(seq(0.7, 3.2, by = 0.1))) {
    trend <- get_trend(dist_measure[[i]], seq(0.7, 3.2, by = 0.1)[j])
    dist_measure[[i]]$trend_dat[[j]] <- trend$trend_dat
    dist_measure[[i]]$cpt_total[[j]] <- trend$cpt_total
  }
  
}

before_cpt <- bind_rows(dist_measure$before$cpt_total) %>% 
  pivot_longer(cols = - thres, names_to = "district", values_to = "number_cpt") %>% 
  ggplot(aes(factor(thres), number_cpt, group = district)) +
  geom_line(alpha = 0.3) +
  ggtitle("Before")
after20_cpt <- bind_rows(dist_measure$after20$cpt_total) %>% 
  pivot_longer(cols = - thres, names_to = "district", values_to = "number_cpt") %>% 
  ggplot(aes(factor(thres), number_cpt, group = district)) +
  geom_line(alpha = 0.3) +
  ggtitle("2020")
after23_cpt <- bind_rows(dist_measure$after23$cpt_total) %>% 
  pivot_longer(cols = - thres, names_to = "district", values_to = "number_cpt") %>% 
  ggplot(aes(factor(thres), number_cpt, group = district)) +
  geom_line(alpha = 0.3) +
  ggtitle("2020-2023")

pdf("final_images/firearm/no_cpt_threshold.pdf")
gridExtra::grid.arrange(before_cpt, after20_cpt, after23_cpt)
dev.off()

for (i in 1:length(dist_measure)) {
  for (j in 1:length(seq(0.7, 3.2, by = 0.1))) {
    dist_measure[[i]]$diff[[j]] <- dist_measure[[i]]$dat[-1] - dist_measure[[i]]$trend_dat[[j]][-1]
  }
}

names(dist_measure$before$diff) <- seq(0.7, 3.2, by = 0.1)
names(dist_measure$after20$diff) <- seq(0.7, 3.2, by = 0.1)
names(dist_measure$after23$diff) <- seq(0.7, 3.2, by = 0.1)

difference <- function(dat) {
  left <- (dat$sse - dat$sse[1]) / dat$thres
  right <- (dat$sse[length(dat$sse)] - dat$sse) / (dat$thres[length(dat$thres)] - dat$thres)
  return(left - right)
}
before_ave <- bind_rows(dist_measure$before$diff, .id = "thres") %>%
  pivot_longer(cols = -thres, names_to = "district", values_to = "ss") %>%
  group_by(thres) %>%
  summarise(sse = sum(ss^2)) %>% 
  mutate(thres = as.numeric(thres))

before_elbow <- tibble(diff = difference(before_ave)) %>% 
  bind_cols(thres = seq(0.7, 3.2, by = 0.1)) %>% 
  ggplot(aes(thres, diff)) +
  geom_line() +
  ggtitle("Before")
after20_ave <- bind_rows(dist_measure$after20$diff, .id = "thres") %>%
  pivot_longer(cols = -thres, names_to = "district", values_to = "ss") %>%
  group_by(thres) %>%
  summarise(sse = sum(ss^2)) %>% 
  mutate(thres = as.numeric(thres))
after20_elbow <- tibble(diff = difference(after20_ave)) %>% 
  bind_cols(thres = seq(0.7, 3.2, by = 0.1)) %>% 
  ggplot(aes(thres, diff)) +
  geom_line()+
  ggtitle("2020")
after23_ave <- bind_rows(dist_measure$after23$diff, .id = "thres") %>%
  pivot_longer(cols = -thres, names_to = "district", values_to = "ss") %>%
  group_by(thres) %>%
  summarise(sse = sum(ss^2)) %>% 
  mutate(thres = as.numeric(thres))
after23_elbow <- tibble(diff = difference(after23_ave)) %>% 
  bind_cols(thres = seq(0.7, 3.2, by = 0.1)) %>% 
  ggplot(aes(thres, diff)) +
  geom_line()+
  ggtitle("2020-2023")

pdf("final_images/firearm/elbow_threshold.pdf")
gridExtra::grid.arrange(before_elbow, after20_elbow, after23_elbow)
dev.off()

tibble(before = difference(before_ave),
       after20 = difference(after20_ave),
       after23 = difference(after23_ave),
       thres = seq(0.7, 3.2, by = 0.1)) %>% 
  filter(thres < 2.5) %>% 
  pivot_longer(cols = -thres, names_to = "time", values_to = "elbow") %>% 
  group_by(time) %>% 
  filter(elbow == max(elbow))

#################################################################################
find_alpha <- function(dist, geo){
  a_norm <- data.frame(choicealpha(dist, geo, range.alpha=seq(0, 1, 0.1), K= 4, graph = F)$Qnorm)[-c(1,11),] %>% 
    rownames_to_column(var = "alpha") %>% mutate(diff = abs(Q0norm - Q1norm))
  ## takes min and returns it as a number
  value <- as.numeric(str_remove(a_norm[which.min(a_norm$diff),]$alpha, "alpha="))
  return(value)
}

for (i in 1:length(dist_measure)) {
  if (i == 1) {
    trend <- get_trend(dist_measure[[i]], thres = 1.5) 
  }
  else if (i == 2){
    trend <- get_trend(dist_measure[[i]], thres = 1.2)
  }
  else {
    trend <- get_trend(dist_measure[[i]], thres = 2.3)
  }
  
  ### 
  trend_data <- trend$trend_dat[-1]
  dist_measure[[i]]$trend_dat <- trend_data
  
  ## trend
  dist_measure[[i]]$eucl <- TSclust::diss(trend_data, METHOD = "EUCL")
  ## dtw2
  ndtw <- function(x, y, w){
    dtw(x, y, window.type = "sakoechiba", window.size =w)$distance
  }
  
  dtw2_diss_t <- as.list(trend_data) %>%
    expand.grid(., .) %>% 
    mutate(distance = map2(Var1, Var2, ndtw, 2)) %>%
    as_tibble()
  
  dtw2_diss_t <- cbind(dtw2_diss_t, expand.grid(name1 = colnames(trend_data), name2 = colnames(trend_data)))
  dtw2_diss_t <- dtw2_diss_t[3:5] %>% pivot_wider(names_from = name1, values_from = distance) %>% column_to_rownames(var = "name2")
  dist_measure[[i]]$dtw2 <- as.dist(dtw2_diss_t)
  
  #### dtw1
  dtw1_diss_t <- as.list(trend_data) %>%
    expand.grid(., .) %>% 
    mutate(distance = map2(Var1, Var2, ndtw, 1)) %>%
    as_tibble()
  
  dtw1_diss_t <- cbind(dtw1_diss_t, expand.grid(name1 = colnames(trend_data), name2 = colnames(trend_data)))
  dtw1_diss_t <- dtw1_diss_t[3:5] %>% pivot_wider(names_from = name1, values_from = distance) %>% column_to_rownames(var = "name2")
  dist_measure[[i]]$dtw1 <- as.dist(dtw1_diss_t)
  #### dtw3 or eucl
  dtw3_diss_t <- as.list(trend_data) %>%
    expand.grid(., .) %>% 
    mutate(distance = map2(Var1, Var2, ndtw, 3)) %>%
    as_tibble()
  
  dtw3_diss_t <- cbind(dtw3_diss_t, expand.grid(name1 = colnames(trend_data), name2 = colnames(trend_data)))
  dtw3_diss_t <- dtw3_diss_t[3:5] %>% pivot_wider(names_from = name1, values_from = distance) %>% column_to_rownames(var = "name2")
  dist_measure[[i]]$dtw3 <- as.dist(dtw3_diss_t)
  
  
  dist_measure[[i]]$clusters <- list(eucl = hclustgeo(dist_measure[[i]]$eucl, dist_measure[[i]]$geo, alpha = find_alpha(dist_measure[[i]]$eucl, dist_measure[[i]]$geo)),
                                     dtw1 = hclustgeo(dist_measure[[i]]$dtw1, dist_measure[[i]]$geo, alpha = find_alpha(dist_measure[[i]]$dtw1, dist_measure[[i]]$geo)),
                                     dtw2 = hclustgeo(dist_measure[[i]]$dtw2, dist_measure[[i]]$geo, alpha = find_alpha(dist_measure[[i]]$dtw2, dist_measure[[i]]$geo)),
                                     dtw3 = hclustgeo(dist_measure[[i]]$dtw3, dist_measure[[i]]$geo, alpha = find_alpha(dist_measure[[i]]$dtw3, dist_measure[[i]]$geo))) 
}

clusters <- lapply(dist_measure, function(x) pluck(x, 'clusters'))


lapply(seq_along(clusters), function(i){
  pdf( paste0("final_images/firearm/", names(clusters)[i], "_dendro.pdf"))
  dendro <- lapply(clusters[[i]], function(y) pluck(y))
  par(mfrow = c(2, 3))
  for (j in 1:length(dendro)) {
    plot(dendro[[j]], main = paste("distance measure ", names(dendro)[j], split = ""))
  }
  dev.off()
})

lapply(seq_along(clusters)[-1], function(i){
  for (j in 2:3){
    plot <- bind_rows(before = bind_rows(lapply(clusters[[1]], function(y) cutree(y,j)), .id = "diss"), 
                      after = bind_rows(lapply(clusters[[i]], function(y) cutree(y,j)), .id = "diss"), .id = "time") %>%
      pivot_longer(cols = -c(time, diss), names_to = "district", values_to = "cluster") %>% 
      left_join(district_population_spatial) %>% 
      mutate(cluster = factor(cluster),
             time = factor(time, levels = c("before", "after"))) %>%
      st_as_sf() %>% 
      ggplot(aes(fill = cluster)) +
      geom_sf() +
      scale_fill_viridis(discrete = T) +
      facet_grid(diss~time) +
      ggtitle(paste0(j, " Clusters")) 
    ggsave(paste0("final_images/firearm/", names(clusters)[i], "_cluster", j,"_map.pdf"))
  }
})

clust <- list()
for (i in 2:3) {
  clust[[i]] <- bind_rows(before = bind_rows(lapply(clusters[[1]], function(y) cutree(y,i)), .id = "diss"), 
                          after20 = bind_rows(lapply(clusters[[2]], function(y) cutree(y,i)), .id = "diss"),
                          after23 = bind_rows(lapply(clusters[[3]], function(y) cutree(y,i)), .id = "diss"),
                          .id = "time") %>% 
    mutate(cluster_size = i)
}

cluster_info <- bind_rows(clust) %>% 
  pivot_longer(cols = -c(time, diss, cluster_size), names_to = "district", values_to = "cluster")

write_rds(dist_measure, "data/dist_measure_firearm_list.rds")
write_rds(cluster_info, "data/cluster_assignments_firearm.rds")

##################################################################
## Evaluation Measures
################################################################

##################################################################
## cluster 2
##############################################################

# 2
library(factoextra)

for (i in c("eucl", "dtw1", "dtw2", "dtw3")) {
  for (j in seq_along(names(dist_measure))) {
    clust_numb <- filter(cluster_info, cluster_size == 2, diss == i, time == names(dist_measure)[j]) %>% 
      select(district, cluster) %>% 
      rename(name = district, value = cluster) %>% 
      deframe()
    assign(paste0(names(dist_measure)[j], "_", i), silhouette(clust_numb, dist_measure[[j]][[i]]))
  }
}

pdf("final_images/firearm/silhouette_plots2.pdf")
fviz_silhouette(before_eucl, main = "eucl before")
fviz_silhouette(after20_eucl, main = "eucl after20")
fviz_silhouette(after23_eucl, main = "eucl after23")
fviz_silhouette(before_dtw1, main = "dtw1 before")
fviz_silhouette(after20_dtw1, main = "dtw1 after20")
fviz_silhouette(after23_dtw1, main = "dtw1 after23")
fviz_silhouette(before_dtw2, main = "dtw2 before")
fviz_silhouette(after20_dtw2, main = "dtw2 after20")
fviz_silhouette(after23_dtw2, main = "dtw2 after23")
fviz_silhouette(before_dtw3, main = "dtw3 before")
fviz_silhouette(after20_dtw3, main = "dtw3 after20")
fviz_silhouette(after23_dtw3, main = "dtw3 after23")

data.frame(
  cluster = 1:2,
  before_eucl = summary(before_eucl)$clus.avg.widths,
  before_dtw1 = summary(before_dtw1)$clus.avg.widths,
  before_dtw2 = summary(before_dtw2)$clus.avg.widths,
  before_dtw3 = summary(before_dtw3)$clus.avg.widths) %>% 
  left_join(data.frame(
    cluster = 1:2,
    after20_eucl = summary(after20_eucl)$clus.avg.widths,
    after20_dtw1 = summary(after20_dtw1)$clus.avg.widths,
    after20_dtw2 = summary(after20_dtw2)$clus.avg.widths,
    after20_dtw3 = summary(after20_dtw3)$clus.avg.widths))  %>% 
  left_join(data.frame(
    cluster = 1:2,
    after23_eucl = summary(after23_eucl)$clus.avg.widths,
    after23_dtw1 = summary(after23_dtw1)$clus.avg.widths,
    after23_dtw2 = summary(after23_dtw2)$clus.avg.widths,
    after23_dtw3 = summary(after23_dtw3)$clus.avg.widths)) %>% 
  pivot_longer(cols = -cluster, names_to = "sil_coeff", values_to = "value") %>% 
  separate(sil_coeff, c("time", "diss")) %>% 
  mutate(time = factor(time, levels = c("before", "after20", "after21", "after23")),
         diss = factor(diss, labels = c("DTW1", "DTW2", "DTW3", "Euclidean"))) %>% 
  ggplot(aes(cluster, value)) +
  geom_bar(aes(fill = diss), stat = "identity", position = "dodge") +
  facet_wrap(~time) +
  labs(title = "Average Cluster Silhouette by Distance Measure", 
       x = "Cluster",
       y = "Ave. Silhouette",
       fill = "Distance Measures")
dev.off()

##################################################################
## cluster 3
##############################################################

for (i in c("eucl", "dtw1", "dtw2", "dtw3")) {
  for (j in seq_along(names(dist_measure))) {
    clust_numb <- filter(cluster_info, cluster_size == 3, diss == i, time == names(dist_measure)[j]) %>% 
      select(district, cluster) %>% 
      rename(name = district, value = cluster) %>% 
      deframe()
    assign(paste0(names(dist_measure)[j], "_", i), silhouette(clust_numb, dist_measure[[j]][[i]]))
  }
}

pdf("final_images/firearm/silhouette_plots3.pdf")
fviz_silhouette(before_eucl, main = "eucl before")
fviz_silhouette(after20_eucl, main = "eucl after20")
fviz_silhouette(after23_eucl, main = "eucl after23")
fviz_silhouette(before_dtw1, main = "dtw1 before")
fviz_silhouette(after20_dtw1, main = "dtw1 after20")
fviz_silhouette(after23_dtw1, main = "dtw1 after23")
fviz_silhouette(before_dtw2, main = "dtw2 before")
fviz_silhouette(after20_dtw2, main = "dtw2 after20")
fviz_silhouette(after23_dtw2, main = "dtw2 after23")
fviz_silhouette(before_dtw3, main = "dtw3 before")
fviz_silhouette(after20_dtw3, main = "dtw3 after20")
fviz_silhouette(after23_dtw3, main = "dtw3 after23")

data.frame(
  cluster = 1:3,
  before_eucl = summary(before_eucl)$clus.avg.widths,
  before_dtw1 = summary(before_dtw1)$clus.avg.widths,
  before_dtw2 = summary(before_dtw2)$clus.avg.widths,
  before_dtw3 = summary(before_dtw3)$clus.avg.widths) %>% 
  left_join(data.frame(
    cluster = 1:3,
    after20_eucl = summary(after20_eucl)$clus.avg.widths,
    after20_dtw1 = summary(after20_dtw1)$clus.avg.widths,
    after20_dtw2 = summary(after20_dtw2)$clus.avg.widths,
    after20_dtw3 = summary(after20_dtw3)$clus.avg.widths)) %>% 
  left_join(data.frame(
    cluster = 1:3,
    after23_eucl = summary(after23_eucl)$clus.avg.widths,
    after23_dtw1 = summary(after23_dtw1)$clus.avg.widths,
    after23_dtw2 = summary(after23_dtw2)$clus.avg.widths,
    after23_dtw3 = summary(after23_dtw3)$clus.avg.widths)) %>% 
  pivot_longer(cols = -cluster, names_to = "sil_coeff", values_to = "value") %>% 
  separate(sil_coeff, c("time", "diss")) %>% 
  mutate(time = factor(time, levels = c("before", "after20", "after21", "after23")),
         diss = factor(diss, labels = c("DTW1", "DTW2", "DTW3", "Euclidean"))) %>% 
  ggplot(aes(cluster, value)) +
  geom_bar(aes(fill = diss), stat = "identity", position = "dodge") +
  facet_wrap(~time) +
  labs(title = "Average Cluster Silhouette by Distance Measure", 
       x = "Cluster",
       y = "Ave. Silhouette",
       fill = "Distance Measures")
dev.off()

##################################################################
## within internia 
##################################################################

within_error <- function(dataset, dist_trend, dist_geo, dist_type, time_period, cluster_num){
  I_cluster <- list()
  for (m in cluster_num) {
    dist_clust <- unique(dplyr::filter(dataset, diss == dist_type & 
                                         time == time_period & cluster == m)$district)
    a <- find_alpha(dist_trend, dist_geo)
    grid <- expand.grid(dist_clust, dist_clust) %>% 
      mutate(Var1 = as.character(Var1),
             Var2 = as.character(Var2))
    
    scaled_dist <- dist_trend/max(dist_trend)
    scaled_geo <- dist_geo/max(dist_geo)
    
    ts_sum <- c()
    geo_sum <- c()
    for (i in 1:dim(grid)[1]) {
      ts_sum <- append(ts_sum, as.matrix(scaled_dist)[grid[i,1],grid[i,2]])
      geo_sum <- append(geo_sum, as.matrix(scaled_geo)[grid[i,1],grid[i,2]])
    }
    
    
    d_ts <-sum(ts_sum)/(2*length(dist_clust))
    d_geo <- sum(geo_sum)/(2*length(dist_clust))
    I_normal <- (1-a)*d_ts + a*d_geo
    
    I_removed <- c()
    for (j in 1:length(dist_clust)) {
      clust_removed <- dist_clust[-j]
      grid_removed <- expand.grid(clust_removed, clust_removed) %>% 
        mutate(Var1 = as.character(Var1),
               Var2 = as.character(Var2))
      
      ts_sum_removed <- c()
      geo_sum_removed <- c()
      for (k in 1:dim(grid_removed)[1]) {
        ts_sum_removed <- append(ts_sum_removed, as.matrix(scaled_dist)[grid_removed[k,1],grid_removed[k,2]])
        geo_sum_removed <- append(geo_sum_removed, as.matrix(scaled_geo)[grid_removed[k,1],grid_removed[k,2]])
      }
      
      
      d_ts_removed <-sum(ts_sum_removed)/(2*length(clust_removed))
      d_geo_removed <- sum(geo_sum_removed)/(2*length(clust_removed))
      I_removed <- append(I_removed, (1-a)*d_ts_removed + a*d_geo_removed)
    }
    
    I_cluster[[m]] <- data.frame(district = dist_clust, cluster = m, I_normal = I_normal, I_removed = I_removed) %>% 
      mutate(diff= (I_normal - I_removed),
             diff_sq = diff^2)
    
  }
  return(I_cluster)
}

#######################################################################################
full2_cluster <- filter(cluster_info, cluster_size ==2) %>% 
  left_join(mutate(deseasoned_month_guns, time = case_when(
    month < ymd("2020-01-01") ~ "before",
    month > ymd("2019-12-31") & month < ymd("2021-01-01") ~ "after20",
    month > ymd("2021-12-31") & month < ymd("2024-01-01") ~ "after23")))


for (i in c("eucl", "dtw1", "dtw2", "dtw3")) {
  for (j in seq_along(names(dist_measure))) {
    full <- within_error(full2_cluster, dist_measure[[j]][[i]], dist_measure[[j]]$geo, i, names(dist_measure)[j], 1:2)
    ave <- bind_cols(lapply(full, function(x) sum(x$diff_sq)/length(x$district)))
    names(ave) <- c("1", "2")
    assign(paste0(i, "_within_", names(dist_measure)[j]), full)
    assign(paste0(i, "_within_", names(dist_measure)[j], "_ave"), ave)
  }
}

#####################################################

##########################################
pdf("final_images/firearm/inertia_withinerror_ss_comm2.pdf")
ave_cluster_inertia <- list(before = bind_rows(dtw1 = bind_cols(lapply(dtw1_within_before, function (x) unique(x$I_normal))),
                                               dtw2 = bind_cols(lapply(dtw2_within_before, function (x) unique(x$I_normal))),
                                               dtw3 = bind_cols(lapply(dtw3_within_before, function (x) unique(x$I_normal))),
                                               eucl = bind_cols(lapply(eucl_within_before, function (x) unique(x$I_normal)))) %>% 
                              mutate(time = rep("before", 4),
                                     diss = c("dtw1", "dtw2", "dtw3", "eucl")),
                            after20 = bind_rows(dtw1 = bind_cols(lapply(dtw1_within_after20, function (x) unique(x$I_normal))),
                                                dtw2 = bind_cols(lapply(dtw2_within_after20, function (x) unique(x$I_normal))),
                                                dtw3 = bind_cols(lapply(dtw3_within_after20, function (x) unique(x$I_normal))),
                                                eucl = bind_cols(lapply(eucl_within_after20, function (x) unique(x$I_normal)))) %>% 
                              mutate(time = rep("after20", 4),
                                     diss = c("dtw1", "dtw2", "dtw3","eucl")),
                            after23 = bind_rows(dtw1 = bind_cols(lapply(dtw1_within_after23, function (x) unique(x$I_normal))),
                                                dtw2 = bind_cols(lapply(dtw2_within_after23, function (x) unique(x$I_normal))),
                                                dtw3 = bind_cols(lapply(dtw3_within_after23, function (x) unique(x$I_normal))),
                                                eucl = bind_cols(lapply(eucl_within_after23, function (x) unique(x$I_normal)))) %>% 
                              mutate(time = rep("after23", 4),
                                     diss = c("dtw1", "dtw2", "dtw3", "eucl")))
####inertia
bind_rows(ave_cluster_inertia) %>% 
  rename("1" = "...1", "2" = "...2") %>% 
  pivot_longer(cols = 1:2, names_to = "cluster", values_to = "I_normal") %>% 
  mutate(cluster = factor(cluster),
         diss = factor(diss, labels = c("DTW1", "DTW2", "DTW3", "Euclidean")),
         time =  factor(time, levels = c("before", "after20",  "after23"), labels =  c("2018-2019", "2020",  "2020-2024"))) %>% 
  ggplot(aes(cluster, I_normal, fill = diss)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~time) +
  labs(title = "Inertia per Cluster by Distance Measure for 2 Clusters",
       fill = "Distance Measures",
       x = "Cluster",
       y = "Inertia")

bind_rows(eucl_within_before_ave, eucl_within_after20_ave,  eucl_within_after23_ave,
          dtw1_within_before_ave, dtw1_within_after20_ave,  dtw1_within_after23_ave,
          dtw2_within_before_ave, dtw2_within_after20_ave,  dtw2_within_after23_ave,
          dtw3_within_before_ave, dtw3_within_after20_ave,  dtw3_within_after23_ave) %>% 
  mutate(time = rep(c("before", "after20", "after23"), 4),
         diss = rep(c("eucl","dtw1", "dtw2", "dtw3"), each = 3)) %>% 
  pivot_longer(cols = 1:2, names_to = "cluster", values_to = "error") %>% 
  mutate(diss = factor(diss, labels = c("DTW1", "DTW2", "DTW3",  "Euclidean")),
         time = factor(time, levels = c("before", "after20", "after23"), labels =  c("2018-2019", "2020",  "2020-2024"))) %>% 
  ggplot(aes(cluster, error)) +
  geom_bar(aes(fill = diss), stat = "identity", position = "dodge") +
  facet_wrap(~time) +
  labs(title = "Average Within Cluster Variation for 2 Clusters",
       fill = "Distance Measures",
       x = "Cluster", 
       y = "Difference Between Inertia and Inertia Removed")



##### between sum of squares
bind_rows(ave_cluster_inertia) %>% 
  pivot_longer(cols = 1:2, names_to = "cluster", values_to = "I_normal") %>% 
  mutate(cluster = str_remove_all(cluster, "..."),
         time = factor(time, levels = c("before", "after20", "after23"))) %>% 
  group_by(time, diss) %>% 
  mutate(ave_interia = mean(I_normal, na.rm = T),
         diff_interia = (I_normal - ave_interia)^2) %>% 
  summarise(ss = sum(diff_interia, na.rm = T)) %>% 
  mutate(diss = factor(diss, labels =  c("DTW1", "DTW2", "DTW3",  "Euclidean")),
         time = factor(time, levels = c("before", "after20", "after23"), labels =  c("2018-2019", "2020",  "2020-2024"))) %>% 
  ggplot(aes(diss, ss, fill = time)) +
  geom_bar(stat = "identity", position = "dodge") +
  # coord_cartesian(ylim=c(0, 5)) +
  labs( title = "Sum of Squares by Distance Measure for 2 Clusters",
        x = "Distance Measures",
        y = "Sum of Squares Value",
        fill = "Time Period")
dev.off()


####################################################################################### 3333
full3_cluster <- filter(cluster_info, cluster_size ==3) %>% 
  left_join(mutate(deseasoned_month_guns, time = case_when(
    month < ymd("2020-01-01") ~ "before",
    month > ymd("2019-12-31") & month < ymd("2021-01-01") ~ "after20",
    month > ymd("2021-12-31") & month < ymd("2024-01-01") ~ "after23")))


for (i in c("eucl", "dtw1", "dtw2", "dtw3")) {
  for (j in seq_along(names(dist_measure))) {
    full <- within_error(full3_cluster, dist_measure[[j]][[i]], dist_measure[[j]]$geo, i, names(dist_measure)[j], 1:3)
    ave <- bind_cols(lapply(full, function(x) sum(x$diff_sq)/length(x$district)))
    names(ave) <- c("1", "2", "3")
    assign(paste0(i, "_within_", names(dist_measure)[j]), full)
    assign(paste0(i, "_within_", names(dist_measure)[j], "_ave"), ave)
  }
}


##########################################
pdf("final_images/firearm/inertia_withinerror_ss_comm3.pdf")
ave_cluster_inertia <- list(before = bind_rows(dtw1 = bind_cols(lapply(dtw1_within_before, function (x) unique(x$I_normal))),
                                               dtw2 = bind_cols(lapply(dtw2_within_before, function (x) unique(x$I_normal))),
                                               dtw3 = bind_cols(lapply(dtw3_within_before, function (x) unique(x$I_normal))),
                                               eucl = bind_cols(lapply(eucl_within_before, function (x) unique(x$I_normal)))) %>% 
                              mutate(time = rep("before", 4),
                                     diss = c("dtw1", "dtw2", "dtw3", "eucl")),
                            after20 = bind_rows(dtw1 = bind_cols(lapply(dtw1_within_after20, function (x) unique(x$I_normal))),
                                                dtw2 = bind_cols(lapply(dtw2_within_after20, function (x) unique(x$I_normal))),
                                                dtw3 = bind_cols(lapply(dtw3_within_after20, function (x) unique(x$I_normal))),
                                                eucl = bind_cols(lapply(eucl_within_after20, function (x) unique(x$I_normal)))) %>% 
                              mutate(time = rep("after20", 4),
                                     diss = c("dtw1", "dtw2", "dtw3","eucl")),
                            after23 = bind_rows(dtw1 = bind_cols(lapply(dtw1_within_after23, function (x) unique(x$I_normal))),
                                                dtw2 = bind_cols(lapply(dtw2_within_after23, function (x) unique(x$I_normal))),
                                                dtw3 = bind_cols(lapply(dtw3_within_after23, function (x) unique(x$I_normal))),
                                                eucl = bind_cols(lapply(eucl_within_after23, function (x) unique(x$I_normal)))) %>% 
                              mutate(time = rep("after23", 4),
                                     diss = c("dtw1", "dtw2", "dtw3", "eucl")))
####inertia
bind_rows(ave_cluster_inertia) %>% 
  rename("1" = "...1", "2" = "...2", "3" = "...3") %>% 
  pivot_longer(cols = 1:3, names_to = "cluster", values_to = "I_normal") %>% 
  mutate(cluster = factor(cluster),
         diss = factor(diss, labels = c("DTW1", "DTW2", "DTW3", "Euclidean")),
         time =  factor(time, levels = c("before", "after20", "after23"), labels =  c("2018-2019", "2020",  "2020-2024"))) %>% 
  ggplot(aes(cluster, I_normal, fill = diss)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~time) +
  labs(title = "Inertia per Cluster by Distance Measure for 3 Clusters",
       fill = "Distance Measures",
       x = "Cluster",
       y = "Inertia")

bind_rows(eucl_within_before_ave, eucl_within_after20_ave,  eucl_within_after23_ave,
          dtw1_within_before_ave, dtw1_within_after20_ave,  dtw1_within_after23_ave,
          dtw2_within_before_ave, dtw2_within_after20_ave,  dtw2_within_after23_ave,
          dtw3_within_before_ave, dtw3_within_after20_ave,  dtw3_within_after23_ave) %>% 
  mutate(time = rep(c("before", "after20", "after23"), 4),
         diss = rep(c("eucl","dtw1", "dtw2", "dtw3"), each = 3)) %>% 
  pivot_longer(cols = 1:3, names_to = "cluster", values_to = "error") %>% 
  mutate(diss = factor(diss, labels = c("DTW1", "DTW2", "DTW3",  "Euclidean")),
         time = factor(time, levels = c("before", "after20", "after23"), labels =  c("2018-2019", "2020",  "2020-2024"))) %>% 
  ggplot(aes(cluster, error)) +
  geom_bar(aes(fill = diss), stat = "identity", position = "dodge") +
  facet_wrap(~time) +
  labs(title = "Average Within Cluster Variation for 3 Clusters",
       fill = "Distance Measures",
       x = "Cluster", 
       y = "Difference Between Inertia and Inertia Removed")



##### between sum of squares
bind_rows(ave_cluster_inertia) %>% 
  pivot_longer(cols = 1:3, names_to = "cluster", values_to = "I_normal") %>% 
  mutate(cluster = str_remove_all(cluster, "..."),
         time = factor(time, levels = c("before", "after20", "after23"))) %>% 
  group_by(time, diss) %>% 
  mutate(ave_interia = mean(I_normal, na.rm = T),
         diff_interia = (I_normal - ave_interia)^2) %>% 
  summarise(ss = sum(diff_interia, na.rm = T)) %>% 
  mutate(diss = factor(diss, labels =  c("DTW1", "DTW2", "DTW3",  "Euclidean")),
         time = factor(time, levels = c("before", "after20", "after23"), labels =  c("2018-2019", "2020",  "2020-2023"))) %>% 
  ggplot(aes(diss, ss, fill = time)) +
  geom_bar(stat = "identity", position = "dodge") +
  # coord_cartesian(ylim=c(0, 5)) +
  labs( title = "Sum of Squares by Distance Measure for 3 Clusters",
        x = "Distance Measures",
        y = "Sum of Squares Value",
        fill = "Time Period")
dev.off()
