library(glatos)
library(sf)
library(data.table)
library(raster)
library(gganimate)
library(ggplot2)

#setwd("~/Documents/Lake_Huron_cisco/cisco_analyses/glatos_2020")

# read polygon of LEC
lakes  <- st_read("data/Lec.gpkg", quiet = TRUE, agr = "constant")
lakes <- st_transform(lakes, crs = 3175)
lakes_crop <- st_crop(lakes, c(xmin = 904057, xmax = 937863, ymin = 1034733, ymax = 1057822))
lakes_crop <- st_transform(lakes_crop, crs = 4326)

# bring in raster of bathymetery
base_lay <- raster("data/huron_lld.tif")

# read in receivers
recs <- read_glatos_receivers("data/GLATOS_receiverLocations_20200109_170415.csv")
setDT(recs)
recs <- recs["LECCI", on = "glatos_project"]
recs_sf <- st_as_sf(recs, coords = c("deploy_long", "deploy_lat"), crs = 4326, agr = "constant")

#read detections
dtc <- read_glatos_detections("data/LECCI_detectionsWithLocs_20200109_170952.csv")
setDT(dtc)

# remove sync tags
dtc <- dtc[!c("60035_2018__0", "60774_2018__0", "61372_2018__0", "62024_2018__0", "62031_2018__0"), on = "animal_id"]

# identify possible false detections
dtc <- false_detections(dtc, (30*240), show_plot = TRUE)

# remove possible false detections
dtc <- dtc[.(1), on = "passed_filter"]

# calculate unique number of detections on each receiver






# make animation
p <-  ggplot(data = bathy) +
  geom_raster(aes(x=x,y=y,fill=depth_m)) +
  geom_sf(data = lakes_crop, color = "black", fill = NA) +
  scale_fill_viridis_c(option="E", na.value = "transparent") +
  geom_point(data = out, inherit.aes = FALSE, aes(x=X, y=Y, group = what, color = colr, size = size), show.legend = FALSE) +
  theme(panel.grid = element_blank(),
        text = element_text(size = 15),
        axis.text = element_blank(),
         axis.title = element_blank(),
         line = element_blank(),
         rect = element_blank(),
         plot.title = element_text(hjust = 0.5)) +
  coord_sf(xlim = c(904057, 937863), ylim = c(1034733, 1049000))+
  scale_color_manual(values = c('orange' = 'orange', 'red4' = 'red4', "red" = "red")) +
labs(title = "{as.Date(frame_time)}") +
  transition_time(bin_timestamp) +
  shadow_wake(0.03, colour = "black", wrap = FALSE, size=0.1, alpha=0.5)
