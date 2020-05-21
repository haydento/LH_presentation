library(data.table)
library(glatos)
library(sf)
library(gifski)
library(sp)
library(raster)
library(ggplot2)

###################
# read polygon of LEC
lakes  <- st_read("data/Lec.gpkg", quiet = TRUE, agr = "constant")
lakes <- st_transform(lakes, crs = 3175)
lakes_crop <- st_crop(lakes, c(xmin = 904057, xmax = 937863, ymin = 1034733, ymax = 1057822))

# bring in raster of bathymetery
base_lay <- raster("data/huron_lld.tif")

# convert raster to different extent (crs = 3175)
newproj <- "+proj=aea +lat_1=42.122774 +lat_2=49.01518 +lat_0=45.568977 +lon_0=-83.248627 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
bathy <- projectExtent(base_lay, newproj)
bathy <- projectRaster(base_lay, bathy)

# crop raster to extent of polygon
bathy <- crop(bathy, extent(lakes_crop))

# mask raster to lakes polygon outline (only need bathymetry for areas where water)
bathy <- mask(bathy, lakes_crop)

# make raster bathy layer a data.frame for plotting with ggplot2
bathy <- as.data.frame(bathy, xy=TRUE)
colnames(bathy) <- c("x", "y", "depth_m")

# read in receivers
recs <- read_glatos_receivers("data/GLATOS_receiverLocations_20200109_170415.csv")
setDT(recs)
recs <- recs["LECCI", on = "glatos_project"]
recs[, glatos_array2 := glatos_array][glatos_array == "OUT", glatos_array2 := paste(glatos_array, station_no, sep = "-")]

# read detections
dtc <- read_glatos_detections("data/LECCI_detectionsWithLocs_20200109_170952.csv")
setDT(dtc)

# condense receiver deployment times within receiver lines
recs[glatos_array != "OUT", deploy_date_time_condensed := min(deploy_date_time), by = "glatos_array2"][glatos_array == "OUT", deploy_date_time_condensed := deploy_date_time]

recs[glatos_array != "OUT", recover_date_time_condensed := max(recover_date_time), by = "glatos_array2"][glatos_array == "OUT", recover_date_time_condensed := recover_date_time]

# extract unique receiver deployments
recs <- unique(recs, by = c("glatos_array2", "deploy_date_time_condensed", "recover_date_time_condensed"))

# use overlap join from data.table to identify receivers deployed in each time interval
recs[, start := deploy_date_time_condensed]
recs[, end := recover_date_time_condensed]

# create time vector
t_seq <- seq(from = as.POSIXct("2018-11-01 00:00:00", tz = "UTC"), to = as.POSIXct("2019-10-01 00:00:00", tz = "UTC"), by = "1 month")

# create start and end columns for bins
foo <- data.table(start = t_seq, end = t_seq)
setkey(foo, start, end)
setkey(recs, start, end)

# recs_1 "start" and "end" columns correspond to receviers deployed during that period
recs <- foverlaps(recs, foo)

# remove sync tags
dtc <- dtc[!c("60035_2018__0", "60774_2018__0", "61372_2018__0", "62024_2018__0", "62031_2018__0"), on = "animal_id"]

# identify possible false detections
dtc <- false_detections(dtc, (30*240), show_plot = FALSE)

# remove possible false detections
dtc <- dtc[.(1), on = "passed_filter"]

# create glatos_array2
dtc[, glatos_array2 := glatos_array][glatos_array == "OUT", glatos_array2 := paste(glatos_array, station_no, sep = "-")]

# bin detections by time bins
dtc[, t_bin := t_seq[findInterval(detection_timestamp_utc, t_seq)]]

# create temp dir for writing images
t_dir <- tempdir()


# make plots
for(i in 1:(length(t_seq)-1)){
  recs.i <- recs[start == t_seq[i],]
  dtc.i <- dtc[t_bin == t_seq[i],]


fish_sum <- summarize_detections(det = dtc.i, location_col = "glatos_array2", receiver_locs = recs.i, summ_type = "location")
fish_sum <- fish_sum[, c("glatos_array2", "num_fish", "num_dets")]

coords <- unique(recs.i[, c("glatos_array2", "deploy_lat", "deploy_long")], by = c("glatos_array2", "deploy_lat", "deploy_long"))

fish_sum <- coords[fish_sum, on = "glatos_array2"]

# change 0 to NA
fish_sum[num_fish == 0, num_fish := NA]

# convert dtc to sf for plotting
fish_sum_sf <- st_as_sf(fish_sum, coords = c("deploy_long", "deploy_lat"), crs = 4326, agr = "constant")
fish_sum_sf <- st_transform(fish_sum_sf, crs = 3175)


#####
p <- ggplot(data = bathy) +
  geom_raster(aes(x=x,y=y,fill=depth_m), alpha = 0.7, show.legend = FALSE) +
  geom_sf(data = lakes_crop, color = "black", fill= NA) +
  scale_fill_viridis_c(option="E", na.value = "transparent") +
  
  geom_sf(data = fish_sum_sf, inherit.aes = FALSE, show.legend = TRUE, aes(color = num_fish), size = 10, shape = ifelse(is.na(fish_sum_sf$num_fish), 13, 19), stroke = 2) +
  scale_color_gradient(low = "white", high = "red", na.value = "black", limits = c(1,25)) +
geom_sf(data=fish_sum_sf, inherit.aes = FALSE, show.legend = FALSE, size = 10, shape = 21, color = "black", stroke = 2, fill = NA) +
  theme(panel.grid = element_blank(),
        text = element_text(size = 15),
        axis.text = element_blank(),
         axis.title = element_blank(),
         line = element_blank(),
         rect = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 30)
        ) +
  coord_sf(xlim = c(904057, 937863), ylim = c(1034733, 1049000)) +
  labs(title = format(t_seq[i], "%B %Y"))
 
  ggsave(plot = p, filename = file.path(t_dir, paste0(i, "_", ".png")), dpi = "screen", width = 16, height = 9)

}

library(gifski)

details <- file.info(list.files(t_dir, "*.png", full.names = TRUE), extra_cols = FALSE)
details = details[with(details, order(as.POSIXct(mtime))),]
fls <- rownames(details) 

gifski(fls, gif_file = "images/monthly_dtc.gif", width = 1600, height = 900, delay = 2, loop = TRUE)











