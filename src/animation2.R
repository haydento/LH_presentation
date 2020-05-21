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
dtc <- false_detections(dtc, (30*240), show_plot = FALSE)

# remove possible false detections
dtc <- dtc[.(1), on = "passed_filter"]

#lakes_crop <- as(lakes_crop, "Spatial")
#tran <- make_transition(lakes_crop, res = c(0.0001, 0.0001), all_touched = TRUE)
tran <- make_transition3(lakes_crop, res = c(0.0001, 0.0001))

# plot to check
## plot(tran$rast)
## plot(lakes_crop, add = TRUE)
## plot(st_geometry(recs_sf), add = TRUE, col = "red", pch = 16 )

dtc <- dtc[detection_timestamp_utc <= as.POSIXct("2019-06-01 00:00:00", tz = "UTZ") &  animal_id %in% c("11477_2018_F_430", "11479_2018_M_420",  "11480_2018_M_440")]

# interpolate detections
pos2 <- interpolate_path(dtc, trans = tran$transition, start_time = "2018-11-01 00:00:00", int_time_stamp = (3600*12), out_class = NULL, lnl_thresh = 2, show_progress = FALSE)

##pos2 <- interpolate_path2(dtc, trans = tran$transition, start_time = "2018-08-01 00:00:00", int_time_stamp = (86400/3), out_class = NULL)

setDT(pos2)

# prep for animation
int <- unique(pos2[, "bin_timestamp"], by = "bin_timestamp")

# create file of receivers
recs <- recs[int, .(deploy_lat = x.deploy_lat, deploy_long = x.deploy_long, station = x.station, deploy_date_time = x.deploy_date_time, recover_date_time = x.recover_date_time, bin_timestamp = i.bin_timestamp), on = .(deploy_date_time <= bin_timestamp, recover_date_time >= bin_timestamp), allow.cartesian = TRUE]

# convert lakes polygon to crs 3175 
lakes_crop <- st_transform(lakes_crop, crs=3175) 

# convert raster to different extent (crs = 3175)
newproj <- "+proj=aea +lat_1=42.122774 +lat_2=49.01518 +lat_0=45.568977 +lon_0=-83.248627 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
bathy <- projectExtent(base_lay, newproj)
bathy <- projectRaster(base_lay, bathy)

# crop raster to extent of polygon
bathy <- crop(bathy, extent(lakes_crop))

# mask raster to lakes polygon outline (only need bathymetry for areas where water)
bathy <- mask(bathy, lakes_crop)

# convert recs to crs 3175
recs_sf <- st_as_sf(recs, coords = c("deploy_long", "deploy_lat"), agr = "constant", crs = 4326)
recs_sf <- st_transform(recs_sf, crs = 3175)
recs <- data.table(st_coordinates(recs_sf), st_drop_geometry(recs_sf))

# convert crs for interpolated pts
pos_out <- st_as_sf(pos2, coords = c("longitude", "latitude"), agr = "constant", crs = 4326)
pos_out <- st_transform(pos_out, crs = 3175)
pos_out <- data.table(st_coordinates(pos_out), st_drop_geometry(pos_out))

# make raster bathy layer a data.frame for plotting with ggplot2
bathy <- as.data.frame(bathy, xy=TRUE)
colnames(bathy) <- c("x", "y", "depth_m")

# combine recs and output
# recs
foo <- recs[, what := station][,c("X", "Y", "what", "bin_timestamp")][, record_type := "receiver"]

# set colors and sizes
foo[record_type == "receiver", size := 3.2][record_type == "receiver", colr := "black"]

# detections
bar  <- pos_out[, what := animal_id][,c("X", "Y", "what", "bin_timestamp", "record_type")]
bar[record_type == "detection", size := 3.5][record_type == "interpolated", size := 3.5]
bar[record_type == "detection", colr := "red4"][record_type == "interpolated", colr := "red4"]


#
## bin_vec <- unique(bar$bin_timestamp)
## setkey(foo, bin_timestamp)
## setkey(bar, bin_timestamp)

## for( i in 1:100){
##   recs_i <- foo[.(bin_vec[i])]
##   dtc_i <- bar[.(bin_vec[i])]

## p <-  ggplot(data = bathy) +
##   geom_raster(aes(x=x,y=y,fill=depth_m)) +
##   geom_sf(data = lakes_crop, color = "black", fill = NA) +
##   scale_fill_viridis_c(option="E", na.value = "transparent") +

##   geom_point(data = recs_i, inherit.aes = FALSE, aes(x=X, y=Y, color = colr, size = size), show.legend = FALSE) +
##   geom_point(data = dtc_i, inherit.aes = FALSE, aes(x=X, y=Y, group = what, color = colr, size = size), show.legend = FALSE) +

  
##   theme(panel.grid = element_blank(),
##         text = element_text(size = 15),
##         axis.text = element_blank(),
##          axis.title = element_blank(),
##          line = element_blank(),
##          rect = element_blank(),
##          plot.title = element_text(hjust = 0.5)) +
##   coord_sf(xlim = c(904057, 937863), ylim = c(1034733, 1049000))+
##   scale_color_manual(values = c("red" = "red", "red4" = "red4", "orange" = "orange")) +
##   labs(title = as.Date(bin_vec[i]) )

## ggsave(plot = p, filename = paste0("frame_", as.numeric(bin_vec[i]), ".png"), path = "anim", device = "png", dpi = "retina", width = 8, height = 3.5)
## }


## library(gifski)
## file <- list.files(file.path(getwd(), "anim"), full.names = TRUE)
## gifski(file, file.path("anim", "test.gif"))


 ##  +
 ##  transition_time(bin_timestamp) #+
 ## # shadow_wake(0.03, colour = "black", wrap = FALSE, size=0.1, alpha=0.5)

## animate(p, duration = 60, start_pause = 3, end_pause = 5, width = 800, height = 450, nframes = 1000)




  
# combine for gganimate
out <- rbind(foo, bar)
setorder(out, bin_timestamp, -record_type)

# extract two representative fish- overwinter in islands
## out <- out[record_type == "detection" & what %in% c("11477_2018_F_430", "11479_2018_M_420",  "11480_2018_M_440", "11482_2018_M_410", "11466_2018_M_410") | record_type == "receiver"]
## out <- out[record_type == "detection" & what %in% c("11477_2018_F_430") | record_type == "receiver"]
## out <- out[bin_timestamp <= as.POSIXct("2019-06-01 00:00:00", tz = "UTZ")]
#setkey(out, bin_timestamp, record_type)



# make animation
p <-  ggplot(data = bathy) +
  geom_raster(aes(x=x,y=y,fill=depth_m)) +
  geom_sf(data = lakes_crop, color = "black", fill = NA) +
  scale_fill_viridis_c(option="E", na.value = "transparent") +
  geom_point(data = out, inherit.aes = FALSE, aes(x=X, y=Y, group = what, color = colr), size = 3.5, show.legend = FALSE) +
  theme(panel.grid = element_blank(),
        text = element_text(size = 15),
        axis.text = element_blank(),
         axis.title = element_blank(),
         line = element_blank(),
         rect = element_blank(),
         plot.title = element_text(hjust = 0.5)) +
  coord_sf(xlim = c(904057, 937863), ylim = c(1034733, 1049000))+
  scale_color_manual(values = c("red4" = "red4", "black" = "black")) +
labs(title = "{as.Date(frame_time)}") +
  transition_time(bin_timestamp) #+
 # shadow_wake(0.03, colour = "black", wrap = FALSE, size=0.1, alpha=0.5)

## animate(p, duration = 60, start_pause = 3, end_pause = 5, width = 800, height = 450, nframes = 1000)

animate(p, duration = 15, start_pause = 10, end_pause = 10, width = 800, height = 450, fps = 20)

# one minute animation
anim_save(filename = "images/animation2.gif")
#gganimate::animate(p, renderer = file_renderer("output/anim"), device = "png", nframes = 10)

#gganimate::animate(p, renderer = file_renderer("output/anim"), device = "png", nframes = 1000, fps = 1, width = 1000, height = 1000)
#gganimate::animate(p, device = "png", nframes = 100, fps = 10, duration = 60, width = 1000, height = 1000)

## gganimate::anim_save(path = "output", filename = "cisco.gif") 





##################
## make_video2 <- function(dir = getwd(),
##                        pattern = "*.png",
##                        output = "animation.mp4",
##                        fps_in = 30,
##                        start_frame = 1,
##                        end_frame = NULL,
##                        codec = NULL,
##                        vfilter = NULL,
##                        verbose = TRUE,
##                        audio = NULL){

##   files <- list.files(dir, full.names = TRUE, pattern = pattern)

##   #check if dir exists
##   if(!dir.exists(dir)) stop(paste0("Input dir '", dir , "' not found."), 
##                             .call = FALSE)
  
##   # subset out frames if needed
##   if(is.null(end_frame)){end_frame <- length(files)}
##    fls <- files[start_frame:end_frame]

##   if(is.null(vfilter)) {vfilt <- "null"} else {vfilt <- vfilter }
  
##   av::av_encode_video(input = fls, output = output, framerate = fps_in, vfilter = vfilt, codec = codec, verbose = verbose)

##     message("Video file written to ", output, ".")
## }

## ################
## # make video
## # adjust file paths...

## make_video2(dir = "output/anim", output = "output/anim/test.mp4", vfilter="setpts=2*PTS")










## setDT(pos2)
## int <- unique(pos2, by = "bin_timestamp")

## # join int with recs
## recs <- recs[int, .(deploy_lat = x.deploy_lat, deploy_long = x.deploy_long, station = x.station, deploy_date_time = x.deploy_date_time, recover_date_time = x.recover_date_time, bin_timestamp = i.bin_timestamp), on = .(deploy_date_time <= bin_timestamp, recover_date_time >= bin_timestamp), allow.cartesian = TRUE]

## ## # make animation

## p <- ggplot(data = lakes) +
##   geom_sf(color= "black", fill = "lightgrey") +
##   coord_sf(crs = 4326, datum = NA, xlim = c(-84.5, -84.0), ylim = c(45.8, 46.0)) +
##   theme(panel.grid = element_blank(),
##         text = element_text(size = 15),
##         axis.text = element_blank(),
##         axis.title = element_blank(),
##         line = element_blank(),
##         rect = element_blank(),
##         plot.title = element_text(hjust = 0.5)) +
##   geom_point(data = recs, aes(x = deploy_long, y = deploy_lat), size = 2.2, color = "orange", inherit.aes = FALSE) +
##   geom_point(data = pos2, aes(x = longitude, y = latitude, group=animal_id), size = 1.5, color = "red", inherit.aes = FALSE, show.legend = FALSE) +
##   xlab("Longitude") +
##   ylab("Latitude") +
##  # scale_color_manual(values = c("red", "blue")) +
##  # scale_size_manual(values = c(2,1)) +
##   transition_time(bin_timestamp) +
##   ggtitle('{frame_time}')

##  gganimate::animate(p, device = "png", nframes = 100, fps = 10, duration = 60, width = 1000, height = 1000, detail = 2)
## gganimate::anim_save(path = "output", filename = "cisco_move1.gif") 

