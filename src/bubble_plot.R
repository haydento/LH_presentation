library(data.table)
library(glatos)
library(sf)
library(gifski)
library(sp)

#####################################33
# read polygon of LEC
lakes  <- st_read("~/Documents/Lake_Huron_cisco/cisco_analyses/2020_LHTC_talk/data/Lec.gpkg")
                      
#read detections
dtc <- read_glatos_detections("data/LECCI_detectionsWithLocs_20200109_170952.csv")
setDT(dtc)

# remove sync tags
setkey(dtc, animal_id)
dtc <- dtc[!c("60035_2018__0", "60774_2018__0", "61372_2018__0", "62024_2018__0", "62031_2018__0")]

# identify possible false detections
dtc <- false_detections(dtc, (30*240), show_plot = TRUE)

# remove possible false detections
setkey(dtc, passed_filter)
dtc <- dtc[.(1)]

# read in receiver locations
recs <- read_glatos_receivers("data/GLATOS_receiverLocations_20200109_170415.csv")
setDT(recs)

# remove receivers except in the LECCI project
setkey(recs, glatos_project)
recs <- recs["LECCI"]
#recs <- st_as_sf(recs, coords = c("deploy_long", "deploy_lat"))

recs[, glatos_array2 := glatos_array][glatos_array == "OUT", glatos_array2 := paste(glatos_array, station_no, sep = "-")]

####
dtc[, glatos_array2 := glatos_array][glatos_array == "OUT", glatos_array2 := paste(glatos_array, station_no, sep = "-")]

# make bubble plot
# add receivers by time..

#t_seq <- seq(from = as.POSIXct("2018-09-01 00:00:00", tz = "UTC"), to = as.POSIXct("2020-08-01 00:00:00", tz = "UTC"), by = "3 months")
t_seq <- seq(from = as.POSIXct("2018-11-01 00:00:00", tz = "UTC"), to = as.POSIXct("2019-10-01 00:00:00", tz = "UTC"), by = "1 month")

dtc[, t_bin := t_seq[findInterval(detection_timestamp_utc, t_seq)]]

recs[glatos_array != "OUT", deploy_date_time_condensed := min(deploy_date_time), by = "glatos_array2"][glatos_array == "OUT", deploy_date_time_condensed := deploy_date_time]

recs[glatos_array != "OUT", recover_date_time_condensed := max(recover_date_time), by = "glatos_array2"][glatos_array == "OUT", recover_date_time_condensed := recover_date_time]

recs <- unique(recs, by = c("glatos_array2", "deploy_date_time_condensed", "recover_date_time_condensed"))


#fwrite(recs, "check.csv")


# use overlap join from data.table to identify receivers deployed in each time interval
recs[, start := deploy_date_time_condensed]
recs[, end := recover_date_time_condensed]

# create start and end columns for bins
foo <- data.table(start = t_seq, end = t_seq)
setkey(foo, start, end)
setkey(recs, start, end)

# recs_1 "start" and "end" columns correspond to receviers deployed during that period
recs_1 <- foverlaps(recs, foo)

# make plots
for(i in 1:(length(t_seq)-1)){
  recs.i <- recs_1[start == t_seq[i],]
  dtc.i <- dtc[t_bin == t_seq[i],]

  custom_bubble(det = dtc.i, location_col = "glatos_array2", receiver_locs = recs.i, map = NULL, col_grad = c("white", "red"),
                symbol_radius = 1.5, background_ylim = c(45.8, 46.05), background_xlim = c(-84.5,-84.0),
                scale_loc = c(-84.02, 45.80, -84.0, 45.92), out_file = paste0("output/gif/", "dist_", dtc.i$t_bin[1], ".png"))
  
}

# create gif
foo <- list.files(path = "~/Documents/Lake_Huron_cisco/cisco_analyses/glatos_2020/output/gif", full.names = TRUE)

gifski(foo, gif_file = "~/Documents/Lake_Huron_cisco/presentations/GLATOS_2020_xar/bar.gif", width = 1600, height = 900, delay = 1, loop = TRUE)

#####
# how many fish detected on receivers in each time bin?
unique_fish <- dtc[, .(num_fish = uniqueN(animal_id)), by = c("glatos_array2")]

# outer receivers
unique_fish[glatos_array2 %in% c("OUT-1", "OUT-3", "OUT-5", "OUT-7", "OUT-8", "OUT-10")]

# inner row of receivers
unique_fish[glatos_array2 %in% c("OUT-2", "OUT-4", "OUT-6", "OUT-9")]

