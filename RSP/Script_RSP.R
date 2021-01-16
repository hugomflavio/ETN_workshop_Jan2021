#----------------------------------#
# Lake Macquarie acoustic tracking #
#----------------------------------#
library(RSP)
library(actel)
library(raster)
library(ggplot2)
library(patchwork)
library(ggsn)
library(cmocean)


#=================================#
# 1. Prepare the data using actel #
#=================================#

# Process acoustic data
study.data <- explore(tz = "Australia/NSW", inactive.error = 10, save.tables.locally = TRUE)
y # reuse detections?
n # save strays?
y # invalidate/expand moves in A69-9004-483?
33
y # confirm?
n # invalidate more?
y # invalidate/expand moves in A69-9004-492?
1:186
y # confirm?
n # invalidate/expand moves in A69-9004-493?
y # invalidate/expand moves in A69-9004-494?
1:208
y # confirm?
n # invalidate/expand moves in A69-9004-495?
y # invalidate/expand moves in A69-9004-496?
1:757
y # confirm?
n # invalidate/expand moves in A69-9004-497?
n # invalidate/expand moves in A69-9002-10473?
y # invalidate/expand moves in A69-9002-10474?
107
y # confirm?
n # invalidate more?
n # invalidate/expand moves in A69-9002-10480?
n # save results?
n # do not save copy of analysis to log

spatial <- loadSpatial()
head(spatial)

# Import river shapefile
water <- loadShape(shape = "Lake_Macquarie.shp", size = 0.0002, buffer = 0.05,
									 coord.x = "Longitude", coord.y = "Latitude")

# Create a transition layer with 8 directions (the default is 16)
tl <- transitionLayer(water, directions = 8)


#==============================#
# 2. Analyse the data with RSP #
#==============================#

#========================================#
# 2.1. Recreating the paths inside water #
#========================================#

# Check that with imported shapefile stations are all inside the water
plotRaster(input = study.data, base.raster = water, coord.x = "Longitude", coord.y = "Latitude")

# Recaptures dataset:
recap <- read.csv("recaptures.csv")
recap
plotRaster(input = study.data, base.raster = water, coord.x = "Longitude", coord.y = "Latitude") +
	addRecaptures(Signal = 485, fill = "green")

# Run RSP analysis
?runRSP # You can set up the analysis the way you need!
rsp.data <- runRSP(input = study.data, t.layer = tl, coord.x = "Longitude", coord.y = "Latitude", verbose = TRUE, 
	recaptures = TRUE) # Adding recapture locations as input for RSP: default = FALSE!
load("rsp.data.Rdata") # Load RSP analysis because it takes a long time to run
rsp.data$bio # Biometrics file

# Check overall distances travelled by biological group
distance.data <- getDistances(rsp.data)
plotDistances(distance.data, group = "A")
plotDistances(distance.data, group = "B", compare = FALSE) # Only the RSP distances! 

# Inspect runRSP output
names(rsp.data) 
rsp.data$track$`A69-9004-481` # Some tracks were INVALID!
rsp.data$track$`A69-9004-489`

# Example detection output
rsp.data$detections$`A69-9004-489`[1:11, ]

# Plot RSP track
rsp.data$track$`A69-9002-10474` 
plotTracks(input = rsp.data, base.raster = water, tag = "A69-9002-10474", track = 4)
plotTracks(input = rsp.data, base.raster = water, tag = "A69-9002-10474", track = 5) + 
	addStations(input = rsp.data) # Adds the acoustic stations to the plot

rsp.data$track$`A69-9004-485` 
recap # Recaptures on Track 3 and Track 6: First.time = Recapture.date
plotTracks(input = rsp.data, base.raster = water, tag = "A69-9004-485", track = 3) + 
	addStations(rsp.data) + addRecaptures(Signal = "485") # Also adds the recapture locations
plotTracks(input = rsp.data, base.raster = water, tag = "A69-9004-485", track = 6) + 
  	addStations(rsp.data) + addRecaptures(Signal = "485") 

# Inspect RSP output when recaptures are included:
rsp.data$detections$`A69-9004-485`[90:150, ]


#====================================================#
# 2.2. Apply dynamic Brownian Bridge Movement Models #
#====================================================#

#==============================================================================#
# 2.2.1. Running a group analysis (overall movements over the tracking period) #
#==============================================================================#
dbbmm.total <- dynBBMM(input = rsp.data, base.raster = water, UTM = 56, verbose = TRUE, 
  start.time = "2014-11-01 00:00:00", stop.time = "2014-11-05 00:00:00") # Choose part of the tracking period

names(dbbmm.total)
dbbmm.total$valid.tracks

# Plot dBBMM models
plotContours(input = dbbmm.total, tag = "A69-9004-489") # Group A dBBMM
plotContours(input = dbbmm.total, tag = "A69-9002-10480", 
	land.col = "#55903b60", col = cmocean('solar')(4)) # Group B dBBMM: custom colours
plotContours(input = dbbmm.total, tag = "A69-9004-485", land.col = "#55903b60",
	title = "Animal 485", scale.type = "continuous") # Group B dBBMM: levels with a continuous scale

# RSP plots are ggplot2! :) 
plotContours(input = dbbmm.total, tag = "A69-9002-10480", land.col = "#55903b60", title = "Group B: animal 10480") +
	ggsn::scalebar(x.min = 151.5, x.max = 151.55, y.min = -32.95, y.max = -32.92, transform = TRUE, 
      dist_unit = "km", dist = 5, st.dist = 0.4, st.size = 3, height = 0.2, border.size = 0.5)

plotContours(input = dbbmm.total, tag = "A69-9002-10480", land.col = "#55903b60", title = "2014-11-01 | 2014-11-05",
	breaks = c(0.99, 0.95, 0.90, 0.70, 0.50, 0.25, 0.20), col = rev(cmocean('thermal')(7))) +
	ggsn::scalebar(x.min = 151.5, x.max = 151.55, y.min = -32.95, y.max = -32.92, transform = TRUE, 
      dist_unit = "km", dist = 5, st.dist = 0.4, st.size = 3, height = 0.2, border.size = 0.5)


	# Sorry but I hated your plotting options...no worries, you can create your own! :)
	dbbmm.total$group.rasters$B$A69.9002.10480_Track_05
	aux.raster <- dbbmm.total$group.rasters$B$A69.9002.10480_Track_05
	plot(aux.raster) # UTM projection (dBBMM!)
	aux.raster.latlon <- projectRaster(aux.raster, crs = CRS("+proj=longlat +datum=WGS84 +no_defs ")) # Reproject to latlong!
	plot(aux.raster.latlon)

	raster.df <- as.data.frame(aux.raster.latlon, xy = TRUE) # Convert raster to a dataframe
	names(raster.df) <- c("x", "y", "dBBMM")
	raster.df <- raster.df[-which(is.na(raster.df$dBBMM) == TRUE), ] # Remove empty values
	raster.df <- subset(raster.df, dBBMM <= 0.99) # Select better values to plot

	Lake <- shapefile("Lake_Macquarie.shp") # Load shapefile for plotting land contour

	ggplot() + theme_minimal() +
		geom_raster(data = raster.df, aes(x = x, y = y, fill = dBBMM)) +
		scale_fill_gradientn(colors = rev(cmocean('thermal')(100))) +
		geom_polygon(data = Lake, aes(x = long, y = lat), color = NA, fill = "#38bd8d40", alpha = 0.5, size = 0.2) +
		coord_cartesian(xlim = c(151.47, 151.7), ylim = c(-33.18, -32.9), expand = FALSE) +
		labs(x = "Longitude", y = "Latitude", title = "A69-9002-10480 (2014-11-01 | 2014-11-05)") +
		ggsn::scalebar(x.min = 151.52, x.max = 151.57, y.min = -32.95, y.max = -32.92, transform = TRUE, 
    		  dist_unit = "km", dist = 4, st.dist = 0.4, st.size = 3, height = 0.2, border.size = 0.5)


# Get total areas of space use at different resolutions: track and group 
dbbmm.total$valid.tracks 
areas.track <- getAreas(dbbmm.total, type = "track")
areas.track$areas

areas.tot <- getAreas(dbbmm.total, type = "group", breaks = c(0.25, 0.5, 0.95))
areas.tot$areas

plotAreas(areas.tot, base.raster = water, group = "A")
plotAreas(areas.tot, base.raster = water, group = "B")


# Analyse overlaps between two biological groups: only when getAreas is of type = "group"!
over.tot <- getOverlaps(areas.tot)
over.tot$areas$`0.25`
over.tot$areas$`0.5`
over.tot$areas$`0.95`

plotOverlaps(over.tot, areas = areas.tot, base.raster = water, groups = c("A", "B"), 
	level = 0.5, title = "Overlap at 50% dBBMM")


#====================================================================================#
# 2.2.2. Running a timeslot analysis (analysis is run over fixed temporal intervals) #
#====================================================================================#
dbbmm.daily <- dynBBMM(input = rsp.data, base.raster = water, UTM = 56, verbose = TRUE, 
  timeframe = 24, # Apply dBBMM according to 24h intervals
  start.time = "2014-11-01 00:00:00", stop.time = "2014-11-05 00:00:00")

dbbmm.daily$timeslots    # Metadata on timeslots 
dbbmm.daily$valid.tracks # Tracks are broken into periods with a maximum of 24h

# Plot dBBMM models: specify timeslot to plot!
plot1 <- plotContours(input = dbbmm.daily, tag = "A69-9004-485", timeslot = 1, 
	title = "Tag 485 (2014-11-01)", scale.type = "continuous") +
	ggsn::scalebar(x.min = 151.5, x.max = 151.55, y.min = -32.95, y.max = -32.92, transform = TRUE, 
    		  dist_unit = "km", dist = 5, st.dist = 0.4, st.size = 3, height = 0.2, border.size = 0.5)
plot2 <- plotContours(input = dbbmm.daily, tag = "A69-9004-485", timeslot = 2, 
	title = "Tag 485 (2014-11-02)", scale.type = "continuous") +
	ggsn::scalebar(x.min = 151.5, x.max = 151.55, y.min = -32.95, y.max = -32.92, transform = TRUE, 
	    	  dist_unit = "km", dist = 5, st.dist = 0.4, st.size = 3, height = 0.2, border.size = 0.5)
plot3 <- plotContours(input = dbbmm.daily, tag = "A69-9004-485", timeslot = 3, 
	title = "Tag 485 (2014-11-03)", scale.type = "continuous") +
	ggsn::scalebar(x.min = 151.5, x.max = 151.55, y.min = -32.95, y.max = -32.92, transform = TRUE, 
	    	  dist_unit = "km", dist = 5, st.dist = 0.4, st.size = 3, height = 0.2, border.size = 0.5)
plot4 <- plotContours(input = dbbmm.daily, tag = "A69-9004-485", timeslot = 4, 
	title = "Tag 485 (2014-11-04)", scale.type = "continuous") +
	ggsn::scalebar(x.min = 151.5, x.max = 151.55, y.min = -32.95, y.max = -32.92, transform = TRUE, 
	    	  dist_unit = "km", dist = 5, st.dist = 0.4, st.size = 3, height = 0.2, border.size = 0.5)
(plot1 + plot2) / (plot3 + plot4)


# All rasters are also saved and can be accessed similarly to a group analysis:
dbbmm.daily$group.rasters$B$`1`$A69.9004.485_Track_06
plot(dbbmm.daily$group.rasters$B$`1`$A69.9004.485_Track_06)


# Get daily areas of space use at the group levels
areas.daily <- getAreas(dbbmm.daily, type = "group")
areas.daily$areas

plot1 <- plotAreas(areas.daily, base.raster = water, group = "A", timeslot = 1, land.col = "#55903b60",
	title = "Group A (2014-11-01)")
plot2 <- plotAreas(areas.daily, base.raster = water, group = "B", timeslot = 1, land.col = "#55903b60", 
	title = "Group B (2014-11-01)")
plot1 + plot2


# Analyze movements in dBBMM contour locations: centroid locations of UDs
df.centroid.B <- getCentroids(input = dbbmm.daily, areas = areas.daily, type = "group", level = 0.5, group = "B", UTM = 56)
df.centroid.B

	# Check centroid location:
	plotAreas(areas.daily, base.raster = water, group = "B", timeslot = 2, title = "Group B (2014-11-02)") +
		addCentroids(input = df.centroid.B, timeslot = 2)

# Obtain dBBMM centroids also at track level: but addCentroids only works for group! 
areas.track <- getAreas(dbbmm.daily, type = "track")
areas.track$areas

df.centroid.track <- getCentroids(input = dbbmm.daily, areas = areas.track, type = "track", level = 0.5, UTM = 56)
df.centroid.track


# Analyse overlaps between two biological groups (this time in both space and time!)
over.daily <- getOverlaps(areas.daily)
over.daily$areas$`0.5`$percentage # 97.14% overlap on 2014-11-04!

plotOverlaps(over.daily, areas = areas.daily, base.raster = water, groups = c("A", "B"), land.col = "#55903b60",
	level = 0.5, timeslot = 4, title = "Overlap at 50% dBBMM on 2014-11-04")


# Extract overlap data for further investigation:
df.aux.5 <- getOverlapData(input = over.daily, dbbmm = dbbmm.daily, groups = c("A", "B"), level = 0.5)
df.aux.5
df.aux.95 <- getOverlapData(input = over.daily, dbbmm = dbbmm.daily, groups = c("A", "B"), level = 0.95)
df.aux.95

df.aux.5$Level <- "50%"
df.aux.95$Level <- "95%"
df.aux <- rbind(df.aux.5, df.aux.95)
df.aux

ggplot() + theme_bw() +
	geom_line(data = df.aux, aes(x = start, y = Absolute_A_B, colour = Level)) +
	labs(y = expression(paste("Absolute overlap betwwen A and B (m"^"2", ")")), x = "Date")
ggplot() + theme_bw() +
	geom_line(data = df.aux, aes(x = start, y = Percentage_A_B, colour = Level)) +
	labs(y = "Percentage overlap between A and B (%)", x = "Date")
