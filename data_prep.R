#################################################
# Applying human impacts data to sites in CB manuscript

# Author: Robin Elahi
# Date: 150807
#################################################

rm(list=ls(all=TRUE))

library(dplyr)
library(rgdal)
library(ggplot2)
library(raster)

# set working directory
setwd("~elahi/github/marBiodivChange_impacts/")

# Load data table of impact scores from Ginger Allington
siteList <- read.csv("./data/elahi_extractedVals.csv")
unique(siteList$studyName)

# Need to remove large-scale studies that were not used in the CB analysis
sl2 <- siteList[siteList$studyName != "Keller" &
               siteList$studyName != "Bebars" &
               siteList$studyName != "Greenwood" &
               siteList$studyName != "Sonnewald" &
               siteList$studyName != "SwedFishTrawl"  , ]

unique(sl2$studyName)

# Categorize impact score or not, rename and select columns
sl2$impact <- with(sl2, ifelse(Impact.Score == -9999, "no", "yes"))
head(sl2)

sl3 <- sl2 %>% rename(Long = Long_, impactScore = Impact.Score, 
                      originalID = OBJECTID..) %>% 
  dplyr::select(originalID, studyName, Lat, Long, impactScore, impact)

head(sl3)

# write.csv(sl3, './data/elahi_cb_sites.csv')

###############################
###############################
# First order of business, as proof of concept
# Can I replicate Ginger's extracted scores?

head(sl3)
with(sl3, table(impact)) 
# 62 sites did not get an impact score, because
# the points were on land or data were NA
# remove these 62

sl4 <- sl3 %>% filter(impact == "yes")
summary(sl4)

# rename file
pointsObject <- sl4
head(pointsObject)

# set spatial coordinates to create a spatial object 
# c(x, y) i.e.,  c(long, lat)
coordinates(pointsObject) <- c("Long", "Lat") 
pointsObject
head(pointsObject)
str(pointsObject)

# set projection reference
projection(pointsObject) <- 
  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Use the same map that Ginger used. 
imp_map <- raster("../bigFiles/nceas_wgs.tif")

# Use the points to extract impact scores from the map
impactsRE <- extract(imp_map, pointsObject)
impactsRE
summary(impactsRE) 

# These scores should be the same...but they are not
qplot(sl4$impactScore, impactsRE) +
  geom_abline(a = 0, b = 1)

qplot(impactsRE)
qplot(sl4$impactScore)

# Plot the raster with the sites
plot(imp_map)
# plot(imp_map, colNA = 'black')
points(pointsObject, pch = 17, col = "black")

pdf("./figs/robinsPlot.pdf")
plot(imp_map)
points(pointsObject, pch = 17, col = "black")
dev.off()









###################################
# Try nudging sites
###################################

# Load kml file with nudged sites (only 60, should be 62)
nudged <- readOGR(dsn = "./data/sites_nudged.kml", 
                  layer = "sites_nudged")
summary(nudged)
nudged

# Change to dataframe
nudgedDF <- as.data.frame(nudged)
nudgedDF
# Rename to match original dataframe
nudgedDF2 <- nudgedDF %>% rename(originalID = Description, 
                                nudgedLong = coords.x1, 
                                nudgedLat = coords.x2)
nudgedDF2

###################################
# Join datasets
glimpse(sl3)
sl3$originalID <- as.factor(sl3$originalID)
glimpse(nudgedDF2)

# Filter out sites with impact scores to figure out which I am missing from the 
# nudged dataset
sl4 <- sl3 %>% filter(impact == "no") %>%
  full_join(nudgedDF2, by = "originalID")

write.csv(sl4, './output/sl4.csv')

# Now I can use sl4 nudged lat longs to get new impact scores

#######################################################################
# Read in Human Impacts tiff file as an object with the formal class 'RasterLayer'
# Tiff file is too big for github, so I call it from a folder outside 
# of the working directory
# (code from Jillian Dunic)
library(raster)

# Load map as is:
# imp_map <- raster("../bigFiles/model_class_wgs84_lzw.tif")
# plot(imp_map)

# Alternatively we can read in the geotiff with each band as an individual 
# raster. This allows us to extract the RGB data from each 'band'
# See an iterative solution here: 
# http://stackoverflow.com/questions/15270107/r-get-a-specific-band-of-a-rasterlayer

imp_map_b1 <- raster("../bigFiles/model_class_wgs84_lzw.tif", band = 1)

imp_map_b2 <- raster("../bigFiles/model_class_wgs84_lzw.tif", band = 2)

imp_map_b3 <- raster("../bigFiles/model_class_wgs84_lzw.tif", band = 3)

# We can subsequently combine these into a stack which allows us to plot using 
# the RGB colour scheme.
imp_stack <- stack(imp_map_b1, imp_map_b2, imp_map_b3)
plotRGB(imp_stack)
imp_stack

# Check the projection just to be sure it's WGS84
projection(imp_stack)

# Test coordinates, with impact level eye-balled off the halpern map and points
# taken off google maps.
# I do not assign a projection because they are coming from google, which uses
# the WGS84 (EPSG: 4326) CRS. 
imp_level <- c('very low', 'low', 'medium', 'very high', 'land', 'sf_land', 'sf_bay')
lat <- c(-74.903173,  -6.680232,  18.728598, 58.438787, 45.213004, 37.22158,  38.065723)
long <- c(-45.068090, -105.823552, -110.122704, -0.357954, -116.556702, -122.202644, -122.392813)
test_vals <- data.frame("imp_level" = imp_level, "lat" = lat, "long" = long)
test_vals

# Convert test_vals into a spatial object
# The assignments specifies which columns have your c(lat, long)
coordinates(test_vals) <- c(3, 2)
projection(test_vals) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
impl <- extract(imp_stack, test_vals, buffer = 2000, small = T)
impl

# Plot the raster stack with the test values. Use plotRGB to plot the rgb 
# defined colours stored in the different bands. 
plotRGB(imp_stack)
plotRGB(imp_stack, colNA = 'black')
points(test_vals)

#######################################################################
# Convert sl4 into a spatial object
# The assignment specifies which columns have your c(long, lat)
head(sl4)
names(sl4)

# rename file
pointsObject <- sl4
head(pointsObject)

# set coordinates - c(x, y) i.e.,  c(long, lat)
coordinates(pointsObject) <- c("nudgedLong", "nudgedLat") 
pointsObject

# set projection reference
projection(pointsObject) <- "+proj=longlat +datum=WGS84 +no_defs 
+ellps=WGS84 +towgs84=0,0,0"

# Load relevant raster
imp_map <- raster("../bigFiles/nceas_wgs.tif")
# Check the projection just to be sure it's WGS84
projection(imp_map)

# Extract impact scores for the points
nudgedImpacts1 <- extract(imp_map, pointsObject)
nudgedImpacts1
summary(nudgedImpacts1)
# still 41 out of 62 NA

nudgedImpacts2 <- extract(imp_map, pointsObject, method = "bilinear")
nudgedImpacts2
qplot(nudgedImpacts1, nudgedImpacts2) +
  geom_abline(a = 0, b = 1)

nudgedImpacts3 <- extract(imp_map, pointsObject, buffer = 3000, 
                          fun = mean, na.rm = TRUE)
nudgedImpacts3
summary(nudgedImpacts3)
qplot(nudgedImpacts1, nudgedImpacts3) +
  geom_abline(a = 0, b = 1)

# Plot the raster with the sites
plot(imp_map)
# plot(imp_map, colNA = 'black')
points(pointsObject, pch = 17, col = "black")
# something is not right with the google kml coords



