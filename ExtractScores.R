#packages

library(raster)
library(rgdal)
library(dismo)
library(maptools)

#####SET UP DATA###############################################
#LOAD POINT FILES 

#don't know if you need this poiece or not, but here's what I started with from a diff dataset

## I don't have the following file
# Samplepts<-readShapePoints("Samplepts.shp")

## Instead, I pulled Vellend's shape file
Samplepts<-readShapePoints("./data/vellendpts.shp") # doesn't work

## Try Dornelas next
Samplepts<-readShapePoints("./data/Dornelas_mar.shp")

# Use file in maptools
Samplepts <- readShapePoints(system.file("shapes/baltim.shp", package="maptools")[1])
plot(xx)
summary(xx)

names(Samplepts)[names(Samplepts)=="Latitude"]<- "y"
names(Samplepts)[names(Samplepts)=="Longitude"]<- "x"
#reorder x & y
Dornxy<- Samplepts[,c(2,1)]

######NCEAS Marine Impacts DATA - Halpern et al.###############################
nceas <- raster("../bigFiles/nceas_wgs.tif")
#MAKE A MASK
mat4<- c(0, 0, NA, 0.00001, 80, 1) #assign 0 to NA
na.mat4<-matrix(mat4, ncol=3, byrow=TRUE) #transform to matrix
n01<- reclassify(nceas, na.mat4)  #apply to raster to reclassify pixels to NA or 1 to make a mask
n.masked<- mask(nceas, n01, maskvalue=0)
#
# reclass to quantiles
quantile(nceas)
nquants <- c(0,0,0, 0.0001,3.957,1, 3.957,6.99,2,  6.99,9.13,3, 9.13,80, 4) 
rcln <- matrix(nquants,ncol=3,byrow=TRUE)
nq <- reclassify(n.masked, rcln) #reclassed to quantiles
#
##################EXTRACT DATA TO POINTS#####################################
#3. Extract Marine Impacts to Dornelas Sample Points
Scoreextr<- extract(nq, Samplepts)      #nq= the raster, Samplepts= the x/y data.
Scoreextr

###################################
###################################
###################################
?readShapePoints

library(maptools)
xx <- readShapePoints(system.file("shapes/baltim.shp", package="maptools")[1])
plot(xx)
summary(xx)
xxx <- xx[xx$PRICE < 40,]
tmpfl <- paste(tempdir(), "xxpts", sep="/")
writePointsShape(xxx, tmpfl)
getinfo.shape(paste(tmpfl, ".shp", sep=""))
axx <- readShapePoints(tmpfl)
plot(axx, col="red", add=TRUE)
unlink(paste(tmpfl, ".*", sep=""))
xx <- readShapePoints(system.file("shapes/pointZ.shp", package="maptools")[1])
dimensions(xx)
plot(xx)
summary(xx)





