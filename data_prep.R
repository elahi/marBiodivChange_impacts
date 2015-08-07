#################################################
# Modifying human impacts data
# Original data table from Ginger Allington

# Author: Robin Elahi
# Date: 150805
#################################################

library(dplyr)

siteList <- read.csv("./data/elahi_extractedVals.csv")
unique(siteList$studyName)

sl2 <- siteList[siteList$studyName != "Keller" &
               siteList$studyName != "Bebars" &
               siteList$studyName != "Greenwood" &
               siteList$studyName != "Sonnewald" &
               siteList$studyName != "SwedFishTrawl"  , ]

unique(sl2$studyName)
sl2$impact <- with(sl2, ifelse(Impact.Score == -9999, "no", "yes"))

sl2 <- sl2 %>% rename(Long = Long_, impactScore = Impact.Score, 
                      originalID = OBJECTID..) %>% 
  select(originalID, studyName, Lat, Long, impactScore, impact)
head(sl2)

# write.csv(sl2, './data/elahi_cb_sites.csv')


# Load kml file with nudged sites (only 60, should be 62)
library(rgdal)
library(maptools)
library(ggplot2)
nudged <- readOGR(dsn = "./data/sites_nudged.kml", 
                  layer = "sites_nudged")

nudged <- readOGR(dsn = "./data/sites_nudged.kml", 
                  layer = "sites_nudged", stringsAsFactors = FALSE)

summary(nudged)



nudged2 <- sapply(as(nudged, "data.frame"), class)
nudged2

nudged_coords <- getKMLcoordinates("./data/sites_nudged.kml", 
                                   ignoreAltitude = TRUE)



nudged <- nudged %>% rename(originalID = Description)

nudged.points <- fortify(model = nudged)

