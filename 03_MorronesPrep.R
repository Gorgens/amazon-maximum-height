require(raster)
require(tidyverse)
require(tmap)
require(sf)
require(rgdal)
require(sp)
require(maptools)

amaz = shapefile('./data/amazbioma.shp')
amaz = spTransform(amaz, CRS("+proj=longlat +datum=WGS84"))

morrones = shapefile('./data/Morrones.shp',  encoding="UTF-8")
morrones <- spTransform(morrones, CRS("+proj=longlat +datum=WGS84"))

tm_shape(amaz) +
  tm_polygons(col="grey", border.col="white") +
  tm_shape(morrones) + tm_borders()

amazRegions = raster::intersect(amaz, morrones)
amazRegions@data$label = c("cerrado", "I", "II", "III", "IV",
                           "V", "VI", "VII", "VIII")
amazRegions = subset(amazRegions, label != "cerrado")

writeSpatialShape(amazRegions, "./data/amazRegions")

tm_shape(amaz) + tm_borders() +
  tm_shape(amazRegions) + tm_polygons(col = "label") +
  tm_text("label", size = 1/2)


rm(morrones)