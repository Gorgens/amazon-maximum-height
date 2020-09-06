require(ggplot2)
require(raster)
require(tidyverse)
require(tmap)
require(sf)
require(rgdal)
require(sp)
require(gridExtra)
require(magrittr)
require(maptools)

## Importing maximum height dataset -------------------------
amaz = shapefile('./data/amazbioma.shp')
amaz = spTransform(amaz, CRS("+proj=longlat +datum=WGS84"))

amazRegions = shapefile('./data/Morrones.shp',  encoding="UTF-8")
crs(amazRegions) = crs(amaz)
amazRegions = raster::intersect(amazRegions, amaz)


# tm_shape(amaz) +
#   tm_polygons(col="grey", border.col="white") +
#   tm_shape(amazRegions) + tm_borders()

maximas = sf::as_Spatial(st_read(dsn = './data/maximum height 200321.gpkg', layer = 'pontos_wgs84'))
maximas@data = maximas@data %>% select(3)

mapAll = tm_shape(amaz) + tm_polygons() + 
  tm_shape(maximas) + tm_dots("altura_cop", size = 0.1, palette = "RdYlGn")

# tmap_save(mapAll, "./plot/mapAll.png", width = 1920, height = 1080)
print(mapAll)

## Maximum tree height distribution -------------------------
taller50 = maximas[maximas$altura_cop > 50,]
map50 = tm_shape(amazRegions) + tm_polygons(col = "Province_1", title = "Regions", palette = "Pastel1") +
  #tm_shape(amaz) + tm_polygons(col="grey", border.col="white") +
  tm_shape(taller50) + tm_dots("black", size=.1) +
  tm_shape(amazRegions) + tm_borders() +
  tm_layout(title = "trees > 50 m", legend.show=FALSE)
#tm_grid(labels.inside.frame = FALSE, projection = "+proj=longlat", col = 'gray')
rm(taller50)

# tmap_save(map50, "./plot/map50.png", width = 1920, height = 1080)
# print(map50)


taller60 = maximas[maximas$altura_cop > 60,]
map60 = tm_shape(amazRegions) + tm_polygons(col = "Province_1", title = "Regions", palette = "Pastel1") +
  #tm_shape(amaz) + tm_polygons(col="grey", border.col="white") +
  tm_shape(taller60) + tm_dots("black", size=.1) +
  tm_shape(amazRegions) + tm_borders() +
  tm_layout(title = "trees > 60 m", legend.show=FALSE)
#tm_grid(labels.inside.frame = FALSE, projection = "+proj=longlat", col = 'gray')
rm(taller60)

# tmap_save(map60, "./plot/map60.png", width = 1920, height = 1080)
# print(map60)


taller70 = maximas[maximas$altura_cop > 70,]
map70 = tm_shape(amazRegions) + tm_polygons(col = "Province_1", title = "Regions", palette = "Pastel1") +
  #tm_shape(amaz) + tm_polygons(col="grey", border.col="white") +
  tm_shape(taller70) + tm_dots("black", size=.1) +
  tm_shape(amazRegions) + tm_borders() +
  tm_layout(title = "trees > 70 m", legend.show=FALSE)
#tm_grid(labels.inside.frame = FALSE, projection = "+proj=longlat", col = 'gray')
rm(taller70)

# tmap_save(map70, "./plot/map70.png", width = 1920, height = 1080)
# print(map70)


taller80 = maximas[maximas$altura_cop > 80,]

map80 = tm_shape(amazRegions) + tm_polygons(col = "Province_1", title = "Regions", palette = "Pastel1") +
  #tm_shape(amaz) + tm_polygons(col="grey", border.col="white") +
  tm_shape(taller80) + tm_dots("black", size=.1) +
  tm_shape(amazRegions) + tm_borders() +
  tm_layout(title = "trees > 80 m", legend.show=FALSE)
#tm_grid(labels.inside.frame = FALSE, projection = "+proj=longlat", col = 'gray')
rm(taller80)

# tmap_save(map80, "./plot/map80.png", width = 1920, height = 1080)
# print(map80)


panelHeightDist = tmap_arrange(map50, map60, map70, map80, ncol=2)
# tmap_save(panelHeightDist, "./plot/panelHeightDistv15042020.png", width = 25, height = 25, units = "cm")
print(panelHeightDist)