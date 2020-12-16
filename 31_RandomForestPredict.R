print('Initializing...')
require(raster)
require(tmap)
require(magrittr)
require(randomForest)


#source('10_ImportingRasters.R')
print('1 of 4')

load('../amazon maximum height extras/randomForestCor80v14042020.Rdata')
heightRaster = predict(layers2estimate, rf.heightAll)
heightRaster = setMinMax(heightRaster)
# writeRaster(heightRaster, filename = '../amazon maximum height extras/rfHeightRasterCor80v14042020.tif')
print('2 of 4')

## Prepare random forest map -----------
amazRegions = shapefile('./data/amazRegions.shp',  encoding="UTF-8")
crs(amazRegions) = crs(amaz)
print('3 of 4')

map = tm_shape(heightRaster) + tm_raster(breaks = c(0, 40, 50, 60, 70, 80, Inf),
            #n = 15,
            palette = "Greens",
            legend.hist = TRUE,
            title = "Maximum height (m)") +
  tm_shape(amazRegions) + tm_borders(col='black') + tm_text('label', size = 1) +
  #tm_shape(amaz) + tm_borders() +
  tm_legend(outside = TRUE, hist.width = 2) +
  tm_grid(lines = FALSE,
          labels.inside.frame = FALSE,
          projection = "+proj=longlat")

tmap_save(map, "./plot/rfHeightRasterCor80v15042020.png", width = 25, height = 15, units = "cm")
rm(map)
print('4 of 4. Done!')