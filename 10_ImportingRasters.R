## Open required packages
require(ggplot2)
require(raster)
require(sf)
require(rgdal)
require(tidyverse)
require(tmap)
require(gridExtra)
require(factoextra) 

## Importing raster files ---------------------------
amaz = shapefile('./data/amazbioma.shp')


## u-speed ---------------------------
uspeed = raster('./data/uspeed.tif')
uspeed = setMinMax(uspeed)
ref = uspeed

# map = tm_shape(uspeed) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/uspeed_image.png", width = 1920, height = 1080)

## v-speed ---------------------------

vspeed = raster('./data/vspeed.tif') %>% crop(ref)
vspeed = raster(vals=values(vspeed),
                ext=extent(ref), 
                nrows=dim(ref)[1],
                ncols=dim(ref)[2])
vspeed = setMinMax(vspeed)

# map = tm_shape(vspeed) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/vspeed_image.png", width = 1920, height = 1080)

## Lightning ---------------------------
lightning = raster('./data/lightning.tif') %>% crop(ref)
lightning = raster(vals=values(lightning),
                   ext=extent(ref), 
                   nrows=dim(ref)[1],
                   ncols=dim(ref)[2])
lightning = setMinMax(lightning)

# map = tm_shape(lightning) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/lightning_image.png", width = 1920, height = 1080)

## Days with more than 20 mm ---------------------------
days20 = raster('./data/days20.tif') %>% crop(ref)
days20 = raster(vals=values(days20),
                ext=extent(ref), 
                nrows=dim(ref)[1],
                ncols=dim(ref)[2])
days20 = setMinMax(days20)

# map = tm_shape(days20) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/days20_image.png", width = 1920, height = 1080)

## SRTM ---------------------------
srtm = raster('./data/srtm.tif') %>% crop(ref)
srtm = raster(vals=values(srtm),
              ext=extent(ref), 
              nrows=dim(ref)[1],
              ncols=dim(ref)[2])

srtm[srtm <= 0] = NA
srtm[srtm >= 1000] = NA
srtm = setMinMax(srtm)

# map = tm_shape(srtm) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("darkolivegreen4","yellow", "brown"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/srtm_image.png", width = 1920, height = 1080)


## Potential Evapotranspiration ---------------------------
pet = raster('./data/pet.tif') %>% crop(ref)
pet = raster(vals=values(pet),
             ext=extent(ref), 
             nrows=dim(ref)[1],
             ncols=dim(ref)[2])
pet = setMinMax(pet)

# map = tm_shape(pet) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/pet_image.png", width = 1920, height = 1080)

## FAPAR ---------------------------
# fapar = raster('./data/fapar.tif') %>% crop(ref)
# summary(getValues(fapar))
# tm_shape(fapar) + tm_raster(n = 10)
# 
# fill.na <- function(x) {
#   center = 0.5 + (width*width/2) 
#   if( is.na(x)[center] ) {
#     return( median(x, na.rm=TRUE) )
#   } else {
#     return( x[center] )
#   }
# } 
# 
# fapar2 = faparteste
# before = summary(getValues(fapar2))[7]
# print(before)
# for(i in seq(1, 10, 1)){
#   width = 9
#   fapar2 = focal(fapar2, w = matrix(1,width,width), fun = fill.na, pad = TRUE, na.rm = FALSE) %>% crop(ref)
#   after = summary(getValues(fapar2))[7]
#   print(after)
# }
# 
# fapar2 = raster(vals=values(fapar2),
#                 ext=extent(ref), 
#                 nrows=dim(ref)[1],
#                 ncols=dim(ref)[2])
# fapar2 = setMinMax(fapar2)
# 
# writeRaster(fapar2, "./data/faparProcessed.tif", overwrite=TRUE)
# 
# tm_shape(fapar2) + tm_raster(n = 10)

fapar = raster('./data/fapar.tif') %>% crop(ref)

fapar = raster(vals=values(fapar),
               ext=extent(ref), 
               nrows=dim(ref)[1],
               ncols=dim(ref)[2])
fapar = setMinMax(fapar)

# map = tm_shape(fapar) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/fapar_image.png", width = 1920, height = 1080)

fapar2 = raster('./data/faparProcessed.tif') %>% crop(ref)

fapar2 = raster(vals=values(fapar2),
                ext=extent(ref), 
                nrows=dim(ref)[1],
                ncols=dim(ref)[2])
fapar2 = setMinMax(fapar2)


## Clear days ---------------------------
clearDays = raster('./data/clearDays.tif') %>% crop(ref)
clearDays = raster(vals=values(clearDays),
                   ext=extent(ref), 
                   nrows=dim(ref)[1],
                   ncols=dim(ref)[2])
clearDays = setMinMax(clearDays)

# map = tm_shape(clearDays) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/clearDays_image.png", width = 1920, height = 1080)

## Month with less than 100 mm ---------------------------
# month100 = raster('./data/month100.tif') %>% crop(ref)
# month100 = raster(vals=values(month100),
#                   ext=extent(ref), 
#                   nrows=dim(ref)[1],
#                   ncols=dim(ref)[2])
# month100 = setMinMax(month100)

# map = tm_shape(month100) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/month100_image.png", width = 1920, height = 1080)

## Annual precipitation ---------------------------
pannual = raster('./data/pannual.tif') %>% crop(ref)
pannual = raster(vals=values(pannual),
                 ext=extent(ref), 
                 nrows=dim(ref)[1],
                 ncols=dim(ref)[2])
pannual = setMinMax(pannual)

# map = tm_shape(pannual) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/pannual_image.png", width = 1920, height = 1080)


## Precipitation of the driest month ---------------------------
# pdriest = raster('./data/pdriest.tif') %>% crop(ref)
# pdriest = raster(vals=values(pdriest),
#                  ext=extent(ref), 
#                  nrows=dim(ref)[1],
#                  ncols=dim(ref)[2])
# pdriest = setMinMax(pdriest)

# map = tm_shape(pdriest) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/pdriest_image.png", width = 1920, height = 1080)



## Precipitation of the wettest month ---------------------------
pwettest = raster('./data/pwettest.tif') %>% crop(ref)
pwettest = raster(vals=values(pwettest),
                  ext=extent(ref), 
                  nrows=dim(ref)[1],
                  ncols=dim(ref)[2])
pwettest = setMinMax(pwettest)

# map = tm_shape(pwettest) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/pwettest_image.png", width = 1920, height = 1080)


## Precipitation seasonality ---------------------------
pseason = raster('./data/pseason.tif') %>% crop(ref)
pseason = raster(vals=values(pseason),
                 ext=extent(ref), 
                 nrows=dim(ref)[1],
                 ncols=dim(ref)[2])
pseason = setMinMax(pseason)

# map = tm_shape(pseason) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/pseason_image.png", width = 1920, height = 1080)


## Temperature seasonality ---------------------------
tseason = raster('./data/tseason.tif') %>% crop(ref)
tseason = raster(vals=values(tseason),
                 ext=extent(ref), 
                 nrows=dim(ref)[1],
                 ncols=dim(ref)[2])
tseason = setMinMax(tseason)

# map = tm_shape(tseason) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/tseason_image.png", width = 1920, height = 1080)



## Annual temperature ---------------------------
tannual = raster('./data/tannual.tif') %>% crop(ref)
tannual = raster(vals=values(tannual),
                 ext=extent(ref), 
                 nrows=dim(ref)[1],
                 ncols=dim(ref)[2])
tannual = setMinMax(tannual)

# map = tm_shape(tannual) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/tannual_image.png", width = 1920, height = 1080)


## Maximum temperature ---------------------------
tmax = raster('./data/tmax.tif') %>% crop(ref)
tmax = raster(vals=values(tmax),
              ext=extent(ref), 
              nrows=dim(ref)[1],
              ncols=dim(ref)[2])
tmax = setMinMax(tmax)

# map = tm_shape(tmax) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/tmax_image.png", width = 1920, height = 1080)


## Clay content ---------------------------
clayContent = raster('./data/clayContent30m.tif') %>% crop(ref)
clayContent = raster(vals=values(clayContent),
                     ext=extent(ref), 
                     nrows=dim(ref)[1],
                     ncols=dim(ref)[2])
clayContent = setMinMax(clayContent)
clayContent[clayContent <= 0] = NA  

# map = tm_shape(clayContent) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/clayContent_image.png", width = 1920, height = 1080)

## Water content ---------------------------
waterContent = raster('./data/waterContent30m.tif') %>% crop(ref)
waterContent = raster(vals=values(waterContent),
                      ext=extent(ref), 
                      nrows=dim(ref)[1],
                      ncols=dim(ref)[2])
waterContent = setMinMax(waterContent)
waterContent[waterContent <= 0] = NA  

# map = tm_shape(waterContent) + 
#   tm_raster(n = 15,
#             palette = colorRampPalette( c("blue","yellow", "red"))(12),
#             legend.hist = TRUE) +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/waterContent_image.png", width = 1920, height = 1080)


## Stack das variáveis ambientais ---------------------------
layers = stack(fapar, 
               srtm, 
               uspeed, 
               vspeed, 
               clearDays, 
               days20, 
               lightning, 
               #month100, 
               pannual, 
               #pdriest, 
               pet, 
               pseason, 
               pwettest, 
               tannual, 
               tseason, 
               tmax, 
               clayContent, 
               waterContent)

layers2estimate = stack(fapar2, 
                        srtm, 
                        uspeed, 
                        vspeed, 
                        clearDays, 
                        days20, 
                        lightning, 
                        #month100, 
                        pannual, 
                        #pdriest, 
                        pet, 
                        pseason, 
                        pwettest, 
                        tannual, 
                        tseason, 
                        tmax, 
                        clayContent, 
                        waterContent)

# save(layers2estimate, 
#     file = 'C:/Users/gorge/Documents/GIS DataBase/amazon maximum height extras/objects/layers2estimate.Rdata')

# atualiza nome das camadas
names(layers) = c("fapar", 
                  "srtm", 
                  "uspeed", 
                  "vspeed", 
                  "clearDays", 
                  "days20",
                  "lightning",
                  #"month100",
                  "pannual",
                  #"pdriest",
                  "pet",
                  "pseason",
                  "pwettest",
                  "tannual",
                  "tseason",
                  "tmax",
                  "clayContent",
                  "waterContent")

names(layers2estimate) = c("fapar", 
                           "srtm",
                           "uspeed", 
                           "vspeed",
                           "clearDays",
                           "days20",
                           "lightning",
                           #"month100",
                           "pannual",
                           #"pdriest",
                           "pet",
                           "pseason",
                           "pwettest",
                           "tannual",
                           "tseason",
                           "tmax",
                           "clayContent",
                           "waterContent")

## Cliping stack by maximum height location ---------------------------
maximas = sf::as_Spatial(st_read(dsn = './data/maximum height 200321.gpkg', layer = 'pontos_wgs84'))
maximas@data = maximas@data %>% select(3)

# tm_shape(amaz) + tm_polygons() + 
#   tm_shape(maximas) + tm_dots("altura_cop", size = 0.2, palette = "RdYlGn")

explanatoryVariables = raster::extract(layers, maximas)                        # extrai valor das variáveis ambientais para cada altura máxima mapeada
maximas@data = cbind(maximas@data, explanatoryVariables)                       # adiciona no shapefile das alturas máximas os valores das variáveis ambientais extraídas
#writeSpatialShape(maximas, "./data/maximasAndFactors")                        # salva shapefile com os atributos clipados dos rasters

## Limpa memória ------------------------------
rm(explanatoryVariables)
rm(layers)
rm(srtm, uspeed, vspeed, clearDays, days20, lightning, month100, pannual, pdriest, pet, pseason, pwettest, tannual, tseason, tmax, clayContent, waterContent, fapar, fapar2, ref)
