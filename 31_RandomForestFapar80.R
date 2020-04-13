require(ggplot2)
require(raster)
require(tidyverse)
require(tmap)
require(sf)
require(leaflet)
require(sp)
require(gridExtra)
require(magrittr)
require(factoextra) 
require(randomForest)
require(caret)


source('10_ImportingRasters.R')

maximas80fapar = subset(maximas, fapar > 80)
input = maximas80fapar@data %>% na.omit() %>% select(-altura_cop)
output = maximas80fapar@data %>% na.omit() %>% select(altura_cop)
names(output) = c("height")

drivers = cbind(output, input)
rm(output, input)
folds = drivers %>% row.names() %>% groupKFold(k = 15)           

group_fit_control = trainControl(index = folds, method = "cv")     
rf.intactForest = train(height ~ .,                                         
                        data = drivers,
                        method='rf',                   # define modelo a ser treinado
                        importance=T,                  # análise de importancia para as variaveis
                        trControl = group_fit_control) # parâmetros para validação cruzada

print(rf.intactForest)
# save(rf.intactForest, 
#      file = 'C:/Users/gorge/Documents/GIS DataBase/amazon maximum height extras/objects/randomForestIntactForest.Rdata')

rm(folds, group_fit_control)

varImp(rf.intactForest)


# #heightRaster = predict(scaledLayers, rf.intactForest)
# heightIntactForestRaster = predict(layers2estimate, rf.intactForest)
# heightIntactForestRaster = setMinMax(heightIntactForestRaster)
# #writeRaster(heightIntactForestRaster, filename = 'C:/Users/gorge/Documents/GIS DataBase/amazon maximum height extras/rfHeightIntactForestRaster.tif')
# 
# 
# map = tm_shape(heightIntactForestRaster) + 
#   tm_raster(n = 15,
#             palette = "Greens",
#             legend.hist = TRUE,
#             title = "Maximum height (m)") +
#   tm_shape(amaz) + tm_borders() +
#   tm_legend(outside = TRUE, hist.width = 2)
# #print(map)
# tmap_save(map, "./plot/heightRaster_RandomForest_InstactForest.png", width = 1920, height = 1080)
