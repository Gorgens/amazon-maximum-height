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
library(randomForestExplainer)

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
#      file = '../amazon maximum height extras/randomForestIntactForestv24092020.Rdata')

rm(folds, group_fit_control)

# load('../amazon maximum height extras/randomForestIntactForest.Rdata')
load('../amazon maximum height extras/randomForestIntactForestv24092020.Rdata')

varImp(rf.intactForest)
importance(rf.intactForest$finalModel)

# Analisando a importância
importance_rfAll = measure_importance(rf.intactForest$finalModel)
png('../amazon maximum height/gcb review/SF Importance IntactForest.png', height = 7, 
    width = 15, units = 'cm', res = 300)
plot_multi_way_importance(importance_rfAll, 
                          x_measure = "mse_increase", 
                          y_measure = "node_purity_increase")
dev.off()

# plot_min_depth_distribution(rf.intactForest$finalModel)

heightIntactForestRaster = predict(layers2estimate, rf.intactForest)
heightIntactForestRaster = setMinMax(heightIntactForestRaster)
writeRaster(heightIntactForestRaster, 
            filename = '../amazon maximum height extras/rfHeightIntactForestRasterv24092020.tif')
