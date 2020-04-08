require(ggplot2)
require(raster)
require(tidyverse)
require(tmap)
require(gridExtra)
require(magrittr)
require(factoextra) 
require(randomForest)
require(caret)

# source('10_ImportingRasters.R')
  
## Random Forest -----------------------

input = maximas@data %>% na.omit() %>% select(-altura_cop)              # separa as variáveis explicativas
output = maximas@data %>% na.omit() %>% select(altura_cop)              # separa a variável altura maxima
names(output) = c("height")                                             # renomeia a variável


drivers = cbind(output, input)
rm(output, input)                                                       # limpa variáveis que não serão mais utilizadas
folds = drivers %>% row.names() %>% groupKFold(k = 15)                  # divide os trasectos em k-folds

group_fit_control = trainControl(index = folds, method = "cv")          # parametriza os parametros de controle da função train do pacote caret
rf.heightAll = train(height ~ .,                                        # define variáveis dependentes e independentes
                     data = drivers,
                     method='rf',                                       # define modelo a ser treinado
                     importance=T,                                      # análise de importancia para as variaveis
                     trControl = group_fit_control)                     # parâmetros para validação cruzada
print(rf.heightAll)
save(rf.heightAll, 
    file = 'C:/Users/gorge/Documents/GIS DataBase/amazon maximum height extras/objects/randomForestCor80.Rdata')

rm(folds, group_fit_control)                                            # limpa da memória parâmetros para Random Forest
varImp(rf.heightAll)                                     # apresenta importância das variáveis com base nos modelos Random Forest

heightRaster = predict(layers2estimate, rf.heightAll)
heightRaster = setMinMax(heightRaster)
writeRaster(heightRaster, filename = 'C:/Users/gorge/Documents/GIS DataBase/amazon maximum height extras/rfHeightRasterCor80.tif')

map = tm_shape(heightRaster) +
 tm_raster(n = 15,
           palette = "Greens",
           legend.hist = TRUE,
           title = "Maximum height (m)") +
 tm_shape(amaz) + tm_borders() +
 tm_legend(outside = TRUE, hist.width = 2)

print(map)
tmap_save(map, "./plot/rfHeightRasterCor80.png", width = 25, height = 20, units = "cm")

rm(map, drivers)
