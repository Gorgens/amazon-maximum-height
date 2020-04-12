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
# save(rf.heightAll, 
#     file = '../amazon maximum height extras/randomForestCor80v20042020.Rdata')
# load('../amazon maximum height extras/randomForestCor80v20042020.Rdata')
     
rm(folds, group_fit_control)                                            # limpa da memória parâmetros para Random Forest
varImp(rf.heightAll)                                                    # apresenta importância das variáveis com base nos modelos Random Forest

heightRaster = predict(layers2estimate, rf.heightAll)
heightRaster = setMinMax(heightRaster)
# writeRaster(heightRaster, filename = '../amazon maximum height extras/rfHeightRasterCor80v20042020.tif')

map = tm_shape(heightRaster) +
  tm_raster(breaks = c(0, 40, 50, 60, 70, 80, Inf),
            #n = 15,
            palette = "Greens",
            legend.hist = TRUE,
            title = "Maximum height (m)") +
  tm_shape(amaz) + tm_borders() +
  tm_legend(outside = TRUE, hist.width = 2) +
  tm_grid(lines = FALSE,
          labels.inside.frame = FALSE,
          projection = "+proj=longlat")

#print(map)
tmap_save(map, "./plot/rfHeightRasterCor80v20042020.png", width = 25, height = 15, units = "cm")

rm(map, drivers)
