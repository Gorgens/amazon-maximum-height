require(raster)
require(tidyverse)
require(tmap)
require(factoextra) 
require(dismo)
require(rJava)  
require(jsonlite)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_241')

source('10_ImportingRasters.R')

## Envelop by MaxEnt ------------------------
maximas_70<-maximas[maximas$altura_cop>70,]

model.extent = extent(min(maximas_70$coords.x1)-10,                      # Coordenadas para a extensão de corte do mapa
                      max(maximas_70$coords.x1)+10,
                      min(maximas_70$coords.x2)-10,
                      max(maximas_70$coords.x2)+10)


rattlerocc = data.frame(x1=maximas_70$coords.x1,                         # data frame com latitude e longitude
                        x2=maximas_70$coords.x2)


fold = kfold(rattlerocc, k=5)                                            # add an index that makes five random groups of observations
oco_teste = rattlerocc[fold == 1, ]                                      # hold out one fifth as test data
oco_treino = rattlerocc[fold != 1, ]                                     # the other four fifths are training data

nicheHeight70 = maxent(modelEnv, oco_treino)                             #note we just using the training data
save(nicheHeight70, 
     file = 'C:/Users/gorge/Documents/GIS DataBase/amazon maximum height extras/objects/maxentHeight70Cor80.Rdata')

plot(nicheHeight70)

png('./plot/meMarginalPlotsCor80.png', units = 'cm', width = 20, height = 30, res = 300)
response(nicheHeight70)
dev.off()

modelo_70m = predict(nicheHeight70, layers2estimate)

map = tm_shape(modelo_70m) +
  tm_raster(n = 15,
            palette = "Greens",
            legend.hist = FALSE,
            title = "Prob. height > 70 m") +
  tm_shape(maximas_70) +
  tm_dots("black", size = 0.07) +
  tm_shape(amaz) + tm_borders() +
  tm_legend(outside = TRUE) +
  tm_grid(lines = FALSE,
          labels.inside.frame = FALSE,
          projection = "+proj=longlat")

tmap_save(map, "./plot/maxentHeight70.png", width = 25, height = 18, units = 'cm')
