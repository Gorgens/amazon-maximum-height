require(ggplot2)
require(raster)
require(tidyverse)
require(tmap)
require(gridExtra)
require(magrittr)
require(factoextra) 
require(randomForest)
require(caret)

source('10_ImportingRasters.R')
  
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

# save(rf.heightAll, 
#     file = '../amazon maximum height extras/randomForestCor80v14042020.Rdata')
     
rm(folds, group_fit_control, drivers)                                            # limpa da memória parâmetros para Random Forest
print(rf.heightAll)
varImp(rf.heightAll)
importance(rf.heightAll) # apresenta importância das variáveis com base nos modelos Random Forest


# Gráfico obs vs estimado Random Forest

load('../amazon maximum height extras/randomForestCor80v14042020.Rdata')
predictions = predict(rf.heightAll, input)

png('./gcb review/SF3 ObservedPredicted_v2.png', 
    width = 10, height = 10, units = 'cm', res = 300)
ggplot() + geom_point(aes(x = output$height, y = predictions)) + 
  xlab('Observed height (m)') + ylab('Predicted height (m)') + 
  xlim(25, 90) + ylim(25, 90) + theme_bw() + 
  geom_abline(intercept = 0, slope = 1) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()
