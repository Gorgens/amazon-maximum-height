require(ggplot2)
require(tidyverse)
require(gridExtra)
require(grid)
require(magrittr)
require(factoextra) 
require(randomForest)

## Random Forest
#load("C:\\Users\\gorge\\Documents\\GIS DataBase\\amazon maximum height extras\\objects\\randomForestCor80.Rdata")
meanVariables = maximas@data %>% na.omit() %>% sapply(mean)

faparPredict = data.frame(fapar = seq(min(na.omit(maximas@data$fapar)),
                                      max(na.omit(maximas@data$fapar)), 
                                      length.out = 500),
                          elevation = rep(meanVariables["elevation"], 500),
                          uspeed = rep(meanVariables["uspeed"], 500),
                          vspeed = rep(meanVariables["vspeed"], 500),
                          clearDays = rep(meanVariables["clearDays"], 500),
                          days20 = rep(meanVariables["days20"], 500),
                          lightning = rep(meanVariables["lightning"], 500),
                          #month100 = rep(meanVariables["month100"], 500),
                          pannual = rep(meanVariables["pannual"], 500),
                          #pdriest = rep(meanVariables["pdriest"], 500),
                          pet = rep(meanVariables["pet"], 500),
                          pseason = rep(meanVariables["pseason"], 500),
                          pwettest = rep(meanVariables["pwettest"], 500),
                          tannual = rep(meanVariables["tannual"], 500),
                          tseason = rep(meanVariables["tseason"], 500),
                          tmax = rep(meanVariables["tmax"], 500),
                          clayContent = rep(meanVariables["clayContent"], 500),
                          waterContent = rep(meanVariables["waterContent"], 500))

heightfapar = predict(rf.intactForest, faparPredict)

marginalFapar = ggplot() + 
  geom_line(aes(faparPredict$fapar, heightfapar), color='red') +
  xlab('FAPAR (%)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

elevationPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                              elevation = seq(min(na.omit(maximas@data$elevation)),
                                              max(na.omit(maximas@data$elevation)), 
                                              length.out = 500),
                              uspeed = rep(meanVariables["uspeed"], 500),
                              vspeed = rep(meanVariables["vspeed"], 500),
                              clearDays = rep(meanVariables["clearDays"], 500),
                              days20 = rep(meanVariables["days20"], 500),
                              lightning = rep(meanVariables["lightning"], 500),
                              #month100 = rep(meanVariables["month100"], 500),
                              pannual = rep(meanVariables["pannual"], 500),
                              #pdriest = rep(meanVariables["pdriest"], 500),
                              pet = rep(meanVariables["pet"], 500),
                              pseason = rep(meanVariables["pseason"], 500),
                              pwettest = rep(meanVariables["pwettest"], 500),
                              tannual = rep(meanVariables["tannual"], 500),
                              tseason = rep(meanVariables["tseason"], 500),
                              tmax = rep(meanVariables["tmax"], 500),
                              clayContent = rep(meanVariables["clayContent"], 500),
                              waterContent = rep(meanVariables["waterContent"], 500))

heightelevation = predict(rf.intactForest, elevationPredict)

marginalelevation = ggplot() + 
  geom_line(aes(elevationPredict$elevation, heightelevation), color='red') +
  xlab('Elevation (m)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

uspeedPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                           elevation = rep(meanVariables["elevation"], 500),
                           uspeed = seq(min(na.omit(maximas@data$uspeed)),
                                        max(na.omit(maximas@data$uspeed)), 
                                        length.out = 500),
                           vspeed = rep(meanVariables["vspeed"], 500),
                           clearDays = rep(meanVariables["clearDays"], 500),
                           days20 = rep(meanVariables["days20"], 500),
                           lightning = rep(meanVariables["lightning"], 500),
                           #month100 = rep(meanVariables["month100"], 500),
                           pannual = rep(meanVariables["pannual"], 500),
                           #pdriest = rep(meanVariables["pdriest"], 500),
                           pet = rep(meanVariables["pet"], 500),
                           pseason = rep(meanVariables["pseason"], 500),
                           pwettest = rep(meanVariables["pwettest"], 500),
                           tannual = rep(meanVariables["tannual"], 500),
                           tseason = rep(meanVariables["tseason"], 500),
                           tmax = rep(meanVariables["tmax"], 500),
                           clayContent = rep(meanVariables["clayContent"], 500),
                           waterContent = rep(meanVariables["waterContent"], 500))

heightuspeed = predict(rf.intactForest, uspeedPredict)

marginalUspeed = ggplot() + 
  geom_line(aes(uspeedPredict$uspeed, heightuspeed), color='red') +
  xlab('u-Speed (m/s)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

vspeedPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                           elevation = rep(meanVariables["elevation"], 500),
                           uspeed = rep(meanVariables["uspeed"], 500),
                           vspeed = seq(min(na.omit(maximas@data$vspeed)),
                                        max(na.omit(maximas@data$vspeed)), 
                                        length.out = 500),
                           clearDays = rep(meanVariables["clearDays"], 500),
                           days20 = rep(meanVariables["days20"], 500),
                           lightning = rep(meanVariables["lightning"], 500),
                           #month100 = rep(meanVariables["month100"], 500),
                           pannual = rep(meanVariables["pannual"], 500),
                           #pdriest = rep(meanVariables["pdriest"], 500),
                           pet = rep(meanVariables["pet"], 500),
                           pseason = rep(meanVariables["pseason"], 500),
                           pwettest = rep(meanVariables["pwettest"], 500),
                           tannual = rep(meanVariables["tannual"], 500),
                           tseason = rep(meanVariables["tseason"], 500),
                           tmax = rep(meanVariables["tmax"], 500),
                           clayContent = rep(meanVariables["clayContent"], 500),
                           waterContent = rep(meanVariables["waterContent"], 500))

heightvspeed = predict(rf.intactForest, vspeedPredict)

marginalVspeed = ggplot() + 
  geom_line(aes(vspeedPredict$vspeed, heightvspeed), color='red') +
  xlab('v-Speed (m/s)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

clearDaysPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                              elevation = rep(meanVariables["elevation"], 500),
                              uspeed = rep(meanVariables["uspeed"], 500),
                              vspeed = rep(meanVariables["vspeed"], 500),
                              clearDays = seq(min(na.omit(maximas@data$clearDays)),
                                              max(na.omit(maximas@data$clearDays)), 
                                              length.out = 500),
                              days20 = rep(meanVariables["days20"], 500),
                              lightning = rep(meanVariables["lightning"], 500),
                              #month100 = rep(meanVariables["month100"], 500),
                              pannual = rep(meanVariables["pannual"], 500),
                              #pdriest = rep(meanVariables["pdriest"], 500),
                              pet = rep(meanVariables["pet"], 500),
                              pseason = rep(meanVariables["pseason"], 500),
                              pwettest = rep(meanVariables["pwettest"], 500),
                              tannual = rep(meanVariables["tannual"], 500),
                              tseason = rep(meanVariables["tseason"], 500),
                              tmax = rep(meanVariables["tmax"], 500),
                              clayContent = rep(meanVariables["clayContent"], 500),
                              waterContent = rep(meanVariables["waterContent"], 500))

heightclearDays = predict(rf.intactForest, clearDaysPredict)

marginalClearDays = ggplot() + 
  geom_line(aes(clearDaysPredict$clearDays, heightclearDays), color='red') +
  xlab('Clear days (days/yr)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

days20Predict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                           elevation = rep(meanVariables["elevation"], 500),
                           uspeed = rep(meanVariables["uspeed"], 500),
                           vspeed = rep(meanVariables["vspeed"], 500),
                           clearDays = rep(meanVariables["clearDays"], 500),
                           days20 = seq(min(na.omit(maximas@data$days20)),
                                        max(na.omit(maximas@data$days20)), 
                                        length.out = 500),
                           lightning = rep(meanVariables["lightning"], 500),
                           #month100 = rep(meanVariables["month100"], 500),
                           pannual = rep(meanVariables["pannual"], 500),
                           #pdriest = rep(meanVariables["pdriest"], 500),
                           pet = rep(meanVariables["pet"], 500),
                           pseason = rep(meanVariables["pseason"], 500),
                           pwettest = rep(meanVariables["pwettest"], 500),
                           tannual = rep(meanVariables["tannual"], 500),
                           tseason = rep(meanVariables["tseason"], 500),
                           tmax = rep(meanVariables["tmax"], 500),
                           clayContent = rep(meanVariables["clayContent"], 500),
                           waterContent = rep(meanVariables["waterContent"], 500))

heightdays20 = predict(rf.intactForest, days20Predict)

marginalDays20 = ggplot() + 
  geom_line(aes(days20Predict$days20, heightdays20), color='red') +
  xlab('Days > 20 mm (days/yr)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

lightningPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                              elevation = rep(meanVariables["elevation"], 500),
                              uspeed = rep(meanVariables["uspeed"], 500),
                              vspeed = rep(meanVariables["vspeed"], 500),
                              clearDays = rep(meanVariables["clearDays"], 500),
                              days20 = rep(meanVariables["days20"], 500),
                              lightning = seq(min(na.omit(maximas@data$lightning)),
                                              max(na.omit(maximas@data$lightning)), 
                                              length.out = 500),
                              #month100 = rep(meanVariables["month100"], 500),
                              pannual = rep(meanVariables["pannual"], 500),
                              #pdriest = rep(meanVariables["pdriest"], 500),
                              pet = rep(meanVariables["pet"], 500),
                              pseason = rep(meanVariables["pseason"], 500),
                              pwettest = rep(meanVariables["pwettest"], 500),
                              tannual = rep(meanVariables["tannual"], 500),
                              tseason = rep(meanVariables["tseason"], 500),
                              tmax = rep(meanVariables["tmax"], 500),
                              clayContent = rep(meanVariables["clayContent"], 500),
                              waterContent = rep(meanVariables["waterContent"], 500))

heightlightning = predict(rf.intactForest, lightningPredict)

marginalLightning = ggplot() + 
  geom_line(aes(lightningPredict$lightning, heightlightning), color='red') +
  xlab('Rate of lightnings') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

# month100Predict = data.frame(fapar = rep(meanVariables["fapar"], 500),
#                              elevation = rep(meanVariables["elevation"], 500),
#                              uspeed = rep(meanVariables["uspeed"], 500),
#                              vspeed = rep(meanVariables["vspeed"], 500),
#                              clearDays = rep(meanVariables["clearDays"], 500),
#                              days20 = rep(meanVariables["days20"], 500),
#                              lightning = rep(meanVariables["lightning"], 500),
#                              month100 = seq(min(na.omit(maximas@data$month100)),
#                                             max(na.omit(maximas@data$month100)), 
#                                             length.out = 500),
#                              pannual = rep(meanVariables["pannual"], 500),
#                              pdriest = rep(meanVariables["pdriest"], 500),
#                              pet = rep(meanVariables["pet"], 500),
#                              pseason = rep(meanVariables["pseason"], 500),
#                              pwettest = rep(meanVariables["pwettest"], 500),
#                              tannual = rep(meanVariables["tannual"], 500),
#                              tseason = rep(meanVariables["tseason"], 500),
#                              tmax = rep(meanVariables["tmax"], 500),
#                              clayContent = rep(meanVariables["clayContent"], 500),
#                              waterContent = rep(meanVariables["waterContent"], 500))
# 
# heightmonth100 = predict(rf.intactForest, month100Predict)
# 
# marginalMonth100 = ggplot() + 
#   geom_line(aes(month100Predict$month100, heightmonth100), color='red') +
#   xlab('Month with precipitation greater than 100 mm (months/yr)') + ylab('Height (m)') + ylim(50, 65) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))


pannualPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                            elevation = rep(meanVariables["elevation"], 500),
                            uspeed = rep(meanVariables["uspeed"], 500),
                            vspeed = rep(meanVariables["vspeed"], 500),
                            clearDays = rep(meanVariables["clearDays"], 500),
                            days20 = rep(meanVariables["days20"], 500),
                            lightning = rep(meanVariables["lightning"], 500),
                            #month100 = rep(meanVariables["month100"], 500),
                            pannual = seq(min(na.omit(maximas@data$pannual)),
                                          max(na.omit(maximas@data$pannual)), 
                                          length.out = 500),
                            #pdriest = rep(meanVariables["pdriest"], 500),
                            pet = rep(meanVariables["pet"], 500),
                            pseason = rep(meanVariables["pseason"], 500),
                            pwettest = rep(meanVariables["pwettest"], 500),
                            tannual = rep(meanVariables["tannual"], 500),
                            tseason = rep(meanVariables["tseason"], 500),
                            tmax = rep(meanVariables["tmax"], 500),
                            clayContent = rep(meanVariables["clayContent"], 500),
                            waterContent = rep(meanVariables["waterContent"], 500))

heightpannual = predict(rf.intactForest, pannualPredict)

marginalPannual = ggplot() + 
  geom_line(aes(pannualPredict$pannual, heightpannual), color='red') +
  xlab('Annual precip. (mm)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


# pdriestPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
#                             elevation = rep(meanVariables["elevation"], 500),
#                             uspeed = rep(meanVariables["uspeed"], 500),
#                             vspeed = rep(meanVariables["vspeed"], 500),
#                             clearDays = rep(meanVariables["clearDays"], 500),
#                             days20 = rep(meanVariables["days20"], 500),
#                             lightning = rep(meanVariables["lightning"], 500),
#                             month100 = rep(meanVariables["month100"], 500),
#                             pannual = rep(meanVariables["pannual"], 500),
#                             pdriest = seq(min(na.omit(maximas@data$pdriest)),
#                                           max(na.omit(maximas@data$pdriest)), 
#                                           length.out = 500),
#                             pet = rep(meanVariables["pet"], 500),
#                             pseason = rep(meanVariables["pseason"], 500),
#                             pwettest = rep(meanVariables["pwettest"], 500),
#                             tannual = rep(meanVariables["tannual"], 500),
#                             tseason = rep(meanVariables["tseason"], 500),
#                             tmax = rep(meanVariables["tmax"], 500),
#                             clayContent = rep(meanVariables["clayContent"], 500),
#                             waterContent = rep(meanVariables["waterContent"], 500))
# 
# heightpdriest = predict(rf.intactForest, pdriestPredict)
# 
# marginalPdriest = ggplot() + 
#   geom_line(aes(pdriestPredict$pdriest, heightpdriest), color='red') +
#   xlab('Precipitation of the driest month (mm)') + ylab('Height (m)') + ylim(50, 65) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))


petPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                        elevation = rep(meanVariables["elevation"], 500),
                        uspeed = rep(meanVariables["uspeed"], 500),
                        vspeed = rep(meanVariables["vspeed"], 500),
                        clearDays = rep(meanVariables["clearDays"], 500),
                        days20 = rep(meanVariables["days20"], 500),
                        lightning = rep(meanVariables["lightning"], 500),
                        #month100 = rep(meanVariables["month100"], 500),
                        pannual = rep(meanVariables["pannual"], 500),
                        #pdriest = rep(meanVariables["pdriest"], 500),
                        pet = seq(min(na.omit(maximas@data$pet)),
                                  max(na.omit(maximas@data$pet)), 
                                  length.out = 500),
                        pseason = rep(meanVariables["pseason"], 500),
                        pwettest = rep(meanVariables["pwettest"], 500),
                        tannual = rep(meanVariables["tannual"], 500),
                        tseason = rep(meanVariables["tseason"], 500),
                        tmax = rep(meanVariables["tmax"], 500),
                        clayContent = rep(meanVariables["clayContent"], 500),
                        waterContent = rep(meanVariables["waterContent"], 500))

heightpet = predict(rf.intactForest, petPredict)

marginalPet = ggplot() + 
  geom_line(aes(petPredict$pet, heightpet), color='red') +
  xlab('Potential Evap. (mm)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


pseasonPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                            elevation = rep(meanVariables["elevation"], 500),
                            uspeed = rep(meanVariables["uspeed"], 500),
                            vspeed = rep(meanVariables["vspeed"], 500),
                            clearDays = rep(meanVariables["clearDays"], 500),
                            days20 = rep(meanVariables["days20"], 500),
                            lightning = rep(meanVariables["lightning"], 500),
                            #month100 = rep(meanVariables["month100"], 500),
                            pannual = rep(meanVariables["pannual"], 500),
                            #pdriest = rep(meanVariables["pdriest"], 500),
                            pet = rep(meanVariables["pet"], 500),
                            pseason = seq(min(na.omit(maximas@data$pseason)),
                                          max(na.omit(maximas@data$pseason)), 
                                          length.out = 500),
                            pwettest = rep(meanVariables["pwettest"], 500),
                            tannual = rep(meanVariables["tannual"], 500),
                            tseason = rep(meanVariables["tseason"], 500),
                            tmax = rep(meanVariables["tmax"], 500),
                            clayContent = rep(meanVariables["clayContent"], 500),
                            waterContent = rep(meanVariables["waterContent"], 500))

heightpseason = predict(rf.intactForest, pseasonPredict)

marginalPseason = ggplot() + 
  geom_line(aes(pseasonPredict$pseason, heightpseason), color='red') +
  xlab('Precip. seasonality (mm)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


pwettestPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                             elevation = rep(meanVariables["elevation"], 500),
                             uspeed = rep(meanVariables["uspeed"], 500),
                             vspeed = rep(meanVariables["vspeed"], 500),
                             clearDays = rep(meanVariables["clearDays"], 500),
                             days20 = rep(meanVariables["days20"], 500),
                             lightning = rep(meanVariables["lightning"], 500),
                             #month100 = rep(meanVariables["month100"], 500),
                             pannual = rep(meanVariables["pannual"], 500),
                             #pdriest = rep(meanVariables["pdriest"], 500),
                             pet = rep(meanVariables["pet"], 500),
                             pseason = rep(meanVariables["pseason"], 500),
                             pwettest = seq(min(na.omit(maximas@data$pwettest)),
                                            max(na.omit(maximas@data$pwettest)), 
                                            length.out = 500),
                             tannual = rep(meanVariables["tannual"], 500),
                             tseason = rep(meanVariables["tseason"], 500),
                             tmax = rep(meanVariables["tmax"], 500),
                             clayContent = rep(meanVariables["clayContent"], 500),
                             waterContent = rep(meanVariables["waterContent"], 500))

heightpwettest = predict(rf.intactForest, pwettestPredict)

marginalPwettest = ggplot() + 
  geom_line(aes(pwettestPredict$pwettest, heightpwettest), color='red') +
  xlab('Precip. wettest month (mm)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


tannualPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                            elevation = rep(meanVariables["elevation"], 500),
                            uspeed = rep(meanVariables["uspeed"], 500),
                            vspeed = rep(meanVariables["vspeed"], 500),
                            clearDays = rep(meanVariables["clearDays"], 500),
                            days20 = rep(meanVariables["days20"], 500),
                            lightning = rep(meanVariables["lightning"], 500),
                            #month100 = rep(meanVariables["month100"], 500),
                            pannual = rep(meanVariables["pannual"], 500),
                            #pdriest = rep(meanVariables["pdriest"], 500),
                            pet = rep(meanVariables["pet"], 500),
                            pseason = rep(meanVariables["pseason"], 500),
                            pwettest = rep(meanVariables["pwettest"], 500),
                            tannual = seq(min(na.omit(maximas@data$tannual)),
                                          max(na.omit(maximas@data$tannual)), 
                                          length.out = 500),
                            tseason = rep(meanVariables["tseason"], 500),
                            tmax = rep(meanVariables["tmax"], 500),
                            clayContent = rep(meanVariables["clayContent"], 500),
                            waterContent = rep(meanVariables["waterContent"], 500))

heighttannual = predict(rf.intactForest, tannualPredict)

marginalTannual = ggplot() + 
  geom_line(aes(tannualPredict$tannual, heighttannual), color='red') +
  xlab('Annual temp. (°C)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


tseasonPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                            elevation = rep(meanVariables["elevation"], 500),
                            uspeed = rep(meanVariables["uspeed"], 500),
                            vspeed = rep(meanVariables["vspeed"], 500),
                            clearDays = rep(meanVariables["clearDays"], 500),
                            days20 = rep(meanVariables["days20"], 500),
                            lightning = rep(meanVariables["lightning"], 500),
                            #month100 = rep(meanVariables["month100"], 500),
                            pannual = rep(meanVariables["pannual"], 500),
                            #pdriest = rep(meanVariables["pdriest"], 500),
                            pet = rep(meanVariables["pet"], 500),
                            pseason = rep(meanVariables["pseason"], 500),
                            pwettest = rep(meanVariables["pwettest"], 500),
                            tannual = rep(meanVariables["tannual"], 500),
                            tseason = seq(min(na.omit(maximas@data$tseason)),
                                          max(na.omit(maximas@data$tseason)), 
                                          length.out = 500),
                            tmax = rep(meanVariables["tmax"], 500),
                            clayContent = rep(meanVariables["clayContent"], 500),
                            waterContent = rep(meanVariables["waterContent"], 500))

heighttseason = predict(rf.intactForest, tseasonPredict)

marginalTseason = ggplot() + 
  geom_line(aes(tseasonPredict$tseason, heighttseason), color='red') +
  xlab('Temp. seasonality (°C)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


tmaxPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                         elevation = rep(meanVariables["elevation"], 500),
                         uspeed = rep(meanVariables["uspeed"], 500),
                         vspeed = rep(meanVariables["vspeed"], 500),
                         clearDays = rep(meanVariables["clearDays"], 500),
                         days20 = rep(meanVariables["days20"], 500),
                         lightning = rep(meanVariables["lightning"], 500),
                         #month100 = rep(meanVariables["month100"], 500),
                         pannual = rep(meanVariables["pannual"], 500),
                         #pdriest = rep(meanVariables["pdriest"], 500),
                         pet = rep(meanVariables["pet"], 500),
                         pseason = rep(meanVariables["pseason"], 500),
                         pwettest = rep(meanVariables["pwettest"], 500),
                         tannual = rep(meanVariables["tannual"], 500),
                         tseason = rep(meanVariables["tseason"], 500),
                         tmax = seq(min(na.omit(maximas@data$tmax)),
                                    max(na.omit(maximas@data$tmax)), 
                                    length.out = 500),
                         clayContent = rep(meanVariables["clayContent"], 500),
                         waterContent = rep(meanVariables["waterContent"], 500))

heighttmax = predict(rf.intactForest, tmaxPredict)

marginalTmax = ggplot() + 
  geom_line(aes(tmaxPredict$tmax, heighttmax), color='red') +
  xlab('Maximum temp. (°C)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


clayContentPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                                elevation = rep(meanVariables["elevation"], 500),
                                uspeed = rep(meanVariables["uspeed"], 500),
                                vspeed = rep(meanVariables["vspeed"], 500),
                                clearDays = rep(meanVariables["clearDays"], 500),
                                days20 = rep(meanVariables["days20"], 500),
                                lightning = rep(meanVariables["lightning"], 500),
                                #month100 = rep(meanVariables["month100"], 500),
                                pannual = rep(meanVariables["pannual"], 500),
                                #pdriest = rep(meanVariables["pdriest"], 500),
                                pet = rep(meanVariables["pet"], 500),
                                pseason = rep(meanVariables["pseason"], 500),
                                pwettest = rep(meanVariables["pwettest"], 500),
                                tannual = rep(meanVariables["tannual"], 500),
                                tseason = rep(meanVariables["tseason"], 500),
                                tmax = rep(meanVariables["tmax"], 500),
                                clayContent = seq(min(na.omit(maximas@data$clayContent)),
                                                  max(na.omit(maximas@data$clayContent)), 
                                                  length.out = 500),
                                waterContent = rep(meanVariables["waterContent"], 500))

heightclayContent = predict(rf.intactForest, clayContentPredict)

marginalClayContent = ggplot() + 
  geom_line(aes(clayContentPredict$clayContent, heightclayContent), color='red') +
  xlab('Clay content (%)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


waterContentPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                                 elevation = rep(meanVariables["elevation"], 500),
                                 uspeed = rep(meanVariables["uspeed"], 500),
                                 vspeed = rep(meanVariables["vspeed"], 500),
                                 clearDays = rep(meanVariables["clearDays"], 500),
                                 days20 = rep(meanVariables["days20"], 500),
                                 lightning = rep(meanVariables["lightning"], 500),
                                 #month100 = rep(meanVariables["month100"], 500),
                                 pannual = rep(meanVariables["pannual"], 500),
                                 #pdriest = rep(meanVariables["pdriest"], 500),
                                 pet = rep(meanVariables["pet"], 500),
                                 pseason = rep(meanVariables["pseason"], 500),
                                 pwettest = rep(meanVariables["pwettest"], 500),
                                 tannual = rep(meanVariables["tannual"], 500),
                                 tseason = rep(meanVariables["tseason"], 500),
                                 tmax = rep(meanVariables["tmax"], 500),
                                 clayContent = rep(meanVariables["clayContent"], 500),
                                 waterContent = seq(min(na.omit(maximas@data$waterContent)),
                                                    max(na.omit(maximas@data$waterContent)), 
                                                    length.out = 500))

heightwaterContent = predict(rf.intactForest, waterContentPredict)

marginalWaterContent = ggplot() + 
  geom_line(aes(waterContentPredict$waterContent, heightwaterContent), color='red') +
  xlab('Water content (%)') + ylab('Height (m)') + ylim(50, 65) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

## Gráfico com paineis sem letras -------------------------

png('./plot/rfMarginalPlotsIntactv29042020.png', units = 'cm', width = 20, height = 30, res = 300)
grid.arrange(marginalFapar, 
             marginalelevation, 
             marginalUspeed, 
             marginalVspeed, 
             marginalClearDays, 
             marginalDays20,
             marginalLightning,
             marginalPannual,
             marginalPet,
             marginalPseason,
             marginalPwettest,
             marginalTannual,
             marginalTseason,
             marginalTmax,
             marginalClayContent,
             marginalWaterContent,
             ncol=4)
dev.off()