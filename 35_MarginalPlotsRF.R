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
                          srtm = rep(meanVariables["srtm"], 500),
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

heightfapar = predict(rf.heightAll, faparPredict)

marginalFapar = ggplot() + 
  geom_line(aes(faparPredict$fapar, heightfapar), color='red') +
  xlab('FAPAR (%)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

srtmPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                         srtm = seq(min(na.omit(maximas@data$srtm)),
                                    max(na.omit(maximas@data$srtm)), 
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

heightsrtm = predict(rf.heightAll, srtmPredict)

marginalSrtm = ggplot() + 
  geom_line(aes(srtmPredict$srtm, heightsrtm), color='red') +
  xlab('SRTM (m)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

uspeedPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                           srtm = rep(meanVariables["srtm"], 500),
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

heightuspeed = predict(rf.heightAll, uspeedPredict)

marginalUspeed = ggplot() + 
  geom_line(aes(uspeedPredict$uspeed, heightuspeed), color='red') +
  xlab('u-Speed (m/s)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

vspeedPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                           srtm = rep(meanVariables["srtm"], 500),
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

heightvspeed = predict(rf.heightAll, vspeedPredict)

marginalVspeed = ggplot() + 
  geom_line(aes(vspeedPredict$vspeed, heightvspeed), color='red') +
  xlab('v-Speed (m/s)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

clearDaysPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                              srtm = rep(meanVariables["srtm"], 500),
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

heightclearDays = predict(rf.heightAll, clearDaysPredict)

marginalClearDays = ggplot() + 
  geom_line(aes(clearDaysPredict$clearDays, heightclearDays), color='red') +
  xlab('Number of clear days (days/yr)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

days20Predict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                           srtm = rep(meanVariables["srtm"], 500),
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

heightdays20 = predict(rf.heightAll, days20Predict)

marginalDays20 = ggplot() + 
  geom_line(aes(days20Predict$days20, heightdays20), color='red') +
  xlab('Days with more then 20 mm (days/yr)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

lightningPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                              srtm = rep(meanVariables["srtm"], 500),
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

heightlightning = predict(rf.heightAll, lightningPredict)

marginalLightning = ggplot() + 
  geom_line(aes(lightningPredict$lightning, heightlightning), color='red') +
  xlab('Lightning (rate of lights)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

# month100Predict = data.frame(fapar = rep(meanVariables["fapar"], 500),
#                              srtm = rep(meanVariables["srtm"], 500),
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
# heightmonth100 = predict(rf.heightAll, month100Predict)
# 
# marginalMonth100 = ggplot() + 
#   geom_line(aes(month100Predict$month100, heightmonth100), color='red') +
#   xlab('Month with precipitation greater than 100 mm (months/yr)') + ylab('Height (m)') + ylim(50, 70) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))


pannualPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                            srtm = rep(meanVariables["srtm"], 500),
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

heightpannual = predict(rf.heightAll, pannualPredict)

marginalPannual = ggplot() + 
  geom_line(aes(pannualPredict$pannual, heightpannual), color='red') +
  xlab('Annual precipitation (mm)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


# pdriestPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
#                             srtm = rep(meanVariables["srtm"], 500),
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
# heightpdriest = predict(rf.heightAll, pdriestPredict)
# 
# marginalPdriest = ggplot() + 
#   geom_line(aes(pdriestPredict$pdriest, heightpdriest), color='red') +
#   xlab('Precipitation of the driest month (mm)') + ylab('Height (m)') + ylim(50, 70) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))


petPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                        srtm = rep(meanVariables["srtm"], 500),
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

heightpet = predict(rf.heightAll, petPredict)

marginalPet = ggplot() + 
  geom_line(aes(petPredict$pet, heightpet), color='red') +
  xlab('Potential Evapotranspiration (mm)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


pseasonPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                            srtm = rep(meanVariables["srtm"], 500),
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

heightpseason = predict(rf.heightAll, pseasonPredict)

marginalPseason = ggplot() + 
  geom_line(aes(pseasonPredict$pseason, heightpseason), color='red') +
  xlab('Precipitation seasonality (mm)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


pwettestPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                             srtm = rep(meanVariables["srtm"], 500),
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

heightpwettest = predict(rf.heightAll, pwettestPredict)

marginalPwettest = ggplot() + 
  geom_line(aes(pwettestPredict$pwettest, heightpwettest), color='red') +
  xlab('Precipitation of the wettest month (mm)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


tannualPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                            srtm = rep(meanVariables["srtm"], 500),
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

heighttannual = predict(rf.heightAll, tannualPredict)

marginalTannual = ggplot() + 
  geom_line(aes(tannualPredict$tannual, heighttannual), color='red') +
  xlab('Annual temperature (°C)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


tseasonPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                            srtm = rep(meanVariables["srtm"], 500),
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

heighttseason = predict(rf.heightAll, tseasonPredict)

marginalTseason = ggplot() + 
  geom_line(aes(tseasonPredict$tseason, heighttseason), color='red') +
  xlab('Temperature seasonality (°C)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


tmaxPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                         srtm = rep(meanVariables["srtm"], 500),
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

heighttmax = predict(rf.heightAll, tmaxPredict)

marginalTmax = ggplot() + 
  geom_line(aes(tmaxPredict$tmax, heighttmax), color='red') +
  xlab('Maximum temperature (°C)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


clayContentPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                                srtm = rep(meanVariables["srtm"], 500),
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

heightclayContent = predict(rf.heightAll, clayContentPredict)

marginalClayContent = ggplot() + 
  geom_line(aes(clayContentPredict$clayContent, heightclayContent), color='red') +
  xlab('Clay content (%)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))


waterContentPredict = data.frame(fapar = rep(meanVariables["fapar"], 500),
                                 srtm = rep(meanVariables["srtm"], 500),
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

heightwaterContent = predict(rf.heightAll, waterContentPredict)

marginalWaterContent = ggplot() + 
  geom_line(aes(waterContentPredict$waterContent, heightwaterContent), color='red') +
  xlab('Water content (%)') + ylab('Height (m)') + ylim(50, 70) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

## Gráfico com paineis sem letras -------------------------

png('./plot/rfMarginalPlotsCor80.png', units = 'cm', width = 20, height = 30, res = 300)
grid.arrange(marginalFapar, 
             marginalSrtm, 
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

## Gráfico com paineis com letras -------------------------

# myplot1 <- arrangeGrob(marginalFapar, top = textGrob("A", x = unit(0, "npc")
#                                                      , y   = unit(1, "npc"), just=c("left","top"),
#                                                      gp=gpar(col="black", fontsize=18)))
# 
# myplot2 <- arrangeGrob(marginalSrtm, top = textGrob("B", x = unit(0, "npc")
#                                                     , y = unit(1, "npc"), just=c("left","top"),
#                                                     gp=gpar(col="black", fontsize=18)))
# 
# myplot3 <- arrangeGrob(marginalUspeed, top = textGrob("C", x = unit(0, "npc")
#                                                       , y  = unit(1, "npc"), just=c("left","top"),
#                                                       gp=gpar(col="black", fontsize=18)))
# 
# myplot4 <- arrangeGrob(marginalVspeed, top = textGrob("D", x = unit(0, "npc")
#                                                       , y = unit(1, "npc"), just=c("left","top"),
#                                                       gp=gpar(col="black",    fontsize=18)))
# 
# myplot5 <- arrangeGrob(marginalClearDays, top = textGrob("E", x = unit(0, "npc")
#                                                          , y   = unit(1, "npc"), just=c("left","top"),
#                                                          gp=gpar(col="black", fontsize=18)))
# 
# myplot6 <- arrangeGrob(marginalDays20, top = textGrob("F", x = unit(0, "npc")
#                                                       , y = unit(1, "npc"), just=c("left","top"),
#                                                       gp=gpar(col="black", fontsize=18)))
# 
# 
# myplot7 <- arrangeGrob(marginalLightning, top = textGrob("A", x = unit(0, "npc")
#                                                          , y   = unit(1, "npc"), just=c("left","top"),
#                                                          gp=gpar(col="black", fontsize=18)))
# 
# # myplot2 <- arrangeGrob(marginalMonth100, top = textGrob("B", x = unit(0, "npc")
# #                                                         , y = unit(1, "npc"), just=c("left","top"),
# #                                                         gp=gpar(col="black", fontsize=18)))
# 
# myplot8 <- arrangeGrob(marginalPannual, top = textGrob("C", x = unit(0, "npc")
#                                                        , y  = unit(1, "npc"), just=c("left","top"),
#                                                        gp=gpar(col="black", fontsize=18)))
# 
# # myplot4 <- arrangeGrob(marginalPdriest, top = textGrob("D", x = unit(0, "npc")
# #                                                        , y = unit(1, "npc"), just=c("left","top"),
# #                                                        gp=gpar(col="black",    fontsize=18)))
# 
# myplot9 <- arrangeGrob(marginalPet, top = textGrob("E", x = unit(0, "npc")
#                                                    , y   = unit(1, "npc"), just=c("left","top"),
#                                                    gp=gpar(col="black", fontsize=18)))
# 
# myplot10 <- arrangeGrob(marginalPseason, top = textGrob("F", x = unit(0, "npc")
#                                                        , y = unit(1, "npc"), just=c("left","top"),
#                                                        gp=gpar(col="black", fontsize=18)))
# 
# myplot11 <- arrangeGrob(marginalPwettest, top = textGrob("A", x = unit(0, "npc")
#                                                         , y   = unit(1, "npc"), just=c("left","top"),
#                                                         gp=gpar(col="black", fontsize=18)))
# 
# myplot12 <- arrangeGrob(marginalTannual, top = textGrob("B", x = unit(0, "npc")
#                                                        , y = unit(1, "npc"), just=c("left","top"),
#                                                        gp=gpar(col="black", fontsize=18)))
# 
# myplot13 <- arrangeGrob(marginalTseason, top = textGrob("C", x = unit(0, "npc")
#                                                        , y  = unit(1, "npc"), just=c("left","top"),
#                                                        gp=gpar(col="black", fontsize=18)))
# 
# myplot14 <- arrangeGrob(marginalTmax, top = textGrob("D", x = unit(0, "npc")
#                                                     , y = unit(1, "npc"), just=c("left","top"),
#                                                     gp=gpar(col="black",    fontsize=18)))
# 
# myplot15 <- arrangeGrob(marginalClayContent, top = textGrob("E", x = unit(0, "npc")
#                                                            , y   = unit(1, "npc"), just=c("left","top"),
#                                                            gp=gpar(col="black", fontsize=18)))
# 
# myplot16 <- arrangeGrob(marginalWaterContent, top = textGrob("F", x = unit(0, "npc")
#                                                             , y = unit(1, "npc"), just=c("left","top"),
#                                                             gp=gpar(col="black", fontsize=18)))
# 
# png('./plot/rfMarginalPlotsCor80.png', units = 'cm', width = 20, height = 30, res = 300)
# grid.arrange(myplot1, 
#              myplot2, 
#              myplot3, 
#              myplot4, 
#              myplot5, 
#              myplot6,
#              myplot7,
#              myplot8,
#              myplot9,
#              myplot10,
#              myplot11,
#              myplot12,
#              myplot13,
#              myplot14,
#              myplot15,
#              myplot16,
#              ncol=4)
# dev.off()

