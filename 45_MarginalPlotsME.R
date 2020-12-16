require(ggplot2)
require(tidyverse)
require(gridExtra)
require(grid)
require(magrittr)
require(factoextra) 
require(dismo)
require(rJava)

load('../amazon maximum height extras/maxentHeight70Cor80v14042020.Rdata')

# source('./10_ImportingRasters.R')
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

Probfapar = predict(me.height70, faparPredict)

marginalFapar = ggplot() + 
  geom_line(aes(faparPredict$fapar, Probfapar), color='red') +
  xlab('FAPAR (%)') + ylab('Probability') + ylim(0, 1) +
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

Probelevation = predict(me.height70, elevationPredict)

marginalelevation = ggplot() + 
  geom_line(aes(elevationPredict$elevation, Probelevation), color='red') +
  xlab('Elevation (m)') + ylab('Probability') + ylim(0, 1) +
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

Probuspeed = predict(me.height70, uspeedPredict)

marginalUspeed = ggplot() + 
  geom_line(aes(uspeedPredict$uspeed, Probuspeed), color='red') +
  xlab('u-Speed (m/s)') + ylab('Probability') + ylim(0, 1) +
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

Probvspeed = predict(me.height70, vspeedPredict)

marginalVspeed = ggplot() + 
  geom_line(aes(vspeedPredict$vspeed, Probvspeed), color='red') +
  xlab('v-Speed (m/s)') + ylab('Probability') + ylim(0, 1) +
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

ProbclearDays = predict(me.height70, clearDaysPredict)

marginalClearDays = ggplot() + 
  geom_line(aes(clearDaysPredict$clearDays, ProbclearDays), color='red') +
  xlab('Clear days (days/yr)') + ylab('Probability') + ylim(0, 1) +
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

Probdays20 = predict(me.height70, days20Predict)

marginalDays20 = ggplot() + 
  geom_line(aes(days20Predict$days20, Probdays20), color='red') +
  xlab('Days >20 mm (days/yr)') + ylab('Probability') + ylim(0, 1) +
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

Problightning = predict(me.height70, lightningPredict)

marginalLightning = ggplot() + 
  geom_line(aes(lightningPredict$lightning, Problightning), color='red') +
  xlab('Rate of lightnings') + ylab('Probability') + ylim(0, 1) +
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
# Probmonth100 = predict(me.height70, month100Predict)
# 
# marginalMonth100 = ggplot() + 
#   geom_line(aes(month100Predict$month100, Probmonth100), color='red') +
#   xlab('Month with precipitation greater than 100 mm (months/yr)') + ylab('Probability') + ylim(0, 1) +
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

Probpannual = predict(me.height70, pannualPredict)

marginalPannual = ggplot() + 
  geom_line(aes(pannualPredict$pannual, Probpannual), color='red') +
  xlab('Annual precip. (mm)') + ylab('Probability') + ylim(0, 1) +
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
# Probpdriest = predict(me.height70, pdriestPredict)
# 
# marginalPdriest = ggplot() + 
#   geom_line(aes(pdriestPredict$pdriest, Probpdriest), color='red') +
#   xlab('Precipitation of the driest month (mm)') + ylab('Probability') + ylim(0, 1) +
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

Probpet = predict(me.height70, petPredict)

marginalPet = ggplot() + 
  geom_line(aes(petPredict$pet, Probpet), color='red') +
  xlab('Potential Evap. (mm)') + ylab('Probability') + ylim(0, 1) +
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

Probpseason = predict(me.height70, pseasonPredict)

marginalPseason = ggplot() + 
  geom_line(aes(pseasonPredict$pseason, Probpseason), color='red') +
  xlab('Precip. season. (mm)') + ylab('Probability') + ylim(0, 1) +
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

Probpwettest = predict(me.height70, pwettestPredict)

marginalPwettest = ggplot() + 
  geom_line(aes(pwettestPredict$pwettest, Probpwettest), color='red') +
  xlab('Precip. wettest (mm)') + ylab('Probability') + ylim(0, 1) +
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

Probtannual = predict(me.height70, tannualPredict)

marginalTannual = ggplot() + 
  geom_line(aes(tannualPredict$tannual, Probtannual), color='red') +
  xlab('Annual temp. (°C)') + ylab('Probability') + ylim(0, 1) +
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

Probtseason = predict(me.height70, tseasonPredict)

marginalTseason = ggplot() + 
  geom_line(aes(tseasonPredict$tseason, Probtseason), color='red') +
  xlab('Temp. season. (°C)') + ylab('Probability') + ylim(0, 1) +
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

Probtmax = predict(me.height70, tmaxPredict)

marginalTmax = ggplot() + 
  geom_line(aes(tmaxPredict$tmax, Probtmax), color='red') +
  xlab('Maximum temp. (°C)') + ylab('Probability') + ylim(0, 1) +
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

ProbclayContent = predict(me.height70, clayContentPredict)

marginalClayContent = ggplot() + 
  geom_line(aes(clayContentPredict$clayContent, ProbclayContent), color='red') +
  xlab('Clay content (%)') + ylab('Probability') + ylim(0, 1) +
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

ProbwaterContent = predict(me.height70, waterContentPredict)

marginalWaterContent = ggplot() + 
  geom_line(aes(waterContentPredict$waterContent, ProbwaterContent), color='red') +
  xlab('Water content (%)') + ylab('Probability') + ylim(0, 1) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

## Gráfico com paineis sem letras -------------------------

png('./plot/meMarginalPlotsCor80v11092020.png', units = 'cm', width = 20, height = 30, res = 300)
grid.arrange(marginalUspeed,
             marginalelevation,
             marginalPannual,
             marginalWaterContent,
             marginalPet,
             marginalLightning,
             marginalClayContent,
             marginalFapar, 
             marginalPseason, 
             marginalClearDays, 
             marginalPwettest,
             marginalDays20,
             marginalTannual,
             marginalVspeed, 
             marginalTseason,
             marginalTmax,
             ncol=4)
dev.off()

rm(clayContentPredict, clearDaysPredict, days20Predict, elevationPredict, faparPredict, 
   lightningPredict, marginalClayContent, marginalClearDays, marginalDays20, marginalelevation, 
   marginalFapar, marginalLightning, marginalPannual, marginalPet, marginalPseason, marginalPwettest,
   marginalTannual, marginalTmax, marginalTseason, marginalUspeed, marginalVspeed, marginalWaterContent,
   meanVariables, pannualPredict, petPredict, ProbclayContent, ProbclearDays, Probdays20, Probelevation,
   Probfapar, Problightning, Probpannual, Probpet, Probpseason, Probpwettest, Probtannual, Probtmax,
   Probtseason, Probuspeed, Probvspeed, ProbwaterContent, pseasonPredict, pwettestPredict, tannualPredict,
   tmaxPredict, tseasonPredict, uspeedPredict, vspeedPredict, waterContentPredict)
