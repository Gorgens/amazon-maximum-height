require(ggplot2)
require(raster)
require(tidyverse)
require(tmap)
require(sf)
require(sp)
require(gridExtra)
require(grid)
require(factoextra) 
require(minpack.lm)
require(corrgram)


source('10_ImportingRasters.R')

## Principal components analysis --------------------
# # separa as variáveis explicativas
# input = maximas@data %>% na.omit() %>% select(-altura_cop)
# # separa a variável altura maxima
# output = maximas@data %>% na.omit() %>% select(altura_cop)
# # renomeia a variável
# names(output) = c("height")
# # normaliza as variáveis explicativas
# inputScaled = input %>% preProcess(method = c("center", "scale")) %>% predict(input)
# # salvar parâmetros da normalização
# normParam = preProcess(input, method = c("center", "scale"))
# 
# # ajusta os componentes principais
# res.pca = prcomp(inputScaled)                                           
# #res.pca = prcomp(input)                                           
# # calcula a correlação da variável dependente e componentes
# target.coord = cor(output, res.pca$x) 
# # cria biplot
# p = fviz_pca_var(res.pca, col.var = "contrib", # Color by contributions to the PC
#                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#                  repel = TRUE)     # Avoid text overlapping)
# # adiciona variável dependente no biplot
# png("./plot/pca.png", width = 960, height = 720)
# fviz_add(p, target.coord, color ="black", geom="arrow")
# dev.off()
# 
# rm(p, target.coord)

## Covariance and Correlation --------------------
# covMatrix = maximas@data %>% na.omit() %>% cov()
# write.csv(covMatrix, file = './tables/covariance.csv')
# corMatrix = maximas@data %>% na.omit() %>% cor()
# write.csv(corMatrix, file = './tables/correlation.csv')

png("./plot/correlogramLowerCor80.png", width = 25, height = 20, units = 'cm',res = 300)
corrgram(na.omit(maximas@data), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.ellipse, text.panel=panel.txt,
         diag.panel=panel.minmax)
dev.off()

## Efficience frontier by variable  --------------------

# ### u-Speed
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$uspeed)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$uspeed > bins[b] & temp$uspeed <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, uspeed)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, uspeed)
#   
# }
# 
# # chute inicial dos parâmetros modelo linear
# # res2 = lm(altura_cop ~ uspeed, data=efficientBorderFiltered)
# # uspeedLm = function(a = coef(res2)[1], b = coef(res2)[2], x){
# #   return(a + b*x)
# # }
# # 
# # 
# # estHeight = data.frame(uspeed = seq(min(temp$uspeed), max(temp$uspeed), length.out = 100), altura_cop = uspeedLm(x = seq(min(temp$uspeed), max(temp$uspeed), length.out = 100)))
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=1, b=mean(efficientBorderFiltered$uspeed), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((uspeed/b)^(a-1)) * exp(-(uspeed/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# uspeedWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(uspeed = seq(min(temp$uspeed), max(temp$uspeed), length.out = 100), altura_cop = uspeedWeibull(x = seq(min(temp$uspeed), max(temp$uspeed), length.out = 100)))
# 
# 
# uspeedPlot = ggplot(temp, aes(uspeed, altura_cop)) + 
#   geom_point(colour = 'darkgray') +
#   geom_point(data = efficientBorderFiltered, colour = "black") +
#   geom_line(data = estHeight, aes(uspeed, altura_cop)) +
#   xlab('u-Speed (m/s)') + ylab('Height (m)') + ylim(c(0, 100)) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/uspeed_plot.png")
# 
# 
# ### v-Speed
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$vspeed)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$vspeed > bins[b] & temp$vspeed <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, vspeed)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, vspeed)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=1, b=mean(efficientBorderFiltered$vspeed), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((vspeed/b)^(a-1)) * exp(-(vspeed/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# vspeedWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(vspeed = seq(min(temp$vspeed), max(temp$vspeed), length.out = 100), altura_cop = vspeedWeibull(x = seq(min(temp$vspeed), max(temp$vspeed), length.out = 100)))
# 
# # ggplot(temp, aes(vspeed, altura_cop)) + 
# #   geom_point(colour = 'darkgray') +
# #   geom_point(data = efficientBorderFiltered, colour = "black") +
# #   geom_line(data = estHeight, aes(vspeed, altura_cop)) +
# #   xlab('Speed (m/s)') + ylab('Height (m)') + ylim(c(0, 100)) +
# #   theme_bw() + theme(panel.grid.major = element_blank(), 
# #                      panel.grid.minor = element_blank(),
# #                      panel.background = element_blank(), 
# #                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/vspeed_plot.png")
# 
# 
# ### Days with 20 mm or more
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$days20)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$days20 > bins[b] & temp$days20 <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, days20)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, days20)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=1, b=mean(efficientBorderFiltered$days20), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((days20/b)^(a-1)) * exp(-(days20/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# days20Weibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(days20 = seq(min(temp$days20), max(temp$days20), length.out = 100), altura_cop = days20Weibull(x = seq(min(temp$days20), max(temp$days20), length.out = 100)))
# 
# # ggplot(temp, aes(days20, altura_cop)) + 
# #   geom_point(colour = 'darkgray') +
# #   geom_point(data = efficientBorderFiltered, colour = "black") +
# #   geom_line(data = estHeight, aes(days20, altura_cop)) +
# #   xlab('Number of days > 20 mm (days)') + ylab('Height (m)') + ylim(c(0, 100)) +
# #   theme_bw() + theme(panel.grid.major = element_blank(), 
# #                      panel.grid.minor = element_blank(),
# #                      panel.background = element_blank(), 
# #                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/days20_plot.png")
# 
# 
# ### Lightning
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$lightning)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$lightning > bins[b] & temp$lightning <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, lightning)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, lightning)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=1, b=mean(efficientBorderFiltered$lightning), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((lightning/b)^(a-1)) * exp(-(lightning/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# lightningWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(lightning = seq(min(temp$lightning), max(temp$lightning), length.out = 100), altura_cop = lightningWeibull(x = seq(min(temp$lightning), max(temp$lightning), length.out = 100)))
# 
# lightPlot = ggplot(temp, aes(lightning, altura_cop)) + 
#   geom_point(colour = 'darkgray') +
#   geom_point(data = efficientBorderFiltered, colour = "black") +
#   geom_line(data = estHeight, aes(lightning, altura_cop)) +
#   xlab('Lightning rate (flashes rate)') + ylab('Height (m)') + ylim(c(0, 100)) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/lightning_plot.png")
# 
# 
# ### SRTM
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$srtm)
# nbins = 20
# bins = seq(rangeT[1], rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$srtm > bins[b] & temp$srtm <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, srtm)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, srtm)
#   
# }
# 
# # chute inicial dos parâmetros
# pars <- c(a=10, b=180, d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((srtm/b)^(a-1)) * exp(-(srtm/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# srtmWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(srtm = seq(min(temp$srtm), max(temp$srtm), length.out = 100), altura_cop = srtmWeibull(x = seq(min(temp$srtm), max(temp$srtm), length.out = 100)))
# 
# srtmPlot = ggplot(temp, aes(srtm, altura_cop)) + 
#   geom_point(colour = 'darkgray') +
#   geom_point(data = efficientBorderFiltered, colour = "black") +
#   geom_line(data = estHeight, aes(srtm, altura_cop)) +
#   xlab('Elevation (m)') + ylab('Height (m)') + ylim(c(0, 100)) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/srtm_plot.png")
# 
# 
# ### Potential Evapotranspiration
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$pet)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$pet > bins[b] & temp$pet <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, pet)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, pet)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=1, b=mean(efficientBorderFiltered$pet), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((pet/b)^(a-1)) * exp(-(pet/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# petWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(pet = seq(min(temp$pet), max(temp$pet), length.out = 100), altura_cop = petWeibull(x = seq(min(temp$pet), max(temp$pet), length.out = 100)))
# 
# # ggplot(temp, aes(pet, altura_cop)) + 
# #   geom_point(colour = 'darkgray') +
# #   geom_point(data = efficientBorderFiltered, colour = "black") +
# #   geom_line(data = estHeight, aes(pet, altura_cop)) +
# #   xlab('PET (mm)') + ylab('Height (m)') + ylim(c(0, 100)) +
# #   theme_bw() + theme(panel.grid.major = element_blank(), 
# #                      panel.grid.minor = element_blank(),
# #                      panel.background = element_blank(), 
# #                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/pet_plot.png")
# 
# 
# ### FAPAR
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$fapar)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$fapar > bins[b] & temp$fapar <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, fapar)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, fapar)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=1, b=mean(efficientBorderFiltered$fapar), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((fapar/b)^(a-1)) * exp(-(fapar/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# faparWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(fapar = seq(min(temp$fapar), max(temp$fapar), length.out = 100), altura_cop = faparWeibull(x = seq(min(temp$fapar), max(temp$fapar), length.out = 100)))
# 
# faparPlot = ggplot(temp, aes(fapar, altura_cop)) + 
#   geom_point(colour = 'darkgray') +
#   geom_point(data = efficientBorderFiltered, colour = "black") +
#   geom_line(data = estHeight, aes(fapar, altura_cop)) +
#   xlab('FAPAR (%)') + ylab('Height (m)') + ylim(c(0, 100)) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/fapar_plot.png")
# 
# 
# ### Clear days
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$clearDays)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$clearDays > bins[b] & temp$clearDays <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, clearDays)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, clearDays)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=1, b=mean(efficientBorderFiltered$clearDays), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((clearDays/b)^(a-1)) * exp(-(clearDays/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# clearDaysWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(clearDays = seq(min(temp$clearDays), max(temp$clearDays), length.out = 100), altura_cop = clearDaysWeibull(x = seq(min(temp$clearDays), max(temp$clearDays), length.out = 100)))
# 
# clearDaysPLot = ggplot(temp, aes(clearDays, altura_cop)) + 
#   geom_point(colour = 'darkgray') +
#   geom_point(data = efficientBorderFiltered, colour = "black") +
#   geom_line(data = estHeight, aes(clearDays, altura_cop)) +
#   xlab('Number of clear days (days)') + ylab('Height (m)') + ylim(c(0, 100)) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/clearDays_plot.png")
# 
# 
# ### Number of months lower than 100 mm
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$month100)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$month100 > bins[b] & temp$month100 <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, month100)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, month100)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=5, b=mean(efficientBorderFiltered$month100), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((month100/b)^(a-1)) * exp(-(month100/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# month100Weibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(month100 = seq(min(temp$month100), max(temp$month100), length.out = 100), altura_cop = month100Weibull(x = seq(min(temp$month100), max(temp$month100), length.out = 100)))
# 
# # ggplot(temp, aes(month100, altura_cop)) + 
# #   geom_point(colour = 'darkgray') +
# #   geom_point(data = efficientBorderFiltered, colour = "black") +
# #   geom_line(data = estHeight, aes(month100, altura_cop)) +
# #   xlab('Number of months < 100 mm (months)') + ylab('Height (m)') + ylim(c(0, 100)) +
# #   theme_bw() + theme(panel.grid.major = element_blank(), 
# #                      panel.grid.minor = element_blank(),
# #                      panel.background = element_blank(), 
# #                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/month100_plot.png")
# 
# 
# ### Precipitation in the driest month
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$pdriest)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$pdriest > bins[b] & temp$pdriest <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, pdriest)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, pdriest)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=5, b=mean(efficientBorderFiltered$pdriest), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((pdriest/b)^(a-1)) * exp(-(pdriest/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# pdriestWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(pdriest = seq(min(temp$pdriest), max(temp$pdriest), length.out = 100), altura_cop = pdriestWeibull(x = seq(min(temp$pdriest), max(temp$pdriest), length.out = 100)))
# 
# # ggplot(temp, aes(pdriest, altura_cop)) + 
# #   geom_point(colour = 'darkgray') +
# #   geom_point(data = efficientBorderFiltered, colour = "black") +
# #   geom_line(data = estHeight, aes(pdriest, altura_cop)) +
# #   xlab('Precipitation in the driest month (mm)') + ylab('Height (m)') + ylim(c(0, 100)) +
# #   theme_bw() + theme(panel.grid.major = element_blank(), 
# #                      panel.grid.minor = element_blank(),
# #                      panel.background = element_blank(), 
# #                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/pdriest_plot.png")
# 
# 
# ### Precipitation in the wettest month
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$pwettest)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$pwettest > bins[b] & temp$pwettest <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, pwettest)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, pwettest)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=5, b=mean(efficientBorderFiltered$pwettest), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((pwettest/b)^(a-1)) * exp(-(pwettest/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# pwettestWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(pwettest = seq(min(temp$pwettest), max(temp$pwettest), length.out = 100), altura_cop = pwettestWeibull(x = seq(min(temp$pwettest), max(temp$pwettest), length.out = 100)))
# 
# # ggplot(temp, aes(pwettest, altura_cop)) + 
# #   geom_point(colour = 'darkgray') +
# #   geom_point(data = efficientBorderFiltered, colour = "black") +
# #   geom_line(data = estHeight, aes(pwettest, altura_cop)) +
# #   xlab('Precipitation in the wettest month (mm)') + ylab('Height (m)') + ylim(c(0, 100)) +
# #   theme_bw() + theme(panel.grid.major = element_blank(), 
# #                      panel.grid.minor = element_blank(),
# #                      panel.background = element_blank(), 
# #                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/pwettest_plot.png")
# 
# 
# 
# ### Average annual precipitation
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$pannual)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$pannual > bins[b] & temp$pannual <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, pannual)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, pannual)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=5, b=mean(efficientBorderFiltered$pannual), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((pannual/b)^(a-1)) * exp(-(pannual/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# pannualWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(pannual = seq(min(temp$pannual), max(temp$pannual), length.out = 100), altura_cop = pannualWeibull(x = seq(min(temp$pannual), max(temp$pannual), length.out = 100)))
# 
# pannualPlot = ggplot(temp, aes(pannual, altura_cop)) + 
#   geom_point(colour = 'darkgray') +
#   geom_point(data = efficientBorderFiltered, colour = "black") +
#   geom_line(data = estHeight, aes(pannual, altura_cop)) +
#   xlab('Average Annual Precipitation (mm)') + ylab('Height (m)') + ylim(c(0, 100)) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/pannual_plot.png")
# 
# 
# 
# ### Precipitation seasonality
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$pseason)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$pseason > bins[b] & temp$pseason <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, pseason)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, pseason)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=5, b=mean(efficientBorderFiltered$pseason), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((pseason/b)^(a-1)) * exp(-(pseason/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# pseasonWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(pseason = seq(min(temp$pseason), max(temp$pseason), length.out = 100), altura_cop = pseasonWeibull(x = seq(min(temp$pseason), max(temp$pseason), length.out = 100)))
# 
# pseasonPlot = ggplot(temp, aes(pseason, altura_cop)) + 
#   geom_point(colour = 'darkgray') +
#   geom_point(data = efficientBorderFiltered, colour = "black") +
#   geom_line(data = estHeight, aes(pseason, altura_cop)) +
#   xlab('Precipitation seasonality (mm)') + ylab('Height (m)') + ylim(c(0, 100)) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/pseason_plot.png")
# 
# 
# 
# ### Annual average temperature
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$tannual)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$tannual > bins[b] & temp$tannual <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, tannual)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, tannual)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=5, b=mean(efficientBorderFiltered$tannual), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((tannual/b)^(a-1)) * exp(-(tannual/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# tannualWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(tannual = seq(min(temp$tannual), max(temp$tannual), length.out = 100), altura_cop = tannualWeibull(x = seq(min(temp$tannual), max(temp$tannual), length.out = 100)))
# 
# # ggplot(temp, aes(tannual, altura_cop)) + 
# #   geom_point(colour = 'darkgray') +
# #   geom_point(data = efficientBorderFiltered, colour = "black") +
# #   geom_line(data = estHeight, aes(tannual, altura_cop)) +
# #   xlab('Annual temperature (°C)') + ylab('Height (m)') + ylim(c(0, 100)) +
# #   theme_bw() + theme(panel.grid.major = element_blank(), 
# #                      panel.grid.minor = element_blank(),
# #                      panel.background = element_blank(), 
# #                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/tannual_plot.png")
# 
# 
# 
# ### Temperature seasonality
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$tseason)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$tseason > bins[b] & temp$tseason <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, tseason)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, tseason)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=5, b=mean(efficientBorderFiltered$tseason), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((tseason/b)^(a-1)) * exp(-(tseason/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# tseasonWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(tseason = seq(min(temp$tseason), max(temp$tseason), length.out = 100), altura_cop = tseasonWeibull(x = seq(min(temp$tseason), max(temp$tseason), length.out = 100)))
# 
# # ggplot(temp, aes(tseason, altura_cop)) + 
# #   geom_point(colour = 'darkgray') +
# #   geom_point(data = efficientBorderFiltered, colour = "black") +
# #   geom_line(data = estHeight, aes(tseason, altura_cop)) +
# #   xlab('Temperature seasonality (°C)') + ylab('Height (m)') + ylim(c(0, 100)) +
# #   theme_bw() + theme(panel.grid.major = element_blank(), 
# #                      panel.grid.minor = element_blank(),
# #                      panel.background = element_blank(), 
# #                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/tseason_plot.png")
# 
# 
# 
# ### Maximum temperature
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$tmax)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$tmax > bins[b] & temp$tmax <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, tmax)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, tmax)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=5, b=mean(efficientBorderFiltered$tmax), d = 1000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((tmax/b)^(a-1)) * exp(-(tmax/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# tmaxWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(tmax = seq(min(temp$tmax), max(temp$tmax), length.out = 100), altura_cop = tmaxWeibull(x = seq(min(temp$tmax), max(temp$tmax), length.out = 100)))
# 
# # ggplot(temp, aes(tmax, altura_cop)) + 
# #   geom_point(colour = 'darkgray') +
# #   geom_point(data = efficientBorderFiltered, colour = "black") +
# #   geom_line(data = estHeight, aes(tmax, altura_cop)) +
# #   xlab('Maximum temperature (°C)') + ylab('Height (m)') + ylim(c(0, 100)) +
# #   theme_bw() + theme(panel.grid.major = element_blank(), 
# #                      panel.grid.minor = element_blank(),
# #                      panel.background = element_blank(), 
# #                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/tmax_plot.png")
# 
# 
# ### Clay content at 30 cm
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$clayContent)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$clayContent > bins[b] & temp$clayContent <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, clayContent)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, clayContent)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=5, b=mean(efficientBorderFiltered$clayContent), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((clayContent/b)^(a-1)) * exp(-(clayContent/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# clayContentWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(clayContent = seq(min(temp$clayContent), max(temp$clayContent), length.out = 100), altura_cop = clayContentWeibull(x = seq(min(temp$clayContent), max(temp$clayContent), length.out = 100)))
# 
# clayPlot = ggplot(temp, aes(clayContent, altura_cop)) + 
#   geom_point(colour = 'darkgray') +
#   geom_point(data = efficientBorderFiltered, colour = "black") +
#   geom_line(data = estHeight, aes(clayContent, altura_cop)) +
#   xlab('Clay Content (%)') + ylab('Height (m)') + ylim(c(0, 100)) +
#   theme_bw() + theme(panel.grid.major = element_blank(), 
#                      panel.grid.minor = element_blank(),
#                      panel.background = element_blank(), 
#                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/clayContent_plot.png")
# 
# 
# ### Water content at 30 cm
# temp = maximas@data
# temp = na.omit(temp)
# 
# rangeT = range(temp$waterContent)
# nbins = 20
# bins = seq(rangeT[1]-0.1, rangeT[2], length.out = nbins)
# 
# efficientBorder = data.frame()
# for(b in seq(1, nbins-1, 1)){
#   tempB = temp[temp$waterContent > bins[b] & temp$waterContent <= bins[b+1],]
#   tempB = arrange(tempB, altura_cop)
#   efficientBorder = rbind(efficientBorder, tail(tempB, 1))
# }
# 
# efficientBorderFiltered = data.frame()
# l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]
# while(l != 0){
#   
#   efficientBorderFiltered = data.frame()
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[1, ])
#   efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[ dim(efficientBorder)[1], ])
#   
#   for (i in seq(2, dim(efficientBorder)[1]-1, 1)){
#     if(efficientBorder[i, "altura_cop"] <= efficientBorder[i+1, "altura_cop"] & efficientBorder[i, "altura_cop"] <= efficientBorder[i-1, "altura_cop"]) {
#       
#     } else {
#       efficientBorderFiltered = rbind(efficientBorderFiltered, efficientBorder[i, ])
#       
#     }
#   }
#   l = dim(efficientBorder)[1] - dim(efficientBorderFiltered)[1]  
#   efficientBorder = arrange(efficientBorderFiltered, waterContent)
#   efficientBorderFiltered = arrange(efficientBorderFiltered, waterContent)
#   
# }
# 
# # ajuste dos parâmetros weibull
# pars <- c(a=5, b=mean(efficientBorderFiltered$waterContent), d = 10000)
# res2 = nlsLM(altura_cop ~ d * ((a/b) * ((waterContent/b)^(a-1)) * exp(-(waterContent/b)^a)),
#              data=efficientBorderFiltered,
#              start=pars)
# 
# waterContentWeibull = function(a = coef(res2)[1], b = coef(res2)[2], d = coef(res2)[3], x){
#   return(d*((a/b) * ((x/b)^(a-1)) * exp(-(x/b)^a)))
# }
# 
# estHeight = data.frame(waterContent = seq(min(temp$waterContent), max(temp$waterContent), length.out = 100), altura_cop = waterContentWeibull(x = seq(min(temp$waterContent), max(temp$waterContent), length.out = 100)))
# 
# # ggplot(temp, aes(waterContent, altura_cop)) + 
# #   geom_point(colour = 'darkgray') +
# #   geom_point(data = efficientBorderFiltered, colour = "black") +
# #   geom_line(data = estHeight, aes(waterContent, altura_cop)) +
# #   xlab('Water Content (%)') + ylab('Height (m)') + ylim(c(0, 100)) +
# #   theme_bw() + theme(panel.grid.major = element_blank(), 
# #                      panel.grid.minor = element_blank(),
# #                      panel.background = element_blank(), 
# #                      axis.line = element_line(colour = "black"))
# 
# #ggsave("./plot/waterContent_plot.png")
# 
# 
# ### Gráfico com paineis
# myplot1 <- arrangeGrob(clearDaysPLot, top = textGrob("A", x = unit(0, "npc"), 
#                                                      y   = unit(1, "npc"), just=c("left","top"),
#                                                      gp=gpar(col="black", fontsize=18, fontfamily="Times Roman")))
# 
# myplot2 <- arrangeGrob(clayPlot, top = textGrob("B", x = unit(0, "npc"), 
#                                                 y = unit(1, "npc"), just=c("left","top"),
#                                                 gp=gpar(col="black", fontsize=18, fontfamily="Times Roman")))
# 
# myplot3 <- arrangeGrob(pannualPlot, top = textGrob("C", x = unit(0, "npc"), 
#                                                    y  = unit(1, "npc"), just=c("left","top"),
#                                                    gp=gpar(col="black", fontsize=18, fontfamily="Times Roman")))
# 
# myplot4 <- arrangeGrob(faparPlot, top = textGrob("D", x = unit(0, "npc"), 
#                                                  y = unit(1, "npc"), just=c("left","top"),
#                                                  gp=gpar(col="black",    fontsize=18, fontfamily="Times Roman")))
# 
# myplot5 <- arrangeGrob(uspeedPlot, top = textGrob("E", x = unit(0, "npc"), 
#                                                   y   = unit(1, "npc"), just=c("left","top"),
#                                                   gp=gpar(col="black", fontsize=18, fontfamily="Times Roman")))
# 
# myplot6 <- arrangeGrob(lightPlot, top = textGrob("F", x = unit(0, "npc"), 
#                                                  y = unit(1, "npc"), just=c("left","top"),
#                                                  gp=gpar(col="black", fontsize=18, fontfamily="Times Roman")))
# 
# myplot7 <- arrangeGrob(pseasonPlot, top = textGrob("G", x = unit(0, "npc"), 
#                                                    y  = unit(1, "npc"), just=c("left","top"),
#                                                    gp=gpar(col="black", fontsize=18, fontfamily="Times Roman")))
# 
# myplot8 <- arrangeGrob(srtmPlot, top = textGrob("H", x = unit(0, "npc"), 
#                                                 y = unit(1, "npc"), just=c("left","top"),
#                                                 gp=gpar(col="black",    fontsize=18, fontfamily="Times Roman")))
# 
# png('./plot/efficientFrontiersFeatures.png', units = 'cm', width = 20, height = 30, res = 300)
# grid.arrange(myplot1, myplot2, myplot3, myplot4, myplot5, myplot6, myplot7, myplot8, ncol=2)
# dev.off()