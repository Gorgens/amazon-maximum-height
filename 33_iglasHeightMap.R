require(ggplot2)
require(raster)
require(tidyverse)
require(tmap)
require(gridExtra)
require(magrittr)
require(hexbin)


iglas = read.csv('data/iglasShtos.csv')
coordinates(iglas)=~x+y
crs(iglas) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

amaz2 = spTransform(amaz, crs(iglas))
iglas2 = iglas %>% crop(amaz2)                        						# pegar apenas pontos dentro dos limites do bioma
iglasEBA = raster::extract(heightRaster, iglas2)                        # extrai valor das variáveis ambientais para cada altura máxima mapeada
iglas2@data = cbind(iglas2@data, iglasEBA)                              # une arquivo clipado com shapefile original    
iglas2@data = na.omit(iglas2@data)                                      # remove observações com NA
#writeSpatialShape(iglas2, "../amazon maximum height extras/iglasXeba")  # salva shapefile com os atributos clipados dos rasters

myColor <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
myColor_scale_fill <- scale_fill_gradientn(colours = myColor)
iglasXEBA = ggplot(iglas2@data, aes(rh100,iglasEBA)) + stat_binhex(bins = 64) + geom_density2d(colour = "black") +
  xlab('IceSat height (m)') + xlim(0, 85) + ylab('Maximum height (m)') + ylim(0,85) + myColor_scale_fill +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))

ggsave(iglasXEBA, filename = './plot/iglasXEBA.png', width = 20, height = 20, units = 'cm', dpi = 300)

#tm_shape(amaz2) + tm_polygons() + 
#  tm_shape(iglas2) + tm_dots("rh100", size = 0.1, palette = "RdYlGn")

rm(iglas, amaz2, iglasEBA)