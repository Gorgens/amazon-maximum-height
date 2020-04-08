require(ggplot2)
require(raster)
require(tidyverse)
require(tmap)
require(gridExtra)
require(magrittr)


iglas = read.csv('data/iglasShtos.csv')
coordinates(iglas)=~x+y

# map = tm_shape(amaz) + tm_polygons() + 
#   tm_shape(iglas) + tm_dots("rh100", size = 0.1, palette = "RdYlGn")
# print(map)


