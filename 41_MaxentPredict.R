print('Initializing...')
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
require(raster)
require(dismo)
require(rJava)  
require(jsonlite)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_241')

source('10_ImportingRasters.R')
print('1 of 4')

load('../amazon maximum height extras/maxentHeight70Cor80v14042020.Rdata')
probHeightMap70m = predict(me.height70, layers2estimate)   
print('2 of 4')

# writeRaster(probHeightMap70m, 
			# filename = '../amazon maximum height extras/meprobHeightMap70mCor80v15042020.tif')

## Criar mapa final -----------------
amazRegions = shapefile('./data/amazRegions.shp',  encoding="UTF-8")
crs(amazRegions) = crs(amaz)

maximas_70 = maximas[maximas$altura_cop>70,]
print('3 of 4')

		
map = tm_shape(probHeightMap70m) + tm_raster(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
            #n = 15,
            palette = "Greens",
            legend.hist = FALSE,
            title = "Prob. height > 70 m") +
  tm_shape(maximas_70) + tm_dots("black", size = 0.07) +
  tm_shape(amazRegions) + tm_borders(col='black') + tm_text('label', size = 1) +
  #tm_shape(amaz) + tm_borders() +
  tm_legend(outside = TRUE) +
  tm_grid(lines = FALSE,
          labels.inside.frame = FALSE,
          projection = "+proj=longlat")

tmap_save(map, "./plot/meHeightRasterCor80v15042020.png", width = 25, height = 18, units = 'cm')

rm(dadosTeste, dadosTreino, fold, ocorrenciaHeight70, maximas_70)
print('4 of 4')
