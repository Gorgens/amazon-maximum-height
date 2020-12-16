options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
require(raster)
require(dismo)
require(rJava)  
require(jsonlite)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_241')

# source('10_ImportingRasters.R')
load('../amazon maximum height extras/maxentHeight70Cor80v14042020.Rdata')

SplitRas(raster=layers2estimate,ppside=3,save=TRUE,plot=TRUE)
list2 <- list()
for(i in 1:9){ # change this 9 depending on your number of pieces
  rx <- raster(paste("SplitRas",i,".tif",sep=""))
  rx_processed = predict(me.height70, rx)
  writeRaster(rx_processed,filename=paste("SplitRas",i,sep=""),
              format="GTiff",datatype="FLT4S",overwrite=TRUE)
}

# read each piece back in R
list2 <- list()
for(i in 1:9){ # change this 9 depending on your number of pieces
  rx <- raster(paste("SplitRas",i,".tif",sep=""))
  list2[[i]] <- rx
}
# mosaic them, plot mosaic & save output
list2$fun   <- max
rast.mosaic <- do.call(mosaic,list2)
plot(rast.mosaic,axes=F,legend=F,bty="n",box=FALSE)
writeRaster(rast.mosaic,filename=paste("Mosaicked_ras",sep=""),
            format="GTiff",datatype="FLT4S",overwrite=TRUE)

## Criar mapa final -----------------
amaz = shapefile('./data/amazbioma.shp')
amaz = spTransform(amaz, CRS("+proj=longlat +datum=WGS84"))

amazRegions = shapefile('./data/amazRegions.shp',  encoding="UTF-8")
crs(amazRegions) = crs(amaz)
			
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

tmap_save(map, "./plot/meHeightRasterCor80v20042020.png", width = 25, height = 18, units = 'cm')

rm(dadosTeste, dadosTreino, fold, ocorrenciaHeight70, maximas_70)