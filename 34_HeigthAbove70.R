require(raster)

maxHeightMap = raster('../amazon maximum height extras/rfHeightRasterCor80v14042020.tif')
heigth70 = maxHeightMap > 70

sum(heigth70[heigth70 >= 1])

writeRaster(heigth70, filename = '../amazon maximum height extras/heigthAbove70.tif')


