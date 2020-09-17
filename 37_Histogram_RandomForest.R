require(raster)

rf.height = raster('../amazon maximum height extras/rfHeightRasterCor80v14042020.tif')

jpeg(filename = './gcb review/histMaxHeightRF.jpg', width = 13, height = 10, units = 'cm', res = 300)
hist(rf.height, breaks = 20, xlab = "Maximum Height (m)", ylab = 'Frequency', main ='')
dev.off()
