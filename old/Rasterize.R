setwd("C:/Users/bsmiley/My Documents/Sask_work/Sask/SKmask")
SKmask <- raster("SK_mask.dat")

setwd("C:/Users/bsmiley/My Documents/Sask_work/Canada_concept")
Sask_area <- readOGR(dsn = "layers", layer = "Sask_scape_reproj")

Sask_area_raster <- rasterize(Sask_area, SKmask, mask=TRUE)


#Check time it takes to run a code block
startTime <- Sys.time()
#put code here
Sask_area_raster <- rasterize(Sask_area, SKmask, mask=TRUE)
endTime <- Sys.time()
elapsedTime <- endTime - startTime