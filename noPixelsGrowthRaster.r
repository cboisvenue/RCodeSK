# SK growth raster
# need to know number of pixels

skDir <- "F:\\saskatchewan"

library(raster)
library(sp)
library(rgdal)
library(maptools)
library(SpaDES)

# I have shape files, that I need to clip and rasterise to match these rasters-------------
bio_1984 <- raster::raster(paste0(skDir,"\\bio_1984.tif"))

