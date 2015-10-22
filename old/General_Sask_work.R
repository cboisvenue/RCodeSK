##set working directory for SKmask file and import SK_mask
setwd("C:/Users/bsmiley/Documents/Sask_work/Sask/SKmask")
SKmask <- raster("SK_mask.dat")

## Set working directed on remote workstation to testing area
setwd("C:/Users/bsmiley/Documents/Sask_work/Canada_concept/layers/ReProject/Test")

library(snow)
library(rgeos)
library(maptools)
library(dplyr)
library(rgdal)
library(sp)
library(raster)
library(gdata)
library(SpaDES)
library(grid)
library(parallel)
install.packages("snow")

SDT1 <- raster("SDT1.tif.tif")
test <- projectRaster(SDT1, to = SKmask)


fileList <-
  data.frame(files =
               dir(file.path(
                 "C:/Users/bsmiley/Documents/Sask_work/Canada_concept/layers/ReProject"
                 ,"Test"),
                 full.names=FALSE, pattern= "tif$"),
             functions="raster",
             packages="raster",
             stringsAsFactors=FALSE)
loadFiles(fileList=fileList)

newfiles <-lapply(fileList,writeRaster(fileList, filename="new", format='GTiff'))


filenames <- list.files(pattern=".tif$", full.names=FALSE)   

for(i in (1:max(length(loadFiles)))){
  writeRaster(loadFiles[[i]],paste(deparse(substitute(Sask)),loadFiles[i]), format='GTiff')
}


#Plot all maps on a new window
if(is.null(dev.list())) {
  dev(2)
} else {
  if(any(names(dev.list())=="RStudioGD")) {
    dev(which(names(dev.list())=="RStudioGD")+3)
  } else {
    dev(max(dev.list()))
  }
}