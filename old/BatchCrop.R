## Creates a Raster of Sask study area at 250m resolution (Sask pixel=1) and renames to 'Reference' for
# BatchCrop run
startTime <- Sys.time()
setwd("C:/Users/bsmiley/My Documents/Sask_work/Canada_concept")
Sask_area <- readOGR(dsn = "layers", layer = "Sask_scape_reproj") # add SK vector Sask province
setwd("C:/Users/bsmiley/Documents/Sask_work/Canada_concept/layers")
template250 <- raster("DT1.tif")
Sask_area_reproj <- spTransform(Sask_area, crs(template250))
Sask250_temp <- rasterize(Sask_area_reproj, template250, field=1)
Sask250 <- crop(Sask250_temp, Sask_area_reproj, snap='out')
endTime <- Sys.time()
elapsedTime <- endTime - startTime
Reference <- Sask250
###########################################

#BatchCrop is a function that takes a list of rasters, crops them to a reference study area
# and reprojects them. Below the function is a set up script which adds the rasterizes a reference
#layer from a shp file of Sask and masks out non-forest areas. Also below is the BatchCrop execution
#script
###################################################################################################
BatchCrop<-function(Reference,OutName,OutPrj,OutRes){
  filenames <- list.files(pattern=".tif$", full.names=FALSE)   #Extract list of  file names from working directory
  library(raster) #Calls 'raster' library
  #Function 'f1' imports data listed in 'filenames' and assigns projection
  f1<-function(x,z) {
    y <- raster(x)
    projection(y) <- CRS(z)
    return(y)
  }
  import <- lapply(filenames,f1,projection(Reference))
  #Function multiply was used to crop rasters because interesct only crops to extent corners, multiply
  # crops to edge of study area using Reference (Sask250 - see below)
  multiply<-function(x,y) {
    x<- x * Reference
    return(x)
  }  
  cropped <- lapply(import,multiply,Reference)    #Crop imported layers to reference layer, argument 'x'
  #Function 'f2' changes projectection of cropped layers
  f2<-function(x,y) {
    x<-projectRaster(x, crs=OutPrj, res=OutRes)
    return(x)
  }
  output <- lapply(cropped,f2,OutPrj)
  #Use a 'for' loop to iterate writeRaster function for all cropped layers
  for(i in (1:max(length(filenames)))){
    writeRaster(output[[i]],paste(deparse(substitute(Sask)),filenames[i]), format='GTiff')
  }
}
###################################################################################################
## Prebatch crop parameter setup
beginCluster(30)
#starttime
startTime <- Sys.time()
##Rasterize
setwd("C:/Users/bsmiley/My Documents/Sask_work/Sask/SKmask")
    SKmask <- raster("SK_mask.dat") # add SK raster non-forest mask
setwd("C:/Users/bsmiley/My Documents/Sask_work/Canada_concept")
  Sask_area <- readOGR(dsn = "layers", layer = "Sask_scape_reproj") # add SK vector Sask province
    Reference <- rasterize(Sask_area, reso, mask=TRUE) ## make reference layer the rasterized 
    #Sask province with masked out non-forest  
  #set working directory for Batchcrop
  setwd("C:/Users/bsmiley/Documents/Sask_work/Canada_concept/layers")
startTime <- Sys.time() 
##Run BatchCrop
BatchCrop(Reference=Reference,OutName=Sask, OutPrj= "+proj=lcc +datum=NAD83 +units=m 
                    +lat_1=49 +lat_2=77 +lon_0=-95 +lat_0=49", OutRes=250)
#end time
endTime <- Sys.time()
elapsedTime <- endTime - startTime


# OTHER
SDT1.tif.tif <-raster("SDT1.tif.tif")
test <- lapply(filenames, loadFiles, projectRaster(SDT1.tif.tif, to = Sask_area_raster))

reso <- raster("SDT1.tif.tif")
startTime <- Sys.time()
SKmask250 <- aggregate(SKmask, fact=4, fun=mean)
#end time
endTime <- Sys.time()
elapsedTime <- endTime - startTime




setwd("C:/Users/bsmiley/My Documents/Sask_work/Canada_concept/layers")
DT1 <- raster("DT1.tif")


