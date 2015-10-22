##################################################################################################
## Creates a Raster of Sask study area at 250m resolution (Sask pixel=1) and renames to 'Reference' for
# BatchCrop run
##################################################################################################
##Generate 250m reference layer (done)
startTime <- Sys.time()
setwd("C:/Users/bsmiley/My Documents/Sask_work/Canada_concept")
Sask_area <- readOGR(dsn = "layers", layer = "Sask_scape_reproj") # add SK vector Sask province
setwd("C:/Users/bsmiley/Documents/Sask_work/Canada_concept/layers")
template250 <- raster("DT1.tif")
Sask_area_reproj <- spTransform(Sask_area, crs(template250)) # reproject Sask_area to Recliner inputs
Sask250_temp <- rasterize(Sask_area_reproj, template250, field=1) # rasterize Saskarea using 250m template
Sask250 <- crop(Sask250_temp, Sask_area_reproj, snap='out') # remove NA areas from study area
Sask250expand <- buffer(Sask250,doEdge=TRUE, width=500) # add pixels to outer edge to ensure full
#  coverage within province polygon
Reference <- Sask250expand #set up Reference layer for BatchCrop

##################################################################################################
#newSart
setwd("C:/Users/bsmiley/Documents/Sask_work/Canada_concept/layers")
Reference <- raster("Sask250expand.tif")
OutPrj= "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 
        +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
OutRes = 30
setwd("C:/Users/bsmiley/My Documents/Sask_work/Sask/SKmask")
writeRaster(Sask30, filename="Sask30_new.tif", format='GTiff', datatype='INT1U')
Sask30 <- raster("Sask30.tif") # add Sask study area raster (30m res)
###################################################################################################
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
    a<- x * y # this stays at 250m and change resolution to 30m when running BatchCrop
    #.... still will have extra area outside of Sask but this can be clipped after
    b <- buffer(a, doEdge=TRUE, width=500)
    c <- (b-b)
    x <- merge(a, c)
    return(x)
  }
  cropped <- lapply(import,multiply, Reference)    #Crop imported layers to reference layer, argument 'x'
  f2<-function(x,y) {
    x<-projectRaster(x, crs=OutPrj, res=OutRes, method="ngb")
    return(x)
  } 
  closeAllConnections()
  beginCluster(30)
    output <- lapply(cropped,f2,OutPrj)
  multiply2<-function(x,y) {
    origin(y) <- 10 # give 30m reference same origin as X
    x <- x * y # this stays at 250m and change resolution to 30m when running BatchCrop
    #.... still will have extra area outside of Sask but this can be clipped after
    return(x)
  }
  
  clipped <- lapply(output,multiply2,Sask30)    #Clip AGAIN using 30m resolution
  #Use a 'for' loop to iterate writeRaster function for all cropped layers
  for(i in (1:max(length(filenames)))){
    writeRaster(clipped[[i]],paste(deparse(substitute(Sask)),filenames[i]), format='GTiff',
                datatype='INT1U')
  }
  removeTmpFiles_edit(0)
}
###################################################################################################
#Run BatchCrop
beginCluster(30)
setwd("C:/Users/bsmiley/Documents/Sask_work/Canada_concept/layers/ReProject/Test")
startTime <- Sys.time()
BatchCrop(Reference=Reference,OutName=Sask, OutPrj= "+proj=lcc +lat_1=49 +lat_2=77 
                    +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
                    +units=m +no_defs", OutRes=30)
endTime <- Sys.time()
elapsedTime <- endTime - startTime


##################################################################################################
##Working
setwd("C:/Users/bsmiley/Documents/Sask_work/Canada_concept/layers/ReProject/Test")
NEWfilenames <- list.files(pattern="Sask$", full.names=FALSE)

setwd("C:/Users/bsmiley/My Documents/Sask_work/Sask/Ouput_30m")
for(i in (1:max(length(NEWfilenames)))){
  writeRaster(output[[i]],paste(deparse(substitute(Clipped)),filenames[i]), format='GTiff', 
              datatype= "INT1U")
}
}
setwd("C:/Users/bsmiley/Documents/Sask_work/Canada_concept/layers")
Reference <- raster("Sask250expand.tif")
OutPrj= "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 
        +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
OutRes = 30
buff <- raster("Sask DT1crop_buff.tif")
#end time

endTime <- Sys.time()
elapsedTime <- endTime - startTime
##Working

###################################################################################################
New and improved removeTmpFiles fun (old one was broken)
##################################################################################################
removeTmpFiles_edit<-function(h=24)
{
  warnopt <- getOption("warn")
  on.exit(options(warn = warnopt))
  tmpdir <- tmpDir(create = FALSE)
  if (!is.na(tmpdir)) {
    d <- (tmpdir)
    f <- list.files(path=d, pattern='[.]gr[di]', full.names=TRUE, 
                    include.dirs=TRUE)
    fin <- file.info(f)
    dif <- Sys.time() - fin$mtime
    dif <- as.numeric(dif, units = "hours")
    dif[is.na(dif)] <- h + 1
    f <- f[dif > h]
    if (length(f) > 1) {
      unlink(f, recursive = TRUE)
    }
  }
  options(warn = warnopt)
}
#################################################################################################