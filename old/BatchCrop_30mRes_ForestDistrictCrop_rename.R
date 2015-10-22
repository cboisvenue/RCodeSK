# Batch Crop LANDSAT composite image rasters to a specific study area--------------------------------

#This script:

# 1) Setups up template study area raster used to clip landsat images
# 2) Crops a set of rasters in a batch process, renames and exports them as .tif files

#Required packages---------------------------------------------------------------------
library(rgdal)
library(sp)
library(raster)
library(snow)
# DO I NEED THIS???? library(spatial.tools)
library(parallel)
#Set temp file... has to have >50gb of room
rasterOptions(tmpdir="H:/saskatchewan/data/temp")

#----------------------------------------------------------------------------------------
## SETUP ## Generates 30m raster of a specific forest district in Province of Saskatchewan
# Total batchcrop time at 30m on A125345 workstation (30 cluster): 7.226512 (setup+batchcrop)
#----------------------------------------------------------------------------------------

#Subset Sask area to include only 'WCL Prince Albert FMA' forest district
beginCluster(30)
startTime <- Sys.time()
setwd("H:/saskatchewan/data/originals/landsat/LCC_proxy_composites") # these are from Joanne White
template30 <- raster("skprx1984c1") 
setwd("H:/saskatchewan/data")
###?? where is this from? Joanne? - from pspu.shp Max used for wall-to-wall. I selected just Sask
# polygons and made a new file
Sask_area <- readOGR(dsn = "reference", layer = "Sask_scape_reproj") # add SK vector Sask province

Sask_area <- spTransform(Sask_area, crs(template30)) # reproject Sask_area to Recliner inputs

# Change AdminBoundary based on area to be clipped by----------------------

PaFMA <- Sask_area[Sask_area$AdminBou_1 == 'WCL Prince Albert FMA',]

# End of Study area polygon select-----------------------------------------------------

# Rasterize study area based on LANDSAT projection and cell size attributes (30m res)----

PaFMA_30_temp_new <- rasterize(PaFMA, template30, field=1)

# Remove NA areas from study area
PaFMA_30_temp2 <- crop(PaFMA_30_temp, PaFMA, snap='out')

# Add pixels to outer edge to ensure full
PaFMA_30_temp3 <- buffer(PaFMA_30_temp2,doEdge=TRUE, width=500)

# Set up Reference layer for BatchCrop
Crop30 <- PaFMA_30_temp3

#Write Study area raster to 'reference' directory (change name to reflect forest district)
setwd("H:/saskatchewan/data/reference")
writeRaster(Crop30, "Crop30_PaFMA.tif", overwrite=TRUE, format='GTiff', datatype='INT2U')

endTime <- Sys.time()
elapsedTime <- endTime - startTime
# 4.526815 hours to here

# End of raster template setup script--------------------------------------------------------

#Create timeVariant and timeInvariant with'Year' (1984-2012) directories-----------------------------
#(SHOULD ONLY NEED TO DO THIS ONCE FOR EACH Clipped AREA)

# Define outdir and year folders
outdir = "H:/saskatchewan/spatialGrowth/"
years <- (1984:2012)

# Create folders
for (i in 1:length(years)) {
  dir.create(paste(outdir, OutName, "timeVariantRasters/", years[i], sep="/"), recursive=TRUE)
}
dir.create(paste(outdir, OutName, "timeInvariantRasters/", sep="/"))

setwd("H:/saskatchewan")
# End of create directories---------------------------------------------------------------

#WORKING to assigned names to clipped files-----------------------------------------------------

#assign names
for(i in filenames) {
  clipped2 <- clipped
  assign(i,clipped2)  
}

assignNames<-function(x,y) {
  assign(x,y)
  return(x)
} 

clipped2 <- lapply(filenames, assignNames, clipped)

clipped2 <- lapply(filenames, assign(filenames), clipped)


clipped2 <- names(clipped)

b1 <- as.list(clipped, pattern= "c1", full.names=FALSE)

b1 <- stack(clipped, pattern="c1", fullnames=FALSE)

b1.list <- list.files(paste(indir), pattern="c1.tif$", full.names=FALSE)






for(i in (1:max(length(filenames)))){
  names(clipped[[i]],filenames[i]))
}

#list files (group into same bands for stacking)
stack <- stack(clipped)

#stack bands for export


#export (writeRaster to directories)


# If SETUP is already completed, add study area raster to run BatchCrop on list of rasters-----------
# Time for BatchCrop with 6 bands of LANDSAT rasters (e.g. 1 year's worth) :11.06133 hours

# Add template raster (30m res) and set BatchCrop parameters---------------------------------
setwd("H:/saskatchewan/data/reference")
Crop30 <- raster("Crop30_PaFMA.tif")
OutPrj= "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
OutRes = 30
OutName = "fmapa"

# End set Batchcrop parameters--------------------------------------------------------------


startTime <- Sys.time()
beginCluster(30)

# location of rasters to be clipped
setwd("H:/saskatchewan/data/temp/clip") # This is where you put the rasters you would like to clip
#Run BatchCrop (For clarity, replace OutName with name of forest district clipped by)
BatchCrop(Reference=Crop30,OutName=PrinceAlbertFMA, OutPrj= "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 
+lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", OutRes=30)
endTime <- Sys.time()
elapsedTime_6 <- endTime - startTime
removeTmpFiles_edit(0)
###################################################################################################
##################################################################################################
#New and improved removeTmpFiles fun (old one was broken)
##################################################################################################
# CB: if I need to change the location of my temp files:
# TMP = '<your-desired-tempdir>'.
# write("TMP = '<your-desired-tempdir>'", file=file.path(Sys.getenv('R_USER'), '.Renviron'))
# I have NOT tried this yet

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
###################################################################################################
#BatchCrop is a function that takes a list of rasters, crops them to a reference study area
# and reprojects them. Below the function is a set up script which adds the rasterizes a reference
#layer from a shp file of Sask and masks out non-forest areas. Also below is the BatchCrop execution
#script
###################################################################################################
BatchCrop<-function(Reference,OutName,OutPrj,OutRes){
  filenames <- list.files(full.names=FALSE)   #Extract list of  file names from working directory
  library(raster) #Calls 'raster' library
  #Function 'f1' imports data listed in 'filenames' and assigns projection
  f1<-function(x,z) {
    y <- raster(x)
    projection(y) <- CRS(z)
    return(y)
  }
  import <- mclapply(filenames,f1,projection(Crop30))
  f2<-function(x,y) {
    x<-projectRaster(x, crs=OutPrj, res=OutRes, method="ngb")
    return(x)
  } 
  closeAllConnections()
  beginCluster(30)
  output <- mclapply(import,f2,OutPrj)
  multiply2<-function(x,y) {
    origin(y) <- 10 # give 30m reference same origin as X
    x <- x * y # this stays at 250m and change resolution to 30m when running BatchCrop
    #.... still will have extra area outside of Sask but this can be clipped after
    return(x)
  }  
  clipped <- mclapply(import,multiply2,Crop30)    #Clip AGAIN using 30m resolution
  #Use a 'for' loop to iterate writeRaster function for all cropped layers
  #Updated writeRaster to write to new directory structure
  for(i in (1:max(length(filenames)))){
    writeRaster(clipped[[i]],file.path(paste(outdir, OutName, "/timeVariantRasters/", years[i], 
                                             filenames[i], sep="/")), format='GTiff',datatype='INT2U')
  }
}



#### Things are not working as I thought...checking rasters, code etc.

#This batching took 1.15 hours...that seems wrong ##HAHA checked the wrong file!! it took 10 hours :)
# check rasters
setwd("H:/Saskatchewan/TestingRasters/ToClip") # same as working dir when the BatchCrop function was run
out.rast1 <- list.files(pattern=".tif$", full.names=FALSE)
bands85 <- stack(out.rast1)
# looks like the right values....