# Batch Crop LANDSAT composite image rasters to a specific study area--------------------------------

#This script:

# 1) Crops a set of rasters in a batch process, renames and exports them as .tif files


# Related scripts:

# 1) RasterTemplateSetup.r

#Required packages---------------------------------------------------------------------
library(rgdal)
library(sp)
library(raster)
library(snow)
library(spatial.tools)
library(parallel)

#Setup Parameters/files----------------------------------------------------------------

#Set temp file... has to have >50gb of room
rasterOptions(tmpdir="H:/saskatchewan/data/temp")

#Template rasters generated and exported from RasterTemplateSetup.r
setwd("H:/saskatchewan/data/reference")
Crop30 <- raster("Crop30_PaFMA.tif")

# Define outdir and year folders
indir = "H:/saskatchewan/data/temp/clip"
outdir = "H:/saskatchewan/spatialGrowth"
years <- (1984:2012)

#Set batchcrop parameters
OutPrj= "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
OutRes = 30
OutName = "fmapa"

# End of raster template setup script--------------------------------------------------------

#Create timeVariant and timeInvariant with'Year' (1984-2012) directories-----------------------------
#(SHOULD ONLY NEED TO DO THIS ONCE FOR EACH Clipped AREA)

# Create folders
for (i in 1:length(years)) {
  dir.create(paste(outdir, OutName, "timeVariantRasters", years[i], sep="/"), recursive=TRUE)
}
dir.create(paste(outdir, OutName, "timeInvariantRasters", sep="/"))

# End of create directories---------------------------------------------------------------


# Run BATCHCROP function (find function script below)-------------------------------------------
# Time for BatchCrop with 6 bands of LANDSAT rasters (e.g. 1 year's worth) :11.06133 hours

#FIRST Create 'BatchCrop' and 'removeTmpFiles_edit' functions (lines 72-190)

startTime <- Sys.time()
beginCluster(30)

# location of rasters to be clipped
setwd("H:/saskatchewan/data/temp/clip") # This is where you put the rasters you would like to clip
#Run BatchCrop (For clarity, replace OutName with name of forest district clipped by)
BatchCrop(Reference=Crop30,OutName=OutName, OutPrj= "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 
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
  filenames <- list.files(paste(indir), full.names=TRUE) # list of  file names from input directory
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
  #Assign filenames layer names to rasters in 'clipped' list (occurs in order)
  names(clipped) <- filenames  
  # Create stacks for each band to export into 'year' subdirectories
  subset.f <- function(rasters, pattern) {
    ind <- grepl(pattern, names(rasters))
    return(rasters[ind])
  }
  #function to select all other clipped rasters for export
  subset_other.f <- function(rasters, pattern) {
    ind <- !grepl(pattern, names(rasters))
    return(rasters[ind])
  }
  b1 <- subset.f(clipped, pattern= "c1$")
  b2 <- subset.f(clipped, pattern= "c2$")
  b3 <- subset.f(clipped, pattern= "c3$")
  b4 <- subset.f(clipped, pattern= "c4$")
  b5 <- subset.f(clipped, pattern= "c5$")
  b6 <- subset.f(clipped, pattern= "c6$")
  #select all other clipped rasters for export (most likely time InVARIANT rasters)
  #and create a new subset list for naming
  other <- subset_other.f(clipped, pattern="skprx*")
  shortFilenames <- list.files(paste(indir), full.names=FALSE)
  shortFilenames_other <- shortFilenames[!grepl("skprx*", shortFilenames)]
  
  #Use a 'for' loop to iterate writeRaster function for all cropped layers
  #Updated writeRaster to write to new directory structure
  setwd(indir)
  for(i in (1:max(length(b1)))){
    writeRaster(b1[[i]],file.path(paste(outdir, OutName, "timeVariantRasters", years[i], 
                                             "b1", sep="/")), format='GTiff',datatype='INT2U',
                overwrite=TRUE)
  }
  for(i in (1:max(length(b2)))){
    writeRaster(b2[[i]],file.path(paste(outdir, OutName, "timeVariantRasters", years[i], 
                                        "b2", sep="/")), format='GTiff',datatype='INT2U',
                overwrite=TRUE)
  }
  for(i in (1:max(length(b3)))){
    writeRaster(b3[[i]],file.path(paste(outdir, OutName, "timeVariantRasters", years[i], 
                                        "b3", sep="/")), format='GTiff',datatype='INT2U',
                overwrite=TRUE)
  }
  for(i in (1:max(length(b4)))){
    writeRaster(b4[[i]],file.path(paste(outdir, OutName, "timeVariantRasters", years[i], 
                                        "b4", sep="/")), format='GTiff',datatype='INT2U',
                overwrite=TRUE)
  }
  for(i in (1:max(length(b5)))){
    writeRaster(b5[[i]],file.path(paste(outdir, OutName, "timeVariantRasters", years[i], 
                                        "b5", sep="/")), format='GTiff',datatype='INT2U',
                overwrite=TRUE)
  }
  for(i in (1:max(length(b6)))){
    writeRaster(b6[[i]],file.path(paste(outdir, OutName, "timeVariantRasters", years[i], 
                                        "b6", sep="/")), format='GTiff',datatype='INT2U',
                overwrite=TRUE)
  }
  for(i in (1:max(length(other)))){
    writeRaster(other[[i]],file.path(paste(outdir, OutName, "timeInvariantRasters", 
                                           shortFilenames_other[i], sep="/")),
                format='GTiff',datatype='INT2U', overwrite=TRUE)
  }
}

#End of BatchCrop function--------------------------------------------------------------------------
