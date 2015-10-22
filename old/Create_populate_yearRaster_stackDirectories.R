#----------------------------------------------
# Creation of Directories by year with all rasters required as predictor variables for biomass spreading

# This script:
# 1-Creates year rasters for clipped study area (i.e.Crop30_PaFMA.tif)
# 2-Creates by-year directory structure (should only need to be done once)
# 3-Imports and renames clipped landsat images into the by-year directory structure

# Bsmiley
# May 14, 2015
#-----------------------------------------------------------

require(sp)
require(rgdal)
require(raster)
require(rgeos)
require(parallel)

#File Locations-----------------------------------------------------------------------------------

timeINvariant = "H:/saskatchewan/spatialGrowth/timeInvariantRasters/"
years <- (1984:2012)
timeVARIANT = paste("H:/saskatchewan/spatialGrowth/timeVariantRasters/", years[1:29],"/",sep="")

#-----Create and export 'YEAR' raster for stack for each year and insert into newly created 
# directory labelled for each year---------------------------------------------------------------------

#Add in study area raster (pixel values equal to 1)
setwd("H:/Saskatchewan/bsmiley_work/Sask/Sask_area_datasets")
StudyArea <- raster("Crop30_PaFMA.tif") # raster where forestDistrict study area pixels =1

#Function to multiple study area raster by each year values 
# (e.g. for each pixel, 1X 1984 = 1984 pixel value)
create.year<-function(x,b) {
  x <- x * b
  
  return(x)
}

# Mutlicore lapply to multiple year by study area raster
year.ras <- mclapply(years,create.year,StudyArea)
#create.year
#Stack year rasters for export
year.stack <- stack(year.ras)

#Create 'Year' (1984-2012) directories (SHOULD ONLY NEED TO DO THIS ONCE)-----------------

for (i in 1:length(years)) {
  dir.create(paste("H:/Saskatchewan/Biomass_Growth/Time_Variant_rasters/", years[i], sep=""))
}
# End of create directories---------------------------------------------------------------


#Export each raster into its designated year directory------------------------------------

writeRaster(year.stack, file.path(paste(timeVARIANT, years,"/",sep="")),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)

#End of year raster/directory creation-----------------------------------------------------------------


# Add clipped LANDSAT rasters to folder structure------------------------------------------------------

#Create lists for each band stack
indir <- "H:/Saskatchewan/clipped"
b1.list <- list.files(paste(indir), pattern="c1.tif$", full.names=FALSE)
b2.list <- list.files(paste(indir), pattern="c2.tif$", full.names=FALSE)
b3.list <- list.files(paste(indir), pattern="c3.tif$", full.names=FALSE)
b4.list <- list.files(paste(indir), pattern="c4.tif$", full.names=FALSE)
b5.list <- list.files(paste(indir), pattern="c5.tif$", full.names=FALSE)
b6.list <- list.files(paste(indir), pattern="c6.tif$", full.names=FALSE)

#Create stacks of each band
setwd(paste(indir))
b1_stack <- stack(b1.list)
b2_stack <- stack(b2.list)
b3_stack <- stack(b3.list)
b4_stack <- stack(b4.list)
b5_stack <- stack(b5.list)
b6_stack <- stack(b6.list)


#select rasters from the stack that end in b1 ---> export, b2, export, etc.

#Export each raster into its designated year directory
writeRaster(b1_stack, file.path(paste(timeVARIANT,"/",sep=""), "b1"),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)
writeRaster(b2_stack, file.path(paste(timeVARIANT,"/",sep=""), "b2"),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)
writeRaster(b3_stack, file.path(paste(timeVARIANT,"/",sep=""), "b3"),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)
writeRaster(b4_stack, file.path(paste(timeVARIANT,"/",sep=""), "b4"),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)
writeRaster(b5_stack, file.path(paste(timeVARIANT,"/",sep=""), "b5"),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)
writeRaster(b6_stack, file.path(paste(timeVARIANT,"/",sep=""), "b6"),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)

#End of addition of clipped rasters to 'Year' directories----------------------------------------------