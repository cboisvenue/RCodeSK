#----------------------------------------------
# Creation of YEAR, age1 and ht1 rasters and by-year directories for biomass spreading 
# Also relcassifies soil_moist_regime and casfri_dom rasters to coincide with RFModel factors

# This script:
# 1-Creates YEAR rasters for clipped study area (i.e.Crop30_PaFMA.tif)
# 2-Processes yearly age and height rasters
# 3-Creates by-year directory structure (should only need to be done once)
# 4-exports and renames YEAR, age and ht rasters into the by-year directory structure
# 5- Reclassifies raster values of soil_moist_regime and casfri_dom and exports to 
# timeInvariant folder

# Bsmiley
# May 27, 2015
#-----------------------------------------------------------

require(spatial.tools)
require(rgdal)
require(rgeos)
require(parallel)
require(raster)

#File Locations-----------------------------------------------------------------------------------
OutName = "fmapa"
intermediate_input = paste("H:/saskatchewan/spatialGrowth/",OutName, "/casfri_intermediate",
                      sep="")
years <- (1984:2012)
timeVARIANT = paste("H:/saskatchewan/spatialGrowth/",OutName, "/timeVariantRasters/",
                    years[1:29],"/",sep="")

timeINvariant = paste("H:/saskatchewan/spatialGrowth/",OutName, "/timeInvariantRasters",
                      sep="")

#-----Create a 'YEAR' raster for stack for each year---------------------------------------------

#Add in study area raster (pixel values equal to 1)
setwd("H:/Saskatchewan/data/reference")
StudyArea <- raster("Crop30_PaFMA.tif") # raster where forestDistrict study area pixels =1

beginCluster(30)
#Function to multiple study area raster by each year values 
# (e.g. for each pixel, 1X 1984 = 1984 pixel value)
create.year<-function(x,b) {
  x <- x * b  
  return(x)
}

# Mutlicore lapply to multiple year by study area raster
year.ras <- mclapply(years,create.year,StudyArea)

#Stack year rasters for export
year.stack <- stack(year.ras)

# End of YEAR raster stack creation---------------------------------------------------------------

# Creates the age and height rasters for each year (1984-2012) which are input--------------------
# into the RF model
beginCluster(30)
#Add dist_yr and photo_yr rasters
setwd(intermediate_input)
dist_yr <- raster("dist_yr.tif")
photo_yr <- raster("photo_yr.tif")

#make casfri age raster
cas_age <- photo_yr-dist_yr
#this creates a raster brick of the 29 age rasters for each year (1984-2012)
age_perYr <- cas_age - (photo_yr - year.stack)
#need to reclassify rasters so that cells where no age exists become zero (otherwise the cell values
# correspond to the YEAR (1984, 1985, etc.))
sub_df <- data.frame(years, 0)
age_reclass <- subs(test, sub_df, by=1, which=2, subsWithNA=FALSE)

# End of create age rasters---------------------------------------------------------

# Create height rasters-------------------------------------------------------------

# Add height rasters
setwd(intermediate_input)
ht <- raster("ht.tif")
# MASK HERE???????????????????
#ht <- reclassify(test, reclass_df)

#function to create per-year ht rasters
ht_yr <- (photo_yr - year.stack) * 0.25+ ht
#reclass_df <- c(-99, 0, 0)
#reclass_df <- matrix(reclass_df, ncol=3, byrow=TRUE)
#ht_reclass <- reclassify(test, reclass_df)

# End of create ht rasters---------------------------------------------------------

#Write Rasters to file-------------------------------------------------------------

#Export each YEAR raster into its designated year directory-----------------

writeRaster(year.stack, file.path(paste(timeVARIANT, "YEAR",sep="")),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)

#End of export YEAR rasters------------------------------------------------

#Export each age raster into its designated year directory-----------------

writeRaster(age_reclass, file.path(paste(timeVARIANT, "age1" ,sep="")),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)

#End of export YEAR rasters------------------------------------------------

#Export each ht raster into its designated year directory-----------------

writeRaster(ht_yr, file.path(paste(timeVARIANT, "ht1" ,sep="")),
            format='GTiff', bylayer=TRUE, datatype='FLT4S', overwrite=TRUE)

#End of export YEAR rasters------------------------------------------------

#End of export rasters------------------------------------------------------------

# End of create YEAR, age and ht rasters--------------------------------------------------------------

# Reclassify casfri_dom and soil_moist_regime rasters-------------------------------------------------

#Get rasters--------------------------------------------------------------------------------------
setwd(intermediate_input)
casfri_dom <- raster("casfri_dom.tif")
soil_moist_regime <- raster("soil_moist_regime.tif")
# End get rasters---------------------------------------------------------------------------------

# Reclassify casfri_dom using the substitute function (subs) to substitute existing raster values
#with ones that match the RFModel-----------------------------------------------------------------
casfri_table <- data.frame(old=c(1:3, 6, 8:10), update=c(3,7,5,4,6,1,2))
beginCluster(30)
casfri_dom_reclass <- subs(casfri_dom, casfri_table, by=1, which=2, subsWithNA=TRUE)
writeRaster(casfri_dom_reclass, file.path(paste(timeINvariant, "casfri_dom" ,sep="/")),
            format='GTiff', datatype='INT2U', overwrite=TRUE)

# End reclassify casfri_dom----------------------------------------------------------------------

# Reclassify soil_moist_regime using the substitute function (subs) to substitute existing raster values
#with ones that match the RFModel-----------------------------------------------------------------

soilMoist_table <- data.frame(old=c(1:5,7), update=c(5,4,1,6,3,2))
beginCluster(30)
soil_moist_reclass <- subs(soil_moist_regime, soilMoist_table, by=1, which=2, subsWithNA=TRUE)
writeRaster(soil_moist_reclass, file.path(paste(timeINvariant, "soil_moist_reg" ,sep="/")),
            format='GTiff', datatype='INT2U', overwrite=TRUE)

# End reclassify soil_moist_reg------------------------------------------------------------------



