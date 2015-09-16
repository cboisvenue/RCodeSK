#----------------------------------------------
# RF Model Biomass spreading across Raster
# Predict biomass across study area raster
# This script is to help predict (dominant species) biomass values for creating a raster
# It:
# 1-lists the packages used to develop the random forest model
# 2-loads the appropriate RF model for dominant species prediction
# 3-gives a 1st try at "predicting"...
# 
# Bsmiley
# May 12, 2015
#-----------------------------------------------------------

require(sp)
require(rgdal)
require(raster)
require(rgeos)
require(parallel)
require(randomForest)

#File Locations-----------------------------------------------------------------------------------
OutName="casfri"
years <- (1984:2012)
timeINvariant = paste("H:/saskatchewan/spatialGrowth/",OutName, "/timeInvariantRasters/", sep="")
timeVARIANT = paste("H:/Saskatchewan/spatialGrowth/", 
                    OutName, "/timeVariantRasters/", years[1:29],"/",sep="")

# Create Raster Stacks for biomass prediction--------------------------------------------------------

# This creates the Time-invariant Raster stack ---------------------------------------------------
invar.list <- list.files(paste(timeINvariant), pattern=".tif$", full.names=FALSE)
setwd(paste(timeINvariant))
invar.stack <- stack(invar.list)
# End of time-invariant raster stack creation -----------------------------------

# This creaste the Time-variant Raster Stack-----------------------------------------------------
x <- list.dirs(timeVARIANT)

stack.create<-function(x,y,z) {
  setwd(x)
  y <- list.files()
  z <- stack(y)
  return(z)
}

# Mutlicore lapply to multiple year by study area raster
# This stacks the time variant rasters, creates a list of stacks
stacks <- mclapply(x,stack.create)
# End of time variant stack creation ------------------------------------------------

# Double-stacking---------------------
# this stacks the list of time variant (stacks) with the time invariant
stack.double<- mclapply(stacks,stack,invar.stack)
# End double-stacking-----------------

#Apply rf model----------------------------------------------------------------------------------------
load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/biomass/RFModel3.RData")

#predict biomass using mclapply function
#For fmapa:
# took 1.075484 days for all 29 rasters
  # 1.012083 days to spread, 45 min to write

#For CASFRI area:
# took4.599206 days to spread, 5.865828 hours to write------------------------------------------------
mc_predict<-function(x,z) {
  z <- predict(x, model=rf.mix1, type="response", na.rm=TRUE)
  return(z)
}
beginCluster(30)
startTime <- Sys.time()
biomass <- mclapply(stack.double, mc_predict)
endTime <- Sys.time()
elapsedTime_spread <- endTime - startTime

#write rasters (46.61109 min)-------------------------------------------------------------------
startTime <- Sys.time()
for(i in (1:max(length(biomass)))){
  writeRaster(biomass[[i]],file.path(paste("H:/saskatchewan/spatialGrowth/", OutName, "/biomass/", 
                                           "bio_", years[i], sep="")), format='GTiff',datatype='INT2U', 
              overwrite=TRUE)
}
endTime <- Sys.time()
writeTime <- endTime - startTime
# End of biomass spreading-------------------------------------------------------------------------------

#TEST------------------------------------------ #Dont bother trying to impute NA values at this point
test3 <- predict(stack.double[[29]], model=rf.mix1, type="response", na.action="na.impute")
test2 <- predict(stack.double[[29]], model=rf.mix1, type="response", na.rm=FALSE) ##???? try?

