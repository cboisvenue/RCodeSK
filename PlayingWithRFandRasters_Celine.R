#----------------------------------------------
# Playing around with the dominant species RF model
# Trying to spread it across the lanscape (Prince ALbert FMA)
# BUilding on Byron's code and creation of raster stacks
#
# NOTE: for this code to work, all the predictor variable rasters
#   for the RF have to be in one place (in this case here: 
#   H:/Saskatchewan/TestingRasters/TrialswDOM).
#
# First playing with the dom_sps RF
# second trying to see if I can create more YEARS rasters
# third trying to see what I can do with a biomass model (based on all the previous RF work for biomass)
# 
# CBoisvenue
# April 16, 2015
#-----------------------------------------------------------

# PACKAGES 
require(plyr)
require(ggplot2)         # Graphics engine for generating all types of plots
require(dplyr)           # Better data manipulations
require(reshape2)        # for transforming wide data into long data (melt)

#library(randomForest)
#library(randomForestSRC) # random forests for survival, regression and classification
#library(ggRandomForests) # ggplot2 random forest figures

# 4- I think that the "predict" function does not care what type of object you pass to it. 
# So you could load these packages:
require(sp)
require(rgdal)
require(raster)
require(rgeos)


#CLIPPED LANDSAT Rasters rasters can be found in 
# H:\Saskatchewan\clipped

#-------------------------------------------------------------

# X VARIABLES USED TO PREDICT DOMINANT SPECIES
# YEAR,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b5,b6,ndvi
# ndvi = ((b4-b3)/(b4+b3)) )

#-------------------------------------------------------------
# Create ndvi and YEAR rasters and add raster stack

# the use of cluster() is automoatic for these raster package functions: 
#predict
#projectRaster
#resample
#extract

beginCluster(30)

# CREATE LIST OF RASTERS
# #Create and export ndvi raster with values multiplied by 10000
# setwd("H:/Saskatchewan/ForNDVI") # Copy CLIPPED bands 3 and 4 from year of interest into this folder
# b3 <- raster("Sask skprx2012c3.tif")
# b4 <- raster("Sask skprx2012c4.tif")
# ndvi<- (((b4-b3)/(b4+b3)) * 10000)
# setwd("H:/Saskatchewan/TestingRasters") # put ndvi raster here so it can be stacked
# writeRaster(ndvi, filename="ndvi.tif", format='GTiff', datatype='INT2S', overwrite=TRUE)

#Create and export 'YEAR' raster for stack

# setwd("H:/Saskatchewan/bsmiley_work/Sask/Sask_area_datasets")
# # raster where forestDistrict study area pixels =1
# StudyArea <- raster("Crop30_PaFMA.tif") 
# # multiple by the year for which you are predicting
# yr = 2012
# YEAR <- StudyArea * yr 
# # put YEAR raster here so it can be stacked
# setwd("H:/Saskatchewan/TestingRasters/TrialswDOM") 
# writeRaster(YEAR, filename=paste("YEAR",yr,".tif",sep=""), format='GTiff', datatype='INT2U', overwrite=TRUE)

# Need to copy and rename bands 1, 2, 5 and 6 from year of interest and other predictor rasters in:
# H:\Saskatchewan/TestingRasters so we can create a stack
setwd("H:/Saskatchewan/TestingRasters/TrialswDOM")
#Extract list of  file names from working directory
rlist <- list.files(pattern=".tif$", full.names=FALSE)

# CREATE RASTER STACK
xvars <- stack(rlist) #for model

#-------------------------------------------------------------
# trying to import the RF built with SRC - this model is named rf.spsSRC
load("rfdomspeciesSRC.RData")
rf.spsSRC
## DID NOT WORK SAME ERROR AS BYRON

# TRY THIS ONE BUILT WITH RF PCKG
require(randomForest)
load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/DominantSpeciesPrediction/rfdomspeciesRF.RData")
rf.spsRF
#rm(rf.spsRF)
#BF=1 BP=2 BS = 3 JP = 4 TA = 5 TL = 6 WB = 7 WS = 8

load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/DominantSpeciesPrediction/rfdomspeciesSRC1.RData")
rf.spsSRC1
rm(rf.spsSRC1)
#-------------------------------------------------------------------
# PREDICT MODEL 

# use rf model built from randomForestSRC
predsps.src <- predict(object=xvars, model=rf.spsSRC, type="response", na.rm=TRUE)
# FAILED Current error:"Error in p[-naind, ] <- predv : 
# number of items to replace is not a multiple of replacement length")

# tried to predict that model another way...
# DO NOT RUN THE FOLLOWING LINE IT TAKES UP TOO MUCH OF THE MERMORY!!
#results <- predict.rfsrc(rf.spsSRC,as.data.frame(xvars),importance="permute",na.action="na.impute")

#  use rf model built from randomForest package
predsps.rf <- predict(object=xvars, model=rf.spsRF, type="response", na.rm=TRUE)
# works but takes for ever
require(snow)
beginCluster()
startTime <- Sys.time()
predsps.rf1 <- predict(object=xvars, model=rf.spsRF, type="response", na.rm=TRUE)
endTime <- Sys.time()
elapsedTime <- endTime - startTime
endCluster()
# could clusterR() make this faster?
endCluster

predsps.src1 <- predict(object=xvars, model=rf.spsSRC1, type="response", na.rm=TRUE)
# same error as above
# Error in p[-naind, ] <- predv : 
#   number of items to replace is not a multiple of replacement length

setwd("H:/Saskatchewan/DOMSp_rasters")
writeRaster(predsps.rf, filename="domspsRF2012.tif", format='GTiff', datatype='INT2U')


#pred.sps <- raster("DomSpsRF1.img")


