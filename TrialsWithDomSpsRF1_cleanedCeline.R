#----------------------------------------------
# MISSY CODE!! NOT CLEANED-UP
# Predicting Dominant Species with random forest model
# This script is to help predict dominant species values for creating a raster
# It:
# 1-lists the packages used to develop the random forest model
# 2-loads the appropriate RF model for dominant species prediction
# 3-gives a 1st try at "predicting"...
# 
# CBoisvenue
# March 19, 2015
#-----------------------------------------------------------

# PACKAGES USED IN RF MODEL BUILDING
require(plyr)
require(ggplot2)         # Graphics engine for generating all types of plots
require(dplyr)           # Better data manipulations
#library(gridExtra)  
require(reshape2)        # for transforming wide data into long data (melt)
library(RColorBrewer)    # Nice color palettes

#library(randomForest)
library(randomForestSRC) # random forests for survival, regression and classification
library(ggRandomForests) # ggplot2 random forest figures

# 4- I think that the "predict" function does not care what type of object you pass to it. 
# So you could load these packages:
require(sp)
require(rgdal)
require(raster)
require(rgeos)


#CLIPPED LANDSAT Rasters and predictor rasterscan be found in 
# H:\Saskatchewan\clipped

#-------------------------------------------------------------

# X VARIABLES USED TO PREDICT DOMINANT SPECIES
# PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b5,b6,ndvi
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
#Create and export ndvi raster with values multiplied by 10000
setwd("H:/Saskatchewan/ForNDVI") # Copy CLIPPED bands 3 and 4 from year of interest into this folder
b3 <- raster("Sask skprx2012c3.tif")
b4 <- raster("Sask skprx2012c4.tif")
ndvi<- (((b4-b3)/(b4+b3)) * 10000)
setwd("H:/Saskatchewan/TestingRasters") # put ndvi raster here so it can be stacked
writeRaster(ndvi, filename="ndvi.tif", format='GTiff', datatype='INT2S', overwrite=TRUE)

#Create and export 'YEAR' raster for stack
setwd("H:/Saskatchewan/bsmiley_work/Sask/Sask_area_datasets")
StudyArea <- raster("Crop30_PaFMA.tif") # raster where forestDistrict study area pixels =1
YEAR <- StudyArea * 2012 # multiple by the year for which you are predicting
setwd("H:/Saskatchewan/TestingRasters") # put YEAR raster here so it can be stacked
writeRaster(YEAR, filename="YEAR.tif", format='GTiff', datatype='INT2U', overwrite=TRUE)

#Copy and rename bands 1, 2, 5 and 6 from year of interest and other predictor rasters in:
#H:\Saskatchewan/TestingRasters
setwd("H:/Saskatchewan/TestingRasters/TrailswDOM")
#Extract list of  file names from working directory
rlist <- list.files(pattern=".tif$", full.names=FALSE)

# CREATE RASTER STACK
xvars <- stack(rlist) #for model

#-------------------------------------------------------------

# CREATE THE RF MODEL

#bsmiley efforts for building random forest model-------------------------------------------

setwd("H:/Saskatchewan/bsmiley_work/Sask")
PSPs <- readOGR(dsn = "Sask_area_datasets", layer = "Sask_PSP_plots") # add PSPs
SaskPolygons <- readOGR(dsn = "Sask_area_datasets", layer = "Sask_scape_reproj") # add Sask polygons
ForestDistrict <- SaskPolygons[SaskPolygons$AdminBou_1 == 'WCL Prince Albert FMA',]
PSPs <- spTransform(PSPs, crs(YEAR)) # reproject PSPs to match the projection in the stack (YEAR)
ForestDistrict <- spTransform(ForestDistrict, crs(YEAR)) # reproject PSPs to match the projection in the stack (YEAR)

# this removes all plots outside of the specific Forest District boundary
over(PSPs,as(ForestDistrict,"SpatialPolygons"))
PSPs = PSPs[!is.na(over(PSPs,as(ForestDistrict,"SpatialPolygons"))),]

#extract predictor variable values form raster stack where plots are and create dataframe from it
train <- extract(xvars, PSPs, na.rm=TRUE)
train.df <- as.data.frame(train)
PLOT_ID <- as.data.frame(PSPs[,3])[,3]
train2 <- cbind(train.df,PLOT_ID)

#join domsps to plot predictor attributes
#change domsps (now 'sps') to numeric
#BS = 3
#JP = 4
#TA = 5
#TL = 6
#WB = 7
#WS = 8
domsps <- read.table("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/DominantSpeciesPrediction/PSP418_DomSpecies.txt",header=TRUE,sep=",")
domsps <- domsps[,c(1:3)]
train2 <- train2[,-10]
train3 <- left_join(train2,domsps, by="PLOT_ID")
sps <- as.factor(as.numeric(train3$domsps))
train4 <- cbind(train3[,-12],sps)

#randomForestSRC building (this is the ideal model platform but does not seem to work well with the
# raster package 'predict' function
# model <- sps ~ YEAR + b1 + b2 + b5 + b6 + CDED + ndvi + SLOPE_DEG + TSRI + TWI
# rfsrc1 <- rfsrc(model, train4,na.action="na.impute")

# dealing with NA error in predictors (should have removed NAs now....)
# na.train4 <- which(is.na(train4))
# train4[is.na(train4)] <- 10

# randomForest build (this seems to work with the raster package 'predict' function)
require(randomForest)
rf1.1 <- randomForest(data=train4,y=train4[,12],x=train4[,c(1:9,11)],ntree=2000,importance=TRUE,mtry=6,na.action=na.pass)
rf1.1
#plot(rf1.1)

# PREDICT
# the only way I know how to predict with RF models is this:
# 1- create a dataset with the same x variables as your training data set. For us this means these variables:
# PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b5,b6,ndvi (so maybe a stack??)
# I would create a dataframe would those in it (let's call it x.to.predict.sps)
# 2- use the predict fonction to create an object with the predictions in it. Example:
#pred.dom.sps <- predict(rf.ndvi,x.to.predict.sps)
# 3- The object created is and S3. If I just wanted to predicted values I would extract them this way:
#pred.dom.sps$predicted


# the model building dataframe looked like this:
# > head(ndvi)
# PLOT_ID YEAR domsps CDED SLOPE_DEG      TSRI      TWI  b1  b2  b5  b6      ndvi
# 1   20004 1951     BS  494  0.796191 0.0738212 9.971191  NA  NA  NA  NA        NA
# 2   20004 1961     BS  494  0.796191 0.0738212 9.971191  NA  NA  NA  NA        NA
# 3   20004 1970     BS  494  0.796191 0.0738212 9.971191  NA  NA  NA  NA        NA
# 4   20004 1979     BS  494  0.796191 0.0738212 9.971191  NA  NA  NA  NA        NA
# 5   20004 1996     BS  494  0.796191 0.0738212 9.971191 249 417 689 329 0.6822857
# 6   20005 1951     BS  495  0.946502 0.0061300 9.855331  NA  NA  NA  NA        NA
# so I assume the names would have to be the same...(easy for me to change the names in the input)

#-------------------------------------------------------------------
# PREDICT MODEL (bsmiley)

# DOES NOT WORK AT PRESENT #
# generates a predicted DOM species raster from the xvars stack of predictor variables and
# the randomForestSRC model (Current error:"Error in p[-naind, ] <- predv : 
# number of items to replace is not a multiple of replacement length")

DomSpsRF1_SRC <- predict(object=xvars, model=rf1.1, type="response", na.rm=TRUE)

setwd("H:/Saskatchewan/DOMSp_rasters")
writeRaster(DomSpsRF1_SRC, filename="DomSpsRF1_SRC.tif", format='GTiff', datatype='INT2U')

# WORKS # 
# Generates a predicted DOM species raster from the xvars stack of predictor variables and
# the randomForest model
DomSpsRF1 <- predict(object=xvars, model=rf1.1, type="response", na.rm=TRUE)

setwd("H:/Saskatchewan/DOMSp_rasters")
writeRaster(DomSpsRF1, filename="DomSpsRF1.tif", format='GTiff', datatype='INT2U')


pred.sps <- raster("DomSpsRF1.img")


