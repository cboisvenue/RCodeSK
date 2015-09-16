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
library(gridExtra)  
require(reshape2)        # for transforming wide data into long data (melt)
library(RColorBrewer)    # Nice color palettes

library(randomForest)
library(randomForestSRC) # random forests for survival, regression and classification
library(ggRandomForests) # ggplot2 random forest figures

#-------------------------------------------------------------

# X VARIABLES USED TO PREDICT DOMINANT SPECIES
# PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b5,b6,ndvi
# ndvi = ((b4-b3)/(b4+b3)) )

#-------------------------------------------------------------

# LOAD THE RF MODEL
# this was the final model, it will have the same name once you load it

#rf.ndvi <- rfsrc(domsps~.,data=ndvi,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/DominantSpeciesPrediction/rfdomspeciesNOID.RData")

#bsmiley efforts for building random forest model-------------------------------------------------------------
setwd("C:/Users/bsmiley/Documents/Sask_work/Sask")
PSPs <- readOGR(dsn = "Sask_area_datasets", layer = "Sask_PSP_plots") # add SK vector Sask province
PSPs <- spTransform(PSPs, crs(YEAR)) # reproject Sask_area to Recliner inputs
PSPs <- crop(PSPs, YEAR)

train <- extract(xvars, PSPs, na.rm=TRUE)
train.df <- as.data.frame(train)
PLOT_ID <- as.data.frame(PSPs[,3])[,3]
train2 <- cbind(train.df,PLOT_ID)

domsps <- read.table("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/DominantSpeciesPrediction/PSP418_DomSpecies.txt",header=TRUE,sep=",")
domsps <- domsps[,c(1:3)]
train2 <- train2[,-10]
train3 <- left_join(train2,domsps, by="PLOT_ID")
sps <- as.factor(as.character(train3$domsps))
train4 <- cbind(train3[,-12],sps)

model <- sps ~ YEAR + b1 + b2 + b5 + b6 + CDED + ndvi + SLOPE_DEG + TSRI + TWI
rf1 <- rfsrc(model, train4,na.action="na.impute")

# dealing with NA error in predictors
na.train4 <- which(is.na(train4))
train4[is.na(train4)] <- 10
rf1.1 <- randomForest(data=train4,y=train4[,12],x=train4[,c(1:9,11)],ntree=500,importance=TRUE,mtry=6,na.action=na.pass)

plot(rf1)

# PREDICT
# the only way I know how to predict with RF models is this:
# 1- create a dataset with the same x variables as your training data set. For us this means these variables:
# PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b5,b6,ndvi (so maybe a stack??)
# I would create a dataframe would those in it (let's call it x.to.predict.sps)
# 2- use the predict fonction to create an object with the predictions in it. Example:
pred.dom.sps <- predict(rf.ndvi,x.to.predict.sps)
# 3- The object created is and S3. If I just wanted to predicted values I would extract them this way:
pred.dom.sps$predicted

# 4- I think that the "predict" function does not care what type of object you pass to it. 
# So you could load these packages:
require(sp)
require(rgdal)
require(raster)

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

# CREATE LIST OF RASTERS

setwd("H:/Saskatchewan/TestingRasters")
b3 <- raster("Sask skprx2012c3.tif")
b4 <- raster("Sask skprx2012c4.tif")
YEAR <- raster("YEAR.tif")
YEAR <- YEAR * 12

beginCluster(30)
ndvi<-((b4-b3)/(b4+b3))
writeRaster(ndvi, filename="ndvi.tif", format='GTiff', datatype='INT2U')
writeRaster(YEAR, filename="YEAR.tif", format='GTiff', datatype='INT2U')

#Extract list of  file names from working directory
rlist <- list.files(pattern=".tif$", full.names=FALSE)
rlist2 <- list.files(pattern=".tif$", full.names=FALSE)

b1 <- raster("b1.tif")
b2 <- raster("b2.tif")
b5 <- raster("b5.tif")
CDED <- raster("CDED.tif")
ndvi <- raster("ndvi.tif")
SLOPE_DEG <- raster("SLOPE_DEG.tif")
TSRI <- raster("TSRI.tif")
TWI <- raster("TWI.tif")
#YEAR <- raster("YEAR.tif")
b6 <- raster("b6.tif")

# CREATE RASTER STACK
xvars <- stack(rlist) #for model
xvars2 <- stack(rlist2) #for prediction


# PREDICT?
# PREDICT MODEL
predict(object=rf.ndvi.noID,newdata=xvars,importance="permute",na.action="na.impute")

# PREDICT MODEL (bsmiley)
test <- predict(object=xvars2, model=rf1, na.rm=FALSE)
predict(xvars, rf1.1, filename="DomSpsRF1.img", type="response", 
        na.rm=TRUE, progress="window", overwrite=TRUE)
setwd("H:/Saskatchewan/TestingRasters")
pred.sps <- raster("DomSpsRF1.img")



#trials
xvars3 <- as.data.frame(xvars2)
test <- predict.rfsrc(object=rf1, newdata=xvars3)




