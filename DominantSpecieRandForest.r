#----------------------------------------------
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
library("RColorBrewer")    # Nice color palettes

library("randomForestSRC") # random forests for survival, regression and classification
library("ggRandomForests") # ggplot2 random forest figures

#-------------------------------------------------------------

# X VARIABLES USED TO PREDICT DOMINANT SPECIES
# PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b5,b6,ndvi
# ndvi = ((b4-b3)/(b4+b3)) )

#-------------------------------------------------------------

# LOAD THE RF MODEL
# this was the final model, it will have the same name once you load it

#rf.ndvi <- rfsrc(domsps~.,data=ndvi,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

load("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/DominantSPeciesRF/rfdomspecies.RData")

#-------------------------------------------------------------

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
rlist=list.files(getwd(), pattern="img$", full.names=TRUE) 

# CREATE RASTER STACK
xvars <- stack(rlist)     

# PREDICT?
# PREDICT MODEL
predict(object=rf.ndvi,newdata=xvars,importance="permute",na.action="na.impute")
  






