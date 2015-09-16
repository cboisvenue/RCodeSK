#----------------------------------------------
# Growth Raster project: seeing how well the RS data can predict PSP species to 
# eventually use species as an input to a biomassRandomForest predict model
# 
# sources of sps info: PSPs (real), Species2010_binary, Species2010_proportions6, CASFRI
# up to now, only the SPECIES2010 have been compared to the PSP data
#
# CBoisvenue
# March 2nd, 2015
#-----------------------------------------------------------


require(plyr)
#install.packages("ggplot2")
require(ggplot2)         # Graphics engine for generating all types of plots
#install.packages("dplyr")
require(dplyr)           # Better data manipulations
#  install.packages("gridExtra")
library(gridExtra)  

#require(randomForest)
require(reshape2)        # for transforming wide data into long data (melt)

library("RColorBrewer")    # Nice color palettes

# Analysis packages.
library("randomForestSRC") # random forests for survival, regression and classification
library("ggRandomForests") # ggplot2 random forest figures

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/")
#-------------------------------------------------------------
# 1. Read in the PSP domninant species (real)
dompsp <- read.table("G:/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/PSP418_DomSpecies.txt",
                     sep=",",header=TRUE)

# 2. Read-in the RS data
rs.raw <-read.table("./data/SK_psp_extract.txt",header=TRUE,sep=",")

# reformat RS data
# columns 20:193 are the bands per each of the 29 years, 4:15 are species from the SPECIES2010 
# data, 16:19 are the elevation, slope, and calculated indices Topographic Solar Radiation Index
# (TSRI) and the Topographic Wetness Index (TWI).

# time-invariant RS variables
rs1 <- select(rs.raw,PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI)

# time-variant columns - need to be put in long format
rs2 <- rs.raw[,c(1,20:193)]
rs3 <- melt(rs2,id.vars="PLOT_ID",variable.name="YEAR",value.name="proxy")
# separating the band number and the year
rs3$YEAR <- as.character(rs3$YEAR)
rs4 <- cbind(rs3[-2],read.fwf(textConnection(rs3$YEAR),widths = c(3,4),col.names=c("band","YEAR")))
# the bands need to be variables per year per plot
rs5<-dcast(rs4,PLOT_ID+YEAR~band,value.var="proxy")
names(rs5) = c("PLOT_ID","YEAR","b1","b2","b3","b4","b5","b6")

# this dataframe is for future use - output
rs.out <- left_join(rs5,rs1)
write.table(x=rs.out,"G:/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/RS_12vars.txt",
            sep=",",row.names=FALSE)

# dompsp with time invariant plot info
w.rs1 <- left_join(dompsp,rs1)
# the bands
in.rf <- left_join(w.rs1,rs5)


#-------------------------------------------
#3. Random Forest model -------------------------------------------------------------------------
# PLAN: 
#   1. predict sps using the RS non-time variant variables (coloumns 16:19) and the bands (20:193)
#       match the bands per year...
# run model all models looking only at overall error rate, then pick on and create all evaluation plots
# as per the ggRanForesTrial.r.

# for a repeatable model
set.seed(215)

# graph of correlations
# library(corrplot)
# M <- cor(in.rf[,c(2:5,7:12)])
# corrplot(M)#, method="number"

# 1st try  this was done with the randomForest package in R, I am now going with another,randomForestSRC
# because I can use the ggRandomForests package with this new package
# set.seed(215)
# rf.base <- randomForest(x=in.rf[,c(2:5,7:12)], y=in.rf[,13],ntree=20000,importance=TRUE,proximity=TRUE) 
# varImpPlot(rf.base, type=1) # most important variable to least from top (most) to bottom (least) on
# the y-axis

in.sps <- select(in.rf,PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b3,b4,b5,b6,domsps)
rf.base1 <-rfsrc(domsps~.,data=in.sps,ntree=500,importance="permute",mtry=6,na.action="na.impute") 
rf.base2 <-rfsrc(domsps~.,data=in.sps[,-1],ntree=500,importance="permute",mtry=6,na.action="na.impute") 


# 2nd try
# separating the sample into training and prediction sets 
# this could be used for building a ROC curve to evaluate the model
# using the last rows of the data frame for a training set

rf.test <- rfsrc(domsps~.,data=in.sps[500:1075,],ntree=500,importance="permute",mtry=6,na.action="na.impute")
# predicting the other values
test.pred <- predict(rf.test,in.sps[1:499,])
# the confusion table is presently not working...leaving it for now
#table(test.pred$predicted,in.sps[1:199,13]) # confusion matrix for the test set
# out of 199 predictions, 79 are right...~40% right

# 3rd try
# build on just the species that have >0.6
# not all levels (i.e., species) are in that split of the data frame, so I have to rebuild the
# y-var to have only the levels that exist in that list

sps1 <-as.character(in.rf[which(in.rf$domperc>0.6),3])
sps1 <- as.factor(sps1)
in0.6 <- as.data.frame(cbind(in.rf[which(in.rf$domperc>0.6),c(-2,-3)],sps1))
rf.pure <- rfsrc(sps1~.,data=in0.6,ntree=500,importance="permute",mtry=6,na.action="na.impute") 
rf.pure1 <- rfsrc(sps1~.,data=in0.6[,-1],ntree=500,importance="permute",mtry=6,na.action="na.impute") 
# 4th try
# with ndvi
ndvi <- mutate(in.rf,ndvi = ((b4-b3)/(b4+b3)) ) %>%
  select(-b3,-b4,-domperc)
rf.ndvi <- rfsrc(domsps~.,data=ndvi,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

rf.ndvi.noID <- rfsrc(domsps~.,data=ndvi[,-1],ntree=2000,importance="permute",mtry=6,na.action="na.impute") 
## THIS IS THE MODEL. Need to adjust sps to numbers and see if I can import the model for spreading
# try putting all the vars in the same order as the xvars rasters:
#b1,     b2,     b5,     b6,   CDED,   ndvi, SLOPE_DEG,   TSRI,    TWI, YEAR2012 

## Need to change the sps from characters to numbers so that the resulting rasters are numbers
#change domsps (now 'sps') to numeric
#BF=1 BP=2 BS = 3 JP = 4 TA = 5 TL = 6 WB = 7 WS = 8
sps <- as.factor(as.numeric(ndvi$domsps))
ndvi1 <- cbind(ndvi[,-3],sps)

# re-order (and rename...may need to change that b/c YEAR2012 would be confusing...)
# to match the xvars rasters
ndvi2 <- select(ndvi1,b1,b2,b5,b6,CDED,ndvi,SLOPE_DEG,TSRI,TWI,YEAR2012=YEAR,sps)
rf.spsSRC <- rfsrc(sps~.,data=ndvi2,ntree=2000,importance="permute",mtry=6,na.action="na.impute") 

# can try imputing the data 1st?
imputeSRC <- impute.rfsrc(sps~.,data=ndvi2,ntree=2000,importance="permute",mtry=6,na.action="na.impute")
rf.spsSRC1 <- rfsrc(sps~.,data=imputeSRC,ntree=2000,importance="permute",mtry=6) 

# try this to predict ### DON'T IT USES ALL THE PHYSICAL MEMORY ###
#results <- randomForestSRC::predict(rf.spsSRC,xvars,importance="permute",na.action="na.impute")
#CAREFUL THE RANDOM FOREST PACKAGE MESSES WITH DPLYR
require(randomForest)
######### THIS IS THE MODEL USED RIGHT NOW ####################################
na.imputed <- rfImpute(sps ~ ., ndvi2)
rf.spsRF <- randomForest(sps~., na.imputed,ntree=2000)
###############################################################################

#This is the extent of the effort to predict dominant species-------------------------------------------
# this model performs well, so save it


# to save the selected RF model - than Byron can load it using load(irisrf.RData)
#save(rf.ndvi,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/DominantSPeciesRF/rfdomspecies.RData")
save(rf.spsSRC,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/DominantSPeciesRF/rfdomspeciesSRC.RData")
save(rf.spsSRC1,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/DominantSPeciesRF/rfdomspeciesSRC1.RData")

######### THIS IS THE MODEL USED RIGHT NOW ####################################
save(rf.spsRF,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/DominantSPeciesRF/rfdomspeciesRF.RData")
###############################################################################


# Trying to predict each species proportion ------------------------------------------------------------

# Read-in data 
# This data frame was created to try to predict sps proportions by species, it has individual 
# columns for all sps
spsprop <- read.table("G:/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/PSP418_SpeciesProportions.txt",
                      sep=",",header=TRUE)
prop.rs1 <- left_join(spsprop,rs1)
prop.rs <- left_join(prop.rs1,rs5)


# For each sps, select only the rows that are non-zeros
# BF---------------------------------------------------------
bf.in <- filter(prop.rs,BF>0) %>%
  select(PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b3,b4,b5,b6,BF)
# with ndvi
bf.ndvi <- mutate(bf.in,ndvi = ((b4-b3)/(b4+b3)) ) %>%
  select(-b3,-b4)

# Random Forest Model
rf.bf <- rfsrc(BF~.,data=bf.ndvi,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

# BP-----------------------------------------------------------
bp.in <- filter(prop.rs,BP>0) %>%
  select(PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b3,b4,b5,b6,BP)
# with ndvi
bp.ndvi <- mutate(bp.in,ndvi = ((b4-b3)/(b4+b3)) ) %>%
  select(-b3,-b4)

# Random Forest Model
rf.bp <- rfsrc(BP~.,data=bp.ndvi,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

# BS-----------------------------------------------------------
bs.in <- filter(prop.rs,BS>0) %>%
  select(PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b3,b4,b5,b6,BS)
# with ndvi
bs.ndvi <- mutate(bs.in,ndvi = ((b4-b3)/(b4+b3)) ) %>%
  select(-b3,-b4)

# Random Forest Model
rf.bs <- rfsrc(BS~.,data=bs.ndvi,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

# JP-----------------------------------------------------------
jp.in <- filter(prop.rs,JP>0) %>%
  select(PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b3,b4,b5,b6,JP)
# with ndvi
jp.ndvi <- mutate(jp.in,ndvi = ((b4-b3)/(b4+b3)) ) %>%
  select(-b3,-b4)

# Random Forest Model
rf.jp <- rfsrc(JP~.,data=jp.ndvi,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

# TA-----------------------------------------------------------
ta.in <- filter(prop.rs,TA>0) %>%
  select(PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b3,b4,b5,b6,TA)
# with ndvi
ta.ndvi <- mutate(ta.in,ndvi = ((b4-b3)/(b4+b3)) ) %>%
  select(-b3,-b4)

# Random Forest Model
rf.ta <- rfsrc(TA~.,data=ta.ndvi,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

# TL-----------------------------------------------------------
tl.in <- filter(prop.rs,TL>0) %>%
  select(PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b3,b4,b5,b6,TL)
# with ndvi
tl.ndvi <- mutate(tl.in,ndvi = ((b4-b3)/(b4+b3)) ) %>%
  select(-b3,-b4)

# Random Forest Model
rf.tl <- rfsrc(TL~.,data=tl.ndvi,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

# WB-----------------------------------------------------------
wb.in <- filter(prop.rs,WB>0) %>%
  select(PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b3,b4,b5,b6,WB)
# with ndvi
wb.ndvi <- mutate(wb.in,ndvi = ((b4-b3)/(b4+b3)) ) %>%
  select(-b3,-b4)

# Random Forest Model
rf.wb <- rfsrc(WB~.,data=wb.ndvi,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

# WS-----------------------------------------------------------
ws.in <- filter(prop.rs,WS>0) %>%
  select(PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI,b1,b2,b3,b4,b5,b6,WS)
# with ndvi
ws.ndvi <- mutate(ws.in,ndvi = ((b4-b3)/(b4+b3)) ) %>%
  select(-b3,-b4)

# Random Forest Model
rf.ws <- rfsrc(WS~.,data=ws.ndvi,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

# to save the selected RF model fpr each species proportion
save(rf.bf,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/SpeciesProp/rfBF.RData")
save(rf.bp,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/SpeciesProp/rfBP.RData")
save(rf.bs,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/SpeciesProp/rfBS.RData")
save(rf.jp,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/SpeciesProp/rfJP.RData")
save(rf.ta,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/SpeciesProp/rfTA.RData")
save(rf.tl,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/SpeciesProp/rfTL.RData")
save(rf.wb,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/SpeciesProp/rfWB.RData")
save(rf.ws,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/rf.species/SpeciesProp/rfWS.RData")






