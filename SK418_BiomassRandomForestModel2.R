#--------------------------------------------------------------------
# Try predicting biomass with domspecies, and 4 top species proportions, 
# rs data, with and without ndvi
#
## CBoisvenue
# March 24nd, 2015
#-----------------------------------------------------------
require(plyr)
require(ggplot2)         # Graphics engine for generating all types of plots
require(dplyr)           # Better data manipulations
library(gridExtra)  
require(reshape2)        # for transforming wide data into long data (melt)
library("RColorBrewer")    # Nice color palettes
library("randomForestSRC") # random forests for survival, regression and classification
library("ggRandomForests") # ggplot2 random forest figures

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/")
indir = "G:/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/"

# Input data ------------------------------------------------

# get the biomass per ha
#biomass <- read.table("./data/SK418Biomass_ha.txt",header=TRUE,sep=",")
## NEED to replace this input data with the recalculated biomass
sk.raw1 <- read.table("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04/plot_header.csv", header=TRUE, sep=",")
loc.raw = select(sk.raw1, LOC_ACCURACY, PLOT_ID,contains("Z13nad83"))
# check in we have plot numbers repeated
dim(loc.raw) #[1] 2048   13
length(unique(loc.raw[,2])) # 2048 ##NO - GOOD.
loc.acc10 = filter(loc.raw, LOC_ACCURACY <=10) %>% #418
  select(PLOT_ID)
plotbiom <- read.table(paste(indir,"SK_2000Biomass_ha.txt",sep=""),header=TRUE,sep=",")
biomass <- inner_join(loc.acc10,plotbiom)


# bring in species proportions and dom species

domsps <- read.table("./data/CleanedUpForUsing/PSP418_DomSpecies.txt",header=TRUE,sep=",")
spsprop <- read.table("./data/CleanedUpForUsing/PSP418_SpeciesProportions.txt",header=TRUE,sep=",")
spsinfo <- inner_join(domsps[,1:3],spsprop)
plotbiomass <- inner_join(biomass,spsinfo)

# rs variables
rs <- read.table("./data/CleanedUpForUsing/RS_12vars.txt", header=TRUE,sep=",")
rs.time <- select(rs,PLOT_ID,YEAR,b1,b2,b3,b4,b5,b6)
rs.notime <- select(rs,PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI)
rs.notime2 <- unique(rs.notime)
# join the two dataframes and keep all the plot info
rf.data <- left_join(plotbiomass,rs.time) %>%
  left_join(rs.notime2)

#-------------------------------------------------------------
# Random forest models for biomass 

# all data no changes
rf.biom.bands1 <-rfsrc(biom.ha~.,data=rf.data,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

# with domspecies, 5 top species proportions, bands
rf.data1 <- select(rf.data,PLOT_ID,YEAR,biom.ha,domsps,BP,BS,JP,TA,WS,b1,b2,b3,b4,b5,b6,CDED,SLOPE_DEG,TSRI,TWI)

rf.biom.bands2 <-rfsrc(biom.ha~.,data=rf.data1,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

# with ndvi and top domspecies, 5 top species proportions, bands -b3 and b4
ndvi.data <- mutate(rf.data,ndvi = ((b4-b3)/(b4+b3)) ) %>%
  select(-b3,-b4)
rf.biom.ndvi1 <-rfsrc(biom.ha~.,data=ndvi.data,ntree=500,importance="permute",mtry=6,na.action="na.impute") 

# CONCLUSION: ndvi does not seem to improve anything, but does not hinder either ----------------------

# Bring in psp ages
tree <- read.table(paste(indir,"SK_2000TreeMeasurements.csv",sep=""),header=TRUE,sep=",")
psp.ages <- select(tree,PLOT_ID,YEAR,age) %>%
   group_by(PLOT_ID,YEAR) %>%  
  summarise(realage = round(mean(age)))
 
pspages.for.rf <- left_join(rf.data,psp.ages)
 
 
rf.pspages1 <- rfsrc(biom.ha~.,data=pspages.for.rf[,-1],ntree=2000,importance="permute",mtry=6,na.action="na.impute") 
# trying the same model but with randomForest package b/c we cannot seem to be able to predict on a 
# raster with the model resulting from the randomForestSRC package ##########################
#library("randomForest")
## CAREFUL THE RANDOM FOREST PACKAGE MESSES WITH DPLYR
# na.imputed <- rfImpute(biom.ha ~ ., pspages.for.rf)
# rf.biom <- randomForest(biom.ha~., na.imputed[,-2],ntree=2000)
# save(rf.biom,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/biomass/rfbiomass.RData")


# knn ages
ages <- read.table("./data/PSP/PSP_UTM_wKnnAGE.csv", header=TRUE,sep=",")
ages <- ages[,c(2,7)]
names(ages) =c("PLOT_ID","knnage")
knnyr <- rep(2001,dim(ages)[[1]])
df1 <- cbind(ages,knnyr)
df2 <- left_join(pspages.for.rf,df1)

# calculate all the other years
df3 <- mutate(df2,ageknn = YEAR-knnyr+knnage)
# can't have negatives
df3$ageknn[which(df3$ageknn<=0)] = df3$realage[which(df3$ageknn<=0)]
twoages.for.rf <- select(df3,-knnage,-knnyr)

rf.twoages <- rfsrc(biom.ha~.,data=twoages.for.rf[,-1],ntree=500,importance="permute",mtry=6,na.action="na.impute") 

## CONCLUSION: the best model seems to be 66.38% explained variable: rf.pspages1
## NOTE: cannot used plot_id 

# Output dataframe used to build this model
write.table(x = pspages.for.rf,file= "G:/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/RF_dataframePSPInfo.txt",sep=",",row.names = FALSE)

# Evaluating this model below
# Plot the OOB errors against the growth of the forest.---------------------------------------
gg_e <- gg_error(rf.pspages1)
plot(gg_e)
plot(gg_rfsrc(rf.pspages), alpha=.5)

# Plot the VIMP rankings of independent variables.--------------------------------------------
plot(gg_vimp(rf.pspages1))
plot.gg_vimp(rf.pspages1)

# Load the data, from the call:
varsel_rf.pspages <- var.select(rf.pspages1)
data(varsel_rf.pspages)

# Save the gg_minimal_depth object for later use.
gg_md <- gg_minimal_depth(varsel_rf.pspages)

# plot the object-----------------------------------------------------------------------------
plot(gg_md)

# gg_minimal_depth objects contain information about
# both minimal depth and VIMP.----------------------------------------------------------------
plot.gg_minimal_vimp(gg_md)
#INTERPRETATION: Points above the red dashed line are ranked higher by VIMP than by minimal 
#depth, indicating the variables are sensitive to misspecification. Those 
#below the line have a higher minimal depth ranking, indicating they are better 
#at dividing large portions of the population.

