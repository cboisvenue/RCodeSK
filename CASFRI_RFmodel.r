# 
# Building a random forest model using the CASFRI sps and age info to see how it 
# compares to the selected model 
#
# April 9th, 2015
# CBoisvenue
#----------------------------------------------------------------------------------

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

casfri <- read.table(paste(indir,"casfri.txt",sep=""), header=TRUE,sep=",")
rsdata <- read.table(paste(indir,"RS_12vars.txt",sep=""), header=TRUE,sep=",")
pspbiom <- read.table(paste(indir,"SK_2000Biomass_ha.txt",sep=""), header=TRUE,sep=",")
treeinfo <- read.table(paste(indir,"SK_2000TreeMeasurements.csv",sep=""), header=TRUE,sep=",")
plotinfo <- group_by(treeinfo,PLOT_ID) %>%
  summarize(casfri_dom = first(dom),age1=mean(age),ht1=mean(HEIGHT))
plotinfo <- plotinfo[,c(1,3,4,2)]

# rstimeinv <- select(rsdata,PLOT_ID,CDED,SLOPE_DEG,TSRI,TWI)
# rstimeinv <- unique(rstimeinv)
# rstimevar <- rsdata[,1:8]
pspkeep1 <- left_join(pspbiom,rstimevar)
pspkeep2 <- left_join(pspkeep1,rstimeinv)
#Trying with no NAs
pspkeep3 <- inner_join(pspbiom,rsdata)
keep1 <- inner_join(pspkeep3,plotinfo)

 pspdata <- read.table(paste(indir,"RF_dataframePSPInfo.txt",sep=""), header=TRUE,sep=",")
# pspreplace <- select(pspdata,PLOT_ID,domsps,BF,BP,BS,JP,TA,TL,WB,WS,realage)
# pspkeep <- select(pspdata,PLOT_ID,YEAR,biom.ha,b1,b2,b3,b4,b5,b6,CDED,SLOPE_DEG,TSRI,TWI)

# calculate ages and heights for CASFRI -----------------------------------------------------------
cas1 <- mutate(casfri,cas_age = photo_year-dist_yr)
# dealing with the the differences in years...both for ages and for ht
# NOTE: assuming a linear relationship with height to simplify things for now
psp1 <- select(pspkeep3,PLOT_ID,YEAR)
cas2 <- select(cas1,PLOT_ID=plot_id,photo_year,ht,casfri_dom,cas_age) 
# %>%
#   mutate(htrate = as.numeric(as.character(revalue(cas2$casfri_dom,c("Abie bals"="1","Betu papy"="2","Lari lari"="1.5","Pice glau"="1",
#                                                                     "Pice mari"="1","Pinu bank"="1.8","Popu balb"="2.4","Popu trem"="2")))))

ageht1 <- left_join(psp1,cas2) %>%
  mutate(age1 = cas_age-(photo_year-YEAR),ht1 = (photo_year-YEAR)*0.25+ht) %>%
  select(PLOT_ID,YEAR,age1,ht1)
data1 <- inner_join(pspkeep3,ageht1)
# END of ht and age------------------------------------------------------------------------------

# make a dataframe for the RF model ------------------------------------------------------------
## Here I am ignoring the year of the species composition under the assumption that
# species don't change that fast unless the stand is disturbed (which they are not here...)
cas3 <- select(casfri,PLOT_ID=plot_id,soil_moist_reg,casfri_dom,Abie.bals,Betu.papy,Lari.lari,Pice.glau,Pice.mari,Pinu.bank,Popu.balb,Popu.trem)
casfri.for.rf <- left_join(data1,cas3)
for.rf <- inner_join(keep1,cas3[,-3])
# other than change the species composition and leading species to what CASFRI 
# has, this adds the ht and the soil_moist_reg
# END dataframe RF model -----------------------------------------------------------------------

# build the RF model ---------------------------------------------------------------------------
rf.casfri1 <- rfsrc(biom.ha~.,data=casfri.for.rf[,],ntree=500,importance="permute",mtry=6,na.action="na.impute") 

rf.casfri2 <- rfsrc(biom.ha~.,data=casfri.for.rf[,c(-1,-16,-18,-19,-20,-21,-22,-23,-24,-25)],ntree=500,importance="permute",mtry=6,na.action="na.impute") 

casfri.for.rf.noNA <- filter(casfri.for.rf,!is.na(b1))
rf.casfri3 <- rfsrc(biom.ha~.,data=casfri.for.rf.noNA[,-1],ntree=2000,importance="permute",mtry=6) 

rf.mix <- rfsrc(biom.ha~.,data=for.rf[,-1],ntree=2000,importance="permute",mtry=6,na.action="na.impute") 
#write.table(for.rf[,-1],)

# Evaluating this model below
# Plot the OOB errors against the growth of the randforest.--------------
gg_e <- gg_error(rf.mix)
plot(gg_e)
plot(gg_rfsrc(rf.casfri), alpha=.5)

# Plot the VIMP rankings of independent variables.-------------------
plot(gg_vimp(rf.mix))
plot.gg_vimp(rf.casfri3)

# Load the data, from the call:
varsel_rf.casfri <- var.select(rf.casfri)
data(varsel_rf.casfri)

# Save the gg_minimal_depth object for later use.
gg_md <- gg_minimal_depth(varsel_rf.casfri)

# plot the object---------------------------------
plot(gg_md)

# gg_minimal_depth objects contain information about
# both minimal depth and VIMP.---------------------
plot.gg_minimal_vimp(gg_md)
#INTERPRETATION: Points above the red dashed line are ranked higher by VIMP than by minimal 
#depth, indicating the variables are sensitive to misspecification. Those 
#below the line have a higher minimal depth ranking, indicating they are better 
#at dividing large portions of the population.

# END of model building ------------------------------------------------------------------------------

# Export a version of the chosen model built with randomForest package ------------------------------
require(randomForest)
# changing the factor levels for the dom species from factors to numbers
for.rf1 <- mutate(for.rf,casfri_dom=as.numeric(casfri_dom),soil_moist_reg=as.numeric(soil_moist_reg))

na.imputed <- rfImpute(biom.ha ~ ., for.rf1[,-1])
write.table(na.imputed,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/biomass/RF3Input.csv",row.names=FALSE, sep=",")
#rf.casfriRF <- randomForest(biom.ha ~ ., na.imputed,ntree=2000)
rf.mix1 <- randomForest(biom.ha ~ ., na.imputed,ntree=2000)
save(rf.mix1,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/biomass/RFModel3.RData")
##Units: predictions are in t/ha
############################################## This is the model I want to use ######################

# Try PSP with ht1 and moist_reg from CASFRI ---------------------------------------------------------

cas4 <- select(casfri,PLOT_ID=plot_id,soil_moist_reg)
data2 <- left_join(pspdata,ageht1[,-3]) %>%
  left_join(cas4)
rf.pspCAS <- rfsrc(biom.ha~.,data=data2[,-1],ntree=2000,importance="permute",mtry=6,na.action="na.impute") 

#---------------------------------------------------------------------------------------------------
# Creating a simple model for biomass as a temporary place holder until I can have the CASFRI layers
require(dplyr)
# use
holder.in <- select(casfri.for.rf,YEAR,biom.ha,b1,b2,b3,b4,b5,b6,CDED,SLOPE_DEG,TSRI,TWI,age1)
#require(randomForest)
na.holder <- rfImpute(biom.ha ~ ., holder.in)
rf.holder <- randomForest(biom.ha ~ ., na.holder,ntree=2000)
save(rf.holder,file = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/biomass/rfholderRF.RData")

