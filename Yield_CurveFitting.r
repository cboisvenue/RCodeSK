# ---------------------------------------------------------------------------
# Growth curves for the purposes of creating growth tables for CBMCFS3 runs 
# SK. by strata for the "basic" spatial CBM-CFS3 runs in SK
# 
# June 26, 2016
# CBoisvenue
#---------------------------------------------------------------------------


#library(nlme)
library(ggplot2)
require(plyr)
require(dplyr)

# Read-in data
setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/")

#yield.data <- read.table("Plot_Inc_m3_ha_yr.txt",sep=",",header=TRUE)
total.vol <-  read.table("totalVol_haPlot.txt",sep=",",header=TRUE)
hist(total.vol$totalm3_ha)


#install.packages("lme4")
library(lme4)


# linearize this model-----------------------------------------------------------
dataT1 <- select(total.vol,PLOT_ID,plot.age,totalm3_ha,stratum=stratum.1) %>%
  mutate(ltotvol = log(totalm3_ha),l.age=log(plot.age))

# Trying total vol -----------------------------------------------------
glmT1 <- glm(ltotvol~stratum+l.age+plot.age,data=dataT1)
glmT2 <- glm(ltotvol~stratum+l.age*stratum+plot.age*stratum,data=dataT1)
anova(glmT1,glmT2,test="F")
AIC(glmT1)#6333.412
AIC(glmT2)#6283.533

## only one slope varies with strata
glmT3 <- glm(ltotvol~stratum+l.age*stratum+plot.age,data=dataT1)
AIC(glmT3)#[1] 6326.133

# try mem---------------------------------------
library(lme4)
memT <- lmer(formula= ltotvol~stratum+l.age*stratum+plot.age*stratum+(1|PLOT_ID),data=dataT1,REML=FALSE)
AIC(memT)#[1] 3589.153
anova(glmT2,memT)
memT1 <- lmer(formula= ltotvol~stratum+l.age*stratum+plot.age+(1|PLOT_ID),data=dataT1,REML=FALSE)
AIC(memT1)#3765.832
##################################################################################
#BEST MODEL TOTAL VOL: varying intercepts, and slopes for each stratum (fixed)
# and random effects on plot_ID
summary(memT)
memT.lyhat <- fitted(memT)
plot(dataT1$ltotvol,memT.lyhat)
obs.pred1 <- lm(memT.lyhat~dataT1$ltotvol)
# Residual standard error: 0.1862 on 3708 degrees of freedom
# Multiple R-squared:  0.9434,	Adjusted R-squared:  0.9434 
#
# PROBLEM WITH TAG??
plot(dataT1$ltotvol[which(dataT1$stratum=="TAG")],memT.lyhat[which(dataT1$stratum=="TAG")])
# Get rid of TAG and refit-----------------------
dataT3 <-filter(dataT1, stratum!="TAG")
dataT3$stratum <- as.character(dataT3$stratum)


# try again
memTnotag <- lmer(formula= ltotvol~stratum+l.age*stratum+plot.age*stratum+(1|PLOT_ID),data=dataT3,REML=FALSE)
AIC(memTnotag)#[1] 2736.334
## BEST MODEL FOR TOTAL VOLUME DOES NOT HAVE TAG DATA##############################


#Pretty curves? predict from this for each stratum?------------------------------------
plot.age <- rep(1:250)
l.age <-log(plot.age)

topredMM <- as.data.frame(cbind(plot.age,l.age))
names(topredMM) = c("plot.age","l.age")
# for total volume with TAG model: memT
stratum <- sort(rep(as.character(levels(dataT1$stratum)),250))
plot10 <- sample(dataT1$PLOT_ID,10)
PLOT_ID <- sort(rep(plot10,250))
topredMM <- cbind(PLOT_ID,stratum,topredMM)

lyhat <- predict(memT,newdata=topredMM)
predMM1 <- cbind(topredMM,lyhat)
predMM  <- mutate(predMM1 ,memTyhat = exp(lyhat)) 
MEM_all <- ggplot(data=predMM,aes(x=plot.age,y=memTyhat,group=stratum,colour=stratum)) + geom_line(size=1.5)
MEM_all + ggtitle("Predicting total m3/ha by Strata") +  scale_fill_brewer(palette="Spectral")
# CAN SEE THE TAG issue here - have tried all kinds of other 
# solutions (see YieldCurveFittingScratch.r) and chose to remove the TAG lines and refit

# for total volume no TAG model: memTnotag
topredMM <- as.data.frame(cbind(plot.age,l.age))
names(topredMM) = c("plot.age","l.age")
stratum <- stratum[which(stratum!="TAG")]
plot10 <- sample(dataT3$PLOT_ID,9)
PLOT_ID <- sort(rep(plot10,250))
topredMM <- cbind(PLOT_ID,stratum,topredMM)

noTAGlyhat <- predict(memTnotag,newdata=topredMM)
noTAGMM <- cbind(topredMM,noTAGlyhat)
noTAGMM  <- mutate(noTAGMM ,memT.notagyhat = exp(noTAGlyhat)) 


MEM_noTAG <- ggplot(data=noTAGMM,aes(x=plot.age,y=memT.notagyhat,group=stratum,colour=stratum)) + geom_line(size=1.5)
MEM_noTAG + ggtitle("Predicting total m3/ha by Strata") +  scale_fill_brewer(palette="Spectral")
ggsave("plotGrowthCurvesMEModelPSP2_noTAG.jpeg")

write.table(noTAGMM,file="MEMPredictedGrowth_noTAG.txt",sep=",",row.names=FALSE)
save(memTnotag,file = "GrowthCurvesMEModel_noTAG.RData")
# end of pretty graphs--------------------------------------


# build growth tables for CBM----------------

# add TAG
TAG <- noTAGMM[noTAGMM$stratum=="TAM",]
TAG$stratum <- "TAG"
TvolCBM <- rbind(noTAGMM,TAG)

library(reshape2)
growth.table.out <- dcast(TvolCBM,stratum~plot.age,value.var="memT.notagyhat")

strata.def <-read.table("YieldCurveStrata.csv",sep=",",header=TRUE)
strata.def <- select(strata.def,dom,prodClass, stratum)

growth.table.out <-left_join(strata.def,growth.table.out)

write.table(growth.table.out,file="GrowthTables_PSP_forCBM.txt",row.names=FALSE,sep=",")


