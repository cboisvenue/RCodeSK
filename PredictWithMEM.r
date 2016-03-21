#--------------------------------------------------------
# Figuring out how to get projections from mixed effects models
#
# Playing with the MEM built on the PSP delta biomass for PAFMA
# and the MEM built on the pixel-based estimates of PAFMA
#
# January 29, 2016
# CBoisvenue
#---------------------------------------------------------

library(data.table)
library(lme4)
library(ggplot2)
indir <- "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/"

# these are the models---------------------------------------------------------------------
fit.data1 <- fread(paste(indir,"FittingData_BiomassPSPModel.txt",sep=""),sep=",",header=TRUE)
modl1 <- lmer(formula= ly~stratum+l.age*stratum+age1+(1|PLOT_ID),data=fit.data1,REML=FALSE)


dt.all <-fread("M:/Spatially_explicit/01_Projects/07_SK_30m/exchangeFolder/DataTables/DT_all.txt",sep=",",header=TRUE)
# names in the data.table-building process were not changed to actually represent 
# what each column contains. These names are more appropriate
setnames(dt.all,names(dt.all),c("age","logAge","l.dbiom","strata","RasterID"))
# the strata that correspond to the PSP "JP" strata are 41-42-43 
# see "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/StrataCorrespondencePSP_PA.csv"
#dt.JP <- dt.all[strata %in% 40:44]
#rm(dt.JP)

load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Cboisvenue/CleanedUpForUsing/MEM_RS_OneSample.RData")
#l.dbiom ~ strataPSP + logAge * strataPSP + age + (1 | RasterID)
#-----------------------------------------------------------------------------------------

library(piecewiseSEM)
sem.model.fits(memPA.s1)
# Error: cannot allocate vector of size 584.1 Gb
# In addition: Warning messages:
#   1: In diag(Z %*% Sigma %*% t(Z)) :
#   Reached total allocation of 65459Mb: see help(memory.size)
# 2: In diag(Z %*% Sigma %*% t(Z)) :
#   Reached total allocation of 65459Mb: see help(memory.size)
# 3: In diag(Z %*% Sigma %*% t(Z)) :
#   Reached total allocation of 65459Mb: see help(memory.size)
# 4: In diag(Z %*% Sigma %*% t(Z)) :
#   Reached total allocation of 65459Mb: see help(memory.size)


#------------------------------------------------------------------------------------------
## PSP model-------------------------------------------------------------------------------
# starting with the psp mem (modl1) I can use the predict() to get estimates------------------------

AIC(modl1)#[1] 1778.642
# look at the curves
plot.age <- rep(1:250) #this is my x-axis
l.age <-log(plot.age) # in the right transmormation
topredMM <- as.data.frame(cbind(plot.age,l.age)) # put them together
#names(topredMM) = c("age1","l.age") # name them the same this as in the fitting data
stratum <- sort(rep(c("BF","BP","BSG","BSM","JP","TAG","TAM","WB","WSG"),250)) # same number of 
# strata as in the fit.data

# need the PLOT_ID b/c that is where the ranef are
# get them from this input data
plots <- sample(fit.data1$PLOT_ID,9)
PLOT_ID <- sort(rep(plots,250))

# create a df for prediction
topredMM <- cbind(PLOT_ID,stratum,topredMM)
names(topredMM) = c("PLOT_ID","stratum","age1","l.age") # name them the same this as in the fitting data
NoWSMlhat2 <- predict(modl1,newdata=topredMM)
NoWSMhat2 <- exp(NoWSMlhat2)
NoWSMpred2 <- cbind(topredMM,NoWSMhat2,NoWSMlhat2)
fig7 <- ggplot(data=NoWSMpred2,aes(x=age1,y=NoWSMhat2,group=stratum,colour=stratum,linetype=stratum)) + 
  geom_line(size=1) + xlab("Age") + ylab("MgC/ha") + theme(legend.position=c(0.9,0.62))
fig7
fig7.JP <- ggplot(data=NoWSMpred2[stratum=="JP",],aes(x=age1,y=NoWSMhat2,group=stratum,colour=stratum,linetype=stratum)) + 
  geom_line(size=1) + xlab("Age") + ylab("MgC/ha") + theme(legend.position=c(0.9,0.62))
# End of predict() THIS WORKS with PSP model --------------------------------

# can I do this with the parameter values only? ------------------------------------------------

# this is the parameter file. Coudl extract this with summary(modl1)$coefficients
params1 <- fread(paste(indir,"BiomModelParamsCI.txt",sep=""),sep=",",head=TRUE)
# one strata: JP
JP.fixed <- params1[stratum=="JP"|stratum=="ALL"][,.(b,value)]

# 2nd try PLOT_ID 20004
# row.names(ranef(modl1)$ PLOT_ID) gives the Plot_ID as characters
# ranef(modl1)$ PLOT_ID$ `(Intercept)` gives the random effects for each plot number (only on intercept)
# PLOT_ID 301686
age <- 0:250
l.JP <- JP.fixed[b=="b0",value]+0.0078177205 + JP.fixed[b=="b1",value]*log(age) + JP.fixed[b=="b2",value]*age
JPhat <- exp(l.JP)
plot(age,JPhat)

# the dist here does not look right...but I think I now have the model syntax right

# FAILED: did not work with extracted param values -----------------------------------------------------

######------------------------------------------------------------------------------------------------------
## Pixel based exploration ----------------------------------------------------------------------------
# predict() with pixel-based model----------------
strataPSP <- sort(rep(c("BSG","BSM","JP","TAG","TAM","WB","WSG"),250))
# check that the strataPSP matched the ones above
#memPA.s1@ frame$ strataPSP
# BSG BSM JP TAG TAM WB WSG
# yes they do

# new data to predict---------------
logAge <- l.age 
age <- plot.age
RasterID <- rep(sample(rownames(ranef(memPA.s1)$ RasterID),10),25)
# sRasterID <- sample(ranef(memPA.s1)$RasterID$`(Intercept)`,10)
# name.ind <- which[ranef(memPA.s1)$RasterID$`(Intercept)` %in% sRasterID]
# ranef(modl1)$PLOT_ID$`(Intercept)`[which(ranef(modl1)$PLOT_ID$`(Intercept)` %in% sPLOT_ID)]

# attempt predict ------------------------
pixel.in <- as.data.frame(cbind(strataPSP,logAge,age,RasterID))
pixel.lhat <- predict(memPA.s1,newdata=pixel.in)
# the predict() does not work...
# Error in X %*% fixef(object) : non-conformable arguments
#tried a suggested fix 
library(nlme)
source("http://lab.thegrandlocus.com/static/code/predict.lme_patched.txt")
pixel.lhat <- predict.lme(memPA.s1,pixel.in)
# different error
# Error in object$dims : $ operator not defined for this S4 class
# this is not working...
# predict() failed------------------------------

# try with the parameters... ---------------------------------------------------

paramsPA <- fread(paste(indir,"DeltaBiomRsParams.txt",sep=""),sep=",",header=TRUE)
# RasterID 719952  -0.15420083
l.BP <- paramsPA[stratum=="BP" & b=="b0",mean(value)]+-0.15420083 + paramsPA[stratum=="BP" & b=="b1",mean(value)]*log(age) + paramsPA[b=="b2",mean(value)]*age
BPhat <- exp(l.BP)
plot(age,BPhat)
l.JP <- paramsPA[stratum=="JP" & b=="b0",mean(value)]+-0.15420083 + paramsPA[stratum=="JP" & b=="b1",mean(value)]*log(age) + paramsPA[b=="b2",mean(value)]*age
JPhat <- exp(l.JP)
plot(age,JPhat)

# BP mean() param value seems to give something reasonable...look more closely:
# BSG, BSM looks bad

params.BP <- paramsPA[stratum=="BP"]
BP.b <- cbind(params.BP[b=="b0",.(b0=value)],params.BP[b=="b1",.(b1=value)])
# RasterID 719952  -0.15420083

l.BPmean <- BP.b[,mean(b0)]+-0.15420083 + BP.b[,mean(b1)]*log(age) + paramsPA[b=="b2",mean(value)]*age
BPhatmean <- exp(l.BPmean)
plot(age,BPhatmean)
# this works...
hist(BP.b$ b0)
# play around with values of b0
l.BPb0s1 <- 10+-0.15420083 + BP.b[,mean(b1)]*log(age) + paramsPA[b=="b2",mean(value)]*age
BPhatmeanb0s1 <- exp(l.BPb0s1)
plot(age,BPhatmeanb0s1)
# played from -30 to +10, still looks like a growth curve

hist(BP.b$ b1)
l.BPb1 <- BP.b[,mean(b0)]+-0.15420083 + 2*log(age) + paramsPA[b=="b2",mean(value)]*age
BPhatmeanb1 <- exp(l.BPb1)
plot(age,BPhatmeanb1)


hist(paramsPA[b=="b2"]$value)
paramsPA[b=="b2", mean(value)]
l.BPb2 <- BP.b[,mean(b0)]+-0.15420083 + BP.b[,mean(b1)]*log(age) + -0.0070*age
BPhatmeanb2 <- exp(l.BPb2)
plot(age,BPhatmeanb2)
# from -0.0080 to -0.0070 still looks like a growth curve


modeCalc <- function(data) {
  # Function for mode estimation of a continuous variable
  # Kernel density estimation by Ted Harding & Douglas Bates (found on RSiteSearch)	
  x<-data
  lim.inf=min(x)-1; lim.sup=max(x)+1
  
  hist(x,freq=FALSE,breaks=seq(lim.inf,lim.sup,0.2))
  s<-density(x,from=lim.inf,to=lim.sup,bw=0.2)
  n<-length(s$y)
  v1<-s$y[1:(n-2)];
  v2<-s$y[2:(n-1)];
  v3<-s$y[3:n]
  ix<-1+which((v1<v2)&(v2>v3))
  
  lines(s$x,s$y,col="red")
  points(s$x[ix],s$y[ix],col="blue")

  md <- s$x[which(s$y==max(s$y))] 
  
  return(md)
  
}

paramsPA[b=="b2", modeCalc(value)]
BP.b[,modeCalc(b0)]
l.BPmode <- BP.b[,modeCalc(b0)]+-0.15420083 + BP.b[,modeCalc(b1)]*log(age) + paramsPA[b=="b2",modeCalc(value)]*age
BPhatmode <- exp(l.BPmode)
plot(age,BPhatmode)
### HERE COULD USE MEAN AND MODE AND VARIATIONS AROUND IT.....

# # JP for pixel.in
# JP.in <- pixel.in[strataPSP=="JP",]
# b0 <- paramsPA[b=="b0"]
# JPb0 <- b0[stratum=="JP"]
# JPb1 <- paramsPA[b=="b1"][stratum=="JP"]
# b2 <- paramsPA[b=="b2"]

# # get random effects on intercept for JP only
# #HERE: have to match the RasterID in the JP.in with those in this
# str(ranef(memPA.s1))
# # but pixels are the names...
# which(rownames(ranef(memPA.s1)$ RasterID) %in% JP.in$RasterID)

# Using fitted() -----------------------------------------
# make a data frame
strata <- as.character(memPA.s1@ frame$strataPSP)
age <- as.numeric(memPA.s1@ frame$age)
logDBiomass <- fitted(memPA.s1)
DBiomass <- exp(logDBiomass)
plot(age,DBiomass)

fit.pixel <- as.data.table(cbind(age,strata,DBiomass))
fit.p2 <- fit.p1[as.numeric(age)>50 & DBiomass<1.75]
fit.p1 <- fit.pixel[strata!="TAM" & strata!="WSG"]
plot(fit.p1$age,fit.p1$DBiomass)

# ggplot a version of the above?
g.fit.p1 <- ggplot(fit.p1,aes(x=as.numeric(age),y=DBiomass)) + geom_point()
# takes too long
plot(fit.p1$age,fit.p1$DBiomass,type="l")