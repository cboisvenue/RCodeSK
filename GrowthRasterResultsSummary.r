# -----------------------------------------------------------------
# Result summaries for the results used in the Growth Raster paper
# G:\RES_Work\Work\JoanneWhite\SK_work\WritingBin\GrowthRaster
#
# January 14, 2016
# CBoisvenue
#------------------------------------------------------------------


library(data.table)
library(ggplot2)

indir <- "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/"
# outfigs <- "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Sask_runs/11_CASFRI/results/figures/"

# the random forest model used is this one
library(randomForest)
load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/biomass_spread/RFModel3.Rdata")
# it is named rf.mix1
rf.mix1

# PSP biomass per hectare values-----------------------------------------
## NOTES: the t_haBiom_yr.txt file is the one used for model fitting. Individual tree
## increments were calculated and sumed over the plot
biom.ha.psp <- fread(paste(indir,"SK_2000Biomass_ha.txt",sep=""),sep=",", header=TRUE)
range(biom.ha.psp$biom.ha)
# [1]   2.77457 464.03525
# the above is not what I give in the manuscripts...

#there are gaps in Figure 4 (biom.ha evolution through time from the psps)
# checking these gaps

# Repeating Fig.4 with the SK_2000Biomass_ha.txt, instead of the calculated ones I originally used
pspAvgBiom.yr <- biom.ha.psp[,.(mean=mean(biom.ha),sd=sd(biom.ha),no.plot=.N),by=YEAR]
fig4 <- ggplot(data=pspAvgBiom.yr,aes(YEAR,mean))
fig4 + geom_point(aes(size=no.plot),colour="blue") + ylab("Mg/ha") + 
  geom_errorbar(aes(ymin=(mean)-1.96*(sd),ymax=(mean)+1.96*(sd)))
## Same "gaps" 1999-2005, and 1974-1978
tree.biom <- read.table(paste(indir,"SK_2000TreeBiomass.csv",sep=""),sep=",",header=TRUE)
tree.biom <- as.data.table(tree.biom)
count.trees <- tree.biom[,.(no.trees = .N),by=c("YEAR","PLOT_ID")]
check.gap1 <- count.trees[(YEAR>1974 & YEAR<1978)|(YEAR>1999 & YEAR<2005)]
#There are not plots measured in those years.
# Original Fig4 is fine #####################
count.plots <- count.trees[,.(no.plots=.N),by=YEAR]
range(count.trees$no.trees)
#[1]   11 1086


# the fitting data for the random forest model is here:
rf.input <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/biomass_spread/RF3Input.csv",sep=",",header = TRUE)
# These values are in the text
# range(rf.input$biom.ha)
# [1]  10.86702 464.03525
# > range(biom.ha.psp$YEAR)
# [1] 1949 2009

# PSP no of measurement per species info----------------------------------
#count the number of measurement per species
# Figure 3
# NOT SURE WHY THIS IS NOT WORKING TODAY...IT WORKED YESTERDAY
psp.tree <- fread(paste(indir,"SK_2000TreeMeasurements.csv",sep=""),sep=",",header=TRUE)
no.meas.psp <- psp.tree[,.(count = .N),by=dom]
# better plot with a table
library(gridExtra)
g <- ggplot(data=psp.tree) + geom_histogram(aes(round(age), fill=SPECIES),colour="black") +
  xlab("Plot age") + ylab("Number tree-level measurements") 
#ggtitle("Measured Trees by Age and Species - SK 418") + 
#theme(plot.title = element_text(lineheight=1.2, face="bold"))
g+annotation_custom(tableGrob(no.meas.psp),xmin=175, xmax=225,ymin=14000,ymax=75000) +
  theme(panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))
 ggsave("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/WritingBin/figures/SK2000_TreeMeasAgeSps.jpeg")
# END PSP info---------------------------------------------------------------


#Redoing figure 5 to ensure we are looking at all the same pixels through time ---------------------

raster.biom <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/growth/biomassHaEvaluation/RasterAvg_SD_constantPixels.txt",sep=",",header=TRUE)
pixel.biom <- ggplot(raster.biom, aes(year,avg.biomMask)) + geom_point(colour="red") +
  geom_line(colour="red")+ ylab("Mg/ha") + 
  geom_errorbar(aes(ymin=avg.biomMask-1.96*sd.bioMask,ymax=avg.biomMask+1.96*sd.bioMask))

setnames(raster.biom,names(raster.biom),c("YEAR","mean","sd"))
# add PSP info from Figure 4
pspAvgAGB84 <- pspAvgBiom.yr[YEAR>1983]
pspAvgAGB84 <- pspAvgAGB84[,no.plot := NULL]
allABG.ha.yr <- rbind(raster.biom,pspAvgAGB84)
Source <- c(rep("pixel",dim(raster.biom)[1]),rep("PSP",dim(pspAvgAGB84)[1]))
allABG.ha.yr <- cbind(allABG.ha.yr,Source)

fig5 <- ggplot(data=allABG.ha.yr,aes(YEAR,mean,group=Source,colour=Source, fill=Source)) + 
  geom_point() + geom_line() + geom_errorbar(aes(ymin=(mean)-1.96*(sd),ymax=(mean)+1.96*(sd)))+
  ylab("Mg/ha") +
  scale_colour_manual(values=c("black", "red"))

## we see the same trends with all the "undisturbed pixels through time
# need to figure out the number of pixels per year...
no.pixels <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/growth/biomassHaEvaluation/cellValue_freq_BIOMASSstack_constantPixels.txt",sep=",",header=TRUE)
# last row is a count of the NAs
nopixels <- no.pixels[1:257]
pix.yr <- colSums(nopixels,na.rm=TRUE)
range(pix.yr[2:30])
#[1] 14342960 14342960

# check age dist for these pixels:
pixel.age <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/growth/biomassHaEvaluation/maskedAge84_binned10.csv",sep=",",header=TRUE)
g.pixelAge <- ggplot(pixel.age,aes(x=AgeBin,y=hectares/1000000)) + 
  geom_bar(stat="identity") +  xlab("10-year Age Classes") + ylab("Mha") 
# this is fig10
# End of Biomass/ha raster summary--------------------------------------------------------

# Checks on the lme model used -----------------------------------------------------------
library(lme4)
fit.data <- fread(paste(indir,"FittingData_BiomassPSPModel.txt",sep=""),sep=",",header=TRUE)
modl <- lmer(formula= ly~stratum+l.age*stratum+age1+(1|PLOT_ID),data=fit.data,REML=FALSE)

# load the model, it is names mem7
#load(file = "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/growth/MEM_t_haPSP/MEM_t_ha.Rdata")
# save fitting data
#fit.data <- fread(paste(indir,"FittingData_BiomassPSPModel.txt",sep=""),sep=",",header=TRUE)
g.indata <- ggplot(data=fit.data,aes(x=age1,y=biom.ha.inc,group=stratum,colour=stratum)) +
  geom_point()

# Install latest version from CRAN
#install.packages("piecewiseSEM")
library(piecewiseSEM)
sem.model.fits(modl)
# Class   Family     Link  Marginal Conditional      AIC
# 1 lmerMod gaussian identity 0.2767173   0.5131608 1778.642
# Marginal represents the proportion of variance explained by fixed effects
# conditional representa the proportion of the variance explained by the fixed and random effects

# Cheking assumptions for our model:
# Figure 6-------------------------------------------------------------------------
# this checks that there are no trends in the residuals
error1 <- as.data.frame(cbind(c(1:1353),residuals(modl)))
names(error1) = c("Index","Error")
plot.er1 <- ggplot(data=error1, aes(Index,Error)) + geom_point(size=2) + 
  geom_hline(aes(yintercept=0),size=1) 
# this checks the assumption of normality, it plots the theoritical quantiles 
# of a normal distribution (theoretical) compared to that of your random effects
# quantile plots compare 2 data sets. In our case our random effects and a normal distribution
error2 <- as.data.frame(ranef(modl)$PLOT_ID)
names(error2) <- "Intercept"
plot.er2 <- ggplot(data=error2,aes(sample=Intercept)) +stat_qq(shape=1) + 
  geom_abline(intercept = mean(error2$Intercept), slope = sd(error2$Intercept), size=1) 
# END Figure 6--------------------------------------------------------------------

# DROPPED OLD Figure 7 --------------------------------------------------------------------
# load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Cboisvenue/CleanedUpForUsing/MEM_RS_OneSample.RData")
# sem.model.fits(memPA.s1)
# # cannot fit get the above evaluation b/c I need memory then my local machine has...
# error1 <- as.data.frame(cbind(c(1:280000),residuals(memPA.s1)))
# names(error1) = c("Index","Error")
# plot.pixeler1 <- ggplot(data=error1, aes(Index,Error)) + geom_point(size=2) +
#   geom_hline(aes(yintercept=0),size=1)
# error2 <- as.data.frame(ranef(memPA.s1)$RasterID)
# names(error2) <- "Intercept"
# plot.pixeler2 <- ggplot(data=error2,aes(sample=Intercept)) +stat_qq(shape=1) + 
#   geom_abline(intercept = mean(error2$Intercept), slope = sd(error2$Intercept), size=1)  

# New Figure 7: biomass change from field plots fitted curves------------------------
AIC(modl)#[1] 1778.642
# look at the curves
plot.age <- rep(1:250) #this is my x-axis
l.age <-log(plot.age) # in the right transmormation
topredMM <- as.data.frame(cbind(plot.age,l.age)) # put them together
#names(topredMM) = c("age1","l.age") # name them the same this as in the fitting data
# stratum <- sort(rep(c("BF - Balsam Fir","BP - Balsam Poplar","BSG - Black Spruce Good","BSM - Black Spruce Medium",
#                       "JP - Jack Pine","TAG - Trembling Aspen","TAM - Trembling Aspen","WB - White Birch","WS - White Spruce"),250)) # same number of 
stratum <- sort(rep(c("BF","BP","BSG","BSM",
                      "JP","TAG","TAM","WB","WSG"),250)) # same number of 

# strata as in the fit.data
plots <- sample(fit.data$PLOT_ID,9)
PLOT_ID <- sort(rep(plots,250))
topredMM <- cbind(PLOT_ID,stratum,topredMM)
names(topredMM) = c("PLOT_ID","stratum","age1","l.age") # name them the same this as in the fitting data
NoWSMlhat2 <- predict(modl,newdata=topredMM)
NoWSMhat2 <- exp(NoWSMlhat2)
NoWSMpred2 <- cbind(topredMM,NoWSMhat2,NoWSMlhat2)
library(plyr)
NoWSMpred2$stratum <- mapvalues(NoWSMpred2$stratum,unique(NoWSMpred2$stratum),c("BF - Balsam Fir",
                      "BP - Balsam Poplar","BSG - Black Spruce Good","BSM - Black Spruce Medium",
                      "JP - Jack Pine","TAG - Trembling Aspen Good","TAM - Trembling Aspen Medium",
                      "WB - White Birch","WS - White Spruce"))
fig7 <- ggplot(data=NoWSMpred2,aes(x=age1,y=NoWSMhat2,group=stratum,colour=stratum,linetype=stratum)) + 
  geom_line(size=1) + xlab("Age") + ylab("MgC ha-1 ")+ theme_bw() + theme(legend.position=c(0.8,0.62))
fig7 
ggsave(file="C:/Celine/GitHub/RCodeSK/figures/fig7_v1.jpeg")
NoWSM <- as.data.table(NoWSMpred2)
avgDelta <- NoWSM[,.(avgDelta = mean(NoWSMhat2)),by=stratum]

#---------END Fig7-----------------------------------------------------------------

# Building a bunch of curves from the parameter value range for BP in the pixel-based fit
# this is the model
load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Cboisvenue/CleanedUpForUsing/MEM_RS_OneSample.RData")
#l.dbiom ~ strataPSP + logAge * strataPSP + age + (1 | RasterID)

# this is the predicted values from the psp model for JP
g.BPpsp <- ggplot(data=NoWSMpred2[stratum=="BP",],aes(x=age1,y=NoWSMhat2)) + 
  geom_line(size=1) + xlab("Age") + ylab("MgC/ha") 
g.BPpsp
# # can I repeat that with the parameters?
# psp <- fread(paste(indir,"BiomModelParamsCI.txt",sep=""),sep=",",header=TRUE) # psp params
# pspJP <- psp[stratum=="JP"|stratum=="ALL"][,.(b,value)]
# lJP.psp <- pspJP[b=="b0",value]+pspJP[b=="b1",value]*plot.age+pspJP[b=="b2",value]*l.age
# JP.inc <- exp(lJP.psp)
# JP.psp <- as.data.frame(cbind(plot.age,l.age,lJP.psp,JP.inc))
# g.JP.params <- ggplot(data=JP.psp, aes(x=`plot.age`,y=JP.inc)) + geom_line()
# ## Apparently not...

# # can I read-in the DT on this computer?
# library(data.table)
# dt.all <-fread("M:/Spatially_explicit/01_Projects/07_SK_30m/exchangeFolder/DataTables/DT_all.txt",sep=",",header=TRUE)
# # names in the data.table-building process were not changed to actually represent 
# # what each column contains. These names are more appropriate
# setnames(dt.all,names(dt.all),c("age","logAge","l.dbiom","strata","RasterID"))
# # the strata that correspond to the PSP "JP" strata are 41-42-43 
# # see "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/StrataCorrespondencePSP_PA.csv"
# dt.JP <- dt.all[strata %in% 40:44]
# rm(dt.all)
# strataPSP <- rep("JP",250)
# sRasterID<- sample(dt.JP$RasterID,10)
# 
# # this is a list with $RasterID as a char and ranef.PA$RasterID$`(Intercept)` as a number
# ranef.PA <- ranef(memPA.s1)
# # I can extract the row.names which are the RasterID values
# sRasterIndex <- which(as.numeric(head(row.names(ranef.PA$RasterID))) %in% sRasterID)
# which(sRasterID == 47298130)
# # these are the names of the variables in the memPA.s1
# #c("age","logAge","l.dbiom","strata","RasterID")
# stratum <- rep("JP",250)
# JPpredPSP <- 
# RScurveJP.in <- as.data.frame(cbind(plot.age,l.age,strata))
# names(RScurveJP.in) <- c("age","logAge","strataPSP")
# lhat.JPRS <- predict(memPA.s1,newdata=RScurveJP.in)
# 
# 
# #setkey(psp,stratum,b)
# rs <-  fread(paste(indir,"DeltaBiomRsParams.txt",sep=""),sep=",",header=TRUE)
# #setkey(rs,stratum,b)
# b0.modes <- read.table(file=paste(indir,"b0RSmodes.txt",sep=""),sep=",",header=TRUE)
# b1.modes <- read.table(file=paste(indir,"b1RSmodes.txt",sep=""),sep=",",header=TRUE)
# b2.modes <- read.table(file=paste(indir,"b2RSmodes.txt",sep=""),sep=",",header=TRUE)
# #stratum <- "ALL"
# b2.mode <- as.data.frame(cbind(stratum,b2.modes))
paramsPA <- fread(paste(indir,"DeltaBiomRsParams.txt",sep=""),sep=",",header=TRUE)

# Make a data.table with mean, mode, and variations and plot
# RasterID 719952  -0.15420083
age <- 1:250
l.age <- log(age)
params.BP <- paramsPA[stratum=="BP"]
BP.b <- cbind(params.BP[b=="b0",.(b0=value)],params.BP[b=="b1",.(b1=value)])
# RasterID 719952  -0.15420083
l.BPmean <- BP.b[,mean(b0)]+-0.15420083 + BP.b[,mean(b1)]*log(age) + paramsPA[b=="b2",mean(value)]*age
BPhatmean <- exp(l.BPmean)
plot(age,BPhatmean)
# this works...
#hist(BP.b$ b0)
# play around with values of b0
l.BPb0s1 <- -5+-0.15420083 + BP.b[,mean(b1)]*log(age) + paramsPA[b=="b2",mean(value)]*age
BPhatmeanb0s1 <- exp(l.BPb0s1)
plot(age,BPhatmeanb0s1)
l.BPb0s2 <- -10+-0.15420083 + BP.b[,mean(b1)]*log(age) + paramsPA[b=="b2",mean(value)]*age
BPhatmeanb0s2 <- exp(l.BPb0s2)
plot(age,BPhatmeanb0s2)
# played from -30 to +10, still looks like a growth curve
#hist(BP.b$ b1)
l.BPb1s1 <- BP.b[,mean(b0)]+-0.15420083 + 1.85*log(age) + paramsPA[b=="b2",mean(value)]*age
BPhatmeanb1s1 <- exp(l.BPb1s1)
plot(age,BPhatmeanb1s1)
l.BPb1s2 <- BP.b[,mean(b0)]+-0.15420083 + 0.5*log(age) + paramsPA[b=="b2",mean(value)]*age
BPhatmeanb1s2 <- exp(l.BPb1s2)
plot(age,BPhatmeanb1s2)
#hist(paramsPA[b=="b2"]$value)
paramsPA[b=="b2", mean(value)]
l.BPb2s1 <- BP.b[,mean(b0)]+-0.15420083 + BP.b[,mean(b1)]*log(age) + -0.0080*age
BPhatb2s1 <- exp(l.BPb2s1)
plot(age,BPhatb2s1)
l.BPb2s2 <- BP.b[,mean(b0)]+-0.15420083 + BP.b[,mean(b1)]*log(age) + -0.0070*age
BPhatb2s2 <- exp(l.BPb2s2)
plot(age,BPhatb2s2)
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

# data.table
BP.c1 <- as.data.table(cbind(age,BPhatmode,BPhatmean,BPhatb2s2,BPhatb2s1,BPhatmeanb1s2,BPhatmeanb1s1,BPhatmeanb0s2,BPhatmeanb0s1))
setnames(BP.c1,names(BP.c1),c("age","mode","mean","b2s2","b2s1","b1s2","b1s1","b0s2","b0s2"))
BPcurves <- melt(BP.c1, id.vars = c("age"),
                variable.name = "curve", value.name = "yhat")
#g.BPcurves <- ggplot(BPcurves[curve!="b0s2" & curve!="b0s1"],aes(x=age,y=yhat,group=curve,colour=curve)) + geom_point()
g.BPcurves <- ggplot(BPcurves,aes(x=age,y=yhat,group=curve,colour=curve)) + geom_line() +
  theme(legend.position="none")
ggsave(g.BPcurves, file="C:/Celine/GitHub/RCodeSK/figures/fig9_v1.jpeg")
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





