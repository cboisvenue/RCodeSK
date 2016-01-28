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
# psp.tree <- fread(paste(indir,"SK_2000TreeMeasurements.csv",sep=""),sep=",",header=TRUE)
# no.meas.psp <- psp.tree[,.(count = .N),by=dom]
# # better plot with a table
# library(gridExtra)
# g <- ggplot(data=psp.tree) + geom_bar(aes(round(age), fill=SPECIES),colour="black") +
#   xlab("Plot age") + ylab("Number tree-level measurements") 
# #ggtitle("Measured Trees by Age and Species - SK 418") + 
# #theme(plot.title = element_text(lineheight=1.2, face="bold"))
# g+annotation_custom(tableGrob(no.meas.psp),xmin=175, xmax=225,ymin=14000,ymax=75000) 
# ggsave("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/WritingBin/figures/SK2000_TreeMeasAgeSps.jpeg")
# END PSP info---------------------------------------------------------------


#Redoing figure 5 to ensure we are looking at all the same pixels through time ---------------------
raster.biom <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/growth/biomassHaEvaluation/RasterAvg_SD.txt",sep="\t",header=TRUE)
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
  scale_colour_manual(values=c("black", "red"))

## we see the same trends with all the "undisturbed pixels through time
# need to figure out the number of pixels per year...
no.pixels <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/growth/biomassHaEvaluation/cellValue_freq.txt",sep=",",header=TRUE)
# last row is a count of the NAs
nopixels <- no.pixels[1:257]
pix.yr <- colSums(nopixels,na.rm=TRUE)
range(pix.yr[2:30])
#[1] 14342960 14374940
# 14374940-14342960
#[1] 31980
# 31980*0.09
#[1] 2878.2
mean(pix.yr[2:30])
# [1] 14367666
mean(pix.yr[2:30])*0.09
#1293090 ha
sd(pix.yr[2:30])
#[1] 12566.45
sd(pix.yr[2:30])*0.09
#1130.98
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
stratum <- sort(rep(c("BF","BP","BSG","BSM","JP","TAG","TAM","WB","WSG"),250)) # same number of 
# strata as in the fit.data
topredMM <- cbind(stratum,topredMM)
names(topredMM) = c("stratum","age1","l.age") # name them the same this as in the fitting data
NoWSMlhat2 <- predict(modl,newdata=topredMM)
NoWSMhat2 <- exp(NoWSMlhat2)
NoWSMpred2 <- cbind(topredMM,NoWSMhat2,NoWSMlhat2)
fig7 <- ggplot(data=NoWSMpred2,aes(x=age1,y=NoWSMhat2,group=stratum,colour=stratum,linetype=stratum)) + 
  geom_line(size=1) + xlab("Age") + ylab("MgC/ha") + theme(legend.position=c(0.9,0.62))
fig7
ggsave(fig7, file="C:/Celine/GitHub/RCodeSK/figures/fig7_v1.jpeg")
#---------END Fig7-----------------------------------------------------------------

# Building a bunch of curves from the parameter value range for JP in the pixel-based fit
# this is the model
#load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Cboisvenue/CleanedUpForUsing/MEM_RS_OneSample.RData")
g.JPpsp <- ggplot(data=NoWSMpred2[stratum=="JP",],aes(x=age1,y=NoWSMhat2)) + 
  geom_line(size=1) + xlab("Age") + ylab("MgC/ha") 
g.JPpsp

# these are the names of the variables in the memPA.s1
#c("age","logAge","l.dbiom","strata","RasterID")
strata <- rep("JP",250)
RScurveJP.in <- as.data.frame(cbind(plot.age,l.age,strata))
names(RScurveJP.in) <- c("age","logAge","strataPSP")
lhat.JPRS <- predict(memPA.s1,newdata=RScurveJP.in)


psp <- fread(paste(indir,"BiomModelParamsCI.txt",sep=""),sep=",",header=TRUE) # psp params
pspJP <- psp[stratum=="JP"|stratum=="ALL"][,.(b,value)]
lJP.psp <- pspJP[b=="b0",value]+pspJP[b=="b1",value]*plot.age+pspJP[b=="b2",value]*l.age
JP.psp <- exp(lJP.psp)
#setkey(psp,stratum,b)
rs <-  fread(paste(indir,"DeltaBiomRsParams.txt",sep=""),sep=",",header=TRUE)
#setkey(rs,stratum,b)
b0.modes <- read.table(file=paste(indir,"b0RSmodes.txt",sep=""),sep=",",header=TRUE)
b1.modes <- read.table(file=paste(indir,"b1RSmodes.txt",sep=""),sep=",",header=TRUE)
b2.modes <- read.table(file=paste(indir,"b2RSmodes.txt",sep=""),sep=",",header=TRUE)
#stratum <- "ALL"
b2.mode <- as.data.frame(cbind(stratum,b2.modes))





