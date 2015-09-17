#-----------------------------------------------------
# RS Growth project for SK
#
# This script:
# - Compare the parameters estimates from the PSP delta-biomass to the parameter 
#   estimates from the RS delta-biomass.
#
# CBoisvenue Sept 16th, 2015
#-----------------------------------------------------

library(data.table)
library(ggplot2)

# Read in both parameter estimates
#-----------------------------------------
data.in = "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/"

psp <- fread(paste(data.in,"BiomModelParamsCI.txt",sep=""),sep=",",header=TRUE)
setkey(psp,stratum,b)
rs <-  fread(paste(data.in,"DeltaBiomRsParams.txt",sep=""),sep=",",header=TRUE)
setkey(rs,stratum,b)
# End of reading in data -------------------------

# Compare one parameter type at a time
#-----------------------------------------------------------------

# graphically compare the intercepts only
b0psp <- psp[.(stratum,"b0")][stratum!="ALL"]
b0rs <- rs[b=="b0"]


b0.plot <- ggplot(data=b0psp, aes(y=value,x=stratum)) + 
  geom_errorbar(aes(ymin=lower.b,ymax=upper.b), width=.2,colour="purple") + geom_point(colour="purple") +
  geom_boxplot(data=b0rs,aes(x=stratum,y=value,alpha=0.2)) +
  theme(legend.position="none") +
  ggtitle("Intercept values comparison, RS and PSP") +
  theme(plot.title=element_text(face="bold",size=20))
ggsave(b0.plot, file="G:/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/ParamCompare/b0.jpeg")
# End intercept graphical comparison----

# graphically compare the logAge slopes only
b1psp <- psp[b=="b1"]
b1rs <- rs[b=="b1"]

b1.plot <- ggplot(data=b1psp, aes(y=value,x=stratum)) + 
  geom_errorbar(aes(ymin=lower.b,ymax=upper.b), width=.2,colour="purple") + geom_point(colour="purple") +
  geom_boxplot(data=b1rs,aes(x=stratum,y=value,alpha=0.2)) +
  theme(legend.position="none") +
  ggtitle("Slope for logAge comparison, RS and PSP") +
  theme(plot.title=element_text(face="bold",size=20))
ggsave(b1.plot, file="G:/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/ParamCompare/b1.jpeg")
# End logAge slope graphical comparison----

# graphically compare Age slopes
b2psp <- psp[b=="b2"]
b2rs <- rs[b=="b2"]

b2.plot <- ggplot(data=b2psp, aes(y=value,x=stratum)) + 
  geom_errorbar(aes(ymin=lower.b,ymax=upper.b), width=.2,colour="purple") + geom_point(colour="purple") +
  geom_boxplot(data=b2rs,aes(x=stratum,y=value,alpha=0.2)) + 
  theme(legend.position="none") +
  ggtitle("Slope for logAge comparison, RS and PSP") +
  theme(plot.title=element_text(face="bold",size=20))
ggsave(b2.plot, file="G:/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/ParamCompare/b2.jpeg")
# End Age slope graphical comparison----

# NEXT?
# create histograms of all RS values to get an idea of the mode?
# calculate mode 
# make curves from mode
# try another set of samples to see of results compare




