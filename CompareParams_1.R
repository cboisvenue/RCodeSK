#-----------------------------------------------------
# RS Growth project for SK
#
# This script:
# - Compare the parameters estimates from the PSP delta-biomass to the parameter 
#   estimates from the RS delta-biomass.
# - this is done for the 1st sample (lines 21 to 66)
# - then 6 other RSsample-estimated parameter files are read in and all are compared
#  on the same graph
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

# Are samples consistent?
# trying other samples to see if results are consistent
#-------------------------------------------------------------------------------------
s.no = c(1,2,3,4,5,6,99)

for(i in 1:length(s.no)){
  # read  
  rss <- fread(paste(data.in,"DeltaBiomRsParams_s",s.no[i],".txt",sep=""),sep=",",header=TRUE)
  setkey(rs,stratum,b)
  
  #graphs
  b0rs <- rss[b=="b0"]
  b0.s <- b0.plot + 
    geom_boxplot(data=b0rs,aes(x=stratum,y=value, colour="red", alpha=.2)) 
  ggsave(b0.s, file=paste("G:/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/ParamCompare/b0s",s.no[i],".jpeg",sep=""))
   
  b1rs <- rss[b=="b1"]
  b1.s <-  b1.plot  +
    geom_boxplot(data=b1rs,aes(x=stratum,y=value, colour="red", alpha=.2)) 
  ggsave(b1.s, file=paste("G:/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/ParamCompare/b1s",s.no[i],".jpeg",sep=""))
  
  b2rs <- rss[b=="b2"]
    b2.s <- b2.plot +
      geom_boxplot(data=b2rs,aes(x=stratum,y=value, colour="red", alpha=.2))  
    ggsave(b2.s, file=paste("G:/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/ParamCompare/b2s",s.no[i],".jpeg",sep=""))
    
}
# Results of sampling seem consistent-------------------------------------------------

# working with the 1st sample (since they are all the same)
# Calculate the mode for each param
#-------------------------------------------------------------------------------------

# try this function--------------------
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
# function end------------------

b0.modes <- b0rs[,.(mode=modeCalc(value)),by=stratum]
b1.modes <- b1rs[,.(mode=modeCalc(value)),by=stratum]
b2.modes <- b2rs[,.(mode=modeCalc(value))]
# Modes calculated-------------------------------------------------------------------


# NEXT?
# create histograms/lines of all RS values to get an idea of the mode?
denslines <- function(data){
  lim.inf=min(b2rs$value)-1; lim.sup=max(b2rs$value)+1
  s<-density(b2rs$value,from=lim.inf,to=lim.sup,bw=0.2)
  n<-length(s$y)
  v1<-s$y[1:(n-2)];
  v2<-s$y[2:(n-1)];
  v3<-s$y[3:n]
  ix<-1+which((v1<v2)&(v2>v3))
  
  dlines <- as.data.table(cbind(s$x,s$y))
  dlines
}
dens <- density(,from=lim.inf,to=lim.sup,bw=0.2)




