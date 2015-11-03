# Results Growth paper
# Total Biomass over age and time in the PSP
# for results section
# CBoisvenue
# October 22nd, 2015
# ----------------------------------------------------

library(data.table)
require(plyr)
library(dplyr)
library(ggplot2)

indir = "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/"
pspBiomInc <- read.table(paste(indir,"t_haBiom_yr.txt",sep=""),header=TRUE,sep=",")
tree.biom <- read.table(paste(indir,"SK_2000TreeBiomass.csv",sep=""),sep=",",header=TRUE)
mem.fit <- read.table(paste(indir,"FittingData_BiomassPSPModel.txt",sep=""),sep=",",header=TRUE)
strata <- as.data.table(unique(cbind(PLOT_ID=mem.fit$PLOT_ID,strata=mem.fit$stratum)))

# the biomass here in in kg (checked in pub)
tree.biom <- as.data.table(tree.biom)
# sum per plot, yr, sps
plot.biom <- tree.biom[,.(tot.biom = sum(biomass),dom = first(dom),age=round(mean(age))),by=.(PLOT_ID,YEAR,SPECIES)]
# get plot sizes
plotInfo <- read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/BiomassEstimation/measurement_header.csv",sep=",", header=TRUE)
plotInfo <- as.data.table(plotInfo)
plotSize <- plotInfo[,.(PLOT_ID,PLOT_SIZE)]
plotSize <- unique(plotSize)
# merge
setkey(plot.biom,PLOT_ID)
setkey(plotSize,PLOT_ID)
setkey(strata,PLOT_ID)
plot.biom.ha <- merge(plot.biom,plotSize)
plot.biom.ha[,kg.ha := (tot.biom/PLOT_SIZE)]

psp <- merge(plot.biom.ha,strata)

# just by year
by.year <- psp[,.(allSps.kg.ha=sum(kg.ha)),by=.(PLOT_ID,YEAR)]
mean.year <- by.year[,.(mean=mean(allSps.kg.ha),sd=sd(allSps.kg.ha),no.plot=.N),by=YEAR]
setkey(mean.year,YEAR)

# plot
pb <- ggplot(mean.year,aes(YEAR,mean/1000))
pb + geom_point(aes(size=no.plot),colour="red") + ylab("Mg/ha") + theme(text = element_text(size=20)) +
  geom_errorbar(aes(ymin=(mean/1000)-1.96*(sd/1000),ymax=(mean/1000)+1.96*(sd/1000)))
ggsave("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/WritingBin/figures/Biomass_Range_PSPs.jpeg")

# hist
hist(mean.year$mean/1000)
ggplot(mean.year,aes(mean/1000)) + geom_histogram()
