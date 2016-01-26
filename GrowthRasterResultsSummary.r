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
biom.ha.psp <- fread(paste(indir,"SK_2000Biomass_ha.txt",sep=""),sep=",", header=TRUE)
# range(biom.ha.psp$biom.ha)
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
psp.tree <- fread(paste(indir,"SK_2000TreeMeasurements.csv",sep=""),sep=",",header=TRUE)
no.meas.psp <- psp.tree[,.(count = .N),by=dom]
# better plot with a table
g <- ggplot(data=psp.tree) + geom_bar(aes(round(age), fill=SPECIES),colour="black") +
  xlab("Plot age") + ylab("Number tree-level measurements") 
#ggtitle("Measured Trees by Age and Species - SK 418") + 
#theme(plot.title = element_text(lineheight=1.2, face="bold"))
g+annotation_custom(tableGrob(no.meas.psp),xmin=175, xmax=225,ymin=14000,ymax=75000) 
ggsave("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/WritingBin/figures/SK2000_TreeMeasAgeSps.jpeg")
# END PSP info---------------------------------------------------------------


#Redoing figure 5 to ensure we are looking at all the same pixels through time
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

pix.yr <- colSums(no.pixels,)
pix.yr %in% 623940100
# total no of pixels per year is 623940100 which equals 56154609 ha

623940100*0.09
56154609




