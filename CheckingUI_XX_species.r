#-------------------------------------------------------------------------------------
# Growth Raster Project for SK
#
# afte cleaning-up the SK2000 (as opposed to the SK418 that I had been mostly working with)
# I realized that I did not have to eleminate "UI" and "XX" species. So, in the SK2000 
# calculations the two species are left there and either filled-in from other years were 
# species were identified or logically
#
# the purpose of this script is to check it is is worth incluing thoses species 
# in SK418 data used in RF development...
#
# May 11, 2015
# CBoisvenue
#---------------------------------------------------------------------------------

indir = "G:/RES_Work/Work/JoanneWhite/SK_work/data/"
outdir = "G:/RES_Work/Work/JoanneWhite/SK_work/data/PSP/"

library(plyr)
library(dplyr)
library(reshape2)
require(ggplot2)

# Read-in SK418 and SK2000_Biomass_ha
in418 <- read.table(paste(indir,"SK418Biomass_ha.txt",sep=""), header=TRUE,sep=",")
in2000 <- read.table(paste(indir,"CleanedUpForUsing/SK_2000Biomass_ha.txt",sep=""), header=TRUE,sep=",")

both1 <- select(in418, PLOT_ID,YEAR,biom.ha1 = biom.ha) %>%
  inner_join(in2000, by=c("PLOT_ID","YEAR"))

ggplot(data=both1,aes(biom.ha,biom.ha1)) + geom_point()

diff418_2000 <- both1$biom.ha1-both1$biom.ha

# CONCLUSION: the are very close to each other. So, I will use the new sk2000 biomass 
# calculation for consistency

tree418 <- read.table(paste(indir,"SK418TreeBiomass.txt",sep=""), header=TRUE,sep=",")
tree2000 <- read.table(paste(indir,"CleanedUpForUsing/SK_2000TreeBiomass.csv",sep=""), header=TRUE,sep=",")
both.tree <- inner_join(tree418,tree2000,by=c("PLOT_ID","TREE_NO","YEAR"))

ggplot(data=both.tree,aes(biomass.y,biomass.x)) + geom_point()
tree.diff = both.tree$biomass.x-both.tree$biomass.y
