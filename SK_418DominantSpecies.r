#----------------------------------------------------------
# Making the dataframe of PSP domninant species per plot per year
#
# sources of sps info: PSPs (real), Species2010_binary, Species2010_proportions6, CASFRI
# up to now, only the SPECIES2010 have been compared to the PSP data
# it was compared to both the demographic-based species % and the biomass-based
# this was done in SK418_RandomForestModelFristTryExploration.R
#
# CBoisvenue
# March 2nd, 2015
#-----------------------------------------------------------


require(plyr)
#install.packages("ggplot2")
require(ggplot2)
#install.packages("dplyr")
require(dplyr)
#  install.packages("gridExtra")
library(gridExtra)  

require(reshape2)

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/")
#-------------------------------------------------------------
# 1. Check if biomass-based leading species differs from demography-based in PSPs

# Read in plot-level biomass data
biom.psp <- read.table("./data/SK418Biomass_ha.txt",header=TRUE,sep=",")

# get psp sps
tree.psp <- read.table("./data/SK418TreeBiomass.txt",header=TRUE,sep=",")

# get rid of the off-plot trees and keep plot, year, sps, biomass per tree
psp.sps <- filter(tree.psp,OFF_PLOT_TREE!="Y" | is.na(OFF_PLOT_TREE)) %>%
  select(PLOT_ID,YEAR,SPECIES,biomass)

# species % by tree count 
# count the trees by species by year
sps_no <- group_by(psp.sps,PLOT_ID,YEAR,SPECIES) %>%
  summarise(n())
# total number of trees per plot per year
sps_treetotal <- group_by(psp.sps,PLOT_ID,YEAR) %>%
  summarise(n())
# make one df
psp.sps1 <- left_join(sps_no,sps_treetotal, by=c("PLOT_ID", "YEAR")) 
# proportion by counting the number in a sps over the overall number of trees
dem_prop = psp.sps1[,4]/psp.sps1[,5]

# species % by proportion of the total biomass
# sum the biomass by species
sps_biom <- group_by(psp.sps,PLOT_ID,YEAR,SPECIES) %>%
  summarise(sum(biomass))
# sum the biomass over the whole plot
sps_biomtotal <- group_by(psp.sps,PLOT_ID,YEAR) %>%
  summarise(sum(biomass))
# make one df
psp.sps2 <- left_join(sps_biom,sps_biomtotal, by=c("PLOT_ID", "YEAR")) 
biom_prop = psp.sps2[,4]/psp.sps2[,5]

#### import data frame
## this dataframe has both proportions: demographically-based and biomass-based
# for all the species, not just dominant
sps_by_psp <- cbind(psp.sps1[,1:3],dem_prop,biom_prop)
# rename the columns to make life easier
names(sps_by_psp) = c("PLOT_ID","YEAR","SPECIES","dem","biomass")

# domninant species by plot and year from tree count %
psp.dom.sps.dem <- arrange(sps_by_psp,PLOT_ID,YEAR,dem) %>%
  group_by(PLOT_ID,YEAR) %>%
  summarise(last(SPECIES))
# keep the % dem
psp.dom.sps.dem1 <- arrange(sps_by_psp,PLOT_ID,YEAR,dem) %>%
  group_by(PLOT_ID,YEAR) %>%
  summarise(last(dem))
psp.dom.dem <- cbind(psp.dom.sps.dem,psp.dom.sps.dem1[,3])
# domninant species by plot and year from biomass %
psp.dom.sps.biomass <- arrange(sps_by_psp,PLOT_ID,YEAR,biomass) %>%
  group_by(PLOT_ID,YEAR) %>%
  summarise(last(SPECIES))
psp.dom.sps.biomass1 <- arrange(sps_by_psp,PLOT_ID,YEAR,biomass) %>%
  group_by(PLOT_ID,YEAR) %>%
  summarise(last(biomass))
psp.dom.biomass <- cbind(psp.dom.sps.biomass,psp.dom.sps.biomass1[,3])

check1 <- which(psp.dom.dem[,3]!=psp.dom.biomass[,3])
length(check1) #144 of 1075 are not equal.
check2 <- which(psp.dom.biomass[,3]!=psp.dom.dem[,3])
length(check2) #144
# Yes, they differ -------------------------------------------------------

# 2. check the plots that differ more closely ----------------
# get the plot numbers for the non-matched ones
pspcheck1 <- unique(psp.dom.dem[check1,1]) 
#79 plots have a non-match over 418
check.dom1 <- cbind(psp.dom.dem[check1,],psp.dom.biomass[check1,])
diff.biomass = check.dom1[,8]-check.dom1[,4]
hist(diff.biomass)
hist(check.dom1[,8])
hist(check.dom1[,4])

###CONCLUSION: Because we are looking at reflectances and not individual trees,
# I will take the biomass-based numbers--------------------------------------

## PSP-based dominant species by plot by year for 418 plots
# rename to make life easier
names(psp.dom.biomass) = c("PLOT_ID","YEAR", "domsps","domperc")
write.table(x=psp.dom.biomass,"G:/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/PSP418_DomSpecies.txt",
            sep=",",row.names=FALSE)

# ENd of dominant species per plot script-----------------------------------------------------------------

# Extra bits: creating a column for each species % to that I can try to use randomforests to 
# predict % for each species------------------------------------------------------------------------------
# If I tried to predict a proportion for each species...I would need this info
table(sps_by_psp$SPECIES)

# there are only 3 MM, which are they and what is the biomass? (same tree measured 3 times)
#Get rid of them, the proportion are very low
# get rid of the dem proportions also since we decided to got with the biomass-based proportions
sps.prop1 <- sps_by_psp[-which(sps_by_psp$SPECIES=="MM"),-4]

# Now put it in wide format so that each species is a variable
sps.prop <- dcast(sps.prop1,PLOT_ID+YEAR ~SPECIES)
# get rid of the NAs
sps.prop[is.na(sps.prop)] <-0
write.table(x=sps.prop,"G:/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/PSP418_SpeciesProportions.txt",
            sep=",",row.names=FALSE)
# End of creating a dataframe for running random forests on each individual species to predict percent cover
#----------------------------------------------------------------------------------------------------------