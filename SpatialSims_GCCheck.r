# --------------------------------------------------------------------
# Checking the GC that were used in the spatial simulations for SK
# Problem: they do not seem to match the growth curves I have.
#
# January 8th, 2016
# CBoisvenue
#--------------------------------------------------------------------


library(data.table)
library(ggplot2)

indir <- "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Sask_runs/11_CASFRI/results/"
outfigs <- "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Sask_runs/11_CASFRI/results/figures/"

# THis all started b/c I wanted to know how many hectares were modelled with the TA growth curve
# the one with very high productivity

# Background code
    growth.ha <- fread(paste(indir,"NIRComparison/HaByGrowthCurves_Spatial.csv",sep=""),sep=",",header=TRUE)
    #which(growth.ha$fm_scenario!=growth.ha$forest_type)
    # Can get rid of some columns and rename others to make it easier
    growth.ha <- growth.ha[,c("fm_scenario","stocking_class"):= NULL]
    setnames(growth.ha,names(growth.ha),c("spu_id","ecozone","Forest_District","species",
                                          "site_quality","gcid","ha"))
    strata <- growth.ha$species
    strata[which(growth.ha$species=="White spruce")] <- paste("WhiteSpruce",growth.ha$site_quality[which(growth.ha$species=="White spruce")],sep="")
    strata[which(growth.ha$species=="Black spruce")] <- paste("BlackSpruce",growth.ha$site_quality[which(growth.ha$species=="Black spruce")],sep="")
    ha.strata <- cbind(growth.ha,strata)
    
    g.ha.strata <- ggplot(data=ha.strata, aes(x=strata,y=sum(ha)/1000000)) +
      geom_bar(stat="identity") + ylab("Mha")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
    g.ha.strata 
    ggsave(file=paste(outfigs,"HaByStrata_withUnspecified.jpeg",sep=""))
    
    # how do the "Unspecified conifers - Genus type" grow?
    un.gcid <- ha.strata[strata=="Unspecified conifers - Genus type"]
    no.un <- ha.strata[strata!="Unspecified conifers - Genus type"]
    any.gcid <- un.gcid$gcid %in% no.un$gcid
    ## They have no matching gcid in the rest of the strata...
    
    ## looking for what growth curves these were assigned to
    find.gcid <- unique(un.gcid$gcid)
    # these are all the growth curves used in the simulation
    yieldc <- fread(paste(indir,"ToolboxResults/tblYieldLookup.txt",sep=""),sep=",",header=TRUE)
    find.gcid %in% yieldc$GCSpeciesID
    yield.find <- yieldc[GCSpeciesID %in% find.gcid]
    
    # This shows the yield curve for the "Un"
    g.yield.un <- ggplot(data=yield.find,aes(x=Age,y=Volume,colour=GCSpeciesID)) +
      geom_line(size=1) + ylab("m3/ha") + theme(legend.position="none") +ggtitle("Unspecified conifer")
    #ggsave(g.yield.un,file=paste(outfigs,"UnspecifiedSpeciesGC.jpeg",sep=""))
    # This is most similar to BF so we will add the unspecified species #ha to BF
    ## PROBLEM: Byron said these "Unspecified conifer were assigned JP curve...which does not
    # seem to be the case

## Graph all curves used in spatial simulations

# this is from the INPUT DB
g.yieldc <- ggplot(data=yieldc,aes(x=Age,y=Volume,group=GCSpeciesID,colour=GCSpeciesID))+ geom_line(size=1) + ylab("m3/ha")
ggsave(g.yieldc,file=paste(outfigs,"GC_Check/InputDBYield.jpeg",sep=""))

# Now look at what recliner reads in
recYield <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Sask_runs/11_CASFRI/LookupTables/yield_1_7.csv",sep=",",header=TRUE)
unique(recYield$ManagementStatus)
length(unique(recYield$ForestDistrict))#30
# in the ha DB
length(unique(ha.strata$Forest_District))#28
#in the Input DB there is no districts

unique(recYield$Species)
# [1] "Balsam fir"                        "Balsam poplar"                    
# [3] "Black spruce"                      "Jack pine"                        
# [5] "Trembling aspen"                   "Unspecified conifers - Genus type"
# [7] "White birch"                       "White spruce"                     
unique(ha.strata$species)
# [1] "Balsam fir"                        "Balsam poplar"                    
# [3] "Black spruce"                      "Jack pine"                        
# [5] "Trembling aspen"                   "Unspecified conifers - Genus type"
# [7] "White birch"                       "White spruce"                     

recYield <- melt(recYield[,c("ManagementStatus","ForestDistrict") := NULL],id.vars = 
                   c("prodClass","Species"),variable.name = "Age",value.name = "m3/ha")
g.recYield <- ggplot(data=recYield,aes(x=Age,y=`m3/ha`,group=interaction(prodClass,Species),
                                       colour=interaction(prodClass,Species))) +
  geom_line(size=1) + ylab("m3/ha") +theme(legend.position="none") + ggtitle("Recliner Iput")
ggsave(g.recYield,file=paste(outfigs,"GC_Check/ReclinerGC.jpeg",sep=""))

# is the GrowthTable_PSP.csv the same?
GT <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Sask_runs/PSP_YieldTables/GrowthTables_PSP.csv",sep=",",header=TRUE)
GT <- melt(GT,id.vars = c("dom","prodClass","stratum"),variable.name = "Age",value.name = "m3/ha")
g.GT <-ggplot(data=GT,aes(x=Age,y=`m3/ha`,group=stratum,colour=stratum)) +
  geom_line(size=1) + ylab("m3/ha") +theme(legend.position="none") + ggtitle("GrowthTable_PSP")
ggsave(g.GT,file=paste(outfigs,"GC_Check/GrowthTable_fromR.jpeg",sep=""))


load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/GrowthCurvesMEModel.RData")
