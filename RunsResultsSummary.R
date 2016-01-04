# -------------------------------------------------
# SK spatial runs for CASFRI
# creating graphs for results
# first attempt
#
# Background: Byron created some summaries of , NEP, NBP, area disturbed, and Total stocks
# for the runs done for the CASFRI area of SK.
#
# CBoisvenue
# November 19, 2015
#--------------------------------------------------

library(data.table)
library(ggplot2)

indir <- "C:/Celine/sync2/Sync/CBM_runs/CASFRI_results/"
outfigs <- "C:/Celine/sync2/Sync/Figures/"

list.files(indir)

# Input description----------------------------------------------

# age class dist in 1984 ---------------------------------------
age10 <- fread(paste(indir,"CBMPlotterResults/age_binned_10.csv",sep=""),sep=",",header=TRUE)
age10<- melt(age10, value.name = "ha")
year <- sort(rep(1984:2012,20))
age10 <- cbind(year,age10)
age10[,"variable":=NULL]
setcolorder(age10,c("group","year","ha"))

age10[group=="<10",group := "0"]
age10[group==">180",group := "180"]
#age10[,group :=as.numeric(group)]
age1stLast <- age10[year==1984|year==2012]
# age1984 <- age10[year==1984]
# age1984[,group := (as.numeric(group)+5)]
age1stLast[,group := (as.numeric(group)+5)]
age1stLast[,year := as.character(year)]
# colour blind palette
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

g1 <- ggplot(age1stLast,aes(x=group,y=ha/1000000,group=year,fill=year)) + 
  geom_bar(stat="identity",position="dodge") +
  xlab("10-year Age Classes") + ylab("Mha") +
  theme(legend.position=c(0.85,0.8)) + scale_fill_manual(values=c("#3399CC","#000066"))
# age class done-------------------------------------------------

# disturbances per year  ----------------------------
distsums <- fread(paste(indir,"ToolboxResults/AllDisturbance.csv",sep=""),sep=",",header=TRUE)
names(distsums)
setnames(distsums,names(distsums),c("timestep","fire","harvest","deforestation","mortality 20%"))
dist.yr <- melt(distsums, id.vars = c("timestep"),
                variable.name = "disturbance", value.name = "ha")

year <- rep(1984:2012,4)
dist.yr <- cbind(year,dist.yr)
# get rid of 1984 and 2012 b/c of 0 hectares disturbed
dist.yr <- dist.yr[ha>0]

g2 <- ggplot(data=dist.yr,aes(x=year,y=ha/1000000,group=disturbance,fill=disturbance)) + 
  geom_bar(stat="identity") + ylab("Mha") + theme(legend.position=c(0.1,0.65))
ggsave(g2,file=paste(outfigs,"Figure5_haDistyr.jpeg",sep=""))
# end dist per year---------------------------------

# dom species ha ---------------------------------------
domspsha <- fread(paste(indir,"CASFRI_domSp.csv",sep=""),sep=",",header=TRUE)
domspsha[,"value" := NULL]  # for some unknown reason this is not getting rid of the value column...

g4 <- ggplot(data=domspsha,aes(x=Name,y=hectares,fill=Name)) + geom_bar(stat="identity") +
  theme(legend.position="none")
# end of species---------------------------------------

# Growth curves-----------------------------------------------------------
growth1 <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/MEMPredictedGrowth_noTAG.txt",sep=",",header=TRUE)
# changing the names of the strata
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","black")

library(plyr)
strata <- revalue(growth1$stratum,c("BSG"="BSGood","BSM"="BSMedium","TAM"="TA","WSG"="WSGood","WSM"="WSMedium"))
growth <- cbind(growth1[,stratum:= NULL],strata)
g3 <- ggplot(data=growth,aes(x=plot.age,y=memT.notagyhat,group=strata,colour=strata)) + 
  geom_line(size=1) + ylab("m3/ha") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","black"))
g3 + theme(legend.position=c(0.1,0.65)) 
  

ggsave(file=paste(outfigs,"growthCurves.jpeg",sep=""))

#+  scale_fill_brewer(palette="Spectral")
# end of growth curves-----------------------------------------------------

# growth model error-------------------------------------------

# this is the model it is called memTnotag
load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/GrowthCurvesMEModel_noTAG.RData")

# error graph NOT SURE IF I NEED TO SHOW THESE--------------------------------------------
error1 <- as.data.frame(cbind(c(1:length(residuals(memTnotag))),residuals(memTnotag)))
names(error1) = c("Index","Error")
plot.er1 <- ggplot(data=error1, aes(Index,Error)) + geom_point(size=2) +geom_hline(y=0,size=1) + theme(text = element_text(size=20))
library(lme4)
error2 <- as.data.frame(ranef(memTnotag)$PLOT_ID)
names(error2) <- "Intercept"
plot.er2 <- ggplot(data=error2,aes(sample=Intercept)) +stat_qq(shape=1) +theme(text = element_text(size=20))
# Multiple plot functions
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(plot.er1,plot.er2)
# END of error plots-------------------------------------------------

# carbon values are in metric tonnes/megagrams of C --------------------------------
# stocks--------------
stocks <- fread(paste(indir,"ToolboxResults/total_ecosystemCStocks_total.csv",sep=""),sep=",",header=TRUE)
year <- 1984:2012
stocks <- cbind(year,stocks)
g.stocks <- ggplot(data=stocks,aes(x=year,y=`Total Ecosystem`,group=1)) + geom_line(size=1)
# g.stocks +ggtitle("Total Ecosystem Carbon")
stocks.84 <- stocks[year=="1984"]
stocks.12 <- stocks[year=="2012"]
# if the values are already in Mg...here it becomes 10^12 (teragrams)
d.stocks <- (stocks.12$`Total Ecosystem` - stocks.84$`Total Ecosystem`)/1000000
avgStock <- 
# for NIR comparison
stocks.90 <- stocks[year=="1990"]
stocks.12 <- stocks[year=="2012"]
# if the values are already in Mg...here it becomes 10^12 (teragrams)
d.stocks2 <- (stocks.12$`Total Ecosystem` - stocks.90$`Total Ecosystem`)/1000000
# end of stocks--------

# carbon density -----------------
Cdensity <- fread(paste(indir,"ToolboxResults/totalEcosystemStocks_perHa.csv",sep=""),sep=",",header=TRUE)
year <- 1984:2012
Cdensity <- cbind(year,Cdensity)
g.density <- ggplot(data=Cdensity,aes(x=year,y=`perHa Total Ecosystem`,group=1)) + geom_line(size=1)
# g.stocks +ggtitle("Total Ecosystem Carbon")
avgDensity <- Cdensity[,.(Avg = mean(`perHa Total Ecosystem`), sd=sd(`perHa Total Ecosystem`))]
avgArea <- Cdensity[,.(Avg = mean(hectares), sd=sd(hectares))]
# END carbon density -------------

# Fluxes through time, totals in tonnes of C (megaGrams)
# NBP/NEP----------------------------
nppnep <- fread(paste(indir,"ToolboxResults/NEP&NPP_totals.csv",sep=""),sep=",",header=TRUE)
nppnep[,c("V4","V5") := NULL]
setnames(nppnep,names(nppnep),c("timestep","NPP","NEP"))
year <- 1984:2012
nppnep <- cbind(year,nppnep)
nppnep <- nppnep[2:29,]

nbp <-  fread(paste(indir,"ToolboxResults/NBP_total.csv",sep=""),sep=",",header=TRUE)
setnames(nbp,names(nbp),c("timestep","NBP"))
nbp <- nbp[timestep>0]
year <- 1985:2012
nbp <- cbind(year,nbp)
setkey(nbp,year,timestep)
setkey(nppnep,year,timestep)
fluxes <- merge(nbp,nppnep)
fluxes[,Rh := (NPP-NEP)]
fluxes2 <- melt(fluxes,id.vars = c("year","timestep"),
                variable.name = "flux", value.name = "MgC")

g.fluxes <- ggplot(data=fluxes2,aes(x=year,y=MgC,group=flux,colour=flux)) + 
  geom_line(size=1.2) + geom_hline() +theme(legend.position=c(0.85,0.65))
ggsave(g.fluxes,file=paste(outfigs,"Figure8_fluxesTotal.jpeg",sep=""))
# End fluxes-------------------------


# ABGbiomass -----------------------------
agbiom <- fread(paste(indir,"ToolboxResults/agBiomass_totals.csv",sep=""),sep=",",header=TRUE)
agbiom <- agbiom[,c("Foliage(SW) + Foliage(HW)","Other(SW) + Other(HW)","Merch(SW) + Merch(HW)") := NULL]
year <- 1984:2012
agbiom <- cbind(year,agbiom)
g.abgbiom <- ggplot(data=agbiom,aes(x=year,y=`Aboveground Biomass`)) + geom_line(colour="green",size=1)
# End ABGbiomass -------------------------

# emissions ------------------------------

fire <- fread(paste(indir,"ToolboxResults/FireEmissionsBySource.csv",sep=""),sep=",",header=TRUE)
harv <- fread(paste(indir,"ToolboxResults/HarvestEmissionsBySource.csv",sep=""),sep=",",header=TRUE)
defor <-fread(paste(indir,"ToolboxResults/DeforestationEmissionsBySource.csv",sep=""),sep=",",header=TRUE)

dist <- c(rep("fire",27),rep("harvest",27),rep("deforestation",27))
dist.em <- cbind(rbind(fire,harv,defor),dist)

# calculate total emissions from dist by gas 
# Checked: this matches the StinsonTable_v2.xls values lines 37+38
tot.em <- dist.em[,.(tot.CO2 = sum(`Total CO2`), tot.CO = sum(`Total CO`),tot.CH4 = sum(`Total CH4`)), by=`year`]
tot<- tot.em[,.(tot = (tot.CO2+tot.CO+tot.CH4))]
tot.em <- cbind(tot.em,tot)
CO2 <- mean(tot.em[,(CO2 = (tot.CO2/tot)*100)]) # 90%
CO <- mean(tot.em[,(CO = (tot.CO/tot)*100)]) # 9%
CH4 <- mean(tot.em[,(CH4 = (tot.CH4/tot)*100)]) # 1%
Gas <- c("CO2","CO","CH4")
percent <- c(round(CO2),round(CO),round(CH4))
gas.prop <-cbind(Gas,percent)
  
# tot.em1 <- melt(tot.em,id.vars = c("year"),
#                 variable.name = "gas", value.name = "MgC")
#library(gridExtra)
#grid.table(gas.prop)
g.emissions.tot <- ggplot(data=tot.em,aes(x=year,y=tot)) +
   geom_line(size=1.2)
# g.emissions.tot + theme(legend.position=c(0.9,0.62))
#+ 
  #annotation_custom(grob= tableGrob(gas.prop),xmin=2005,xmax=2005,ymin=4000000,ymax=6000000) 

# totals by dist type
dist.tot <- dist.em[,.(dist.tot=sum(`Total CO2`,`Total CO`,`Total CH4`)), by=.(year,dist)]
year <- 1985:2011
dist <- rep("Total",27)
add.tot <- as.data.table(cbind(year,dist,tot))
setnames(add.tot,names(add.tot),c("year","dist","dist.tot"))
dist.tot2 <- rbind(add.tot,dist.tot)
# tried to put all values in one data.frame so that the legend would have the tot also...did not
# work, don't know why...left it for now
# dist <- c(dist,rep("total",27))
# year <-c(dist.tot$year,1985:2011)
# em <- c(dist.tot$dist.tot,tot.em$tot)
# all.em <- as.data.frame(cbind(year,dist,em))
# g.emissions <-  ggplot(data=all.em) +
#                        geom_line(aes(x=year,y=em,fill=dist,group=dist,colour=dist), size=1.2, linetype=2) + 
#   theme(legend.position=c(0.9,0.62))

# g.emissions.tot +geom_line(data=dist.tot,aes(x=year,y=dist.tot,fill=dist,group=dist,colour=dist), 
#                            size=1.2, linetype=2) + theme(legend.position=c(0.9,0.62))
# ggsave(file=paste(outfigs,"Figure9_DistEmissions.jpeg",sep=""))  
g.emissions.tot2 <- ggplot(data=dist.tot2,aes(x=year,y=dist.tot/1000000,fill=dist,group=dist,colour=dist,linetype=dist)) +
  geom_line(size=1.2) + theme(legend.position=c(0.9,0.62)) + ylab("TgC") +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73","black"))
ggsave(file=paste(outfigs,"Figure9_DistEmissions.jpeg",sep=""))    

# NIR Comparison-------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
# Growth curves--------------------
NIR.curves <- fread("C:/Celine/sync2/Sync/CBM_runs/NIRgrowth_curve_details.csv",sep=",",header=TRUE)
NIR.curves[is.na(NIR.curves)] <- 0
NIRc1 <-melt(NIR.curves,id.vars = c("gcid","forest_type"),
             variable.name = "ageclass", value.name = "growth")
g.NIRc1 <- ggplot(data=NIRc1,aes(x=as.numeric(ageclass),y=growth,colour=interaction(gcid,forest_type))) +
  geom_line() + ylab("m3/ha") + xlab("Age Class") + theme(legend.position="none")
ggsave(g.NIRc1, file=paste(outfigs,"Figure11_NIRgrowthCurves.jpeg",sep=""))
# END growth curves-----------------

#Total Ecosystem, this is copied from the CASFRI_SK_Recliner_vs_NIR2015_Version6.xlxs 
# QAQC spreadsheet ----------------------------------------------------------------
#require(bit64)
compare.totE <- read.table(paste(indir,"NIRComparison/Compare_TotE.csv",sep=""),sep=",",header=TRUE)
compare.totE <- as.data.table(compare.totE)
compare.totE <- compare.totE[,.(DB_Source,SimYear,totlE, totlE.ha=totlE/ha)]

g.compare.totE <- ggplot(data=compare.totE,aes(x=SimYear,y=totlE,fill=DB_Source,coulor=DB_Source)) +
  geom_line() 

diff.totE.NIR <- compare.totE[DB_Source=="SK_NIR" & SimYear==2013]$totlE - compare.totE[DB_Source=="SK_NIR" & SimYear==1990]$totlE
diff.totE.CAS <- compare.totE[DB_Source=="SK_Recliner" & SimYear==2012]$totlE - compare.totE[DB_Source=="SK_Recliner" & SimYear==1984]$totlE
# > diff.totE.CAS
# [1] 9164768
# > diff.totE.NIR
# [1] -67274739
diff.totE.NIR1 <- compare.totE[DB_Source=="SK_NIR" & SimYear==2012]$totlE - compare.totE[DB_Source=="SK_NIR" & SimYear==1990]$totlE
diff.totE.CAS1 <- compare.totE[DB_Source=="SK_Recliner" & SimYear==2012]$totlE - compare.totE[DB_Source=="SK_Recliner" & SimYear==1990]$totlE
#> diff.totE.CAS1
# [1] 4089597
# > diff.totE.NIR1
# [1] -66018471
# END Total Ecosystem -------------------------------------------------------------

# compare C density-------------
compare.avgDensity <- compare.totE[,.(Avg = mean(totlE.ha), sd=sd(totlE.ha)), by=DB_Source]
# DB_Source      Avg        sd
# 1:      SK_NIR 324.1308 2.8984050
# 2: SK_Recliner 357.9183 0.6007114

# Compare fluxes ----------------------------
compare.flux <- fread(paste(indir,"NIRComparison/Compare_Fluxes.csv",sep=""),sep=",",header=TRUE)
compare.flux.ha <-compare.flux[,.(DB_Source,SimYear,NPP.ha = NPP/as.numeric(ha),Rh.ha = Rh/as.numeric(ha),NEP.ha=NEP/as.numeric(ha),NBP.ha=NBP/ha)]

#area.flux <- compare.flux[,c("NPP","Rh","NEP","NBP") := NULL]
comp.area.avg <- compare.flux[,.(avg = mean(ha), sd=sd(ha), max=max(ha)), by=DB_Source]
# DB_Source     avg        sd     max
# 1:      SK_NIR 6562454 11849.321 6586334
# 2: SK_Recliner 5706787  3983.169 5712993

ratio.area <- 5712993/6586334

compare.flux.tot <- compare.flux[,"ha" := NULL]

comp.flux.tot <- melt(compare.flux.tot,id.vars = c("DB_Source","SimYear"),variable.name = "flux",
                 value.name = "MgC")
comp.flux.ha <- melt(compare.flux.ha,id.vars = c("DB_Source","SimYear"),variable.name = "flux",
                      value.name = "MgC/ha")


setnames(comp.flux.tot,names(comp.flux.tot),c("Simulation","Year","flux","MgC"))
setnames(comp.flux.ha,names(comp.flux.ha),c("Simulation","Year","flux","MgC/ha"))
library(plyr)
comp.flux.tot$Simulation <- revalue(x = comp.flux.tot$Simulation,c("SK_NIR"="Reporting","SK_Recliner"="Spatial"))
comp.flux.ha$Simulation <- revalue(x = comp.flux.ha$Simulation,c("SK_NIR"="Reporting","SK_Recliner"="Spatial"))

g.comp.flux.tot <- ggplot(data=comp.flux.tot,aes(x=Year,y=MgC,group=interaction(Simulation,flux),
                                         colour=interaction(Simulation,flux))) + 
  geom_line(size=1.2) + geom_hline() + ylab("MgC") +#"MgC/ha
  scale_colour_manual(values=cbPalette)

#ggsave(g.comp.flux,filename = paste(outfigs,"Figure10_CompareFluxesTOTALS.jpeg",sep=""))
g.comp.flux.ha <- ggplot(data=comp.flux.ha,aes(x=Year,y=`MgC/ha`,group=interaction(Simulation,flux),
                                                 colour=interaction(Simulation,flux))) + 
  geom_line(size=1.2) + geom_hline() + ylab("MgC/ha") +
  scale_colour_manual(values=cbPalette)
#ggsave(g.comp.flux,filename = paste(outfigs,"Figure10_CompareFluxes.jpeg",sep=""))
# End Comppared fluxes-----------------------

# compare #ha disturbed--------------------------------------
ha.dist.spatial <- fread(paste(indir,"NIRComparison/DistHa.csv",sep=""),sep=",",header=TRUE)
ha.dist.spatial <- ha.dist.spatial[TimeStep!=0 & TimeStep!=28]
#this line if I want proportions
ha.dist.spatial.percent<-ha.dist.spatial[,.(DB_Source,Disturbance,Year, percent=(`Area Disturbed`/`Total area`)*100)]

# otherwise
ha.dist.spatial <- ha.dist.spatial[,c("TimeStep","Total area") := NULL]
setnames(ha.dist.spatial,names(ha.dist.spatial),c("DB_Source","Disturbance","Year","ha"))
#setnames(ha.dist.spatial,names(ha.dist.spatial),c("DB_Source","Disturbance","Year","ha"))
# keep only 1990-2011
ha.dist.spatial <- ha.dist.spatial[Year>1989]
ha.dist.spatial.percent <- ha.dist.spatial.percent[Year>1989]


ha.dist.NIR <- fread(paste(indir,"NIRComparison/NIR_DisturbedArea_byType.csv",sep=""),sep=",",header=TRUE)
ha.dist.NIR <- ha.dist.NIR[,c("TimeStep","DistTypeName") := NULL]
ha.dist.NIR <- ha.dist.NIR[,.(ha=sum(`Area Disturbed`)),by=.(DB_Source,Disturbance,Year)]
# keep only 1990-2011
ha.dist.NIR <- ha.dist.NIR[Year<2012]
setkey(ha.dist.NIR,Year)
# these next 4 lines if I want to do proportions
NIR.ha <- fread(paste(indir,"NIRComparison/NIR_areaCompare.csv",sep=""),sep=",",header=TRUE)
setkey(NIR.ha,Year)
ha.NIR <- merge(ha.dist.NIR,NIR.ha)
ha.NIR <- ha.NIR[,.(DB_Source,Disturbance,Year, percent=(ha.x/ha.y)*100)]
# for proportions
ha.dist.percent <- rbind(ha.dist.spatial.percent,ha.NIR)
# for total ha
ha.dist <- rbind(ha.dist.spatial,ha.dist.NIR)

g.dist.diff <- ggplot(data=ha.dist, aes(x=Year,y=ha/1000, 
                                         group=DB_Source,fill=interaction(DB_Source,Disturbance))) +
  geom_bar(stat="identity",position="dodge") + ylab("Mha") +scale_colour_manual(values=cbPalette)
ggsave(g.dist.diff,file=paste(outfigs,"Figure11_CompareDistInHa.jpeg",sep=""))

g.dist.diff2 <- ggplot(data=ha.dist.percent, aes(x=Year,y=percent, 
                                        group=DB_Source,fill=interaction(DB_Source,Disturbance))) +
  geom_bar(stat="identity",position="dodge") + scale_colour_manual(values=cbPalette)
ggsave(g.dist.diff,file=paste(outfigs,"Figure11_CompareDistPercent.jpeg",sep=""))
#HERE
g.tot.diff <- ggplot(data=ha.dist, aes(x=Year,y=ha, group=DB_Source, fill=DB_Source)) +
  geom_bar(stat="identity",position="dodge") 
ggsave(g.tot.diff,file=paste(outfigs,"Figure11_CompareDistTOTALinHa.jpeg",sep=""))

g.fire.diff <- ggplot(data=ha.dist[Disturbance=="fire"], aes(x=Year,y=percent, group=DB_Source, fill=DB_Source)) +
  geom_bar(stat="identity",position="dodge") 
ggsave(g.fire.diff,file=paste(outfigs,"Figure11_CompareDistFIRE.jpeg",sep=""))

g.harv.diff <- ggplot(data=ha.dist[Disturbance=="harvest"], aes(x=Year,y=ha, group=DB_Source, fill=DB_Source)) +
  geom_bar(stat="identity",position="dodge") 
ggsave(g.harv.diff,file=paste(outfigs,"Figure11_CompareDistHARVESTInHA.jpeg",sep=""))
g.harv.diff2 <- ggplot(data=ha.dist.percent[Disturbance=="harvest"], aes(x=Year,y=percent, group=DB_Source, fill=DB_Source)) +
  geom_bar(stat="identity",position="dodge") 
ggsave(g.harv.diff2,file=paste(outfigs,"Figure11_CompareDistHARVESTpercent.jpeg",sep=""))

g.defor.diff <- ggplot(data=ha.dist[Disturbance=="deforestation"], aes(x=Year,y=percent, group=DB_Source, fill=DB_Source)) +
  geom_bar(stat="identity",position="dodge") 
ggsave(g.defor.diff,file=paste(outfigs,"Figure11_CompareDistDEFOR.jpeg",sep=""))

g.mort20.diff <- ggplot(data=ha.dist[Disturbance=="mortality20"], aes(x=Year,y=percent, group=DB_Source, fill=DB_Source)) +
  geom_bar(stat="identity",position="dodge") 
ggsave(g.defor.diff,file=paste(outfigs,"Figure11_CompareDistMORT20.jpeg",sep=""))

mort20.ha.dist.yr <- ha.dist[Disturbance=="mortality20",.(Total.ha = sum(ha)/1000000),by=DB_Source]

# For discussion
#NIR harvest, all of SK ----------------------------------

mdir = "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Sask_runs/CASFRI_results/"

NIRSK.harv <- fread(paste(mdir,"NIRComparison/NIR_harvestArea_ALL.csv",sep=""),sep=",",header=TRUE)
DB_Source <- rep("ALL_NIR",24)
year <- 1990:2013
NIRSK.harv <- cbind(DB_Source,year,NIRSK.harv)
NIRSK.harv <- NIRSK.harv[,c("TimeStep","DistTypeName") := NULL]
setnames(NIRSK.harv,names(NIRSK.harv),c("DB_Source","Year","ha"))
ha.dist.harv <- ha.dist[Disturbance=="harvest"]
ha.dist.harv <- ha.dist.harv[,"Disturbance" := NULL]
ALL.harv <- rbind(NIRSK.harv,ha.dist.harv)
ALL.harv <- ALL.harv[Year<2012]
# this is all the harvesting in our modelled area
harv.spatial <- dist.yr[disturbance=="harvest" & year>1989]
harv.spatial <- harv.spatial[,c("timestep","disturbance") := NULL]
DB_Source <- rep("ALL_Spatial",22)
harv.spatial <- cbind(DB_Source,harv.spatial)
setnames(harv.spatial,names(harv.spatial),c("DB_Source","Year","ha"))

ALL.harv <- rbind(ALL.harv,harv.spatial)

g.ALL.harv <- ggplot(data=ALL.harv,aes(x=Year,y=ha/1000, group=DB_Source, fill=DB_Source)) +
         geom_bar(stat="identity",position="dodge") + ylab("Mha") +scale_fill_manual(values=c("#999999","#E69F00","#56B4E9","#009E73"))
ggsave(g.ALL.harv,file=paste(outfigs,"HarvestedHa_All.jpeg",sep=""))



