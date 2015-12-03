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

g2 <- ggplot(data=dist.yr,aes(x=year,y=ha/1000,group=disturbance,fill=disturbance)) + 
  geom_bar(stat="identity") + ylab("1000ha") + theme(legend.position=c(0.1,0.65))
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
library(plyr)
strata <- revalue(growth1$stratum,c("BSG"="BSGood","BSM"="BSMedium","TAM"="TA","WSG"="WSGood","WSM"="WSMedium"))
growth <- cbind(growth1[,stratum:= NULL],strata)
g3 <- ggplot(data=growth,aes(x=plot.age,y=memT.notagyhat,group=strata,colour=strata)) + 
  geom_line(size=1)
g3 + theme(legend.position=c(0.1,0.65))
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

# calculate total emissions from dist
tot.em <- dist.em[,.(tot.CO2 = sum(`Total CO2`), tot.CO = sum(`Total CO`),tot.CH4 = sum(`Total CH4`)), by=`year`]
tot<- tot.em[,.(tot = (tot.CO2+tot.CO+tot.CH4))]
tot.em <- cbind(tot.em,tot)
CO2 <- mean(tot.em[,(CO2 = (tot.CO2/tot)*100)]) # 90%
CO <- mean(tot.em[,(CO = (tot.CO/tot)*100)]) # 9%
CH4 <- mean(tot.em[,(CH4 = (tot.CH4/tot)*100)]) # 1%
Gas <- c("CO2","CO","CH4")
percent <- c(round(CO2),round(CO),round(CH4))
gas.prop <- as.data.table(cbind(Gas,percent))
  
# tot.em1 <- melt(tot.em,id.vars = c("year"),
#                 variable.name = "gas", value.name = "MgC")
library(gridExtra)
g.emissions.tot <- ggplot(data=tot.em,aes(x=year,y=tot)) +
  geom_line(size=1.2) 
