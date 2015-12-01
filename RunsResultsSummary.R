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
age10 <- fread(paste(indir,"age_binned_10.csv",sep=""),sep=",",header=TRUE)
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


# disturbances per year  ----------------------------
distsums <- fread(paste(indir,"DistSums.csv",sep=""),sep=",",header=TRUE)
# this in wide form, I need short form
# I want the names as opposed to the numbers - this list is in order with
# increasing integers 0,1,3,168,234
distnames <- c("not distrubed","fire","harvest","mortality 20%","deforestation")
distsums <- cbind(distsums,distnames)
dist.yr <- melt(distsums, id.vars = c("DistType","distnames"),
                variable.name = "yr.ch", value.name = "ha")
year <- sort(rep(1984:2012,5))
dist.yr <- cbind(year,dist.yr)
# get rid of redondant cols and re-order 
dist1 <- dist.yr[,c("DistType","yr.ch"):=NULL]
setcolorder(dist1,c("distnames","year","ha"))

dist <- dist1[distnames %in% c("fire","harvest","mortality 20%","deforestation")]

g2 <- ggplot(data=dist,aes(x=year,y=ha/1000,group=distnames,fill=distnames)) + 
  geom_bar(stat="identity") + ylab("1000ha")

# growth curves-------------------------------------------

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

# Growth curves-----------------------------------------------------------
growth1 <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/MEMPredictedGrowth_noTAG.txt",sep=",",header=TRUE)
# changing the names of the strata
library(plyr)
strata <- revalue(growth1$stratum,c("BSG"="BSGood","BSM"="BSMedium","TAM"="TA","WSG"="WSGood","WSM"="WSMedium"))
growth <- cbind(growth1[,stratum:= NULL],strata)
g3 <- ggplot(data=growth,aes(x=plot.age,y=memT.notagyhat,group=strata,colour=strata)) + 
  geom_line(size=1)
 g3 + theme(legend.position=c(0.1,0.65))
+  scale_fill_brewer(palette="Spectral")
# end of growth curves
 
 


#HERE
# carbon values are in metric tonnes/megagrams of C 
stocks <- fread(paste(indir,"totalE.csv",sep=""),sep=",",header=TRUE)
setnames(stocks,names(stocks),c("Year","gC"))
agbiom <- fread(paste(indir,"agbio.csv",sep=""),sep=",",header=TRUE)
setnames(agbiom,names(agbiom),c("Year","g"))

nbp <- fread(paste(indir,"NBP.csv",sep=""),sep=",",header=TRUE)
nep <- fread(paste(indir,"NEP.csv",sep=""),sep=",",header=TRUE)
range(nbp)
#[1] -6428598  1869711
range(nep)
#[1]       0 1925205


# THIS FILE BELLOW IS NOT CORRECT WAIT FOR BYRON TO UPDATE FILE
areaDist <- fread(paste(indir,"DistArea.csv",sep=""),sep=",",header=TRUE)
range(areaDist$DistArea)

# graph of these through time
g.stocks <- ggplot(data=stocks,aes(x=Year,y=gC/1000000,group=1)) + geom_line(size=1)
g.stocks +ggtitle("Total Ecosystem Carbon")
g.nbp <- ggplot(data=nbp,aes(x=Year,y=NBP/1000000,group=1)) + geom_line(size=1,colour="red") + geom_hline()
g.nbp +ggtitle("Total Ecosystem Yearly Flux") +
    geom_line(data=nep,aes(x=Year,y=NEP/1000000,group=1))

g.abgbiom <- ggplot(data=agbiom,aes(x=Year,y=g/2000000,group=1)) + geom_line(colour="green",size=1)

# NOT WORKIGN B/C of scale issues
g.dist <- ggplot(data=areaDist,aes(x=Year,y=areaDist,group=1)) + geom_bar()

