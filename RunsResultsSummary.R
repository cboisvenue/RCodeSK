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

