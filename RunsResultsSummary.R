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
age1984 <- age10[year==1984]
age1984[,group := (as.numeric(group)+5)]
age1stLast[,group := (as.numeric(group)+5)]

ggplot(age1stLast,aes(x=group,y=ha/1000000,group=year,fill=year,colour=year)) + 
  geom_bar(stat="identity",position="dodge",show_guide=FALSE) +
  xlab("10-year Age Classes") + ylab("Mha") 

# still need to figure out how to get the proper legend
+ scale_fill_identity(name = 'Year', guide = 'legend',labels = as.character(c('1984','2012')))


theme(legend.position=c(0.85,0.8))
+
  
+
  scale_color_discrete(breaks=as.factor(C("1984","2012")))

+ scale_fill_continuous(name="Year",breaks=c("1984","2012"))







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
dist <- dist.yr[,c("DistType","yr.ch"):=NULL]
setcolorder(dist,c("distnames","year","ha"))






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

