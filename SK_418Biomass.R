#----------------------------------
# SK_418 Biomass calculation for SK growth raster project
# Calculation biomass by tree for the 418 PSPs for the development of
# a growth raster
#
# Oct.22, 2014
# CBoisvenue
#----------------------------------

#1 Admin --------------
#install.packages("plyr")
require(plyr)
#install.packages("ggplot2")
require(ggplot2)
#install.packages("dplyr")
require(dplyr)
#install.packages("reshape2")
require(reshape2)
#  install.packages("gridExtra")
library(gridExtra)  

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data")

# Read-in Data-------------
sk.tree <- read.table("SK_418PlotsCleaned1.txt",sep=",")

# priority: calculate biomass from dbh and height, or from dbh alone -----------
# SK PSP documentation has height f(dbh) equations, so checking how many lines have no dbh ----------

# no height
length(which(is.na(sk.tree$HEIGHT))) #1633
#no dbh
no.dbh1 = filter(sk.tree,is.na(DBH))
no.dbh1 = unique(no.dbh1) #1610 14
neither = filter(no.dbh1,is.na(HEIGHT)) # dim 1609 14
off.plot = filter(neither,OFF_PLOT_TREE=="Y") #dim 1571 14
# don't need to calculate biomass for off plot trees but may need these trees for age later
length(which(is.na(sk.tree$agecombo))) #22982
unique(sk.tree$OFF_PLOT_TREE) #[1] <NA> N    Y   
biom.tree1 = select(sk.tree,PLOT_ID,TREE_NO,YEAR,SPECIES,OFF_PLOT_TREE) %>%
  group_by(OFF_PLOT_TREE) %>%
  summarise(n())
# check how many off-plot-tree tree could have biomass estimates
off.plot1 <- filter(off.plot,!is.na(DBH)) # NONE

biom.tree2 = filter(sk.tree, OFF_PLOT_TREE!="Y" | is.na(OFF_PLOT_TREE)) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,DBH,HEIGHT)
no.dbh2 = filter(biom.tree2,is.na(DBH))
no.dbh2 = unique(no.dbh2) #39 6
# are there any other measurements for those trees in that same year?
no.dbh3 = left_join(no.dbh2,sk.tree,by=c("PLOT_ID", "TREE_NO", "YEAR", "SPECIES")) #NO
# there is one dead tree 300092 4 2005 WS
# are there any other measurements for those trees in another year?
no.dbh4 = left_join(no.dbh2,sk.tree,by=c("PLOT_ID", "TREE_NO", "SPECIES")) #YES #65 17
# how many trees have measurements in other years?
no.dbh5 = group_by(no.dbh4,PLOT_ID,TREE_NO,SPECIES) %>%
  summarise(n())
count.no.biom = no.dbh5[no.dbh5[,4]==1,]
which(no.dbh5[,4]==1)
# Conclusion: only 20 lines have no individual tree measurements to guide biomass estimate
# calculate biomass for all trees with height and dbh by year
# lines (i.e., measurements) with info missing will be filled-in 
# 1st by measurements in other years from the same tree
# 2nd from the average for that year, for that plot, for that species
# 3rd from the average size tree in that plot in that year

# biomass for all lines with dbh and ht ----------------------

biom.tree3 = select(sk.tree,PLOT_ID,TREE_NO,YEAR,SPECIES,DBH,HEIGHT)
params = read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/BiomassEstimation/SK_SPSBiomassParametersChecked.csv",sep=",", header=TRUE)
params = params[,1:13]

biom.tree4 = left_join(biom.tree3,params) %>%
  select(DBH,HEIGHT,b1w,b2w,b3w,b1b,b2b,b3b,b1br,b2br,b3br,b1f,b2f,b3f)
# this was my 1st attempt at making a function...it did not work, there are missing values 
# in the last parameter and the function aparently does not know what to do with that
# calcbiom <- function(dbh,ht,b1w,b2w,b3w,b4w,b1b,b2b,b3b,b1br,b2br,b3br,b1f,b2f,b3f){
#   wood <- b1w*(dbh^b2w)*(ht^b3w)
#   bark <- b1b*(dbh^b2b)*(ht^b3b)
#   branches <- b1br*(dbh^b2br)*(ht^b3br)
#   foliage <- b1f*(dbh^b2f)*(ht^b3f)
#   biomass <- wood+bark+foliage+branches
#   return(wood,bark,branches,foliage,biomass)
# }
# 
# biom.tree5 = calcbiom(biom.tree4[,1],biom.tree4[,2],biom.tree4[,3],biom.tree4[,4],biom.tree4[,5],biom.tree4[,6],biom.tree4[,7],biom.tree4[,8],biom.tree4[,9],biom.tree4[,10],biom.tree4[,11],biom.tree4[,12],biom.tree4[,13],biom.tree4[,14])

biom.tree5 <- mutate(biom.tree4,wood=b1w*(DBH^b2w)*(HEIGHT^b3w),bark =b1b*(DBH^b2b)*(HEIGHT^b3b))
biom.tree5 <- mutate(biom.tree5,branches = b1br*(DBH^b2br)*(HEIGHT^b3br),foliage = b1f*(DBH^b2f)*(HEIGHT^b3f))
biom.tree5 <- mutate(biom.tree5,biomass = wood+bark+foliage+branches)
biom.tree6 <- cbind(sk.tree[,c(1:4,7:9,11:14)],biom.tree5$biomass)
names(biom.tree6) = c("PLOT_ID", "TREE_NO", "YEAR", "SPECIES","DBH","HEIGHT","TREE_STATUS","MORTALITY","INGROWTH","OFF_PLOT_TREE","agecombo","biomass")

# check biomass results ---------------
range(biom.tree6$biomass,na.rm=TRUE)#    0.000 3547.016
length(which(is.na(biom.tree6$biomass)))
# which trees are over 3000kg?
biomcheck1 <- filter(biom.tree6,biomass>=3000) 
#BP 93.7cm DBH and 19.7m high...Balsam Poplar...could be right.
ggplot(data=biom.tree6) + geom_bar(aes(biomass, fill=SPECIES),colour="black")
# there might only be one tree over 1000
biomcheck2 <- filter(biom.tree6,biomass>=1000) 
#NO - 56 lines, some of which are the same tree in different years
ggplot(data=biomcheck2) + geom_bar(aes(biomass, fill=SPECIES),colour="black")
# there are very few of these, so not impossible
biomcheck3 <- filter(biom.tree6, biomass<1000)
ggplot(data=biomcheck3) + geom_bar(aes(biomass, fill=SPECIES),colour="black")
# distribution looks ok: big hump at low numbers and long tail
unique(biom.tree6$INGROWTH)#  <NA> Y   
biomcheck4 <- filter(biom.tree6,INGROWTH=="Y")# 688
ggplot(data=biomcheck4) + geom_bar(aes(biomass, fill=SPECIES),colour="black")
# Results ok

# Fill in or sort out the 1634 NAs -----------------
# no need to calc biomass for off-plot-trees
biom.tree7 <- filter(biom.tree6, OFF_PLOT_TREE!="Y" | is.na(OFF_PLOT_TREE))
length(which(is.na(biom.tree7$biomass))) # only 57 left
biom.na <- filter(biom.tree7,is.na(biomass))
biom.fill <- filter(biom.tree7,!is.na(biomass)) %>%
  select(PLOT_ID, TREE_NO, YEAR, SPECIES,agecombo,biomass)
  
# 1 - match these to the the same tree with biomass estimates in other years ----------------
fill1 <- left_join(biom.na,biom.fill, by=c("PLOT_ID", "TREE_NO", "SPECIES")) %>%
  select(PLOT_ID, TREE_NO, SPECIES,YEAR.x, YEAR.y,agecombo.x,agecombo.y,biomass.y)%>%
  arrange(PLOT_ID, TREE_NO, SPECIES,YEAR.x, YEAR.y)
# how many have more than one measurements?
fill2 <- group_by(fill1,PLOT_ID, TREE_NO, SPECIES) %>%
  summarise(n()) 
fill2 <- fill2[which(fill2[,4]!=1),]
fill3 <- left_join(fill2,fill1) %>%
  group_by(PLOT_ID, TREE_NO, SPECIES) %>%
  summarise(max(YEAR.y),min(YEAR.y),max(biomass.y),min(biomass.y),mean(YEAR.x))
names(fill3) = c("PLOT_ID", "TREE_NO", "SPECIES","maxyr","minyr","maxbio","minbio","targetyr")
fill4 <- mutate(fill3,inc = (maxbio-minbio)/(maxyr-minyr)) 
wh = which(fill4$inc>10)
fill4$inc[wh] = fill4$inc[wh]/10

fill5 <- mutate(fill4,biomass1 = maxbio-((maxyr-targetyr)*inc)) %>%
  select(PLOT_ID, TREE_NO, SPECIES,targetyr,inc,biomass1) %>%
  arrange(PLOT_ID,TREE_NO,targetyr,SPECIES)

# need to link these to the NAs in biom.tree7###
names(fill5) = c("PLOT_ID","TREE_NO","SPECIES","YEAR","inc","biomass1")
fill.na1 <- merge(biom.tree6,fill5, all=TRUE)

# 2 - Average biomass for that year, that plot, that species -------------
#for the trees with only 1 yr of measurement
temp1 <- group_by(fill1,PLOT_ID, TREE_NO, SPECIES) %>%
  summarise(n()) 
fill6 <- temp1[which(temp1[,4]==1),]
# need the year of the above
fill7 <- left_join(fill6,biom.na) %>%
  select(PLOT_ID, TREE_NO, SPECIES,YEAR) 

fill8<- left_join(fill7,biom.fill, by=c("PLOT_ID", "YEAR", "SPECIES")) %>%
  group_by(PLOT_ID,SPECIES,YEAR) %>%
  summarise(biomass2 = mean(biomass))
#join it with the 1st biomass calculations
fill.na2 <- merge(fill.na1,fill8,all=TRUE)

# 3 - How many of the 57 are left? -----------------
wh.biom <- which(is.na(fill.na2$biomass))
fill.na2$biomass[wh.biom] = fill.na2$biomass1[wh.biom]
wh.biom1 <- which(is.na(fill.na2$biomass))
fill.na2$biomass[wh.biom1] = fill.na2$biomass2[wh.biom1]
nacheck1 <-filter(fill.na2, OFF_PLOT_TREE!="Y" | is.na(OFF_PLOT_TREE)) 
nacheck1 <- filter(nacheck1,is.na(biomass)) # 0 15
biom.all <- fill.na2[,1:12]
## got them all
write.table(biom.all,file="C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/SK418TreeBiomass.txt",sep=",")

#4 - Calculate plot level biomass by year without out-of-plot trees -------------------
plot.biom <- filter(biom.all,OFF_PLOT_TREE!="Y" | is.na(OFF_PLOT_TREE)) %>%
  group_by(PLOT_ID,YEAR) %>%
  summarise(sum(biomass))
names(plot.biom) = c("PLOT_ID","YEAR","sumbiom")
# get plot size
plot.size <-read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/BiomassEstimation/measurement_header.csv",sep=",", header=TRUE)
plot.size <- plot.size[,c(1,3)]
# biomass per ha per plot per year
biom.ha <- left_join(plot.biom,plot.size)
biom.ha = unique(biom.ha)

biom.ha1 <- mutate(biom.ha,biom.ha = sumbiom/PLOT_SIZE) %>%
  select(PLOT_ID,YEAR,PLOT_SIZE,biom.ha)
sk.raw1 <- read.table("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04/plot_header.csv", header=TRUE, sep=",")
loc.raw = select(sk.raw1, LOC_ACCURACY, PLOT_ID,contains("Z13nad83"))
loc.raw = unique(loc.raw)
# check in we have plot numbers repeated
dim(loc.raw) #[1] 2048   13
length(unique(loc.raw[,2])) # 2048 ##NO - GOOD.

biom.ha2 <- left_join(biom.ha1,loc.raw) %>%
  select(PLOT_ID,YEAR,PLOT_SIZE,biom.ha,contains("Z13"))

write.table(biom.ha2,file="C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/SK418Biomass_ha.txt",sep=",")


# # checking why some still don't have age
# temp2 <- filter(biom.all,PLOT_ID==20004) %>% group_by(YEAR) %>% summarise(n())
# temp3 <- group_by(biom.all,PLOT_ID,YEAR) %>% summarise(mean(agecombo))



