#----------------------------------
# SK_2000 Biomass calculation for SK growth raster project
# Calculation biomass by tree to get plot estimate
# Maybe of volume per plot also...
#
# May 1, 2015
# CBoisvenue
# Revised June 4th, 2015
# decision to calculate biomass with the DBH only equations to minimize erro inputs from height estimation
#
#----------------------------------

#1 Admin --------------
#install.packages("plyr")
require(plyr)
#install.packages("ggplot2")
require(ggplot2)
#install.packages("dplyr")
require(dplyr)

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/")

# Read-in Data-------------
sk.tree <- read.table("SK_2000PlotsCleaned.txt",sep=",",header=TRUE)
sk.tree <- unique(sk.tree)

# priority: calculate biomass from dbh and height, or from dbh alone -----------
# filling-in dbh

    # no height
    length(which(is.na(sk.tree$HEIGHT))) #6485
    #no dbh
    no.dbh1 = filter(sk.tree,is.na(DBH))# dim 4391    8
    neither = filter(no.dbh1,is.na(HEIGHT)) #[1] 4384    8


# get rid of all off-plot trees---------------------------
# create a dataframe of OFF_PLOT_TREE
sk.age <- read.table("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04/age_samples.csv", header=TRUE, sep=",")
off.plot = filter(sk.age,OFF_PLOT_TREE=="Y") %>%#dim 1571 14
  select(PLOT_ID,TREE_NO,YEAR,OFF_PLOT_TREE)
off.plot=unique(off.plot)

off.plot1 <- left_join(sk.tree,off.plot) 

biom1 <- filter(off.plot1,is.na(OFF_PLOT_TREE))
table(biom1$SPECIES)
# BF     BP     BS     JP     MM     TA     TL     WB     WS 
# 9838  11751 139008  75007      3 146864   1478  10317 137488 
# no more off-plot trees---------------------------------

# dbh check - how many with no dbh? Can I fill them? ------------------------------------------
no.dbh2 = filter(biom1,is.na(DBH))
dim(no.dbh2) #[1]4391    9 - same as for the sk.tree
# what species?
no.dbh.spsNA <- filter(no.dbh2,is.na(SPECIES))
dim(no.dbh.spsNA)#[1] 4268    9
# check for species in 
no.dbh.sps <-  filter(no.dbh2,!is.na(SPECIES))
dim(no.dbh.sps)#[1] 123   9

# there seems to be a missing species issue also -------------
  table(biom1$SPECIES)
  # BF     BP     BS     JP     MM     TA     TL     WB     WS 
  # 9838  11751 139008  75007      3 146864   1478  10317 137488 
  spsNA <- filter(biom1, is.na(SPECIES)) #42361     9
  # are there species identified for that same tree in other years?
  spscheck1 <- inner_join(spsNA,biom1,by=c("PLOT_ID","TREE_NO"))
  dim(spscheck1)#[1] 92526    16
  # Yes
  # all?
  spsNA1 <- group_by(spsNA,PLOT_ID, TREE_NO, YEAR) %>%
    summarise(n=n())
  dim(spsNA1)# 42361     5
  spscheck2 <- filter(spscheck1,!is.na(SPECIES.y)) %>%
    select(PLOT_ID, TREE_NO,YEAR=YEAR.x,-SPECIES.x,-DBH.x,-HEIGHT.x,-INGROWTH.x,-age.x,-OFF_PLOT_TREE.x,-YEAR.y,
           SPECIES=SPECIES.y,-DBH.y,-HEIGHT.y,-INGROWTH.y,-age.y,-OFF_PLOT_TREE.y)
  dim(spscheck2)
  # more than one species per plot, tree?
  spscheck3 <- group_by(spscheck2, PLOT_ID, TREE_NO) %>%
    summarise(sps.no=n()) %>%
    filter(sps.no>1)
  dim(spscheck3)#[1] 10473     3
  # yes 23%, select the 1st species
  spscheck4 <- group_by(spscheck2,PLOT_ID,TREE_NO) %>%
    summarize(SPECIES=first(SPECIES))
  add.sps <- left_join(biom1,spscheck4,by=c("PLOT_ID","TREE_NO"))
  SPECIES <- add.sps$SPECIES.x
  SPECIES[is.na(add.sps$SPECIES.x)] = add.sps$SPECIES.y[is.na(add.sps$SPECIES.x)]
  length(which(is.na(SPECIES))) #[1] 9597
  biom2 <- cbind(biom1[,-4],SPECIES)
# Species fill-in with dom sps-----------------
plots.no.sps <- filter(biom2, is.na(SPECIES)) %>%
  group_by(PLOT_ID) %>% summarise(count=n())
dim(plots.no.sps)#[1] 600   2  
# calculate the dominant species for each plot for each year of measurement
# this is per demographics
dom1 <- group_by(biom2,PLOT_ID,YEAR) %>%
  summarise(totaltrees=n())
dom2 <- group_by(biom2,PLOT_ID,YEAR,SPECIES) %>%
  summarise(spstrees=n())
dom3 <- left_join(dom2,dom1) %>%
  mutate(prop = spstrees/totaltrees) %>%
  arrange(PLOT_ID,YEAR,prop) %>%
  group_by(PLOT_ID,YEAR) %>%
  summarise(dom=last(SPECIES))
dom3.NA <- filter(dom3,is.na(dom))
# 5 plots

biom3 <- left_join(biom2,dom3) 
SPECIES <- biom3$SPECIES
SPECIES[is.na(biom3$SPECIES)] = biom3$dom[is.na(biom3$SPECIES)]
biom4 <- cbind(biom3[,-9],SPECIES)
#199 lines with no sps - why?
spsNA2 <- filter(biom4,is.na(SPECIES))
NA.plots <- as.data.frame(unique(spsNA2$PLOT_ID))
names(NA.plots)="PLOT_ID"
# check-out these plots
spsNA3 <- inner_join(biom4,NA.plots)
NA.1 <- filter(spsNA3, PLOT_ID==222213)# all NAs for this plot will be replaced by JP - do it in dom3
NA.2 <- filter(spsNA3, PLOT_ID==300311) # all NAs in this plot will be replaced by WS
NA.3 <- filter(spsNA3, PLOT_ID==322325) # all NAs in this plot will be replaced by WS
NA.4 <- filter(spsNA3, PLOT_ID==910033) # all NAs in this plot will be replaced by JP
NA.5 <- filter(spsNA3, PLOT_ID==910075) # all NAs in this plot will be replaced by JP
dom4 <- dom3
dom4[dom3$PLOT_ID==222213,3] <- "JP"
dom4[dom3$PLOT_ID==300311,3] <- "WS"
dom4[dom3$PLOT_ID==322325,3] <- "WS"
dom4[dom3$PLOT_ID==910033,3] <- "JP"
dom4[dom3$PLOT_ID==910075,3] <- "JP"
biom5 <- left_join(biom2,dom4) 
SPECIES <- biom5$SPECIES
SPECIES[is.na(biom5$SPECIES)] = biom5$dom[is.na(biom5$SPECIES)]
biom6 <- cbind(biom5[,-9],SPECIES)
dim(filter(biom6,is.na(SPECIES)))
# End of sepcies fill-in-----------------------
# No more missing species------------------------------------

# are there any other dbh measurements for those trees in that same year?--
no.dbh3 <- filter(biom6,is.na(DBH))
dbh.0 <- filter(biom6,DBH==0)
no.dbh.0 = rbind(no.dbh3,dbh.0)
no.dbh.trees = select(no.dbh.0,PLOT_ID,TREE_NO,YEAR)
no.dbh.trees = unique(no.dbh.trees)  
no.dbh.trees1 <- left_join(no.dbh.trees,biom6)
hist(no.dbh.trees1$age) # ages between 60-120
# NO----------------------------------------------------------------------

# assign the avg dbh for that species on that plot for that year-----
dbh1 <- group_by(biom6,PLOT_ID,YEAR,SPECIES) %>%
  summarise(dbh.bar = mean(DBH,na.rm=TRUE))
dbh2 <- group_by(biom6,PLOT_ID,YEAR) %>%
  summarise(dbh.bar2 = mean(DBH,na.rm=TRUE))
dbh3 <- inner_join(dbh1,dbh2)
DBH.bar <- dbh3$dbh.bar
DBH.bar[which(is.na(dbh3$dbh.bar))] <- dbh3$dbh.bar2[which(is.na(dbh3$dbh.bar))]
# still have 0s...
dbh4 <- filter(dbh3,dbh.bar==0)
dbh5 <- inner_join(dbh4,biom6)
# ok, all the dbh.bar = 0 lines have 80+ ages, so replace the DBH.bar with dbh.bar2
DBH.bar[which(dbh3$dbh.bar==0)] <- dbh3$dbh.bar2[which(dbh3$dbh.bar==0)]
dbh6 <- cbind(dbh3,DBH.bar)
dbh7 <- select(dbh6,-dbh.bar,-dbh.bar2)

dbh.add <- left_join(no.dbh.0,dbh7) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,DBH.bar)

biom7 <- left_join(biom6,dbh.add)
biom7$DBH[which(is.na(biom7$DBH))] <- biom7$DBH.bar[which(is.na(biom7$DBH))]

biom7.1 <- filter(biom7,DBH==0)
biom7$DBH[which(biom7$DBH==0)] <- biom7$DBH.bar[which(biom7$DBH==0)]

# How many have dbh=0?
dbh.0 <- filter(biom7,DBH==0)
# 0
biom8 <- select(biom7,-OFF_PLOT_TREE,-DBH.bar)
# End of DBH fill-in, all lines have a dbh--------------------------------------------------------

# HEIGHTS ----------------------------------------------------------------------------------------
#how many heights missing?
no.ht <- filter(biom8, is.na(HEIGHT))
dim(no.ht)# 6475    9 - so 1.127823% of the total no of measurements

# Calculating heights for each tree using the SK ht-diam model form and estimated parameters------
ht.models <- read.table("G:/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/height_dbh_models.csv",header=TRUE, sep=",")

ht.params <- mutate(ht.models,beta1=BETA1,beta2=BETA2,beta3=BETA3) %>%
  select(-BETA1,-BETA2,-BETA3)
ht.calc <- left_join(biom8,ht.params) %>% mutate(dbh=DBH) %>% select(-DBH) 
ht.calc2 <- mutate(ht.calc,step1 = 1.3+beta1*(1-exp(-1*beta2*dbh))^beta3,step2=(1-exp(-1*beta2*dbh))^beta3,
                   step3 = beta1*log(1-exp(-1*beta2*dbh))*((1-exp(-1*beta2*dbh))^beta3),
                   step4 = step1+b1*step2+b3*step3, compare.ht = HEIGHT-round(step4)) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,dbh,HEIGHT,age,dom,step4,compare.ht)
ht.na <- filter(ht.calc2, is.na(HEIGHT)) # 6475 10
ht.calc2$HEIGHT[which(is.na(ht.calc$HEIGHT))] <- ht.calc2$step4[which(is.na(ht.calc$HEIGHT))]
ht.na <- filter(ht.calc2, is.na(HEIGHT)) # 1 10
# plot 300105 tree 17
ht.na1 <- filter(ht.calc2,PLOT_ID==300105,TREE_NO==17) # one tree
#
ht.na2 <- filter(ht.params,PLOT_ID==300105)
ht.na3 <- filter(biom8,PLOT_ID==300105,TREE_NO==17)
# THis is why this height did not get calculated: there are no BP in that plot to build a height model
ht.BP <- filter(ht.params,SPECIES=="BP")
no.ht.BP.plots <- unique(ht.BP$PLOT_ID)
# going to use the parameters from another plots #300106
BP.dbh = 27.24283
ht.BP.calc1 <- filter(ht.params,PLOT_ID==300106,SPECIES=="BP") %>%
  mutate(step1 = 1.3+beta1*(1-exp(-1*beta2*BP.dbh))^beta3,step2=(1-exp(-1*beta2*BP.dbh))^beta3,
         step3 = beta1*log(1-exp(-1*beta2*BP.dbh))*((1-exp(-1*beta2*BP.dbh))^beta3),
         BP.ht = step1+b1*step2+b3*step3) 
ht.calc2$HEIGHT[(ht.calc2$PLOT_ID==300105 &ht.calc2$TREE_NO==17)] <- ht.BP.calc1$BP.ht
ht.na <- filter(ht.calc2, is.na(HEIGHT)) # 1 10
# END of calculating heights --------------------------------------------------------------------------

# these should be equal...final check
biom9 <- select(ht.calc2,PLOT_ID,TREE_NO,YEAR,SPECIES,DBH=dbh,HEIGHT,age,dom)

## NOTE: checked on June 23rd, 2015 - this data frame mathces the saved one
write.table(biom9,file="C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/SK_2000TreeMeasurements.csv",sep=",")
###############IMPORTANT DATA FRAME
######### This dataframe has all the info to calculate biomass or volume per tree

# Biomass calculations -------------------------------------------------------------------------
# Calculate biomass for all trees in data frame-------------------------------------------------
# calculate biomass for all trees with dbh by year
params1 <- read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/BiomassEstimation/SK_SPSBiomassParametersChecked.csv",sep=",", header=TRUE)
params2 <- read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/BiomassEstimation/SK_SPSBiomassParametersCheckedDBH_only.csv",sep=",", header=TRUE)
params2 <- params2[1:9,1:9]

# biomass
biom.tree1 <- left_join(biom9,params2) 

biom.dbh <- mutate(biom.tree1,wood.dbh=b1w*(DBH^b2w),bark.dbh =b1b*(DBH^b2b),
                     branches.dbh = b1br*(DBH^b2br),foliage.dbh = b1f*(DBH^b2f),
                     biom.dbh = wood.dbh+bark.dbh+foliage.dbh+branches.dbh) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,HEIGHT,age,DBH,wood.dbh,bark.dbh,branches.dbh,foliage.dbh,biom.dbh)

biom.tree2 <- left_join(biom9,params1) 

biom.tree3 <- mutate(biom.tree2,wood=b1w*(DBH^b2w)*(HEIGHT^b3w),bark =b1b*(DBH^b2b)*(HEIGHT^b3b),
                     branches = b1br*(DBH^b2br)*(HEIGHT^b3br),foliage = b1f*(DBH^b2f)*(HEIGHT^b3f),
                     biomass = wood+bark+foliage+branches) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,dom, HEIGHT,age,DBH,wood,bark,branches,foliage,biomass)

biom.tree4 <- inner_join(biom.tree3,biom.dbh)

write.table(biom.tree4,file="C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/SK_2000TreeBiomass.csv",sep=",",row.names = FALSE)

# biomass from these equations are in kg
ggplot(data=biom.tree4) + geom_bar(aes(biomass, fill=SPECIES),colour="black")
# there might only be one tree over 1000
biomcheck1 <- filter(biom.tree4,biomass>=1000) 
ggplot(data=biomcheck1) + geom_bar(aes(biomass, fill=SPECIES),colour="black")
dim(biomcheck1)#[1] 460  12
biomcheck2 <- filter(biom.tree4, biomass<1000)
ggplot(data=biomcheck2) + geom_bar(aes(biomass, fill=SPECIES),colour="black")
# distribution looks ok
# Results ok
# End of calculating biomass per tree--------------------------------------------------------------


#Calculate plot level biomass by year -------------------
plot.biom <- group_by(biom.tree4,PLOT_ID,YEAR) %>%
  summarise(sumbiom=sum(biomass))

# get plot size
plot.size <-read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/BiomassEstimation/measurement_header.csv",sep=",", header=TRUE)
plot.size <- plot.size[,c(1,3,16,20)]
      checksize <- group_by(plot.size,PLOT_ID) %>%
        summarise(mean=mean(PLOT_SIZE))
      checksize2 <-  left_join(plot.size,checksize)
      sizediff <- mutate(checksize2,diff=PLOT_SIZE-mean)
      range(sizediff$diff)
      # plot size does not change over time
      ecocheck <- group_by(plot.size,PLOT_ID) %>%
        summarise(neco=n()) %>%
        filter(neco>1)
      ecocheck <- unique(ecocheck)
      #there are 1378 plots with more then one ecosite

plot.size1 <- unique(plot.size[,c(1,2)])
#for Sasha
#write.table(plot.size1,file="G:/RES_Work/Work/Analysis_PSPs/SashaHararuk/plotSize.txt",sep=",",row.names = FALSE)

# biomass per ha per plot per year, divide by 1000 to have the units in t/ha or Mg/ha (those are equivalent)
biom.ha <- left_join(plot.biom,plot.size1) %>%
  mutate(biom.ha = (sumbiom/PLOT_SIZE)/1000) %>%
  select(PLOT_ID,YEAR,biom.ha)

write.table(biom.ha,file="C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/SK_2000Biomass_ha.txt",sep=",",row.names=FALSE)



