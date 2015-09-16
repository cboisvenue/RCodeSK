#------------------------------------------------------------------------------------
# Building a delta biomass per hectare per year model for SK based pn PSP data
# Goals:
# 1) start with plot level biom/ha
# 2) attach the "strata" info (casfri soil_moist/prodClass, and dom species)
# 3) calculate yearly per ha increments 
# 4) evalute grafically 
# 5) Save file
# 6) need to go another way? per tree then per plot like volume?
#
# CBoisvenue
# July 2nd, 2015
#------------------------------------------------------------------------------------

#library(nlme)
library(ggplot2)
library(RColorBrewer)
require(plyr)
require(dplyr)
library(tidyr)

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/")

# read-in base file---------------------------------------------
#t/ha or Mg/ha (those are equivalent)
plot.biom <-read.table("SK_2000Biomass_ha.txt",header=TRUE,sep=",")
# this is for the strata-----------
casfri <- read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/casfri.txt",sep=",", header=TRUE)
soil <- select(casfri, PLOT_ID=plot_id, soil_moist_reg) 
soil$soil_moist_reg <- revalue(soil$soil_moist_reg,c("-1111" = "M","A"="P","D"="M","F"="G","M"="G","W"="M"))
soil$soil_moist_reg[which(is.na(soil$soil_moist_reg))] <- "M"
tree.meas <- read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/SK_2000TreeMEasurements.csv",sep=",", header=TRUE)
dom.sps <-group_by(tree.meas,PLOT_ID) %>%
  summarise(dom=first(SPECIES))
# there are not enough plots with leaving TL so they ae lumped-in with the BF
dom.sps$dom[which(dom.sps$dom=="TL")] <- "BF"

strata <- inner_join(soil,dom.sps)

stratum <- (as.numeric(strata$dom)*10)+(as.numeric(strata$soil_moist_reg))
stratum[which(stratum<15)] <- "BF"
stratum[which(stratum<25 & stratum>15)] <- "BP"
stratum[which(stratum>25 & stratum<33)] <- "BSM"
stratum[which(stratum==33)] <- "BSG"
stratum[which(stratum>40 &stratum<60)] <- "JP"
stratum[which(stratum>60 &stratum<63)] <- "TAM"
stratum[which(stratum==63)] <- "TAG"
stratum[which(stratum>65 &stratum<85)] <- "WB"
stratum[which(stratum>90 &stratum<93)] <- "WSM"
stratum[which(stratum==93)] <- "WSG"
strata <- cbind(strata, stratum)
# strata done----------------------

# age---------------------------
age.plot <- group_by(tree.meas,PLOT_ID,YEAR) %>%
  summarise(age=mean(age))
# end age-----------------------

check1 <- inner_join(plot.biom,age.plot)
plot.check <- ggplot(data=check1, aes(x=age,y=biom.ha)) + geom_point()
plot.check + ggtitle("Plot-level biomass per hectare estiamtes t/ha")
ggsave(plot.check,file="G:/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/BiomassHaModelAssessment/PlotBiomass_t_ha.jpeg")
### These estimates seem really high.....

# End of reading-in files---------------------------------------

# How many measurements per plot?----------------------------------
meas.plot <- group_by(plot.biom, PLOT_ID) %>%
  summarize(count=n())

table(meas.plot$count)
# 1   2   3   4   5   6   7   8 
# 457 365 153 196  59  82  55  14 
#------------------------------------------------------------------


# Calculate the delta for years, biom.ha, and track ages per plot-------------------------
yr.meas <- matrix(nrow=dim(meas.plot)[[1]],ncol = 8)
biom.meas <- matrix(nrow=dim(meas.plot)[[1]],ncol = 8)
age.table <- matrix(nrow=dim(meas.plot)[[1]],ncol = 8)

age1 <- group_by(age.plot,PLOT_ID) %>%
  summarise(age1=first(age),age.last=last(age))

table1 <- group_by(plot.biom,PLOT_ID) %>%
  summarise(col1 = first(YEAR), col.last = last(YEAR),meas1=first(biom.ha),
            meas.last=last(biom.ha)) %>%
  inner_join(meas.plot) %>%
  inner_join(age1) %>%
  arrange(PLOT_ID,col1)


# dealing with the 1st and last measurements of each plot--------------
yr.meas[,1] <- table1$col1
yr.meas[which(table1$count<3),2] <- table1$col.last[which(table1$count<3)]
yr.meas[which(table1$count==3),3] <- table1$col.last[which(table1$count==3)]
yr.meas[which(table1$count==4),4] <- table1$col.last[which(table1$count==4)]
yr.meas[which(table1$count==5),5] <- table1$col.last[which(table1$count==5)]
yr.meas[which(table1$count==6),6] <- table1$col.last[which(table1$count==6)]
yr.meas[which(table1$count==7),7] <- table1$col.last[which(table1$count==7)]
yr.meas[which(table1$count==8),8] <- table1$col.last[which(table1$count==8)]

biom.meas[,1] <- table1$meas1
biom.meas[which(table1$count<3),2] <- table1$meas.last[which(table1$count<3)]
biom.meas[which(table1$count==3),3] <- table1$meas.last[which(table1$count==3)]
biom.meas[which(table1$count==4),4] <- table1$meas.last[which(table1$count==4)]
biom.meas[which(table1$count==5),5] <- table1$meas.last[which(table1$count==5)]
biom.meas[which(table1$count==6),6] <- table1$meas.last[which(table1$count==6)]
biom.meas[which(table1$count==7),7] <- table1$meas.last[which(table1$count==7)]
biom.meas[which(table1$count==8),8] <- table1$meas.last[which(table1$count==8)]

age.table[,1] <- table1$age1
age.table[which(table1$count<3),2] <- table1$age.last[which(table1$count<3)]
age.table[which(table1$count==3),3] <- table1$age.last[which(table1$count==3)]
age.table[which(table1$count==4),4] <- table1$age.last[which(table1$count==4)]
age.table[which(table1$count==5),5] <- table1$age.last[which(table1$count==5)]
age.table[which(table1$count==6),6] <- table1$age.last[which(table1$count==6)]
age.table[which(table1$count==7),7] <- table1$age.last[which(table1$count==7)]
age.table[which(table1$count==8),8] <- table1$age.last[which(table1$count==8)]
# 1st and last done---------------------------------------------------------


# Three measurements, middle one----------------------
table2 <- inner_join(plot.biom,table1) %>%
  inner_join(age.plot) %>%
  arrange(PLOT_ID, YEAR) %>%
  filter(YEAR!=col1 & YEAR!=col.last)

yr.meas[which(table1$count==3),2] <- table2$YEAR[which(table2$count==3)]
biom.meas[which(table1$count==3),2] <- table2$biom.ha[which(table2$count==3)]
age.table[which(table1$count==3),2] <- table2$age[which(table2$count==3)]
# Three measurements middle one done-----------------

# 2nd measurements for all with >3; for 4th measument, 3rd, for 5th, 4th, for 6th, 5th,
# for 7th, 6th, and for 8th, 7th-------------------------------------------------------
table3 <- filter(table2,count>3) %>%
  group_by(PLOT_ID) %>%
  summarise(col2=first(YEAR),col.last2=last(YEAR),
            biom2=first(biom.ha),biom.last2=last(biom.ha),
            age2=first(age),age.last2=last(age)) %>%
  inner_join(meas.plot)


yr.meas[which(table1$count==4),2] <- table3$col2[which(table3$count==4)]
yr.meas[which(table1$count==4),3] <- table3$col.last2[which(table3$count==4)]
yr.meas[which(table1$count==5),2] <- table3$col2[which(table3$count==5)]
yr.meas[which(table1$count==5),4] <- table3$col.last2[which(table3$count==5)]
yr.meas[which(table1$count==6),2] <- table3$col2[which(table3$count==6)]
yr.meas[which(table1$count==6),5] <- table3$col.last2[which(table3$count==6)]
yr.meas[which(table1$count==7),2] <- table3$col2[which(table3$count==7)]
yr.meas[which(table1$count==7),6] <- table3$col.last2[which(table3$count==7)]
yr.meas[which(table1$count==8),2] <- table3$col2[which(table3$count==8)]
yr.meas[which(table1$count==8),7] <- table3$col.last2[which(table3$count==8)]

biom.meas[which(table1$count==4),2] <- table3$biom2[which(table3$count==4)]
biom.meas[which(table1$count==4),3] <- table3$biom.last2[which(table3$count==4)]
biom.meas[which(table1$count==5),2] <- table3$biom2[which(table3$count==5)]
biom.meas[which(table1$count==5),4] <- table3$biom.last2[which(table3$count==5)]
biom.meas[which(table1$count==6),2] <- table3$biom2[which(table3$count==6)]
biom.meas[which(table1$count==6),5] <- table3$biom.last2[which(table3$count==6)]
biom.meas[which(table1$count==7),2] <- table3$biom2[which(table3$count==7)]
biom.meas[which(table1$count==7),6] <- table3$biom.last2[which(table3$count==7)]
biom.meas[which(table1$count==8),2] <- table3$biom2[which(table3$count==8)]
biom.meas[which(table1$count==8),7] <- table3$biom.last2[which(table3$count==8)]

age.table[which(table1$count==4),2] <- table3$age2[which(table3$count==4)]
age.table[which(table1$count==4),3] <- table3$age.last2[which(table3$count==4)]
age.table[which(table1$count==5),2] <- table3$age2[which(table3$count==5)]
age.table[which(table1$count==5),4] <- table3$age.last2[which(table3$count==5)]
age.table[which(table1$count==6),2] <- table3$age2[which(table3$count==6)]
age.table[which(table1$count==6),5] <- table3$age.last2[which(table3$count==6)]
age.table[which(table1$count==7),2] <- table3$age2[which(table3$count==7)]
age.table[which(table1$count==7),6] <- table3$age.last2[which(table3$count==7)]
age.table[which(table1$count==8),2] <- table3$age2[which(table3$count==8)]
age.table[which(table1$count==8),7] <- table3$age.last2[which(table3$count==8)]
#all with <4 DOne------------------------------------------------------------------

# greater than 4 measurements --------------------------------------------------
# for 5th, 3rd, for 6th, 3rd and 4th, for 7th, 3rd and 5th, for 8 3rd and 6th---

table4 <- inner_join(plot.biom,table1) %>%
  inner_join(age.plot) %>%
  inner_join(table3) %>%
  arrange(PLOT_ID, YEAR) %>%
  filter(YEAR!=col1 & YEAR!=col2 & YEAR!=col.last & YEAR!=col.last2)

yr.meas[which(table1$count==5),3] <- table4$YEAR[which(table4$count==5)]
biom.meas[which(table1$count==5),3] <- table4$biom.ha[which(table4$count==5)]
age.table[which(table1$count==5),3] <- table4$age[which(table4$count==5)]

table5 <- filter(table4,count>5) %>%
  group_by(PLOT_ID) %>%
  summarise(col3=first(YEAR),col.last3=last(YEAR),
            biom3=first(biom.ha),biom.last3=last(biom.ha),
            age3=first(age),age.last3=last(age)) %>%
  inner_join(meas.plot)

yr.meas[which(table1$count==6),3] <- table5$col3[which(table5$count==6)]
yr.meas[which(table1$count==6),4] <- table5$col.last3[which(table5$count==6)]
yr.meas[which(table1$count==7),3] <- table5$col3[which(table5$count==7)]
yr.meas[which(table1$count==7),5] <- table5$col.last3[which(table5$count==7)]
yr.meas[which(table1$count==8),3] <- table5$col3[which(table5$count==8)]
yr.meas[which(table1$count==8),6] <- table5$col.last3[which(table5$count==8)]

biom.meas[which(table1$count==6),3] <- table5$biom3[which(table5$count==6)]
biom.meas[which(table1$count==6),4] <- table5$biom.last3[which(table5$count==6)]
biom.meas[which(table1$count==7),3] <- table5$biom3[which(table5$count==7)]
biom.meas[which(table1$count==7),5] <- table5$biom.last3[which(table5$count==7)]
biom.meas[which(table1$count==8),3] <- table5$biom3[which(table5$count==8)]
biom.meas[which(table1$count==8),6] <- table5$biom.last3[which(table5$count==8)]

age.table[which(table1$count==6),3] <- table5$age3[which(table5$count==6)]
age.table[which(table1$count==6),4] <- table5$age.last3[which(table5$count==6)]
age.table[which(table1$count==7),3] <- table5$age3[which(table5$count==7)]
age.table[which(table1$count==7),5] <- table5$age.last3[which(table5$count==7)]
age.table[which(table1$count==8),3] <- table5$age3[which(table5$count==8)]
age.table[which(table1$count==8),6] <- table5$age.last3[which(table5$count==8)]
# end greater than 4 measurements --------------------------------------------------

# greater than 5 measurements --------------------------------------------------
# for 7th, 4th, for 8th, 4th and 5th
table6 <- inner_join(plot.biom,table1) %>%
  inner_join(age.plot) %>%
  inner_join(table3) %>%
  inner_join(table5) %>%
  arrange(PLOT_ID, YEAR) %>%
  filter(YEAR!=col1 & YEAR!=col2 & YEAR!=col3 & YEAR!=col.last & YEAR!=col.last2 & YEAR!=col.last3)

yr.meas[which(table1$count==7),4] <- table6$YEAR[which(table6$count==7)]
biom.meas[which(table1$count==7),4] <- table6$biom.ha[which(table6$count==7)]
age.table[which(table1$count==7),4] <- table6$age[which(table6$count==7)]

table7 <- filter(table6,count>7) %>%
  group_by(PLOT_ID) %>%
  summarise(col4=first(YEAR),col.last4=last(YEAR),
            biom4=first(biom.ha),biom.last4=last(biom.ha),
            age4=first(age),age.last4=last(age)) %>%
  inner_join(meas.plot)

yr.meas[which(table1$count==8),4] <- table7$col4[which(table7$count==8)]
yr.meas[which(table1$count==8),5] <- table7$col.last4[which(table7$count==8)]
biom.meas[which(table1$count==8),4] <- table7$biom4[which(table7$count==8)]
biom.meas[which(table1$count==8),5] <- table7$biom.last4[which(table7$count==8)]
age.table[which(table1$count==8),4] <- table7$age4[which(table7$count==8)]
age.table[which(table1$count==8),5] <- table7$age.last4[which(table7$count==8)]
# end of greater than 5 measurements -----------------------------------------

# put all other values to 0 -----------------------------
yr.meas[is.na(yr.meas)] <- 0
biom.meas[is.na(biom.meas)] <- 0
age.table[is.na(age.table)] <- 0
# end of 0 ----------------------------------------------

# calculating differences -------------------------------------
yr.df <- matrix(nrow=dim(yr.meas)[[1]], ncol=dim(yr.meas)[[2]]-1)
biom.df <- matrix(nrow=dim(biom.meas)[[1]], ncol=dim(biom.meas)[[2]]-1)
for(i in 1:7){
  yr.df[,i] = yr.meas[,i+1] - yr.meas[,i]
  biom.df[,i]=biom.meas[,i+1] - biom.meas[,i]
}

yr.df[which(yr.df<1)] <- NA
biom.df[which(biom.df<1)] <- NA

delta.t.ha <- biom.df/yr.df

biom.inc <-cbind(table1[,1],delta.t.ha)
names(biom.inc) = c("PLOT_ID","inc1","inc2","inc3","inc4","inc5","inc6","inc7")
biom.inc2 <- biom.inc %>%
  gather(meas.no,inc,-PLOT_ID) %>%
  arrange(PLOT_ID)

biom.age <-cbind(table1[,1],age.table[,1:7])
names(biom.age) = c("PLOT_ID","age1","age2","age3","age4","age5","age6","age7")
biom.age2 <- biom.age %>%
  gather(ageno,age,-PLOT_ID) %>%
  arrange(PLOT_ID)

biom.data1 <- cbind(biom.inc2,biom.age2[,3])
biom.data2 <- biom.data1[-which(is.na(biom.data1$inc)),]
biom.data <- left_join(biom.data2,strata) 
names(biom.data) = c("PLOT_ID","meas.no","inc","age","soil_moist_reg","dom","stratum")
  
write.table(biom.data,file="t_haBiom_yr.txt",row.names=FALSE,sep=",")
# end of calculating differences--------------------------------

# graphs----------------------------------------------------------------------

biom.inc.graph <- ggplot(data=biom.data,aes(x=age,y=inc)) + geom_point()
biom.inc.graph + geom_point(aes(colour=factor(stratum)))

# seems to be some outliers ....HERE - calculate from tree level?
#---------------------------------------------------------------------------------------------
#### TREE LEVEL
# Tree-level delta calculation to see of plot level estimates become more reasonable...

# read-in data------------------------------------------------------------------------
tree.biom <- read.table("SK_2000TreeBiomass.csv",sep=",",header=TRUE)
# clean-up
tree.biom1 <- group_by(tree.biom, PLOT_ID, YEAR,TREE_NO) %>%
  summarise(sps=first(SPECIES),dom=first(dom),age=mean(age),biom=mean(biomass))
# same number, no clean-up to do
meas.tree <- group_by(tree.biom1, PLOT_ID,TREE_NO) %>%
  summarize(tmeas=n())
table(meas.tree$tmeas)
# 1      2      3      4      5 
# 306320  67716  25268  12291   1479 
# end of read-in data-----------------------------------------------------------------

# Calculate the delta for years, biom.tree, and track ages per tree-------------------------
yr.t <- matrix(nrow=dim(meas.tree)[[1]],ncol = 5)
biom.t <- matrix(nrow=dim(meas.tree)[[1]],ncol = 5)
age.t <- matrix(nrow=dim(meas.tree)[[1]],ncol = 5)

# age1 <- group_by(tree.biom1,PLOT_ID) %>%
#   summarise(age1=first(age),age.last=last(age))

table1 <- group_by(tree.biom1,PLOT_ID,TREE_NO) %>%
  summarise(col1 = first(YEAR), col.last = last(YEAR),meas1=first(biom),
            meas.last=last(biom),age1=first(age),age.last=last(age)) %>%
  inner_join(meas.tree) %>%
  arrange(PLOT_ID,TREE_NO,col1)

# dealing with the 1st and last measurements of each tree--------------
yr.t[,1] <- table1$col1
yr.t[which(table1$tmeas<3),2] <- table1$col.last[which(table1$tmeas<3)]
yr.t[which(table1$tmeas==3),3] <- table1$col.last[which(table1$tmeas==3)]
yr.t[which(table1$tmeas==4),4] <- table1$col.last[which(table1$tmeas==4)]
yr.t[which(table1$tmeas==5),5] <- table1$col.last[which(table1$tmeas==5)]

biom.t[,1] <- table1$meas1
biom.t[which(table1$tmeas<3),2] <- table1$meas.last[which(table1$tmeas<3)]
biom.t[which(table1$tmeas==3),3] <- table1$meas.last[which(table1$tmeas==3)]
biom.t[which(table1$tmeas==4),4] <- table1$meas.last[which(table1$tmeas==4)]
biom.t[which(table1$tmeas==5),5] <- table1$meas.last[which(table1$tmeas==5)]


age.t[,1] <- table1$age1
age.t[which(table1$tmeas<3),2] <- table1$age.last[which(table1$tmeas<3)]
age.t[which(table1$tmeas==3),3] <- table1$age.last[which(table1$tmeas==3)]
age.t[which(table1$tmeas==4),4] <- table1$age.last[which(table1$tmeas==4)]
age.t[which(table1$tmeas==5),5] <- table1$age.last[which(table1$tmeas==5)]
# 1st and last done---------------------------------------------------------

# Three measurements, middle one----------------------
table2 <- left_join(table1,tree.biom1) %>%
  arrange(PLOT_ID, YEAR,TREE_NO) %>%
  filter(YEAR!=col1 & YEAR!=col.last) 
table2 <- unique(table2)
  

yr.t[which(table1$tmeas==3),2] <- table2$YEAR[which(table2$tmeas==3)]
biom.t[which(table1$tmeas==3),2] <- table2$biom[which(table2$tmeas==3)]
age.t[which(table1$tmeas==3),2] <- table2$age[which(table2$tmeas==3)]
# Three measurements middle one done-----------------

# 2nd measurements for all with >3; for 4th measument, 3rd, for 5th, 4th, 
table2count <- group_by(table2,PLOT_ID,TREE_NO) %>%
  summarise(count.meas =n())
table(table2count$count.meas)
# table2 has the right number of meas per tree
# 1     2     3 
# 25268 12291  1479 
table(table2$tmeas)
# 3     4     5 
# 25268 24582  4437 
table3 <- filter(table2,tmeas>3) %>%
  group_by(PLOT_ID, TREE_NO) %>%
  summarise(col2=first(YEAR),col.last2=last(YEAR),
            biom2=first(biom),biom.last2=last(biom),
            age2=first(age),age.last2=last(age)) %>%
  inner_join(meas.tree) %>%
  arrange(PLOT_ID,TREE_NO,col2)

yr.t[which(table1$tmeas==4),2] <- table3$col2[which(table3$tmeas==4)]
yr.t[which(table1$tmeas==4),3] <- table3$col.last2[which(table3$tmeas==4)]
yr.t[which(table1$tmeas==5),2] <- table3$col2[which(table3$tmeas==5)]
yr.t[which(table1$tmeas==5),4] <- table3$col.last2[which(table3$tmeas==5)]

biom.t[which(table1$tmeas==4),2] <- table3$biom2[which(table3$tmeas==4)]
biom.t[which(table1$tmeas==4),3] <- table3$biom.last2[which(table3$tmeas==4)]
biom.t[which(table1$tmeas==5),2] <- table3$biom2[which(table3$tmeas==5)]
biom.t[which(table1$tmeas==5),4] <- table3$biom.last2[which(table3$tmeas==5)]


age.t[which(table1$tmeas==4),2] <- table3$age2[which(table3$tmeas==4)]
age.t[which(table1$tmeas==4),3] <- table3$age.last2[which(table3$tmeas==4)]
age.t[which(table1$tmeas==5),2] <- table3$age2[which(table3$tmeas==5)]
age.t[which(table1$tmeas==5),4] <- table3$age.last2[which(table3$tmeas==5)]
#all with =<4 DONE---------------------------------------------------------------------

# middle for 5 meas-------------------------------------------------------------------
table4 <- inner_join(tree.biom1,table1) %>%
  inner_join(table3) %>%
  arrange(PLOT_ID, YEAR,TREE_NO) %>%
  filter(YEAR!=col1 & YEAR!=col2 & YEAR!=col.last & YEAR!=col.last2)

yr.t[which(table1$tmeas==5),3] <- table4$YEAR[which(table4$tmeas==5)]
biom.t[which(table1$tmeas==5),3] <- table4$biom[which(table4$tmeas==5)]
age.t[which(table1$tmeas==5),3] <- table4$age[which(table4$tmeas==5)]
# all year, meas, age tables filled---------------------------------------------------

# put all other values to 0 -----------------------------
yr.t[is.na(yr.t)] <- 0
biom.t[is.na(biom.t)] <- 0
age.t[is.na(age.t)] <- 0
# end of 0 ----------------------------------------------

# calculating differences -------------------------------------
yr.df.t <- matrix(nrow=dim(yr.t)[[1]], ncol=dim(yr.t)[[2]]-1)
biom.df.t <- matrix(nrow=dim(biom.t)[[1]], ncol=dim(biom.t)[[2]]-1)
for(i in 1:4){
  yr.df.t[,i] = yr.t[,i+1] - yr.t[,i]
  biom.df.t[,i]=biom.t[,i+1] - biom.t[,i]
}

yr.df.t[which(yr.df.t<1)] <- NA
biom.df.t[which(biom.df.t<1)] <- NA

delta.kg.tree <- biom.df.t/yr.df.t

biom.inc.t <-cbind(table1[,1:2],delta.kg.tree)
names(biom.inc.t) = c("PLOT_ID","TREE_NO","inc1","inc2","inc3","inc4")
biom.inc2.t <- biom.inc.t %>%
  gather(meas.no,inc,-PLOT_ID,-TREE_NO) %>%
  arrange(PLOT_ID,TREE_NO)

biom.age.t <-cbind(table1[,1:2],age.t[,1:4])
#names(biom.age.t) = c("PLOT_ID","TREE_NO","age1","age2","age3","age4")
biom.age.t2 <- biom.age.t %>%
  gather(ageno,age,-PLOT_ID,-TREE_NO) %>%
  arrange(PLOT_ID,TREE_NO)

tree.biom.inc1 <- cbind(biom.inc2.t,biom.age.t2[,4])
names(tree.biom.inc1) = c("PLOT_ID","TREE_NO","meas.no","inc","biom.age")
## NEXT: remove NAs and save - this is in kg/tree
tree.biom.inc <- tree.biom.inc1[!is.na(tree.biom.inc1$inc),]
write.table(tree.biom.inc,file="TreeBiomInc.txt",sep=",",row.names=FALSE)
# END tree-level increment------------------------------------------------------------------

#sum over plot and calculate the per ha value------------------------------------
tree.inc.plot <- group_by(tree.biom.inc,PLOT_ID,meas.no) %>%
  summarise(plot.inc = sum(inc),age1=mean(biom.age))
# get plot sizes---------------
plot.size <-read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/BiomassEstimation/measurement_header.csv",sep=",", header=TRUE)
plot.size <- plot.size[,c(1,3)]
plot.size <- unique(plot.size)
# t per ha and attach strata
plot.inc <- inner_join(tree.inc.plot, plot.size) %>%
  mutate(biom.ha=(plot.inc/PLOT_SIZE)/1000) %>%
  inner_join(strata)

# which records did not get a match?
# has created "all" meas plot.inc and then the match with the strata
#no.match <- anti_join(plot.inc,plot.inc.strata)
# this plot no is not in the original casfri data...cannot attach strata

# graph: tree-level
biom.inc.tree <- ggplot(data=plot.inc,aes(x=age1,y=biom.ha)) + geom_point() + 
  geom_point(aes(colour=factor(stratum))) +
  ggtitle("t/ha increment tree-level") + theme(legend.position="none")



# read delta calculated from the plot level
biom.data <- read.table("t_haBiom_yr.txt",sep=",",header=TRUE)
biom.inc.plot <- ggplot(data=biom.data,aes(x=age,y=inc)) + geom_point() + 
  geom_point(aes(colour=factor(stratum))) +
  ggtitle("t/ha increment plot-level")+ theme(legend.position="none")

# compare
multiplot(biom.inc.tree,biom.inc.plot,cols=2)

#### CONCLUSION: the estimates calculated from the tree-level data then aggregated to 
# the plot-level seem way more reasonable than the differences calculated from the plot-level
# biomass per ha estimates
### ##############

plot.inc <- select(plot.inc,PLOT_ID,meas.no,age1,biom.ha.inc=biom.ha, soil_moist_reg,dom,stratum)
write.table(plot.inc,file="t_haBiom_yr.txt",row.names=FALSE,sep=",")
# end of calculating differences--------------------------------


# Multiple plot function--------------------------------------------------------------------
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






