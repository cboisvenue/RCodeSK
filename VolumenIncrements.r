#------------------------------------------------------------------------------------
# Building a yield table for use in base-CBM runs for SK
# Goals:
# 1) tree-level volume increment calculations 
# 2) plot-level increment summaries
# 3) yearly per ha increments
# 4) stratification
# 5) Fit yield curve
#
# CBoisvenue
# June 22nd, 2015
#------------------------------------------------------------------------------------

#library(nlme)
library(ggplot2)
library(RColorBrewer)
require(plyr)
require(dplyr)
library(tidyr)

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/")

# read-in base file-----------------------
tree.vol <-read.table("SK2000_treeVol.txt",header=TRUE,sep=",")

# calculate tree-level increments in merch vol------------------------------------------------------------

# how many trees have >1 measurements?
trees <- group_by(tree.vol,PLOT_ID,TREE_NO,YEAR) %>%
  summarize(sps = first(SPECIES),ht=mean(HEIGHT),age=mean(age),merch=mean(merchvol),dbh=mean(dbh)) %>%
  rename(target.yr = YEAR)
meas.tree <- arrange(trees,PLOT_ID,target.yr,TREE_NO) %>%
  group_by(PLOT_ID,TREE_NO) %>%
  summarize(no.meas=n(),first.yr=first(target.yr),last.yr=last(target.yr)) %>%
  filter(no.meas>1)
dim(meas.tree)#   104557      3
range(meas.tree$no.meas)
table(meas.tree$no.meas)
# 2     3     4     5 
# 68032 23686 11680  1159 

# calculating differences in merch between measurements-------------------------------
# allmerch <- select(trees,PLOT_ID, TREE_NO,target.yr,merch)
# merch1 <- spread(allmerch,target.yr,merch)
# THE ABOVE WORKS (puts it wide...) but I cannot figure out how to substract the right columns 
# for each measurement interval...
#colSums(merch1[,3:45],na.rm=TRUE)

############## THis is the per tree approach: calculate the increment per tree per year, and sum
### the per tree per year increment for the whole plot. Convert to inc.ha.yr for that plot 
#################################################################################################

# Dealing with the plot/tree combos with 2 measurements only--------------------------------------
survey2 <-  filter(meas.tree,no.meas==2) 
meas2.merch1 <- select(survey2,PLOT_ID,TREE_NO,target.yr=first.yr)%>%
  left_join(trees) %>%
  rename(yr1=target.yr,ht1=ht,age1=age,merch1=merch,dbh1=dbh)
meas2.merch2 <- select(survey2,PLOT_ID,TREE_NO,target.yr=last.yr)%>%
  left_join(trees) %>%
  rename(yr2=target.yr,ht2=ht,age2=age,merch2=merch,dbh2=dbh)
meas2.delta <- inner_join(meas2.merch2,meas2.merch1) %>%#note: we lost one tree here...weird
  mutate(yr.delta = yr2-yr1,ht.delta=ht2-ht1,age.delta=age2-age1,dbh.delta=dbh2-dbh1,merch.delta=merch2-merch1,
         inc.yr = merch.delta/yr.delta) %>%
  select(PLOT_ID,TREE_NO,first.yr = yr1,inc.yr,age1)
# how many negatives?
dim(filter(meas2.delta,inc.yr<0)) #[1] 11398     6
# can't keep those
meas2.inc <- filter(meas2.delta, inc.yr>=0)
# End of trees measured twice -------------------------------------------------------------------


# Dealing with the plot/tree combos with 3 measurements----------------------------------------
survey3 <- filter(meas.tree,no.meas==3)
meas3.merch1 <- select(survey3,PLOT_ID,TREE_NO,target.yr=first.yr)%>%
  left_join(trees) %>%
  rename(yr1=target.yr,age1=age,merch1=merch) %>% #dbh1=dbh ht1=ht,
  select(-sps,-ht,-dbh)
# how do I get that middle year??
meas3.merch2 <- left_join(survey3,trees) %>%
  filter(target.yr!=first.yr & target.yr!=last.yr) %>%
  rename(yr2=target.yr,age2=age,merch2=merch) %>% #ht2=ht,dbh2=dbh
  select(-no.meas,-sps,-ht,-dbh)
meas3.merch3 <- select(survey3,PLOT_ID,TREE_NO,target.yr=last.yr)%>%
  left_join(trees) %>%
  rename(yr3=target.yr,age3=age,merch3=merch) %>%#ht3=ht,,dbh3=dbh
  select(-sps,-ht,-dbh)
meas3.delta <- inner_join(meas3.merch3,meas3.merch2) %>%
  inner_join(meas3.merch1) %>%
  # range(meas3.delta$yr1-meas3.delta$first.yr)# 0 0 
  # range(meas3.delta$yr3-meas3.delta$last.yr)# 0 0
  mutate(yr.delta1 = yr2-yr1,merch.delta1=merch2-merch1,
         inc.yr1 = merch.delta1/yr.delta1,yr.delta2 = yr3-yr2,merch.delta2=merch3-merch2,
         inc.yr2 = merch.delta2/yr.delta2) %>%
  select(-merch1,-merch2,-merch3,-first.yr,-last.yr)
meas3.1 <-  select(meas3.delta, PLOT_ID,TREE_NO,first.yr=yr1,inc.yr=inc.yr1,age1=age1) %>%
  filter(inc.yr>=0)
meas3.2 <-  select(meas3.delta, PLOT_ID,TREE_NO,first.yr=yr2,inc.yr=inc.yr2,age1=age2) %>%
  filter(inc.yr>=0)
meas3.inc <- rbind(meas3.1,meas3.2)
# End of trees measured thrice -------------------------------------------------------------------

# Dealing with the plot/tree combos with 4 measurements----------------------------------------
survey4 <- filter(meas.tree,no.meas==4)
meas4.merch1 <- select(survey4,PLOT_ID,TREE_NO,target.yr=first.yr)%>%
  left_join(trees) %>%
  rename(yr1=target.yr,age1=age,merch1=merch) %>% #dbh1=dbh ht1=ht,
  select(-sps,-ht,-dbh)

# how do I get that middle year??
meas4.merch2 <- left_join(survey4,trees) %>%
  filter(target.yr!=first.yr & target.yr!=last.yr) 

# first last of this one-------------
    mid.yrs <- arrange(meas4.merch2,PLOT_ID,target.yr,TREE_NO) %>%
      group_by(PLOT_ID,TREE_NO) %>%
      summarize(yr2=first(target.yr),yr3=last(target.yr))
    mid.yr1 <- left_join(mid.yrs,trees) %>%
      filter(yr2==target.yr) %>%
      select(PLOT_ID,TREE_NO,yr2,age2=age,merch2=merch)
    mid.yr2 <- left_join(mid.yrs,trees) %>%
      filter(yr3==target.yr) %>%
      select(PLOT_ID,TREE_NO,yr3,age3=age,merch3=merch)
# Mid-years done--------------------

meas4.merch4 <- select(survey4,PLOT_ID,TREE_NO,target.yr=last.yr)%>%
  left_join(trees) %>%
  rename(yr4=target.yr,age4=age,merch4=merch) %>%#ht3=ht,,dbh3=dbh
  select(-sps,-ht,-dbh)
meas4.delta <- inner_join(meas4.merch1,mid.yr1) %>%
  inner_join(mid.yr2) %>%
  inner_join(meas4.merch4) %>%
  mutate(yr.delta1 = yr2-yr1,merch.delta1=merch2-merch1,
         inc.yr1 = merch.delta1/yr.delta1,yr.delta2 = yr3-yr2,merch.delta2=merch3-merch2,
         inc.yr2 = merch.delta2/yr.delta2,yr.delta3 = yr4-yr3,merch.delta3=merch4-merch3,
         inc.yr3 = merch.delta3/yr.delta3) %>%
  select(PLOT_ID,TREE_NO, yr1,yr2,yr3,age1,age2,age3,contains("inc."))
meas4.1 <-  select(meas4.delta, PLOT_ID,TREE_NO,first.yr=yr1,inc.yr=inc.yr1,age1=age1) %>%
  filter(inc.yr>=0)
meas4.2 <-  select(meas4.delta, PLOT_ID,TREE_NO,first.yr=yr2,inc.yr=inc.yr2,age1=age2) %>%
  filter(inc.yr>=0)
meas4.3 <-  select(meas4.delta, PLOT_ID,TREE_NO,first.yr=yr3,inc.yr=inc.yr3,age1=age3) %>%
  filter(inc.yr>=0)
meas4.inc <- rbind(meas4.1,meas4.2,meas4.3)
# End of trees measured four times -------------------------------------------------------------------


# Dealing with the plot/tree combos with 5 measurements----------------------------------------
survey5 <- filter(meas.tree,no.meas==5)
meas5.merch1 <- select(survey5,PLOT_ID,TREE_NO,target.yr=first.yr)%>%
  left_join(trees) %>%
  rename(yr1=target.yr,age1=age,merch1=merch) %>% #dbh1=dbh ht1=ht,
  select(-sps,-ht,-dbh)

# get those middle year
meas5.merch2 <- left_join(survey5,trees) %>%
  filter(target.yr!=first.yr & target.yr!=last.yr) 

# first last of this one-------------
mid.yrs <- arrange(meas5.merch2,PLOT_ID,target.yr,TREE_NO) %>%
  group_by(PLOT_ID,TREE_NO) %>%
  summarize(yr2=first(target.yr),yr4=last(target.yr))
mid.yr1 <- left_join(mid.yrs,trees) %>%
  filter(yr2==target.yr) %>%
  select(PLOT_ID,TREE_NO,yr2,age2=age,merch2=merch)
mid.yr3 <- left_join(mid.yrs,trees) %>%
  filter(yr4==target.yr) %>%
  select(PLOT_ID,TREE_NO,yr4,age4=age,merch4=merch)
mid.yr2 <- left_join(mid.yrs,meas5.merch2) %>%
  filter(target.yr!=yr2 & target.yr!=yr4) %>%
  select(PLOT_ID,TREE_NO,yr3=target.yr,age3=age,merch3=merch)
# Mid-years done--------------------

meas5.merch5 <- select(survey5,PLOT_ID,TREE_NO,target.yr=last.yr)%>%
  left_join(trees) %>%
  rename(yr5=target.yr,age5=age,merch5=merch) %>%#ht3=ht,,dbh3=dbh
  select(-sps,-ht,-dbh)

meas5.delta <- inner_join(meas5.merch1,mid.yr1) %>%
  inner_join(mid.yr2) %>%
  inner_join(mid.yr3) %>%
  inner_join(meas5.merch5) %>%
  mutate(yr.delta1 = yr2-yr1,merch.delta1=merch2-merch1,
         inc.yr1 = merch.delta1/yr.delta1,yr.delta2 = yr3-yr2,merch.delta2=merch3-merch2,
         inc.yr2 = merch.delta2/yr.delta2,yr.delta3 = yr4-yr3,merch.delta3=merch4-merch3,
         inc.yr3 = merch.delta3/yr.delta3,yr.delta4 = yr5-yr4,merch.delta4=merch5-merch4,
         inc.yr4 = merch.delta4/yr.delta4) %>%
  select(PLOT_ID,TREE_NO, yr1,yr2,yr3,yr4,age1,age2,age3,age4,contains("inc."))
meas5.1 <-  select(meas5.delta, PLOT_ID,TREE_NO,first.yr=yr1,inc.yr=inc.yr1,age1=age1) %>%
  filter(inc.yr>=0)
meas5.2 <-  select(meas5.delta, PLOT_ID,TREE_NO,first.yr=yr2,inc.yr=inc.yr2,age1=age2) %>%
  filter(inc.yr>=0)
meas5.3 <-  select(meas5.delta, PLOT_ID,TREE_NO,first.yr=yr3,inc.yr=inc.yr3,age1=age3) %>%
  filter(inc.yr>=0)
meas5.4 <-  select(meas5.delta, PLOT_ID,TREE_NO,first.yr=yr4,inc.yr=inc.yr4,age1=age4) %>%
  filter(inc.yr>=0)
meas5.inc <- rbind(meas5.1,meas5.2,meas5.3,meas5.4)

# End of trees measured five times -------------------------------------------------------------------

# put them all together: this contains inc per year per tree (m3/yr for each record)
inc.yr <- rbind(meas2.inc,meas3.inc,meas4.inc,meas5.inc)
tree.inc <- select(inc.yr,-first.yr)
# get plot sizes
plot.size <-read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/BiomassEstimation/measurement_header.csv",sep=",", header=TRUE)
plot.size <- plot.size[,c(1,3)]
plot.size <- unique(plot.size)
dom.sps <- read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/SK_2000TreeMEasurements.csv",sep=",", header=TRUE)
dom.sps <-group_by(dom.sps,PLOT_ID) %>%
  summarise(dom=first(SPECIES))
###
# there are not enough plots with leaving TL so they ae lumped-in with the BF
dom.sps$dom[which(dom.sps$dom=="TL")] <- "BF"
##

# Creating strata--------------------------------------------------------------------------------
casfri <- read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/casfri.txt",sep=",", header=TRUE)
soil <- select(casfri, PLOT_ID=plot_id, soil_moist_reg) 

# taking the 5 soil_moist_reg classes and making them into 3 productivity classes: 
# moist(M) and mesic(F) = good(G), dry(D) and wet(W) = average(M), aquatic(A) = poor(P)
soil$soil_moist_reg <- revalue(soil$soil_moist_reg,c("-1111" = "M","A"="P","D"="M","F"="G","M"="G","W"="M"))
soil$soil_moist_reg[which(is.na(soil$soil_moist_reg))] <- "M"
plot.inc <- group_by(tree.inc,PLOT_ID) %>%
  summarise(plot.yr.inc = sum(inc.yr),plot.age=mean(age1)) %>%
  inner_join(plot.size) %>%
  mutate(m3.ha.yr = plot.yr.inc/PLOT_SIZE) %>%
  inner_join(soil) %>%
  inner_join(dom.sps) %>%
  select(PLOT_ID,plot.age,m3.ha.yr,prodClass=soil_moist_reg,dom)

# from some assessment (below), there will be a total of 10 strata
stratum <- (as.numeric(plot.inc$dom)*10)+(as.numeric(plot.inc$prodClass))
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

plot.inc <- cbind(plot.inc[,1:3],stratum)
strata <- group_by(plot.inc,stratum) %>%
  summarise(count=n())
in.yr <- ggplot(data=plot.inc,aes(x=plot.age,y=m3.ha.yr)) + geom_point()
in.yr + geom_point(aes(colour=factor(stratum)))
ggsave("Yield_strata.jpeg")
write.table(x=plot.inc,file="Plot_Inc_m3_ha_yr.txt",sep=",",row.names=FALSE)  
################# END OF BY TREE BY YEAR INC ###############################

####### Total volume per plot for "GROWTH" ###############################
## SUM MERCH BY PLOT BY YEAR (THIS WOULD BE USED TO FIT A GROWTH CURVE AS OPPOSED 
#### TO A YIELD CURVE) 
# this let's me check the range of volumes to make sure that they make sense

plot.vol <- group_by(trees, PLOT_ID, target.yr) %>%
  summarise(plot.vol = sum(merch),plot.age = mean(age)) %>%
  inner_join(plot.size) %>%
  mutate(totalm3_ha = plot.vol/PLOT_SIZE) %>%
  inner_join(soil) %>%
  inner_join(dom.sps) %>%
  select(PLOT_ID,target.yr,plot.age, totalm3_ha, prodClass=soil_moist_reg,dom)

# creating one colum that represents all 7 species with each a prodClass (3), 
# so 21 possible values
levels(plot.vol$dom)
#[1] "BF" "BP" "BS" "JP" "MM" "TA" "TL" "WB" "WS"
levels(plot.vol$prodClass)
#[1] "M" "P" "G"
stratum <- (as.numeric(plot.vol$dom)*10)+as.numeric(plot.vol$prodClass)
plot.vol <- cbind(plot.vol,stratum)

range(plot.vol$totalm3_ha) #[1]   0.1640795 621.5990926
ggplot(data=plot.vol) + geom_point(aes(x=plot.age,y=totalm3_ha),colour=factor(stratum))
ggsave("totalVol_haPlot.jpeg")
write.table(x=plot.vol,file="totalVol_haPlot.txt",sep=",",row.names=FALSE)  

############ END OF GROWTH ##############################################











