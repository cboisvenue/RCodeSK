#--------------------------------------------------------------------
# Volume calculations for SK 2000 PSPs per tree
# This is to see trends when comparing to biomass estimates....
# hopefully, this helps connect findings to policy issues.
#
# Part of the Growth Raster project
#
# May 19th, 2015
# CBoisvenue
#--------------------------------------------------------------------

require(plyr)
require(ggplot2)
require(dplyr)

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/")

# read-in tree-level data ------------------------------------------------
trees <- read.table("SK_2000TreeMeasurements.csv",header=TRUE,sep=",")
# general into on this file
no_plots <- length(unique(trees$PLOT_ID))
no_meas <- group_by(trees,PLOT_ID,TREE_NO) %>% summarise(n())
range(no_meas[,3]) # 1 to 5
length(which(no_meas[,3]==1))#[1] 306320
no_trees <- dim(no_meas)[[1]]#[1] 413074
trees_meas <- no_meas[which(no_meas[,3]>1),]
length(unique(trees_meas$PLOT_ID))#[1] 823
dim(trees_meas) #[1]106754      3
# End of reading-in ------------------------------------------------------


# Volume calculations----------------------------------------------------------------------------------
####################################################################################
# Using Kozak equation evaluated in Gal and Bella's report (see Build_PSP_models sheet in 
# "tracking_SK_Work.xlsx in M:\\ in Metadata)
# Using's Newton's equation to calculate merch and total volume per tree
# I also have the height diam models and coefficients here:
# C:\Celine\Big_data\Data\01_RawFiles\SK\Release_2012-04\height_dbh_models.xls
####################################################################################

# From Flewelling 1995 - The utilization criteria behind merchantable volume were: 
# minimum dbh of 9.10 cm. minimum top diameter of 8.01 cm and stump height of 30 cm in all cases.
# Because I need merch volume, height at top diam of 8cm is also needed
# 
# need to estimate ht at 8cm inside bark diam
# the simplest form of the equation to solve for ht at dib 8cm is this, but
# solving for the rel.ht in rel.diam = ((1-(sqrt(rel.ht)))/(1-sqrt(p)))^(1/(rel.ht+k)) is not possible
# used online solver (https://www.wolframalpha.com)
# so cannot do it on the fly 
# SOLUTION: creating categories for each species for 3 diam/height combinations

# need parameter values for a0, a1, a2 for each species (this with dbh gives DI - diameter at inflection point) 
a.params <- read.table("GalBella_params.csv",header=TRUE,sep=",")
# get these from Gal and Bella
# need p for each species (this with ht gives X) - from Gal and Bella
p.cat <- read.table("VolCal_p_values.csv",header=TRUE,sep=",")
# X is needed to get k (k=f(X,dbh/DI,ht))
p.cat1 <- inner_join(p.cat,a.params) %>%
  select(species,p, dbh,ht,a0,a1,a2) %>%
  mutate(X =(1-(sqrt(1.3/ht)))/(1-sqrt(p)),DI = a0*(dbh^a1)*(a2^dbh),k = (log(X) - ((log(dbh/DI))*(1.3/ht)))/log(dbh/DI),
         rel.d8 = 8/DI)
# k was calculated for the 3 height/diam combos for 8 species (not MM)
# each p,k,DI combination was entered into the on-line solver wolframalpha to 
# obtain ht at 8cm diam inside bark - 
# solving for the rel.ht in rel.diam = ((1-(sqrt(rel.ht)))/(1-sqrt(p)))^(1/(rel.ht+k))

# this 1st run through, WS 5cm diam, and BF 5 cm diam did not give a solution
# so, I changed the diam and height values for those two estimates

ht8 = c(0.369695,0.278032,0.215024,0.13,0.13,0.190685,0.232829,0.201267,0.550664,0.584727,
        0.863603,0.789376,0.916937,0.888433,0.93273,0.102506,0.618649,0.915761,0.0542566,0.0370625,
       0.411954,0.913348,0.587658,0.887407)
p.cat3 <-cbind(p.cat1,ht8)
p.cat4 <- select(p.cat3, species,p,dbh,ht8) %>%
  arrange(species,dbh)

# Need to attach a ht8 to each tree withint the diam classes
# Rules:
# BF,TL,BS,WS,WB,BP,TA: 1 is <9, 2 is between 9 and 22, >22 is 3
# JP: 1 is same as above, 4 is between 9 and 18, >18 is 5
key1 <- c(rep(1:3,3),1,4,5,rep(1:3,4))
p.cat5 <-cbind(p.cat4,key1)

p.cat6 <- select(p.cat5,species,p,ht8,key1)

key2 <- rep(1,dim(trees)[[1]])
key2[which(trees$DBH>=9 & trees$DBH<22)]=2
key2[which(trees$DBH>=22)]=3
key2[which(trees$SPECIES=="JP" & trees$DBH>=9 & trees$DBH<18)]=4
key2[which(trees$SPECIES=="JP" & trees$DBH>=18)]=5

trees1 <- cbind(trees,key2)
names(p.cat6) = c("SPECIES","p","ht8","key2")
names(a.params) = c("SPECIES", "a0","a1","a2","b1","b2","b3","b4","b5")

trees2 <- left_join(trees1,p.cat6) %>%
  mutate(ht.se = HEIGHT*ht8) %>%
  select(-key2,-ht8) %>%
  mutate(ht.m=HEIGHT/2) %>%
  left_join(a.params) 
# calculating all the variables for Newton's equation and calculating ind. tree vol
# merch vol (from 0.3m in height to 8cm in top diam inside bark)
# total volume (from 0.3m in height calculated with Newton's equation and to total ht) plus
# a 0.3m cylinder for the stump ((pi*r^2)*0.3m)
trees3 <- mutate(trees2,
                 d.le = (a0*DBH^a1)*(a2^DBH)*(((1-(sqrt(0.3/HEIGHT)))/(1-sqrt(p)))^((b1*((0.3/HEIGHT)^2))+
                  (b2*log((0.3/HEIGHT)+0.001))+(b3*sqrt((0.3/HEIGHT)))+(b4*exp(0.3/HEIGHT))+(b5*(DBH/HEIGHT)))),
                 d.m = (a0*DBH^a1)*(a2^DBH)*(((1-(sqrt(ht.m/HEIGHT)))/(1-sqrt(p)))^((b1*((ht.m/HEIGHT)^2))+
                  (b2*log((ht.m/HEIGHT)+0.001))+(b3*sqrt((ht.m/HEIGHT)))+(b4*exp(ht.m/HEIGHT))+(b5*(DBH/HEIGHT)))),
                 d.se = (a0*DBH^a1)*(a2^DBH)*(((1-(sqrt(ht.se/HEIGHT)))/(1-sqrt(p)))^((b1*((ht.se/HEIGHT)^2))+
                  (b2*log((ht.se/HEIGHT)+0.001))+(b3*sqrt((ht.se/HEIGHT)))+(b4*exp(ht.se/HEIGHT))+(b5*(DBH/HEIGHT)))),
                 l=ht.se-0.3,
                 merchvol = l*pi*((d.le/100)^2+4*(d.m/100)^2+(d.se/100)^2)/24,
                 treevol = (HEIGHT*pi*((d.le/100)^2+4*(d.m/100)^2)/24) + (pi*((d.le/200)^2))*0.3)
      
# Remove all NAs or negatives 
trees4 <- filter(trees3,!is.na(treevol) & treevol>0) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,DBH,HEIGHT,age,dom,merchvol,treevol)

write.table(trees4,"SK2000_treeVol.txt",sep=",")
# END of volume calculations---------------------------------------------------------------------------

# Volume per plot and per ha---------------------------------------------------------------------------
# for Merch vol DBH>9 only
plot.mvol <- filter(trees4,DBH>9) %>% group_by(PLOT_ID,YEAR) %>%
  summarise(mvol=sum(merchvol))
plot.tvol <- group_by(trees4,PLOT_ID,YEAR) %>%
  summarise(tvol=sum(treevol))

# get plot size
plot.size <-read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/BiomassEstimation/measurement_header.csv",sep=",", header=TRUE)
plot.size <- plot.size[,c(1,3,20)]
checksize <- group_by(plot.size,PLOT_ID) %>%
  summarise(mean=mean(PLOT_SIZE))
checksize2 <-  left_join(plot.size,checksize)
sizediff <- mutate(checksize2,diff=PLOT_SIZE-mean)
range(sizediff$diff)
# plot size does not change over time
plot.size1 <- unique(plot.size[,c(1,2)])

# vol per ha per plot per year
merchvol.ha <- left_join(plot.mvol,plot.size1) %>%
  mutate(merch.ha = mvol/PLOT_SIZE) %>%
  select(PLOT_ID,YEAR,merch.ha)
totalvol.ha <- left_join(plot.tvol,plot.size1) %>%
  mutate(vol.ha = tvol/PLOT_SIZE) %>%
  select(PLOT_ID,YEAR,vol.ha)
vol.ha <- full_join(totalvol.ha,merchvol.ha)

write.table(vol.ha,file="C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/SK_2000Vol_ha.txt",sep=",",row.names=FALSE)
# End of volumen per ha calculations---------------------------------------------------------------------

# Experimental code
          # # figuring out things for one tree
          # #BS 
          # #from data
          # dbh=15.6
          # ht = 16.5
          # # height at mid point
          # ht.m = ht/2
          # # inflection point for BS
          # p=0.15
          # ht.i = ht*p
          # # from Gal and Bella
          # 
          # a0=0.933348
          # a1 = 0.993014 
          # a2 = 0.998033 
          # b1 = 1.634033 
          # b2=-0.377332
          # b3= 2.839266
          # b4=-1.523830
          # b5=0.224967
          # # diameter at inflection point
          # DI = a0*(dbh^a1)*(a2^dbh)
          # # to build a curves rel diam over rel height, use dbh height
          # X =(1-(sqrt(1.3/ht)))/(1-sqrt(p))
          # k = (log(X) - ((log(dbh/DI))*(1.3/ht)))/log(dbh/DI)
          # # building the curve
          # #rel.diam = seq(0,2,0.2)
          # rel.ht = c(seq(0,0.1,0.02),seq(0,1,0.1),seq(0.9,1,0.02))
          # rel.diam = ((1-(sqrt(rel.ht)))/(1-sqrt(p)))^(1/(rel.ht+k))
          # rel.vars <- data.frame(rel.ht,rel.diam)
          # ggplot(rel.vars, aes(x=rel.ht, y=rel.diam, ymax=1.75)) +
          #   geom_line() +
          #   geom_point() 
          # # +
          # #   ylim(0,1.75)
          # qplot(x=rel.ht, y=rel.diam, data=rel.vars,ylim=c(0,1.75))
