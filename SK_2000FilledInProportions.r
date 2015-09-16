#----------------------------------------------------------------------------
# Figuring out what proportion of individual tree heights, ages, and dbhs were filled-in
#
# CBoisvenue May 29, 2015
#----------------------------

# Admin --------------------------------------------
#install.packages("plyr")
require(plyr)
#install.packages("ggplot2")
#require(ggplot2)
#install.packages("dplyr")
require(dplyr)

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/")
#--------------

# 1 read in raw tree data--------------------------------------------------------
sk.tree <- read.csv("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04/trees.csv", header=TRUE)
dim(sk.tree) #[1] 845981     21
sk.tree <- unique(sk.tree)
sk.age <- read.table("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04/age_samples.csv", header=TRUE, sep=",")
sk.geo <- read.table("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04/plot_header.csv", header=TRUE, sep=",")
sk.geo <- select(sk.geo,PLOT_ID, LOC_ACCURACY,contains("Z13"))
# for Sasha
#write.table(sk.geo,file="G:/RES_Work/Work/Analysis_PSPs/SashaHararuk/plotLocAccuracy.txt",sep=",",row.names = FALSE)

# 2 read-in the final "all info" tree-level file
sk.clean <- read.table("SK_2000TreeMeasurements.csv",header=TRUE,sep=",")
sk.biom <- read.table("SK_2000TreeBiomass.csv",header=TRUE,sep=",")

#-------read-in end

# no of trees "r" is for raw "p" is for processed --------------------------------

# tree counts per plot per year
r.no.plots <- length(unique(sk.tree$PLOT_ID))
p.no.plots <- length(unique(sk.clean$PLOT_ID))

# let's only compare the plots that we keep...1381 of them because there were only 1381 plots with ages
plots <- data.frame(unique(sk.clean$PLOT_ID))
names(plots) = c("PLOT_ID")

r.trees.per.plot <- group_by(sk.tree,PLOT_ID,YEAR) %>%
  summarize(rtrees=n()) %>%
  arrange(PLOT_ID,YEAR) %>%
  inner_join(plots)

# same number of trees in the processed?
p.trees.per.plot <- group_by(sk.clean,PLOT_ID,YEAR) %>%
  summarize(ptrees=n()) %>%
  arrange(PLOT_ID,YEAR) 
  
no.trees.diff <- inner_join(p.trees.per.plot,r.trees.per.plot) %>%
  mutate(trees.diff = rtrees-ptrees)

# No, there are differences: positive makes sense (raw-processed, so we lost a few), 
# No negatives
neg.no.trees <-filter(no.trees.diff,trees.diff< 0)

          # # this is old code - this problem has now been solved...keeping code anyway
          # # Need to check SK_2000.r to see where the additional trees happen 
          # # Walking through the code and checking no.trees per plot..
          # # check at line 148 - sk2k.1 data frame
          # #use only the matching plots
          # 
          # ck1 <- inner_join(sk2k.9,plots)
          # ck2 <- group_by(ck1,PLOT_ID,YEAR) %>%
          #   summarise(ntrees=n())
          # length(unique(ck2$PLOT_ID))
          # 
          # 
          # 
          # check.add1 <- inner_join(sk2k.1,plots)
          # length(unique(check.add1$PLOT_ID)) #1381
          # check.add2 <- group_by(check.add1,PLOT_ID,YEAR) %>%
          #   summarize(ctrees=n()) %>%
          #   arrange(PLOT_ID,YEAR) 
          # dim(check.add2)#[1] 3714    3
          # # same number of plots and years
          # check.add3 <- inner_join(check.add2,no.trees.diff) %>%
          #   mutate(r_c = rtrees-ctrees,p_c = ptrees-ctrees)
          # range(check.add3$r_c) # 0 0 
          # range(check.add3$p_c) # -108  699 
          # # no differences at this point between raw and the check
          # 
          # # the problem is with the sk2k.2 - it has 20 extra trees.
          # 
          # # check at line 160
          # check.add4 <- inner_join(sk2k.2,plots)
          # length(unique(check.add4$PLOT_ID))
          # check.add5 <- group_by(check.add1,PLOT_ID,YEAR) %>%
          #   summarize(ctrees=n()) %>%
          #   arrange(PLOT_ID,YEAR)
          # check.add6 <- inner_join(check.add5,no.trees.diff) %>%
          #   mutate(r_c = rtrees-ctrees,p_c = ptrees-ctrees)
          # range(check.add6$r_c) # 0 0 
          # range(check.add6$p_c) # -108  699 
          # 
          # # check at line 170
          # check.add7 <- inner_join(sk2k.3.1,plots)
          # length(unique(check.add6$PLOT_ID))
          # check.add8 <- group_by(check.add7,PLOT_ID,YEAR) %>%
          #   summarize(ctrees=n()) %>%
          #   arrange(PLOT_ID,YEAR)
          # check.add9 <- inner_join(check.add8,no.trees.diff) %>%
          #   mutate(r_c = rtrees-ctrees,p_c = ptrees-ctrees)
          # range(check.add9$r_c) # -2 0 
          # check.add9[which(check.add9$r_c<0),]
          # 
          # ## we are adding 2 trees here...why??
          # # check just one plot 20294 for one year 1980 
          # oneplot <- filter(spsAvg,PLOT_ID==20294, YEAR==1980)
          # skone <- filter(sk2k.2,PLOT_ID==20294, YEAR==1980)
          # 
          # # checking all plots with negative 
          # extra1 <- unique(check.add9[which(check.add9$r_c<0),1])
          # extra2 <- inner_join(spsAvg,extra1)
          # # count the trees by plot here
          # extra3 <- group_by(extra2,PLOT_ID,YEAR)%>%
          #   summarize(xtrees=n()) %>%
          #   arrange(PLOT_ID,YEAR)
          # #one tree by plot by year (as it should be)
          # extra4 <- inner_join(sk2k.3,extra1) %>%
          #   group_by(PLOT_ID,YEAR)%>%
          #   summarize(xtrees=n()) %>%
          #   arrange(PLOT_ID,YEAR)
          # extra5 <- inner_join(check.add9,extra4) %>%
          #   mutate(x_r=xtrees-rtrees)
          # range(extra5$x_r)
          # extra6<-inner_join(sk2k.3.1,extra1) %>%
          #   group_by(PLOT_ID, YEAR) %>%
          #   summarize(n())

# how many ages were filled-in? ---------------------------
r.ages <- group_by(sk.age,PLOT_ID, TREE_NO,YEAR) %>%
  summarise(n.r.age = n())
dim(r.ages)#[1] 7753    4

p.ages <- group_by(sk.clean,PLOT_ID,TREE_NO,YEAR) %>%
  summarise(n.p.ages=n())
p.ages2 <- group_by(sk.biom,PLOT_ID,TREE_NO,YEAR) %>%
  summarise(n.p.ages=n())
  
