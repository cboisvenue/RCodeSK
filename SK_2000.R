##--------------
# Clean-up the ~2000 PSP as per the SK_418.R clean-up
#
# CBoisvenue April 28, 2015
#----------------------------

# Admin --------------------------------------------
#install.packages("plyr")
require(plyr)
#install.packages("ggplot2")
#require(ggplot2)
#install.packages("dplyr")
require(dplyr)

setwd("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04")
#--------------

# 1 read in the tree data--------------------------------------------------------
sk.tree <- read.csv("trees.csv", header=TRUE)
dim(sk.tree) #[1] 845981     21

sk2k =  select(sk.tree,PLOT_ID,TREE_NO,YEAR,SPECIES,DBH,HEIGHT,TREE_STATUS,CONDITION_CODE1,CONDITION_CODE2,CONDITION_CODE3,MORTALITY,INGROWTH)
sk2k=unique(sk2k)
# check the range of variables to identify potential issues
      range(sk2k$YEAR)#[1] 1949 2009
      unique(sk2k$SPECIES) # factor:  [1] BS WS TL UI BP XX WB TA JP BF MM
      range(sk2k$DBH,na.rm=TRUE)#0.0 133.2
      dbh0 = filter(sk2k, DBH==0) #2996   12
      dbhNA = filter(sk2k, is.na(DBH))#4754   12
      range(sk2k$HEIGHT,na.rm=TRUE)#0.2 46.5
      htNA = filter(sk2k, is.na(HEIGHT))#7746   12
      # status
      unique(sk2k$TREE_STATUS)#[1] 1 3 2 8 7 4 6 0 5 9
      dead = filter(sk2k, TREE_STATUS>2)#67533    12
      #Condition code
      cond = group_by(sk2k,CONDITION_CODE1) %>%
        summarize(length(PLOT_ID))
      # mort
      mort = group_by(sk2k,MORTALITY) %>%
        summarize(length(PLOT_ID))
      sum(mort[1:8,2])#55150
      # ingrowth
      ingrowth = group_by(sk2k, INGROWTH) %>%
        summarize(length(PLOT_ID))
# INGROWTH length(PLOT_ID)
# 1                   843422
# 2        Y            2559

# 2 Ages----------------------------------------------------------------------

sk.age <- read.table("age_samples.csv", header=TRUE, sep=",")
sk.age <- select(sk.age,PLOT_ID,TREE_NO,SPECIES,YEAR,OFF_PLOT_TREE,DBH,HEIGHT,
                 CRUISED_HEIGHT,HEIGHT_TO_LIVE_CROWN,
                 BORED_HEIGHT,COUNTED_AGE,TOTAL_AGE)

      dim(sk.age)# 7753   12
      range(sk.age$TOTAL_AGE,na.rm=TRUE) #6 214
      range(sk.age$COUNTED_AGE,na.rm=TRUE) #6 214
      # is there a difference between TOTAL_AGE and COUNTED_AGE and 
      # does each line have an age?
      t1 <- which(!is.na(sk.age$TOTAL_AGE))
      length(t1)#[1] 4026
      t2 <- sk.age$TOTAL_AGE[t1]-sk.age$COUNTED_AGE[t1]
      range(t2) #NA NA
      head(sk.age$TOTAL_AGE[t1])#[1]  84  86  87 178 185 184
      range(t2,na.rm=TRUE) #[1] 0 0
      t3 <- which(!is.na(sk.age$COUNTED_AGE))
      length(t3)# 7720
      range(sk.age$COUNTED_AGE)#[1] NA NA
      t4 <- which(is.na(sk.age$COUNTED_AGE))
      length(t4) # 33
      which(is.na(sk.age$TOTAL_AGE[t4])) 
      ## Where TOTAL_AGE exists it is not different than COUNTED_AGE
      # Where COUNTED_AGE==NA, TOTAL_AGE has a value


# developing a bored age correction factor--------------------------

  bor.ht <- table(sk.age$BORED_HEIGHT)
  # 0  0.3  1.3 
  # 3993   21 3739 
  # what species are the 3739? and the 21?
  sps1.3 = filter(sk.age,BORED_HEIGHT==1.3) 
  sps1.3 = table(sps1.3$SPECIES)
  # BF   BP   BS   JP   TA   TL   WB   WS 
  # 63   27  591  774 1009   10   54 1211 
  sps0.3 = filter(sk.age,BORED_HEIGHT==0.3) 
  sps0.3 = table(sps0.3$SPECIES)
  # BF BP BS JP TA TL WB WS 
  # 0  0  2  0  5  0  0 14 
  
  age.meas = sk.age$COUNTED_AGE
  age.meas[which(is.na(age.meas))] = sk.age$TOTAL_AGE[which(is.na(sk.age$COUNTED_AGE))]
  # no more 0s
  
  skage1 <- cbind(select(sk.age,PLOT_ID,TREE_NO,YEAR, SPECIES,BORED_HEIGHT),age.meas)
  
  # group by species differences between ages bored at 0 and those ar 1.3 on the same plot
  bored0 <- filter(skage1,BORED_HEIGHT==0) %>%
    select(PLOT_ID,SPECIES,YEAR, age.meas) %>%
    group_by(PLOT_ID, SPECIES)
  bored1 = filter(skage1, BORED_HEIGHT==1.3) %>%
    select(PLOT_ID,SPECIES, YEAR,age.meas) %>%
    group_by(PLOT_ID, SPECIES)
  names(bored1) = c("PLOT_ID", "SPECIES","YEAR","AGE1")
  correct1 = left_join(bored0,bored1) %>%
    filter(!is.na(AGE1))
  unique(correct1$SPECIES)
  
  # only 2 species are covered in the above: TA and WS
  factor1 = mutate(correct1,factor=age.meas-AGE1)%>%
    group_by(SPECIES) %>%
    summarise(mean(factor))
  names(factor1)=c("sps","fact")
  # correction factors for the BF left to find
  # BS, JP, WB from Stan Vasiliauskas, Han YH Chen 
  #Canadian Journal of Forest Research, 2002, 32:1889-1892, 10.1139/x02-104
  # TL and BF will be the avg btw JP and BS
  # BP will be the TA value
  factor2 = data.frame(cbind("sps"=c("BS","JP","WB","TL","BF","BP"),"fact"=c(19,9,8,14,14,15)))
  agefactors = rbind(factor1,factor2)
# end of bored age factor calculation----------------------------------------------------------------

# Apply the bored age correction factors------------

names(agefactors)=c("SPECIES","fact")
skage2 = left_join(skage1,agefactors)
ind1 = which(skage2$BORED_HEIGHT==1.3)
ind2 = which(skage2$BORED_HEIGHT==0.3)
skage2$age.meas[ind1] = skage2$age.meas[ind1]+as.numeric(skage2$fact[ind1])
skage2$age.meas[ind2] = skage2$age.meas[ind2]+(as.numeric(skage2$fact[ind2])*(0.3/1.3))

skage3 <- select(skage2,PLOT_ID,TREE_NO,YEAR,SPECIES,age.meas)
# All ages corrected for boring height END ------------------

# Join to data frame and calculate the age for plots with ages

sk2k.1 <- left_join(sk2k,skage3)
      # how many plots have an age?
      plot.count <- group_by(sk2k.1,PLOT_ID) %>%
        summarise(plotno = mean(age.meas,na.rm=TRUE))
      dim(plot.count)#[1] 2048    2
      range(plot.count$plotno,na.rm=TRUE)
      #unique(plot.count$plotno)
      t1 = which(is.na(plot.count$plotno))
      plotWages = plot.count[-t1,]
      dim(plotWages)#[1] 1381    2

# trying to avoid adding trees
sk2k.w <- filter(sk2k.1,!is.na(age.meas))
sk2k.wo <- filter(sk2k.1,is.na(age.meas))
sk2k.wo <- unique(sk2k.wo)

age.by.plot <- group_by(sk2k.w,PLOT_ID) %>%
  summarise(n.ages = n())

# Average by plot, year and tree no (tree no will keep the species)----------------------
treeAvg <- group_by(sk2k.w,PLOT_ID,TREE_NO,YEAR) %>%
  summarise(treeAge = mean(age.meas,na.rm=TRUE)) %>%
  filter(!is.na(treeAge)) %>%
  mutate(tree.yr=YEAR) %>%
  select(-YEAR)
treeAvg <- unique(treeAvg)
checkAvg <- group_by(treeAvg,PLOT_ID, TREE_NO,tree.yr) %>%
  summarise(noavg = n()) %>%
  filter(noavg>1)
#names(treeAvg) = c("PLOT_ID","TREE_NO","tree.yr","treeAge")
sk2k.2 <- left_join(sk2k.wo,treeAvg) %>%
  mutate(fill1 = YEAR-tree.yr+treeAge) %>%
  select(-tree.yr,-treeAge)
sk2k.2 <- unique(sk2k.2)

## this added 4 trees...check why
extras <- group_by(sk2k.2,PLOT_ID,TREE_NO,YEAR) %>%
  summarise(extra = n()) %>%
  filter(extra>1) 
#get the plots
out1 <- which(sk2k.2$PLOT_ID==30086 & sk2k.2$TREE_NO==1 & sk2k.2$YEAR==1958)
out2 <- which(sk2k.2$PLOT_ID==30086 & sk2k.2$TREE_NO==1 & sk2k.2$YEAR==1963)
out3 <- which(sk2k.2$PLOT_ID==30086 & sk2k.2$TREE_NO==1 & sk2k.2$YEAR==1968)
out4 <- which(sk2k.2$PLOT_ID==30281 & sk2k.2$TREE_NO==3 & sk2k.2$YEAR==1992)
sk2k.2 <- sk2k.2[c(-out1[2],-out2[2],-out3[2],-out4[2]),]
# It is because the fill1 is not the same - happens in the left_join with treeAvg
# End of filling with ages from the same tree in another year ---------------------------

# Average by plot, year and species (all same sps in the same plot in the same year)-----------
spsAvg <- group_by(sk2k.w,PLOT_ID,SPECIES,YEAR) %>%
  summarise(spsAge = mean(age.meas)) %>%
  filter(!is.na(spsAge)) %>%
  mutate(sps.yr=YEAR)%>%
  select(-YEAR)
spsAvg <- unique(spsAvg)
checkspsAvg <- group_by(spsAvg,PLOT_ID,SPECIES) %>%
  summarise(nospsavg =n()) %>%
  filter(nospsavg>1)
checkspsyr <- left_join(checkspsAvg,spsAvg)
# there are no repeat years (sps.yr) but there are repeat averages, so average again
spsAvg1 <- group_by(spsAvg,PLOT_ID,SPECIES) %>%
  summarise(spsAge = mean(spsAge),sps.yr=mean(sps.yr))
sk2k.3 <- left_join(sk2k.wo,spsAvg1) %>%
  mutate(fill2 = YEAR-sps.yr+spsAge) %>%
  select(-sps.yr,-spsAge)
  
# End of filling with ages from the same sps in another year ---------------------------

# Average age of all trees across species for that plot in that year--------------------
plotAvg <- group_by(sk2k.w,PLOT_ID,YEAR) %>%
  summarise(plotAge = mean(age.meas,na.rm=TRUE)) %>%
  filter(!is.na(plotAge))%>%
  mutate(plot.yr = YEAR) %>%
  select(-YEAR)
plotAvg <- unique(plotAvg)
plotAvg1 <- group_by(plotAvg,PLOT_ID) %>%
  summarise(plotAge=mean(plotAge),plot.yr=mean(plot.yr))
sk2k.4 <- left_join(sk2k.wo,plotAvg1) %>%
  mutate(fill3 = YEAR-plot.yr+plotAge) %>%
  select(-plot.yr,-plotAge)
# End of filling with plot ages for that year ---------------------------

sk2k.5 <- inner_join(sk2k.2,sk2k.3) %>% inner_join(sk2k.4)
# Make one age column with priority on age.meas, then fill1, fill2, and fill3
age = sk2k.5$fill3

age[which(!is.na(sk2k.5$fill2))] = sk2k.5$fill2[which(!is.na(sk2k.5$fill2))]
# all the fill2 are in fill3...
# recheck the steps
age[which(!is.na(sk2k.5$fill1))] = sk2k.5$fill1[which(!is.na(sk2k.5$fill1))]
age[which(!is.na(sk2k.5$age.meas))] = sk2k.5$age.meas[which(!is.na(sk2k.5$age.meas))]

sk2k.6 <- cbind(sk2k.5, age)
sk2k.6 <- select(sk2k.6,-age.meas,-fill1,-fill2,-fill3)
sk2k.6 <- unique(sk2k.6)
# End of filling-in ages-------------------------------------------------------------------

# add the plots/years/trees that already had ages------------------------------------------
names(sk2k.w) = c("PLOT_ID","TREE_NO","YEAR","SPECIES","DBH","HEIGHT","TREE_STATUS","CONDITION_CODE1", 
                  "CONDITION_CODE2","CONDITION_CODE3","MORTALITY","INGROWTH","age")
sk2k.6 <- rbind(sk2k.6,sk2k.w)

# Checks: stillhave all the plots?
plotcheck1 <- unique(sk2k.6$PLOT_ID) # [1] 2048
# YES

# Checks: how many NAs?
ageNA <- sk2k.6[which(is.na(sk2k.6$age)),]# 649862 13
noplotNA <- length(unique(ageNA$PLOT_ID))
# Any of these dead trees?#Condition code
cond = group_by(sk2k.6,CONDITION_CODE1) %>%
  summarize(length(PLOT_ID))

dead2k <- filter(ageNA,(TREE_STATUS>2 | MORTALITY>0 | 
                          CONDITION_CODE1==25 | CONDITION_CODE1==26 |
                          CONDITION_CODE2==25 | CONDITION_CODE2==26 |
                          CONDITION_CODE3==25 | CONDITION_CODE3==26 ))
dim(dead2k)#[1] 22265    13
# get rid of dead trees 
sk2k.7 <- filter(sk2k.6, (TREE_STATUS<3 |
                             CONDITION_CODE1!=25 | CONDITION_CODE1!=26 |
                             CONDITION_CODE2!=25 | CONDITION_CODE2!=26 |
                             CONDITION_CODE3!=25 | CONDITION_CODE3!=26))
                 
# Dealing with remaining NAs--------------------
plotyr <- group_by(sk2k.7,PLOT_ID,YEAR)%>%
  summarise(tree_cycle=n(),avgAge=mean(age,na.rm=TRUE))
plotyr1 <- group_by(plotyr,PLOT_ID) %>%
  summarise(yrs_plot=n(),avgAge1=mean(avgAge))
plotsNA <- filter(plotyr1,is.na(avgAge1))
dim(sk2k.7)#[1] 833491     13
# 667 plots with no age, but they may have dbh, in which case a biomass can be calculated...
# for a given year but would not be able to get age.
# decided to get rid of no age plots
plotskeep <- filter(plotyr1,!is.na(avgAge1)) %>%
  select(PLOT_ID)
sk2k.8 <- inner_join(sk2k.7,plotskeep)
sk2k.8 <- unique(sk2k.8)
# All lines have age ----------------------------------------------------------------------

#fill in UI and XX species with species identified for that tree in another measurement year-------
xx <- filter(sk2k.8,SPECIES=="XX")
dim(xx) #[1] 5448   13
ui <- filter(sk2k.8, SPECIES=="UI")
dim(ui) #[1] 37558    13
ui.plots <- group_by(ui,PLOT_ID,TREE_NO) %>%
  summarise(noyrs = n()) %>%
  select(-noyrs)
ui.plots <- unique(ui.plots)
ui.plots1 <- left_join(ui.plots,sk2k.8) %>%
  filter(SPECIES!="UI") %>%
  select(PLOT_ID, TREE_NO,ui.sps=SPECIES)
ui.plots1 <- unique(ui.plots1)
ui.plots2 <- group_by(ui.plots1,PLOT_ID,TREE_NO) %>%
  summarise(n.sps = n(),ui.sps=first(ui.sps)) %>%
  select(-n.sps)
ui.plots2 <- unique(ui.plots2)

xx.plots <- group_by(xx,PLOT_ID,TREE_NO) %>%
  summarise(noyrs = n()) %>%
  select(-noyrs)
xx.plots <- unique(xx.plots)
xx.plots1 <- left_join(xx.plots,sk2k.8) %>%
  filter(SPECIES!="XX") %>%
  select(PLOT_ID, TREE_NO,xx.sps=SPECIES)
xx.plots1 <- unique(xx.plots1)
xx.plots2 <- group_by(xx.plots1,PLOT_ID,TREE_NO) %>%
  summarise(n.sps = n(),xx.sps=first(xx.sps)) %>%
  select(-n.sps)
xx.plots2 <- unique(xx.plots2)

# 4 UI and 4 XX from the remplacements
ui4 <- xx.plots2[xx.plots2$xx.sps=="UI",]
ui4.1 <- inner_join(ui4,sk2k.8)
xx4 <- ui.plots2[ui.plots2$ui.sps=="XX",] # all the same trees

# go get the dominant species for those plots and that will be those species---
xx4.plots <- inner_join(xx4,sk2k.8,by="PLOT_ID") %>%
  select(PLOT_ID,TREE_NO=TREE_NO.y,YEAR,SPECIES)
dom1 <- group_by(xx4.plots,PLOT_ID,YEAR) %>%
  summarise(totaltrees=n())
dom2 <- group_by(xx4.plots,PLOT_ID,YEAR,SPECIES) %>%
  summarise(spstrees=n())
dom3 <- left_join(dom2,dom1) %>%
  mutate(prop = spstrees/totaltrees) %>%
  arrange(PLOT_ID,YEAR,prop) %>%
  group_by(PLOT_ID,YEAR) %>%
  summarise(dom=last(SPECIES))
# replacing the one year with XX with the dominant species (WS) from all other years
dom3[dom3$dom=="XX",3]="WS"
# got the dom species----------------------------------------------------------

# replace the 4 UI and and 4 XX species with the dominant species
sk2k.8.1 <- left_join(sk2k.8,dom3)
sk2k.8.1[sk2k.8.1$SPECIES=="XX"|sk2k.8.1$SPECIES=="UI",4] = sk2k.8.1[sk2k.8.1$SPECIES=="XX"|sk2k.8.1$SPECIES=="UI",14]
# End of filling in UI and XX with species from another year------------------------------------

# Last clean-up -----------------------------
sk2k.9 <- select(sk2k.8.1, -TREE_STATUS,-CONDITION_CODE1,-CONDITION_CODE2,-CONDITION_CODE3,-MORTALITY,-dom)
ageNA1 <- filter(sk2k.9, is.na(age))
# every one of the lines has an age.
# Clean-up finished--------------------------

#----Save file
write.table(sk2k.9,file="C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/SK_2000PlotsCleaned.txt",
            sep=",",row.names = FALSE)       

