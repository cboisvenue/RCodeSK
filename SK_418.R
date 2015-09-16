#----------------------------
#
# PSP data Saskatchewan - the 418 plots with location accuracy of 10 m or better.
#
# Goals: 
# get a temporal profile for each of the 418 plots (age, measurement year)
# get all the tree level measurements (species, height, dbh, and age) in order for growth layer development
# and/or simple biomass calculations using Lambert et al equations
#
# CBoisvenue
# Sept.29th, 2014
#----------------------------

# Admin --------------------------------------------
#install.packages("plyr")
require(plyr)
#install.packages("ggplot2")
require(ggplot2)
#install.packages("dplyr")
require(dplyr)

setwd("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04")
#--------------

# 1 read in the tree data--------------------------------------------------------
sk.tree <- read.csv("trees.csv", header=TRUE)
dim(sk.tree) #[1] 845981     21

sk.raw1 <- read.table("plot_header.csv", header=TRUE, sep=",")
loc.raw = select(sk.raw1, LOC_ACCURACY, PLOT_ID,contains("Z13nad83"))
# check in we have plot numbers repeated
dim(loc.raw) #[1] 2048   13
length(unique(loc.raw[,2])) # 2048 ##NO - GOOD.

# 2 Check location accuracy as per Joanne White's recommendation-------
loc.acc10 = filter(loc.raw, LOC_ACCURACY <=10) %>% #418
  select(PLOT_ID,contains("Z13nad83"))

# keeping the 418 plots and the variables needed from this tree file
sk418 = inner_join(loc.acc10,sk.tree) 
sk418 =  select(sk418,PLOT_ID,Z13nad83_e,Z13nad83_n,TREE_NO,YEAR,SPECIES,DBH,HEIGHT,TREE_STATUS,CONDITION_CODE1,CONDITION_CODE2,CONDITION_CODE3,MORTALITY,INGROWTH)
# 168793     15

# check the range of variables to see if I need to deal with the same issues as the L1 code and
# to see if I need them all-----------
      
      range(sk418$YEAR)#[1] 1949 2009
      
      unique(sk418$SPECIES) # factor:  [1] BS WS TL UI BP XX WB TA JP BF MM
      
      # dbh
      range(sk418$DBH,na.rm=TRUE)#0 116
      dbh0 = filter(sk418, DBH==0) #662 15
      dbhNA = filter(sk418, is.na(DBH))#1759 15
      
      # height
      range(sk418$HEIGHT,na.rm=TRUE)#0.2 40
      htNA = filter(sk418, is.na(HEIGHT))#2419 15
      
      # status
      unique(sk418$TREE_STATUS)#[1] 1 3 2 8 7 4 6 0 5 9
      dead418 = filter(sk418, TREE_STATUS>2)#17358 15
      #Condition code
      cond = group_by(sk418,CONDITION_CODE1) %>%
        summarize(length(PLOT_ID))
      # mort
      mort = group_by(sk418,MORTALITY) %>%
        summarize(length(PLOT_ID))
      sum(mort[1:8,2])#15014
      # ingrowth
      ingrowth = group_by(sk418, INGROWTH) %>%
        summarize(length(PLOT_ID))

# 3 Add ages----------------------------------------------------------------------

sk.age <- read.table("age_samples.csv", header=TRUE, sep=",")
# keep only the 418 plots
age418 = inner_join(loc.acc10,sk.age) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES, OFF_PLOT_TREE, BORED_HEIGHT,COUNTED_AGE,TOTAL_AGE)
      # # checking one plot...
      # age.20004 = select(age418,PLOT_ID,TREE_NO,YEAR,SPECIES, OFF_PLOT_TREE, BORED_HEIGHT,COUNTED_AGE,TOTAL_AGE) %>%
      #                    filter(PLOT_ID==20004)
      # sk.20004 = filter(sk418,PLOT_ID==20004 & TREE_NO==4 |PLOT_ID==20004 & TREE_NO==6 |PLOT_ID==20004 & TREE_NO==21 |PLOT_ID==20004 & TREE_NO==9601 |PLOT_ID==20004 & TREE_NO==9602 |PLOT_ID==20004 & TREE_NO==9603)
      #plot.20004 <- merge(sk.20004,age.20004, all=TRUE)

sk418 <- merge(sk418,age418,all=TRUE)

# 4 how many have ages that need to be ajusted for boring height?----------------
unique(sk418$BORED_HEIGHT)
corr.ht = group_by(sk418,BORED_HEIGHT) %>%
  summarise(n())
# 0.3      7
# 1.3   1813
# what species are the 1813?
sps1.3 = filter(sk418,BORED_HEIGHT==1.3) %>%
  group_by(SPECIES) %>%
  summarize(n())
range(sk418$COUNTED_AGE)
counted.age = filter(sk418,!is.na(COUNTED_AGE)) 
dim(counted.age) # 2631 18
total.age = filter(sk418,!is.na(TOTAL_AGE)) 
dim(total.age) # 2631 18

# 5 tried to use the existing relationship...----------------
      # # are there trees with counted age and total age that were measured at 1.3?
      # age1 = select(sk418,PLOT_ID,TREE_NO,SPECIES,BORED_HEIGHT,COUNTED_AGE, TOTAL_AGE) %>%
      #   filter(BORED_HEIGHT==1.3) %>%
      #   mutate(correct1=TOTAL_AGE-COUNTED_AGE) 
      # unique(age1$correct1)
      # ## NO
      # # check again
      # age2 = select(sk418,PLOT_ID,TREE_NO,SPECIES,BORED_HEIGHT,COUNTED_AGE, TOTAL_AGE) %>%
      #   filter(!is.na(TOTAL_AGE) &!is.na(COUNTED_AGE) & BORED_HEIGHT==1.3) 
      # ## STILL NO

# 6 check if any of the non-418 plots has both counted and total ages-----------------
    # age3 = select(sk.age,PLOT_ID,TREE_NO,SPECIES,BORED_HEIGHT,COUNTED_AGE, TOTAL_AGE) %>%
    #   filter(BORED_HEIGHT==1.3) %>%
    #   mutate(correct1=TOTAL_AGE-COUNTED_AGE) 
    # unique(age1$correct1)
    # age4 = select(sk.age,PLOT_ID,TREE_NO,SPECIES,BORED_HEIGHT,COUNTED_AGE, TOTAL_AGE) %>%
    #   filter(!is.na(TOTAL_AGE) &!is.na(COUNTED_AGE) & BORED_HEIGHT==1.3) 
    ## STILL NO

# 7 developping a bored age correction factor--------------------------
    # group by species differences between ages bored at 0 and those ar 1.3
    age5 = select(sk.age,PLOT_ID,TREE_NO,YEAR, SPECIES,BORED_HEIGHT,COUNTED_AGE, TOTAL_AGE) %>%
      group_by(SPECIES)
    bored0 = filter(age5,BORED_HEIGHT==0) %>%
      select(PLOT_ID,SPECIES,YEAR, COUNTED_AGE)%>%
      group_by(PLOT_ID, SPECIES)
    bored1 = filter(age5, BORED_HEIGHT==1.3) %>%
      select(PLOT_ID,SPECIES, YEAR,COUNTED_AGE)%>%
      group_by(PLOT_ID, SPECIES)
    names(bored1) = c("PLOT_ID", "SPECIES","YEAR","AGE1")
    correct1 = left_join(bored0,bored1) %>%
      filter(!is.na(AGE1))
    # only 2 species are covered in the above: TA and WS
    factor1 = mutate(correct1,factor=COUNTED_AGE-AGE1) %>%
      group_by(SPECIES) %>%
      summarise(mean(factor))
    names(factor1)=c("sps","fact")
    # correction factors for the 1813 BF left to find
    # BS, JP, WB from Stan Vasiliauskas, Han YH Chen 
    #Canadian Journal of Forest Research, 2002, 32:1889-1892, 10.1139/x02-104
    # TL and BF will be the avg btw JP and BS
    # BP will be the TA value
    factor2 = data.frame(cbind("sps"=c("BS","JP","WB","TL","BF","BP"),"fact"=c(19,9,8,14,14,15)))
    agefactors = rbind(factor1,factor2)


# 8 calculate average counted and total ages by plot, year, and species and tree_no-----------
avg.age = select(sk.age,PLOT_ID,TREE_NO,YEAR,SPECIES,COUNTED_AGE,TOTAL_AGE)%>%
  group_by(PLOT_ID,TREE_NO,YEAR,SPECIES) %>%
  summarise_each(funs(mean))
keep1 = which(!is.na(avg.age$TOTAL_AGE))
keep2 = which(!is.na(avg.age$COUNTED_AGE))
keep = sort(c(keep1,keep2))
avg.age = avg.age[keep,]
avg.age=unique(avg.age)
# create one coloumn that is age with no NAs
fromTOTAL = which(!is.na(avg.age$TOTAL_AGE))
fromCOUNTED = which(!is.na(avg.age$COUNTED_AGE))
age2 = vector(mode='numeric',length=dim(avg.age)[1])
age2[fromCOUNTED]=avg.age$COUNTED_AGE[fromCOUNTED]
age2[fromTOTAL]=avg.age$TOTAL_AGE[fromTOTAL]
avg.age = cbind(avg.age[,1:3],age2)

# 9 make an age variable that will have a value for every line------------
age = sk418$COUNTED_AGE
length(age) #170401
length(which(is.na(age)))#[1] 167770
keep5 = which(sk418$BORED_HEIGHT==1.3 & !is.na(sk418$COUNTED_AGE))

# NOTE: TOTAL_AGE will override this later on, so don't worry about TOTAL_AGE for now
age1.3 = select(sk418,PLOT_ID,TREE_NO,YEAR,SPECIES,DBH,HEIGHT,BORED_HEIGHT,COUNTED_AGE,TOTAL_AGE) %>%
  filter(BORED_HEIGHT==1.3 & !is.na(sk418$COUNTED_AGE))

# 10 attach the right correction factor for bored age (agedfactors from code block 7)---------
names(agefactors)=c("SPECIES","fact")
age1.3 = left_join(age1.3,agefactors)
age3 = age1.3$COUNTED_AGE+as.numeric(age1.3$fact)

age[keep5] = age3
length(which(is.na(age))) # still 16770 but the 1.3 were corrected

# 11 Here is where TOTAL_AGE overrided anything above---------------------
keep3 = which(!is.na(sk418$TOTAL_AGE))
age[keep3] = sk418$TOTAL_AGE[keep3]
length(which(is.na(age))) #[1] 167739
still.na1 = which(is.na(age))

# 12 Average Age per plot---------------------
# for those still NA, try to match the avg.age (by plot, yr, tree, sps)
match.avg = sk418[still.na1,]
match.avg = left_join(match.avg,avg.age) #Joining by: c("PLOT_ID", "TREE_NO", "YEAR")
# any gain?
still.na2 = which(is.na(match.avg$age2))
length(still.na2)#[1] 167113
# Yes 657 gain
length(match.avg$age2)
age[still.na1] = match.avg$age2
length(age)
length(which(is.na(age))) #167113

# add age to sk418 
sk418 = cbind(sk418[,1:18],age)

# 13 these are probably repeat measurements. do all the plots have at least one age?------------
plt.age = group_by(sk418,PLOT_ID,YEAR,SPECIES) %>%
  summarise(mean(age, na.rm=TRUE))
length(which(is.na(plt.age[,4]))) #[1] 2869


## 14 -------Calculating the age at all censuses when one was measured-------


### CALCULATE AGE FOR PLOTS that have an age--------
ageprofile <- select(sk418,PLOT_ID,TREE_NO,YEAR,SPECIES,age) %>%
  #filter(PLOT_ID==20004,TREE_NO==4) %>%
  arrange(PLOT_ID,TREE_NO,YEAR) #%>%

wh <- mutate(ageprofile, wh=!is.na(ageprofile$age)) %>%
  filter(wh==TRUE) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,age)

names(wh) = c("PLOT_ID","TREE_NO","yratwh","SPECIES","whage")

ageprofile <- left_join(ageprofile,wh) %>%
  mutate(fill.age =YEAR-yratwh+whage)
# I gained 4 lines here...not sure why.
ageprofile <- select(ageprofile,PLOT_ID,TREE_NO,YEAR,SPECIES,fill.age)
sk1 <- left_join(sk418,ageprofile)
length(which(!is.na(sk1$age))) #[1] 3292
length(which(!is.na(sk1$fill.age))) #[1] 4315

# link the average values by species----------------------##############
names(plt.age) = c("PLOT_ID","YEAR","SPECIES","avg.age")
sk2 <- left_join(sk1,plt.age) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,age,avg.age) %>%
  arrange(PLOT_ID,TREE_NO,SPECIES,YEAR)
wh2 <- mutate(sk2,wh2=!is.na(sk2$avg.age)) %>%
  filter(wh2==TRUE) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,avg.age)
names(wh2) = c("PLOT_ID","TREE_NO","yratwh2","SPECIES","wh2age")

### PROBLEM: there is more than 1 average per plot...choose the 1st?
prob1 = group_by(wh2,PLOT_ID,TREE_NO,SPECIES) %>%
  summarise(first(yratwh2))
prob2 = group_by(wh2,PLOT_ID,TREE_NO,SPECIES) %>%
  summarise(first(wh2age))
prob1 = cbind(prob1,prob2[,4])
names(prob1) = c("PLOT_ID","TREE_NO", "SPECIES","yratwh3","wh3age")
prob3 = left_join(prob1,wh2)
prob3 = prob3[,1:5]
prob3 = unique(prob3)

avgprofile <- left_join(sk2,prob3) %>%
  mutate(fill2.age = YEAR-yratwh3+wh3age)
avgprofile <- select(avgprofile,PLOT_ID,TREE_NO,YEAR,SPECIES,fill2.age)

# this data database should have three age columns: age - the combination of what was 
# in the original db, fill.age - the plots with one age measured with the other censuses 
# filled-in, and fill2.age - the plots that were filled-in with average plot age.
sk3 <- left_join(sk1,avgprofile)
# gained 8 lines here

# keep one age per line. Rules: "age" gets priority, fill-age, 2nd, and fill2age, 3rd.-----
agecombo = sk3$fill2.age
# how many NAs?
length(which(is.na(agecombo)))#62595
# fill with fill.age
keep4 = which(!is.na(sk3$fill.age))#4323
agecombo[keep4] = sk3$fill.age[keep4]
length(which(is.na(agecombo)))#62595
keep8 = which(!is.na(sk3$age))
agecombo[keep8] = sk3$age[keep8]
length(which(is.na(agecombo)))

sk418 <- select(sk418,PLOT_ID, TREE_NO, YEAR, SPECIES, Z13nad83_e, Z13nad83_n,DBH,HEIGHT, 
               TREE_STATUS,CONDITION_CODE1,MORTALITY,INGROWTH,OFF_PLOT_TREE)
sk3 <- cbind(select(sk3,PLOT_ID, TREE_NO, YEAR, SPECIES),agecombo)
sk4 <- left_join(sk418,sk3)
sk4 <- unique(sk4)

# sk4 has "all possible caculation of age" from the info in the raw data, by plot and tree combo
### STILL 62595 NAs
# now checking why not all plots have ages---------------
noage <- filter(sk4,is.na(agecombo))
dead <- filter(noage,TREE_STATUS>2 | !is.na(MORTALITY)) 
#13578 dead
noage <- filter(sk4,is.na(agecombo) & (TREE_STATUS<3 | is.na(MORTALITY))) #50952
noageplots <- unique(noage$PLOT_ID)
indb <- which(noageplots %in% sk4$PLOT_ID[which(!is.na(sk4$agecombo))])
# notindb  <- which(noageplots %in% sk4$PLOT_ID[which(is.na(sk4$agecombo))])

length(noageplots)
length(indb)
# all plots but one has ages for another species
noageplots[169] # plot 300415 - had figure this out already (see commented code below)
# # All plots but ONE have an age.--------------------
#       plt.age[194,]
#       noageplot = select(sk418,PLOT_ID,TREE_NO,YEAR,SPECIES,DBH,HEIGHT,TREE_STATUS) %>%
#         filter(PLOT_ID==300415) 
#       by_plot <- noageplot %>% group_by(PLOT_ID)
#       by_plot %>% summarise_each(funs(ul = length(unique(.))))
#       by_sps <- noageplot %>% select(SPECIES,DBH,HEIGHT) %>%
#         group_by(SPECIES) %>%
#         summarise_each(funs(min, max))
#       # SPECIES DBH_min HEIGHT_min DBH_max HEIGHT_max
#       # 1      JP    12.1       13.2    21.0       18.0
#       # 2      TA     1.9        4.1    18.2       19.2
#       # 3      WB     1.7        3.8     8.5       10.6
#       # 4      WS     1.4        2.6    15.6       15.0
#       ## based on heights and assumption of average site quality and http://www.pamodelforest.sk.ca/pubs/PAMF2900.pdf
#       # JP is 50, TA is 50, WS is 50 WB has no numbers so 50
#       # plot 300415 has only one measurement year so age==50 for all species measurements.
#       add.age <- which(sk418$PLOT_ID==300415 & sk418$HEIGHT==18.0)
#       sk418$age[add.age]=50
# length(which(is.na(sk418$age))) #87438
#----------
add.age <- which(sk4$PLOT_ID==300415 & sk4$HEIGHT==18.0)
# all the years 1955 for that plot?
yr.check <- filter(sk4, PLOT_ID==300415) %>%
  summarise(unique(YEAR))
# YES - so all ages for this plot in 50 and I can't use it anyway...
plt300415 = which(sk4$PLOT_ID==300415)
sk4$agecombo[sk4$PLOT_ID==300415] = rep(50,length(plt300415))

# the missing ages are for species that where not measured ----------------
# find the age for each year and spread it to the species with no ages-----
# Note that this is the 1st speculation on age
              # # check species
              # check1 <- select(sk4, PLOT_ID,YEAR,agecombo) %>%
              #   filter(!is.na(agecombo))#PLOT_ID==20004, 
              # names(check1) = c("PLOT_ID", "YEAR", "pltage")
              # matchyr <- left_join(sk4,check1)
              # # for all the is.an(agecombo), they are equal to pltage
              # matchyr$agecombo[which(is.na(matchyr$agecombo))] = matchyr$pltage[which(is.na(matchyr$agecombo))]
              # matchyr = matchyr[,1:14]
              # matchyr = unique(matchyr)
pltprofile <- select(sk4,PLOT_ID,YEAR,agecombo) %>%
  arrange(PLOT_ID,YEAR) #%>%
#pltprofile <- unique(pltprofile)
# these are the ones to change in sk4
ind3 = which(!is.na(pltprofile$agecombo))
wh4 <- mutate(pltprofile, wh4=!is.na(pltprofile$agecombo)) %>%
  filter(wh4==TRUE) %>%
  select(PLOT_ID,YEAR,agecombo) %>%
  arrange(PLOT_ID,YEAR)
names(wh4) = c("PLOT_ID","yratwh4","wh4age")
yratwh5 = vector(mode='numeric',length=dim(sk4)[1])
yratwh5[ind3] = wh4$yratwh4
wh5age = vector(mode='numeric',length=dim(sk4)[1])
wh5age[ind3]= wh4$wh4age
fill3.age = vector(mode='numeric', length=dim(sk4)[1])
fill3.age =pltprofile$YEAR-yratwh5+wh5age
ind5 = which(pltprofile$YEAR==fill3.age)
fill3.age[ind5] = NA

sk5 <- cbind(sk4,fill3.age)
ind1 = which(is.na(sk5$agecombo))
sk5$agecombo[ind1] = sk5$fill3.age[ind1]
sk5 <- sk5[,1:14]
length(which(is.na(sk5$agecombo))) #28841
ind7=which(is.na(sk5$agecombo))
length(unique(sk5$PLOT_ID[-ind7])) #418
## still have all the plots - will deal NAs as they come
#countNAs <- filter(sk5,is.na(agecombo) & (TREE_STATUS>2 | MORTALITY==1)) #5978


# SPECIES CLEAN UP -----------------
# check the UI species since there are so many of them
ui <- filter(sk5,SPECIES=="UI")
# how many of those dead?
ui_dead <- filter(ui,TREE_STATUS>2 | MORTALITY==1)
## ALL OF THEM - Remove!
# all UI SPECIES are dead trees - removing
sk5 <- filter(sk5,SPECIES!="UI")
length(unique(sk5$PLOT_ID))
# checking XX species
xx <- filter(sk5, SPECIES=="XX")
# how many are missing?
xx26 <- filter(xx,CONDITION_CODE1==26) #1608 and the other one is a stump.
# Remove all XX species
sk5 <- filter(sk5,SPECIES!="XX")
length(unique(sk5$PLOT_ID))
# NOTE: the MM is one tree measured three times
filter(sk5,SPECIES=="MM")
#----Save file
write.table(sk5,file="C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/SK_418PlotsCleaned1.txt",sep=",")       

