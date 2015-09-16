#------------------------------------------------------------
# Builing a Random Forest model for the 418 PS in SK with
# calculated biomass
#
# CBoisvenue
# November 5th, 2014
#-----------------------------------------------------------


require(plyr)
#install.packages("ggplot2")
require(ggplot2)
#install.packages("dplyr")
require(dplyr)
#  install.packages("gridExtra")
library(gridExtra)  

require(randomForest)
require(reshape2)

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/")

#1 Read in data ------------------------------------------------------------------------
# Read in plot-level biomass data
psp <- read.table("./data/SK418Biomass_ha.txt",header=TRUE,sep=",")

# side track: spit-out utms and plot IDs for Byron
psp_utm <- select(psp,PLOT_ID,PLOT_SIZE,contains("Z13"))
psp_utm <- unique(psp_utm)

# SPECIES COMPARISON ----------------------------------------------
# get psp sps
tree <- read.table("./data/SK418TreeBiomass.txt",header=TRUE,sep=",")
# get rid of the off-plot trees and keep plot, year, sps, biomass per tree
sps <- filter(tree,OFF_PLOT_TREE!="Y" | is.na(OFF_PLOT_TREE)) %>%
  select(PLOT_ID,YEAR,SPECIES,biomass)
# species % by tree count and % by proportion of biomass
    sps_no <- group_by(sps,PLOT_ID,YEAR,SPECIES) %>%
      summarise(n())
    sps_treetotal <- group_by(sps,PLOT_ID,YEAR) %>%
      summarise(n())
    sps1 <- left_join(sps_no,sps_treetotal, by=c("PLOT_ID", "YEAR")) 
    # proportion by counting the number in a sps over the overall number of trees
    dem_prop = sps1[,4]/sps1[,5]
    # now figure out the sps proportion of the total biomass
    sps_biom <- group_by(sps,PLOT_ID,YEAR,SPECIES) %>%
      summarise(sum(biomass))
    sps_biomtotal <- group_by(sps,PLOT_ID,YEAR) %>%
      summarise(sum(biomass))
    sps2 <- left_join(sps_biom,sps_biomtotal, by=c("PLOT_ID", "YEAR")) 
    biom_prop = sps2[,4]/sps2[,5]
    sps_by_plot <- cbind(sps1[,1:3],dem_prop,biom_prop)
# find the last year for each plot NOTE: comparing the last year with species2010 database
# This may or may not be the best way
sps.last <- arrange(sps_by_plot,PLOT_ID,YEAR) %>%
  group_by(PLOT_ID) %>%
  summarize(last(YEAR))
names(sps.last) <- c("PLOT_ID","YEAR")
spslastyr <- inner_join(sps.last,sps_by_plot)
spslastyr <- as.data.frame(spslastyr)
    # # get this into wide format for comparison with rs data
    # spsw1 <- dcast(spslastyr, PLOT_ID+YEAR~SPECIES,value.var="biom_prop")
    # spsw2 <- dcast(spslastyr, PLOT_ID+YEAR~SPECIES,value.var="dem_prop")
    # # change species names to match rs data
    # names(spsw1)=c("PLOT_ID","YEAR","BF_biomp","BP_biomp","picemar_biomp","pinuban_biomp","poputre_biomp","larilar_biomp","betupap_biomp","picegla_biomp")
    # names(spsw2)=c("PLOT_ID","YEAR","BF_demp","BP_demp","picemar_demp","pinuban_demp","poputre_demp","larilar_demp","betupap_demp","picegla_demp")
    # spsw<- cbind(spsw1,spsw2[,3:10])
    # # make NAs 0
    # spsw[is.na(spsw)]<- 0
# NOT ABLE TO GET A GOOD GRAPH FOR COMPARISON YET
# ggplot(data=spslastyr,aes(x=factor(PLOT_ID),y=dem_prop,fill=SPECIES)) + geom_bar(stat="identity")
# sps_dem <- 
# a <- ggplot(spslastyr,aes(PLOT_ID,fill=SPECIES))
# a + geom_bar(stat="identity")
# ,x=PLOT_ID) +geom_bar(stat="identity",weight=dem_prop)

#THIS IS AN EXAMPLE
# ggplot(data=skTree_byAge) + geom_bar(aes(age, fill=SPECIES),colour="black") +
#   xlab("Age") + ylab("Number of trees measured") +
#   ggtitle("Measured Trees by Age and Species - SK 418") + 
#   theme(plot.title = element_text(lineheight=1.2, face="bold"))
# g+annotation_custom(tableGrob(meas_sps),xmin=175, xmax=225,ymin=7500,ymax=14000) 


# species presence absence by plot (not year for now)
    pa <- table(sps1$PLOT_ID,sps1$SPECIES)
    pa<- as.matrix(pa)
    pa <- 1*(pa>0)
    colnames(pa)
    colnames(pa)=c("BF","BP","picemar","pinuban","MM","poputre","larilar","betupap","picegla")
    pa6 <- pa[,c(8,7,9,3,4,6)]
# maybe by year? not for now...let's see what the results say

# read in remoting sensing data ---------------------------------
rs.raw <-read.table("./data/SK_psp_extract.txt",header=TRUE,sep=",")

# SPECIES extract species binary variables ----------------------------
  rs.spsbin <- select(rs.raw,PLOT_ID,betupap,larilar,picegla,picemar,pinuban,poputre)
  rs.sps01 <- as.matrix(rs.spsbin[,2:7])
  rownames(rs.sps01) <-as.character(rs.spsbin[,1])
# extract species proportion variables
rs.sps.p <- select(rs.raw,PLOT_ID,contains("_p"))
rs.sps.pl <- melt(rs.sps.p,id.vars="PLOT_ID",variable.name="species",value.name="prop")
rs.sps.pl <- arrange(rs.sps.pl,PLOT_ID, species)

# read-in knn ages ---------------------------------------------------
ages <- read.table("./data/PSP_UTM_wKnnAGE.csv", header=TRUE,sep=",")
ages <- ages[,c(2,7)]
names(ages) =c("PLOT_ID","knnage")

#2 Comparing species --------------------------------------------------------------
# Binary sps comparison
# multiply pa06 by a scalar and add rs.sps01. This will give 4 and 0 as a match
# and 3 and 1 as a no-match
pa6 <- pa6*3
sps.diff <- pa6+rs.sps01
table(sps.diff)
# each species was done individually table(sps.diff[,6])
# for each plot
match <-vector(mode="numeric", length=418)
prop_match <-vector(mode="numeric", length=418)
nomatch <-vector(mode="numeric", length=418)
prop_nomatch <-vector(mode="numeric", length=418)

for(i in 1:418){
  tab <- table(sps.diff[i,])
  match[i] = tryCatch(tab[[which(names(tab)=="0")]], error=function(a){0})+tryCatch(tab[[which(names(tab)=="4")]], error=function(a){0})
  nomatch[i] = tryCatch(tab[[which(names(tab)=="3")]], error=function(a){0})+tryCatch(tab[[which(names(tab)=="1")]], error=function(a){0})
  prop_match[i] = match[i]/6
  prop_nomatch[i] = nomatch[i]/6
}

sps.match <-as.data.frame(cbind(psp_utm$PLOT_ID,prop_match,prop_nomatch,match,nomatch))
names(sps.match) <-c("PLOT_ID","prop_match","prop_nomatch","match","nomatch")
write.table(sps.match,file="./data/SK2010RS_spsAgainstPSP418.txt",sep=",")
# species comparison is completed in an excel sheet (G:\RES_Work\Work\JoanneWhite\SK_work\GrowthRaster\Evaluating2010SpeciesRaster.xlsx)
plot_matches <- ggplot(data=sps.match) + geom_bar(aes(match))
# Binary sps comparison end

## Proportions sps comparison
#make factor levels match "BP"="BP","BF"="BF",      
spslastyr$SPECIES<-revalue(spslastyr$SPECIES, c("WB"="betupap_p","TL"="larilar_p", "WS"="picegla_p", "BS"="picemar_p","JP"="pinuban_p", "TA"="poputre_p"))
sps.psp.p <- select(spslastyr,-YEAR) %>%
  arrange(PLOT_ID,SPECIES)
names(rs.sps.pl) = c("PLOT_ID","SPECIES","rs_prop")

spsw.biom <- dcast(sps.psp.p, PLOT_ID~SPECIES,value.var="biom_prop")
spsw.biom[is.na(spsw.biom)] <- 0
spsw.dem <- dcast(sps.psp.p, PLOT_ID~SPECIES,value.var="dem_prop")
spsw.dem[is.na(spsw.dem)] <- 0
# NEXT STEPS: reorder to match and use "diag(cor(t(df1),t(df2)))"
spsw.biom1 <- spsw.biom[,c(1,8,7,9,4,5,6)]
spsw.dem1 <- spsw.dem[,c(1,8,7,9,4,5,6)]
# make sure they are in the same order of PLOT_ID
spsw.biom1$PLOT_ID %in% rs.sps.p$PLOT_ID
spsw.dem1$PLOT_ID %in% rs.sps.p$PLOT_ID
# correlation 
sps.rs <- select(rs.sps.p,-PLOT_ID)
sps.biom <- select(spsw.biom1,-PLOT_ID)
sps.dem <- select(spsw.dem1,-PLOT_ID)
diag(cor(sps.rs,sps.biom))
sps.cor <- cor(sps.rs,sps.biom)
sps.cor2 <- cor(sps.rs,sps.dem)
write.table(x = sps.cor,file = "./GrowthRaster/Results/rf.1/SK418_sps_compCOR.txt", sep=",")
write.table(x = sps.cor2,file = "./GrowthRaster/Results/rf.1/SK418_sps_compCORdemographic.txt", sep=",")

# dominant sps
psp.dom.biom <- arrange(sps.psp.p,PLOT_ID,-biom_prop) %>%
  group_by(PLOT_ID) %>%
  summarise(first(as.character(SPECIES)),first(biom_prop))
psp.dom.dem <- arrange(sps.psp.p,PLOT_ID,-dem_prop) %>%
  group_by(PLOT_ID) %>%
  summarise(first(as.character(SPECIES)),first(dem_prop))
rs.dom <- arrange(rs.sps.pl,PLOT_ID,-rs_prop)%>%
  group_by(PLOT_ID) %>%
  summarise(first(as.character(SPECIES)),first(rs_prop))

length(which(rs.dom[,1]!= psp.dom.biom[,1]))# 0
length(which(rs.dom[,2]!= psp.dom.biom[,2]))# 266
match <- rep("YES",length=dim(rs.dom)[[1]])
match[which(rs.dom[,2]!= psp.dom.biom[,2])] = "NO"
diff <- rs.dom[,3]-psp.dom.biom[,3]
dom.diff <- as.data.frame(cbind(as.factor(match),diff))

dom.p.diff <- ggplot(dom.diff,aes(x=diff,fill=match)) + 
  geom_histogram(binwidth=.1,alpha=.5,position="identity") +
  ggtitle("Differences in proportion of leading species for matching leading and non-matching") 

length(which(psp.dom.dem[,2]!= psp.dom.biom[,2]))# 61
match1 <- rep("YES",length=dim(psp.dom.biom)[[1]])
match1[which(psp.dom.dem[,2]!= psp.dom.biom[,2])] = "NO"
diff1 <- psp.dom.dem[,3]-psp.dom.biom[,3]
psp.check1 <- as.data.frame(cbind(as.factor(match1),diff1))
psp.dem.biom <- ggplot(psp.check1,aes(x=diff1,fill=match1)) + 
  geom_histogram(binwidth=.1,alpha=.5,position="identity") +
  ggtitle("Differences in proportion of leading species biomass vs demographic for matching leading and non-matching") 

sps.match <- psp.dom.biom[which(rs.dom[,2]== psp.dom.biom[,2]),]
spsnomatch <- cbind(psp.dom.biom[which(rs.dom[,2]!= psp.dom.biom[,2]),],rs.dom[which(rs.dom[,2]!= psp.dom.biom[,2]),2])

names(sps.match)<- c("PLOT_ID","lead","biom_prop")
sps.match1 <- group_by(sps.match,lead) %>%
  summarise(n())

names(spsnomatch) = c("PLOT_ID","lead","biom_prop","rs_lead")
spsnomatch1 <- group_by(spsnomatch,lead) %>%
  summarise(n())
spsnomatch2 <- group_by(spsnomatch,rs_lead) %>%
  summarise(n())

#3 Data clean and merge -----------------------------------------------------------------------
psp1 <- select(psp,PLOT_ID,YEAR,biom.ha)
# add ages
# ASSUMPTION: knn ages are ages of the pixels in 2001
knnyr <- rep(2001,dim(psp1)[[1]])
df1 <- left_join(psp1,ages)
df1 <- cbind(df1,knnyr)
# calculate all the other years
df2 <- mutate(df1,age = YEAR-knnyr+knnage)
# can't have negatives
df2$age[which(df2$age<=0)] = df2$knnage[which(df2$age<=0)]
df3 <- select(df2,PLOT_ID,YEAR,biom.ha,age)
## NOTE: would be good to compare this to plot-level ages

# rs data : these are the non-time variant columns
# this first model will be run with the sps2010 in proportions (as opposed to the binary)
rs1 <- rs.raw[,c(1,4:9,16:19)]

# time-variant columns - need to be put in long format
rs2 <- rs.raw[,c(1,20:193)]
rs3 <- melt(rs2,id.vars="PLOT_ID",variable.name="YEAR",value.name="proxy")
rs3$YEAR <- as.character(rs3$YEAR)
rs4 <- cbind(rs3[-2],read.fwf(textConnection(rs3$YEAR),widths = c(3,4),col.names=c("band","YEAR")))
rs5<-dcast(rs4,PLOT_ID+YEAR~band,value.var="proxy")
names(rs5) = c("PLOT_ID","YEAR","b1","b2","b3","b4","b5","b6")

# merging the time variant df
df4 <- inner_join(df3,rs5)
# many NAs
length(which(is.na(df4$b1))) #658
length(which(is.na(df4$b6))) #658
# the RF df will have 417 lines
df5 <- filter(df4,!is.na(b1))
length(unique(df5$PLOT_ID)) #380 plots

# add the non-time variant rs data
df6 <- inner_join(df5,rs1) %>%
  arrange(PLOT_ID,age)
# a few extra checks
# what is the range of years?
range(df6$YEAR) #1984 2009
df7 <- df6[,c(-2)]



#4 Random Forest model -------------------------------------------------------------------------

# for a repeatable model
set.seed(215)

library(corrplot)
M <- cor(df7[,3:19])
corrplot(M)#, method="number"

# RUN RF MODEL base model (all lines)------------------------------------
rf.mdl <- randomForest(x=df7[,c(3:19)], y=df7[,2],ntree=500,importance=TRUE,mtry=6) #ntree=501,
#with a test data set
test <- sample(1:417,100)
xtest=df7[test,c(3:19)]
ytest=df7[test,2]
xbuild=df7[-test,c(3:19)]
ybuild=df7[-test,2]
rf.1 <- randomForest(x=xbuild, y=ybuild,xtest=xtest,ytest=ytest,ntree=500,importance=TRUE) 

# MODELS CHEKS
# CHECK ERROR CONVERGENCE
plot(rf.mdl$rsq)
plot(rf.1)
# PLOT mean decrease in accuracy VARIABLE IMPORTANCE
varImpPlot(rf.mdl, type=1)
varImpPlot(rf.mdl)

rf.mdl
importance(rf.mdl)
rf.mdl$importanceSD
rf.mdl$mse
rf.mdl$rsq
treesize(rf.mdl)
# END

# Seeing if PSP ages would improve the model performance -------------------
psp.ages <- select(tree,PLOT_ID,YEAR,agecombo) %>%
  group_by(PLOT_ID,YEAR) %>%
  summarize(round(mean(agecombo,na.rm=TRUE)))
names(psp.ages) = c("PLOT_ID","YEAR","realage")
# merge that with the rest of the data by PLOT_ID and YEAR
df8 <- inner_join(df6,psp.ages)
# compare ages
age.lm <- lm(realage~age,data=df8)
age.r2 <- summary(age.lm)$r.squared
age.plot <- ggplot(data=df8,aes(x=age,y=realage)) + geom_point() + 
  geom_abline(intercept=0,slope=1) +  geom_smooth(method=lm) +
  xlab("Knn Age") + ylab("Actual Age") +
  ggtitle("Age Comparison - SK 418") + 
  theme(plot.title = element_text(lineheight=1.2, face="bold")) 
# get rid of non-needed vars
df9 <- df8[,-c(1,2,4)]
rf.2 <- randomForest(x=df9[,2:18],y=df9[,1],importance=TRUE)
varImpPlot(rf.2, type=1)
rf.2$rsq
## YEs: improvement - now explaining up to 40% of var

# getting the binary species--------------------------
rs1bin <-rs.raw[,c(1,10:19)]
df10 <- inner_join(df5,rs1bin) %>%
  arrange(PLOT_ID,age)
rf.3 <- randomForest(x=df10[,c(4:20)], y=df10[,3],ntree=500,importance=TRUE,mtry=6) #ntree=501,
varImpPlot(rf.3,type=1)

# inserting the real species proportions (with knnage - no change to age) -------------
df11 <- select(df7,-contains("_p"))

df12 <- inner_join(df11,spsw.biom)
df13 <- select(df12, -PLOT_ID)
rf.4 <- randomForest(x=df13[,c(2:20)], y=df13[,1],ntree=2000,importance=TRUE,mtry=6) #ntree=501,
varImpPlot(rf.4,type=1)

# both real ages and real species prop
just.ages <- select(df8,PLOT_ID,realage)
just.ages <- unique(just.ages)
df12 <- unique(df12)
df14 <- inner_join(df12,just.ages)
df15 <- select(df14,-PLOT_ID,-age)
rf.5 <- randomForest(x=df15[,c(2:20)], y=df15[,1],ntree=500,importance=TRUE,mtry=6) #ntree=501,
rf.5
varImpPlot(rf.5,type=1)

# try with NDVI
ndvi <- mutate(df14,ndvi = ((b4-b3)/(b4+b3)) ) %>%
  select(-b3,-b4)
ndvi1 <- select(ndvi,-PLOT_ID,-age)
rf.6 <- randomForest(x=ndvi1[,c(2:19)], y=ndvi1[,1],ntree=500,importance=TRUE,mtry=6) #ntree=501,
rf.6
varImpPlot(rf.6,type=1)

#try ndvi and no other bands
ndvi2 <-select(ndvi,-PLOT_ID,-age,-b1,-b2,-b5,-b6)
rf.7 <- randomForest(x=ndvi2[,c(2:15)], y=ndvi2[,1],ntree=2000,importance=TRUE,mtry=6) #ntree=501,
rf.7
varImpPlot(rf.7,type=1)








# PREDICT MODEL ### This would be the lines to "spread" the model on a raster
predict(xvars, rf.mdl, filename="RfClassPred.img", type="response", 
        index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

# Attempt at another model...may come back to this later
df8 <- df7[,2:19]
library(party)
ct.mdl <- ctree(biom.ha~.,data=df8)

