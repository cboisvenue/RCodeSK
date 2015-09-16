#------------------------------------------------------------------------------------------
# evaluate CASFRI sps and age against PSP, knn age, and SPECIES2010
#
# CBoisvenue
# April 21st, 2015
#------------------------------------------------------------------------------------------

library(plyr)
library(dplyr)
library(reshape2)

outdir="C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CASFRI/evaluate/"
indir = "G:/RES_Work/Work/JoanneWhite/SK_work/data/"

# Bring in data----------------------------------------------------------------------------------
pspdata <- read.table(paste(indir,"CleanedUpForUsing/RF_dataframePSPInfo.txt",sep=""), header=TRUE,sep=",")
pspsps <- read.table(paste(indir,"CleanedUpForUsing/PSP418_DomSpecies.txt",sep=""), header=TRUE,sep=",")
casfri <- read.table(paste(indir,"CleanedUpForUsing/casfri.txt",sep=""), header=TRUE,sep=",")
# read-in knn ages 
knnages <- read.table(paste(indir,"PSP/PSP_UTM_wKnnAGE.csv",sep=""), header=TRUE,sep=",")
knnages <- knnages[,c(2,7)]
# ASSUMPTION: knn ages are ages of the pixels in 2001

# read in remoting sensing data
rs.raw <-read.table(paste(indir,"SK_psp_extract.txt",sep=""),header=TRUE,sep=",")
# extract species proportion variables
landsat.sps1 <- select(rs.raw,PLOT_ID,contains("_p")) 
landsat.sps2 <- melt(landsat.sps1,id.vars="PLOT_ID",variable.name="landsat_sps",value.name="prop")
landsat.sps <- arrange(landsat.sps2,PLOT_ID, landsat_sps)
# END of loading data section ----------------------------------------------------------------------


# Leading species and % of dominant ----------------------------------------------------------------
# All landsat obs are for 2010
# casfri % and domsps will be taken by the last photo year for each plot
# psp % and domsps will be taken by the last year for each plot

# landsat
land1 <- arrange(landsat.sps,PLOT_ID,desc(prop)) %>%
  group_by(PLOT_ID) %>%
  summarise(landsat_dom = first(landsat_sps),
            landsat_pc = first(prop))

# casfri
cas1 <- arrange(casfri,plot_id, photo_year) %>%
  group_by(plot_id) %>%
  summarise(lastyr = last(photo_year),
    casfri_sps = last(casfri_dom),Abie.bals=last(Abie.bals),Betu.papy=last(Betu.papy),
    Lari.lari=last(Lari.lari),Pice.glau=last(Pice.glau),Pice.mari=last(Pice.mari),
    Pinu.bank=last(Pinu.bank),Popu.balb=last(Popu.balb),Popu.trem=last(Popu.trem))
casfri_pc <- vector(mode="numeric",length=dim(cas1)[[2]])
casfri_pc[which(cas1$casfri_sps=="Abie bals")] = cas1$Abie.bals[which(cas1$casfri_sps=="Abie bals")]
casfri_pc[which(cas1$casfri_sps=="Betu papy")] = cas1$Betu.papy[which(cas1$casfri_sps=="Betu papy")]
casfri_pc[which(cas1$casfri_sps=="Lari lari")] = cas1$Lari.lari[which(cas1$casfri_sps=="Lari lari")]
casfri_pc[which(cas1$casfri_sps=="Pice glau")] = cas1$Pice.glau[which(cas1$casfri_sps=="Pice glau")]
casfri_pc[which(cas1$casfri_sps=="Pice mari")] = cas1$Pice.mari[which(cas1$casfri_sps=="Pice mari")]
casfri_pc[which(cas1$casfri_sps=="Pinu bank")] = cas1$Pinu.bank[which(cas1$casfri_sps=="Pinu bank")]
casfri_pc[which(cas1$casfri_sps=="Popu balb")] = cas1$Popu.balb[which(cas1$casfri_sps=="Popu balb")]
casfri_pc[which(cas1$casfri_sps=="Popu trem")] = cas1$Popu.trem[which(cas1$casfri_sps=="Popu trem")]
cas1 <- mutate(cas1,PLOT_ID=plot_id)
cas2 <- cbind(cas1[,c(3,12)],casfri_pc)

# psp
psp1 <- arrange(pspsps,PLOT_ID,YEAR) %>%
  group_by(PLOT_ID) %>%
  summarise(sps = last(domsps),sps_pc = last(domperc))

# join the dataframes
sps.comp <- left_join(psp1,land1) %>%
  left_join(cas2)
# standardise the nomenclature
# unique(land1$landsat_dom)
# [1] picemar_p poputre_p pinuban_p picegla_p larilar_p betupap_p
# Levels: betupap_p larilar_p picegla_p picemar_p pinuban_p poputre_p
# > levels(casfri$casfri_dom)
# [1] "Abie bals" "Betu papy" "Lari lari" "Pice glau" "Pice mari" "Pinu bank" "Popu balb"
# [8] "Popu trem"
# > levels(pspdata$domsps)
# [1] "BF" "BP" "BS" "JP" "TA" "TL" "WB" "WS"
sps.comp$landsat_dom <- revalue(sps.comp$landsat_dom, c("betupap_p"="WB","larilar_p"="TL",
                              "picegla_p"="WS", "picemar_p"="BS","pinuban_p"="JP","poputre_p"="TA"))
sps.comp$casfri_sps <- revalue(sps.comp$casfri_sps, c("Betu papy"="WB","Lari lari"="TL",
                              "Pice glau"="WS", "Pice mari"="BS","Pinu bank"="JP","Popu trem"="TA","Popu balb"="BP"))

# comparison of dominant species
casfri_match <- vector(mode="numeric", length=dim(sps.comp)[[1]])
landsat_match <- vector(mode="numeric", length=dim(sps.comp)[[1]])  
casfri_match[which(as.character(sps.comp$sps)==as.character(sps.comp$casfri_sps))] = 1
landsat_match[which(as.character(sps.comp$sps)==as.character(sps.comp$landsat_dom))] = 1  
sum(casfri_match)  # 258
sum(landsat_match) # 152

lm.pc.casfri <- lm(sps_pc~casfri_pc,data=sps.comp)
lm.pc.landsat <- lm(sps_pc~landsat_pc,data=sps.comp)
pc.casfri.r2 <- summary(lm.pc.casfri)$r.squared
pc.landsat.r2 <- summary(lm.pc.landsat)$r.squared
pc.plot <- ggplot(data=sps.comp,aes(x=casfri_pc,y=100*sps_pc)) + geom_point() +
  geom_abline(intercept=0,slope=1) + geom_smooth(method=lm) +
  xlab("CASFRI % for DOmSps") + ylab("Actual % of DOM Sps") +
  ggtitle("Evaluate Species Data") 
#--------- END of sps and % dom evaluation ---------------------------------------------------

# Age data evaluation ------------------------------------------------------------------------


# choosing 2001 as the year of age comparison
#casfri
cas.age <- select(casfri,plot_id,dist_yr) %>%
  mutate(cas_age = 2001-dist_yr) %>%
  select(plot_id,cas_age)
#knn
names(knnages) =c("plot_id","knnage")

#psp
pspages <- select(pspdata,plot_id=PLOT_ID,YEAR,realage) %>%
  mutate(age2001 = 2001-YEAR+realage) %>%
  group_by(plot_id) %>%
  summarise(plotage = mean(age2001,na.rm=TRUE))

ages <- left_join(pspages,knnages) %>%
  left_join(cas.age)

lm.pspknn <- lm(plotage~knnage, data=ages)
knn.r2 <- summary(lm.pspknn)$r.square
lm.pspcas <- lm(plotage~cas_age, data=ages)
cas.r2 <- summary(lm.pspcas)$r.square

# compare ages in a graph

age.plot <- ggplot(data=ages,aes(x=cas_age,y=plotage)) + geom_point(colour="blue") + 
  geom_abline(intercept=0,slope=1) +  geom_smooth(method=lm,colour="blue") +
  geom_point(aes(x=knnage),colour="red") + 
  #geom_abline(intercept=lm.pspknn$coefficients[[1]],slope=lm.pspknn$coefficients[[2]],colour="red") +
  geom_smooth(data=ages, aes(x=knnage,y=plotage),method="lm",colour="red") +
  xlab("CASFRI & KNN Ages") + ylab("Actual Age") +
  ggtitle("Age Comparison - SK 418") + 
  theme(plot.title = element_text(lineheight=1.2, face="bold")) 

ggsave(paste(outdir,"CompareAges.jpeg",sep=""),plot=age.plot)


