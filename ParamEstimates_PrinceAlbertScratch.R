#--------------------------------------------------------------
# RS growth raster project
# We now have a mammoth table for the Prince Albert FMA that Byron created with the
# RS_df_tables_bsmiley.R script. 
# This script is a test to see if I can fit parameters to the large data.table
#
# CBoisvenue
# July 31st, 2015
#---------------------------------------------------------------

# this mammoth files presently sits in A105350
#indir <- "H:/saskatchewan/spatialGrowth/fmapa/RSGrowthSmoothSyncNA/tables/"
# Copied it to M
indir ="M:/Spatially_explicit/01_Projects/07_SK_30m/exchangeFolder/DataTables/"

library(data.table)
dt.all <-fread(paste(indir,"DT_all.txt",sep=""),sep=",",header=TRUE)

setnames(dt.all,names(dt.all),c("age","logAge","l.dbiom","strata","RasterID"))

countPA <- dt.all[,.N,by=strata]
# strata       N
# 1:     31 6591144
# 2:     51 7614124
# 3:     71 2078608
# 4:     41 6480180
# 5:     33  830900
# 6:     61  130116
# 7:     43   20580
# 8:     32     168
# 9:     63    3220
# 10:     42   44968
# 11:     73    1708
# 12:     11     980
# 13:     53    3164
# 14:     21    3192
# 15:     13     112
# 16:     52     224

# change the strata from an interger to a character
dt.all[,strata :=as.character(strata)]
#setkey(dt.all,RasterID,strata)


# try fitting it the "normal way"---------------------------------
library(lme4)
# startTime <- Sys.time()
# memPA.all <- lmer(formula= l.dbiom~strata+logAge*strata+age+(1|RasterID),data=dt.all)
# endTime <- Sys.time()
# fit.time <- endTime-startTime #Time difference of 20.40164 mins
# Error:
# Error in abs(evd) : non-numeric argument to mathematical function
# In addition: Warning messages:
#   1: Some predictor variables are on very different scales: consider rescaling 
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                   unable to evaluate scaled gradient
# failed ---------------------------------------------------------

# try to sub-sample
# 1000?
# need to select the number of pixel since I want the pixels to have repeated measurements
pix <- unique(dt.all$RasterID)
pix.sample1 <- (sample(pix,1000,replace=FALSE))
sample1k <- dt.all[RasterID %in% pix.sample1]
# this gives me 28000 lines since I have 1000 pixels that I have 28 deltas for
                
startTime <- Sys.time()
memPA.s1 <- lmer(formula= l.dbiom~strata+logAge*strata+age+(1|RasterID),data=sample1k)
endTime <- Sys.time()
fit.time <- endTime-startTime #Time difference of 20.40164 mins

# evaluate
AIC(memPA.s1)
lyhat <- fitted(memPA.s1)
plot(sample1k$l.dbiom,lyhat)
rm(lyhat)
params.matrix.s1 <- as.data.frame(summary(memPA.s1)$coefficients)
# here...figure out the count
strata.n <- sample1k[,list(n.strata = length(l.dbiom)),by=strata]
# strata31 is the "not-labelled" intercept and slope
# there are only 7 strata sampled here
b0s1 <- params.matrix.s1[1:7,1:2]
b1s1 <- params.matrix.s1[c(8,10:15),1:2]
b2s1 <- params.matrix.s1[9,1:2]

b <- rep("b0",dim(b0s1)[[1]])
b0s1 <- cbind(strata.n,b,b0s1)
setnames(b0s1, c("strata","n.strata","b","Estimate","Std. Error"),c("stratum","n","b","value","se"))
# assuming a normal dist and a 95%
library(plyr)
library(dplyr)
b0s1 <- mutate(b0s1,b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
              upper.b=value+b.er) 

b <- rep("b1",dim(b1s1)[[1]])

b1s1 <- cbind(strata.n,b,b1s1)
setnames(b1s1,c("strata","n.strata","b","Estimate","Std. Error"),c("stratum","n","b","value","se"))
b1s1 <- mutate(b1s1,b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
              upper.b=value+b.er) 

setnames(b2s1,c("Estimate","Std. Error"),c("value","se"))
b2s1 <- mutate(b2s1,stratum="00",n=28000,b="b2",b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
              upper.b=value+b.er) %>%
  select(stratum,n,b,value,se,b.er,lower.b,upper.b)

param.CI.s1 <- rbind(b0s1,b1s1,b2s1)
# data.frame with parameters and their CI----------------------------

library(ggplot2)
ci.plot.s1 <- ggplot(data=param.CI.s1, aes(y=value,x=stratum, group=b, colour=b)) + 
  geom_errorbar(aes(ymin=lower.b,ymax=upper.b), width=.2) + geom_point()


# Playing with probabilities------------------------------------------
counts1 <- sample1k[,.N,by=strata]
counts1
# strata    N
# 1:     51 8820
# 2:     41 8204
# 3:     31 7504
# 4:     61  140
# 5:     33  924
# 6:     71 2380
# 7:     43   28

# can I give each line a prob?
countPA[,prob := N/sum(N)]
# keep the prob and strata column and join with dt.all
probPA <- countPA[,.(strata,prob)]
probPA[,strata :=as.character(strata)]

# not sure if this was needed since it is the pix numbers that need to be sampled
# and there for the 
    # setkey(probPA,strata)
    # setkey(dt.all,RasterID,strata)
    # dt.prob <- merge(dt.all,probPA)
    # rm(dt.prob)
# see if sample with these prob comes out different
# attach a prob to each pixels
pix.prob1 <- dt.all[pix,mult="first"]
setkey(probPA,strata)
setkey(pix.prob1,RasterID,strata)
pix.prob <-merge(pix.prob1,probPA)
# each pixel now has a probability


pix.sample1p <- sample(pix.prob$RasterID,10000,replace=FALSE,prob=pix.prob$prob)

sample1p <- dt.all[RasterID %in% pix.sample1p]
counts1p <- sample1p[,.N,by=strata]

# can I sample proportional to the strata that I have in my PSP?

biom.data <- read.table("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/t_haBiom_yr.txt",sep=",",header=TRUE)

# still linearize the model
#data1 <- mutate(biom.data,ly = log(biom.ha.inc), l.age=log(age1))
countPSP <- as.data.table(table(biom.data$stratum))
# there are 10 unique strata in the PSP
dim(countPSP)
setnames(countPSP,names(countPSP),c("strataPSP","nPSP"))
countPSP[,probPSP := nPSP/sum(nPSP)]
probPSP <- countPSP[,.(strataPSP,probPSP)]

# the strata in the PSP do not match those in the FMAPA.
# PSP model-fitting data has 10 strata while FMAPA data has 16.
# PSP strata as characters while FMAPA are numbers (although I did make them 
# character for model fitting)
# Here I make the 2 the same.
#-------------------------------------------------------------------------------
# read-in correspondence between PSP and PA
# I created this file myself and very carefully with many rechecks
strata.both <- read.table("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/StrataCorrespondencePSP_PA.csv",sep=",",header=TRUE)
strata.both <- as.data.table(strata.both)
setnames(strata.both,names(strata.both),c("strata","strataPSP"))
setkey(strata.both,strataPSP)

# merge this strata.both with proPSP by strataPSP 
setkey(probPSP,strataPSP)
both.prob <- merge(strata.both,probPSP)
both.prob[,strata :=as.character(strata)]
setkey(both.prob,strata)
setkey(dt.all,RasterID)
pix.prob1 <- dt.all[pix,mult="first"]
setkey(pix.prob1,strata)
pix.prob <-merge(pix.prob1,both.prob)


pix.sample1psp <- sample(pix.prob$RasterID,10000,replace=FALSE,prob=pix.prob$probPSP)

sample1psp <- dt.all[RasterID %in% pix.sample1psp]
strata.both[,strata := as.character(strata)]
setkey(strata.both,strata)
setkey(sample1psp,strata)
samplePSP1 <- merge(sample1psp,strata.both)

countPAPSP <- samplePSP1[,.N,by=strataPSP]
#   strataPSP      N
# 1:       BSG  76104
# 2:       BSM   2156
# 3:        JP 166180
# 4:        WB  26208
# 5:       TAG   1736
# 6:        BF   7616
# I sampled 10000 pixels across the 28 years (280 000 lines) with probabilities proportional
# to the distribution of strata within PSPs -----------------------------------------------------

# clean-up
rm(counts1p)
rm(sample1p)
rm(sample1psp)
rm(pix.sample1p)
# try fitting the sample selected with prob ~ PSP strata-----------------------------------------
library(lme4)
startTime <- Sys.time()
memPA.s1 <- lmer(formula= l.dbiom~strataPSP+logAge*strataPSP+age+(1|RasterID),data=samplePSP1)
endTime <- Sys.time()
fit.time <- endTime-startTime #Time difference of 7.435 secs

params.matrix.s1 <- as.data.frame(summary(memPA.s1)$coefficients)

## NEED TO SEE IF THOSE PARAMS MAKE SENSE





















