#--------------------------------------------------------------
# RS growth raster project
# We now have a mammoth table for the Prince Albert FMA that Byron created with the
# RS_growth_tables_creation.R script. 
#
# This present script is a cleaned-up version of ParamEstimates_PrinceAlbertScratch.R.
#
# In this script:
# - Read-in the large data.table that contains all the variables to fit the
#   same model as the one that best fit on PSPs data.The variables are "age",
#   "logAge","l.dbiom","strata","RasterID". 
# - Strata in the large data.table does not match the strata in the PSP, 
#   so making those match.
#
# - I sub-sample the large data.table, 100 times,
# - fit the same form as the PSP-based model for each one of the samples
# - Create a file for that set of samples and save it
# - plot the parameter values 
#
# - repeat all the sampling to see if results are consistent
#
# CBoisvenue
# September 9th, 2015
#---------------------------------------------------------------


# Read-in the large data.table and correct the names----------------------------------------
# this mammoth files presently sits in A105350
#indir <- "H:/saskatchewan/spatialGrowth/fmapa/RSGrowthSmoothSyncNA/tables/"
# Copied it to M
indir ="M:/Spatially_explicit/01_Projects/07_SK_30m/exchangeFolder/DataTables/"

library(data.table)
dt.all <-fread(paste(indir,"DT_all.txt",sep=""),sep=",",header=TRUE)
# names in the data.table-building process were not changed to actually represent 
# what each column contains. These names are more appropriate
setnames(dt.all,names(dt.all),c("age","logAge","l.dbiom","strata","RasterID"))
##END Read-in large data.table--------------------------------------------------------------


# Strata matching 
# the strata in the PSP do not match those in the FMAPA.
# PSP model-fitting data has 9 strata while FMAPA data has 16.
# PSP strata are characters while FMAPA are numbers 
# Here we read-in the correspondence file between the two strata 
# (see Untangled_Strata_KO's work in Tracking_SK_Work.xlsx to follow the steps to 
# make sure strata were correctly matched), 
# calculate a probability for the strataPSP (in case I decide that weighted prob is worth it)
# link that to the large dt
#-------------------------------------------------------------------------------

# read-in correspondence between PSP and PA
strata.both <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/StrataCorrespondencePSP_PA.csv",sep=",",header=TRUE)
setnames(strata.both,names(strata.both),c("strata","strataPSP"))

# read-in PSP data and calculate a proportion for each strata
biom.data <- read.table("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/t_haBiom_yr.txt",sep=",",header=TRUE)
countPSP <- as.data.table(table(biom.data$stratum))
setnames(countPSP,names(countPSP),c("strataPSP","nPSP"))
# in case I decide that the probability of sampling 
# should be proportional to the PSP data (# per strata)
countPSP[,probPSP := nPSP/sum(nPSP)]
probPSP <- countPSP[,.(strataPSP,probPSP)]

# merge this strata.both with probPSP by strataPSP 
setkey(strata.both,strataPSP)
setkey(probPSP,strataPSP)
both.prob <- merge(strata.both,probPSP)

# merge by strata with the large dt
setkey(both.prob,strata)
setkey(dt.all,strata)
dt2 <- merge(dt.all,both.prob)
rm(dt.all)
# END strata matching and probability -----------------------------------------

# Sampling
# need to sample the RasterID so we have "repeated measurements"
#--------------------------------------------------------------------------------

# First sample------------------
pix <- dt2[,.(RasterID,probPSP)]
pix <- unique(pix)

# I was sampling with weighted probability based on the strata proportions in the PSP data
# but I then decided that this was not necessary I want parameter values for each strata
pix.sample <- sample(pix$RasterID,10000,replace=FALSE) #,prob=pix$probPSP
sample1 <- dt2[RasterID %in% pix.sample]

# END 1st sample---------------

# Fit the model to 1st sample
#------------------------------------------
library(lme4)
startTime <- Sys.time()
memPA.s1 <- lmer(formula= l.dbiom~strataPSP+logAge*strataPSP+age+(1|RasterID),data=sample1)
endTime <- Sys.time()
fit.time <- endTime-startTime #Time difference of 7.820782 secs

# Note: with each sample, the parameters and strata are not in the same order
# 1st solution: 
# make a df with the 1st and 2nd columns of the parameter matrix for the 1st sample
# then loop 99 times to select a different sample, fit a model, 
# and add the rows to the initial sample df

# initial df
#-------------------
paramsPA <- as.data.frame(cbind(var = names(summary(memPA.s1)$coefficients[,1]), value = as.vector(summary(memPA.s1)$coefficients[,1])))
# keep track of strata actually sampled in each dt sample and their order
n.sample <- as.data.table(table(sample1$strataPSP))
b <- c(rep("b0",dim(n.sample)[1]),"b1","b2",rep("b1",dim(n.sample)[1]-1))
stratum <-c(n.sample$V1,n.sample$V1[1],"ALL",n.sample$V1[2:length(n.sample$V1)])
n.stratum <- c(n.sample$N,n.sample$N[1],sum(n.sample$N),n.sample$N[2:length(n.sample$N)])
paramsAll <- cbind(stratum,n.stratum,b,paramsPA)
# Initial df done

# Remaining 99 samples
#---------------------------
nsamples <- 100
startTime <- Sys.time()
for(i in 1:(nsamples-1)) {
  pix.s <- sample(pix$RasterID,10000,replace=FALSE)#,prob=pix$probPSP
  sampled.pix <- dt2[RasterID %in% pix.s]
  count.s <- as.data.table(table(sampled.pix$strataPSP))
  b <- c(rep("b0",dim(count.s)[1]),"b1","b2",rep("b1",dim(count.s)[1]-1))
  stratum <-c(count.s$V1,count.s$V1[1],"ALL",count.s$V1[2:length(count.s$V1)])
  n.stratum <- c(count.s$N,count.s$N[1],sum(count.s$N),count.s$N[2:length(count.s$N)])
  memPA <- lmer(formula= l.dbiom~strataPSP+logAge*strataPSP+age+(1|RasterID),data=sampled.pix)
  nextparam <- as.data.frame(cbind(var = names(summary(memPA)$coefficients[,1]), value = as.vector(summary(memPA)$coefficients[,1])))
  nextAll <- cbind(stratum,n.stratum,b,nextparam)
  paramsAll <- rbind(paramsAll,nextAll)
}
endTime <- Sys.time()
fit.time <- endTime-startTime #Time difference of 14.43755 mins
# Completed sampling------------

# Notes: 
# - there are 1688 lines in paramsPA after this loop
length(unique(paramsAll$var))
# - there are 19 unique parameter names 
# Check class of paramsAll
class(paramsAll)
paramsAll <- as.data.table(paramsAll)
# "value" was a factor, so make it numeric
paramsAll[,value :=as.numeric(as.character(value))]
setkey(paramsAll,b,stratum)

write.table(paramsAll,file="H:/saskatchewan/Celine/RsParameters/DeltaBiomRsParams.txt",sep=",",row.names=FALSE)
# the above file was copied into "CleanedUpForUsing" folder on M:/
# Parameter estimation completed--------------------------------------------------------------------

# Graph
#--------------------------------------------------------------------------------------
library(ggplot2)

#calculate the mean, min and max param value, and sum the n.stratum
params.rs.CI <- paramsAll[,.(N.stratum=sum(n.stratum),avg=mean(value),lower = min(value), 
                             upper=max(value)),by=.(stratum,b)]
# To check: avearage values still seem really high
# maybe use mode (most frequent?)
# decided not to use those summary statistics, going with boxplots
library(ggplot2)

rs1 <- ggplot(data=paramsAll,aes(x=stratum,y=value,fill=b))
rs1+geom_boxplot()
# Note: The upper and lower "hinges" correspond to the first and third quartiles 
# (the 25th and 75th percentiles). This differs slightly from the method used by 
# the boxplot function.
# The upper whisker extends from the hinge to the highest value that is within 
# 1.5 * IQR of the hinge, where IQR is the inter-quartile range, or distance between 
# the first and third quartiles. The lower whisker extends from the hinge to the 
# lowest value within 1.5 * IQR of the hinge.

ggsave(file="H:/saskatchewan/Celine/RsParameters/DeltaBiomRsParams.jpeg")
# the above file was copied into "CleanedUpForUsing" folder on M:/
#
# Graph completed-----------------------------------------------------------------------

# repeated sampling
#--------------------------------------------------------------------------------------
for(j in 1:5){
  pix.sample <- sample(pix$RasterID,10000,replace=FALSE) #,prob=pix$probPSP
  sampleX <- dt2[RasterID %in% pix.sample]
  
  # END 1st sample---------------
  
  # Fit the model to 1st sample
  #------------------------------------------
  library(lme4)
  startTime <- Sys.time()
  memPA.s1 <- lmer(formula= l.dbiom~strataPSP+logAge*strataPSP+age+(1|RasterID),data=sampleX)
  endTime <- Sys.time()
  fit.time <- endTime-startTime #Time difference of 7.820782 secs
  
  # Note: with each sample, the parameters and strata are not in the same order
  # 1st solution: 
  # make a df with the 1st and 2nd columns of the parameter matrix for the 1st sample
  # then loop 99 times to select a different sample, fit a model, 
  # and add the rows to the initial sample df
  
  # initial df
  #-------------------
  paramsPA <- as.data.frame(cbind(var = names(summary(memPA.s1)$coefficients[,1]), value = as.vector(summary(memPA.s1)$coefficients[,1])))
  # keep track of strata actually sampled in each dt sample and their order
  n.sample <- as.data.table(table(sampleX$strataPSP))
  b <- c(rep("b0",dim(n.sample)[1]),"b1","b2",rep("b1",dim(n.sample)[1]-1))
  stratum <-c(n.sample$V1,n.sample$V1[1],"ALL",n.sample$V1[2:length(n.sample$V1)])
  n.stratum <- c(n.sample$N,n.sample$N[1],sum(n.sample$N),n.sample$N[2:length(n.sample$N)])
  paramsAll <- cbind(stratum,n.stratum,b,paramsPA)
  # Initial df done
  
  # Remaining 99 samples
  #---------------------------
  nsamples <- 100
  startTime <- Sys.time()
  for(i in 1:(nsamples-1)) {
    pix.s <- sample(pix$RasterID,10000,replace=FALSE)#,prob=pix$probPSP
    sampled.pix <- dt2[RasterID %in% pix.s]
    count.s <- as.data.table(table(sampled.pix$strataPSP))
    b <- c(rep("b0",dim(count.s)[1]),"b1","b2",rep("b1",dim(count.s)[1]-1))
    stratum <-c(count.s$V1,count.s$V1[1],"ALL",count.s$V1[2:length(count.s$V1)])
    n.stratum <- c(count.s$N,count.s$N[1],sum(count.s$N),count.s$N[2:length(count.s$N)])
    memPA <- lmer(formula= l.dbiom~strataPSP+logAge*strataPSP+age+(1|RasterID),data=sampled.pix)
    nextparam <- as.data.frame(cbind(var = names(summary(memPA)$coefficients[,1]), value = as.vector(summary(memPA)$coefficients[,1])))
    nextAll <- cbind(stratum,n.stratum,b,nextparam)
    paramsAll <- rbind(paramsAll,nextAll)
  }
  endTime <- Sys.time()
  fit.time <- endTime-startTime #Time difference of 14.43755 mins
  # Completed sampling------------
  
  # Notes: 
  # - there are 1688 lines in paramsPA after this loop
  length(unique(paramsAll$var))
  # - there are 19 unique parameter names 
  # Check class of paramsAll
  class(paramsAll)
  paramsAll <- as.data.table(paramsAll)
  # "value" was a factor, so make it numeric
  paramsAll[,value :=as.numeric(as.character(value))]
  setkey(paramsAll,b,stratum)
  
  write.table(paramsAll,file=paste("H:/saskatchewan/Celine/RsParameters/DeltaBiomRsParams_s",j,".txt",sep=""),sep=",",row.names=FALSE)
}
# the above file was copied into "CleanedUpForUsing" folder on M:/
# Parameter estimation completed--------------------------------------------------------------------







