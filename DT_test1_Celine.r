#----------------------------------------------
# RSgrowth project
# We are at the point where we are trying to fit mixed effects models on the stack on 
# rasters of log(delta_biomass) as a funciton of log(age) and age) to compare the parameters 
# to the parameters estimated (by strata) from the psps
#
# KO created a bunch of files here:
# H:\saskatchewan\spatialGrowth\fmapa\kangakola\Output_DF
# with the code here:
# M:\Spatially_explicit\01_Projects\07_SK_30m\Working\RScripts\RS_df_tables_kangakola_20150725.R
#
# the job in this script is to see if one of the tables created from that can be 
# used to estimate parameters
#
# CBoisvenue
# July 29, 2015
#-----------------------------------------------

# define input dir
indir <- "H:/saskatchewan/spatialGrowth/fmapa/kangakola/Output_DF/"

# read table
# taking the biggest one...to see
dt0910 <- read.table(paste(indir,"DT_09_10.txt",sep=""),sep=",",header=TRUE)


library(plyr)
library(dplyr)
library(tidyr)
flip1 <- dt0910 %>%
  gather(vars,value,-RasterID1)
table(flip1$vars)
# X2008age       X2008logAge lg_delta_bio_2009          strata09 
# 5926622           5926622           5926622           5926622 
# X2009age       X2009logAge lg_delta_bio_2010          strata10 
# 5926622           5926622           5926622           5926622 

rm(dt0910)
age <- filter(flip1,vars=="X2008age"|vars=="X2009age") %>%
  select(RasterID1,age=value) 
logAge <-  filter(flip1,vars=="X2008logAge"|vars=="X2009logAge") %>%
  transmute(logAge=value) 
l.biom <- filter(flip1,vars=="lg_delta_bio_2009"|vars=="lg_delta_bio_2010") %>%
  transmute(l.biom=value)
strata <- filter(flip1, vars=="strata09"|vars=="strata10") %>%
  transmute(strata=value)
strata$strata <- as.character(strata$strata)
data1 <- cbind(strata,age,logAge,l.biom)
rm(age,strata,logAge,l.biom,flip1)


library(lme4)
startTime <- Sys.time()
mem3deltas <- lmer(formula= l.biom~strata+logAge*strata+age+(1|RasterID1),data=data1,REML=FALSE)
endTime <- Sys.time()
fit.time <- endTime-startTime #Time difference of ~14 mins


# try adding another data frame
dt9900 <-  read.table(paste(indir,"DT_99_00.txt",sep=""),sep=",",header=TRUE)
flip2 <- dt9900 %>%
  gather(vars,value,-RasterID1)
table(flip2$vars)
# X1998age       X1998logAge lg_delta_bio_1999          strata99 
# 1292011           1292011           1292011           1292011 
# X1999age       X1999logAge lg_delta_bio_2000          strata00 
# 1292011           1292011           1292011           1292011 
rm(dt9900)
age <- filter(flip2,vars=="X1998age"|vars=="X1999age") %>%
  select(RasterID1,age=value) 
logAge <-  filter(flip2,vars=="X1998logAge"|vars=="X1999logAge") %>%
  transmute(logAge=value) 
l.biom <- filter(flip2,vars=="lg_delta_bio_1999"|vars=="lg_delta_bio_2000") %>%
  transmute(l.biom=value)
strata <- filter(flip2, vars=="strata99"|vars=="strata00") %>%
  transmute(strata=value)
strata$strata <- as.character(strata$strata)
data2 <- cbind(strata,age,logAge,l.biom)
rm(age,strata,logAge,l.biom,flip2)

data3 <- rbind(data1, data2)
rm(data1,data2)
library(lme4)
startTime <- Sys.time()
mem2DT <- lmer(formula= l.biom~strata+logAge*strata+age+(1|RasterID1),data=data3,REML=FALSE)
endTime <- Sys.time()
fit.time <- endTime-startTime #Time difference of 17.6986 mins

# clear!
rm(list=ls())

# now trying data.table
library(data.table)
dt_0910 <- fread(paste(indir,"DT_09_10.txt",sep=""),sep=",",header = TRUE)

# can I do this with data.table?--------------------------
# flip1 <- dt_0910 %>%
#   gather(vars,value,-RasterID1)
# table(flip1$vars)
# rm(dt9900)
# age <- filter(flip2,vars=="X1998age"|vars=="X1999age") %>%
#   select(RasterID1,age=value) 
# logAge <-  filter(flip2,vars=="X1998logAge"|vars=="X1999logAge") %>%
#   transmute(logAge=value) 
# l.biom <- filter(flip2,vars=="lg_delta_bio_1999"|vars=="lg_delta_bio_2000") %>%
#   transmute(l.biom=value)
# strata <- filter(flip2, vars=="strata99"|vars=="strata00") %>%
#   transmute(strata=value)
# strata$strata <- as.character(strata$strata)
# data2 <- cbind(strata,age,logAge,l.biom)
# rm(age,strata,logAge,l.biom,flip2)

# there are melt and cast functions adapted to data.table!
# load reshape2
library(reshape2)
melt0910 <- melt(dt_0910,id=1)
# "gather" is done
# this would undo it...
cast0910 <- dcast.data.table(melt0910,RasterID1~variable,value.var=list("value"))
table(melt0910$variable)
melt2 <- melt0910[,variable :=as.character(variable)][variable=="X2008age"|variable=="X2009age", variable:="age"]
#table(melt2$variable)
melt3 <- melt2[variable=="X2008logAge"|variable=="X2009logAge", variable:="logAge"]
#table(melt3$variable)
melt4 <- melt3[variable=="lg_delta_bio_2009"|variable=="lg_delta_bio_2010", variable:="l.dbiom"]
melt5 <- melt4[variable=="strata09"|variable=="strata10", variable:="strata"]
#table(melt5$variable)
rm(dt_0910,melt0910,melt2,melt3,melt4)
cast2 <- dcast.data.table(melt5,RasterID1~variable)
# the above line does not preserve the attributes, all vars become integers
RasterID <- melt5[variable=="age"][,RasterID1]
age <- melt5[variable=="age"][,value]
logAge <- melt5[variable=="logAge"][,value]
l.dbiom <- melt5[variable=="l.dbiom"][,value]
strata <-as.character(melt5[variable =="strata"][,value])
# dt1 <- data.table(cbind(as.integer(RasterID),as.character(strata),as.numeric(l.dbiom),as.numeric(age),as.numeric(logAge)))
# str(dt1)
# When I put them all together, everything gets changed to character...
# so, let's run this as vetors
startTime <- Sys.time()
mem1vec <- lmer(formula= l.dbiom~strata+logAge*strata+age+(1|RasterID),REML=FALSE)
endTime <- Sys.time()
fit.time <- endTime-startTime #Time difference of 13.69814 mins

# would this run with dt1??
memdt1 <- lmer(formula= V3~V2+V5*V2+V4+(1|V1),data=dt1,REML=FALSE)
# this is the error...I think it is seeing everything as a character
# Error in qr.default(X, tol = tol, LAPACK = FALSE) : 
#   too large a matrix for LINPACK
# try this
dt2 <- transform(dt1,RasterID = as.numeric(V1),strata=as.character(V2),l.dbiom=as.numeric(V3),age=as.numeric(V4),logAge=as.numeric(V5))
# keeps all the vars...Vs and new names
# but str() seems right for the new vars
# try this
memdt2 <- lmer(formula= l.dbiom~strata+logAge*strata+age+(1|RasterID),data=dt2,REML=FALSE)

rm(b0,b1,dt1,melt5,params.matrix,age,l.dbiom,logAge,RasterID,strata)
# did the results make any sense?
# with the vector inputs
AIC(mem1vec)
lyhat <- fitted(mem1vec)
plot(lyhat,l.dbiom)
rm(lyhat)
params.matrix.v <- as.data.frame(summary(mem1vec)$coefficients)
b0v <- params.matrix.v[1:16,1:2]
# strata11 is the "not-labelled" intercept and slope
# there no slope value for strata23 strata73 (strata*logAge)
#rm(cast2,cast0910)
b1v <- params.matrix.v[c(17,19:31),1:2]
b2v <- params.matrix.v[18,1:2]

# here...figure out the count
strata.n <- dt2[,list(n.strata = length(l.dbiom)),by=strata]
b <- rep("b0",dim(b0v)[[1]])
b0v <- cbind(strata.n,b,b0v)
setnames(b0v, c("strata","n.strata","b","Estimate","Std. Error"),c("stratum","n","b","value","se"))
# assuming a normal dist and a 95%
b0v <- mutate(b0v,b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
             upper.b=value+b.er) 

b <- rep("b1",dim(b1v)[[1]])
# take-out strata23 and strata 73 from strata.n
b1v <- cbind(strata.n[strata!=73 & strata!=23],b,b1v)
setnames(b1v,c("strata","n.strata","b","Estimate","Std. Error"),c("stratum","n","b","value","se"))
b1v <- mutate(b1v,b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
             upper.b=value+b.er) 

setnames(b2v,c("Estimate","Std. Error"),c("value","se"))
b2v <- mutate(b2v,stratum="00",n=11853244,b="b2",b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
             upper.b=value+b.er) %>%
  select(stratum,n,b,value,se,b.er,lower.b,upper.b)

param.CIvector <- rbind(b0v,b1v,b2v)
# data.frame with parameters and their CI----------------------------
# with the dt2 inputs
AIC(memdt2)
# I am stopping here b/c the AIC is EXACTLY the same... so parameters will be also

# make graphs
library(ggplot2)
# save it somewhere-------------------------------------------------
#write.table(param.CI,file="BiomModelParamsCI_RS.txt",sep=",",row.names=FALSE)
#------------------------------------------------------------------

ci.plot <- ggplot(data=param.CIvector, aes(y=value,x=stratum, group=b, colour=b)) + 
  geom_errorbar(aes(ymin=lower.b,ymax=upper.b), width=.2) + geom_point()

# DONE

# Problems: 
# variable classes were all changed to integer
#------------------------------------------------------





