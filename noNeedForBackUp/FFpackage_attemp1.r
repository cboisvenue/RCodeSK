#------------------------------------------------------------------------------
# 1st attempt at ff package
# the goal if to create a dataframe (or vectors) that I can access from
# disk to fit a mixed effect model to.
# The premise being that the dataframe would be too big to handle in RAM
#
# CBoisvenue
# July 16, 2015
#-----------------------------------------------------------------------------

# two example dataframe from RS extraction sit here:
indir = "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/growth/"


# this is what I would do if I had no memory issues:
#------------------------------------------------------------------------------
#startTime <- Sys.time()
delta12 <-read.table(paste(indir,"RS_2012_df.txt",sep=""),sep=",",header = TRUE)
#endTime <- Sys.time()
#loadTime12 <- endTime-startTime #19.4584 secs
#startTime <- Sys.time()
delta11 <-read.table(paste(indir,"RS_2011_df.txt",sep=""),sep=",",header = TRUE)
# endTime <- Sys.time()
# loadTime11 <- endTime-startTime #15.4738 secs
# startTime <- Sys.time()
delta85 <-read.table(paste(indir,"RS_1985_df.txt",sep=""),sep=",",header = TRUE)
# endTime <- Sys.time()
# loadTime85 <- endTime-startTime #15.4738 secs

#in theory I have 28 of these, so no memory problems it would take ~28*20 to load 
# the data - just over 9 minutes

# After some fiddling (below), I realized that I need a few observation on the same 
# pixel to be able to use it for modelling
library(plyr)
library(dplyr)
join.t1 <- inner_join(delta11,delta12)
rm(delta11)
rm(delta12)
join.t2 <- inner_join(join.t1,delta85)
rm(delta85)
rm(join.t1)
# this worked: uses less memory than me doing it "by hand" (commented code below)
### I need more observation per pixle number
# asked for another df from Byron, got 1985, read it in (up)
# pick the pixels that repeat in all three df
#---------------------------------------------------------------------
# # which is the bigger one?
# dim(delta85) #[1] 6694332       5
# # 85 >12>11
# # so, which RasterID are in delta11 and delta12? and in delta85?
# 
# pix1 <- which(delta11$RasterID %in% delta12$RasterID)
# pix2 <- which(delta11$RasterID[pix1] %in% delta85$RasterID)
# 
# delta11.01 <- delta11[pix1,]
# rm(delta11)
# delta11.1 <-delta11.01[pix2,]
# rm(delta11.01)
# 
# pix3 <- which(delta12$RasterID %in% delta11.1$RasterID)
# delta12.1 <- delta12[pix3,]
# rm(delta12)
# 
# pix4 <- which(delta85$RasterID %in% delta11.1$RasterID)

# reshape
library(tidyr)
flip1 <- join.t2 %>%
  gather(vars,value,-RasterID,-strata)

rm(join.t2)

table(flip1$vars)
# X2011age       X2011logAge lg_delta_bio_2011          X2012age 
# 501569            501569            501569            501569 
# X2012logAge lg_delta_bio_2012          X1985age       X1985logAge 
# 501569            501569            501569            501569 
# lg_delta_bio_1985 
# 501569 
age <- filter(flip1,vars=="X2011age"|vars=="X2012age"|vars=="X1985age") %>%
  select(RasterID,strata,age=value) 
logAge <-  filter(flip1,vars=="X2011logAge"|vars=="X2012logAge"|vars=="X1985logAge") %>%
  transmute(logAge=value) 
l.biom <- filter(flip1,vars=="lg_delta_bio_2011"|vars=="lg_delta_bio_2012"|vars=="lg_delta_bio_1985") %>%
  transmute(l.biom=value)
data1 <- cbind(age,logAge,l.biom)
data1$strata <- as.character(data1$strata)
rm(age,logAge,l.biom,flip1)

# can I fit a model on these?
#---------------------------------------

library(lme4)
startTime <- Sys.time()
mem3deltas <- lmer(formula= l.biom~strata+logAge*strata+age+(1|RasterID),data=data1,REML=FALSE)
# messages:
#fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
# Warning message:
#   Some predictor variables are on very different scales: consider rescaling 
endTime <- Sys.time()
fit.time <- endTime-startTime #Time difference of 1.23422 mins

# could I feed-in a data.table?
library(data.table)
dt1 <- data.table(data1)
memDT_3deltas <- lmer(formula= l.biom~strata+logAge*strata+age+(1|RasterID),data=dt1,REML=FALSE)

# Evaluate------------------------------------------------------------------
# plot fitted against observed
AIC(mem3deltas)
lyhat <- fitted(mem3deltas)
data3 <- cbind(data1,lyhat)
library(ggplot2)
obs.pred <- ggplot(data=data3,aes(x=l.biom,y=lyhat)) + geom_point()
obs.pred
rm(obs.pred)
rm(lyhat)
rm(data3)

# pull-out the parameter estimates and their se--------------------------------------
params.matrix <- as.data.frame(summary(mem3deltas)$coefficients)
b0 <- params.matrix[1:16,1:2]
# since there no slope value for starta13, get rid of the intercept
b0 <- b0[-2,]
b1 <- params.matrix[c(17,19:32),1:2]
b2 <- params.matrix[18,1:2]
strata.n <- group_by(data1,strata) %>%
  summarise(n=n())
strata.n <- strata.n[-2,]
b <- rep("b0",dim(b0)[[1]])
b0 <- cbind(strata.n,b,b0)
names(b0) = c("stratum","n","b","value","se")
# assuming a normal dist and a 95%
b0 <- mutate(b0,b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
             upper.b=value+b.er) 

b <- rep("b1",dim(b1)[[1]])
b1 <- cbind(strata.n,b,b1)
names(b1) = c("stratum","n","b","value","se")
b1 <- mutate(b1,b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
             upper.b=value+b.er) 

names(b2) = c("value","se")
b2 <- mutate(b2,stratum="00",n=1504704,b="b2",b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
             upper.b=value+b.er) %>%
  select(stratum,n,b,value,se,b.er,lower.b,upper.b)

param.CI <- rbind(b0,b1,b2)
# data.frame with parameters and their CI----------------------------

# save it somewhere-------------------------------------------------
#write.table(param.CI,file="BiomModelParamsCI_RS.txt",sep=",",row.names=FALSE)
#------------------------------------------------------------------

ci.plot <- ggplot(data=param.CI, aes(y=value,x=stratum, group=b, colour=b)) + 
  geom_errorbar(aes(ymin=lower.b,ymax=upper.b), width=.2) + geom_point()













# BELOW IS FROM BEFORE I STARTED USING dplyr FOR THIS...
std.names <- c("RasterID","age","logAge","lg_delta_biom","strata")
names(delta11) <-std.names
names(delta12) <-std.names
names(delta85) <- std.names

# on one dataframe
library(lme4)
startTime <- Sys.time()
mem11 <- lmer(formula= lg_delta_biom~strata+logAge*strata+age+(1|RasterID),data=delta11,REML=FALSE)
endTime <- Sys.time()
fit.time <- endTime-startTimev #Time difference of 1.103833 mins
# did not fit, this is the error:
# Error: number of levels of each grouping factor must be < number of observations
# I think this means I don't have a repeat of the pixels so I can't use them as 
# Would having a re-measured plot...
range(delta11$RasterID)# [1]     5580 69901580
length(unique(delta11$RasterID))#4454273
dim(delta11)
#[1] 4454273       5
range(delta12$RasterID) #[1]     5575 69901595
length(unique(delta12$RasterID))#4454273
dim(delta12)
#[1] 6120707       5

# try merging the dataframes and see if it can fit
delta.2df <- rbind(delta12,delta11)
dim(delta.2df)#[1] 10574980        5

startTime <- Sys.time()
mem2df <- lmer(formula= lg_delta_biom~strata+logAge*strata+age+(1|RasterID),data=delta11,REML=FALSE)
endTime <- Sys.time()
fit.time2df <- endTime-startTime #Time difference of 55.968 secs
# same error  
#Error: number of levels of each grouping factor must be < number of observations  

table(delta11$strata)
# 11      13      21      23      31      32      33      41      42      43      51 
# 228      19     666       8 1468197      53  135290 1371052    5927    3654 1159779 
# 52      53      61      63      71      73 
# 248     678   34961    1001  272171     341 
table(delta12$strata)
# 11      13      21      23      31      32      33      41      42      43      51 
# 630      10     614       7 2150474      29  253835 2041202    7951    7267 1353883 
# 52      53      61      63      71      73 
# 262    1150   42542    1462  259034     355  

# I think I need these to be characters so that the model fits them as a dummy variable 
# and not a value...
delta.2df$strata <- as.character(delta.2df$strata)
table(delta.2df$strata)
# 11      13      21      23      31      32      33      41      42      43      51 
# 858      29    1280      15 3618671      82  389125 3412254   13878   10921 2513662 
# 52      53      61      63      71      73 
# 510    1828   77503    2463  531205     696   

startTime <- Sys.time()
mem2df <- lmer(formula= lg_delta_biom~strata+logAge*strata+age+(1|RasterID),data=delta11,REML=FALSE)
endTime <- Sys.time()
fit.time2df <- endTime-startTime #Time difference of 56.563 secs
# same error
# Error: number of levels of each grouping factor must be < number of observations


  
  
  
  

