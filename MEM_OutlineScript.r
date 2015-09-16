#-------------------------------------------------------------------------
# bare bones mixed effects model script
# this is an attempt to outline the steps needed to be done on the rasters
# for fitting mixed effects models
#
# CBoisvenue
# July 8th, 2015
#--------------------------------------------------------------------------

require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)


# Read-in data----------------------------------------------------------
setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/")

biom.data <- read.table("t_haBiom_yr.txt",sep=",",header=TRUE)
biom.data <- select(biom.data,PLOT_ID,age1,biom.ha.inc,stratum)

# this is what the fitting data looks like -----------------------------
head(biom.data)
#   PLOT_ID      age1 biom.ha.inc stratum
# 1   20004  82.47944   0.7185486     BSG
# 2   20004  82.64160   0.2680702     BSG
# 3   20005 155.73869   1.1735351     BSG
# 4   20005 168.60951   0.8554414     BSG
# 5   20006  72.98071   0.9299571     BSG
# 6   20006  75.41197   0.3275543     BSG
# etc.

# these are the strata
table(biom.data$stratum)
# BF  BP BSG BSM  JP TAG TAM  WB WSG WSM 
# 53  43 150  28 351 175  15  45 493  26 
# end of read-in data---------------------------------------------------

# transform the variables to "linearize" the model---------------
data1 <- mutate(biom.data,ly = log(biom.ha.inc), l.age=log(age1))
# there was only one strata for WS - "WSG", so this lines makes all spruce "
data2 <- data1
data2$stratum[which(data2$stratum=="WSM")] <- "WSG"
#- end transform-------------------------------------------------

# fit mixed-effect model on the data-----------------------------
library(lme4)
mem <- lmer(formula= ly~stratum+l.age*stratum+age1+(1|PLOT_ID),data=data2,REML=FALSE)

# Need to save the model somewhere...
# note that I anticipate that we will have to sub-sample the RS data and fit this type of model 
# quite a few times to make sure that we have ok param distributions
#save(mem,file="G:/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/MEM_t_ha/MEM_BiomIncRS.RData")
# Fitted done-----------------------------------------------------

# Evaluate------------------------------------------------------------------
# plot fitted against observed
AIC(mem)
lyhat <- fitted(mem)
data3 <- cbind(data2,lyhat)
obs.pred <- ggplot(data=data3,aes(x=ly,y=lyhat)) + geom_point()
obs.pred


# pull-out the parameter estimates and their se--------------------------------------
params.matrix <- as.data.frame(summary(mem)$coefficients)
b0 <- params.matrix[1:9,1:2]
b1 <- params.matrix[c(10,12:19),1:2]
b2 <- params.matrix[11,1:2]
strata.n <- group_by(data2,stratum) %>%
  summarise(n=n())
b <- rep("b0",9)
b0 <- cbind(strata.n,b,b0)
names(b0) = c("stratum","n","b","value","se")
# assuming a normal dist and a 95%
b0 <- mutate(b0,b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
             upper.b=value+b.er) 

b <- rep("b1",9)
b1 <- cbind(strata.n,b,b1)
names(b1) = c("stratum","n","b","value","se")
b1 <- mutate(b1,b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
             upper.b=value+b.er) 

names(b2) = c("value","se")
b2 <- mutate(b2,stratum="ALL",n=1353,b="b2",b.er = qnorm(0.975)*se/sqrt(n),lower.b=value-b.er,
             upper.b=value+b.er) %>%
  select(stratum,n,b,value,se,b.er,lower.b,upper.b)

param.CI <- rbind(b0,b1,b2)
# data.frame with parameters and their CI----------------------------

# save it somewhere-------------------------------------------------
write.table(param.CI,file="BiomModelParamsCI_RS.txt",sep=",",row.names=FALSE)
#------------------------------------------------------------------

ci.plot <- ggplot(data=param.CI, aes(y=value,x=stratum, group=b, colour=b)) + 
  geom_errorbar(aes(ymin=lower.b,ymax=upper.b), width=.2) + geom_point()

# ggsave(file="BiomassMEMparamsCI.jpeg",plot=ci.plot)



