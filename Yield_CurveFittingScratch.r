# ---------------------------------------------------------------------------
# Yield curve fitting by strata for the "basic" spatial CBM-CFS3 runs in SK
# 
# June 26, 2016
# CBoisvenue
#---------------------------------------------------------------------------


#library(nlme)
library(ggplot2)
require(plyr)
require(dplyr)

# Read-in data
setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/")

yield.data <- read.table("Plot_Inc_m3_ha_yr.txt",sep=",",header=TRUE)
total.vol <-  read.table("totalVol_haPlot.txt",sep=",",header=TRUE)
hist(total.vol$totalm3_ha)

# 1st try - all commented b/c I am not using this----------------------------------------------
# this is the 1st nlme try, using the stratum (10) to group---------------------------------
#attach(yield.data)
# data1 = groupedData(m3.ha.yr~plot.age|stratum)
# plot(data1)
# ggplot(data=yield.data,aes(x=plot.age,y=m3.ha.yr)) + geom_point(aes(colour=factor(stratum)))
#ym1=nlsList(m3.ha.yr~a*(plot.age)^b*exp(c*plot.age),data=data1, start=list(a=10,b=5,c=5))
# this does not work at all...can't figure out the error messages either

# trying nls
a=0.000002
b=1.03
c=0.1
table(yield.data$stratum)
# try with the stratum with the most observations
# wsg <- filter(yield.data,stratum=="WSG")
# ym3 <- nlsList()
# ym2 <-nlme(m3.ha.yr~a*(plot.age)^b*exp(c*plot.age),fixed=plot.age~1,data=wsg, start=list(a=a,b=b,c=c))

# End 1st try--------------------------------------------------------------------------------

# keep trying: linearize the model by log--------------------------------------------------
install.packages("lme4")
library(lme4)

# just to make sure I can fit something, try fitting linear models-----------
ylm <- lm(formula= m3.ha.yr~plot.age,data=yield.data)

yglm <- glm(formula= m3.ha.yr~plot.age,data=yield.data)
# varying intercepts
yglm2 <- glm(formula= m3.ha.yr~plot.age + stratum,data=yield.data)
# try with just one strata
#WSGglm <- glm(formula = m3.ha.yr~plot.age,data=wsg)
# these all solve -----------------------------------------------------------

# linearize this model-----------------------------------------------------------
data2 <- mutate(yield.data,ly = log(m3.ha.yr), l.age=log(plot.age)) 

# trying to fit the same equation on total volume...
dataT1 <- select(total.vol,PLOT_ID,plot.age,totalm3_ha,stratum=stratum.1) %>%
  mutate(ltotvol = log(totalm3_ha),l.age=log(plot.age))
# now I can fit a linear mixed effect model:
# varying intercept
lylme <- lmer(formula= ly~l.age+plot.age+(1|stratum),data=data2)

lylme1 <- lmer(formula= ly~l.age+plot.age+(1+l.age|stratum),data=data2)

lylme2 <- lmer(formula= ly~l.age+plot.age+(1+l.age + plot.age|stratum),data=data2)
ltotvollme <- lmer(formula= ltotvol~l.age+plot.age+(1+l.age + plot.age|stratum),data=dataT1)
# does not converge
# rescale dependent vars: (x-mean of that colum)/standard deviation
data3 <- select(data2,PLOT_ID,stratum,ly,plot.age,l.age) %>%
  mutate(age.s = scale(plot.age), l.age.s = scale(l.age))
dataT2 <- mutate(dataT1,age.s = scale(plot.age), l.age.s = scale(l.age))
# here calculate the mean and sd for each column so that I can "descale"
###HERE#### NEED TO CALCULATE SOMETHING

# this syntax let's there be correlation between the intecept and the slopes
lylme3 <- lmer(formula= ly~l.age.s+age.s+(1+l.age.s + age.s|stratum),data=data3)
ltotvollme <- lmer(formula= ltotvol~l.age.s+age.s+(1+l.age.s + age.s|stratum),data=dataT2)
# this syntax does not have
lylme3.1 <- lmer(formula= ly~l.age.s+age.s+(1|stratum)+(0+l.age.s|stratum) + (0+age.s|stratum),data=data3)
# this seems to give population intercept AND an intercept for each slope
lylme3.2 <- lmer(formula= ly~l.age.s+age.s+(1|stratum)+(1+l.age.s|stratum) + (1+age.s|stratum),data=data3)
# this seems to give me an intercept with EACH slope (not what I want) and 
# no "population intercept"
lylme3.3 <- lmer(formula= ly~l.age.s+age.s+(l.age.s|stratum) + (age.s|stratum),data=data3)
# SAME RESIDUALS EVERYWHERE...

#lylme4 <- lmer(formula= ly~l.age.s+age.s+(l.age.s + age.s|stratum),data=data3)
# the above gives the same results as lylme3, but lylme3 seems to have the recomended syntax

plot(lylme3)
lyhat <- fitted(lylme3)
yhat <- exp(lyhat)

fitted.X <- model.matrix(lylme3) # this gives my the input vars back
nstrata <- row.names(ranef(lylme3)$stratum)
# these are the parameters for each strata, random and fixed effects included
params.lme <- as.data.frame(cbind(stratum=row.names(ranef(lylme3)$stratum),ranef(lylme3)$stratum))
names(params.lme) = c("stratum","b0","b1","b2")

#can I predict from this for each stratum?
plot.age <- rep(1:250)
age.s = scale(plot.age)
l.age <-log(plot.age)
l.age.s = scale(l.age)

topredMM <- as.data.frame(cbind(plot.age,l.age,age.s,l.age.s))
names(topredMM) = c("plot.age","l.age","age.s","l.age.s")
stratum <- as.factor(sort(rep(as.character(levels(yield.data$stratum)),250)))
topredMM <- cbind(stratum,topredMM)
lyhat2 <- predict(lylme3,newdata=topredMM)
ltotvolhat <-predict(ltotvollme,newdata=topredMM)
predMM <- cbind(topredMM,lyhat2)
totvolpredMM <- cbind(topredMM,ltotvolhat)
predMM <- mutate(predMM,yhat2 = exp(lyhat2)) 
totvolpredMM <-  mutate(totvolpredMM,totvolhat = exp(ltotvolhat)) 
  
MEMpspYield <- ggplot(data=predMM,aes(x=plot.age,y=yhat2,group=stratum,colour=stratum)) + geom_line(size=1.5)
MEMpspYield + ggtitle("Predicting m3/ha/yr by Strata") +  scale_fill_brewer(palette="Spectral")
ggsave("plotYieldCurvesMEModelPSP.jpeg")

MEMpspVol <- ggplot(data=totvolpredMM,aes(x=plot.age,y=totvolhat,group=stratum,colour=stratum)) + geom_line(size=1.5)
MEMpspVol + ggtitle("Predicting total m3/ha by Strata") +  scale_fill_brewer(palette="Spectral")
ggsave("plotGrowthCurvesMEModelPSP.jpeg")

# volumeYield <- lapply(predMM$yhat2,cumsum) 
# %>%
#   summarise(cumsum=cumsum(yhat2))

MEMpspYield <- ggplot(data=predMM,aes(x=plot.age,y=cumsum,group=stratum,colour=stratum)) + geom_line(size=1.5)
MEMpspYield + ggtitle("Predicting m3/ha by Strata") +  scale_fill_brewer(palette="Spectral")

write.table(predMM,file="MEMPredictedYields.txt",sep=",",row.names=FALSE)
save(lylme3,file = "YieldCurvesMEModel.RData")
write.table(totvolpredMM,file="MEMPredictedGrowth.txt",sep=",",row.names=FALSE)
save(ltotvollme,file = "GrowthCurvesMEModel.RData")
# 

########################################
##############################
################
# Before I could get the "predict" function to work I tried to predict by
# using the ranef() parameters by stratum...there was something wrong, the results 
# were wonky

# by doing this below, I am using the random effects...and the fixed effects
# 
# lme.pred <- merge(params.lme,topredMM) %>%
#   arrange(stratum)%>%
#   mutate(l.yhat = (b0+b1*as.numeric(l.age.s)+b2*as.numeric(age.s)), y.hat = exp(l.yhat))

# ggplot(data=lme.pred,aes(x=plot.age,y=y.hat,group=stratum,colour=stratum)) + geom_line()
# no.bsg <- filter(mle.pred,stratum!="BSG")
# ggplot(data=no.bsg,aes(x=as.numeric(plot.age[1:250]),y=lyhat,group=stratum,colour=stratum)) + geom_line()

# Note: the code from here down was before I figured out how to get ranef()---------------------------------
# I need parameter estimates for each stratum, so fit each stratum seperatly

# # Seperate strata and fit the linear form for each -----------------------------------
# table(stratum)
# wsg <- data3[which(data3$stratum=="WSG"),]
# bf <- data3[which(data3$stratum=="BF"),]
# bp <- data3[which(data3$stratum=="BP"),]
# bsg <- data3[which(data3$stratum=="BSG"),]
# bsm <- data3[which(data3$stratum=="BSM"),]
# jp <- data3[which(data3$stratum=="JP"),]
# tag <- data3[which(data3$stratum=="TAG"),]
# tam <- data3[which(data3$stratum=="TAM"),]
# wb <- data3[which(data3$stratum=="WB"),]
# wsm <- data3[which(data3$stratum=="WSM"),]
# 
# # the the non-scaled version...using glm because of the high correlation
# wsg.glm <- glm(formula= ly~l.age+plot.age,data = wsg)
# bf.glm <- glm(formula= ly~l.age+plot.age,data = bf)
# bp.glm <- glm(formula= ly~l.age+plot.age,data = bp)
# bsg.glm <- glm(formula= ly~l.age+plot.age,data = bsg)
# bsm.glm <- glm(formula= ly~l.age+plot.age,data = bsm)
# jp.glm <- glm(formula= ly~l.age+plot.age,data = jp)
# tag.glm <- glm(formula= ly~l.age+plot.age,data = tag)
# tam.glm <- glm(formula= ly~l.age+plot.age,data = tam)
# wb.glm <- glm(formula= ly~l.age+plot.age,data = wb)
# wsm.glm <- glm(formula= ly~l.age+plot.age,data = wsm)
# 
# # Not significant parameters for: bp,bsm,jp,tam,wb,wsm --------------------------
# #playing with bp to see what can be done
# ## THIS WAS WORKING AND NOW DOES NOT CANNOT FIGURE OUT WHY (MAYBE DON'T NEED TO NOW THAT
# ## I HAVE SEEMINGLY REASONABLE YIELD CURVES FROM THE MEM)
# bp.lm <- lm(formula= ly~l.age+plot.age,data = bp)
# # gives the exact same results as glm
# require(stats4)
# LL <- function(b0,b1,b2,mu,sigma) {
#   # find residuals
#   R=ly-b0-b1*l.age-b2*plot.age
#   R = suppressWarnings(dnorm(R, mean=mu, sd=sigma))
#   -sum(R)
# }
# attach(bp)
# data.in <- select(bp,ly)
# bp.mle <- mle(LL,start = list(b0=-0.219,b1=0.71,b2=-0.012,mu=0,sigma=1),data=data.in)
# # fixing the mean of the residuals to 0
# bp.mle2 <- mle(LL,start = list(b0=-0.219,b1=0.71,b2=-0.12,sigma=1),fixed=list(mu=0),
#                nobs = length(ly))
# # ok - mle2 for all the ones that did not give me significant parameter estimates---
# # 6 out of 10. Initial values taken from the glm
# detach(bp)
# 
# attach(bsm)
# bsm.mle2 <- mle(LL,start = list(b0=-14,b1=4.37,b2=-0.054,sigma=1),fixed=list(mu=0),
#                nobs = length(ly))
# detach(bsm)
# 
# attach(jp)
# jp.mle2 <- mle(LL,start = list(b0=coefficients(jp.glm)[[1]],b1=coefficients(jp.glm)[[2]],b2=coefficients(jp.glm)[[3]],sigma=1),fixed=list(mu=0),
#                 nobs = length(ly))
# detach(jp)
# 
# attach(tam)
# tam.mle2 <- mle(LL,start = list(b0=coefficients(tam.glm)[[1]],b1=coefficients(tam.glm)[[2]],b2=coefficients(tam.glm)[[3]],sigma=1),fixed=list(mu=0),
#                 nobs = length(ly))
# detach(tam)
# 
# attach(wb)
# wb.mle2 <- mle(LL,start = list(b0=coefficients(wb.glm)[[1]],b1=coefficients(wb.glm)[[2]],b2=coefficients(wb.glm)[[3]],sigma=1),fixed=list(mu=0),
#                 nobs = length(ly))
# detach(wb)
# 
# attach(wsm)
# wsm.mle2 <- mle(LL,start = list(b0=coefficients(wsm.glm)[[1]],b1=coefficients(wsm.glm)[[2]],b2=coefficients(wsm.glm)[[3]],sigma=1),fixed=list(mu=0),
#                 nobs = length(ly))
# detach(wsm)
# 
# # Now I have fitted parameters for all 10 strata: calculate fitted values for each year,
# # convert back to m3/ha/yr----------------------------------------------------------------
# 
# # the models fit by glm: bf,bsg,tag,wsg
# plot.age <- 1:250
# l.age <-log(pred.age)
# topred <- as.data.frame(cbind(plot.age,l.age))
# bf.pred <-predict.glm(bf.glm,newdata=topred)
# bf.m3 <- exp(bf.pred)
# bsg.pred <-predict.glm(bsg.glm,newdata=topred)
# bsg.m3 <- exp(bsg.pred)
# tag.pred <-predict.glm(tag.glm,newdata=topred)
# tag.m3 <- exp(tag.pred)
# wsg.pred <-predict.glm(wsg.glm,newdata=topred)
# wsg.m3 <- exp(wsg.pred)
# 
# # likelihood fit: bp,bsm,jp,tam,wb,wsm 
# 
# mlp.bp = c(bp.mle2@coef[[1]],bp.mle2@coef[[2]],bp.mle2@coef[[3]])
# mlp.bsm = c(bsm.mle2@coef[[1]],bsm.mle2@coef[[2]],bsm.mle2@coef[[3]])
# mlp.jp = c(jp.mle2@coef[[1]],jp.mle2@coef[[2]],jp.mle2@coef[[3]])
# mlp.tam = c(tam.mle2@coef[[1]],tam.mle2@coef[[2]],tam.mle2@coef[[3]])
# mlp.wb = c(wb.mle2@coef[[1]],wb.mle2@coef[[2]],wb.mle2@coef[[3]])
# mlp.wsm = c(wsm.mle2@coef[[1]],wsm.mle2@coef[[2]],wsm.mle2@coef[[3]])
# mlp <- as.data.frame(rbind(mlp.bp,mlp.bsm,mlp.jp,mlp.tam,mlp.wb,mlp.wsm))
# names(mlp) = c("b0","b1","b2")
# 
# bp.pred = mlp$b0[1]+mlp$b1[1]*l.age+mlp$b2[1]*plot.age
# bp.m3 = exp(bp.pred)
# plot(bp.m3)
# # PROBLEM: this does NOT come back down
# bsm.pred = mlp$b0[2]+mlp$b1[2]*l.age+mlp$b2[2]*plot.age
# bsm.m3 = exp(bsm.pred)
# plot(bsm.m3)
# 
# jp.pred = mlp$b0[3]+mlp$b1[3]*l.age+mlp$b2[3]*plot.age
# jp.m3 = exp(jp.pred)
# plot(jp.m3)
# 
# tam.pred = mlp$b0[4]+mlp$b1[4]*l.age+mlp$b2[4]*plot.age
# tam.m3 = exp(tam.pred)
# plot(tam.m3)
# # PROBLEM: this does NOT come back down - can lump that with TAG
# wb.pred = mlp$b0[5]+mlp$b1[5]*l.age+mlp$b2[5]*plot.age
# wb.m3 = exp(wb.pred)
# plot(wb.m3)
# 
# wsm.pred = mlp$b0[6]+mlp$b1[6]*l.age+mlp$b2[6]*plot.age
# wsm.m3 = exp(wsm.pred)
# plot(wsm.m3)
# # ? the graph is flat?

# yield data----------------------------------------------------------------------------
# read-in
yield.data <- read.table("Plot_Inc_m3_ha_yr.txt",sep=",",header=TRUE)
# look
yield.inc.yr <- ggplot(data=yield.data,aes(x=plot.age,y=m3.ha.yr)) + geom_point()
yield.inc.yr + geom_point(aes(colour=factor(stratum)))

dataT1 <- select(total.vol,PLOT_ID,plot.age,totalm3_ha,stratum=stratum.1) %>%
  mutate(ltotvol = log(totalm3_ha),l.age=log(plot.age))

total.m3 <- ggplot(data=dataT1,aes(x=plot.age,y=totalm3_ha)) + geom_point()
total.m3 + geom_point(aes(colour=factor(stratum)))
# still linearize the model
datay1 <- mutate(yield.data,ly = log(m3.ha.yr), l.age=log(plot.age))
table(datay1$stratum)

# GLMs-----------------------------------------------------------------
glmy1 <- glm(ly~stratum+l.age+plot.age,data=datay1)
glmy2 <- glm(ly~stratum+l.age*stratum+plot.age*stratum,data=datay1)
anova(glm1,glm2,test="F")
AIC(glmy1)#1788.444
AIC(glmy2)#1802.048
## there are no repeat of plots...so there is no point fitting random effects on plots
glmy3 <- glm(ly~stratum+l.age*stratum+plot.age,data=datay1)
AIC(glmy3)#[1] 1789.368
# CONCLUSION: no need for MEM on yields, and varying intercept model (fixed effects)
# seems to be the best (glm1), no need to vary slopes (glm2 and glm3)

# does scaling change anything?--------
datay2 <- select(datay1,PLOT_ID,stratum,ly,plot.age,l.age) %>%
  mutate(age.s = scale(plot.age), l.age.s = scale(l.age))

glm4 <-  glm(ly~stratum+l.age.s+age.s,data=datay2)
# No, scaling does not help------------
# END GLMs--------------------------------------------------------------

##################################################################################
#BEST MODEL YIELD: varying intercepts (fixed) only and not mixed effects(glmy1)
summary(glmy1)
glmy1.lyhat <- fitted(glmy1)
plot(datay1$ly,glmy1.lyhat)
obs.pred1 <- lm(glmy1.lyhat~datay1$ly)
# Residual standard error: 0.3953 on 820 degrees of freedom
# Multiple R-squared:  0.312,	Adjusted R-squared:  0.3111 

# TOTAL VOLUME###############################################################
# Trying total vol -----------------------------------------------------
glmT1 <- glm(ltotvol~stratum+l.age+plot.age,data=dataT1)
glmT2 <- glm(ltotvol~stratum+l.age*stratum+plot.age*stratum,data=dataT1)
anova(glmT1,glmT2,test="F")
AIC(glmT1)#6333.412
AIC(glmT2)#6283.533

## there are no repeat of plots...so there is no point fitting random effects on plots
glmT3 <- glm(ltotvol~stratum+l.age*stratum+plot.age,data=dataT1)
AIC(glmT3)#[1] 6326.133

# try mem---------------------------------------
library(lme4)
memT <- lmer(formula= ltotvol~stratum+l.age*stratum+plot.age*stratum+(1|PLOT_ID),data=dataT1,REML=FALSE)
AIC(memT)#[1] 3589.153
anova(glmT2,memT)
memT1 <- lmer(formula= ltotvol~stratum+l.age*stratum+plot.age+(1|PLOT_ID),data=dataT1,REML=FALSE)
AIC(memT1)#3765.832
##################################################################################
#BEST MODEL TOTAL VOL: varying intercepts, and slopes for each stratum (fixed)
# and random effects on plot_ID
summary(memT)
memT.lyhat <- fitted(memT)
plot(dataT1$ltotvol,memT.lyhat)
obs.pred1 <- lm(memT.lyhat~dataT1$ltotvol)
# Residual standard error: 0.1862 on 3708 degrees of freedom
# Multiple R-squared:  0.9434,	Adjusted R-squared:  0.9434 
#
# PROBLEM WITH TAG??
plot(dataT1$ltotvol[which(dataT1$stratum=="TAG")],memT.lyhat[which(dataT1$stratum=="TAG")])
# Get rid of TAG and refit-----------------------
dataT3 <-filter(dataT1, stratum!="TAG")
dataT3$stratum <- as.character(dataT3$stratum)


# try again
memTnotag <- lmer(formula= ltotvol~stratum+l.age*stratum+plot.age*stratum+(1|PLOT_ID),data=dataT3,REML=FALSE)
AIC(memTnotag)#[1] 2736.334
## BEST MODEL FOR TOTAL VOLUME DOES NOT HAVE TAG DATA##############################


#Pretty curves? predict from this for each stratum?------------------------------------
plot.age <- rep(1:250)
l.age <-log(plot.age)

topredMM <- as.data.frame(cbind(plot.age,l.age))
names(topredMM) = c("plot.age","l.age")
# for yield 
stratum <- sort(rep(as.character(levels(dataT1$stratum)),250))
plot10 <- sample(dataT3$PLOT_ID,9)
PLOT_ID <- sort(rep(plot10,250))

# for total volume
stratum <- stratum[which(stratum!="TAG")]
plot10 <- sample(dataT3$PLOT_ID,9)
PLOT_ID <- sort(rep(plot10,250))

topredMM <- cbind(PLOT_ID,stratum,topredMM)
# memT.lyhat <- predict(memT,newdata=topredMM)
# predMM <- cbind(topredMM,memT.lyhat)
# predMM <- mutate(predMM,memT.yhat = exp(memT.lyhat)) 

# no TAG
# noTAGMM <- filter(topredMM,stratum!="TAG")
# noTAGMM$stratum <- as.character(noTAGMM$stratum)

noTAGlyhat <- predict(memTnotag,newdata=topredMM)
noTAGMM <- cbind(topredMM,noTAGlyhat)
noTAGMM  <- mutate(noTAGMM ,memT.notagyhat = exp(noTAGlyhat)) 


MEM_forCBM <- ggplot(data=predMM,aes(x=plot.age,y=memT.yhat,group=stratum,colour=stratum)) + geom_line(size=1.5)
MEM_forCBM + ggtitle("Predicting total m3/ha by Strata") +  scale_fill_brewer(palette="Spectral")
### NEED TO GET RID OF TAG...replace it by TAM

## Replacing TAG
MEM_noTAG <- ggplot(data=noTAGMM,aes(x=plot.age,y=memT.notagyhat,group=stratum,colour=stratum)) + geom_line(size=1.5)
MEM_noTAG + ggtitle("Predicting total m3/ha by Strata") +  scale_fill_brewer(palette="Spectral")
ggsave("plotGrowthCurvesMEModelPSP2_noTAG.jpeg")

write.table(noTAGMM,file="MEMPredictedGrowth_noTAG.txt",sep=",",row.names=FALSE)
save(memTnotag,file = "GrowthCurvesMEModel_noTAG.RData")



# build growth tables for CBM----------------
library(reshape2)
growth.table.out <- dcast(predMM,stratum~plot.age,value.var="memT.yhat")

strata.def <-read.table("YieldCurveStrata.csv",sep=",",header=TRUE)
strata.def <- select(strata.def,dom,prodClass, stratum)

growth.table.out <-left_join(strata.def,growth.table.out)

write.table(growth.table.out,file="GrowthTables_PSP.txt",row.names=FALSE,sep=",")


# extract parameters and their se, reconvert to non-log units
plot(wsg$plot.age,wsg.glm$fitted.values)

coefficients(bsg.glm)

wsm.mle2@coef[[1]]#[1] 13.61511
#squareroot of the diagonal of this gives me the sd
wsm.mle2@vcov
wsm.mle2@vcov[[1,1]]

## apparently we need total merch volume for CBM input
yield.table.out <- read.table("YieldTables_PSP.txt",header=TRUE, sep=",")

yield.only <- yield.table.out[,c(-1,-2,-3)]
yield.transposed <- t(yield.only)
strata1 <- cumsum(yield.transposed[,1])
strata4 <- cumsum(yield.transposed[,4])

max.age <- group_by(tree.meas,PLOT_ID) %>%
  