#--------------------------------------------------------------------------------
# Take 2 of fitting mixed effects models on the PSP plots
# After a discussion with Steve Cumming and Eliot McIntire, I realized that 
# the random effects should technically be on the plots (for PSPs and on the pixel for RS)
# This is an attempt to do just that while keeping in mind what I have recently learned
# about fitting models on large datasets (PAFMA would be ~10GB).
# The main limitation seems to be when matrices are inverted...so all glm lmem, etc.
# Note that I have to think through a maximum likelihood approach...but the model would
# be difficult for me to right...i think...
#
# - Do this for biomass (t/ha estimated from tree-level) data
# - Do this for the yield curves
# - Do this for total vol (to feed into CBM)
#
# CBoisvenue July 9th, 2015
#--------------------------------------------------------------------------

library(ggplot2)
require(plyr)
require(dplyr)

# start with biomass data --------------------------------------------------------------

# Read-in data-----------------------------
setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/")

biom.data <- read.table("t_haBiom_yr.txt",sep=",",header=TRUE)

# look
biom.inc.yr <- ggplot(data=biom.data,aes(x=age1,y=biom.ha.inc)) + geom_point()
biom.inc.yr + geom_point(aes(colour=factor(stratum)))

# still linearize the model
data1 <- mutate(biom.data,ly = log(biom.ha.inc), l.age=log(age1))
table(data1$stratum)

# glm --------------------------------------------------------------
# fit an intercept by stratum (10 of them), the 1st "non-labeled" one is BF, 
# the first stratum
glm1 <- glm(ly~stratum+l.age+age1,data=data1)
glm1.yhat <- fitted(glm1)

# fit an intercept and a 2 slopes by stratum
glm2 <- glm(ly~stratum+l.age*stratum+age1*stratum,data=data1)
# none of the sloped per strata are significant...
AIC(glm1) #[1] 1881.358
AIC(glm2) #[1] 1877.933
anova(glm1,glm2,test="F")
# Analysis of Deviance Table
# 
# Model 1: ly ~ stratum + l.age + age1
# Model 2: ly ~ stratum + l.age * stratum + age1 * stratum
# Resid. Df Resid. Dev Df Deviance      F   Pr(>F)   
# 1      1367     310.03                               
# 2      1349     301.29 18   8.7383 2.1736 0.003035 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# End glm--------------------------------

library(lme4)
# mem start here-------------------------
# adding a random effect on the plots in the intercept

# +stratum will give me an intercept per stratum
mem1 <- lmer(formula= ly~stratum+l.age+age1+(1|PLOT_ID),data=data1)
AIC(mem1)#[1] 1861.707

anova(mem1,glm2)
# refitting model(s) with ML (instead of REML)
# Data: data1
# Models:
#   mem1: ly ~ stratum + l.age + age1 + (1 | PLOT_ID)
# glm2: ly ~ stratum + l.age * stratum + age1 * stratum
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# mem1 14 1810.5 1883.7 -891.23   1782.5                        
# glm2 31 1877.9 2040.0 -907.97   1815.9     0     17          1

# fit the same modle with ML, seems to be a better fit
mem2 <- lmer(formula= ly~stratum+l.age+age1+(1|PLOT_ID),data=data1,REML=FALSE)
AIC(mem2) #1810.46

# add the random effects on one slope
mem3 <- lmer(formula= ly~stratum+l.age+age1+(1+l.age|PLOT_ID),data=data1,REML=FALSE)
# non-identifiable - bellow is the proper syntax
mem3.1 <- lmer(formula= ly~stratum+l.age+age1++(1|PLOT_ID)+(0+l.age|PLOT_ID),data=data1)
AIC(mem3.1)
#[1] 1862.192
# but no better

# keep random effects on the intercept only and add the interaction for 
# a slope at each stratum
mem4 <- lmer(formula= ly~stratum+l.age*stratum+age1+(1|PLOT_ID),data=data1,REML=FALSE)
AIC(mem4)#[1] 1804.776
anova(mem2,mem4)
# Data: data1
# Models:
#   mem2: ly ~ stratum + l.age + age1 + (1 | PLOT_ID)
# mem4: ly ~ stratum + l.age * stratum + age1 + (1 | PLOT_ID)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
# mem2 14 1810.5 1883.7 -891.23   1782.5                            
# mem4 23 1804.8 1925.0 -879.39   1758.8 23.684      9   0.004831 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

mem4.1 <- lmer(formula= ly~stratum+l.age*stratum+age1+(1|PLOT_ID)+(0+l.age|PLOT_ID),data=data1,REML=FALSE)
anova(mem4,mem4.1)
# Data: data1
# Models:
#   mem4: ly ~ stratum + l.age * stratum + age1 + (1 | PLOT_ID)
# mem4.1: ly ~ stratum + l.age * stratum + age1 + (1 | PLOT_ID) + (0 + 
#                                                                    mem4.1:     l.age | PLOT_ID)
# Df    AIC  BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# mem4   23 1804.8 1925 -879.39   1758.8                         
# mem4.1 24 1805.5 1931 -878.77   1757.5 1.2297      1     0.2675
## Not better


# # checking with Byron to see if the l.age value can be multiplied by 1000 to make the raster 
# # creation process a bit easier---------------------------------------------------------
# data1.1 <- mutate(data1,lage00 = 1000*l.age)
# mem4.1 <- lmer(formula= ly~stratum+lage00*stratum+age1+(1|PLOT_ID),data=data1.1,REML=FALSE)
# AIC(mem4)#[1] 1804.776
# the answer: better not----------------------------------------------------------------

# add strata on both slopes
mem5 <- lmer(formula= ly~stratum+l.age*stratum+age1*stratum+(1|PLOT_ID),data=data1,REML=FALSE)
AIC(mem5)#[1]  1813.392
# got worse!!
anova(mem4,mem5)

#would scaling variables be better?------------
data2 <- mutate(data1,age.s = scale(age1), l.age.s = scale(l.age))
# try it on the "best" model
mem4.s <- lmer(formula= ly~stratum+l.age.s*stratum+age.s+(1|PLOT_ID),data=data2)
AIC(mem4.s)#[1] 1879.122
#NOPE------------------------------------------

# does no varying intercept get better?
mem6 <-  lmer(formula= ly~l.age+age1+(1|PLOT_ID),data=data1)
AIC(mem6)# [1] 2074.62
# no------------------------------------------

# mem models end here-------------------------
# biomass data end-----------------------------------------------------------------------
##################################################################################
#BEST MODEL BIOMASS: varying intercepts (fixed) and varying slope on l.age (fixed), random effects
# on plots, just one parameter on age1
summary(mem4)
mem4.lyhat <- fitted(mem4)
plot(data1$ly,mem4.lyhat)
obs.pred <- lm(mem4.lyhat~data1$ly)
summary(obs.pred)
# Residual standard error: 0.2 on 1377 degrees of freedom
# Multiple R-squared:  0.6766,	Adjusted R-squared:  0.6764 
# F-statistic:  2881 on 1 and 1377 DF,  p-value: < 2.2e-16
#################################################################################

#Pretty curves?------------------------------------
plot.age <- rep(1:250)
l.age <-log(plot.age)
topredMM <- as.data.frame(cbind(plot.age,l.age))
names(topredMM) = c("age1","l.age")

# for WSM only
stratum <- rep("WSG",250)
wsgplot <- sample(data1$PLOT_ID,5)
PLOT_ID <- sort(rep(wsgplot,50))
topredWSM <- cbind(PLOT_ID,stratum,topredMM)

stratum <- sort(rep(as.character(levels(data1$stratum)),250))
plot10 <- sample(data1$PLOT_ID,10)
PLOT_ID <- sort(rep(plot10,250))
topredMM <- cbind(PLOT_ID,stratum,topredMM)

lyhat <- predict(mem4,newdata=topredMM)
predMM1 <- cbind(topredMM,lyhat)
predMM  <- mutate(predMM1 ,yhat = exp(lyhat)) 
MEM_biom <- ggplot(data=predMM,aes(x=age1,y=yhat,group=stratum,colour=stratum)) + geom_line(size=1.5)
MEM_biom + ggtitle("Predicting delta biomass t/ha by Strata") +  scale_fill_brewer(palette="Spectral")

#################################################
# something funny with WSM------------------------------
WSG <- filter(data1,stratum=='WSG')
hist(WSG$biom.ha.inc)
memWSM <- lmer(formula= ly~l.age+age1+(1|PLOT_ID),data=WSM,REML=FALSE)
glmWSM <- glm(formula= ly~l.age+age1,data=WSM)
AIC(memWSM)#[1] 28.08787
AIC(glmWSM)#[1] 26.99973
memWSM1 <- lmer(formula= ly~l.age+age1+(1|PLOT_ID),data=WSM)
WSMlhat1 <-fitted(memWSM)
WSMhat1 <- exp(WSMlhat1)
WSMlhat2 <- fitted(glmWSM)
WSMhat2 <- exp(WSMlhat2)
plot(WSM$biom.ha.inc,WSMhat1)
plot(WSM$biom.ha.inc,WSMhat2)
plot(WSMhat2,WSMhat1)
WSM.hat <- predict(glmWSM,newdata=topredWSM)
WSW.pred <- cbind(topredWSM,WSM.hat)

#plotWSM <- filter(predMM,stratum=="WSM")--------------------------
WSM.plot <- ggplot(data=WSW.pred,aes(x=age1,y=WSM.hat)) + geom_line(size=1.5)
WSM.plot + ggtitle("Predicting delta biomass t/ha WSM") +  scale_fill_brewer(palette="Spectral")

# fit everything with only one WS strata-----------------------
# grouped WSM and WSG
data3 <- data1
data3$stratum[which(data3$stratum=="WSM")] <- "WSG"
memNoWSM <- lmer(formula= ly~stratum+l.age*stratum+age1+(1|PLOT_ID),data=data3,REML=FALSE)
AIC(memNoWSM)#[1] 1805.379
plot.age <- rep(1:250)
l.age <-log(plot.age)
topredMM <- as.data.frame(cbind(plot.age,l.age))
names(topredMM) = c("age1","l.age")
stratum <- sort(rep(c("BF","BP","BSG","BSM","JP","TAG","TAM","WB","WSG"),250))
plot10 <- sample(data1$PLOT_ID,9)
PLOT_ID <- sort(rep(plot10,250))
topredMM <- cbind(PLOT_ID,stratum,topredMM)
NoWSMlhat <- predict(memNoWSM,newdata=topredMM)
NoWSMhat <- exp(NoWSMlhat)
NoWSMpred <- cbind(topredMM,NoWSMhat)
NoWSMbiom <- ggplot(data=NoWSMpred,aes(x=age1,y=NoWSMhat,group=stratum,colour=stratum)) + geom_line(size=1.5)
NoWSMbiom + ggtitle("Predicting delta biomass t/ha by Strata one WS group") +  scale_fill_brewer(palette="Spectral")


########### CONCLUSION#################################################
# One strata for white spruce
# Mixed effects model with varying intercept and slope for l.age (all fixed),
# but one param for age - random effects on the plots
########################################################################

# See if the fit is better is I get rid of the WSM
data4 <- filter(data1, stratum!="WSM")
mem7 <- lmer(formula= ly~stratum+l.age*stratum+age1+(1|PLOT_ID),data=data4,REML=FALSE)
AIC(mem7)#[1] 1778.642
# look at the curves
plot.age <- rep(1:250)
l.age <-log(plot.age)
topredMM <- as.data.frame(cbind(plot.age,l.age))
names(topredMM) = c("age1","l.age")
stratum <- sort(rep(c("BF","BP","BSG","BSM","JP","TAG","TAM","WB","WSG"),250))
plot10 <- sample(data1$PLOT_ID,9)
PLOT_ID <- sort(rep(plot10,250))
topredMM <- cbind(PLOT_ID,stratum,topredMM)
NoWSMlhat2 <- predict(mem7,newdata=topredMM)
NoWSMhat2 <- exp(NoWSMlhat2)
NoWSMpred2 <- cbind(topredMM,NoWSMhat2)
NoWSMbiom2 <- ggplot(data=NoWSMpred2,aes(x=age1,y=NoWSMhat,group=stratum,colour=stratum)) + geom_line(size=1.5)
NoWSMbiom2 + ggtitle("Predicting delta biomass t/ha by Strata removed SWM") +  scale_fill_brewer(palette="Spectral")
# does not seem to change the curves, does improve the AIC though...
# going with the "no SWM" version
ggsave("FINALBiomassYieldCurves_oneStrataforSpruce.jpeg")

#Final model:# One strata for white spruce (removed the WSM)
# Mixed effects model with random effect on intercept for plots, and 
# strata values for intercept and slope for l.age (all fixed),
# but one fixed param for age 
########################################################################
Finalyhat <- fitted(mem7)
plot(data4$biom.ha.inc,Finalyhat)
r.obs.pred <-lm(formula=Finalyhat~data4$biom.ha.inc)
summary(r.obs.pred)
# Call:
#   lm(formula = Finalyhat ~ data3$biom.ha.inc)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.32326 -0.14304  0.01762  0.16781  0.55181 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.408182   0.014181   28.78   <2e-16 ***
#   data3$biom.ha.inc 0.182567   0.004328   42.19   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2322 on 1377 degrees of freedom
# Multiple R-squared:  0.5638,	Adjusted R-squared:  0.5635 
# F-statistic:  1780 on 1 and 1377 DF,  p-value: < 2.2e-16

# save model
save(mem7,file="G:/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/MEM_t_ha/MEM_t_ha.RData")

# save fitting data
write.table(data4,file="FittingData_BiomassPSPModel.txt",sep=",",row.names = FALSE)

# extract parameters-------------------------------------------------------
params.matrix <- as.data.frame(summary(mem7)$coefficients)
b0 <- params.matrix[1:9,1:2]
b1 <- params.matrix[c(10,12:19),1:2]
b2 <- params.matrix[11,1:2]
strata.n <- group_by(data4,stratum) %>%
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

write.table(param.CI,file="BiomModelParamsCI.txt",sep=",",row.names=FALSE)

ci.plot <- ggplot(data=param.CI, aes(y=value,x=stratum, group=b, colour=b)) + 
  geom_errorbar(aes(ymin=lower.b,ymax=upper.b), width=.2) + geom_point()
# ggsave(file="BiomassMEMparamsCI.jpeg",plot=ci.plot)
