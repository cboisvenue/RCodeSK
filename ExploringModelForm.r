#-----------------------------------------
# exploring the model form:
# y=b0*(age^b1)*exp(b2*age)
#
# CBoisvenue
# July 14th, 2015
#-----------------------------------------


require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(reshape2)

# curve with no data
b0=exp(0.9)
b1=0.85
b2=-0.014471
age <- 1:250
y <- b0*(age^b1)*exp(b2*age)
plot(age,y)
##NOT WORKING

y.f <- function(x){exp(0.9)*(x^0.85)*exp(0.014471*x)}

ggplot(data.frame(x=c(1,250)),aes(x)) +
  stat_function(fun=y.f,geom="line")

ggplot(data.frame(x=c(1,250)),aes(x)) +
     stat_function(fun=function(x)2.45960*(x^0.85)*exp(0.014471*x),geom="line")


# Read-in data----------------------------------------------------------
setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/")

biom.data <- read.table("t_haBiom_yr.txt",sep=",",header=TRUE)
biom.data <- select(biom.data,PLOT_ID,age1,biom.ha.inc,stratum)

# this is what the fitting data looks like -----------------------------
head(biom.data)

range(biom.data$age1) #23.04065 168.60951
# this is in t/ha
range(biom.data$biom.ha.inc)#0.1858394 13.1480263
data1 <- mutate(biom.data,ly = log(biom.ha.inc), l.age=log(age1))
data2 <- data1
data2$stratum[which(data2$stratum=="WSM")] <- "WSG"

# this loads the fitting MEM, it is names mem7
load("G:/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/Results/MEM_t_ha/MEM_t_ha.RData")

# fake data
b0=exp(0.9)
b1=0.85
b2=-0.014471
age <- 1:250
y <- b0*(age^b1)*exp(b2*age)
plot(age,y)






# real parameters-------------------
params.matrix <- as.data.frame(summary(mem7)$coefficients)
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
params <- select(param.CI,stratum, n,b,value)
params <- dcast(params,stratum+n~b)
b2 <- filter(params,!is.na(b2))[,5]
params$b2[is.na(params$b2)] <- b2
params <- filter(params,stratum!="ALL") 
stratum <- sort(rep(as.character(params$stratum),250))
age <- rep(1:250,9)
Age <- as.data.frame(cbind(age,stratum))

fitted.graphs <- inner_join(params,Age)
fitted.graphs$age <- as.numeric(fitted.graphs$age)
fit.graphs <- mutate(fitted.graphs,y = exp(b0)*(age^b1)*exp(b2*age)) %>%
  arrange(stratum,age) %>%
  select(stratum,age,y)

# graphs ---------------------------
pred.biom.graph <- ggplot(data = fit.graphs, aes(x=age,y=y,group=stratum,colour=stratum)) + geom_line()



