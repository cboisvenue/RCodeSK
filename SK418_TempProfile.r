#----------------------------------
# SK_Recliner project
# Determining temporal profile of 418 PSPs for the development of
# a growth raster
#
# Oct.8, 2014
# CBoisvenue
#----------------------------------


#install.packages("plyr")
require(plyr)
#install.packages("ggplot2")
require(ggplot2)
#install.packages("dplyr")
require(dplyr)
#install.packages("reshape2")
require(reshape2)
#  install.packages("gridExtra")
library(gridExtra)  

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data")

sk.tree <- read.table("SK_418PlotsCleaned1.txt",sep=",")



# graph the number of trees per age ----------------
# round ages
skTree_byAge <- select(sk.tree,PLOT_ID,TREE_NO,SPECIES,agecombo) %>%
  mutate(age=round(agecombo)) %>%
  select(PLOT_ID,TREE_NO,SPECIES,age)

qplot(age,data=skTree_byAge, geom="histogram",ylab="# trees",fill=SPECIES)
# same plot with ggplot command with lines around the bars and a titlte
ggplot(data=skTree_byAge) + geom_bar(aes(age, fill=SPECIES),colour="black") +
  xlab("Age") + ylab("Number of trees measured")+
  ggtitle("Measured Trees by Age and Species - SK 418")

#count the number of measurement per species
meas_sps <- group_by(skTree_byAge,SPECIES) %>%
  summarize(n())
# better plot with a table
g <- ggplot(data=skTree_byAge) + geom_bar(aes(age, fill=SPECIES),colour="black") +
  xlab("Plot age") + ylab("Number tree-level measurements") 
  #ggtitle("Measured Trees by Age and Species - SK 418") + 
  #theme(plot.title = element_text(lineheight=1.2, face="bold"))
  g+annotation_custom(tableGrob(meas_sps),xmin=175, xmax=225,ymin=7500,ymax=14000) 
#ggsave("SK418_NoTreesAgeSps.pdf")
#ggsave("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/SK418_TreeMeasAgeSps.pdf")
  ggsave("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/WritingBin/figures/SK418_TreeMeasAgeSps.jpeg")

# graph of the time horizon per plot
# NOTE that this will give me plot-level temporal profile, not tree level...
# We know that some tree ages age not in line with general plot ages
# so 1st calculate a plot level avg age
# -------- Average age for the plot--------------
sk.plt.age = select(sk.tree,PLOT_ID,YEAR,agecombo) %>%
  group_by(PLOT_ID,YEAR) %>%
  summarise(mean(agecombo,na.rm=TRUE))
names(sk.plt.age) = c("PLOT_ID","YEAR","agebar")

# how many NAs?
length(which(is.na(sk.plt.age[,3]))) #13
# do these 13 plots have other year?
ind1 = which(is.na(sk.plt.age[,3]))
check1 = sk.plt.age[ind1,]
check1 = filter(sk.plt.age,PLOT_ID %in% check1$PLOT_ID) %>%
  group_by(PLOT_ID) %>%
  summarise(n())
# all greater then 1

# to avoid problems due to stand-age versus tree age, I choose the last of the years to 
# calculate  the NA

# create a column with the last age and and one with the last year per plot
wh1 = group_by(sk.plt.age,PLOT_ID) %>%
  summarise_each(funs(ul = last(.))) 
names(wh1) = c("PLOT_ID","year1","agelast")
plt.age <- left_join(sk.plt.age,wh1) %>%
  mutate(fill.age =YEAR-year1+agelast)
ind2=which(is.na(plt.age$agebar))
plt.age$agebar[ind2]= plt.age$fill.age[ind2]
plt.age <- mutate(plt.age,plot.age = round(agebar)) %>%
  select(PLOT_ID,YEAR,plot.age) %>%
  arrange(PLOT_ID,YEAR)

# ------ Graph plot-level avg age-------------


h <- ggplot(data=plt.age,aes(x=YEAR,y=plot.age,group=PLOT_ID,)) +
  geom_line()+geom_point()

h+  ggtitle("Plots over time") + theme(plot.title=element_text(face="bold"))
#ggsave("SK418_AvgPlotAge_OverTime.pdf")

#----- how many plots are not been remeasured?--------
plt.meas <- select(plt.age,PLOT_ID,YEAR) %>%
  group_by(PLOT_ID) %>%
  summarise(n())
length(which(plt.meas[,2]<2))
##...hum...many of these plots are measured only once
ind3 = which(plt.meas[,2]<2)
graph1 <- filter(plt.age,PLOT_ID %in% plt.meas$PLOT_ID[-ind3])
ggplot(data=graph1,aes(x=YEAR,y=plot.age,group=PLOT_ID,)) +
  geom_line()+geom_point()+
  ggtitle("Remeasured plots over time") + theme(plot.title=element_text(face="bold"))

#---------Plot Tree-Level Temporal Profile----------
tree <- select(sk.tree, PLOT_ID, TREE_NO,YEAR, SPECIES,agecombo) %>%
  mutate(age=round(agecombo)) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,age) 
trial = mutate(tree,id.tree = paste(PLOT_ID, TREE_NO, SPECIES, sep = '')) %>%
  select(id.tree,YEAR,age) %>%
  arrange(id.tree,YEAR)
t <- ggplot(data=trial,aes(x=YEAR,y=age,group=id.tree)) +geom_line()
ggsave("SK418_TreeAge_OverTime.pdf")
