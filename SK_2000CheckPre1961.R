#---------------------------------------------------------------------------------------------
# Checking SK PSP prior to 1961 for weird dbh distribution
# they were apparently measured in dbh classes of 1 inch
# SK plot description:
# In the early years, trees were tallied in 1-inch DBH classes. Starting with the 1958 establishment of
# the jack pine (#3) survey, tree numbers were assigned and individual tree measurements recorded.
# The final DBH class tally measurement for the trembling aspen (#7), mixedwood (#20), and white
# spruce (#1) surveys was in 1959, and for the black spruce survey in 1961. All subsequent
# measurements have tracked individual trees. In 1979, after adoption the metric system, field
# measurements were collected in metric units, and the previously-collected data was converted from
# Imperial units to metric.
# From 1949 to 1961, trees in the BS, and WS/TA/MW surveys were tallied in 1" diameter classes,
# with most (94%) of the 1431 plot/measurements tallied with a breakpoint DBH in the 1" class
# (0.1" minimum). Card reviews show that the data has been truncated to the 4" class for some
# plots during data entry. As such, tree counts between 0.1cm and 9.2cm are not completely
# representative of the sample; no further truncation to align with the breakpoint DBH in the
# individual tree era has been conducted.
# A set of biometrically created "pseudotrees" were generated from tallies to facilitate incorporation
# of this early data into an individual tree database. As a truncated set of tally data was present in
# PSP_WEY (174 of 1,431 plot measurements), the source files listed in Table 2 were consolidated
# to facilitate pseudotree creation. Within each plot, species, and diameter class combination, a
# "tree" was created for each counted tree and assigned an initial DBH of the DBH class "floor" 
# (for example, the 4" class would have a floor of 3.45"). This initial DBH was then increased by a
# randomly assigned value between 0.0 and 0.9 inches to reflect a 1/10th inch measurement
# precision, and converted from inches to centimetres. The "tree" was then assigned a height using
# the same plot-specific height-diameter model used for individual trees (Section 5), again corrected
# to the sample trees recorded at the time of measurement. This process resulted in 352,416
# "trees" from 29,551 records, with no change in the resultant density or average diameter 
# (9.015 in tallies versus 9.028 in pseudotrees). TREE_STATUS was set to "1" for all live pseudotrees and
# to "3" for all dead pseudotrees, but no additional descriptive attributes are available.
#
# and this plot below is to check if I can see anything in plots prior to 1961

require(plyr)
require(ggplot2)
require(dplyr)

setwd("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/")

# read-in tree-level data ------------------------------------------------
trees <- read.table("SK_2000TreeMeasurements.csv",header=TRUE,sep=",")

plotsPre58 <- filter(trees, YEAR<1957)
no.pre58 <- unique(plotsPre58$PLOT_ID)
length(no.pre58) #398

plots58to61 <- filter(trees,YEAR<1962 & YEAR>1957) 
no.58to61 <- unique(plots58to61$PLOT_ID)
length(no.58to61) #[1] 301

plotsPre61 <-  filter(trees,YEAR<1962)
no.pre61 <- unique(plotsPre61$PLOT_ID) # 471

plot(plotsPre61$age,plotsPre61$DBH)

#-------------Plot/year age distribution-----------------

plot.age <- group_by(trees, PLOT_ID, YEAR) %>%
  summarise(avgAge = mean(age))

ggplot(data=plot.age) + geom_bar(aes(x=avgAge))

# plot survey number
survey <- read.table("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04/plot_header.csv", header=TRUE, sep=",")
survey <- select(survey,PLOT_ID,SURVEY) 
survey <- (unique(survey))
table(survey$SURVEY)
# 1   2   3   7  20  21  22  32  52  91 
# 193 318 301 405 629  25  27  25  25 100

age.survey <- inner_join(plot.age, survey)
no.age.survey <-ggplot(data=age.survey) + geom_bar(aes(avgAge,fill=as.factor(SURVEY), colour="black"))
ggsave(file="G:/RES_Work/Work/JoanneWhite/SK_work/data/PSP/NoPlots_AgeSurvey.jpeg",plot=no.age.survey)

# Checking ecosite classification
meas_header <- read.table("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04/measurement_header.csv", header=TRUE, sep=",")
ggplot(data=meas_header) + geom_bar(aes(FIELD_ECOSITE), colour="black")
eco.sk <- as.data.frame(table(meas_header$FIELD_ECOSITE))
names(eco.sk) = c("class","count")
write.table(x=eco.sk,file="G:/RES_Work/Work/JoanneWhite/SK_work/data/PSP/SK_EcoClassification.csv",sep=",",row.names=FALSE)

# checking DBH
dbh.count <- ggplot(data=trees) + geom_bar(aes(DBH,fill=SPECIES))
