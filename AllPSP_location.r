#---------------------------------------------------------------
# Extract the lat and long for all PSP for Brenton
#
# CBoisvenue January 19, 2016
#--------------------------------------------------------------

library(data.table)
library(plyr)
library(dplyr)

sk.raw <- fread("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04/plot_header.csv", header=TRUE, sep=",")

loc.raw <- select(sk.raw, LOC_ACCURACY, PLOT_ID,contains("Z13nad83"))

psp <- fread("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/SK_2000PlotsCleaned.txt",sep=",", header=TRUE)
psp.no <- unique(psp$PLOT_ID)

head(psp.no)

psp.loc <- loc.raw[PLOT_ID %in% psp.no]
write.table(psp.loc,"M:/Spatially_explicit/01_Projects/07_SK_30m/Working/CBoisvenue/CleanedUpForUsing/psp1381_loc.txt",sep=",")

# read-in tree ring stand reconstruction location
stand.rec <- fread("G:/RES_Work/Work/BrentonChookolingo/treeRing/Tree-Ring Coordinates.csv",sep=",", header=TRUE)
dim(stand.rec)
#[1] 981   4
length(unique(stand.rec$lat))
#[1] 980
length(unique(stand.rec$lon))
#[1] 979
dim(unique(stand.rec[,no:=NULL]))
# one of these is a repeat...

