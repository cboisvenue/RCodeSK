#----------------------------------------------
# RSgrowth project
# We are at the point where we are trying to fit mixed effects models on the stack on 
# rasters of log(delta_biomass) as a function of log(age) and age to compare the parameters 
# to the parameters estimated (by strata) from the psps
#
# KO created a bunch of files here:
# H:\saskatchewan\spatialGrowth\fmapa\kangakola\Output_DF
# with the code here:
# M:\Spatially_explicit\01_Projects\07_SK_30m\Working\RScripts\RS_df_tables_kangakola_20150725.R
#
# the job in this script is to see if I can make one large data.table from all the data.tables 
# Kangakola created
#
# CBoisvenue
# July 30, 2015
#-----------------------------------------------

# define input dir
indir <- "H:/saskatchewan/spatialGrowth/fmapa/kangakola/Output_DF/"

# read table
# taking the biggest one...to see
library(data.table)
dt_0910 <- fread(paste(indir,"DT_09_10.txt",sep=""),sep=",",header = TRUE)
# verbose: Read 5926622 rows and 9 (of 9) columns from 0.299 GB file in 00:00:14
dt_0102 <- fread(paste(indir,"DT_01_02.txt",sep=""),sep=",",header=TRUE)
# Read 5464019 rows and 9 (of 9) columns from 0.270 GB file in 00:00:13

# are there any strata values that are not equal in the same data.table?
length(which(dt_0910$strata09!=dt_0910$strata10))
length(which(dt_0102$strata01 !=dt_0102$strata02))
# no so can drop or ignore one
# dt_0102 <- dt_0102[-strata02] ## DID NOT WORK...and I jsut figured out is redundant...
# this gives me the count by strata
trial1 <- dt_0102[,.N,by=strata01]
# this gets rid one of the columns called strata 
dt_0102[,strata02:=NULL]
# name the otehr one strata
setnames(dt_0102,"strata01","strata")

# same for the other data.table
dt_0910[,strata09:=NULL]
setnames(dt_0910,"strata10","strata")

# set keys
setkey(dt_0102,RasterID1,strata)
setkey(dt_0910,RasterID1,strata)

#try merge
merge0910_0102 <-merge(dt_0102,dt_0910)
# 2003948 obs
rm(merge0910_0102)
# is this different? I cannot tell if I have an inner join
library(plyr)
merge0910_0102 <- join(dt_0102,dt_0910, type="inner")
# 2003948 obs
# looks like it is an inner join! lightening fast...

# How many more can I add?
rm(dt_0102,dt_0910)
# get all the names in that folder
### STOPPED HERE: Byon has succeded in building a stack with all NA removed...
# 3333333333333333no need to mess with this anymore 
