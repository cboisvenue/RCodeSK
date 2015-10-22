# Create a table for time series 1987:2012 of age, log(age), log(delta biomass), strata and Raster cellID

# This script:

# 1) Separates and creates yearly raster stacks in order to group the raster variables by:

#           1) age(1984), 2) log(age(1984)), 3) log(smooth_delta_biomass_1984-1985), 4) strata

# where each raster cell 5) (RasterID) constitutes a record in each given year

# 2) Individual raster stacks are coverted to data.frames then into data.tables which are VERY memory
# efficient and fast to process

# 3) For each data.table, the variable RasterID is created which is assigned the raster cell ID and 
# is set as a data.table key for efficient indexing. NA records within a given year removed
# if any still exists after NA syncing and the strata column is renamed to be associated to a 
# given year (may not need this anymore)

# 4) After individual year data.tables are created they are combined using 'rbindlist' into a single
# data.table and written to file. NOTE that within each step temp files are removed ("rm") in order to 
# avoid unnecessary files being preserved in memory

#Author: Byron Smiley & Kangakola Omendja
#Date: June 14 2015
#Modified: July 30 2015

# With data.table package and removing unneeded/temp data (lists, stacks, tables) as you go this 
#script didn't use any more than 35% of A105350's memory (~23GB), ranging between 20% and 33% of 
# memory capacity

#Required Inputs:

# NOTE: ALL INPUTS HAVE HAD THEIR NA VALUES SYNCED ACROSS ALL YEARSIN ORDER TO REMOVE THOSE
# NAS ACROSS ALL YEARS. THIS REDUCES THE NUMBER OF RECORDS IN EACH YEAR THAT WOULD NEED TO BE
# PROCESSED BY THIS SCRIPT BUT WHICH ARE REDUNDANT FOR THE FINAL OUTPUT. THE SYNCING OF NAS
# OCCURS IN "RS_growth_rate_rasters_smooth.R" WHERE ALL INPUTS FOR THIS SCRIPT ARE DERIVED.

# 1) age rasters (1984-2012)
# 2) logged age rasters (1984-2012)
# 3) logged smoothed delta biomass rasters (1985-2012)
# 4) strata raster

#Packages--------------------------------------------------------------------------------------------
library(data.table)
library(raster)
library(parallel)
library(snow)
library(dplyr)
rasterOptions(tmpdir="H:/saskatchewan/data/temp")
#Clear workspace --------------------------------------------------
rm(list=ls())

#-------------------------------------------------
#Identify input and output directories and constant variables
indir <- "H:/saskatchewan/spatialGrowth/fmapa/RSGrowthSmoothSyncNA"
years <- 1985:2012
outdir <- "H:/saskatchewan/spatialGrowth/fmapa/RSGrowthSmoothSyncNA/tables"

# List and Stack-----------------------------------------------------------------------
#make list of all rasters to be included in dataframe
allfiles <- list.files(paste(indir), full.names=TRUE, pattern="*tif") # make list of file names

#Set directory to where rasters are
setwd(indir)

#list files for only 1 year of data... need all 28 (1985-2012) so we can create stacks then
#extract dataframes from those stacks
 startTime <- Sys.time()
# each stack (or record) must be the strata layer (same one for each stack), age rasters 
#(age and log(age)) from one year and the delta biomass from the following year
year_file1 <-grep("*/1984|*_1985|strata", allfiles, value=TRUE)
year_file2 <-grep("*/1985|*_1986|strata", allfiles, value=TRUE)
year_file3 <-grep("*/1986|*_1987|strata", allfiles, value=TRUE)
year_file4 <-grep("*/1987|*_1988|strata", allfiles, value=TRUE)
year_file5 <- grep("*/1988|*_1989|strata", allfiles, value=TRUE)
year_file6 <- grep("*/1989|*_1990|strata", allfiles, value=TRUE)
year_file7 <- grep("*/1990|*_1991|strata", allfiles, value=TRUE)
year_file8 <- grep("*/1991|*_1992|strata", allfiles, value=TRUE)
year_file9 <- grep("*/1992|*_1993|strata", allfiles, value=TRUE)
year_file10 <- grep("*/1993|*_1994|strata", allfiles, value=TRUE)
year_file11 <- grep("*/1994|*_1995|strata", allfiles, value=TRUE)
year_file12 <- grep("*/1995|*_1996|strata", allfiles, value=TRUE)
year_file13 <- grep("*/1996|*_1997|strata", allfiles, value=TRUE)
year_file14 <- grep("*/1997|*_1998|strata", allfiles, value=TRUE)
year_file15 <- grep("*/1998|*_1999|strata", allfiles, value=TRUE)
year_file16 <- grep("*/1999|*_2000|strata", allfiles, value=TRUE)
year_file17 <- grep("*/2000|*_2001|strata", allfiles, value=TRUE)
year_file18 <- grep("*/2001|*_2002|strata", allfiles, value=TRUE)
year_file19 <- grep("*/2002|*_2003|strata", allfiles, value=TRUE)
year_file20 <- grep("*/2003|*_2004|strata", allfiles, value=TRUE)
year_file21 <- grep("*/2004|*_2005|strata", allfiles, value=TRUE)
year_file22 <- grep("*/2005|*_2006|strata", allfiles, value=TRUE)
year_file23 <- grep("*/2006|*_2007|strata", allfiles, value=TRUE)
year_file24 <- grep("*/2007|*_2008|strata", allfiles, value=TRUE)
year_file25 <- grep("*/2008|*_2009|strata", allfiles, value=TRUE)
year_file26 <- grep("*/2009|*_2010|strata", allfiles, value=TRUE)
year_file27 <- grep("*/2010|*_2011|strata", allfiles, value=TRUE)
year_file28 <- grep("*/2011|*_2012|strata", allfiles, value=TRUE)

 # Make individual stacks each year group
 stack1 <- stack(year_file1)
 stack2 <- stack(year_file2)
 stack3 <- stack(year_file3)
 stack4 <- stack(year_file4)
 stack5 <- stack(year_file5)
 stack6 <- stack(year_file6)
 stack7 <- stack(year_file7)
 stack8 <- stack(year_file8)
 stack9 <- stack(year_file9)
 stack10 <- stack(year_file10)
 stack11 <- stack(year_file11)
 stack12 <- stack(year_file12)
 stack13 <- stack(year_file13)
 stack14 <- stack(year_file14)
 stack15 <- stack(year_file15)
 stack16 <- stack(year_file16)
 stack17 <- stack(year_file17)
 stack18 <- stack(year_file18)
 stack19 <- stack(year_file19)
 stack20 <- stack(year_file20)
 stack21 <- stack(year_file21)
 stack22 <- stack(year_file22)
 stack23 <- stack(year_file23)
 stack24 <- stack(year_file24)
 stack25 <- stack(year_file25)
 stack26 <- stack(year_file26)
 stack27 <- stack(year_file27)
 stack28 <- stack(year_file28)
 junk_startTime <- Sys.time()
# End of list and stack-----------------------------------------------------------------------
 
 # clean some  unused r object in the workspace
 
 rm("year_file1","year_file2","year_file3","year_file4","year_file5","year_file6","year_file7",
    "year_file8","year_file9","year_file10","year_file11","year_file12","year_file13","year_file14",
    "year_file15","year_file16","year_file17","year_file18","year_file19","year_file20",
    "year_file21","year_file22","year_file23","year_file24","year_file25","year_file26",
    "year_file27","year_file28")
#------------------------------------------------------------------------------------------------
 #Convert yearly stacked data (1985-2012) to data.frames then data.tables for efficient 
 #memory processing and remove NAs and add rasterID column
 
 startTime <- Sys.time()
#----1985----------------
 RS_df1985 <- as.data.frame(stack1) # read the stack as data frame
 DT_df1985 <- data.table(RS_df1985)
 DT_df1985[, RasterID1 := as.numeric(rownames(DT_df1985))]
 #DT_df1985_a <- DT_df1985[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1985,RasterID1) # set a key for indexing
 DT_df1985_b <- DT_df1985[is.finite(rowSums(DT_df1985))] # remove NAs from the data table
 setnames(DT_df1985_b,"strata","strata85")
 rm(RS_df1985,DT_df1985)
 
  #---1986-----------------------
 RS_df1986 <- as.data.frame(stack2) # read the stack as data frame
 DT_df1986 <- data.table(RS_df1986)
 DT_df1986[, RasterID1 := as.numeric(rownames(DT_df1986))]
 #DT_df1986_a <- DT_df1986[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1986,RasterID1) # set a key for indexing
 DT_df1986_b <- DT_df1986[is.finite(rowSums(DT_df1986))] # remove NAs from the data table
 setnames(DT_df1986_b,"strata","strata86")
 rm(RS_df1986,DT_df1986)
#######################################################

 #---1987-----------------------
 RS_df1987 <- as.data.frame(stack3) # read the stack as data frame
 DT_df1987 <- data.table(RS_df1987)
 DT_df1987[, RasterID1 := as.numeric(rownames(DT_df1987))]
 #DT_df1987_a <- DT_df1987[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1987,RasterID1) # set a key for indexing
 DT_df1987_b <- DT_df1987[is.finite(rowSums(DT_df1987))] # remove NAs from the data table
setnames(DT_df1987_b,"strata","strata87")
 rm(RS_df1987,DT_df1987)
 
  #---1988-----------------------
 RS_df1988 <- as.data.frame(stack4)
 DT_df1988 <- data.table(RS_df1988)
 DT_df1988[, RasterID1 := as.numeric(rownames(DT_df1988))]
 setkey(DT_df1988,RasterID1) # set a key for indexing
 DT_df1988_b <- DT_df1988[is.finite(rowSums(DT_df1988))] # remove NAs from the data table
 setnames(DT_df1988_b,"strata","strata88")
 rm(RS_df1988,DT_df1988)
#######################################################

 #---1989-----------------------
 RS_df1989 <- as.data.frame(stack5)
 DT_df1989 <- data.table(RS_df1989)
 DT_df1989[, RasterID1 := as.numeric(rownames(DT_df1989))]
 #DT_df1989_a <- DT_df1989[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1989,RasterID1) # set a key for indexing
 DT_df1989_b <- DT_df1989[is.finite(rowSums(DT_df1989))] # remove NAs from the data table
 setnames(DT_df1989_b,"strata","strata89")
 rm(RS_df1989,DT_df1989)
 #---1990-----------------------
 RS_df1990 <- as.data.frame(stack6)
 DT_df1990 <- data.table(RS_df1990)
 DT_df1990[, RasterID1 := as.numeric(rownames(DT_df1990))]
 #DT_df1990_a <- DT_df1990[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1990,RasterID1) # set a key for indexing
 DT_df1990_b <- DT_df1990[is.finite(rowSums(DT_df1990))] # remove NAs from the data table
 setnames(DT_df1990_b,"strata","strata90")
 rm(RS_df1990,DT_df1990)
#######################################################

 #---1991-----------------------
 RS_df1991 <- as.data.frame(stack7)
 DT_df1991 <- data.table(RS_df1991)
 DT_df1991[, RasterID1 := as.numeric(rownames(DT_df1991))]
 #DT_df1991_a <- DT_df1991[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1991,RasterID1) # set a key for indexing
 DT_df1991_b <- DT_df1991[is.finite(rowSums(DT_df1991))] # remove NAs from the data table
 setnames(DT_df1991_b,"strata","strata91")
 rm(RS_df1991,DT_df1991)
 #---1992-----------------------
 RS_df1992 <- as.data.frame(stack8)
 DT_df1992 <- data.table(RS_df1992)
 DT_df1992[, RasterID1 := as.numeric(rownames(DT_df1992))]
 #DT_df1992_a <- DT_df1992[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1992,RasterID1) # set a key for indexing
 DT_df1992_b <- DT_df1992[is.finite(rowSums(DT_df1992))] # remove NAs from the data table
 setnames(DT_df1992_b,"strata","strata92")
 rm(RS_df1992,DT_df1992)
#######################################################

#---1993-----------------------
 RS_df1993 <- as.data.frame(stack9)
 DT_df1993 <- data.table(RS_df1993)
 DT_df1993[, RasterID1 := as.numeric(rownames(DT_df1993))]
 #DT_df1993_a <- DT_df1993[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1993,RasterID1) # set a key for indexing
 DT_df1993_b <- DT_df1993[is.finite(rowSums(DT_df1993))] # remove NAs from the data table
setnames(DT_df1993_b,"strata","strata93")
 rm(RS_df1993,DT_df1993)
#---1994-----------------------
 RS_df1994 <- as.data.frame(stack10)
 DT_df1994 <- data.table(RS_df1994)
 DT_df1994[, RasterID1 := as.numeric(rownames(DT_df1994))]
 #DT_df1994_a <- DT_df1994[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1994,RasterID1) # set a key for indexing
 DT_df1994_b <- DT_df1994[is.finite(rowSums(DT_df1994))] # remove NAs from the data table
 setnames(DT_df1994_b,"strata","strata94")
 rm(RS_df1994,DT_df1994)
#######################################################

 #---1995-----------------------
 RS_df1995 <- as.data.frame(stack11)
 DT_df1995 <- data.table(RS_df1995)
 DT_df1995[, RasterID1 := as.numeric(rownames(DT_df1995))]
 #DT_df1995_a <- DT_df1995[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1995,RasterID1) # set a key for indexing
 DT_df1995_b <- DT_df1995[is.finite(rowSums(DT_df1995))] # remove NAs from the data table
 setnames(DT_df1995_b,"strata","strata95")
 rm(RS_df1995,DT_df1995)

 #---1996-----------------------
 RS_df1996 <- as.data.frame(stack12)
 DT_df1996 <- data.table(RS_df1996)
 DT_df1996[, RasterID1 := as.numeric(rownames(DT_df1996))]
 #DT_df1996_a <- DT_df1996[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1996,RasterID1) # set a key for indexing
 DT_df1996_b <- DT_df1996[is.finite(rowSums(DT_df1996))] # remove NAs from the data table
 setnames(DT_df1996_b,"strata","strata96")
 rm(RS_df1996,DT_df1996)
#######################################################

 #---1997-----------------------
 RS_df1997 <- as.data.frame(stack13)
 DT_df1997 <- data.table(RS_df1997)
 DT_df1997[, RasterID1 := as.numeric(rownames(DT_df1997))]
 #DT_df1997_a <- DT_df1997[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1997,RasterID1) # set a key for indexing
 DT_df1997_b <- DT_df1997[is.finite(rowSums(DT_df1997))] # remove NAs from the data table
 setnames(DT_df1997_b,"strata","strata97")
 rm(RS_df1997,DT_df1997)
 #---1998-----------------------
 RS_df1998 <- as.data.frame(stack14)
 DT_df1998 <- data.table(RS_df1998)
 DT_df1998[, RasterID1 := as.numeric(rownames(DT_df1998))]
 #DT_df1998_a <- DT_df1998[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1998,RasterID1) # set a key for indexing
 DT_df1998_b <- DT_df1998[is.finite(rowSums(DT_df1998))] # remove NAs from the data table
 setnames(DT_df1998_b,"strata","strata98")
 rm(RS_df1998,DT_df1998)
#######################################################

#---1999-----------------------
 RS_df1999 <- as.data.frame(stack15)
 DT_df1999 <- data.table(RS_df1999)
 DT_df1999[, RasterID1 := as.numeric(rownames(DT_df1999))]
 #DT_df1999_a <- DT_df1999[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df1999,RasterID1) # set a key for indexing
 DT_df1999_b <- DT_df1999[is.finite(rowSums(DT_df1999))] # remove NAs from the data table
 setnames(DT_df1999_b,"strata","strata99")
 rm(RS_df1999,DT_df1999)
 #---2000-----------------------
 RS_df2000 <- as.data.frame(stack16)
 DT_df2000 <- data.table(RS_df2000)
 DT_df2000[, RasterID1 := as.numeric(rownames(DT_df2000))]
 #DT_df2000_a <- DT_df2000[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2000,RasterID1) # set a key for indexing
 DT_df2000_b <- DT_df2000[is.finite(rowSums(DT_df2000))] # remove NAs from the data table
 setnames(DT_df2000_b,"strata","strata00")
 rm(RS_df2000,DT_df2000)
#######################################################

 #---2001-----------------------
 RS_df2001 <- as.data.frame(stack17)
 DT_df2001 <- data.table(RS_df2001)
 DT_df2001[, RasterID1 := as.numeric(rownames(DT_df2001))]
 #DT_df2001_a <- DT_df2001[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2001,RasterID1) # set a key for indexing
 DT_df2001_b <- DT_df2001[is.finite(rowSums(DT_df2001))] # remove NAs from the data table
 setnames(DT_df2001_b,"strata","strata01")
 rm(RS_df2001,DT_df2001)
#---2002-----------------------
 RS_df2002 <- as.data.frame(stack18)
 DT_df2002 <- data.table(RS_df2002)
 DT_df2002[, RasterID1 := as.numeric(rownames(DT_df2002))]
 #DT_df2002_a <- DT_df2002[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2002,RasterID1) # set a key for indexing
 DT_df2002_b <- DT_df2002[is.finite(rowSums(DT_df2002))] # remove NAs from the data table
 setnames(DT_df2002_b,"strata","strata02")
 rm(RS_df2002,DT_df2002)
#######################################################

 #---2003-----------------------
 RS_df2003 <- as.data.frame(stack19)
 DT_df2003 <- data.table(RS_df2003)
 DT_df2003[, RasterID1 := as.numeric(rownames(DT_df2003))]
 #DT_df2003_a <- DT_df2003[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2003,RasterID1) # set a key for indexing
 DT_df2003_b <- DT_df2003[is.finite(rowSums(DT_df2003))] # remove NAs from the data table
 setnames(DT_df2003_b,"strata","strata03")
 rm(RS_df2003,DT_df2003)
 #---2004-----------------------
 RS_df2004 <- as.data.frame(stack20)
 DT_df2004 <- data.table(RS_df2004)
 DT_df2004[, RasterID1 := as.numeric(rownames(DT_df2004))]
 #DT_df2004_a <- DT_df2004[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2004,RasterID1) # set a key for indexing
 DT_df2004_b <- DT_df2004[is.finite(rowSums(DT_df2004))] # remove NAs from the data table
 setnames(DT_df2004_b,"strata","strata04")
 rm(RS_df2004,DT_df2004)
###########################

 #---2005-----------------------
 RS_df2005 <- as.data.frame(stack21)
 DT_df2005 <- data.table(RS_df2005)
 DT_df2005[, RasterID1 := as.numeric(rownames(DT_df2005))]
 #DT_df2005_a <- DT_df2005[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2005,RasterID1) # set a key for indexing
 DT_df2005_b <- DT_df2005[is.finite(rowSums(DT_df2005))] # remove NAs from the data table
 setnames(DT_df2005_b,"strata","strata05")
 rm(RS_df2005,DT_df2005)
 #---2006-----------------------
 RS_df2006 <- as.data.frame(stack22)
 DT_df2006 <- data.table(RS_df2006)
 DT_df2006[, RasterID1 := as.numeric(rownames(DT_df2006))]
 #DT_df2006_a <- DT_df2006[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2006,RasterID1) # set a key for indexing
 DT_df2006_b <- DT_df2006[is.finite(rowSums(DT_df2006))] # remove NAs from the data table
 setnames(DT_df2006_b,"strata","strata06")
 rm(RS_df2006,DT_df2006)
################################

 #---2007-----------------------
 RS_df2007 <- as.data.frame(stack23)
 DT_df2007 <- data.table(RS_df2007)
 DT_df2007[, RasterID1 := as.numeric(rownames(DT_df2007))]
 #DT_df2007_a <- DT_df2007[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2007,RasterID1) # set a key for indexing
 DT_df2007_b <- DT_df2007[is.finite(rowSums(DT_df2007))] # remove NAs from the data table
 setnames(DT_df2007_b,"strata","strata07")
 rm(RS_df2007,DT_df2007)
 #---2008-----------------------
 RS_df2008 <- as.data.frame(stack24)
 DT_df2008 <- data.table(RS_df2008)
 DT_df2008[, RasterID1 := as.numeric(rownames(DT_df2008))]
 #DT_df2008_a <- DT_df2008[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2008,RasterID1) # set a key for indexing
 DT_df2008_b <- DT_df2008[is.finite(rowSums(DT_df2008))] # remove NAs from the data table
 setnames(DT_df2008_b,"strata","strata08")
 rm(RS_df2008,DT_df2008)
####################################

#---2009-----------------------
 RS_df2009 <- as.data.frame(stack25)
 DT_df2009 <- data.table(RS_df2009)
 DT_df2009[, RasterID1 := as.numeric(rownames(DT_df2009))]
 #DT_df2009_a <- DT_df2009[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2009,RasterID1) # set a key for indexing
 DT_df2009_b <- DT_df2009[is.finite(rowSums(DT_df2009))] # remove NAs from the data table
 setnames(DT_df2009_b,"strata","strata09")
 rm(RS_df2009,DT_df2009)
 #---2010-----------------------
 RS_df2010 <- as.data.frame(stack26)
 DT_df2010 <- data.table(RS_df2010)
 DT_df2010[, RasterID1 := as.numeric(rownames(DT_df2010))]
 #DT_df2010_a <- DT_df2010[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2010,RasterID1) # set a key for indexing
 DT_df2010_b <- DT_df2010[is.finite(rowSums(DT_df2010))] # remove NAs from the data table
 setnames(DT_df2010_b,"strata","strata10")
 rm(RS_df2010,DT_df2010)
####################################

 #---2011-----------------------
 RS_df2011 <- as.data.frame(stack27)
 DT_df2011 <- data.table(RS_df2011)
 DT_df2011[, RasterID1 := as.numeric(rownames(DT_df2011))]
 #DT_df2011_a <- DT_df2011[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2011,RasterID1) # set a key for indexing
 DT_df2011_b <- DT_df2011[is.finite(rowSums(DT_df2011))] # remove NAs from the data table
 setnames(DT_df2011_b,"strata","strata11")
 rm(RS_df2011,DT_df2011)
#---2012-----------------------
 RS_df2012 <- as.data.frame(stack28)
 DT_df2012 <- data.table(RS_df2012)
 DT_df2012[, RasterID1 := as.numeric(rownames(DT_df2012))]
 #DT_df2012_a <- DT_df2012[,!"RasterID", with=FALSE] # remove column from data.table
 setkey(DT_df2012,RasterID1) # set a key for indexing
 DT_df2012_b <- DT_df2012[is.finite(rowSums(DT_df2012))] # remove NAs from the data table
 setnames(DT_df2012_b,"strata","strata12")
 rm(RS_df2012,DT_df2012)
 
 endTime <- Sys.time()
 elapsedTime_ALL <- endTime - startTime
 #2.27 hours for 1987-2012... probably 2.5hours for all years or so...
 
 # End of individual year data.table creation-----------------------------------------------------
 
 #############################################################################
 # Remove uneeded data
 
 rm(stack1,stack2,stack3,stack4,stack5,stack6,stack7,stack8,stack9,stack10,stack11,stack12,stack13,
    stack14,stack15,stack16,stack17,stack18,stack19,stack20,stack21,stack22,stack23,stack24,stack25,
    stack26,stack27,stack28)
##############################################################################
 
#MERGE data.tables and export--------------------------------------------------------------------
 
 # create a list of all data.table names
 
 allDT_list <- list(DT_df1985_b,DT_df1986_b,DT_df1987_b,DT_df1988_b,DT_df1989_b,DT_df1990_b,
                   DT_df1991_b,DT_df1992_b,DT_df1993_b,DT_df1994_b,DT_df1995_b,DT_df1996_b,
                   DT_df1997_b,DT_df1998_b,DT_df1999_b,DT_df2000_b,DT_df2001_b,DT_df2002_b,
                   DT_df2003_b,DT_df2004_b,DT_df2005_b,DT_df2006_b,DT_df2007_b,DT_df2008_b,
                   DT_df2009_b,DT_df2010_b,DT_df2011_b,DT_df2012_b)
 
 # Combine all individual year data tables into one
 DT_all <- rbindlist(allDT_list, use.names = FALSE, fill=FALSE)
 
 #Write combined data.table to file
 write.table(DT_all, paste(outdir,"DT_all.txt",sep="/"), sep=",",row.names=FALSE)
 
 #End of raster to table export-------------------------------------------------------------------
 