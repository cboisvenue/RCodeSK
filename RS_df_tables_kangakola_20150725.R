# First run at creating RS growth data frame for Celine



#Author: Byron Smiley
#Date: June 14 2015
#        Modification:
#        Kangakola Omendja: July 21 2015
#             1. Change the input and output directories sot taht
#             2. load the library data.table
#             3. write the code to convert data frame to data table
#             4. write the code to merge data table file
#             4. write the code to write data.table to text file
#             
#Computer:a 338
#-------------------------------------------------
library(data.table)
library(raster)
library(parallel)
library(snow)
library(dplyr)
rasterOptions(tmpdir="H:/saskatchewan/data/temp")
#--------------------------------------------------
# clean the workspace
rm(list=ls())

#-------------------------------------------------
#Identify input and output directories (KANGAKOLA - CHANGE THESE FOR YOUR DATA)
indir <- "H:/saskatchewan/spatialGrowth/fmapa/kangakola/data"
years <- 1985:2012
outdir <- "H:/saskatchewan/spatialGrowth/fmapa/kangakola/Output_dataFrame"

# List and Stack-----------------------------------------------------------------------
#make list of all rasters to be included in dataframe... eventually...
allfiles <- list.files(paste(indir), full.names=TRUE, pattern="*tif") # make list of file names

#Stack ALL rasters
setwd(indir)
#allStack <- stack(allfiles)

#list files for only 1 year of data... need all 28 (1985-2012) so we can create stacks then
#extract dataframes from those stacks

# each stack (or record) must be the strata layer (same one for each stack), age rasters 
#(age and log(age)) from one year and the delta biomass from the following year

#Stack ALL rasters
# setwd(indir)
# allStack <- stack(allfiles)
 
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

 # Make individual stacks for 1985 and 1986
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
 ######################################3
 # remove all the nas by synchronization
 # combine all stacks
 #allstacks <- stack(stack1,stack2,stack3,stack4,stack5,stack6,stack7,stack8,stack9,stack10,stack11,stack12,stack13,stack14,stack15,stack16, 
  #                  stack17,stack18,stack19,stack20,stack21,stack22,stack23,stack24,stack25,stack26,stack27,stack28)
 # syncrhonize nas
 #allstacks_NaSync <- mask(allstacks, (calc(allstacks,fun=sum)))
 #junk_endTime <- Sys.time()
 #junk_elapsed <- junk_endTime - junk_startTime
 
 # clean some  unused r object in the workspace
 rm("year_file1","year_file2","year_file3","year_file4","year_file5","year_file6","year_file7","year_file8","year_file9","year_file10","year_file11","year_file12") 
 rm("year_file13","year_file14","year_file15","year_file16","year_file17","year_file18","year_file19","year_file20","year_file21","year_file22","year_file23") 
 rm("year_file24","year_file25","year_file26","year_file27","year_file28") 
 #rm(as.character(year_file24,year_file25,year_file26,year_file27,year_file28)) 
 #Make dataframe and write to csv file--------------------------------------------------------
 
 #Make dataframe out of stack 2 (first year of data)
 #startTime <- Sys.time()
# create dataframe from raster stack of 1 year
#remove records within this year's worth of data that have cell NA values for any of the 4 rasters
#Give the raster ID column a name called 'RasterID'
# write to csv file so can be appended together later own
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
#Not all of the characters in H:/saskatchewan/spatialGrowth/fmapa/kangakola/RS_frame_kangakola_20150724.R could be encoded using ISO8859-1. 
# To save using a different encoding, choose "File | Save with Encoding..." from the main menu.
 setnames(DT_df1986_b,"strata","strata86")
 rm(RS_df1986,DT_df1986)
#######################################################
# merging 85 and 86 
DT_85_86 <- merge(DT_df1985_b,DT_df1986_b,by="RasterID1")
rm(DT_df1985_b,DT_df1986_b, stack1, stack2)
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
# merging 87 and 88 
DT_87_88 <- merge(DT_df1987_b,DT_df1988_b,by="RasterID1")
rm(DT_df1987_b,DT_df1988_b, stack3, stack4)
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
# merging 89 and 90
DT_89_90 <- merge(DT_df1989_b,DT_df1990_b,by="RasterID1")
rm(DT_df1989_b,DT_df1990_b, stack5, stack6)
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
# merging 91 and 92
DT_91_92 <- merge(DT_df1991_b,DT_df1992_b,by="RasterID1")
rm(DT_df1991_b,DT_df1992_b, stack7, stack8)
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
# merging 93 and 94
DT_93_94 <- merge(DT_df1993_b,DT_df1994_b,by="RasterID1")
rm(DT_df1993_b,DT_df1994_b, stack9, stack10)
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
# merging 95 and 96
 DT_95_96 <- merge(DT_df1995_b,DT_df1996_b,by="RasterID1")
 rm(DT_df1995_b,DT_df1996_b, stack11, stack12)
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
# merging 95 and 96
 DT_97_98 <- merge(DT_df1997_b,DT_df1998_b,by="RasterID1")
 rm(DT_df1997_b,DT_df1998_b, stack13, stack14)
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
# merging 95 and 96
 DT_99_00 <- merge(DT_df1999_b,DT_df2000_b,by="RasterID1")
 rm(DT_df1999_b,DT_df2000_b, stack15, stack16)
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
# merging 2001 and 2002
DT_01_02 <- merge(DT_df2001_b,DT_df2002_b,by="RasterID1")
rm(DT_df2001_b,DT_df2002_b, stack17, stack18)
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
# merging 2001 and 2002
 DT_03_04 <- merge(DT_df2003_b,DT_df2004_b,by="RasterID1")
 rm(DT_df2003_b,DT_df2004_b, stack19, stack20)
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
# merging 2001 and 2002
DT_05_06 <- merge(DT_df2005_b,DT_df2006_b,by="RasterID1")
rm(DT_df2005_b,DT_df2006_b, stack21, stack22)
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
# merging 2007 and 2008
 DT_07_08 <- merge(DT_df2007_b,DT_df2008_b,by="RasterID1")
 rm(DT_df2007_b,DT_df2008_b, stack23, stack24)
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
# merging 2009 and 2010
 DT_09_10 <- merge(DT_df2009_b,DT_df2010_b,by="RasterID1")
 rm(DT_df2009_b,DT_df2010_b, stack25, stack26)
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
####################################
# merging 2007 and 2008
DT_11_12 <- merge(DT_df2011_b,DT_df2012_b,by="RasterID1")
rm(DT_df2011_b,DT_df2012_b, stack27, stack28)
###############################################
# FINAL DATA MERGING AND WRITE TXT FILES TO DISK
 
 
write.table(DT_85_86, paste(outdir,"DT_df_85_86.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_87_88, paste(outdir,"DT_87_88.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_89_90, paste(outdir,"DT_89_90.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_91_92, paste(outdir,"DT_91_92.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_93_94, paste(outdir,"DT_93_94.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_95_96, paste(outdir,"DT_95_96.txt",sep="/"), sep=",",row.names=FALSE)
write.table( DT_97_98, paste(outdir,"DT_97_98.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_99_00, paste(outdir,"DT_99_00.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_01_02, paste(outdir,"DT_01_02.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_03_04, paste(outdir,"DT_03_04.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_05_06, paste(outdir,"DT_05_06.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_07_08, paste(outdir,"DT_07_08.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_09_10, paste(outdir,"DT_09_10.txt",sep="/"), sep=",",row.names=FALSE)
write.table(DT_11_12, paste(outdir,"DT_11_12.txt",sep="/"), sep=",",row.names=FALSE)


 DT_merge_1 <- merge(DT_85_86,DT_87_88, by="RasterID1")
 rm(DT_85_86,DT_87_88)
 DT_merge_2 <- merge(DT_89_90,DT_91_92, by="RasterID1")
 rm(DT_89_90,DT_91_92)
 DT_merge_3 <- merge(DT_93_94,DT_95_96, by="RasterID1")
 rm(DT_93_94,DT_95_96)
 DT_merge_4 <- merge(DT_97_98,DT_99_00, by="RasterID1")
 rm(DT_97_98,DT_99_00)
 DT_merge_5 <- merge(DT_01_02,DT_03_04, by="RasterID1")
 rm(DT_01_02,DT_03_04)
 DT_merge_6 <- merge(DT_05_06,DT_07_08, by="RasterID1")
 rm(DT_05_06,DT_07_08)
 DT_merge_7 <- merge(DT_09_10,DT_11_12, by="RasterID1")
 rm(DT_09_10,DT_11_12)
 DT_merge_12 <- merge(DT_merge_1,DT_merge_2, by="RasterID1")
 rm(DT_merge_1, DT_merge_2)
 DT_merge_34 <- merge(DT_merge_3,DT_merge_4, by="RasterID1")
 rm(DT_merge_3,DT_merge_4)
 DT_merge_56 <- merge(DT_merge_5,DT_merge_6, by="RasterID1")
 rm(DT_merge_5, DT_merge_6)
 DT_merge_1234 <- merge(DT_merge_12,DT_merge_34, by="RasterID1")
 rm(DT_merge_12, DT_merge_34)
 DT_merge_567 <- merge(DT_merge_56, DT_merge_7, by="RasterID1")
 rm(DT_merge_56,DT_merge_7)
 DT_merge_85_12 <- merge(DT_merge_1234, DT_merge_567, by="RasterID1")
 rm(DT_merge_1234, DT_merge_567)
 write.table(DT_merge_85_12, paste(outdir,"DT_merge_85_12.txt",sep="/"), sep=",",row.names=FALSE)

 endTime <- Sys.time()
 elapsedTime_dataframe <- endTime - startTime
 

  # End of manual export of  years of datarames----------------------------------------------------
