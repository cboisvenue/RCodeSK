#Summarize CBMPlotter output area attributes (age) 
#for cASFRI data

#This script works when there are the same # of parameter groups in each csv

#Author: Byron Smiley

#Date: Nov 19 2015

#-----------------------------------------------------
#Packages:
library(dplyr)
library(tidyr)

#-------------------------------------------------------
## Change Result name - this is dependent on the folder input and output name
result="age"
#Identify years of analysis for future column
years <- 1984:2012
#Identify input/output directories and set wd
inputdir <- "G:/PFC_UVicFall2015/Project/Working/Sask_runs/casfri_landsat/maps/CASFRI_output"
resultsdir <- paste(inputdir,"/",result,"/", sep="")
output <- paste(inputdir,"/",result,"/output/", sep="")

setwd(resultsdir)

#list all csv files in folder (each file is 1 year of data)
listfiles <- list.files(path=resultsdir, 
                        pattern='\\.csv$', full.names=TRUE)

#Create table with all years (1984-2012) age values with pGroup hectare values------
#bind all years of data (data column and parameter group ID column)
cleaned <- bind_cols(lapply(listfiles, read.csv, header=FALSE)) 
  
#Name columns to correspond wit the data year
names(cleaned) <- c("pGroups", "pixelCount", "areaHa",
                    "pGroups", "y1984","pGroups", "y1985","pGroups", "y1986",
                    "pGroups", "y1987","pGroups", "y1988","pGroups", "y1989",
                    "pGroups", "y1990","pGroups", "y1991","pGroups", "y1992",
                    "pGroups", "y1993","pGroups", "y1994","pGroups", "y1995",
                    "pGroups", "y1996","pGroups", "y1997","pGroups", "y1998",
                    "pGroups", "y1999","pGroups", "y2000","pGroups", "y2001",
                    "pGroups", "y2002","pGroups", "y2003","pGroups", "y2004",
                    "pGroups", "y2005","pGroups", "y2006","pGroups", "y2007",
                    "pGroups", "y2008","pGroups", "y2009","pGroups", "y2010",
                    "pGroups", "y2011","pGroups", "y2012")

#Remove extra parameter group ID columns and convert to df
cleaned2 <- subset(cleaned, select= c(1,2,3,5,7,9,11,13,15,17,19,21,23,25,27,29,
                                      31,33,35,37,39,41,43,45,47,49,51,53,55,57,
                                      59,61))
cleaned3 <- as.data.frame(cleaned2)
#End of create dataframe of age for each year output-------------------------

# Calculate table of area per age for each year and write to disk----------------
#Area of ages in 1984  
group84 <-  group_by(cleaned3, y1984) %>%
  summarise(total = sum(areaHa))
write.table(group84,file=paste(output, "group84", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1985
group85 <-  group_by(cleaned3, y1985) %>%
  summarise(total = sum(areaHa))
write.table(group85,file=paste(output, "group85", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1986
group86 <-  group_by(cleaned3, y1986) %>%
  summarise(total = sum(areaHa))
write.table(group86,file=paste(output, "group86", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1987
group87 <-  group_by(cleaned3, y1987) %>%
  summarise(total = sum(areaHa))
write.table(group87,file=paste(output, "group87", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1988
group88 <-  group_by(cleaned3, y1988) %>%
  summarise(total = sum(areaHa))
write.table(group88, file=paste(output, "group88", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1989
group89 <-  group_by(cleaned3, y1989) %>%
  summarise(total = sum(areaHa))
write.table(group89,file=paste(output, "group89", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1990
group90 <-  group_by(cleaned3, y1990) %>%
  summarise(total = sum(areaHa))
write.table(group90,file=paste(output, "group90", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1991
group91 <-  group_by(cleaned3, y1991) %>%
  summarise(total = sum(areaHa))
write.table(group91,file=paste(output, "group91", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1992
group92 <-  group_by(cleaned3, y1992) %>%
  summarise(total = sum(areaHa))
write.table(group92,file=paste(output, "group92", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1993
group93 <-  group_by(cleaned3, y1993) %>%
  summarise(total = sum(areaHa))
write.table(group93, file=paste(output, "group93", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1994
group94 <-  group_by(cleaned3, y1994) %>%
  summarise(total = sum(areaHa))
write.table(group94,file=paste(output, "group94", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1995
group95 <-  group_by(cleaned3, y1995) %>%
  summarise(total = sum(areaHa))
write.table(group95,file=paste(output, "group95", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1996
group96 <-  group_by(cleaned3, y1996) %>%
  summarise(total = sum(areaHa))
write.table(group96,file=paste(output, "group96", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1997
group97 <-  group_by(cleaned3, y1997) %>%
  summarise(total = sum(areaHa))
write.table(group97, file=paste(output, "group97", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1998
group98 <-  group_by(cleaned3, y1998) %>%
  summarise(total = sum(areaHa))
write.table(group98, file=paste(output, "group98", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 1999
group99 <-  group_by(cleaned3, y1999) %>%
  summarise(total = sum(areaHa))
write.table(group99,file=paste(output, "group99", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2000
group00 <-  group_by(cleaned3, y2000) %>%
  summarise(total = sum(areaHa))
write.table(group00,file=paste(output, "group00", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2001
group01 <-  group_by(cleaned3, y2001) %>%
  summarise(total = sum(areaHa))
write.table(group01,file=paste(output, "group01", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2002
group02 <-  group_by(cleaned3, y2002) %>%
  summarise(total = sum(areaHa))
write.table(group02,file=paste(output, "group02", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2003
group03 <-  group_by(cleaned3, y2003) %>%
  summarise(total = sum(areaHa))
write.table(group03,file=paste(output, "group03", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2004
group04 <-  group_by(cleaned3, y2004) %>%
  summarise(total = sum(areaHa))
write.table(group04,file=paste(output, "group04", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2005
group05 <-  group_by(cleaned3, y2005) %>%
  summarise(total = sum(areaHa))
write.table(group05,file=paste(output, "group05", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2006
group06 <-  group_by(cleaned3, y2006) %>%
  summarise(total = sum(areaHa))
write.table(group06,file=paste(output, "group06", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2007
group07 <-  group_by(cleaned3, y2007) %>%
  summarise(total = sum(areaHa))
write.table(group07, file=paste(output, "group07", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2008
group08 <-  group_by(cleaned3, y2008) %>%
  summarise(total = sum(areaHa))
write.table(group08,file=paste(output, "group08", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2009
group09 <-  group_by(cleaned3, y2009) %>%
  summarise(total = sum(areaHa))
write.table(group09,file=paste(output, "group09", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2010
group10 <-  group_by(cleaned3, y2010) %>%
  summarise(total = sum(areaHa))
write.table(group10,file=paste(output, "group10", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2011
group11 <-  group_by(cleaned3, y2011) %>%
  summarise(total = sum(areaHa))
write.table(group11,file=paste(output, "group11", ".csv", sep=""), sep=",", 
              row.names=FALSE)
#Area of ages in 2012
group12 <-  group_by(cleaned3, y2012) %>%
  summarise(total = sum(areaHa))
write.table(group12,file=paste(output, "group12", ".csv", sep=""), sep=",", 
              row.names=FALSE)

#End of calculate and write per year age areas-----------------------------
#join age areas into one table--------------------------------------------------

#Create dataframe with each possible age value (used as key)
age <- 0:190
age <- as.data.frame(age)

#Join all individual year age areas into one table, renaming column names--------
join <- full_join(age, group84, by= c("age" = "y1984")) %>%
  rename(y1984= total) %>%
  full_join(group85, by=c("age" = "y1985")) %>%
  rename(y1985= total) %>%
  full_join(group86, by=c("age" = "y1986")) %>%
  rename(y1986= total) %>%
  full_join(group87, by=c("age" = "y1987")) %>%
  rename(y1987= total) %>%
  full_join(group88, by=c("age" = "y1988")) %>%
  rename(y1988= total) %>%
  full_join(group89, by=c("age" = "y1989")) %>%
  rename(y1989= total) %>%
  full_join(group90, by=c("age" = "y1990")) %>%
  rename(y1990= total) %>%
  full_join(group91, by=c("age" = "y1991")) %>%
  rename(y1991= total) %>%
  full_join(group92, by=c("age" = "y1992")) %>%
  rename(y1992= total) %>%
  full_join(group93, by=c("age" = "y1993")) %>%
  rename(y1993= total) %>%
  full_join(group94, by=c("age" = "y1994")) %>%
  rename(y1994= total) %>%
  full_join(group95, by=c("age" = "y1995")) %>%
  rename(y1995= total) %>%
  full_join(group96, by=c("age" = "y1996")) %>%
  rename(y1996= total) %>%
  full_join(group97, by=c("age" = "y1997")) %>%
  rename(y1997= total) %>%
  full_join(group98, by=c("age" = "y1998")) %>%
  rename(y1998= total) %>%
  full_join(group99, by=c("age" = "y1999")) %>%
  rename(y1999= total) %>%
  full_join(group00, by=c("age" = "y2000")) %>%
  rename(y2000= total) %>%
  full_join(group01, by=c("age" = "y2001")) %>%
  rename(y2001= total) %>%
  full_join(group02, by=c("age" = "y2002")) %>%
  rename(y2002= total) %>%
  full_join(group03, by=c("age" = "y2003")) %>%
  rename(y2003= total) %>%
  full_join(group04, by=c("age" = "y2004")) %>%
  rename(y2004= total) %>%
  full_join(group05, by=c("age" = "y2005")) %>%
  rename(y2005= total) %>%
  full_join(group06, by=c("age" = "y2006")) %>%
  rename(y2006= total) %>%
  full_join(group07, by=c("age" = "y2007")) %>%
  rename(y2007= total) %>%
  full_join(group08, by=c("age" = "y2008")) %>%
  rename(y2008= total) %>%
  full_join(group09, by=c("age" = "y2009")) %>%
  rename(y2009= total) %>%
  full_join(group10, by=c("age" = "y2010")) %>%
  rename(y2010= total) %>%
  full_join(group11, by=c("age" = "y2011")) %>%
  rename(y2011= total) %>%
  full_join(group12, by=c("age" = "y2012")) %>%
  rename(y2012= total)

write.table(join,file=paste(output, "join", ".csv", sep=""), sep=",", 
            row.names=FALSE)
#End of create combined table and writing to disk-------------------------------

#Bin hectares into age classes for each year of data (1984-2012)-------------

#First step of binning is creating a group attribute for each age 
#This assigns 10 year age bins
df1 <-  transform(join, group=cut(age, breaks=c(0,10,20,30,40,50,60,70,80,90,
                                                100,110,120,130,140,150,160,170,
                                                180, 190),
                                labels=c("<10",10,20,30,40,50,60,70,80,90,
                                         100,110,120,130,140,150,160,170,
                                         ">180")), include.lowest=TRUE)
#Need to remove NAs otherwise binning of age classes does not occur for classes
#with NAs
df1[is.na(df1)] <- 0

#This does the binning (grouping by the 'group' column), summing area (hectares)
#for each bin across all years of data (1984-2012)
binned_10 <- group_by(df1,group) %>% 
  summarise_each(funs(Sum=sum(.)), c(y1984, y1985, y1986,
                                     y1987, y1988, y1989,
                                     y1990, y1991, y1992,
                                     y1993, y1994, y1995,
                                     y1996, y1997, y1998,
                                     y1999, y2000, y2001,
                                     y2002, y2003, y2004,
                                     y2005, y2006, y2007,
                                     y2008, y2009, y2010,
                                     y2011, y2012))
#Write the binned somes to disk
write.table(binned_10,file=paste(output, "age_binned_10", ".csv", sep=""), sep=",", 
            row.names=FALSE)
#End of age area binning for 10 year age bins--------------------------------------
