#Summarize CBMPlotter output area attributes (disturbance) 
#for cASFRI data

#This script (hopefully) works when there a different # of parameter groups 
#in each csv input

#Author: Byron Smiley

#Date: Nov 20 2015

#-----------------------------------------------------
#Packages:
library(dplyr)
library(tidyr)

#-------------------------------------------------------
## Change Result name - this is dependent on the folder input and output name
result="DistArea"
#Identify years of analysis for future column
years <- 1984:2012
#Identify input/output directories and set wd
inputdir <- "G:/PFC_UVicFall2015/Project/Working/Sask_runs/casfri_landsat/maps/CASFRI_output"
resultsdir <- paste(inputdir,"/",result,"/", sep="")
output <- paste(inputdir,"/",result,"/output/", sep="")

setwd(resultsdir)


#csvs must have values. For first year and last year, use just parameter groups for first
# column and zeros for second column

#list all csv files in folder (each file is 1 year of data)
listfiles <- list.files(path=resultsdir, 
                        pattern='\\.csv$', full.names=FALSE)

#Make list of file names to assign to imported yearly data and parameter group/area csv

df <- c("areaHa", "group84", "group85","group86","group87", "group88", 
        "group89","group90", "group91", "group92","group93", "group94", 
        "group95","group96", "group97", "group98","group99", "group00",
        "group01", "group02", "group03", "group04","group05", "group06", 
        "group07", "group08", "group09", "group10","group11", "group12")

#import all csvs and assign names
for (i in 1:length(listfiles)) assign(df[i], read.csv(listfiles[i], 
                                                      header=FALSE))

#join age areas into one table--------------------------------------------------

#Join all individual year age areas into one table, renaming column names--------

#rename area per parameter group csv columns
areaHa <- rename(areaHa, pGroups = V1, pixels = V2, hectares = V3)

#Use full join to join yearly data by parameter group with area values
joinDist <- full_join(areaHa, group84, by= c("pGroups" = "V1")) %>%
  rename(y1984= V2) %>%
  full_join(group85, by=c("pGroups" = "V1")) %>%
  rename(y1985= V2) %>%
  full_join(group86, by=c("pGroups" = "V1")) %>%
  rename(y1986= V2) %>%
  full_join(group87, by=c("pGroups" = "V1")) %>%
  rename(y1987= V2) %>%
  full_join(group88, by=c("pGroups" = "V1")) %>%
  rename(y1988= V2) %>%
  full_join(group89, by=c("pGroups" = "V1")) %>%
  rename(y1989= V2) %>%
  full_join(group90, by=c("pGroups" = "V1")) %>%
  rename(y1990= V2) %>%
  full_join(group91, by=c("pGroups" = "V1")) %>%
  rename(y1991= V2) %>%
  full_join(group92, by=c("pGroups" = "V1")) %>%
  rename(y1992= V2) %>%
  full_join(group93, by=c("pGroups" = "V1")) %>%
  rename(y1993= V2) %>%
  full_join(group94, by=c("pGroups" = "V1")) %>%
  rename(y1994= V2) %>%
  full_join(group95, by=c("pGroups" = "V1")) %>%
  rename(y1995= V2) %>%
  full_join(group96, by=c("pGroups" = "V1")) %>%
  rename(y1996= V2) %>%
  full_join(group97, by=c("pGroups" = "V1")) %>%
  rename(y1997= V2) %>%
  full_join(group98, by=c("pGroups" = "V1")) %>%
  rename(y1998= V2) %>%
  full_join(group99, by=c("pGroups" = "V1")) %>%
  rename(y1999= V2) %>%
  full_join(group00, by=c("pGroups" = "V1")) %>%
  rename(y2000= V2) %>%
  full_join(group01, by=c("pGroups" = "V1")) %>%
  rename(y2001= V2) %>%
  full_join(group02, by=c("pGroups" = "V1")) %>%
  rename(y2002= V2) %>%
  full_join(group03, by=c("pGroups" = "V1")) %>%
  rename(y2003= V2) %>%
  full_join(group04, by=c("pGroups" = "V1")) %>%
  rename(y2004= V2) %>%
  full_join(group05, by=c("pGroups" = "V1")) %>%
  rename(y2005= V2) %>%
  full_join(group06, by=c("pGroups" = "V1")) %>%
  rename(y2006= V2) %>%
  full_join(group07, by=c("pGroups" = "V1")) %>%
  rename(y2007= V2) %>%
  full_join(group08, by=c("pGroups" = "V1")) %>%
  rename(y2008= V2) %>%
  full_join(group09, by=c("pGroups" = "V1")) %>%
  rename(y2009= V2) %>%
  full_join(group10, by=c("pGroups" = "V1")) %>%
  rename(y2010= V2) %>%
  full_join(group11, by=c("pGroups" = "V1")) %>%
  rename(y2011= V2) %>%
  full_join(group12, by=c("pGroups" = "V1")) %>%
  rename(y2012= V2)
  
#Write joined data to file (in case needed later)  
write.table(joinDist,file=paste(output, "joinDist", ".csv", sep=""), sep=",", 
              row.names=FALSE)

#End of create combined table and writing to disk-------------------------------


#Bin areas into disturbance types for each year (column)------------

#Need to remove NAs otherwise binning of age classes does not occur for classes
#with NAs
joinDist[is.na(joinDist)] <- 0

#This makes a new file per year by grouping the disturbance types in each year and 
#Summing the hectares for each unique disturbance type
DistSum84 <- group_by(joinDist, y1984) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum85 <- group_by(joinDist, y1985) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum86 <- group_by(joinDist, y1986) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum87 <- group_by(joinDist, y1987) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum88 <- group_by(joinDist, y1988) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum89 <- group_by(joinDist, y1989) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum90 <- group_by(joinDist, y1990) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum91 <- group_by(joinDist, y1991) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum92 <- group_by(joinDist, y1992) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum93 <- group_by(joinDist, y1993) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum94 <- group_by(joinDist, y1994) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum95 <- group_by(joinDist, y1995) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum96 <- group_by(joinDist, y1996) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum97 <- group_by(joinDist, y1997) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum98 <- group_by(joinDist, y1998) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum99 <- group_by(joinDist, y1999) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum00 <- group_by(joinDist, y2000) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum01 <- group_by(joinDist, y2001) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum02 <- group_by(joinDist, y2002) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum03 <- group_by(joinDist, y2003) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum04 <- group_by(joinDist, y2004) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum05 <- group_by(joinDist, y2005) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum06 <- group_by(joinDist, y2006) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum07 <- group_by(joinDist, y2007) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum08 <- group_by(joinDist, y2008) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum09 <- group_by(joinDist, y2009) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum10 <- group_by(joinDist, y2010) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum11 <- group_by(joinDist, y2011) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)
DistSum12 <- group_by(joinDist, y2012) %>% 
  summarise_each(funs(Sum=sum(.)), hectares)

#This rejoins the data into one file
DistSums <- full_join(DistSum84,DistSum85, by =c("y1984" = "y1985")) %>%
  rename(DistType= y1984, Dist84 = hectares.x, Dist85 = hectares.y) %>%
  full_join(DistSum86, by =c("DistType" = "y1986")) %>%
  rename(Dist86= hectares) %>%
  full_join(DistSum87, by =c("DistType" = "y1987")) %>%
  rename(Dist87= hectares) %>%
  full_join(DistSum88, by =c("DistType" = "y1988")) %>%
  rename(Dist88= hectares) %>%
  full_join(DistSum89, by =c("DistType" = "y1989")) %>%
  rename(Dist89= hectares) %>%
  full_join(DistSum90, by =c("DistType" = "y1990")) %>%
  rename(Dist90= hectares) %>%
  full_join(DistSum91, by =c("DistType" = "y1991")) %>%
  rename(Dist91= hectares) %>%
  full_join(DistSum92, by =c("DistType" = "y1992")) %>%
  rename(Dist92= hectares) %>%
  full_join(DistSum93, by =c("DistType" = "y1993")) %>%
  rename(Dist93= hectares) %>%
  full_join(DistSum94, by =c("DistType" = "y1994")) %>%
  rename(Dist94= hectares) %>%
  full_join(DistSum95, by =c("DistType" = "y1995")) %>%
  rename(Dist95= hectares) %>%
  full_join(DistSum96, by =c("DistType" = "y1996")) %>%
  rename(Dist96= hectares) %>%
  full_join(DistSum97, by =c("DistType" = "y1997")) %>%
  rename(Dist97= hectares) %>%
  full_join(DistSum98, by =c("DistType" = "y1998")) %>%
  rename(Dist98= hectares) %>%
  full_join(DistSum99, by =c("DistType" = "y1999")) %>%
  rename(Dist99= hectares) %>%
  full_join(DistSum00, by =c("DistType" = "y2000")) %>%
  rename(Dist00= hectares) %>%
  full_join(DistSum01, by =c("DistType" = "y2001")) %>%
  rename(Dist01= hectares) %>%
  full_join(DistSum02, by =c("DistType" = "y2002")) %>%
  rename(Dist02= hectares) %>%
  full_join(DistSum03, by =c("DistType" = "y2003")) %>%
  rename(Dist03= hectares) %>%
  full_join(DistSum04, by =c("DistType" = "y2004")) %>%
  rename(Dist04= hectares) %>%
  full_join(DistSum05, by =c("DistType" = "y2005")) %>%
  rename(Dist05= hectares) %>%
  full_join(DistSum06, by =c("DistType" = "y2006")) %>%
  rename(Dist06= hectares) %>%
  full_join(DistSum07, by =c("DistType" = "y2007")) %>%
  rename(Dist07= hectares) %>%
  full_join(DistSum08, by =c("DistType" = "y2008")) %>%
  rename(Dist08= hectares) %>%
  full_join(DistSum09, by =c("DistType" = "y2009")) %>%
  rename(Dist09= hectares) %>%
  full_join(DistSum10, by =c("DistType" = "y2010")) %>%
  rename(Dist10= hectares) %>%
  full_join(DistSum11, by =c("DistType" = "y2011")) %>%
  rename(Dist11= hectares) %>%
  full_join(DistSum12, by =c("DistType" = "y2012")) %>%
  rename(Dist12= hectares)
  
#Write the joined disturbance type summed areas for each year to disk
write.table(DistSums,file=paste(output, "DistSums", ".csv", sep=""), sep=",", 
            row.names=FALSE)
#End of disturbance area binning--------------------------------------
