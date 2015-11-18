#Summarize CBMPlotter csv output for cASFRI data

#Author: Byron Smiley

#Date: Nov 17 2015

#-----------------------------------------------------
#Packages:
library(dplyr)

library(ggplot2)

install.packages("ggplot2")
#-------------------------------------------------------
years <- 1984:2012
result="NEP"
inputdir <- "G:/PFC_UVicFall2015/Project/Working/Sask_runs/casfri_landsat/maps/CASFRI_output"
resultsdir <- paste(inputdir,"/",result,"/", sep="")

setwd(resultsdir)

listfiles <- list.files(path=resultsdir, 
                        pattern='\\.csv$', full.names=TRUE)

cleaned <- bind_cols(lapply(listfiles, read.csv, header=FALSE)) 
  
names(cleaned) <- c("pGroups", "1984","pGroups", "1985","pGroups", "1986",
                    "pGroups", "1987","pGroups", "1988","pGroups", "1989",
                    "pGroups", "1990","pGroups", "1991","pGroups", "1992",
                    "pGroups", "1993","pGroups", "1994","pGroups", "1995",
                    "pGroups", "1996","pGroups", "1997","pGroups", "1998",
                    "pGroups", "1999","pGroups", "2000","pGroups", "2001",
                    "pGroups", "2002","pGroups", "2003","pGroups", "2004",
                    "pGroups", "2005","pGroups", "2006","pGroups", "2007",
                    "pGroups", "2008","pGroups", "2009","pGroups", "2010",
                    "pGroups", "2011","pGroups", "2012")

cleaned2 <- subset(cleaned, select= c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,
                                      32,34,36,38,40,42,44,46,48,50,52,54,56,58))
  
cleaned3 <- colSums(cleaned2 [,-1])
cleaned3 <- data_frame(years,cleaned3)
names(cleaned3) <- c("Year", result)
plot(cleaned3)
write.table(cleaned3, file=paste(result, ".csv", sep=""), sep=",", row.names=FALSE)
