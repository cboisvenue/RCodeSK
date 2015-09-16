#------------------------------------------
# Random notes: spending the day with Byron trying to lear how to run Recliner and
# use R as a GIS
#
# I will try to use the features of SpaDES
# March 6ht, 2015
# CBoisvenue
#----------------------------------------------

library("SpaDES", lib.loc="C:/condor_sync/R/3.1.2/library")

#library(SpaDES)
fileList <-
  data.frame(files =
               dir(file.path(find.package("SpaDES",
                                          lib.loc="C:/condor_sync/R/3.1.2/library",
                                          quiet=FALSE),
                             "maps"),
                   full.names=TRUE, pattern= "tif"),
             functions="rasterToMemory",
             .stackName="landscape",
             packages="SpaDES",
             stringsAsFactors=FALSE)
# Load files to memory (using rasterToMemory) and stack them (because .stackName is provided above)
loadFiles(fileList=fileList)
