#Reclass/round dist_yr raster to be only decade values
#----------------------------------------------------------------
library(raster)
library(parallel)
library(snow)
#----------------------------------------------------------------
OutName <- "casfri"
indir <- paste("H:/saskatchewan/spatialGrowth/", OutName, "/casfri_intermediate/", sep="")
setwd(indir)
dist_yr <- raster("dist_yr.tif")
#--------------------------------------------------------------

dist_decade <- round(dist_yr, digits=-1)
writeRaster(dist_decade, filename= "dist_decade", format='GTiff', datatype='INT2U', overwrite=TRUE)

#End of reclassify disturbance decade raster--------------------------
