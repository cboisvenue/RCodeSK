#replace casfri_dom NoData values with '0' for recliner run
#----------------------------------------------------------------
library(raster)
#----------------------------------------------------------------
setwd("C:/Sask_CBMRuns/fmapa_landsat/layers")
casfri_dom <- raster("casfri_dom.tif")
#--------------------------------------------------------------

recliner_table <- data.frame(old=NA, update=(0))
beginCluster(30)
casfri_dom2 <- subs(casfri_dom, recliner_table, by=1, which=2, subsWithNA=FALSE)

writeRaster(casfri_dom2, filename= "casfri_dom2", format='GTiff', datatype='INT2U', overwrite=TRUE)