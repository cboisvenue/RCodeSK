#Zonal statistics for biomass rasters using CASFRI strata


#Author: Byron Smiley

#Date: November 6th 2015


#------------------------------------------------------------------
#Packages:

require(raster)
require(spatial.tools)

install.packages("raster")

#File Locations----------------------------------------------------
OutName="casfri"
years <- (1984:2012)


strataInput = "G:/PFC_UVicFall2015/Project/Working/Sask_runs/casfri_landsat/layers/"
biomassInput = paste("G:/PFC_UVicFall2015/data/SK/BiomassRasters/SK/", OutName, "/biomass/",sep="")
output = "G:/PFC_UVicFall2015/Project/Working/biomass_spread/results"
#------------------------------------
#Begin create Strata raster---------------------------------------------------------------

casfri_dom <- raster(paste(strataInput, "casfri_dom2.tif", sep=""))
site_productivity <- raster(paste(strataInput, "site_productivity.tif", sep=""))

#multiple speices codes by 10
casfri_dom10 <- casfri_dom*10

#Add species codes to site productivity codes to get unique strata values
strataZones <- casfri_dom10 + site_productivity

writeRaster(strata, file.path(paste(output_SMlog_D_biomass, "strata",sep="")),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)

#End of create strata raster---------------------------------------------------------------
#Add biomass rasters

biomass_list <- list.files(biomassInput, pattern= ".tif$", full.names=TRUE)
biomass_stack <- stack(biomass_list)

startTime <- Sys.time()
meanZonalBio <- zonal(biomass_stack, strataZones, fun="mean", na.rm=TRUE)
# sd function Does not work
sdZonalBio <- zonal(biomass_stack, strataZones, digits= 0, fun="sd", na.rm=TRUE)
countZonalBio <- zonal(biomass_stack, strataZones, fun="count", na.rm=TRUE)

endTime <- Sys.time()
ZonalStatistics <- endTime - startTime
# Time difference of 2.745389 hours

setwd(output)
write.table(meanZonalBio, "meanZonalBio.csv", sep=",", row.names = FALSE)
write.table(countZonalBio, "countZonalBio.csv", sep=",", row.names = FALSE)

#SD attempt 2------------------------

strataPolys <- rasterToPolygons(strataZones, na.rm=TRUE, dissolve=TRUE)
