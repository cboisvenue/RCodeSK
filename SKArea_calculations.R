#----------------------------------------------------------------------------------------------------
# Area Calculations for Sask area, forest cover area, and fmafa study area


# Bsmiley
# May 14, 2015

#-----------------------------------------------------------
require(sp)
require(raster)
require(rgeos)

#Add files-----------------------------------------------------------------------------------------------
refs = "H:/Saskatchewan/bsmiley_work/Sask/Sask_area_datasets/"
masks = "H:/Saskatchewan/bsmiley_work/Sask/SKmask/"

setwd(refs)
SaskPolygons <- readOGR(dsn = "Sask_area_datasets", layer = "Sask_scape_reproj") # add Sask polygons
SaskPolygons <- spTransform(SaskPolygons, crs(Sask30))

#Add raster files
setwd(masks)
Sask30 <- raster("Sask30.tif")
forest_Joanne <- raster("SK_mask.dat")

setwd("H:/Saskatchewan/Sask_runs/Run6/layers")
knn_forest <- raster("Sask Forest_Cov.tif")

setwd("H:/Saskatchewan/LandsatStack/LCC_proxy_composites")
lsComposite <- raster("skprx2012c1")

setwd("H:/Saskatchewan/clipped")
fmapa <- raster("Crop30_PaFMA.tif")

#-----Calculate area using polygon datasets in hectares---------------------------------------------------

#Calcluates Saskatchewan area of polygon dataset
saskPolyAreaHa <- (gArea(SaskPolygons) / 10000)

#Calculates Prince Albert FMA area using polygon dataset
fmapaPolys <- SaskPolygons[SaskPolygons$AdminBou_1 == 'WCL Prince Albert FMA',]
fmapaArea <- (gArea(fmapaPolys) / 10000)

percentSaskCoveredByfmapaPOLYS <- (fmapaArea/saskPolyAreaHa) *100

#-----End of Calculate area using polygon datasets--------------------------------------------------------



#-----Calculate pixel count and area using raster datasets------------------------------------------------

#Calculate total number of non-NA pixels of:---NOTE: CELL VALUE MUST BE = to 1---------------------------
#1- Study area (all of Saskatchewan)
#2- forest area based on 30m resolution classification from Joanne White
#3- forest area based on 250m resolution (downscaled to 30m resolution) knn forest cover
#4- forest area based on clipped Prince Albert forest management area

studyAreaCellCount <- cellStats(Sask30 , 'sum', na.rm=TRUE)
joanneFCHaCellCount <- cellStats(forest_Joanne , 'sum', na.rm=TRUE)
knnFCHaCellCount <- cellStats(knn_forest , 'sum', na.rm=TRUE)
fmapaCellCount <- cellStats(fmapa , 'sum', na.rm=TRUE)

#End of pixel count calculations--------------------------------------------------------------------------

#Calculate total area in hectares of:--------------------------------------------------------------
#1- Study area (all of Saskatchewan)
#2- Area of forests based on 30m resolution classification from Joanne White
#3- Area of forests based on 250m resolution (downscaled to 30m resolution) knn forest cover
#4- Prince Albert FMA Study area

studyAreaHa <- (cellStats(Sask30 , 'sum', na.rm=TRUE)  * prod(res(Sask30)) / 10000)
joanneFCHa <- (cellStats(forest_Joanne , 'sum', na.rm=TRUE)  * prod(res(forest_Joanne)) / 10000)
knnFCHa <- (cellStats(knn_forest , 'sum', na.rm=TRUE)  * prod(res(knn_forest)) / 10000)
fmapaHa <- (cellStats(fmapa , 'sum', na.rm=TRUE)  * prod(res(fmapa)) / 10000)

#End of area calculations--------------------------------------------------------------------------

#Calculate 'percent of" values---------------------------------------------------------------------

#Percent area forest (based on Joanne's forest mask)
percentForestJW <- (joanneFCHa/studyAreaHa) *100
percentForestknn <- (knnFCHa/studyAreaHa) *100

#Percent of total Saskatchewan area covered by Prince Albert FMA
percentSaskCoveredByfmapa <- (fmapaHa/studyAreaHa) *100

#End of 'percent of" calculations---------------------------------------------------------------------

