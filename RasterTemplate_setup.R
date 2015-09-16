# Study area template raster setup script (used as input for BATCHCROP script)-------------------------

#This script:

# 1) Setups up template study area raster used to clip landsat images

#Required packages---------------------------------------------------------------------
library(rgdal)
library(sp)
library(raster)
library(snow)
library(spatial.tools)
library(parallel)
#Set temp file... has to have >50gb of room
rasterOptions(tmpdir="H:/saskatchewan/data/temp")

#----------------------------------------------------------------------------------------
## SETUP ## Generates 30m raster of a specific forest district in Province of Saskatchewan
# Total batchcrop time at 30m on A125345 workstation (30 cluster): 7.226512 (setup+batchcrop)
#----------------------------------------------------------------------------------------

#Initial setup when using a forest district as a study area
###############################################################################################
#Subset Sask area to include only 'WCL Prince Albert FMA' forest district
beginCluster(30)
startTime <- Sys.time()
setwd("H:/saskatchewan/data/originals/landsat/LCC_proxy_composites") # these are from Joanne White
template30 <- raster("skprx1984c1") 
setwd("H:/saskatchewan/data")

### from pspu.shp Max used for wall-to-wall. I selected just Sask polygons and made a new file
Sask_area <- readOGR(dsn = "reference", layer = "Sask_scape_reproj") # add SK vector Sask province

# reproject Sask_area to Recliner inputs
Sask_area <- spTransform(Sask_area, crs(template30))

# Change AdminBoundary based on area to be clipped by----------------------

PaFMA <- Sask_area[Sask_area$AdminBou_1 == 'WCL Prince Albert FMA',]

# End of Study area forest district select
################################################################################################

#CASFRI extent template----------------------------------------------------------------
CASFRI_area <- readOGR(dsn = "reference", layer = "casfri_extent")
CASFRI_area <- spTransform(CASFRI_area, crs(template30))

# Rasterize study area based on LANDSAT projection and cell size attributes (30m res)----
startTime <- Sys.time()
CASFRI_30_temp_new <- rasterize(CASFRI_area, template30, field=1)

# Remove NA areas from study area
CASFRI_30_temp2 <- crop(CASFRI_30_temp_new, CASFRI_area, snap='out')

# Add pixels to outer edge to ensure full coverage
CASFRI_30_temp3 <- buffer(CASFRI_30_temp2,doEdge=TRUE, width=500)

# Set up Reference layer for BatchCrop
Crop30 <- CASFRI_30_temp3

#Write Study area raster to 'reference' directory (change name to reflect forest district)
setwd("H:/saskatchewan/data/reference")
writeRaster(Crop30, "Crop30_CASFRI.tif", overwrite=TRUE, format='GTiff', datatype='INT2U')

endTime <- Sys.time()
elapsedTime <- endTime - startTime
# 4.526815 hours to here
# 6.023127 days to here for all CASFRI area

# End of raster template setup script--------------------------------------------------------