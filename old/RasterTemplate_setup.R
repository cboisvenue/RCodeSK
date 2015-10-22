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

# End of Study area polygon select-----------------------------------------------------

# Rasterize study area based on LANDSAT projection and cell size attributes (30m res)----

PaFMA_30_temp_new <- rasterize(PaFMA, template30, field=1)

# Remove NA areas from study area
PaFMA_30_temp2 <- crop(PaFMA_30_temp, PaFMA, snap='out')

# Add pixels to outer edge to ensure full
PaFMA_30_temp3 <- buffer(PaFMA_30_temp2,doEdge=TRUE, width=500)

# Set up Reference layer for BatchCrop
Crop30 <- PaFMA_30_temp3

#Write Study area raster to 'reference' directory (change name to reflect forest district)
setwd("H:/saskatchewan/data/reference")
writeRaster(Crop30, "Crop30_PaFMA.tif", overwrite=TRUE, format='GTiff', datatype='INT2U')

endTime <- Sys.time()
elapsedTime <- endTime - startTime
# 4.526815 hours to here

# End of raster template setup script--------------------------------------------------------