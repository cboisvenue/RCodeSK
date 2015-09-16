#-------------------------------------------------------------------------------------
# Taking the Int_age.tif layer from the 250m-Canada runs and creating a raster of ages
# for the forested area of SK
#
# CBoisvenue
# April 23rd, 2015
#-------------------------------------------------------------------------------------

#Required packages
library(rgdal)
# these two are required by gdal and should load automotically
# library(sp)
library(raster)
library(snow)
library(spatial.tools)
library(parallel)
#-------------------------------------------------------------


# Steps:
# read-in knn-ages
# subset knn_age to only 'WCL Prince Albert FMA' forest district
# reproject to 30m
# create an age layer for each year 1984-2012

# read-in knn_age ---------------------------------------------
# the 250m age raster is here:
dir = "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Sask_runs/layers/"
#add stage age integers (trunkated knn age raster) (250 meters)
# this is for the year 2001
knn_age <- raster(paste(dir,"Stand_age_Integers1.tif",sep=""))
# End read-in ------------------------------------------------

# This is the extent and resolution I want
dir = "M:/Spatially_explicit/01_Projects/07_SK_30m/exchangeFolder/Crop30_PaFMA.tif"
crop30 <- raster(dir)
#make same projection as knn
crop30 <- projectRaster(crop30, knn_age)

#Then crop by multiplying
crop_mult <- crop30 * knn_age

###Try this.....

setwd("H:/Saskatchewan/bsmiley_work/Sask")
SaskPolygons <- readOGR(dsn = "Sask_area_datasets", layer = "Sask_scape_reproj") # add Sask polygons
ForestDistrict <- SaskPolygons[SaskPolygons$AdminBou_1 == 'WCL Prince Albert FMA',]
#make same projection as knn_age
ForestDistrict <- spTransform(ForestDistrict, crs(knn_age)) 

# Make a SpatialPolygon from the extent of crop30 but this is a square...
#crop_extent <- as(extent(crop30),'SpatialPolygons')
# Assign this SpatialPolygon the right projection
#proj4string(crop_extent) <- proj4string(crop30)
#outres = proj4string(crop_extent)
# Transform the projection to that of r1
#cropextr1proj <- spTransform(knn_age, CRS(proj4string(crop_extent))) # this does not work
#cropextr1proj <- spTransform(knn_age, CRS(proj4string(crop30)))
# but this does
#cropextr1proj <- spTransform(crop_extent, CRS(proj4string(knn_age)))
#cropextr2proj <- spTransform(knn_age, CRS(outres)) # this would be huge


#Finally, you can crop raster knn_age using the polygon cropextr1proj which represents 
#the extent of r2 in the projection of r1. 

### And this...

#Mask only removes the pixel values for areas outside the polygon but keeps the total extent of the raster
crop_age <- mask(knn_age,ForestDistrict)
#Crop removes the extent outside the polygon (NW, NE, SE, SW corners)
crop_age <- crop(crop_age, ForestDistrict)

## PROBLEM now is that the crop_age is a "block" not just the PAFMA...crop30 is at 30 m and has the right extent
# and they have different CRS
# this makes their CRS the same
#cropage1 <- projectRaster(crop_age, crs=outres)
# but this still does not work:
#crop2 <- cropage1*crop30

# How to "cut-out" just the FMA?
PAFMA_age <- spatial_sync_raster(cropage1,crop30)
plot(PAFMA_age)
PAFMA_age1 <- spatial_sync_raster(cropage1,crop30,method='bilinear')
plot(PAFMA_age1)

PAFMA_age2 <- raster(crop30,PAFMA_age1,field=1)
## Not solved ####None of the lines below work
# none of these work...----------------------------------------------------------
crop2 <- rasterize(crop30,crop_age)
paFMA_age <- spTransform(crop_age,CRS(proj4string(crop30)))
pile2 <- stack(extent(cropage1,crop30)
               
#Then plot the two rasters.



PaFMA_30age <- raster(crop_age,crop30,field=1)



PaFMA_30age <- rasterize(knn_age, crop30, field=1) # rasterize Saskarea using 30 template

require(rgdal)
sk_ages <- sp::spTransform(knn_age, crs(crop30)) # reproject Sask_area to to 30m
