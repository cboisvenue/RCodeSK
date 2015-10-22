#Sask testing

setwd("C:/Hammock/Sask/SKmask")

library(rgeos)
library(maptools)
library(dplyr)
library(rgdal)
library(sp)
library(raster)
library(gdata)
library(SpaDES)
library(grid)

install.packages("mapproj")
install.packages("maptools")
install.packages("rgeos")
install.packages("geosphere")
install.packages("spdep")
install.packages("grid")

dsn <- system.file("vectors", package = "rgdal")
canada_polys<-readOGR(dsn = "C:/Hammock/Canada_concept/layers", layer = "pspu")

canada_polys <- readShapePoly("pspu.shp")
plot(canada_polys)
head(canada_polys)

canada_polys_frame <- data.frame(canada_polys)

distinct(select(canada_polys, ProvinceNa))
Sask <- filter(canada_polys, ProvinceNa == "Saskatchewan")
head(Sask)
?filter
?ISunique
?unique
duplicated(Sask)

#check structure of spatial data
str(sk_extent)
#accessing the data slot
canada_polys@data
#accessing corrdinates (centroid)
coordinates(canada_polys)
#plotting spatial data
plot(canada_polys)


#Add raster data (in tifs) from a folder (/layers) using SPADES

fileList <-
  data.frame(files =
               dir(file.path(
                 "C:/Hammock/Canada_concept"
                 ,"layers"),
                 full.names=TRUE, pattern= ".tif$"),
             functions="raster",
             packages="raster",
             stringsAsFactors=FALSE)
loadFiles(fileList=fileList)

#add mask raster
SKmask <- raster("SK_mask")

#ReProject rasters to sk_extent projection
DT1_proj <- projectRaster(DT1, sk_extent)

#Crop DT1 to sk_extent

sk_DT1 <- crop(DT1, sk_extent, snap="near")

#CHECKS
plot(DT5)
fc_can <- CBM_inputs$Forest_Cover
plot(fc_can)

Forest_Cover <- CBM-inputs$DEM
DEM
Plot(DT5, add=FALSE)
str(sk_extent)
plot(NFI_MODIS250m_kNN_Structure_Volume_Merch_v0)
fc_cover <- readGDAL("Forest_Cov.tif", silent = TRUE)

#create SK extent raster
sk_extent <- raster(nrows = 43170, ncols=24200, 
                    xmn = -1095793.4762, xmx = -369793.4762, 
                    ymn = 14227.9315, ymx = 1309327.9315, 
                    crs = "+proj=lcc +datum=NAD83 +units=m 
                    +lat_1=49 +lat_2=77 +lon_0=-95 +lat_0=49",
                    resolution = 30)
#populate sk_extent with number of pixels as value
values(sk_extent) <- 1:ncell(sk_extent)
#populate all sk_extent pixels with a value of 1
a <- 1
values(sk_extent) <- 1:ncell(a)

#write raster to disk
writeRaster(sk_extent, filename = "sk_extent2", format="GTiff")

#convert sk_extent raster to extent polygon
sk_extent_poly <- rasterToPolygons(sk_extent, fun=NULL, n=4, digits=2, dissolve=FALSE) 

inMemory(sk_extent)
#check structure of spatial data
str(sk_extent)
res(sk_extent)
dim(sk_extent)
extent(sk_extent)
sk_extent
plot(sk_extent)
