# playing with rasters to learn
# CBoisvenue
# September 14th, 2015
#-----------------------------------------------

# figuring out what packages do what
require(spatial.tools)
# require(rgdal)
# require(rgeos)
# require(parallel)
require(raster)

# what are the values in these rasters?
setwd("H:/saskatchewan/spatialGrowth/fmapa/casfri_intermediate")
# NOTE: to load as below, I needed both the raster package an the spatial.tools package
casfri_dom <- raster("casfri_dom.tif")
soil_moist_regime <- raster("soil_moist_regime.tif")

# to see the values just type the name of the raster in
casfri_dom
# gives:
# class       : RasterLayer 
# dimensions  : 6946, 10064, 69904544  (nrow, ncol, ncell)
# resolution  : 30, 30  (x, y)
# extent      : -857443.5, -555523.5, 546937.9, 755317.9  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
# data source : H:\saskatchewan\spatialGrowth\fmapa\casfri_intermediate\casfri_dom.tif 
# names       : casfri_dom 
# values      : 1, 10  (min, max)

soil_moist_regime
# class       : RasterLayer 
# dimensions  : 6946, 10064, 69904544  (nrow, ncol, ncell)
# resolution  : 30, 30  (x, y)
# extent      : -857443.5, -555523.5, 546937.9, 755317.9  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
# data source : H:\saskatchewan\spatialGrowth\fmapa\casfri_intermediate\soil_moist_regime.tif 
# names       : soil_moist_regime 
# values      : 1, 6  (min, max)

reclass1_dom <-raster("H:/saskatchewan/spatialGrowth/fmapa/timeInvariantRasters/casfri_dom.tif")
reclass1_moist <- raster("H:/saskatchewan/spatialGrowth/fmapa/timeInvariantRasters/soil_moist_reg.tif")
site_prod <- raster("site_productivity.tif")

# check values for the casfri area rasters (as opposed to the fmspa above)
setwd("H:/saskatchewan/spatialGrowth/casfri/casfri_intermediate")
casfri_dom <- raster("casfri_dom.tif")
# values [1:13]
soil_moist_regime <- raster("soil_moist_regime.tif")
# values [1:6]
reclass1_dom <-raster("H:/saskatchewan/spatialGrowth/casfri/timeInvariantRasters/casfri_dom.tif")
# values 1:7
reclass1_moist <- raster("H:/saskatchewan/spatialGrowth/casfri/timeInvariantRasters/soil_moist_reg.tif")
# values 1:6

strata <- raster("H:/saskatchewan/spatialGrowth/fmapa/RSGrowth/smoothed_dbio/strata.tif")

# check KO's site productivity
casfri_site_prod1 <- raster("H:/saskatchewan/kangakola/site_productivity.tif")
casfri_site_prod2 <- raster("H:/saskatchewan/kangakola/OutPut/site_productivity_casfri.tif")
