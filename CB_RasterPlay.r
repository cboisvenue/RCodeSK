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
# OR
#setwd("H:/saskatchewan/spatialGrowth/casfri/casfri_intermediate")
# NOTE: to load as below, I needed both the raster package an the spatial.tools package
casfri_dom <- raster("casfri_dom.tif")
soil_moist_regime <- raster("soil_moist_regime.tif")
site_productivity_casfri <-raster("site_productivity_casfri.tif")
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

site_productivity_casfri

# the only place that we have strata (and we only have it for the PAFMA) is here:


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

# Read-in a FMAPA raster

# trying to calculate the average biomass per ha (not change in biomass but total biomass) for 
# each year-----------------------------------------------------------------------------------------
setwd("H:/saskatchewan/spatialGrowth/casfri/biomass")
biom1984 <- raster("bio_1984.tif")
avg1984 <- cellStats(biom1984,'mean')
biomassR_list <- list.files()
biomassR_stack <- stack(biomassR_list)
avg.biom1 <-cellStats(biomassR_stack,'mean')
sd.biom1 <- cellStats(biomassR_stack,'sd') # Note: this took a very long time...

library(data.table)
avg.biom2 <- as.data.table(avg.biom1)
sd.biom2 <- as.data.table(sd.biom1)
year <- 1984:2012
avg.biom3 <- as.data.table(cbind(year, avg.biom2, sd.biom2))
write.table(avg.biom3,"H:/saskatchewan/Celine/figures/RasterAvg_SD.txt",sep=",",row.names = FALSE)

# plot
library(ggplot2)
pixel.biom <- ggplot(avg.biom3, aes(year,avg.biom1))
pixel.biom + geom_point(colour="red")+ ylab("Mg/ha") + theme(text = element_text(size=20)) +
  geom_errorbar(aes(ymin=avg.biom1-1.96*sd.biom1,ymax=avg.biom1+1.96*sd.biom1))
# note that I am not sure where the kg/ha got changed to Mg/ha...in the pixel processing...
ggsave("H:/saskatchewan/Celine/figures/Biomass_Range_Rasters.jpeg")
# End of calculating averages on total biomass raster estimates
#----------------------------------------------------------------------------------------------------
#this is where to "not-smoothed" delta biomass rasters are
H:\saskatchewan\spatialGrowth\fmapa\RSGrowth\scaledAge







