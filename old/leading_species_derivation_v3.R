## Leading species Raster Derivation##

#needed libraries
library(rgdal)
library(sp)
library(raster)
library(snow)
startTime <- Sys.time()
setwd("C:/Users/bsmiley/Documents/Sask_work/Sask/species_prop")
beginCluster(30)
#Extract list of  file names from working directory
filenames <- list.files(pattern=".tif$", full.names=FALSE)
#create raster stack of raster in 'filenames' list
species <- stack(filenames)
#create two rasters 1) the leading species proportion raster (% compostion for each cell (<1)) and
# 2) leading species layer where cell value equals raster with highest proportion according to:

#[1] "predictBETUPAP2_PROJ_CLIP.tif" 
#[2] "predictLARILAR3_PROJ_CLIP.tif"
#[3] "predictPICEGLA2_PROJ_CLIP.tif"
#[4] "predictPICEMAR2_PROJ_CLIP.tif" 
#[5] "predictPINUBAN2_PROJ_CLIP.tif" 
#[6] "predictPOPUTRE2_PROJ_CLIP.tif"

leadSp_prop <- stackApply(species, indices=c(1,1,1,1,1,1), max)
leadSp_layer <- which.max(species)

##Add in unknwn species and non forest classes
setwd("C:/Users/bsmiley/Documents/Sask_work/Sask/SKmask")
mask2 <- raster("mask2_reproj.tif") # add forest-nonforest mask (projection=species projection)
leadSp_layer_newExtent <- extend(leadSp_layer, mask2, value=0) #make species extent=to mask
species_classes<- data.frame(oldclass=c(NA,0:6), newclass=c(7,7,1:6)) # create reclassify table
leadSp_reclass0 <- subs(leadSp_layer_newExtent, species_classes, by="oldclass", which="newclass") #reclassify
# "0" values (unknown species) to "7"
leadSp_layerwMask <- mask(leadSp_reclass0, mask2, maskvalue=0, updatevalue=0) # mask out non-forest
# areas with species to equal "0" non-forest
setwd("C:/Users/bsmiley/Documents/Sask_work/Canada_concept")
Sask_polys <-readOGR(dsn="layers", layer= "Sask_scape_reproj")
Sask_polys <- spTransform(Sask_polys, crs(leadSp_layerwMask))
leadSp_crop <- crop(leadSp_layerwMask,Sask_polys)

leadSp_crop <- mask(leadSp_layer, Sask_polys, updatevalue=NA)

leadSp_crop <- SpatialPixelsDataFrame(leadSp_crop)
leadSp_crop <- !is.na(overlay(leadSp_crop, Sask_polys))

#export Rasters (proportion raster = signed floating, leading species raster = unsigned integer)
setwd("C:/Users/bsmiley/Documents/Sask_work/Sask/Leading_species")
writeRaster(leadSp_prop, "leadSp_prop_final.tif", format= "GTiff", datatype="FLT4S")
writeRaster(leadSp_crop, "leadSp_layer_recrop_final_test.tif", format= "GTiff", datatype="INT1U")

endTime <- Sys.time()
elapsedTime <- endTime - startTime

##########################################################################################