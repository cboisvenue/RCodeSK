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
leadSp_laterwMask3 <- mask(leadSp_reclass0, mask2, maskvalue=0, updatevalue=0) # mask out non-forest
# areas with species to equal "0" non-forest
Sask30new <- raster("Sask30_new.tif")
leadSp_wMask_reproj <- projectRaster(leadSp_laterwMask3, crs="+proj=lcc +lat_1=49 +lat_2=77
                                     +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80
                                     +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
leadSp_crop <- leadSp_wMask_reproj*Sask30new # crop to Saskatchewan borders

#export Rasters (proportion raster = signed floating, leading species raster = unsigned integer)
setwd("C:/Users/bsmiley/Documents/Sask_work/Sask/Leading_species")
writeRaster(leadSp_prop, "leadSp_prop.tif", format= "GTiff", datatype="FLT4S")
writeRaster(leadSp_crop, "leadSp_layer_reclass_v2.tif", format= "GTiff", datatype="INT1U")

endTime <- Sys.time()
elapsedTime <- endTime - startTime

#####WORKING#####################################################################################



leadSp_wMask2 <- cover(leadSp_layer, mask2)



writeRaster(mask2, "mask2_reproj.tif", format= "GTiff", datatype="INT1U")




leadSp_layer2 <- crop(leadSp_layer, mask2)
leadSp_wMask <- mask(leadSp_layer, mask3, maskvalue=0)


leadSp_2 <- merge(leadSp_layer2,mask2, overlap=TRUE)
leadSp_2_crop <- crop(leadSp_2,leadSp_layer)
leadSp_3_crop <- crop(leadSp_2_crop,leadSp_layer)

beginCluster(30)

setExtent(, mask, keepres=TRUE, snap=TRUE)
setExtent(mask2, leadSp_layer)


writeRaster(leadSp_layer_newExtent, "leadSp_newExtent.tif", format= "GTiff", datatype="INT1U")

extend(leadSp_layer, mask,value=NA)

##OTHER########################################################################################
# CONTROL READ/WRITE BLOCKSIZE 
( tr <-  blockSize(rb1000) )
s <- writeStart(rb1000[[1]], filename="test.tif", format="GTiff", overwrite=TRUE)
for (i in 1:tr$n) {
  v <- getValuesBlock(rb1000, row=tr$row[i], nrows=tr$nrows)     
  writeValues(s, apply(v, MARGIN=1, FUN=which.max), tr$row[i])
}
s <- writeStop(s)
