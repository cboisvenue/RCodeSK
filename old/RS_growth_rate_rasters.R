#-------------------------------------------------------------------------------------------------
# # RS Growth Rate Method Variable generation script

# This script:
# 1-Generates delta biomass from per-year biomass rasters (see RFModel_biomass_spreading.R)
# 2- Smooth out delta_biomass rasters to minimize negative and zero cell values
# 3-Logs the smoothed delta biomass rasters
# 4-Creates per year 'age' and 'log age' rasters (age raster already created in ...age_height_YEAR.R)
# 5-Creates strata raster (DOM species plus site productivity)

# Bsmiley
# July 6, 2015
#-------------------------------------------------------------------------------------------------

require(spatial.tools)
require(rgdal)
require(rgeos)
require(parallel)
require(raster)

#File Locations-----------------------------------------------------------------------------------
OutName = "fmapa"
years <- (1984:2012)
change_years <- (1985:2012)
inputdir = paste("H:/saskatchewan/spatialGrowth/",OutName, "/timeVariantRasters/",
                    years[1:29],"/",sep="")

outputdir = paste("H:/saskatchewan/spatialGrowth/",OutName, "/RSGrowth/",
                      sep="")

outputdir_old = paste("H:/saskatchewan/spatialGrowth/",OutName, "/UnscaledRSGrowth/",
                      sep="")

#Calculates delta biomass rasters from per year biomass rasters----------------------------

biomass_input = paste("H:/saskatchewan/spatialGrowth/",OutName, "/biomass",
                      sep="")

#create biomass stack
biomass_list <- list.files(biomass_input, full.names=TRUE)
biomass_stack <- stack(biomass_list)

beginCluster(30)
#create subset stack of only first raster (e.g. a-b=diff, 'firstvalue' = 'a' and secondvalue=b)
firstvalue <- stack(biomass_stack[[2:29]])
secondvalue <- stack(biomass_stack[[1:28]])

beginCluster(30)
startTime <- Sys.time()
#Subtract first value (beginning with 1985 biomass) from second value (beginning with 1984 biomass)
delta_bio <- firstvalue - secondvalue

remove_negs <- c(-Inf, 0, 0)
delta_bio_reclass <- reclassify(delta_bio, remove_negs)

logdelta_bio <- log(delta_bio_reclass)

#write rasters to file (error here but it still writes fine... I think...)
for(i in (1:max(length(logdelta_bio)))){
  writeRaster(logdelta_bio[[i]],file.path(paste(outputdir, "/lg_delta_bio_", change_years[[i]], sep="")), 
              format='GTiff',datatype='FLT4S', overwrite=TRUE)
}

for(i in (1:max(length(delta_bio_reclass)))){
  writeRaster(delta_bio_reclass[[i]],file.path(paste(outputdir_old, "/delta_bio_", change_years[[i]], sep="")), 
              format='GTiff',datatype='INT2U', overwrite=TRUE)
}

endTime <- Sys.time()
elapsedTime_delta_biomass <- endTime - startTime
# End of generating delta biomass rasters (note: negative values are permissible)----------

#Import per year age rasters already created-----------------------------------------------
beginCluster(30)
startTime <- Sys.time()
# create stack of 29 age rasters
age_stack <- stack(paste(inputdir,"age1.tif",sep="/"))

scale_age <- scale(age_stack)
# create stack of 29 log age rasters

logAge_stack <- log(age_stack)

scale_logAge <- scale(logAge_stack)

#write age and log age rasters to file

for(i in (1:max(length(scale_age)))){
  writeRaster(age_stack[[i]],file.path(paste(outputdir, years[[i]],"age", sep="")), 
              format='GTiff',datatype='INT2U', overwrite=TRUE)
}

for(i in (1:max(length(scale_logAge)))){
  writeRaster(logAge_stack[[i]],file.path(paste(outputdir, years[[i]],"logAge", sep="")), 
              format='GTiff',datatype='FLT4S', overwrite=TRUE)
}

endTime <- Sys.time()
elapsedTime_age <- endTime - startTime
#End of create age/log_age rasters--------------------------------------------------------

#Begin create Strata raster---------------------------------------------------------------

stratainput1 <- paste("H:/saskatchewan/spatialGrowth/",OutName, "/timeInvariantRasters/",sep="")
stratainput2 <- paste("H:/saskatchewan/spatialGrowth/",OutName, "/casfri_intermediate/",sep="")

casfri_dom <- raster(paste(stratainput1, "casfri_dom.tif", sep=""))
site_productivity <- raster(paste(stratainput2, "site_productivity.tif", sep=""))

#multiple speices codes by 10
casfri_dom10 <- casfri_dom*10

#Add species codes to site productivity codes to get unique strata values
strata <- casfri_dom10 + site_productivity

writeRaster(strata, file.path(paste(outputdir, "strata",sep="")),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)

#End of create strata raster---------------------------------------------------------------

#MOVING WINDOW FUNCTION-----------------------------------------------------------------------

rasfun <- function(x=rascube, win=2, outdir=getwd()){
  
  #Sanity check
  if(!(length(raspaths)/win)%%1==0){stop("Number of rasters must be divisible by window size")}
  
  #Create ```mat``` , an index that determines how rasters in ```rascube``` are aggregated: 
  #indices in the same row refer to rasters to be averaged into the ith output.
  
  mat <- matrix(data=1:length(raspaths), ncol=win, byrow=T)
  
  #Loop over ```rascube```, calculating the moving average as controlled by ```mat```
  
  for (i in 1:nrow(mat)){
    
    #Compute ith moving mean, You can alter this to compute a moving "whatever you like"
    #Note the usage of ```[[ ]]``` to subset raster bands: see ```raster``` docu.
    #Also Note the usage of ```na.rm=T```, just in case your images have NA's you dont care about
    
    res_i <- sum(x[[ mat[i,1]:mat[i,win] ]], na.rm=T)/win #
    
    #Write output to file: note how output filename is derived from the respective input rasters
    #makes it possible to trace the outputs back to their source rasters.
    
    writeRaster(x=res_i, filename=paste(mat[i,1:win], collapse=""),
                format="GTiff", overwrite=T)
    
  }
}
raspaths <- d_biomass_list
smooth_delta <- rasfun(x=delta_bio, win=7, outdir="moving_mean_rasters/")
