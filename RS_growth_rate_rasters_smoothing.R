#-------------------------------------------------------------------------------------------------
# # RS Growth Rate Method Variable generation script

# This script:
# 1-Generates delta biomass from per-year biomass rasters (see RFModel_biomass_spreading.R)
# 2- Smooths out delta_biomass rasters to minimize negative and zero cell values
# 3-Logs the smoothed delta biomass rasters
# 4-Creates per year 'age' and 'log age' rasters (age raster already created in ...age_height_YEAR.R)
# 5-Creates strata raster (DOM species plus site productivity)
# 6-Syncs NA's between all 87 rasters as to eliminate all NA values between years

# Bsmiley
# July 6, 2015 - Updated July 31 2015
#-------------------------------------------------------------------------------------------------

require(spatial.tools)
require(rgdal)
require(rgeos)
require(parallel)
require(raster)
require(snow)

#File Locations-----------------------------------------------------------------------------------
OutName = "fmapa"
years <- (1984:2012)
change_years <- (1985:2012)

biomass_input = paste("H:/saskatchewan/spatialGrowth/",OutName, "/biomass",
                      sep="")

ageinput = paste("H:/saskatchewan/spatialGrowth/",OutName, "/timeVariantRasters/",
                    years[1:29],"/",sep="")

stratainput1 <- paste("H:/saskatchewan/spatialGrowth/",OutName, "/timeInvariantRasters/",sep="")
stratainput2 <- paste("H:/saskatchewan/spatialGrowth/",OutName, "/casfri_intermediate/",sep="")

output_delta_biomass <- paste("H:/saskatchewan/spatialGrowth/",OutName, "/RSGrowth/scaledAge/",
                              sep="")

output_logDelta_biomass <- paste("H:/saskatchewan/spatialGrowth/",OutName, "/RSGrowth/unsmoothed_d_bio/",
                              sep="")

output_SMlog_D_biomass <- paste("H:/saskatchewan/spatialGrowth/",OutName, "/RSGrowth/smoothed_dbio/",
                                 sep="")

outputdir = paste("H:/saskatchewan/spatialGrowth/",OutName, "/RSGrowth/",
                      sep="")

outputdir2 = paste("H:/saskatchewan/spatialGrowth/",OutName, "/RSGrowthSmooth/",
                  sep="")

outputdir_old = paste("H:/saskatchewan/spatialGrowth/",OutName, "/UnscaledRSGrowth/",
                      sep="")

#Calculates delta biomass rasters from per year biomass rasters----------------------------

#create biomass stack
biomass_list <- list.files(biomass_input, full.names=TRUE)
biomass_stack <- stack(biomass_list)

#create subset stack of only first raster (e.g. a-b=diff, 'firstvalue' = 'a' and secondvalue=b)
firstvalue <- stack(biomass_stack[[2:29]])
secondvalue <- stack(biomass_stack[[1:28]])

beginCluster(30)
startTime <- Sys.time()
#Subtract first value (beginning with 1985 biomass) from second value (beginning with 1984 biomass)
delta_bio <- firstvalue - secondvalue

#Reclassify negative delta_biomass values to zero
remove_negs <- c(-Inf, 0, 0)
delta_bio_reclass <- reclassify(delta_bio, remove_negs)

#Export reclassified delta biomass
for(i in (1:max(length(delta_bio_reclass)))){
  writeRaster(delta_bio_reclass[[i]],file.path(paste(
    output_delta_biomass, "/delta_bio_", change_years[[i]], sep="")), format='GTiff',
    datatype='FLT4S', overwrite=TRUE)
}
#log delta biomass
logdelta_bio <- log(delta_bio_reclass)

#Export logged reclassified delta biomass
for(i in (1:max(length(logdelta_bio)))){
  writeRaster(logdelta_bio[[i]],file.path(paste(
    output_logDelta_biomass, "/lg_delta_bio_", change_years[[i]], sep="")), format='GTiff',
    datatype='FLT4S', overwrite=TRUE)
}

#Smooth delta biomass using moving window average---------------------------------------------

#created new delta biomass rasters using a 3-rasters moving window average whereby the year before,
# year of and year after values are averaged. d85 and d12 only use a two-year window, averaging 
#values from the year of and adjacent year
d85 <- overlay(delta_bio_reclass[[1:2]], fun=function(a,b) {return((a+b)/2)})
d86 <- overlay(delta_bio_reclass[[1:3]], fun=function(a,b,c) {return((a+b+c)/3)})
d87 <- overlay(delta_bio_reclass[[2:4]], fun=function(a,b,c) {return((a+b+c)/3)})
d88 <- overlay(delta_bio_reclass[[3:5]], fun=function(a,b,c) {return((a+b+c)/3)})
d89 <- overlay(delta_bio_reclass[[4:6]], fun=function(a,b,c) {return((a+b+c)/3)})
d90 <- overlay(delta_bio_reclass[[5:7]], fun=function(a,b,c) {return((a+b+c)/3)})
d91 <- overlay(delta_bio_reclass[[6:8]], fun=function(a,b,c) {return((a+b+c)/3)})
d92 <- overlay(delta_bio_reclass[[7:9]], fun=function(a,b,c) {return((a+b+c)/3)})
d93 <- overlay(delta_bio_reclass[[8:10]], fun=function(a,b,c) {return((a+b+c)/3)})
d94 <- overlay(delta_bio_reclass[[9:11]], fun=function(a,b,c) {return((a+b+c)/3)})
d95 <- overlay(delta_bio_reclass[[10:12]], fun=function(a,b,c) {return((a+b+c)/3)})
d96 <- overlay(delta_bio_reclass[[11:13]], fun=function(a,b,c) {return((a+b+c)/3)})
d97 <- overlay(delta_bio_reclass[[12:14]], fun=function(a,b,c) {return((a+b+c)/3)})
d98 <- overlay(delta_bio_reclass[[13:15]], fun=function(a,b,c) {return((a+b+c)/3)})
d99 <- overlay(delta_bio_reclass[[14:16]], fun=function(a,b,c) {return((a+b+c)/3)})
d00 <- overlay(delta_bio_reclass[[15:17]], fun=function(a,b,c) {return((a+b+c)/3)})
d01 <- overlay(delta_bio_reclass[[16:18]], fun=function(a,b,c) {return((a+b+c)/3)})
d02 <- overlay(delta_bio_reclass[[17:19]], fun=function(a,b,c) {return((a+b+c)/3)})
d03 <- overlay(delta_bio_reclass[[18:20]], fun=function(a,b,c) {return((a+b+c)/3)})
d04 <- overlay(delta_bio_reclass[[19:21]], fun=function(a,b,c) {return((a+b+c)/3)})
d05 <- overlay(delta_bio_reclass[[20:22]], fun=function(a,b,c) {return((a+b+c)/3)})
d06 <- overlay(delta_bio_reclass[[21:23]], fun=function(a,b,c) {return((a+b+c)/3)})
d07 <- overlay(delta_bio_reclass[[22:24]], fun=function(a,b,c) {return((a+b+c)/3)})
d08 <- overlay(delta_bio_reclass[[23:25]], fun=function(a,b,c) {return((a+b+c)/3)})
d09 <- overlay(delta_bio_reclass[[24:26]], fun=function(a,b,c) {return((a+b+c)/3)})
d10 <- overlay(delta_bio_reclass[[25:27]], fun=function(a,b,c) {return((a+b+c)/3)})
d11 <- overlay(delta_bio_reclass[[26:28]], fun=function(a,b,c) {return((a+b+c)/3)})
d12 <- overlay(delta_bio_reclass[[27:28]], fun=function(a,b) {return((a+b)/2)})

smooth_d_bio <- stack(d85,d86,d87,d88,d89,
                      d90,d91,d92,d93,d94,d95,d96,d97,d98,d99,
                      d00,d01,d02,d03,d04,d05,d06,d07,d08,d09,
                      d10,d11,d12)

#Remove negatives again from window averaged delta biomass rasters
remove_negs <- c(-Inf, 0, 0)
smoooth_d_bio_recl <- reclassify(smooth_d_bio, remove_negs)

# log the smoothed delta biomass rasters
log_smooth_dbio <- log(smoooth_d_bio_recl)

# export the logged smoothed delta biomass rasters
for(i in (1:max(length(log_smooth_dbio)))){
  writeRaster(log_smooth_dbio[[i]],file.path(paste(
    output_SMlog_D_biomass, "/lg_SMdelta_bio_", change_years[[i]], sep="")), format='GTiff',
    datatype='FLT4S', overwrite=TRUE)
}

#End of export of logged smooth delta biomas----------------------------------------------------
endTime <- Sys.time()
elapsedTime_delta_biomass <- endTime - startTime
# End of generating delta biomass rasters----------

#Import per year age rasters already created-----------------------------------------------
beginCluster(30)
startTime <- Sys.time()
# create stack of 29 age rasters
age_stack <- stack(paste(ageinput,"age1.tif",sep="/"))

logAge_stack <- log(age_stack)

#NO LONGER SCALING VARIABLES
#scale_age <- scale(age_stack)
# create stack of 29 log age rasters
#scale_logAge <- scale(logAge_stack)

#write age and log age rasters to file

for(i in (1:max(length(scale_age)))){
  writeRaster(age_stack[[i]],file.path(paste(output_SMlog_D_biomass, years[[i]],"age", sep="")), 
              format='GTiff',datatype='INT2U', overwrite=TRUE)
}

for(i in (1:max(length(logAge_stack)))){
  writeRaster(logAge_stack[[i]],file.path(paste(output_SMlog_D_biomass, years[[i]],"logAge", sep="")), 
              format='GTiff',datatype='FLT4S', overwrite=TRUE)
}

endTime <- Sys.time()
elapsedTime_age <- endTime - startTime
#End of create age/log(age) rasters--------------------------------------------------------

#Begin create Strata raster---------------------------------------------------------------

casfri_dom <- raster(paste(stratainput1, "casfri_dom.tif", sep=""))
site_productivity <- raster(paste(stratainput2, "site_productivity.tif", sep=""))

#multiple speices codes by 10
casfri_dom10 <- casfri_dom*10

#Add species codes to site productivity codes to get unique strata values
strata <- casfri_dom10 + site_productivity

writeRaster(strata, file.path(paste(output_SMlog_D_biomass, "strata",sep="")),
            format='GTiff', bylayer=TRUE, datatype='INT2U', overwrite=TRUE)

#End of create strata raster---------------------------------------------------------------


#Synced NAs for Growth df input-----------------------------------------------------------------
stackAll_list <- list.files(output_SMlog_D_biomass, full.names = TRUE)

outSync <- (paste("H:/saskatchewan/spatialGrowth/",OutName, "/RSGrowth/smooth_NAsync/",
                  sep=""))

stackAll <- stack(stackAll_list)
# THIS LINE TAKES AWAY ALL NA VALUES THROUGH THE STACK####################################
stackAll_sync <- mask(stackAll, (calc(stackAll,fun=sum)))

ages <- subset(stackAll_sync, grep("*age", names(stackAll_sync), value=TRUE))
year_ages <- 1984:2012

for(i in (1:max(length(ages)))){
  writeRaster(ages[[i]],file.path(paste(outSync, year_ages[[i]], "age", sep ="")), 
              format='GTiff',datatype='INT2U', overwrite=TRUE)
}

log_ages <- subset(stackAll_sync, grep("log*", names(stackAll_sync), value=TRUE))

for(i in (1:max(length(log_ages)))){
  writeRaster(log_ages[[i]],file.path(paste(outSync, year_ages[[i]], "logAge", sep="")), 
              format='GTiff',datatype='FLT4S', overwrite=TRUE)
}

strata <- subset(stackAll_sync, grep("strata", names(stackAll_sync), value=TRUE))

writeRaster(strata,file.path(paste(outSync, "strata", sep="")), 
              format='GTiff',datatype='INT2U', overwrite=TRUE)

log_dbio <- subset(stackAll_sync, grep("*bio", names(stackAll_sync), value=TRUE))

for(i in (1:max(length(log_dbio)))){
  writeRaster(log_dbio[[i]],file.path(paste(outSync,  "lg_SMdelta_bio_",change_years[[i]], sep="")), 
              format='GTiff',datatype='FLT4S', overwrite=TRUE)
}
# End of Sync NAs for Growth raster parameter comparision and table generation---------------------

