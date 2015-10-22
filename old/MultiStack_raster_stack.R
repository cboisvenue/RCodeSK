#----------------------------------------------------------------------
# Raster Stack time invariant and time variant rasters
#
# May 11 2015
# CBoisvenue & BSmiley
#Stack time-invariant rasters and time variant rasters separately then stack both stacks together---
#---------------------------------------------------------------------------------------------------
library(raster)

setwd ("H:/Saskatchewan/Biomass_Growth/Time_Invariant_rasters/")
invar.list <- list.files( pattern=".tif$", full.names=FALSE)

setwd ("H:/Saskatchewan/TestingRasters/TrialswDOM/Variant")
var.list <- list.files( pattern=".tif$", full.names=FALSE)
varStack <- stack(var.list)

stackALL <- stack(varStack, InvarStack)

#IGNORE----------------------------------------------------------------

timeVARIANT = "H:/Saskatchewan/TestingRasters/TrialswDOM/Variant"


var.list <- list.files(paste(timeVARIANT), pattern=".tif$", full.names=FALSE)

indir <-"H:/Saskatchewan/Biomass_Growth/Time_Variant_rasters/"
years <- 1984:2012
var.list <- list.files( paste(indir,years[29],sep=""),pattern=".tif$", full.names=FALSE)
setwd(paste(indir,years[29],"/",sep=""))
  
varStack <- stack(var.list)
