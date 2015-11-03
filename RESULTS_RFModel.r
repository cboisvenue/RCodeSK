#----------------------------------------------------------------------
# Results summary for RF model used to estimate biomass in each pixel
#
# CBoisvenue
# October 20, 2015
#---------------------------------------------------------------------

library(randomForest)

load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/biomass_spread/RFModel3.RData")

varImpPlot(rf.mix1)
rf.mix1
