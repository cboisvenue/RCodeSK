#-------------------------------------------------------------------------------------------------
# # Summarize area of Landsat disturbance attribution

# This script:
# 1-This script subsets the individual years of landsat disturbance attribution
# by disturbance type and then populates a dataframe with the area (in hectares) for
#each year (1985-2011)

# Bsmiley
# Jan 18, 2016
#-------------------------------------------------------------------------------------------------

require(spatial.tools)
require(rgdal)
require(rgeos)
require(parallel)
require(raster)
require(snow)
library(data.table)
library(dplyr)
library(ggplot2)


#File Locations-----------------------------------------------------------------------------------
dist_att_input = "I:/PFC_UVicFall2015/data/SK/LandsatStack/"

#End of file locations------------------------------------------------------

CASFRIdistAreas <- data.frame(year=1985:2011,
                        FireArea=numeric(27),
                        HarvestArea=numeric(27),
                        LcondArea=numeric(27),
                        RoadArea=numeric(27),
                        UnclassArea=numeric(27))

# casfriData <- readOGR(paste(casfriData_input, "Working", 
#                         sep=""), "CASFRIDataMask")
# 
# CASFRImodelleddistAreas <- data.frame(year=1985:2011,
#                               FireArea=numeric(27),
#                               HarvestArea=numeric(27),
#                               LcondArea=numeric(27),
#                               RoadArea=numeric(27),
#                               UnclassArea=numeric(27))

startTime <- Sys.time()
#Compute disturbance type areas for 1985 for modelled CASFRI area
dist85 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1985_Classified")
fire85<- subset(dist85, dist85@data$CLASSIFIED == "Fire")
harvest85<- subset(dist85,dist85@data$CLASSIFIED == "Harvesting")
Lcond85<- subset(dist85,dist85@data$CLASSIFIED == "Lcondition")
road85<- subset(dist85,dist85@data$CLASSIFIED == "Road")
unclass85<- subset(dist85,dist85@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[1] <- (gArea(fire85,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[1] <- (gArea(harvest85,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[1] <- (gArea(Lcond85,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[1] <- (gArea(road85,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[1] <- (gArea(unclass85,byid=FALSE) /10000)

endTime <- Sys.time()
elapsedTime <- endTime - startTime

#Compute disturbance type areas for 1986 for modelled CASFRI area
dist86 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1986_Classified")
fire86<- subset(dist86, dist86@data$CLASSIFIED == "Fire")
harvest86<- subset(dist86,dist86@data$CLASSIFIED == "Harvesting")
Lcond86<- subset(dist86,dist86@data$CLASSIFIED == "Lcondition")
road86<- subset(dist86,dist86@data$CLASSIFIED == "Road")
unclass86<- subset(dist86,dist86@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[2] <- (gArea(fire86,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[2] <- (gArea(harvest86,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[2] <- (gArea(Lcond86,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[2] <- (gArea(road86,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[2] <- (gArea(unclass86,byid=FALSE) /10000)

#Compute disturbance type areas for 1987 for modelled CASFRI area
dist87 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1987_Classified")
fire87<- subset(dist87, dist87@data$CLASSIFIED == "Fire")
harvest87<- subset(dist87,dist87@data$CLASSIFIED == "Harvesting")
Lcond87<- subset(dist87,dist87@data$CLASSIFIED == "Lcondition")
road87<- subset(dist87,dist87@data$CLASSIFIED == "Road")
unclass87<- subset(dist87,dist87@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[3] <- (gArea(fire87,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[3] <- (gArea(harvest87,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[3] <- (gArea(Lcond87,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[3] <- (gArea(road87,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[3] <- (gArea(unclass87,byid=FALSE) /10000)

#Compute disturbance type areas for 1988 for modelled CASFRI area
dist88 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1988_Classified")
fire88<- subset(dist88, dist88@data$CLASSIFIED == "Fire")
harvest88<- subset(dist88,dist88@data$CLASSIFIED == "Harvesting")
Lcond88<- subset(dist88,dist88@data$CLASSIFIED == "Lcondition")
road88<- subset(dist88,dist88@data$CLASSIFIED == "Road")
unclass88<- subset(dist88,dist88@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[4] <- (gArea(fire88,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[4] <- (gArea(harvest88,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[4] <- (gArea(Lcond88,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[4] <- (gArea(road88,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[4] <- (gArea(unclass88,byid=FALSE) /10000)

#Compute disturbance type areas for 1989 for modelled CASFRI area
dist89 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1989_Classified")
fire89<- subset(dist89, dist89@data$CLASSIFIED == "Fire")
harvest89<- subset(dist89,dist89@data$CLASSIFIED == "Harvesting")
Lcond89<- subset(dist89,dist89@data$CLASSIFIED == "Lcondition")
road89<- subset(dist89,dist89@data$CLASSIFIED == "Road")
unclass89<- subset(dist89,dist89@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[5] <- (gArea(fire89,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[5] <- (gArea(harvest89,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[5] <- (gArea(Lcond89,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[5] <- (gArea(road89,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[5] <- (gArea(unclass89,byid=FALSE) /10000)

#Compute disturbance type areas for 1990 for modelled CASFRI area
dist90 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1990_Classified")
fire90<- subset(dist90, dist90@data$CLASSIFIED == "Fire")
harvest90<- subset(dist90,dist90@data$CLASSIFIED == "Harvesting")
Lcond90<- subset(dist90,dist90@data$CLASSIFIED == "Lcondition")
road90<- subset(dist90,dist90@data$CLASSIFIED == "Road")
unclass90<- subset(dist90,dist90@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[6] <- (gArea(fire90,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[6] <- (gArea(harvest90,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[6] <- (gArea(Lcond90,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[6] <- (gArea(road90,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[6] <- (gArea(unclass90,byid=FALSE) /10000)

#Compute disturbance type areas for 1991 for modelled CASFRI area
dist91 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1991_Classified")
fire91<- subset(dist91, dist91@data$CLASSIFIED == "Fire")
harvest91<- subset(dist91,dist91@data$CLASSIFIED == "Harvesting")
Lcond91<- subset(dist91,dist91@data$CLASSIFIED == "Lcondition")
road91<- subset(dist91,dist91@data$CLASSIFIED == "Road")
unclass91<- subset(dist91,dist91@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[7] <- (gArea(fire91,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[7] <- (gArea(harvest91,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[7] <- (gArea(Lcond91,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[7] <- (gArea(road91,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[7] <- (gArea(unclass91,byid=FALSE) /10000)

#Compute disturbance type areas for 1992 for modelled CASFRI area
dist92 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1992_Classified")
fire92<- subset(dist92, dist92@data$CLASSIFIED == "Fire")
harvest92<- subset(dist92,dist92@data$CLASSIFIED == "Harvesting")
Lcond92<- subset(dist92,dist92@data$CLASSIFIED == "Lcondition")
road92<- subset(dist92,dist92@data$CLASSIFIED == "Road")
unclass92<- subset(dist92,dist92@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[8] <- (gArea(fire92,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[8] <- (gArea(harvest92,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[8] <- (gArea(Lcond92,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[8] <- (gArea(road92,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[8] <- (gArea(unclass92,byid=FALSE) /10000)

#Compute disturbance type areas for 1993 for modelled CASFRI area
dist93 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1993_Classified")
fire93<- subset(dist93, dist93@data$CLASSIFIED == "Fire")
harvest93<- subset(dist93,dist93@data$CLASSIFIED == "Harvesting")
Lcond93<- subset(dist93,dist93@data$CLASSIFIED == "Lcondition")
road93<- subset(dist93,dist93@data$CLASSIFIED == "Road")
unclass93<- subset(dist93,dist93@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[9] <- (gArea(fire93,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[9] <- (gArea(harvest93,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[9] <- (gArea(Lcond93,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[9] <- (gArea(road93,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[9] <- (gArea(unclass93,byid=FALSE) /10000)

#Compute disturbance type areas for 1994 for modelled CASFRI area
dist94 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1994_Classified")
fire94<- subset(dist94, dist94@data$CLASSIFIED == "Fire")
harvest94<- subset(dist94,dist94@data$CLASSIFIED == "Harvesting")
Lcond94<- subset(dist94,dist94@data$CLASSIFIED == "Lcondition")
road94<- subset(dist94,dist94@data$CLASSIFIED == "Road")
unclass94<- subset(dist94,dist94@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[10] <- (gArea(fire94,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[10] <- (gArea(harvest94,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[10] <- (gArea(Lcond94,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[10] <- (gArea(road94,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[10] <- (gArea(unclass94,byid=FALSE) /10000)

#Compute disturbance type areas for 1995 for modelled CASFRI area
dist95 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1995_Classified")
fire95<- subset(dist95, dist95@data$CLASSIFIED == "Fire")
harvest95<- subset(dist95,dist95@data$CLASSIFIED == "Harvesting")
Lcond95<- subset(dist95,dist95@data$CLASSIFIED == "Lcondition")
road95<- subset(dist95,dist95@data$CLASSIFIED == "Road")
unclass95<- subset(dist95,dist95@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[11] <- (gArea(fire95,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[11] <- (gArea(harvest95,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[11] <- (gArea(Lcond95,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[11] <- (gArea(road95,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[11] <- (gArea(unclass95,byid=FALSE) /10000)

#Compute disturbance type areas for 1996 for modelled CASFRI area
dist96 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1996_Classified")
fire96<- subset(dist96, dist96@data$CLASSIFIED == "Fire")
harvest96<- subset(dist96,dist96@data$CLASSIFIED == "Harvesting")
Lcond96<- subset(dist96,dist96@data$CLASSIFIED == "Lcondition")
road96<- subset(dist96,dist96@data$CLASSIFIED == "Road")
unclass96<- subset(dist96,dist96@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[12] <- (gArea(fire96,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[12] <- (gArea(harvest96,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[12] <- (gArea(Lcond96,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[12] <- (gArea(road96,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[12] <- (gArea(unclass96,byid=FALSE) /10000)

#Compute disturbance type areas for 1997 for modelled CASFRI area
dist97 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1997_Classified")
fire97<- subset(dist97, dist97@data$CLASSIFIED == "Fire")
harvest97<- subset(dist97,dist97@data$CLASSIFIED == "Harvesting")
Lcond97<- subset(dist97,dist97@data$CLASSIFIED == "Lcondition")
road97<- subset(dist97,dist97@data$CLASSIFIED == "Road")
unclass97<- subset(dist97,dist97@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[13] <- (gArea(fire97,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[13] <- (gArea(harvest97,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[13] <- (gArea(Lcond97,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[13] <- (gArea(road97,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[13] <- (gArea(unclass97,byid=FALSE) /10000)

#Compute disturbance type areas for 1998 for modelled CASFRI area
dist98 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1998_Classified")
fire98<- subset(dist98, dist98@data$CLASSIFIED == "Fire")
harvest98<- subset(dist98,dist98@data$CLASSIFIED == "Harvesting")
Lcond98<- subset(dist98,dist98@data$CLASSIFIED == "Lcondition")
road98<- subset(dist98,dist98@data$CLASSIFIED == "Road")
unclass98<- subset(dist98,dist98@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[14] <- (gArea(fire98,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[14] <- (gArea(harvest98,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[14] <- (gArea(Lcond98,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[14] <- (gArea(road98,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[14] <- (gArea(unclass98,byid=FALSE) /10000)

#Compute disturbance type areas for 1999 for modelled CASFRI area
dist99 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id1999_Classified")
fire99<- subset(dist99, dist99@data$CLASSIFIED == "Fire")
harvest99<- subset(dist99,dist99@data$CLASSIFIED == "Harvesting")
Lcond99<- subset(dist99,dist99@data$CLASSIFIED == "Lcondition")
road99<- subset(dist99,dist99@data$CLASSIFIED == "Road")
unclass99<- subset(dist99,dist99@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[15] <- (gArea(fire99,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[15] <- (gArea(harvest99,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[15] <- (gArea(Lcond99,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[15] <- (gArea(road99,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[15] <- (gArea(unclass99,byid=FALSE) /10000)

#Compute disturbance type areas for 2000 for modelled CASFRI area
dist00 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2000_Classified")
fire00<- subset(dist00, dist00@data$CLASSIFIED == "Fire")
harvest00<- subset(dist00,dist00@data$CLASSIFIED == "Harvesting")
Lcond00<- subset(dist00,dist00@data$CLASSIFIED == "Lcondition")
road00<- subset(dist00,dist00@data$CLASSIFIED == "Road")
unclass00<- subset(dist00,dist00@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[16] <- (gArea(fire00,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[16] <- (gArea(harvest00,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[16] <- (gArea(Lcond00,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[16] <- (gArea(road00,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[16] <- (gArea(unclass00,byid=FALSE) /10000)

#Compute disturbance type areas for 2001 for modelled CASFRI area
dist01 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2001_Classified")
fire01<- subset(dist01, dist01@data$CLASSIFIED == "Fire")
harvest01<- subset(dist01,dist01@data$CLASSIFIED == "Harvesting")
Lcond01<- subset(dist01,dist01@data$CLASSIFIED == "Lcondition")
road01<- subset(dist01,dist01@data$CLASSIFIED == "Road")
unclass01<- subset(dist01,dist01@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[17] <- (gArea(fire01,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[17] <- (gArea(harvest01,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[17] <- (gArea(Lcond01,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[17] <- (gArea(road01,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[17] <- (gArea(unclass01,byid=FALSE) /10000)

#Compute disturbance type areas for 2002 for modelled CASFRI area
dist02 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2002_Classified")
fire02<- subset(dist02, dist02@data$CLASSIFIED == "Fire")
harvest02<- subset(dist02,dist02@data$CLASSIFIED == "Harvesting")
Lcond02<- subset(dist02,dist02@data$CLASSIFIED == "Lcondition")
road02<- subset(dist02,dist02@data$CLASSIFIED == "Road")
unclass02<- subset(dist02,dist02@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[18] <- (gArea(fire02,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[18] <- (gArea(harvest02,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[18] <- (gArea(Lcond02,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[18] <- (gArea(road02,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[18] <- (gArea(unclass02,byid=FALSE) /10000)

#Compute disturbance type areas for 2003 for modelled CASFRI area
dist03 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2003_Classified")
fire03<- subset(dist03, dist03@data$CLASSIFIED == "Fire")
harvest03<- subset(dist03,dist03@data$CLASSIFIED == "Harvesting")
Lcond03<- subset(dist03,dist03@data$CLASSIFIED == "Lcondition")
road03<- subset(dist03,dist03@data$CLASSIFIED == "Road")
unclass03<- subset(dist03,dist03@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[19] <- (gArea(fire03,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[19] <- (gArea(harvest03,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[19] <- (gArea(Lcond03,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[19] <- (gArea(road03,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[19] <- (gArea(unclass03,byid=FALSE) /10000)

#Compute disturbance type areas for 2004 for modelled CASFRI area
dist04 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2004_Classified")
fire04<- subset(dist04, dist04@data$CLASSIFIED == "Fire")
harvest04<- subset(dist04,dist04@data$CLASSIFIED == "Harvesting")
Lcond04<- subset(dist04,dist04@data$CLASSIFIED == "Lcondition")
road04<- subset(dist04,dist04@data$CLASSIFIED == "Road")
unclass04<- subset(dist04,dist04@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[20] <- (gArea(fire04,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[20] <- (gArea(harvest04,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[20] <- (gArea(Lcond04,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[20] <- (gArea(road04,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[20] <- (gArea(unclass04,byid=FALSE) /10000)

#Compute disturbance type areas for 2005 for modelled CASFRI area
dist05 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2005_Classified")
fire05<- subset(dist05, dist05@data$CLASSIFIED == "Fire")
harvest05<- subset(dist05,dist05@data$CLASSIFIED == "Harvesting")
Lcond05<- subset(dist05,dist05@data$CLASSIFIED == "Lcondition")
road05<- subset(dist05,dist05@data$CLASSIFIED == "Road")
unclass05<- subset(dist05,dist05@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[21] <- (gArea(fire05,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[21] <- (gArea(harvest05,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[21] <- (gArea(Lcond05,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[21] <- (gArea(road05,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[21] <- (gArea(unclass05,byid=FALSE) /10000)

#Compute disturbance type areas for 2006 for modelled CASFRI area
dist06 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2006_Classified")
fire06<- subset(dist06, dist06@data$CLASSIFIED == "Fire")
harvest06<- subset(dist06,dist06@data$CLASSIFIED == "Harvesting")
Lcond06<- subset(dist06,dist06@data$CLASSIFIED == "Lcondition")
road06<- subset(dist06,dist06@data$CLASSIFIED == "Road")
unclass06<- subset(dist06,dist06@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[22] <- (gArea(fire06,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[22] <- (gArea(harvest06,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[22] <- (gArea(Lcond06,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[22] <- (gArea(road06,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[22] <- (gArea(unclass06,byid=FALSE) /10000)

#Compute disturbance type areas for 2007 for modelled CASFRI area
dist07 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2007_Classified")
fire07<- subset(dist07, dist07@data$CLASSIFIED == "Fire")
harvest07<- subset(dist07,dist07@data$CLASSIFIED == "Harvesting")
Lcond07<- subset(dist07,dist07@data$CLASSIFIED == "Lcondition")
road07<- subset(dist07,dist07@data$CLASSIFIED == "Road")
unclass07<- subset(dist07,dist07@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[23] <- (gArea(fire07,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[23] <- (gArea(harvest07,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[23] <- (gArea(Lcond07,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[23] <- (gArea(road07,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[23] <- (gArea(unclass07,byid=FALSE) /10000)

#Compute disturbance type areas for 2008 for modelled CASFRI area
dist08 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2008_Classified")
fire08<- subset(dist08, dist08@data$CLASSIFIED == "Fire")
harvest08<- subset(dist08,dist08@data$CLASSIFIED == "Harvesting")
Lcond08<- subset(dist08,dist08@data$CLASSIFIED == "Lcondition")
road08<- subset(dist08,dist08@data$CLASSIFIED == "Road")
unclass08<- subset(dist08,dist08@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[24] <- (gArea(fire08,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[24] <- (gArea(harvest08,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[24] <- (gArea(Lcond08,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[24] <- (gArea(road08,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[24] <- (gArea(unclass08,byid=FALSE) /10000)

#Compute disturbance type areas for 2009 for modelled CASFRI area
dist09 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2009_Classified")
fire09<- subset(dist09, dist09@data$CLASSIFIED == "Fire")
harvest09<- subset(dist09,dist09@data$CLASSIFIED == "Harvesting")
Lcond09<- subset(dist09,dist09@data$CLASSIFIED == "Lcondition")
road09<- subset(dist09,dist09@data$CLASSIFIED == "Road")
unclass09<- subset(dist09,dist09@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[25] <- (gArea(fire09,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[25] <- (gArea(harvest09,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[25] <- (gArea(Lcond09,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[25] <- (gArea(road09,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[25] <- (gArea(unclass09,byid=FALSE) /10000)

#Compute disturbance type areas for 2010 for modelled CASFRI area
dist10 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2010_Classified")
fire10<- subset(dist10, dist10@data$CLASSIFIED == "Fire")
harvest10<- subset(dist10,dist10@data$CLASSIFIED == "Harvesting")
Lcond10<- subset(dist10,dist10@data$CLASSIFIED == "Lcondition")
road10<- subset(dist10,dist10@data$CLASSIFIED == "Road")
unclass10<- subset(dist10,dist10@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[26] <- (gArea(fire10,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[26] <- (gArea(harvest10,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[26] <- (gArea(Lcond10,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[26] <- (gArea(road10,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[26] <- (gArea(unclass10,byid=FALSE) /10000)

#Compute disturbance type areas for 2011 for modelled CASFRI area
dist11 <- readOGR(paste(dist_att_input, "CASFRI_NULL_NIR_overlap", 
                        sep=""), "id2011_Classified")
fire11<- subset(dist11, dist11@data$CLASSIFIED == "Fire")
harvest11<- subset(dist11,dist11@data$CLASSIFIED == "Harvesting")
Lcond11<- subset(dist11,dist11@data$CLASSIFIED == "Lcondition")
road11<- subset(dist11,dist11@data$CLASSIFIED == "Road")
unclass11<- subset(dist11,dist11@data$CLASSIFIED == "Unclass")

CASFRIdistAreas$FireArea[27] <- (gArea(fire11,byid=FALSE) /10000)
CASFRIdistAreas$HarvestArea[27] <- (gArea(harvest11,byid=FALSE) /10000)
CASFRIdistAreas$LcondArea[27] <- (gArea(Lcond11,byid=FALSE) /10000)
CASFRIdistAreas$RoadArea[27] <- (gArea(road11,byid=FALSE) /10000)
CASFRIdistAreas$UnclassArea[27] <- (gArea(unclass11,byid=FALSE) /10000)

write.table(CASFRIdistAreas,paste(dist_att_input, 
                                  "CASFRI_NULL_NIR_overlap_LS_distAreas.txt", sep=""),
            sep=",",row.names = FALSE)


endTime <- Sys.time()
elapsedTime <- endTime - startTime

  
