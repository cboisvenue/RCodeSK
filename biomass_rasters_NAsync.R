#-------------------------------------------------------------------------------------------------
# # RS Growth Rate Method Variable generation script

# This script:
# 1-Loads biomass rasters for CASFRI that are created using "RFModel_biomass_spreading.R
# 2-Syncs NA's between all years of biomass rasters  to eliminate all NA values between years

# Bsmiley
# Jan 9, 2016
#-------------------------------------------------------------------------------------------------

require(spatial.tools)
require(rgdal)
require(rgeos)
require(parallel)
require(raster)
require(snow)
library(data.table)
library(dplyr)

#Set temp file... has to have >50gb of room
rasterOptions(tmpdir="H:/Saskatchewan/Sask_tempfiles")
rasterOptions(maxmemory=1e+10)


#File Locations-----------------------------------------------------------------------------------
OutName = "casfri"
years <- (1984:2012)
change_years <- (1985:2012)

biomass_input = paste("H:/saskatchewan/spatialGrowth/",OutName, "/biomass",
                      sep="")

dist_mask_input = paste("H:/saskatchewan/spatialGrowth/",OutName, "/casfri_intermediate",
                      sep="")

biomass_output = paste("H:/saskatchewan/spatialGrowth/",OutName, "/biomass/output/",
                      sep="")
#End of file locations------------------------------------------------------

#Load distubance mask (this was generated in ArcMap from the cumulative footprint of 
#MW and JW disturbance attribution product (1985-2012). After creating the raster it 
#was clipped to the spatial extent of the CASFRI area to conform to the biommas stack
#------------------------------------------------------------------------------------
dist_mask <- raster(paste(dist_mask_input, "distMask_clip.tif", sep = "/"))

#create biomass stack
biomass_list <- list.files(biomass_input, full.names=TRUE, pattern= ".tif$")
biomass_stack <- stack(biomass_list)

beginCluster(30)

#Mask biomass cellscells that overlap disturbed polygons (based off of combining 
#all years of MW and JW's disturbance attribution layer)
mask_disturbedBiomass <- mask(biomass_stack, dist_mask)

#Do stats on masked stack
avg.biomMask <-cellStats(mask_disturbedBiomass,'mean')
sd.bioMask <- cellStats(mask_disturbedBiomass,'sd') # Note: this took a very long time...
freq.bioMask <- freq(mask_disturbedBiomass, useNA= "ifany")

freq.bioMASS <- freq(biomass_stack, useNA= "ifany")
freq.bioMask <- freq.bioMASS

#make data tables of the average and sd for the masked biomass stack
avg.biomMask_DT <- as.data.table(avg.biomMask)
sd.biomMask_DT <- as.data.table(sd.bioMask)

#join and export average biomass and sd for biomass stack
year <- 1984:2012
avg.biom3 <- as.data.table(cbind(year, avg.biomMask_DT, sd.biomMask_DT))

write.table(avg.biom3,paste(biomass_output, "RasterAvg_SD.txt", sep=""),
            sep=",",row.names = FALSE)

#Make datatables for each individual year of frequency for rejoining

freq.biomMask84_DT <- as.data.table(freq.bioMask$bio_1984)
freq.biomMask85_DT <- as.data.table(freq.bioMask$bio_1985)
freq.biomMask86_DT <- as.data.table(freq.bioMask$bio_1986)
freq.biomMask87_DT <- as.data.table(freq.bioMask$bio_1987)
freq.biomMask88_DT <- as.data.table(freq.bioMask$bio_1988)
freq.biomMask89_DT <- as.data.table(freq.bioMask$bio_1989)
freq.biomMask90_DT <- as.data.table(freq.bioMask$bio_1990)
freq.biomMask91_DT <- as.data.table(freq.bioMask$bio_1991)
freq.biomMask92_DT <- as.data.table(freq.bioMask$bio_1992)
freq.biomMask93_DT <- as.data.table(freq.bioMask$bio_1993)
freq.biomMask94_DT <- as.data.table(freq.bioMask$bio_1994)
freq.biomMask95_DT <- as.data.table(freq.bioMask$bio_1995)
freq.biomMask96_DT <- as.data.table(freq.bioMask$bio_1996)
freq.biomMask97_DT <- as.data.table(freq.bioMask$bio_1997)
freq.biomMask98_DT <- as.data.table(freq.bioMask$bio_1998)
freq.biomMask99_DT <- as.data.table(freq.bioMask$bio_1999)
freq.biomMask00_DT <- as.data.table(freq.bioMask$bio_2000)
freq.biomMask01_DT <- as.data.table(freq.bioMask$bio_2001)
freq.biomMask02_DT <- as.data.table(freq.bioMask$bio_2002)
freq.biomMask03_DT <- as.data.table(freq.bioMask$bio_2003)
freq.biomMask04_DT <- as.data.table(freq.bioMask$bio_2004)
freq.biomMask05_DT <- as.data.table(freq.bioMask$bio_2005)
freq.biomMask06_DT <- as.data.table(freq.bioMask$bio_2006)
freq.biomMask07_DT <- as.data.table(freq.bioMask$bio_2007)
freq.biomMask08_DT <- as.data.table(freq.bioMask$bio_2008)
freq.biomMask09_DT <- as.data.table(freq.bioMask$bio_2009)
freq.biomMask10_DT <- as.data.table(freq.bioMask$bio_2010)
freq.biomMask11_DT <- as.data.table(freq.bioMask$bio_2011)
freq.biomMask12_DT <- as.data.table(freq.bioMask$bio_2012)

#create pixel value ID field to join all years to
freq_id <- data.frame("freqID" = (c(50:306, NA)))

#join all years of data by pixel value, renaming count field to specific year
join_freq <- full_join(freq_id, freq.biomMask84_DT, by= c("freqID" = "value")) %>%
  rename(pixelCount84= count) %>%
  full_join(freq.biomMask85_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount85= count) %>%
  full_join(freq.biomMask86_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount86= count) %>%
  full_join(freq.biomMask87_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount87= count) %>%
  full_join(freq.biomMask88_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount88= count) %>%
  full_join(freq.biomMask89_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount89= count) %>%
  full_join(freq.biomMask90_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount90= count) %>%
  full_join(freq.biomMask91_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount91= count) %>%
  full_join(freq.biomMask92_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount92= count) %>%
  full_join(freq.biomMask93_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount93= count) %>%
  full_join(freq.biomMask94_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount94= count) %>%
  full_join(freq.biomMask95_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount95= count) %>%
  full_join(freq.biomMask96_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount96= count) %>%
  full_join(freq.biomMask97_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount97= count) %>%
  full_join(freq.biomMask98_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount98= count) %>%
  full_join(freq.biomMask99_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount99= count) %>%
  full_join(freq.biomMask00_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount00= count) %>%
  full_join(freq.biomMask01_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount01= count) %>%
  full_join(freq.biomMask02_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount02= count) %>%
  full_join(freq.biomMask03_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount03= count) %>%
  full_join(freq.biomMask04_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount04= count) %>%
  full_join(freq.biomMask05_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount05= count) %>%
  full_join(freq.biomMask06_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount06= count) %>%
  full_join(freq.biomMask07_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount07= count) %>%
  full_join(freq.biomMask08_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount08= count) %>%
  full_join(freq.biomMask09_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount09= count) %>%
  full_join(freq.biomMask10_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount10= count) %>%
  full_join(freq.biomMask11_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount11= count) %>%
  full_join(freq.biomMask12_DT, by=c("freqID" = "value")) %>%
  rename(pixelCount12= count)
  

write.table(join_freq,paste(biomass_output, "cellValue_freq_BIOMASSstack.txt", sep=""),
            sep=",",row.names = FALSE)


#End of masked biomass raster stack statistics----------------------------------
