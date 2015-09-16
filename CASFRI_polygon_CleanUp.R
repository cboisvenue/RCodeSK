#----------------------------------------------------------------------
# Growth Raster project
# CASFRI - Canada's Forest Resource Inventory
# Data clean-up for tables used to join to CASFRI polygons in Saskatchewan
#
# Starting with 5 attribute tables provided by SCumming
# Will create:
# 1) casfri_sps (dom, and sps proportion, 1st 5 species have proportions)
# 2) height for the photo year
# 3) Age-relevant variables for age calculation
# 4) productivity indicator data frame
#
# May 2015
# CBoisvenue & BSmiley
#---------------------------------------------------------------------

indir = "H:/saskatchewan/data/originals/CASFRI/"
outdir = "H:/saskatchewan/data/reference/"


library(plyr)
library(dplyr)
library(reshape2)

# Read-in the 5 CASFRI attribute tables-------------------------------------------------

cas <- read.table(paste(indir,"sk_cas.csv",sep=""), header=TRUE,sep=",")
dst <- read.table(paste(indir,"sk_dst.csv",sep=""), header=TRUE,sep=",")
eco <- read.table(paste(indir,"sk_eco.csv",sep=""), header=TRUE,sep=",")
lyr <- read.table(paste(indir,"sk_lyr.csv",sep=""), header=TRUE,sep=",")
nfl <- read.table(paste(indir,"sk_nfl.csv",sep=""), header=TRUE,sep=",")

# remove all variables that will not be useful
cas1 <- select(cas,cas_id,photo_year,gis_area)
lyr1 <- select(lyr,cas_id,contains("species_"),contains("height_"),contains("origin_"))
lyr1 <- lyr1[,c(1:11,22:25)]
nfl1 <- select(nfl,cas_id,soil_moist_reg,nat_non_veg,non_for_anth,non_for_veg)
dst1 <-select(dst,cas_id,dist_1, dist_yr_1, dist_ext_upper_1, dist_ext_lower_1)
eco1 <- select(eco,cas_id,wetland_type, wet_veg_cover, wet_landform_mod)

# CASFRI photo_year and height ---------------------------------------------------------
ht1 <- lyr1[,c(1,12,13)]
ht2 <- filter(ht1,height_upper>0)
diff1 <- mutate(ht2,ht = round(((height_upper-height_lower)/2)+height_lower))
casfri_height <- select(diff1, cas_id,ht)
casfri_year <- cas1[,1:2]

# END CASFRI height --------------------------------------------------------------------

# CASFRI species -----------------------------------------------------------------------
# [1] "Abie bals" "Betu papy" "Lari lari" "Pice glau" "Pice mari" "Pinu bank"
# [7] "Popu balb" "Popu trem" "XXXX MISS"
  l1 <- lyr1[,c(1:3)]
  l2 <- lyr1[,c(1,4,5)]
  l3 <- lyr1[,c(1,6,7)]
  l4 <- lyr1[,c(1,8,9)]
  l5 <- lyr1[,c(1,10,11)]
names(l1)=c("cas_id","species","percent")
names(l2)=c("cas_id","species","percent")
names(l3)=c("cas_id","species","percent")
names(l4)=c("cas_id","species","percent")
names(l5)=c("cas_id","species","percent")

lyr2 <- rbind(l1,l2,l3,l4,l5)
ck1 <- filter(lyr2,species=="XXXX MISS")
ck2 <- filter(ck1,percent!=0)
sps1 <- filter(lyr2,species!="XXXX MISS") ## whats == vs = here?

sps2 <- dcast(sps1,cas_id~species,value.var="percent")
sps3 <-transmute(lyr1,cas_id=cas_id,casfri_dom = species_1)
sps4 <- left_join(sps3,sps2)
casfri_sps <- filter(sps4,casfri_dom!="XXXX MISS")
# replace all NAs with 0
casfri_sps[is.na(casfri_sps)] <- 0

# END CASFRI sps ---------------------------------------------------------------------

# CASFRI year of origin -------------------------------------------------------------------

# retreive the year of origin
or1 <- select(lyr1,cas_id,contains("origin_"))
# get rid of the negatives
or2 <- filter(or1,origin_upper>0) # we loose 300531 lines
# pulling the year of dist from a normal distribution with a sd of 1 
# (so we don't have negatives) and the mean of origin_lower+4.5
to.add <- rnorm(dim(or2)[[1]],mean=4.5,sd=1)
or3 <- transmute(or2,cas_id=cas_id,dist_yr = round(origin_lower+to.add))

# check if looking at the disturbance year adds any info
# only clearcuts (CO) and burns (BU)
# there is only one disturbance recorded in these plots (dist_2 does not have any values)
# remember that the extent of the polygon may be much more than the plots, so these 
# disturbances may not apply to the plots

dist1 <- select(dst1,cas_id,dist_yr_1) %>%
  filter(dist_yr_1>0)

check_dist1 <- left_join(or3,dist1) %>%
  mutate(diffdist = dist_yr_1-dist_yr)
check_dist2 <- filter(check_dist1, diffdist>10)
# what was the extend of the disturbance?
check_dist3 <- left_join(check_dist2,dst1)
# what is the size of the polygon?
check_dist4 <- left_join(check_dist3,cas1) %>%
  mutate(plot_perc_area = 0.0809/gis_area)

# CONCLUSION: all plots that are less then 17% of the are of the polygon disturbed
# and all polygons are disturbed between 30 and 70%. Further, some plots have a
# disturbance already assigned to them in the "origin" variable in the lyr dataframe 
# provided - so NOT taking into concideration the dist year only the origin yr

casfri_origin <- or3
  
# END year of origin -------------------------------------------------------------------

# Productivity: see if it adds any info to the biomass model ------------------------------

lyr3 <- select(lyr,cas_id,soil_moist_reg,unproductive_for) %>%
  filter(soil_moist_reg!="-1111" | unproductive_for!="-1111")
# each plot in lyr has one or the other

# are there plots that are both in the lyr and in the nfl? 
check_prod1 <- inner_join(lyr3,nfl1)
# no

# are there plots that are both in the lyr and in the eco? 
check_prod2 <- inner_join(lyr3,eco1)
# 217394 plots are in both

# plots in nfl and eco?
check_prod3 <- inner_join(nfl1,eco1)
# 257576 plots are in both

# one var for lyr3
lyr3[which(lyr3$unproductive_for=="TM"),2] <- "W"
lyr3[which(lyr3$unproductive_for=="TR"),2] <- "D"
lyr4 <- select(lyr3, cas_id,soil_moist_reg)

check_prod4 <-filter(nfl1,nat_non_veg!="-1111")
unique(check_prod4$soil_moist_reg)
# in nfl1 there are no values in the soil_moist_reg if the nat_non_veg has a value
# so change soil_moist_reg to "A" if there is a values in nat_non_veg (values are "LA" or "FL")
#NOT SURE ABOUT THIS............

check_prod5 <-filter(nfl1,non_for_anth!="-1111")
unique(check_prod5$soil_moist_reg)
# there are values here that have a soil_moist_reg
levels(nfl1$soil_moist_reg) <- c("-1111","D","F","M","W","A")
nfl1[which(nfl1$soil_moist_reg=="-1111" & (nfl1$nat_non_veg=="LA" |nfl1$nat_non_veg=="FL")),2] <-"A"
nfl1[which(nfl1$soil_moist_reg=="-1111" & (nfl1$non_for_anth=="ST" |nfl1$non_for_anth=="OM")),2] <- "W"
nfl2 <- select(nfl1,cas_id, soil_moist_reg)

casfri_moist <- rbind(lyr4,nfl2)

# see if we can get any info from eco
check_prod6 <- filter(casfri_moist,soil_moist_reg=="-1111")
check_prod6$cas_id %in% eco1$cas_id
# no, no overlap ##MAYBE????
# END of productivity (moist_regime) ---------------------------------------

# Clean-up and save the dataframes ----------------------------------------------------------------------

# photo_year is the only var with 2038
casfri_clean_table <- left_join(casfri_year,casfri_origin) %>%
  left_join(casfri_moist) %>%
  left_join(casfri_height) %>%
  left_join(casfri_sps)

casfri_clean_table <- read.table(paste(outdir,"casfri_cleaned.txt",sep=""), header=TRUE,sep=",")
#Join CASFRI table attributes to polygons---------------------------------------------------------------

#Spatial packages

require(rgdal)
require(raster)
require(parallel)

spatialindir <- ("H:/saskatchewan/data/originals/")
spatialoutdir <- ("H:/Saskatchewan/bsmiley_work/Sask/")
setwd(spatialindir)

casfri_polygons <- readOGR(dsn = "CASFRI", layer = "casfri_poly")

#Join spatial polygons to attribute data
beginCluster(30)
casfri_joined <- merge(casfri_polygons, casfri_clean_table, by.x="CAS_ID", by.y="cas_id", all.x=TRUE)
# 20 table records cannot be matched to polygons

# End of CASFRI table attribute to polygon join--------------------------------------------------------

#Select CASFRI polygons within study area -------------------------------------------------------------
setwd("H:/saskatchewan/data")

### from pspu.shp Max used for wall-to-wall. I selected just Sask polygons and made a new file
Sask_area <- readOGR(dsn = "reference", layer = "Sask_scape_reproj") # add SK vector Sask province

# reproject Sask_area to Recliner inputs
Sask_area <- spTransform(Sask_area, crs(casfri_joined))

# Change AdminBoundary based on area to be clipped by----------------------
fmapa <- Sask_area[Sask_area$AdminBou_1 == 'WCL Prince Albert FMA',]

#Only select CASFRI polygons that overlap with Prince Albert FMA
CASFRI_fmapa <- intersect(fmapa, casfri_joined)

#End of select CASFRI polygons in study area (Prince Albert FMA)

# Write CASFRI SpatialPolygonsDataFrame to file

setwd("H:/saskatchewan/data")
writeOGR(casfri_joined, dsn="reference", layer="casfri_joined2", driver="ESRI Shapefile")
## shapefile limitations result in some field names being abbreviated after joining and exporting

# use Arc ModelBuilder in ArcGIS to rasterize each variable as the rasterize function in ArcGIS
# is MUCH faster than in R

#Location of ModelBuilder Model: M:\Spatially_explicit\01_Projects\07_SK_30m\Working\biomass\CASFRI.tbx

#End of spatial CASFRI data cleanup--------------------------------------------------------------