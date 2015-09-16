#Extract cell values from PSP locations

#read in plot coordinate CSV file
PSP <- read.table("PSP_UTM.csv", header=TRUE, sep=",")
head(PSP)

#select just the coordinate fields, creating new dataframe
#make spatialpoints class file from coordinates (spatialpointsdataframe does not work with extract)
PSPxy <- PSP[c("Z13nad83_e", "Z13nad83_n")]
PSP_spatial <- SpatialPoints(PSPxy, proj4string=CRS("+proj=utm +zone=13 +datum=NAD83"))

#add stage age integers (trunkated knn age raster) (250 meters)
Stand_age_Integers1 <- raster("Stand_age_Integers1.tif")
plot(Stand_age_Integers1)

#Reproject PSP xy into lcc
PSP_spatial_lcc <- spTransform(PSP_spatial, CRS("+proj=lcc +datum=NAD83 +units=m 
                    +lat_1=49 +lat_2=77 +lon_0=-95 +lat_0=49"))

#Extract cell age values from PSP locations
cell.values <- extract(Stand_age_Integers1, PSP_spatial_lcc, method= 'simple')
age <- as.data.frame(cell.values)
write.table(age, file = "age.csv", sep = ",")
                     