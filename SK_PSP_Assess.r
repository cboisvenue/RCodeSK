#__________________________
#
# PSP data Saskatchewan
#
# There are 3 versions of the SK PSP data: raw, ascii, L3
# The purpose of this analysis is to determine 1) what is the best to use and 
# 2) get the location of all possible PSP to JW regardless of what version of the data we end-up using
#
# info to get to JW: shape file of all PSP location, and plot frequency per year of measurement
#
# CBoisvenue
# SEpt.26th, 2014
#__________________________

#install.packages("plyr")
require(plyr)
#install.packages("ggplot2")
require(ggplot2)
#install.packages("dplyr")
require(dplyr)

## RAW DATA SOURCE_____________________________________________________________
#read in all three sources ###NOTE: there is no way (yet?) to link this the L# data
# "raw" plot location from plot_header.xls
sk.raw1 <- read.table("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04/plot_header.csv", header=TRUE, sep=",")
loc.raw = select(sk.raw1, LOC_ACCURACY, PLOT_ID,contains("Z13nad83"))
# check in we have plot numbers repeated
dim(loc.raw) #[1] 2048   13
length(unique(loc.raw[,1])) ##NO - GOOD

      # Check location accuracy as per Joanne White's 
      range(loc.raw$LOC_ACCURACY)#[1]     10 800000
      
      loc.acc10 = filter(loc.raw, LOC_ACCURACY <=10) #418
      loc.acc75 = filter(loc.raw, LOC_ACCURACY <80) #569
      

## asked JWhite is she can map a shape easily - if not, the following lines as a start
          # # make a map
          library(rgdal)
          require(maptools)
plots.raw<-SpatialPointsDataFrame(coords = cbind(loc.raw$Z13nad83_e,loc.raw$Z13nad83_n),data = loc.raw, proj4string=CRS("+proj=utm +zone=13"))#
plot(plots.raw)

# loc1 <- SpatialPointsDataFrame(coords = loc.raw[,2:3],data = loc.raw,
          #                                proj4string=CRS("+proj=utm +zone=13"))
          # SpatialPointsDataFrame(coords = loc.raw[,2:3],data = loc.raw,proj4string =  CRS("+proj=utm +zone=13"))

# number of plots per year of measurement from measurement_header.xls
sk.raw2 <- read.table("C:/Celine/Big_data/Data/01_RawFiles/SK/Release_2012-04/measurement_header.csv", header=TRUE, sep=",")
dim(sk.raw2) #[1] 2048   13
no_plots.raw = select(sk.raw2,PLOT_ID,MEASUREMENT_YEAR) %>%
                group_by(MEASUREMENT_YEAR) %>%
                summarise(length(PLOT_ID))

      # Checking the temporal distribution of the loc.acc10 plots
      no_plots.acc10 = inner_join(loc.acc10,select(sk.raw2,PLOT_ID,MEASUREMENT_YEAR)) %>%
                group_by(MEASUREMENT_YEAR) %>%
                summarise(length(PLOT_ID))
      ggplot(data=no_plots.acc10, aes(x=MEASUREMENT_YEAR,y=no_plots.acc10[,2])) + 
        geom_bar(stat="identity") +
        xlab("Measurement Years") +  ylab("No of Plots") +
        ggtitle("Temporal Distribution of PSP - Saskatchewan Raw")
      ## WHere are these 418 plots?
      acc10 = select(loc.acc10,PLOT_ID,contains("Z13nad83"))
      acc10 = unique(acc10)
      plots.acc10<-SpatialPointsDataFrame(coords = cbind(acc10$Z13nad83_e,acc10$Z13nad83_n),data = acc10, proj4string=CRS("+proj=utm +zone=13"))#
      plot(plots.acc10, add=TRUE, color="red",point=0)
      writeSpatialShape(x=plots.acc10,fn = "G:/RES_Work/Work/JoanneWhite/SK_work/Location418PSP")

      # Checking the temporal distribution of the loc.acc75 plots
      no_plots.acc75 = inner_join(loc.acc75,select(sk.raw2,PLOT_ID,MEASUREMENT_YEAR)) %>%
        group_by(MEASUREMENT_YEAR) %>%
        summarise(length(PLOT_ID))
      ggplot(data=no_plots.acc75, aes(x=MEASUREMENT_YEAR,y=no_plots.acc75[,2])) + 
        geom_bar(stat="identity") +
        xlab("Measurement Years") +  ylab("No of Plots") +
        ggtitle("Temporal Distribution of PSP - Saskatchewan Raw")



# GRAPH
ggplot(data=no_plots.raw, aes(x=MEASUREMENT_YEAR,y=no_plots.raw[,2])) + 
  geom_bar(stat="identity") +
  xlab("Measurement Years") +  ylab("No of Plots") +
  ggtitle("Temporal Distribution of PSP - Saskatchewan Raw")


## ASCII DATA SOURCE____________________________________________________
load("C:/Celine/Big_data/Data/02_ProcessingSteps/Rfiles/SKplot.RData")
sk.text = as.data.frame(plot.level)
#sk.ranges = apply(sk.text,2,FUN="range",na.rm=TRUE)
dim(sk.text)
loc.text = select(sk.text,ID_PlotS,Lat,Lon)
 dim(loc.text) #[1] 5313    3
loc.text = unique(loc.text)
 dim(loc.text) #[1] 2048    3
write.table(loc.text,file = "G:/RES_Work/Work/JoanneWhite/SK_work/SK_PSP_LatLon.txt",quote = FALSE,append = FALSE)

# No of plots per year of measurement
no_plots.text = select(sk.text,ID_PlotS,Year) %>%
  group_by(Year) %>%
  summarise(length(ID_PlotS))
# plot the number of plots per year
ggplot(data=no_plots.text, aes(x=Year,y=no_plots.text[,2])) + 
  geom_bar(stat="identity") +
  xlab("Measurement Years") +  ylab("No of Plots") +
  ggtitle("Temporal Distribution of PSP - Saskatchewan")

## L3 DATA SOURCE____________________________________________________
sk.L3 <- read.table("R:/PSP_NationalDataBase/Projects/ForCAT/ByJurisdiction/PSP-NADB_TL_ByJur_SK_R14a.csv",header = TRUE, sep = ",",as.is = TRUE,na.strings = "NaN",skip = 7)
loc.L3 = select(sk.L3,ID_SourceDB,Lat,Lon)
loc.L3 = unique(loc.L3)
dim(loc.L3) #[1] 1286    3 # all unique plots

unique(sk.l3$ID_Source)

no_plots.L3 = select(sk.L3,ID_SourceDB,Year_t0)
no_plots.L3 = unique(no_plots.L3)  %>%
#dim(no_plots.L3) [1] 2971    2 ## isn't this a bit strange?
  group_by(Year_t0) %>%
  summarise(length(ID_SourceDB))
ggplot(data=no_plots.L3, aes(x=Year_t0,y=no_plots.L3[,2])) + 
  geom_bar(stat="identity") +
  xlab("Measurement Years") +  ylab("No of Plots") +
  ggtitle("Temporal Distribution of PSP - Saskatchewan")


# Differences between raw and text? Can't compare to L3 because no link__________________________________

which(no_plots.text-no_plots.raw !=0) # same no_plots

# Checking is the 418 plots with good accuracy are in L3 data ________________________________

# match the "raw" with the "text" via plot number
names(loc.text) = c("PLOT_ID","Lat","Lon")
plots418 <- acc10[,1]
x1 = match(plots418,loc.text[,1])
text418 = loc.text[x1,]

#match the text418 to loc.L3 
L3.418 = left_join(text418,loc.L3, by="Lon" ) 
#### NOT MACTCHES tried by both lat and lon, and by each individually - no matches
#### I suspect that the transformation of lat lon from utm resulted in different 
#### decimals between what I did here and what RH did, hence there is no match.
#### Plots are too close to round lat lon

L3.acc10 = inner_join(acc10.latlon,loc.L3)

### No

acc10.latlon <-SpatialPoints(cbind(acc10$Z13nad83_e,acc10$Z13nad83_n), proj4string=CRS("+proj=utm +zone=13"))
acc10.latlon <- spTransform(acc10.latlon,CRS("+proj=longlat"))
plot(acc10.latlon)
  
### CONCLUSION: JW and CB decided to 1st try developing this growth layer with 418 plots that have
# an assumed accuracy (geolocation) of the 10m or less. That leaves 418 plots. We may expand that 
# if results are poor. L3 data did not keep the plot accuracy field from the raw data. Hence, raw 
# data must be used.

# See SK_418.r for next steps




