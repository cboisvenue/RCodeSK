## Install Celine packages ##


install.packages("bigmemory")
install.packages("rgdal")

library(dplyr)
library(rgdal)
library(sp)
library(raster)
library(gdata)
library(devtools)
library(SpaDES)
library(ff)
library(bigmemory)



??dplyr

install_github("achubaty/SpaDES", build=FALSE)

## TESTING ##

x <- raster()
X<- raster(ncol=36, nrow=18, xmn=-1000, xmx=1000, ymn=-100, ymx=900)
res(x)
plot(x)

#import a Raster
setwd("C:/Users/bsmiley/Documents/Lit_Review/Imagery_Carbon/")

f<- raster("forestcover.tif")

hasValues(f)
Plot(f)

library(SpaDES)
require(SpaDES)
fileList <-
  data.frame(files =
               dir(file.path(
                 find.package("SpaDES",
                              lib.loc=getOption("devtools.path"),
                              quiet=FALSE),
                 "maps"),
                 full.names=TRUE, pattern= "tif"),
             functions="rasterToMemory",
             .stackName="landscape",
             packages="SpaDES",
             stringsAsFactors=FALSE)
loadFiles(fileList=fileList)

DEM <- landscape$DEM
DEM
Plot(landscape, add=FALSE)

caribou <- SpatialPointsNamed(coords=cbind(x=runif(1e2, -50,50),y=runif(1e2,-50,50)),
                                name="caribou")

Plot(caribou, add=TRUE)

#New Colours
library(RColorBrewer)
setColors(landscape, n=50) <-
  list(DEM=topo.colors(50),
       forestCover=brewer.pal(9, "Set1"),
       forestAge=brewer.pal("Blues", n=8),
       habitatQuality=brewer.pal(9, "Spectral"),
       percentPine=brewer.pal("GnBu", n=8))
Plot(landscape[[2:3]])

habitatQuality2 <- ((landscape$forestAge) / 100 +1) ^6

Plot(landscape[[1:3]], add=FALSE)
Plot(habitatQuality2, add=TRUE)

#unqiue name to avoid forestAge overlapping habitat plot
name(habitatQuality2) <- "habitatQuality2"
print(habitatQuality2)

#Overlay displaying

Plot(landscape, caribou, habitatQuality2, add=FALSE)
Plot(caribou)
Plot(caribou, addTo="ForestAge", size=2, axes=F)

#adding an arguement

Plot(landscape, add=FALSE, axes=TRUE)
Plot(caribou, add=TRUE, axes=FALSE)

Plot(landscape[[1:4]], add=FALSE)
Plot(caribou, add=TRUE, axes=FALSE)

Plot(landscape[[1:3]], add=FALSE)
landscape$forestAge = (landscape$forestAge +10 %% 100)
landscape$forestCover = (landscape$forestCover +10 %% 30)
Plot(landscape[[2:3]], add=TRUE)

#Overplotting
Plot(caribou, addTo="forestAge", size=5, axes=FALSE)

dev(4)

#Plot all maps on a new window
if(is.null(dev.list())) {
  dev(2)
} else {
  if(any(names(dev.list())=="RStudioGD")) {
    dev(which(names(dev.list())=="RStudioGD")+3)
  } else {
    dev(max(dev.list()))
  }
}

getwd()

find.package("SpaDES",lib.loc=getOption("devtools.path"), quiet=FALSE)
