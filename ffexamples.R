library(ff)
library(ffbase)
library(raster)

r1984age <- raster("C:\\dev\\ffExamples\\1984age.tif")
r1984logAge <- raster("C:\\dev\\ffExamples\\1984logAge.tif")
r1985age <- raster("C:\\dev\\ffExamples\\1985age.tif")
r1985logAge <- raster("C:\\dev\\ffExamples\\1985logAge.tif")

ageStack <- stack(list(r1984age,  r1985age))
logageStack <- stack(list(r1984logAge,  r1985logAge))

#assumption made that all rasters are of equal dimensions
totalRows <- nrow(ageStack)
totalCols <- ncol(ageStack)

#this variable determines how much data will get read in for each pass
stripHeight <- 100

#with no remainder, this would be the total number of strips to read
totalStrips <- floor(totalRows / stripHeight)
#but there may be a remainder
remainder <- floor(totalRows %% stripHeight)
#add a strip for the remainder strip, if it is more than 0
if(remainder > 0){
  totalStrips = totalStrips + 1
}

#read a strip of data from the raster stack
readStrip <- function(row, height, stack) { 
  strip <- as.data.frame(
    getValuesBlock(stack, row=row, nrows=height, col=1, ncols=totalCols))
  strip
}

#lets hear it for 1 based programming!
getStartingRowNum <- function(stripNum)
{
  (stripNum - 1) * stripHeight + 1
}

#get the height of a strip, this is either 
#the strip height, or the remainder height
getHeight <- function(stripNum) {
  

  if(stripNum == totalStrips && remainder > 0){
    remainder
  } else if(stripNum > totalStrips){
    0
  } else {
    stripHeight
  }
}

#read a strip from the stack and convert it to an ff dataframe
get_ff_Strip <- function(stripNum, stack) {
  row <- getStartingRowNum(stripNum)
  height <- getHeight(stripNum)
  df <- readStrip(row, height, stack)
  as.ffdf(df)
}

#get the list of ff tables
lst1<- lapply(1:totalStrips, FUN=function(x) get_ff_Strip(x, ageStack))
#append them all together
output1 <- Reduce(function(x,y) ffdfappend(x,y, adjustvmode=F), lst1)

lst2 <- lapply(1:totalStrips, FUN=function(x) get_ff_Strip(x, logageStack))
output2 <- Reduce(function(x,y) ffdfappend(x,y, adjustvmode=F), lst2)
