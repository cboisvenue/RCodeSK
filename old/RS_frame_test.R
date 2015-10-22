# First run at creating RS growth data frame for Celine



#Author: Byron Smiley
#Date: June 14 2015

#-------------------------------------------------
library(ff)
library(raster)
library(parallel)
library(snow)
library(dplyr)
rasterOptions(tmpdir="H:/saskatchewan/data/temp")

#-------------------------------------------------
#Identify input and output directories (KANGAKOLA - CHANGE THESE FOR YOUR DATA)
indir <- "H:/saskatchewan/spatialGrowth/fmapa/RSGrowth/"
years <- 1985:2012
outdir <- "H:/saskatchewan/spatialGrowth/fmapa/RSGrowth/dataframes"

# List and Stack-----------------------------------------------------------------------
#make list of all rasters to be included in dataframe... eventually...
allfiles <- list.files(paste(indir), full.names=TRUE, pattern="*tif") # make list of file names

#Stack ALL rasters
setwd(indir)
allStack <- stack(allfiles)

#list files for only 1 year of data... need all 28 (1985-2012) so we can create stacks then
#extract dataframes from those stacks

# each stack (or record) must be the strata layer (same one for each stack), age rasters 
#(age and log(age)) from one year and the delta biomass from the following year
year_file2 <-grep("*/1984|*_1985|strata", allfiles, value=TRUE)
year_file3 <-grep("*/1985|*_1986|strata", allfiles, value=TRUE)

# Make individual stacks for 1985 and 1986
stack2 <- stack(year_file2)
stack3 <- stack(year_file3)

#Make dataframe and export table--------------------------------------------------------

#Make dataframe out of stack 2 (first year of data)
startTime <- Sys.time()
# create dataframe from raster stack of 1 year
RS_df85 <- as.data.frame(stack2,na.rm=TRUE)
#remove records within this year's worth of data that have cell NA values for any of the 4 rasters
RS_df1985_b <- RS_df85[complete.cases(RS_df85),]

#Give the raster ID column a name called 'RasterID'
RS_df1985_c <- cbind(RasterID = rownames(RS_df1985_b), RS_df1985_b)

#Write table (KANGAKOLA - if we can do more work in R like combining the dataframes and just export
#the final combined dataframe that would be best.....)
write.table(RS_df1985_c, paste(outdir,"RS_1985_df.txt",sep="/"), sep=",",row.names=FALSE)

#Make dataframe out of stack 3 -----------------(first year of data) (same as above except for
#following year)
RS_df86 <- as.data.frame(stack3,na.rm=TRUE)
RS_df1986_b <- RS_df86[complete.cases(RS_df86),]
RS_df1986_c <- cbind(RasterID = rownames(RS_df1986_b), RS_df1986_b)
write.table(RS_df1985_c, paste(outdir,"RS_1986_df.txt",sep="/"), sep=",",row.names=FALSE)

endTime <- Sys.time()
elapsedTime_dataframe <- endTime - startTime
# End of manual export of 2 years of datarames----------------------------------------------------

#TESTING stuff
# Make ff dataframes of regular dataframes and merge sideways (ff dataframes good for VERY large DATA)
ff85 <- as.ffdf(RS_df1985_c)

ff86 <- as.ffdf(RS_df1986_c)

join1 <- merge(ff85, ff86, by="RasterID")

# OTHER TESTING---------------------------------------------------------------------------------

nonNas <- allStack[[1]] * allStack[[2]]
nonNas2 <- nonNas * allStack[[3]]
freq4 <- freq(nonNas2)
nonNasFreq <- freq(nonNas)
allStacFreq <- freq(allStack)

freq2 <- freq(allStack[[1]]) #34077283
freq3 <- freq(allStack[[2]]) #52953868

# freq1 = 52953868

#Tried these but didnt work the way I wanted

#CT <- crosstab(stack85,digits=4,long=TRUE, useNA=TRUE)
#exNA <- extract(stack85, NA)
#omNA <- na.omit(stack85)
#calcNA <- calc(stack85, na.rm=TRUE)

#Tried values instead of as.data.frame function - works but does not include RasterID which we need!
startTime <- Sys.time()
value4 <- values(allStack)
values_test2 <- value4[complete.cases(value4),]
endTime <- Sys.time()
elapsedTime_values <- endTime - startTime

#Function 'f1' imports data listed in 'filenames' and assigns projection
#stack.f<-function(x,z) {
#  y <- stack(x)
#  return(y)
#}
#stack85 <- mclapply(year_files,stack.f)


ff_RS <- as.ffdf(RS_dataframe)
ff_test <- as.ffdf(data1)

data_1984 <- subset(data1[1:20])

RS_dataframe <- as.data.frame(data1, na.rm=TRUE, long=TRUE)
#Error: cannot allocate vector of size 31.9 Gb (9.3 hours to determine this....)
endTime <- Sys.time()
elapsedTime_dataframe <- endTime - startTime


#Might be able to make as.data.frame in blocks??? then recombine later??? - this way we can remove
#NA records (cells) through time and within a year possibly
RS_data12 <- getValuesBlock(data1, row=5000, nrows=5, col=5000, ncols=5)

RS_data2_frame <- as.data.frame(data1[[25:26]], na.rm=TRUE, long=TRUE)


RS_data2_matrix <- getValuesBlock(data1, row=5000, nrows=5, col=5000, ncols=5)


RS_dataframe2 <- as.data.frame(data1[[28:29]], na.rm=TRUE, long=TRUE)




#Synchronize NA function


#bsmiley

#July 22 2015
#-----------------------------------------------------

synchroniseNA <- function(x)
{
  if(canProcessInMemory(x, n = 2))
  {
    val <- getValues(x)
    NA.pos <- unique(which(is.na(val), arr.ind = T)[, 1])
    val[NA.pos, ] <- NA
    x <- setValues(x, val)
    return(x)
  } else
  {
    x <- mask(x, calc(x, fun = sum))
    return(x)
  }
}

stack2yrs <- stack(stack2, stack3)
beginCluster(30)
NaSync_stackyrs <- mask(stack2yrs, (calc(stack2yrs,fun=sum)))
NA_freq_stack <- freq(NaSync_stackyrs)
#68294055
68294055-63210593
stack2_freq <- freq(stack2)
NAstack2_freq <- freq(NaSync_stack2)
#63210593

#Make NAs for cells of all rasters that are NAs in any raster
NA_allstack <- mask(allStack, (calc(allStack,fun=sum)))

#Separately by variable for all years----------------------------------------------

#log(years)
alllogYrs_age <-grep("*logAge", allfiles, value=TRUE)
stack_logyrs <- stack(alllogYrs_age)
NA_logyrs <- mask(stack_logyrs, (calc(stack_logyrs,fun=sum)))
stack_logyrs.freq <- freq(NA_logyrs)
#NA: 52953868

#log(years) with strata
logyrsStrata <-grep("*logAge|strata", allfiles, value=TRUE)
logyrs_strat_stack <- stack(logyrsStrata)
NA_logyrs_strat <- mask(logyrs_strat_stack, (calc(logyrs_strat_stack,fun=sum)))
NA_logyrs_strat.freq <- freq(NA_logyrs_strat)
#NAs: 53178910
#-------------------------------------------------------------------
#years
startTime <- Sys.time()
years <-grep("*age", allfiles, value=TRUE)
years_stack <- stack(years)
NA_years <- mask(years_stack, (calc(years_stack,fun=sum)))
NA_years.freq <- freq(NA_years)
#NAs:34077283

#years+ strata
startTime <- Sys.time()
years_st <-grep("*age|strata", allfiles, value=TRUE)
years_stack_st <- stack(years_st)
NA_years_st <- mask(years_stack_st, (calc(years_stack_st,fun=sum)))
NA_years_st.freq <- freq(NA_years_st)
#NAs:53178124

#Delta_biomass
d_biomass <-grep("*bio", allfiles, value=TRUE)
dbiomass_stack <- stack(d_biomass)
dbiomass_stack.freq <- freq(dbiomass_stack)
NA_dbiomass <- mask(dbiomass_stack, (calc(dbiomass_stack,fun=sum)))
NA_dbiomass.freq <- freq(NA_dbiomass)
#NAs:

#Delta_biomass+strata
d_biomass_st <-grep("*bio|strata", allfiles, value=TRUE)
dbiomass_stack_st <- stack(d_biomass_st)
NA_dbiomass_st <- mask(dbiomass_stack_st, (calc(dbiomass_stack_st,fun=sum)))
NA_dbiomass_st.freq <- freq(NA_dbiomass_st)
#NAs:

#Age+Delta_biomass+strata
age_d_biomass_st <-grep("*bio|strata|*age", allfiles, value=TRUE)
age_dbiomass_stack_st <- stack(age_d_biomass_st)
NA_age_dbiomass_st <- mask(age_dbiomass_stack_st, (calc(age_dbiomass_stack_st,fun=sum)))
NA_age_dbiomass_st.freq <- freq(NA_age_dbiomass_st)
#NAs:

#logAGE+Age+Delta_biomass+strata
age2_d_biomass_st <-grep("*bio|strata|*age|logAge", allfiles, value=TRUE)
age2_dbiomass_stack_st <- stack(age2_d_biomass_st)
NA_age2_dbiomass_st <- mask(age2_dbiomass_stack_st, (calc(age2_dbiomass_stack_st,fun=sum)))
NA_age2_dbiomass_st.freq <- freq(NA_age2_dbiomass_st)
#NAs:

endTime <- Sys.time()
elapsedTime_NA <- endTime - startTime



#Full: 52916562

53178910-52953868

