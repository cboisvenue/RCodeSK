# ggRandomForests trial
# I am following this vignette: 
# http://cran.r-project.org/web/packages/ggRandomForests/vignettes/randomForestSRC-Regression.html

#################
# Load_packages #
#################
 library("ggplot2")         # Graphics engine for generating all types of plots
 library("RColorBrewer")    # Nice color palettes
 library("plot3D")          # for 3d surfaces. 
 
 library("dplyr")           # Better data manipulations
 library("reshape2")        # for transforming wide data into long data (melt)
 
 library("parallel")        # mclapply for multicore processing
 
 library("xtable")          # For nice html and latex tables
 
# Analysis packages.
library("randomForestSRC") # random forests for survival, regression and classification
library("ggRandomForests") # ggplot2 random forest figures

#########################################################################
# Default settings
#########################################################################
theme_set(theme_bw())     # A ggplot2 theme with white background

#Note that df7 comes from SK419_RandForestFirsTryExploration.r
## MAY HAVE TO REBUILD THIS DATA SET WITHIN THIS SCRIPT
rfpck2 <- rfsrc(biom.ha~.,data=df7,ntree=500,importance="permute",mtry=6) 

# Plot the OOB errors against the growth of the forest.---------------------------------------
gg_e <- gg_error(rfpck2)
plot(gg_e)
plot(gg_rfsrc(rfpck2), alpha=.5)

# Plot the VIMP rankings of independent variables.--------------------------------------------
plot(gg_vimp(rfpck2))
plot.gg_vimp(rfpck2)

# Load the data, from the call:
varsel_rfpck2 <- var.select(rfpck2)
data(varsel_rfpck2)

# Save the gg_minimal_depth object for later use.
gg_md <- gg_minimal_depth(varsel_rfpck2)

# plot the object-----------------------------------------------------------------------------
plot(gg_md)

# gg_minimal_depth objects contain information about
# both minimal depth and VIMP.----------------------------------------------------------------
plot.gg_minimal_vimp(gg_md)
#INTERPRETATION: Points above the red dashed line are ranked higher by VIMP than by minimal 
#depth, indicating the variables are sensitive to misspecification. Those 
#below the line have a higher minimal depth ranking, indicating they are better 
#at dividing large portions of the population.

# Create the variable dependence object from the random forest--------------------------------
gg_v <- gg_variable(rfpck2)
 
# We want the top ranked minimal depth variables only,
# plotted in minimal depth rank order. -------------------------------------------------------
xvar <- gg_md$topvars
 
# plot the variable list in a single panel plot-----------------------------------------------
plot(gg_v, xvar=xvar, panel=TRUE, se=.95, span=1.2, alpha=.4)
#INTERPRETATION: The closer the panels match, the better the RF prediction. The panels are 
#sorted to match the order of variables in the xvar argument and include a smooth
#loess line (Cleveland 1981; Cleveland and Devlin 1988), with 95% shaded 
#confidence band, to indicates the trend of the prediction dependence over the covariate values.

# Load the data, from the call:
 partial_rfpck2 <- plot.variable(rfpck2,xvar=gg_md$topvars,partial=TRUE, sorted=FALSE,show.plots=FALSE)
# data(partial_Boston) # could use this to load the data (it is computationally intensive)

# generate a list of gg_partial objects, one per xvar.-----------------------------------------
gg_p <- gg_partial(partial_rfpck2)
 
# plot the variable list in a single panel plot------------------------------------------------
plot(gg_p, xvar=xvar, panel=TRUE, se=FALSE)


# Load the data, from the call:
interaction_rfpck2 <- find.interaction(rfpck2)
#data(interaction_rfpck2)
 
# Plot the results in a single panel.
plot(gg_interaction(interaction_rfpck2),xvar=gg_md$topvars, panel=TRUE)
# note sure how to interprete this one yet...

# MORE IN THIS VIGNETTE, BUT ENOUGH FOR ME RIGHT NOW










