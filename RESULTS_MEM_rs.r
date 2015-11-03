#--------------------------------------------
# Looking at MEM model performance from one sample of the RS-based biomass change model.
# Each sample has 280 000 observations (10 000 sampled pixels with each sample having 28 obs)
# 
# C Boisvenue Nov3, 2015
#--------------------------------------------

load("M:/Spatially_explicit/01_Projects/07_SK_30m/Working/Cboisvenue/CleanedUpForUsing/MEM_RS_OneSample.RData")
memPA.s1

summary(memPA.s1)
str(memPA.s1)
plot(memPA.s1)

library (ggplot2)
error1 <- as.data.frame(cbind(c(1:280000),residuals(memPA.s1)))
names(error1) = c("Index","Error")
plot.er1 <- ggplot(data=error1, aes(Index,Error)) + geom_point(size=2) +geom_hline(y=0,size=1) + theme(text = element_text(size=20))


error2 <- as.data.frame(ranef(memPA.s1)$RasterID)
names(error2) <- "Intercept"
plot.er2 <- ggplot(data=error2,aes(sample=Intercept)) +stat_qq(shape=1) +theme(text = element_text(size=20))


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(plot.er1,plot.er2)
# because I couldn't figure out how to save this plot I mannually exported it into 
#G:\RES_Work\Work\JoanneWhite\SK_work\WritingBin\figures/MEM_rs_Figure.jpeg