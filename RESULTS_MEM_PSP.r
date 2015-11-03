#-------------------------------
# Results for manuscript for the MEM PSP model
#
# C Boisvenue
# Novemeber 2nd, 2015
#---------------------------

library(lme4)

# load the model, it is names mem7
load(file = "M:/Spatially_explicit/01_Projects/07_SK_30m/Working/growth/MEM_t_haPSP/MEM_t_ha.Rdata")

summary(mem7)
str(mem7)

residuals(mem7) # this has 1353 values, equal to the number of observations
ranef(mem7)
str(ranef(mem7)) # this has 804, the number of PLOT_ID
plot(ranef(mem7),cex=2)
plot(residuals(mem7))

plot(mem7)


library (ggplot2)
error1 <- as.data.frame(cbind(c(1:1353),residuals(mem7)))
names(error1) = c("Index","Error")
plot.er1 <- ggplot(data=error1, aes(Index,Error)) + geom_point(size=2) +geom_hline(y=0,size=1) + theme(text = element_text(size=20))


error2 <- as.data.frame(ranef(mem7)$PLOT_ID)
names(error2) <- "Intercept"
plot.er2 <- ggplot(data=error2,aes(sample=Intercept)) +stat_qq(shape=1) +theme(text = element_text(size=20))
#,xlab="qnorm",ylab="(intercept)",cex.lab=1.5, DID NOT WORK TO GET AXIS LABELS BIGGER

# both plots on one page
library(grid)
library(gridExtra)
install.packages("cowplot")
library(cowplot)
both.plots <- multiplot(plot.er1,plot.er2)
# this is a cowplot fnct
#save_plot()
#This is better since i don't need to load anything to use it - DID NOT WORK
#savePlot(filename = "C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/WritingBin/figures/MEM_PSP_Figure.jpeg",device=dev.cur())

# ggsave(both.plots,file="C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/WritingBin/figures/MEM_PSP_Figure.jpeg")


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
