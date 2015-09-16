#---------------------------------------------------------
# bits of code that will help when I am ready to spatialise the Random Forest model

require(sp)
require(rgdal)
require(raster)
require(randomForest)

# CREATE LIST OF RASTERS
rlist=list.files(getwd(), pattern="img$", full.names=TRUE) 

# CREATE RASTER STACK
xvars <- stack(rlist)      

# READ POINT SHAPEFILE TRAINING DATA
sdata <- readOGR(dsn=getwd() layer="inshape")

# ASSIGN RASTER VALUES TO TRAINING DATA
v <- as.data.frame(extract(xvars, sdata))
sdata@data = data.frame(sdata@data, v[match(rownames(sdata@data), rownames(v)),])

# RUN RF MODEL
rf.mdl <- randomForest(x=sdata@data[,3:ncol(sdata@data)], y=as.factor(sdata@data[,"train"]),
                       ntree=501, importance=TRUE)

# CHECK ERROR CONVERGENCE
plot(rf.mdl)

# PLOT mean decrease in accuracy VARIABLE IMPORTANCE
varImpPlot(rf.mdl, type=1)

# PREDICT MODEL
predict(xvars, rf.mdl, filename="RfClassPred.img", type="response", 
        index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
