library(sp)
library(gstat)
library(lubridate)
library(dplyr)

df <- na.omit(EP1)

#### CHUNK NUMBER 1 : PREPARE DATA 
df$Date  <- ymd( paste(df$YEAR,"-",df$MONTH, "-", df$DAY, sep="") ) #paste y/m/d values
df$Time  <- hms( paste(df$HOUR, ":", df$MINUTE, ":", df$SECOND, sep="", label=FALSE)) #paste h/m/s values
df$ID    <- ymd_hms(paste(df$Date,"", df$Time)) #create date ID string
df       <- subset(df, select = -c(Created, YEAR,
                                   HOUR, MINUTE, SECOND, Date, 
                                   Time)) #remove used columns
df_hmT   <- subset(df, select = c(ID, Depth, Temp, RDO, DAY, MONTH)) #subset for contour plot of temp
df_hmT$X <- decimal_date(df_hmT$ID) #create decimal date for easy organizing of x axis
df_hmT$Y <- df_hmT$Depth #rename field
df_hmT$Y    <- df_hmT$Y * -1
df <- df_hmT

df2 <- df %>%
       mutate(x.dec = X,
              y = Y,
              y.diff = diff(range(y)),
              x.diff = diff(range(x.dec)),
              x = x.dec/x.diff * y.diff ,
              day = paste(month(ID, label=TRUE), "-", day(ID), sep=""))     %>%
       select(x, y, Temp, RDO, day)


# Create a SPDF spatial object
SPDF <- SpatialPointsDataFrame(cbind(x=df2$x, y=df2$y), df2)
plot(SPDF)

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(SPDF, "regular", n=10000))
names(grd)       <- c("x", "y")
coordinates(grd) <- c("x", "y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

#
# IDW
#

# Interpolate the surface using a power value of 2 (idp=2.0)
dat.idw <- idw(Temp~1,SPDF,newdata=grd,idp=2.0)

# Plot the raster and the sampled points
OP      <- par( mar=c(0,0,1,0))
image(dat.idw,"var1.pred",col=terrain.colors(20))
contour(dat.idw,"var1.pred", add=TRUE, nlevels=10, col="#656565")
plot(SPDF, add=TRUE, pch=16, cex=0.5)
par(OP)
mtext(unique(df2$day), side=3, line=2.2, at= unique(round(df2$x)), cex=0.9)

#
# A loess smooth
#

M1 <- loess(Temp ~ x + y, data = SPDF, span=0.8)
summary(M1)
M1.pred <- predict(M1, as.data.frame(grd))
M1.z <- SpatialGridDataFrame(grid = grd, data = data.frame(z = M1.pred))

OP      <- par( mar=c(0,0,0,0))
image(M1.z,"z",col=terrain.colors(20))
contour(M1.z,"z", add=TRUE, nlevels=10, col="#656565")
plot(SPDF, add=TRUE, pch=16, cex=0.5)
par(OP)

#
# Kriging
#

# First, find an overall trend (this will be removed later when kriging)
f1 <- as.formula(Temp ~ x + y + I(x*x)+I(y*y) + I(x*y))
M2 <- lm(f1, SPDF)
M2.pred <- predict(M2, as.data.frame(grd))
M2.z <- SpatialGridDataFrame(grid = grd, data = data.frame(z = M2.pred))
OP      <- par( mar=c(0,0,0,0))
image(M2.z,"z",col=terrain.colors(20))
contour(M2.z,"z", add=TRUE, nlevels=10, col="#656565")
plot(SPDF, add=TRUE, pch=16, cex=0.5)
par(OP)

# Next, come up with a variogram model
# (note that the function is given the trend model
# which is used by the function to detrend the data)
var.smpl <- variogram(f1, SPDF,alpha =90, cloud = FALSE, 
                      cutoff=5, tol.hor = 2)
plot(var.smpl, pch= 16, col= "red")
dat.fit <- fit.variogram(var.smpl, vgm(psill=100, model="Gau", range=10, nugget=0.0))
plot(var.smpl, dat.fit)

# Now we krige
## Need to remove duplicate points
zd <- zerodist(SPDF)
SPDF2 <- SPDF[-zd[,2], ]
dat.krg <- krige( f1, SPDF2, grd, dat.fit)

OP      <- par( mar=c(0,0,1,0))
image(dat.krg,"var1.pred",col=terrain.colors(20))
contour(dat.krg,"var1.pred", add=TRUE, nlevels=10, col="#656565")
plot(SPDF2, add=TRUE, pch=16, cex=0.5)
par(OP)
mtext(unique(df2$day), side=3, line=2.2, at= unique(round(df2$x)), cex=0.9)
