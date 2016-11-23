install.packages("rgdal")
install.packages("tmap") #for tm
install.packages("gstat")
install.packages("raster") 
install.packages("xlsx")
install.packages("openxlsx")
install.packages("readxl")
install.packages("sp")
install.packages("proj4string")
install.packages("rmapshaper")
library(rgeos)
library(rgdal)
library(raster)
library(gstat)
library(tmap)
library(sp)
library(readxl)
library(dplyr)

###### Step 1. Plotting the shapefiles
# for plotting the shapefiles
ogrListLayers(dsn = getwd())
GroundwaterBasins <- rgdal::readOGR(dsn = getwd(), layer = "i08_B118_CA_GroundwaterBasins")
raster::plot(GroundwaterBasins)
# GroundwaterBasins is located at epsg:4269 
# "+init=epsg:4269 +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
######## This step takes loong time.
# plot(GroundwaterBasins) this 
# plot(gw_subset_1,add = T) to test if they are both on the same plot

###### Step 2. Preparing Data
# 2a.To read the files
gw = read_excel("B118 CA DWR GW_Master.xlsx")
site_code = unique(gw$Site_Code)
gw <- gw %>% mutate(Latitude = round(Latitude, 6), Longitude = round(Longitude,6))
n_distinct(paste(dt$Latitude, dt$Longitude)) #13820
ndt <- dt %>% distinct(Season, Year, Latitude, Longitude,.keep_all = TRUE)
# 2b. Subset only Depth measurement type
gw_subset = gw[which(gw$`Measurement Type` == "Depth"),]
# 2c. Subset only spring and 2011
gw_subset_s11 = gw_subset[which(gw_subset$Season == "Spring"),]
gw_subset_s11 = gw_subset[which(gw_subset$Year == 2011),]


###### Step 3. Make grid
# Change the data frame to spatial data frame
gw_subset_s11 = data.frame(gw_subset_s11)
coordinates(gw_subset_s11) = ~Longitude + Latitude 

# Make it the same as GroundWaterBasin data
# http://spatialreference.org/ref/epsg/4269/
proj4string(gw_subset_s11) <- CRS('+init=epsg:4269')

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(gw_subset_s11, "regular", n=100000))
names(grd) <- c("Longitude", "Latitude")
coordinates(grd) <- c("Longitude", "Latitude")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Test if our grd is good
plot(grd, cex=1.5)
points(gw_subset_s11, pch=1, col='red', cex=1)
title("Interpolation Grid and Sample Points")

# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(gw_subset_s11)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
gw_subset_s11.idw <- gstat::idw(WSEL ~ 1, gw_subset_s11, newdata=grd, idp=2)
r <- raster(gw_subset_s11.idw)
r.m <- mask(r,GroundwaterBasins)

# plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted WSEL \n(in inches)") + 
  tm_shape(gw_subset_s11) + tm_dots(size=0.01) +
  tm_legend(legend.outside=TRUE)

# Fine-tuning the interpolation test for 2.0
# Leave-one-out validation routine for 2.0
IDW.out <- vector(length = length(gw_subset_s11))
for (i in 1:length(gw_subset_s11)) {
  IDW.out[i] <- idw(WSEL ~ 1, gw_subset_s11[-i,], gw_subset_s11[i,], idp=2.0)$var1.pred
}
# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ gw_subset_s11$WSEL, asp=1, xlab="Observed", ylab="Predicted", pch=16, col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ gw_subset_s11$WSEL), col="red", lw=2,lty=2)
abline(0,1)
# Compute RMSE # 155.1917
sqrt(sum((IDW.out - gw_subset_s11$WSEL)^2) / length(gw_subset_s11))

# Leave-one-out validation routine for 0.25
IDW.out_1 <- vector(length = length(gw_subset_s11))
for (i in 1:length(gw_subset_s11)) {
  IDW.out_1[i] <- idw(WSEL ~ 1, gw_subset_s11[-i,], gw_subset_s11[i,], idp=0.25)$var1.pred
}
# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out_1 ~ gw_subset_s11$WSEL, asp=1, xlab="Observed", ylab="Predicted", pch=16, col=rgb(0,0,0,0.5))
abline(lm(IDW.out_1 ~ gw_subset_s11$WSEL), col="red", lw=2,lty=2)
abline(0,1)
# Compute RMSE # 1003.326
sqrt(sum((IDW.out_1 - gw_subset_s11$WSEL)^2) / length(gw_subset_s11))

# Cross-Validation
# Implementation of a jackknife technique to estimate a confidence interval at each unsampled point.
# Create the interpolated surface
img <- gstat::idw(WSEL ~ 1,  gw_subset_s11, newdata=grd, idp=2.0)
n   <- length(gw_subset_s11)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(WSEL ~ 1, gw_subset_s11[-i,], newdata=grd, idp=2.0)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 

# Clip the confidence raster to Texas
r <- raster(img.sig, layer="v")
r.m <- mask(r,GroundwaterBasins)

# Plot the map
tm_shape(r.m) + tm_raster(n=7,title="95% confidence interval \n(in inches)") +
  tm_shape(gw_subset_s11) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

