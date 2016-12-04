setwd("~/Box Sync/2016Fall (jenny91515@gmail.com)/Intern/Groundwater/")
library(rgdal)
library(geojsonio)
library(sp)

# read predicted results which is predicted by sp-Timer model
results = read.csv("hahaha.csv")

# Step 1. Select spring 2011 result
s2011 = results[,c(2,14,15)]  
sp::coordinates(s2011) = ~X2.1 + X1.1 # change the dataframe into spatial polygon
proj4string(s2011) <- CRS('+init=epsg:4326') # change the spatial polygon's projection

# Step 2. Read shapefiles into R
setwd("~/Box Sync/2016Fall (jenny91515@gmail.com)/Intern/Groundwater/Groundwater_Basins_shapefiles") # read shapefiles
ogrListLayers(dsn = getwd())
GroundwaterBasins <- rgdal::readOGR(dsn = getwd(), layer = "i08_B118_CA_GroundwaterBasins")
study_basins <- GroundwaterBasins[which(GroundwaterBasins@data$Basin_Name == "SAN JOAQUIN VALLEY"),] # select only "SAN JOAQUIN VALLEY"
study_basins <- spTransform(study_basins, s2011@proj4string) # change the projection of shapefiles the same as s2011

# Step 3. Select subbasins based on subbasins' ID.
study_basins_KAWEAH <- study_basins[which(study_basins@data$Basin_Subb == '5-022.11'),]
study_basins_WESTSIDE <- study_basins[which(study_basins@data$Basin_Subb == '5-022.09'),]
study_basins_KINGS <- study_basins[which(study_basins@data$Basin_Subb == '5-022.08'),]
study_basins_CHOWCHILLA <- study_basins[which(study_basins@data$Basin_Subb == '5-022.05'),]
study_basins_MADERA <- study_basins[which(study_basins@data$Basin_Subb == '5-022.06'),]
study_basins_MERCED <- study_basins[which(study_basins@data$Basin_Subb == '5-022.04'),]
study_basins_MODESTO <- study_basins[which(study_basins@data$Basin_Subb == '5-022.02'),]
study_basins_TURLOCK <- study_basins[which(study_basins@data$Basin_Subb == '5-022.03'),]
study_basins_DELTA_MENDOTA <- study_basins[which(study_basins@data$Basin_Subb == '5-022.07'),]
study_basins_TRACY <- study_basins[which(study_basins@data$Basin_Subb == '5-022.15'),]
study_basins_PLEASANT_VALLEY<- study_basins[which(study_basins@data$Basin_Subb == '5-022.10'),]
study_basins_WHITE_WOLF <- study_basins[which(study_basins@data$Basin_Subb == '5-022.18'),]
study_basins_TULE <- study_basins[which(study_basins@data$Basin_Subb == '5-022.13'),]
study_basins_KETTLEMAN_PLAIN <- study_basins[which(study_basins@data$Basin_Subb == '5-022.17'),]
study_basins_EASTERN_SAN_JOAQUIN<- study_basins[which(study_basins@data$Basin_Subb == '5-022.01'),]
study_basins_COSUMNES <- study_basins[which(study_basins@data$Basin_Subb == '5-022.16'),]
study_basins_KERN_COUNTY<- study_basins[which(study_basins@data$Basin_Subb == '5-022.14'),]
study_basins_TULARE_LAKE <- study_basins[which(study_basins@data$Basin_Subb == '5-022.12'),]

# Step 4. Select wells which are located in subbasins
study_wells_KAWEAH = s2011[!is.na(sp::over(s2011,as(study_basins_KAWEAH, "SpatialPolygons"))),]
study_wells_KAWEAH$name = "KAWEAH"
study_wells_WESTSIDE = s2011[!is.na(sp::over(s2011,as(study_basins_WESTSIDE, "SpatialPolygons"))),]
study_wells_WESTSIDE$name = "WESTSIDE"
study_wells_KINGS = s2011[!is.na(sp::over(s2011,as(study_basins_KINGS, "SpatialPolygons"))),]
study_wells_KINGS$name = "KINGS"
study_wells_CHOWCHILLA = s2011[!is.na(sp::over(s2011,as(study_basins_CHOWCHILLA, "SpatialPolygons"))),]
study_wells_CHOWCHILLA$name = "CHOWCHILLA"
study_wells_MADERA = s2011[!is.na(sp::over(s2011,as(study_basins_MADERA, "SpatialPolygons"))),]
study_wells_MADERA$name = "MADERA"
study_wells_MERCED = s2011[!is.na(sp::over(s2011,as(study_basins_MERCED, "SpatialPolygons"))),]
study_wells_MERCED$name = "MERCED"
study_wells_MODESTO = s2011[!is.na(sp::over(s2011,as(study_basins_MODESTO, "SpatialPolygons"))),]
study_wells_MODESTO$name = "MODESTO"
study_wells_TURLOCK = s2011[!is.na(sp::over(s2011,as(study_basins_TURLOCK, "SpatialPolygons"))),]
study_wells_TURLOCK$name = "TURLOCK"
study_wells_DELTA_MENDOTA = s2011[!is.na(sp::over(s2011,as(study_basins_DELTA_MENDOTA, "SpatialPolygons"))),]
study_wells_DELTA_MENDOTA$name = "DELTA_MENDOTA"
study_wells_TRACY = s2011[!is.na(sp::over(s2011,as(study_basins_TRACY, "SpatialPolygons"))),]
study_wells_TRACY$name = "TRACY"
study_wells_PLEASANT_VALLEY = s2011[!is.na(sp::over(s2011,as(study_basins_PLEASANT_VALLEY, "SpatialPolygons"))),]
study_wells_PLEASANT_VALLEY$name = "PLEASANT_VALLEY"
study_wells_WHITE_WOLF = s2011[!is.na(sp::over(s2011,as(study_basins_WHITE_WOLF, "SpatialPolygons"))),]
study_wells_WHITE_WOLF$name = "WHITE_WOLF"
study_wells_TULE = s2011[!is.na(sp::over(s2011,as(study_basins_TULE, "SpatialPolygons"))),]
study_wells_TULE$name = "TULE"
study_wells_KETTLEMAN_PLAIN = s2011[!is.na(sp::over(s2011,as(study_basins_KETTLEMAN_PLAIN, "SpatialPolygons"))),]
study_wells_KETTLEMAN_PLAIN$name = "KETTLEMAN_PLAIN"
study_wells_EASTERN_SAN_JOAQUIN = s2011[!is.na(sp::over(s2011,as(study_basins_EASTERN_SAN_JOAQUIN, "SpatialPolygons"))),]
study_wells_EASTERN_SAN_JOAQUIN$name = "EASTERN_SAN_JOAQUIN"
study_wells_COSUMNES = s2011[!is.na(sp::over(s2011,as(study_basins_COSUMNES, "SpatialPolygons"))),]
study_wells_COSUMNES$name = "COSUMNES"
study_wells_KERN_COUNTY = s2011[!is.na(sp::over(s2011,as(study_basins_KERN_COUNTY, "SpatialPolygons"))),]
study_wells_KERN_COUNTY$name = "KERN_COUNTY"
study_wells_TULARE_LAKE = s2011[!is.na(sp::over(s2011,as(study_basins_TULARE_LAKE, "SpatialPolygons"))),]
study_wells_TULARE_LAKE$name = "TULARE_LAKE"

# rbind them into a dataframe
study_wells = rbind(study_wells_KAWEAH,study_wells_WESTSIDE)
study_wells = rbind(study_wells,study_wells_KINGS)
study_wells = rbind(study_wells,study_wells_CHOWCHILLA)
study_wells = rbind(study_wells,study_wells_MADERA)
study_wells = rbind(study_wells,study_wells_MERCED)
study_wells = rbind(study_wells,study_wells_MODESTO)
study_wells = rbind(study_wells,study_wells_TURLOCK)
study_wells = rbind(study_wells,study_wells_DELTA_MENDOTA)
study_wells = rbind(study_wells,study_wells_TRACY)
study_wells = rbind(study_wells,study_wells_PLEASANT_VALLEY)
study_wells = rbind(study_wells,study_wells_WHITE_WOLF)
study_wells = rbind(study_wells,study_wells_TULE)
study_wells = rbind(study_wells,study_wells_KETTLEMAN_PLAIN)
study_wells = rbind(study_wells,study_wells_EASTERN_SAN_JOAQUIN)
study_wells = rbind(study_wells,study_wells_COSUMNES)
study_wells = rbind(study_wells,study_wells_KERN_COUNTY)
study_wells_s2011 = rbind(study_wells,study_wells_TULARE_LAKE)
write.csv(study_wells_s2011,"s2011.csv")