setwd("~/Box Sync/2016Fall (jenny91515@gmail.com)/Intern/Groundwater/Groundwater_Basins_shapefiles")
ogrListLayers(dsn = getwd())
GroundwaterBasins <- rgdal::readOGR(dsn = getwd(), layer = "i08_B118_CA_GroundwaterBasins")
study_basins <- GroundwaterBasins[which(GroundwaterBasins@data$Basin_Name == "SAN JOAQUIN VALLEY"),]
study_basins <- spTransform(study_basins, CRS('+init=epsg:4326'))
study_basins.Leaflet <- toGeoJSON(study_basins)
leaflet(study_basins.Leaflet)