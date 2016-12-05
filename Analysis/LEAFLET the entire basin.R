library(rgdal)

#setwd("~/Box Sync/2016Fall (jenny91515@gmail.com)/Intern/Groundwater/Groundwater_Basins_shapefiles")
ogrListLayers(dsn = "Data/Groundwater_Basins_shapefiles")
GroundwaterBasins <- rgdal::readOGR(dsn = "Data/Groundwater_Basins_shapefiles", layer = "i08_B118_CA_GroundwaterBasins")
study_basins <- GroundwaterBasins[which(GroundwaterBasins@data$Basin_Name == "SAN JOAQUIN VALLEY"),]
study_basins <- spTransform(study_basins, CRS('+init=epsg:4326'))
study_basins.Leaflet <- toGeoJSON(study_basins)
leaflet(study_basins.Leaflet)

target_area <- GroundwaterBasins@data %>% dplyr::filter(Basin_Name ==  "SAN JOAQUIN VALLEY") %>% 
    dplyr::select(Basin_ID, Basin_Subb, Basin_Name, Subbasin_N) 

saveRDS(target_area, "Data/subbasinNames.rds")
