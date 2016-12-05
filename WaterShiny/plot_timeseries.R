#Time series plot based on a sub-basin
library(lubridate)
library(xts)
library(dygraphs)

# Load data
#load("pred.rda")
#load("coords.rda")
#sj_dt <- readRDS("Data/Gwater_SJvalley.rds")


#coords <- as.data.frame(coords)
#colnames(coords) <- c("Latitude","Longitude")
#coords$place <- paste(coords$Longitude, coords$Latitude, sep="_")

#coords_subasins <- sj_dt %>% filter(place %in% coords$place) %>% 
#                      dplyr::select(place, Basin_Subb, Subbasin_N) %>% distinct(place, .keep_all = TRUE)

#ncoords <- coords %>% left_join(coords_subasins, by.x = place)
#saveRDS(ncoords, "coords.rds")

# based on a sub_basin
plot_timeseries = function(sub_basin, coords_dat, pred_dat){
  subbasin_cols = which(coords_dat$Subbasin_N == sub_basin)
  predmean = rowMeans(pred_dat[,subbasin_cols])
  date = c("mar11", "sep11", "mar12", "sep12", "mar13", "sep13", "mar14", "sep14", "mar15", "sep15", "mar16", "sep16")
  pred1 = data.frame(date = date, WSEL = predmean)
  #colnames(pred1) = c("date", "WSEL")
  pred1$date = as.yearmon(pred1[,1],format="%b%y")
  pred_ts = xts(pred1$WSEL, order.by = pred1[, 1])
  colnames(pred_ts) = "WSEL"
  dygraph(pred_ts, main = paste("WSEL Change in ", sub_basin)) %>% 
    dyRangeSelector() %>% 
    dyOptions(stackedGraph = TRUE) %>%
    dyOptions(drawPoints = TRUE, pointSize = 2)
}

# Test
#plot_timeseries("KERN COUNTY", ncoords, pred)
