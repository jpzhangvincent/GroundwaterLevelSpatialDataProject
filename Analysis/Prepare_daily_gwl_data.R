library(tidyverse)
library(lubridate)

# Load csv containing all well data
dt0 <- read.csv("gwl_file.csv")

# Convert measurement date-time to a date
dt0$MEASUREMENT_DATE <- as.Date(dt0$MEASUREMENT_DATE, "%m/%d/%Y %H:%M:%S")

# Convert 15-min interval data to daily data by averaging daily readings
dt <- dt0 %>%
  group_by(CASGEM_STATION_ID, SITE_CODE, MEASUREMENT_DATE, MEASUREMENT_ISSUE_ID,
           MEASUREMENT_METHOD_ID, MEASUREMENT_ACCURACY_ID, ORG_ID, ORG_NAME, COMMENTS) %>%
  summarize(GS_ELEVATION = mean(GS_ELEVATION),
            RP_ELEVATION = mean(RP_ELEVATION),
            WS_READING = mean(WS_READING),
            RP_READING = mean(RP_READING)) %>%
  ungroup()


# Calculate WSEL by adding Ground Surface Elevation to either the Water Surface
# reading or the negative Reference Point reading
dt$WSEL_calc <- dt$GS_ELEVATION + ifelse(dt$RP_READING == 0, dt$WS_READING, -dt$RP_READING)

# Filter out NA WSEL values
dt <- dt[which(!is.na(dt$WSEL_calc)),]

# Calculate date analysis to prepare for daily well analysis
dt$year <- lubridate::year(dt$MEASUREMENT_DATE)

gwl_cleaned <- dt %>%
  select(CASGEM_STATION_ID, MEASUREMENT_DATE, year, GS_ELEVATION,
         RP_ELEVATION, WS_READING, RP_READING, WSEL_calc)

#####################
# Extract daily well data to create annual model of depth
#####################

# Identify wells with daily data
dailies <- dt %>% group_by(CASGEM_STATION_ID, year) %>%
  summarize(records = n()) %>%
  filter((records >= 300 & year >= 2007) | (records > 160 & year == 2016))

daily_median <- dt %>% inner_join(dailies[1:2], by = c("CASGEM_STATION_ID", "year")) %>%
  group_by(CASGEM_STATION_ID) %>%
  summarize(med_wsel = median(WSEL_calc, na.rm = TRUE))

dailies %>% group_by(year) %>% summarize(count = n())

# Bring daily data into a table, convert WSEL to a relative WSEL value, equal to
# the difference from the average for that well
daily_dt <- dt %>% inner_join(dailies[1:2], by = c("CASGEM_STATION_ID", "year")) %>%
  left_join(daily_median, by = "CASGEM_STATION_ID") %>%
  mutate(relative_wsel = WSEL_calc - med_wsel) %>%
  filter(abs(relative_wsel) <= 200) %>%
  select(station_id = CASGEM_STATION_ID, date = MEASUREMENT_DATE, relative_wsel)

daily_dt <- reshape2::dcast(daily_dt, date ~ station_id, mean, value.var = "relative_wsel")

# Convert to matrix, prepare for interpolation
daily_m <- as.matrix(daily_dt[2:ncol(daily_dt)])
year_index <- lubridate::year(daily_dt$date)

daily_imputed <- matrix(NA, nrow = nrow(daily_m), ncol = ncol(daily_m))
dimnames(daily_imputed) <- dimnames(daily_m)

# Interpolate missing values one year at a time for wells with values in at
# least half of the days in the year

for(i in unique(year_index)){
  col_index <- which(apply(daily_m[year_index == i,], MARGIN = 2,
                           function(x) sum(!is.na(x))) > sum(year_index == i)*.5)
  impute_year <- missForest::missForest(daily_m[which(year_index == i), col_index])
  daily_imputed[which(year_index == i), col_index] <- impute_year$ximp
}

# Use interpolated values to find a mean relative wsel for each day
daily_mean <- data.frame(date = daily_dt$date,
                         mean_wsel = apply(daily_imputed, MARGIN = 1,
                                           function(x) mean(x, na.rm = TRUE)))

# Smooth the daily mean
daily_mean$sm_mean <- as.numeric(smooth(daily_mean$mean_wsel, twiceit = TRUE))

# Use loess to build a curve for each smoothed daily mean relative wsel
fit <- loess(sm_mean ~ as.numeric(date), data = daily_mean,
             span = 1/(length(unique(year_index))*2),
             control=loess.control(surface = "direct"))
daily_mean$loess <- fitted(fit)


daily_ts <- ts(daily_mean$loess, start = daily_mean$date[1], frequency = 365/7)

daily_arm <- arima(daily_ts, c(0,1,0), seasonal = list(order = c(1, 0, 1), period = 52))

save(daily_dt, daily_ts, daily_arm, daily_mean, file = "daily_gwl_data.RData")
