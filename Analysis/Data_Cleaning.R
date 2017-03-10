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

# Select the recent 5 years data 
gwl_cleaned <- dt %>%
  select(CASGEM_STATION_ID, MEASUREMENT_DATE, year, GS_ELEVATION,
         RP_ELEVATION, WS_READING, RP_READING, WSEL_calc) %>% 
  filter(MEASUREMENT_DATE > "2011-01-01" & MEASUREMENT_DATE < "2016-12-31") %>%  
  arrange(MEASUREMENT_DATE)

# Load the latitude and longitude data
loc_df <- read.csv("gst_file.csv")
sj_loc_df <- loc_df %>% filter(COUNTY_NAME == "San Joaquin") %>% select(CASGEM_STATION_ID, LATITUDE, LONGITUDE)

gwl_cleaned %>% filter(CASGEM_STATION_ID==50740)
# inner join to get the San Joaquin valley data for modeling
sj_gwl_cleaned <- merge(x = gwl_cleaned, y = sj_loc_df, type = "inner", by = "CASGEM_STATION_ID")

# Clean the ETO data and get the average daily eto 
clean_eto_data <- function(file_name){
  cat(file_name, '\n')
  eto_df <- read.csv(file_name)
  eto_df$MEASUREMENT_DATE <- as.Date(eto_df$Date, '%m/%d/%Y')
  eto_avg_df = eto_df %>% group_by(MEASUREMENT_DATE) %>% summarise(ETO_avg = mean(ETo..in.day.),
                                                       Sol_Rad_avg = mean(Sol.Rad.Avg..Ly.day.))
  return(eto_avg_df)
}

# Load the yearly ETO data
file_names <- list.files('.', pattern = '*_eto.csv')
eto_df_ls <- map(file_names,.f = clean_eto_data)
cleaned_eto_byday_df <- bind_rows(eto_df_ls)

# Merge the daily average ETO data and the groudwater level data
sj_df <- merge(sj_gwl_cleaned, cleaned_eto_byday_df,  by = "MEASUREMENT_DATE")

# split training_set and test_set
sj_train_df <- sj_df %>% filter(MEASUREMENT_DATE < '2016-06-30')
sj_test_df <- sj_df %>% filter(MEASUREMENT_DATE >= '2016-06-30')

# save the data files
write.csv(file = "sj_train_set.csv", x = sj_train_df, row.names = FALSE)
write.csv(file = "sj_test_set.csv", x = sj_test_df, row.names = FALSE)
