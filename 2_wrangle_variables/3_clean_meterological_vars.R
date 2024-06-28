####***********************
#### Code Description ####
# Author: Vivian 
# Date: 10/21/23
# Goal: Generate meterological variables (temperature, precipitation) at zcta level
# note: daymet data is something I generated from google earth engine
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"

# read in data
daymet_ny <- read_csv(paste0(path_data, "daymet/daymet-NY.csv"))
glimpse(daymet_ny)

# Clean daymet data -------------------------------------------------------
daymet_ny <- daymet_ny %>% 
  mutate(date = as.Date(substr(`system:index`, 1, 8), format = "%Y%m%d")) %>% 
  select(-`system:index`) %>% 
  rename(zcta = GEOID10) %>% 
  write_csv(., paste0(path_data, "data_process/daymet_ny_cleaned.csv"))


