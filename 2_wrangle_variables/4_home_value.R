####***********************
#### Code Description ####
# Author: Vivian 
# Date: 11/8/23
# Goal: Extract home values data (from NHGIS)
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"

# read in data
home_value <- read_csv(paste0(path_data, "home_value/nhgis0036_ds239_20185_zcta.csv")) %>% 
  rename(zcta = ZCTA5A,
         med_home_value = AJ3QE001)
glimpse(home_value)

zctas_in_study <- read_fst(paste0(path_data, "/power_outages/zcta_outages.fst")) %>% 
  group_by(zcta) %>% 
  filter(row_number() == 1) %>% 
  select(zcta)

# Clean home_value data -------------------------------------------------------
home_value <- home_value %>% 
  filter(zcta %in% zctas_in_study$zcta) %>% 
  select(zcta, med_home_value)

write_csv(home_value, paste0(path_data, "data_process/med_home_value_cleaned.csv"))



