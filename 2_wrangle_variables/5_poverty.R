####***********************
#### Code Description ####
# Author: Vivian 
# Date: 11/30/23
# Goal: Extract % poverty data (from NHGIS)
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"

# read in data
poverty <- read_csv(paste0(path_data, "poverty/nhgis0038_csv/nhgis0038_ds254_20215_zcta.csv")) %>% 
  select(ZCTA5A, AOXWE001:AOXWE003) %>% 
  rename(zcta = ZCTA5A,
         total = AOXWE001,
         below_50pct_pov = AOXWE002,
         below_99pct_pov = AOXWE003) %>% 
  mutate(pov_pct = (below_50pct_pov + below_99pct_pov)/total * 100,
         pov_pct_gte25 = ifelse(pov_pct >= 25, 1, 0),
         pov_pct_gte50 = ifelse(pov_pct >= 50, 1, 0))
  

zctas_in_study <- read_fst(paste0(path_data, "/power_outages/zcta_outages.fst")) %>% 
  group_by(zcta) %>% 
  filter(row_number() == 1) %>% 
  select(zcta)

# Clean home_value data -------------------------------------------------------
poverty <- poverty %>% 
  filter(zcta %in% zctas_in_study$zcta) %>% 
  select(zcta, starts_with("pov_"))

write_csv(poverty, paste0(path_data, "data_process/poverty_cleaned.csv"))




