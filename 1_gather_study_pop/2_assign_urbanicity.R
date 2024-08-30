####***********************
#### Code Description ####
# Author: Vivian  
# Date: 10/13/23
# Goal: Assign urbanicity to each zip code
# urbanicity would be: NYC, non-NYC urban, and rural
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))

# set data paths
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"

# zips - valid list of zips from UDS Mapper
valid_zips_2017 <- read_excel(paste0(path_data, "zip_zcta/ZIPCodetoZCTACrosswalk2017UDS.xlsx")) %>% 
  filter(STATE == "NY") %>% 
  mutate(year = 2017)

valid_zips_2018 <- read_excel(paste0(path_data, "zip_zcta/ZIPCodetoZCTACrosswalk2018UDS.xlsx")) %>% 
  filter(STATE == "NY") %>% 
  mutate(year = 2018)

valid_zips_ny <- rbind(valid_zips_2017, valid_zips_2018) %>% 
  mutate(zip = ZIP_CODE) %>% 
  select(-ZIP_CODE)

# nyc zctas
nyc_zctas <- read_csv(paste0(path_data, "urban_rural_pop/Modified_Zip_Code_Tabulation_Areas__MODZCTA__20231108.csv")) %>% 
  select(ZCTA) %>% 
  rename(zcta = ZCTA) %>% 
  separate_rows(zcta, sep = ', ') %>%
  distinct()
  
# read in zcta data for urbanicity; used 2010 uc census variable
# nyc zctas will be labeled nyc
# zctas with >50% rural population are rural, else urban 

zcta_urbanicity <- read_csv(paste0(path_data, "urban_rural_pop/nhgis0045_ds258_2020_zcta.csv")) %>% 
  select(ZCTAA, starts_with("U7I")) %>% 
  rename(zcta = ZCTAA,
         total = U7I001,
         urban = U7I002,
         rural = U7I003) %>% 
  mutate(urbanicity = case_when(zcta %in% nyc_zctas$zcta ~ "nyc",
                                rural/total > 0.5 ~ "rural",
                                rural/total <= 0.5 ~ "urban")) %>% 
  filter(zcta %in% valid_zips_ny$ZCTA) %>% 
  write_csv(., paste0(path_data, "data_process/zcta_urbanicity.csv"))

length(unique(zcta_urbanicity$zcta)) #1785 unique zctas, which is NY state specific number

# Check spatial distribution ----------------------------------------------
zcta_urbanicity <- read_csv(paste0(path_data, "data_process/zcta_urbanicity.csv"))

# read in shapefile
zcta_shapefile <- st_read(paste0(path_data, "/shapefile_zcta2010/US_zcta_2010.shp")) %>% 
  rename(zcta = ZCTA5CE10) %>% 
  # mutate(zcta = as.integer(zcta)) %>% 
  left_join(., zcta_urbanicity %>% 
              mutate(in_nys = 1)) %>% 
  filter(in_nys == 1)
nrow(zcta_shapefile)

ggplot(zcta_shapefile) + 
  geom_sf(aes(fill = urbanicity),
          size = .05,
          color = "black") +
  labs(fill = paste0("Spatial distribution of urbanicity - zcta")) +
  theme(legend.text = element_text(size = 7)) +
  theme_map() +
  coord_sf(crs = st_crs("ESRI:102003")) 




