####***********************
#### Code Description ####
# Author: Vivian  
# Date: 10/9/23
# Goal: Identify cvd outcomes from medicare data
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))

# set data paths
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"


# read in medpar hospitalization data across years ------------------------

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

years <- c("2017", "2018")
benes <- data.frame()
hosp <- data.frame()

# beneficiaries - do this separately from hosp because beneficiaries and medpar hospitalizations data are not divide by admission year
for (year in 1:length(years)){
  # beneficiary data - keeping only zips that match valid NY zips 
  benes_year <- read_parquet(paste0(path_data, "mbsf_medpar_denom/mbsf_medpar_denom_", years[year], ".parquet")) %>% 
    filter(zip %in% valid_zips_ny$zip)
  
  benes <- bind_rows(benes, benes_year)
}

# hospitalizations 
for (year in years){
  # hospitalization data 
  hosp_year <- read_parquet(paste0(path_data, "mbsf_medpar_denom/medpar_hospitalizations_", year, ".parquet")) 
  hosp <- bind_rows(hosp, hosp_year)
}

nrow(benes %>% group_by(year, bene_id) %>% filter(row_number() == 1)) 

# join hosp and benes
hosp_2017_2018 <- hosp %>% 
  mutate(admission_year = year(admission_date)) %>% 
  filter(admission_year %in% c("2017", "2018")) %>% 
  select(-year)

hosp_2017_2018_w_benes <- left_join(hosp_2017_2018, benes, by = c("bene_id" = "bene_id", "admission_year" = "year")) %>% 
  filter(!is.na(zip))

# create diagnosis variables - un-array diagnosis variable bc diagnosis var originally array
# Initialize empty lists to store the extracted arrays
position1 <- list()
position2 <- list()
position3 <- list()
position4 <- list()

# Loop through each row and extract the instances of that diagnosis list variable
for (i in 1:nrow(hosp_2017_2018_w_benes)) {
  row <- hosp_2017_2018_w_benes$diagnoses[[i]]
  
  # Extract the first instance
  diag1 <- row[1]
  position1[[i]] <- diag1
  
  # Extract the second instance if it exists (length of the array is at least 2)
  if (length(row) >= 2) {
    diag2 <- row[2]
    position2[[i]] <- diag2
  } else {
    # If the array has only one element, store NA for the second instance
    position2[[i]] <- NA
  }
  
  if (length(row) >= 3) {
    diag3 <- row[3]
    position3[[i]] <- diag3
  } else {
    position3[[i]] <- NA
  }
  
  if (length(row) >= 4) {
    diag4 <- row[4]
    position4[[i]] <- diag4
  } else {
    position4[[i]] <- NA
  }
  
}

# create new variable with each diagnosis position
hosp_2017_2018_w_benes$diag1 <- unlist(position1)
hosp_2017_2018_w_benes$diag2 <- unlist(position2)
hosp_2017_2018_w_benes$diag3 <- unlist(position3)
hosp_2017_2018_w_benes$diag4 <- unlist(position4)

# Ready the study icd codes of interest (for cvd) -------

# codes from rowland papers (the excel was created outside fasse and uploaded for data wrangling)
# https://www.sciencedirect.com/science/article/pii/S0013935121015309
# https://www.sciencedirect.com/science/article/pii/S0160412020318651
icd9_codes_from_prev_studies <- read_excel("/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/icd9_codes_from_prev_studies.xlsx") %>% 
  rename(icd9 = `ICD 9`) %>% 
  mutate(icd9 = gsub("\\.", "", icd9))

# generalized equivalence mappings for 2017, 2018
icd_xwalk_2017 <- read.table(paste0(path_data, "2017-Diagnosis-GEMS (1)/2017_I9gem.txt")) %>% 
  rename(icd9 = V1,
         icd10_2017 = V2) %>% 
  select(-V3)

icd_xwalk_2018 <- read.table(paste0(path_data, "diagnosis_gems_2018 (1)/2018_I9gem.txt")) %>% 
  rename(icd9 = V1,
         icd10_2018 = V2) %>% 
  select(-V3)

# identify cvd icd10 codes for 2017, 2018 and dedupe, keeping only unique codes
# get unique icd10 for 2017
icd10_2017 <- icd9_codes_from_prev_studies %>% 
  left_join(icd_xwalk_2017) %>% 
  group_by(icd10_2017) %>% 
  filter(!is.na(icd10_2017),
         row_number() == 1) %>% 
  mutate(icd10 = icd10_2017,
         year = 2017) %>% 
  ungroup() %>% 
  select(-c(icd9, icd10_2017))

# get unique icd10 for 2018
icd10_2018 <- icd9_codes_from_prev_studies %>% 
  left_join(icd_xwalk_2018) %>% 
  group_by(icd10_2018) %>% 
  filter(!is.na(icd10_2018),
         row_number() == 1) %>% 
  mutate(icd10 = icd10_2018,
         year = 2018) %>% 
  ungroup() %>% 
  select(-c(icd9, icd10_2018))

# get unique icd10 for 2017, 2018
list_icd10 <- rbind(icd10_2017, icd10_2018) %>% 
  group_by(icd10) %>% 
  filter(row_number() == 1) %>% 
  select(-year)

list_myo_infarc <- list_icd10 %>% filter(Diagnosis == "Myocardial Infarction")
list_isch_stroke <- list_icd10 %>% filter(Diagnosis == "Ischemic Stroke")
# list_hem_stroke <- list_icd10 %>% filter(Diagnosis == "Hemorrhagic Stroke")
list_hyper <- list_icd10 %>% filter(Diagnosis == "Hypertension")
list_atr_fib <- list_icd10 %>% filter(Diagnosis == "Atrial Fibrillation")

# filter to cvd outcomes of interest -------
# filter hospitalizations to cvd diagnoses based on the list of icd10 codes
cvd_hosp_w_bene <- hosp_2017_2018_w_benes %>% 
  filter(diag1 %in% list_icd10$icd10 |
         diag2 %in% list_icd10$icd10 |
         diag3 %in% list_icd10$icd10 |
         diag4 %in% list_icd10$icd10) 
 
length(unique(cvd_hosp_w_bene$bene_id)) #245452

# add type of cvd event
cvd_hosp_w_bene <- cvd_hosp_w_bene %>% 
  rowwise() %>% 
  mutate(i_myo_infarc = as.integer(any(c_across(starts_with("diag")) %in% list_myo_infarc$icd10)),
         i_isch_stroke = as.integer(any(c_across(starts_with("diag")) %in% list_isch_stroke$icd10)),
         # i_hem_stroke = as.integer(any(c_across(starts_with("diag")) %in% list_hem_stroke$icd10)),
         i_hyper = as.integer(any(c_across(starts_with("diag")) %in% list_hyper$icd10)),
         i_atr_fib = as.integer(any(c_across(starts_with("diag")) %in% list_atr_fib$icd10)))

cvd_hosp_w_bene <- cvd_hosp_w_bene %>% 
  select(bene_id, admission_date, zip, zcta, starts_with("diag"), -diagnoses, age_dob, sex, starts_with("i_"))

nrow(cvd_hosp_w_bene %>% filter(is.na(zip))) #no missing zips - yay

write_csv(cvd_hosp_w_bene, paste0(path_data, "data_process/cvd_hosp/cvd_hosp_w_bene.csv"))

# test <- read_csv(paste0(path_data, "data_process/cvd_hosp/cvd_hosp_w_bene.csv"))

# Checks ------------------------------------------------------------------
# what does spatial distribution of outcomes look like?
cvd_hosp_w_bene <- read_csv(paste0(path_data, "data_process/cvd_hosp/cvd_hosp_w_bene.csv")) %>% 
  mutate(year = year(admission_date)) %>% 
  left_join(., valid_zips_ny)

cvd_hosp_w_bene_agg <- cvd_hosp_w_bene %>% 
  group_by(ZCTA) %>% 
  mutate(n_cvd_hosp = n()) %>% 
  filter(row_number() == 1)

# read in shapefile
zcta_shapefile <- st_read(paste0(path_data, "/shapefile_zcta2010/US_zcta_2010.shp")) %>% 
  rename(ZCTA = ZCTA5CE10)

map_cvd <- full_join(zcta_shapefile, cvd_hosp_w_bene_agg, by = ("ZCTA")) %>% 
  filter(!is.na(zip))

ggplot(map_cvd) + 
  scale_fill_viridis_c(option = "magma",
                       na.value = "grey",
                       direction = -1) +
  geom_sf(aes(fill = n_cvd_hosp),
          size = .05,
          color = "black") +
  labs(fill = paste0("Spatial distribution of cvd outcomes - zcta")) +
  theme(legend.text = element_text(size = 7)) +
  theme_map() +
  coord_sf(crs = st_crs("ESRI:102003")) 

# what are the most common types of cvd outcomes?
glimpse(cvd_hosp_w_bene)
