####***********************
#### Code Description ####
# Author: Vivian
# Date: 6/28/24
# Goal: create control days for each hospital admission
# Note: this is a sensitivity analysis where we exclude hypertension from our data
####**********************

####********************
#### 0: Preparation #### 
####********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))

path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"
path_data_external <- paste0(here(path_data, "data_process/case_cntrl_days/"))

# collapse to zcta day data because we are most interested in identifying zcta-day combinations for controls
# exclude hypertension only admissions
cvd_hosp_w_bene <- read_csv(paste0(path_data, "data_process/cvd_hosp/cvd_hosp_w_bene.csv")) %>% 
  rename(CaseDateTime = admission_date) %>% 
  group_by(zcta, CaseDateTime) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(-i_hyper) %>% 
  filter(i_myo_infarc + i_isch_stroke + i_atr_fib > 0) %>% 
  select(CaseDateTime, zcta)
  
# is each row is a unique spatial unit / datetime observation?
nrow(cvd_hosp_w_bene %>% distinct(CaseDateTime, zcta)) == nrow(cvd_hosp_w_bene)

# 0d Add year variable to split dataset by year (for computational speed)
cvd_hosp_w_bene$YYYY <- year(cvd_hosp_w_bene$CaseDateTime)

# 0e Add a unique id variable & spatial unit variable
cvd_hosp_w_bene <- cvd_hosp_w_bene %>% ungroup() %>% mutate(id = 1:n(), spatialUnit = 'zcta')

# 0f Split data into a list of yearly dataframes
years.list_zcta <- cvd_hosp_w_bene %>% split(cvd_hosp_w_bene$YYYY)


####*************************************************
#### 1: Define Functions to Create Control days ####
####*************************************************

# Here I edit functions developed by Sebastian Rowland

make_control_day <- function(days1, BeforeAfter, WK){ 
  # The name of the day; accounts for if control or case
  VarName <- paste0(BeforeAfter, '_', str_trunc(WK, 1, 'left', ''))    
  # adds WKs number of weeks, preserves day
  days1 %>% mutate(!!VarName := CaseDateTime+ as.period(7 * WK, 'day'))  
}

# Define function to create control days
create_control_days_by_year <- function(df.YYYY){
  
  # Use function to create bidirectionally symmetric datedays 
  days1 <-  df.YYYY
  ActiveYYYY <- days1$YYYY[1]
  days1 <- make_control_day(days1, 'Before', -4)
  days1 <- make_control_day(days1, 'Before', -3)
  days1 <- make_control_day(days1, 'Before', -2)
  days1 <- make_control_day(days1, 'Before', -1)
  days1 <- make_control_day(days1, 'Caseday', 0)
  days1 <- make_control_day(days1, 'After', 1)
  days1 <- make_control_day(days1, 'After', 2)
  days1 <- make_control_day(days1, 'After', 3)
  days1 <- make_control_day(days1, 'After', 4)
  
  # Put in long format by dayName
  days2 <- days1 %>%
    gather('dayName', 'DayDateTime', contains('Caseday_'),
           contains('Before_'), contains('After_') )
  
  # Stratify by month of event; keep only other obs in the month
  days3 <- days2 %>% filter(month(CaseDateTime) == month(DayDateTime))
  
  # Identify spatial unit
  spatialUnit <- ifelse(days3$spatialUnit[1] == 'zcta', 'zcta', 'zcta')
  
  # Save results
  days3 %>%
    fst::write_fst(paste0(path_data_external, 'cntrl_days_', spatialUnit,
                          '_', ActiveYYYY, '_wo_hypertension.fst'))
}



####************************************
#### 2: Test Function w Single Year ####
####************************************

case_control_2017 <- create_control_days_by_year(years.list_zcta[[1]][1:100,])
head(case_control_2017)

####*****************************************
#### 3: Create control days for all years ####
####*****************************************
for (i in seq_along(1:length(years.list_zcta))){
  create_control_days_by_year(years.list_zcta[[i]])
}


# check
case_control_2017 <- read_fst(paste0(path_data_external, 'cntrl_days_zcta_2017_wo_hypertension.fst'))




