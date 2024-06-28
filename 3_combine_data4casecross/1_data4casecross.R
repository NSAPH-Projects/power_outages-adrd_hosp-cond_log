####***********************
#### Code Description ####
# Author: Vivian 
# Date: 10/21/23
# Goal: Combine exposure, outcome, and confounder data
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"

# read in data

# urbanicity
zcta_urbanicity <- read_csv(paste0(path_data, "data_process/zcta_urbanicity.csv"))

# case control days
case_control_2017 <- read_fst(paste0(path_data, "/data_process/case_cntrl_days/cntrl_days_zcta_2017.fst"))
case_control_2018 <- read_fst(paste0(path_data, "/data_process/case_cntrl_days/cntrl_days_zcta_2018.fst"))
case_control_dates <- rbind(case_control_2017, case_control_2018)

# exposure data
po_data <- read_csv(paste0(path_data, "/data_process/power_outage_metrics/po4casecross.csv")) %>% 
  rename(DayDateTime = day) %>% 
  mutate(zcta = as.character(zcta))

# outcome data
# create new id for individual id
cvd_hosp_w_bene <- read_csv(paste0(path_data, "data_process/cvd_hosp/cvd_hosp_w_bene.csv")) %>% 
  rename(DayDateTime = admission_date) %>% 
  mutate(new_id = 1:n()) 

# confounder temperature/precipitation data
precip_temp_data <- read_csv(paste0(path_data, "data_process/daymet_ny_cleaned.csv")) 

# vulnerability metrics
home_value <- read_csv(paste0(path_data, "data_process/med_home_value_cleaned.csv"))
poverty <- read_csv(paste0(path_data, "data_process/poverty_cleaned.csv"))


# check names of all datasets
glimpse(case_control_dates)
glimpse(po_data)
glimpse(cvd_hosp_w_bene)
glimpse(precip_temp_data)

summary(as.numeric(case_control_dates$zcta))

# Join confounders --------------------------------------------------------------------
# join exposures and confounders
exp_conf <- case_control_dates %>%
  left_join(.,
            po_data %>%
              mutate(zcta = as.double(zcta)),
            by = c("DayDateTime", "zcta")) %>%
  left_join(.,
            precip_temp_data  %>%
              mutate(zcta = as.double(zcta)),
            by = c("DayDateTime" = "date", "zcta"))

# link exp_conf and outcome data; every bene should have their own strata of case and control
data4casecross_ind <- cvd_hosp_w_bene %>% 
  left_join(., exp_conf, by = c("zcta" = "zcta", "DayDateTime" = "CaseDateTime")) %>% 
  mutate_at(vars(starts_with("tot_")), ~ifelse(is.na(.), 0, .))


# Add urbanicity again ----------------------------------------------------
data4casecross_ind <- data4casecross_ind %>% 
  select(-urbanicity) %>% 
  left_join(., zcta_urbanicity %>% 
              mutate(zcta = as.double(zcta)) %>% 
              select(zcta, urbanicity))

# should be 0 (yes)
missing_urbanicity <- data4casecross_ind %>% 
  filter(is.na(urbanicity))

# create subset data for investigation
invest_dist <- data4casecross_ind %>% 
  distinct(bene_id, new_id, .keep_all = TRUE)

# Join vulnerability metrics ----------------------------------------------
data4casecross_ind <- data4casecross_ind %>% 
  left_join(., home_value)

# how many without home value?
invest_home_value <- data4casecross_ind %>% 
  filter(is.na(med_home_value)) %>% 
  group_by(zcta) %>% 
  filter(row_number() == 1) #107
table(invest_home_value$urbanicity) # mostly in rural areas

# distribution of income levels
home_value_nyc <- data4casecross_ind %>% 
  filter(urbanicity == "nyc")
summary(home_value_nyc$med_home_value)

# bottom 25th percentile
med_home_value_threshold <- data4casecross_ind %>% 
  group_by(urbanicity) %>%
  summarize(med_home_value_25pctl = quantile(med_home_value, 0.25, na.rm = TRUE))

data4casecross_ind <- data4casecross_ind %>% 
  left_join(., med_home_value_threshold, by = "urbanicity") %>% 
  mutate(med_home_value_lte_25pctl = ifelse(med_home_value <= med_home_value_25pctl, 1, 0))


# poverty -----------------------------------------------------------------
data4casecross_ind <- data4casecross_ind %>% 
  left_join(., poverty)

hist(data4casecross_ind$pov_pct)
summary(data4casecross_ind$pov_pct)

data4casecross_ind <- data4casecross_ind %>% 
  left_join(., poverty) %>% 
  mutate(pov_pct_gte5 = ifelse(pov_pct > 5, 1, 0),
         pov_pct_gte10 = ifelse(pov_pct > 10, 1, 0))

# age ---------------------------------------------------------------------
hist(invest_dist$age_dob)
summary(invest_dist$age_dob)

data4casecross_ind <- data4casecross_ind %>% 
  mutate(age_lte75 = ifelse(age_dob <= 75, 1, 0),
         age_lte80 = ifelse(age_dob <= 80, 1, 0))


# Season ------------------------------------------------------------------
# warm: may - sept (5 months)
# cool: oct - april (7 months)
data4casecross_ind <- data4casecross_ind %>% 
  mutate(season = ifelse(month(DayDateTime.y) %in% (5:9), "warm", "cool"))

# write data --------------------------------------------------------------
write_csv(data4casecross_ind, paste0(path_data, "data_process/data4casecross/data4casecross_ind.csv"))
  
# how correlated are poverty and median home income?
data4casecross_ind <- read_csv(paste0(path_data, "data_process/data4casecross/data4casecross_ind.csv"))

cor(data4casecross_ind$pov_pct_gte10, data4casecross_ind$med_home_value_25pctl, use = "complete.obs") #0.2820698

