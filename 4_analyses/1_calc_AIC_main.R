####***********************
#### Code Description ####
# Author: Vivian  
# Date: 11/29/23
# Goal: calculate AIC for each geographic strata, range for temperature is 3-4, precipitation is 2-4
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
set.seed(219)

# set data paths
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"
path_results <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/results/"

# read in data
data4casecross_ind <- read_csv(paste0(path_data, "data_process/data4casecross/data4casecross_ind.csv")) %>% 
  mutate(Case = ifelse(dayName == 'Caseday_0', 1, 0))   # bivariate Case variable

# Check data --------------------------------------------------------------
length(unique(data4casecross_ind$bene_id)) # 245452 distinct enrollees
nrow(data4casecross_ind %>% filter(urbanicity == "nyc") %>% distinct(new_id)) 
nrow(data4casecross_ind %>% filter(urbanicity == "urban") %>% distinct(new_id))
nrow(data4casecross_ind %>% filter(urbanicity == "rural") %>% distinct(new_id)) 
nrow(data4casecross_ind %>% filter(dayName == "Caseday_0")) 

# number of zctas per geographic area
nrow(data4casecross_ind %>% filter(urbanicity == "nyc") %>% distinct(zcta)) 
nrow(data4casecross_ind %>% filter(urbanicity == "urban") %>% distinct(zcta))
nrow(data4casecross_ind %>% filter(urbanicity == "rural") %>% distinct(zcta))

# AIC ---------------------------------------------------------------------
tmax_df <- c(3:4)
prcp_df <- c(2:4)
geo_strata <- c("nyc", "urban", "rural")
table_aic <- data.frame()

for (t in 1:length(tmax_df)){
  for (p in 1:length(prcp_df)){
    print(tmax_df[t])
    print(prcp_df[p])
    
    for (g in 1:length(geo_strata)){
      print(geo_strata[g])
      
      model <- clogit(Case ~ tot_hr_gte_10_lag1 +
                        ns(tmax, df = tmax_df[t]) +            
                        ns(prcp, df = prcp_df[p]) +       
                        strata(new_id),
                      method = "efron",
                      data = data4casecross_ind %>% filter(urbanicity == geo_strata[g]))
      
      mod_tmax_df <- tmax_df[t]
      mod_prcp_df <- prcp_df[p]
      mod_aic <- AIC(model)
      mod_geo_strata <- geo_strata[g]
      
      output <- cbind(mod_geo_strata, mod_tmax_df, mod_prcp_df, mod_aic)
      
      table_aic <- rbind(table_aic, output)
    }
  }
}

table_aic_lowest <- table_aic %>% 
  group_by(mod_geo_strata) %>% 
  filter(mod_aic == min(mod_aic)) 

write_csv(table_aic_lowest, paste0(path_results, "lowest_aic.csv"))




