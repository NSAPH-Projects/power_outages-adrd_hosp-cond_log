####***********************
#### Code Description ####
# Author: Vivian  
# Date: 11/29/23
# Goal: Run analysis - main
# spatial scale: zcta
# temporal scale: daily
# study design - case crossover
# statistical analysis - conditional logistic regression
# exposure = variables ending in "_lag1" are the total hours of customers without power over X threshold for the day before admission
# outcome = cvd hospitalization
# confounders = max temperature, precipitation
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

# Seasonal analyses AIC -------------------------------------------------------
tmp_df <- c(1:3)
prcp_df <- c(1:3)
geo_strata <- c("nyc", "urban", "rural")
season <- c("warm", "cool")

table_aic_season <- data.frame()
for (t in 1:length(tmp_df)){
  for (p in 1:length(prcp_df)){
    print(tmp_df[t])
    print(prcp_df[p])
    
    for (g in 1:length(geo_strata)){
      for (s in 1:length(season)){
        if (season[s] == "warm"){
          model <- clogit(Case ~ tot_hr_gte_10 +
                            ns(tmax, df = tmp_df[t]) +            
                            ns(prcp, df = prcp_df[p]) +       
                            strata(new_id),
                          method = "efron",
                          data = data4casecross_ind %>% filter(urbanicity == geo_strata[g],
                                                               season == season[s]))
          mod_tmp_var <- "tmax"
        }
        
        if (season[s] == "cool"){
          model <- clogit(Case ~ tot_hr_gte_10 +
                            ns(tmin, df = tmp_df[t]) +            
                            ns(tmin, df = prcp_df[p]) +       
                            strata(new_id),
                          method = "efron",
                          data = data4casecross_ind %>% filter(urbanicity == geo_strata[g],
                                                               season == season[s]))
          
          mod_tmp_var <- "tmin"
        }
        
        mod_tmp_df <- tmp_df[t]
        mod_prcp_df <- prcp_df[p]
        mod_aic <- AIC(model)
        mod_geo_strata <- geo_strata[g]
        mod_season <- season[s]
        
        output <- cbind(mod_geo_strata, mod_tmp_var, mod_tmp_df, mod_prcp_df, mod_aic, mod_season)
        
        table_aic_season <- rbind(table_aic_season, output)
      }
    }
  }
}

table_aic_season_lowest <- table_aic_season %>% 
  group_by(mod_geo_strata, mod_season) %>% 
  filter(mod_aic == min(mod_aic)) 

write_csv(table_aic_season_lowest, paste0(path_results, "lowest_aic_season.csv"))










