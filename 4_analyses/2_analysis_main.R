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

# Set AIC -----------------------------------------------------------------
lowest_aic <- read_csv(paste0(path_results, "lowest_aic.csv"))
lowest_aic

# Model - different custout threshold, exposure lags, geo strata ----------------------------------------------------------------
custout_thresh <- c("1", "10", "20", ".1", "50")
exp_lag <- c("lag0", "lag1", "lag1_lag2", "lag1_lag3")
geo_strata <- c("nyc", "urban", "rural")
aic_tmax <- c(4, 3, 3)
aic_prcp <- c(2, 2, 4)

main_results <- data.frame()
for (c in 1:length(custout_thresh)){
  for (e in 1:length(exp_lag)){
    for (g in 1:length(geo_strata)){
      
      # establish dynamic formula for clogit
      dyn_formula <- as.formula(paste0("Case ~ tot_hr_gte_", custout_thresh[c], "_", exp_lag[e],
                                       "+ ns(tmax, df = ", aic_tmax[g], ")",
                                       "+ ns(prcp, df = ", aic_prcp[g], ")",
                                       "+ strata(new_id)")
      )
      print(geo_strata[g])
      print(dyn_formula)
      
      mod <- clogit(dyn_formula,
                    method = "efron",
                    data = data4casecross_ind %>% filter(urbanicity == geo_strata[g]))
      
      r_geo_strata <- geo_strata[g]
      r_lag <- exp_lag[e]
      r_custout_thresh <- custout_thresh[c]
      r_aic_tmax <- aic_tmax[g]
      r_aic_prcp <- aic_prcp[g]
      est <- round(exp(coef(mod))[1], 4)
      lci <- round(exp(confint(mod))[1,1], 4)
      uci <- round(exp(confint(mod))[1,2], 4)
      n_case_days_total <- nrow(
        data4casecross_ind %>%
          filter(urbanicity == geo_strata[g],
                 Case == 1) %>%
          distinct(new_id)
      )
      n_case_days_contrast <- nrow(
        data4casecross_ind %>%
          group_by(new_id) %>%
          mutate(n_outages_in_strata = sum(get(
            paste0("tot_hr_gte_", custout_thresh[c], "_", exp_lag[e])
          ), na.rm = TRUE)) %>%
          filter(row_number() == 1) %>%
          filter(urbanicity == geo_strata[g],
                 n_outages_in_strata > 0)
      )
      n_case_days_exposed <- nrow(
        data4casecross_ind %>%
          filter(urbanicity == geo_strata[g],
                 Case == 1,
                 if_any(all_of(
                   paste0("tot_hr_gte_", custout_thresh[c], "_", exp_lag[e])
                 ), ~ . > 0)) %>%
          distinct(new_id)
      )
      
      
      results <- cbind(r_geo_strata, r_lag, r_custout_thresh, r_aic_tmax, r_aic_prcp, est, lci, uci, n_case_days_total, n_case_days_contrast, n_case_days_exposed)
      
      
      main_results <- rbind(main_results, results)
      
    }
  }
}

main_results <- main_results %>% 
  arrange(r_geo_strata, as.numeric(r_custout_thresh)) %>% 
  rename(r_hr_custout_thresh = r_custout_thresh)

write_csv(main_results, paste0(path_results, "main_results.csv"))

