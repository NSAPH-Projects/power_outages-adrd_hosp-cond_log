####***********************
#### Code Description ####
# Author: Vivian  
# Date: 11/29/23
# Goal: Run analysis - main and sensitivity
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
lowest_aic <- read_csv(paste0(path_results, "lowest_aic_season.csv"))
lowest_aic

# Social vulnerability analyses ----------------------------------------------------------------
# vulnerability metrics
# warm (March - Sept) and cool (Oct - April) seasons

custout_thresh <- c("1", "10", "20", ".1", "50")
exp_lag <- c("lag0", "lag1", "lag1_lag2", "lag1_lag3")
geo_strata <- c("nyc", "urban", "rural")
aic_tmax <- c(4, 3, 3)
aic_prcp <- c(2, 2, 4)
em_var <- c("med_home_value_lte_25pctl", "pov_pct_gte10", "age_lte75", "age_lte80", "sex")

em_results <- data.frame()
for (c in 1:length(custout_thresh)){
  for (exp in 1:length(exp_lag)){
    for (g in 1:length(geo_strata)){
      for (e in 1:length(em_var)){
        em_level <- c(na.omit((unique(data4casecross_ind[[em_var[e]]]))))
        print(em_var[e])
        
        for (em in 1:length(em_level)){
          # establish dynamic formula for clogit
          dyn_formula <- as.formula(paste0("Case ~ tot_hr_gte_", custout_thresh[c], "_", exp_lag[exp],
                                           "+ ns(tmax, df = ", aic_tmax[g], ")",
                                           "+ ns(prcp, df = ", aic_prcp[g], ")",
                                           "+ strata(new_id)")
          )
          print(geo_strata[g])
          print(dyn_formula)
          
          mod <- clogit(dyn_formula,
                        method = "efron",
                        data = data4casecross_ind %>% filter(urbanicity == geo_strata[g],
                                                             !!sym(em_var[e]) == em_level[[em]]))
          
          r_geo_strata <- geo_strata[g]
          r_custout_thresh <- custout_thresh[c]
          r_exp_lag <- exp_lag[exp]
          r_em_var <- em_var[e]
          r_em_level <- em_level[em]
          r_aic_tmax <- aic_tmax[g]
          r_aic_prcp <- aic_prcp[g]
          est <- round(exp(coef(mod))[1], 4)
          lci <- round(exp(confint(mod))[1,1], 4)
          uci <- round(exp(confint(mod))[1,2], 4)
          
          n_case_days_total <- nrow(
            data4casecross_ind %>%
              filter(!!sym(em_var[e]) == em_level[[em]],
                     urbanicity == geo_strata[g],
                     Case == 1) %>%
              distinct(new_id)
          )
          n_case_days_contrast <- nrow(
            data4casecross_ind %>%
              filter(!!sym(em_var[e]) == em_level[[em]]) %>% 
              group_by(new_id) %>%
              mutate(n_outages_in_strata = sum(get(
                paste0("tot_hr_gte_", custout_thresh[c], "_", exp_lag[exp] )
              ), na.rm = TRUE)) %>%
              filter(row_number() == 1) %>%
              filter(urbanicity == geo_strata[g],
                     n_outages_in_strata > 0)
          )
          n_case_days_exposed <- nrow(
            data4casecross_ind %>%
              filter(!!sym(em_var[e]) == em_level[[em]],
                     urbanicity == geo_strata[g],
                     Case == 1,
                     if_any(all_of(
                       paste0("tot_hr_gte_", custout_thresh[c], "_", exp_lag[exp] )
                     ), ~ . > 0)) %>%
              distinct(new_id)
          )
          
          results <- cbind(r_geo_strata, r_custout_thresh, r_exp_lag, r_em_var, r_em_level,
                           r_aic_tmax, r_aic_prcp, est, lci, uci, 
                           n_case_days_total, n_case_days_contrast, n_case_days_exposed)
          
          em_results <- rbind(em_results, results)
        }
      }
    }
  }
}

em_results <- em_results %>% 
  arrange(r_geo_strata, as.numeric(r_custout_thresh)) %>% 
  rename(r_hr_custout_thresh = r_custout_thresh)

write_csv(em_results, paste0(path_results, "em_results_social_vars.csv"))


# Seasonal analyses AIC -------------------------------------------------------
# i should re-do the df yes? keeping 3-4 for temperature within warm and cool seems not necessary

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

# Seasonal analyses -------------------------------------------------------
custout_thresh <- c("1", "10", "20", ".1", "50")
geo_strata <- c("nyc", "urban", "rural")
exp_lag <- c("lag0", "lag1", "lag1_lag2", "lag1_lag3")
warm_aic_tmax <- c(3, 1, 2)
warm_aic_prcp <- c(1, 2, 1)
cool_aic_tmin <- c(2, 1, 1)
cool_aic_prcp <- c(1, 1, 3)
season <- c("warm", "cool")

season_results <- data.frame()
for (c in 1:length(custout_thresh)){
  for (exp in 1:length(exp_lag)){
    for (g in 1:length(geo_strata)){
      print(geo_strata[g])
      for (s in 1:length(season)){
        
        if (season[s] == "warm"){

          # establish dynamic formula for clogit
          dyn_formula <- as.formula(paste0("Case ~ tot_hr_gte_", custout_thresh[c], "_", exp_lag[exp] ,
                                           "+ ns(tmax, df = ", warm_aic_tmax[g], ")",
                                           "+ ns(prcp, df = ", warm_aic_prcp[g], ")",
                                           "+ strata(new_id)")
          )

          mod <- clogit(dyn_formula,
                        method = "efron",
                        data = data4casecross_ind %>% filter(urbanicity == geo_strata[g],
                                                             season == "warm"))
          mod_tmp_var <- "tmax"
          n_case_days_total <- nrow(
            data4casecross_ind %>%
              filter(urbanicity == geo_strata[g],
                     season == "warm",
                     Case == 1) %>%
              distinct(new_id)
          )
          n_case_days_contrast <- nrow(
            data4casecross_ind %>%
              group_by(new_id) %>%
              mutate(n_outages_in_strata = sum(get(
                paste0("tot_hr_gte_", custout_thresh[c], "_", exp_lag[exp] )
              ), na.rm = TRUE)) %>%
              filter(row_number() == 1) %>%
              filter(urbanicity == geo_strata[g],
                     season == "warm",
                     n_outages_in_strata > 0)
          )
          n_case_days_exposed <- nrow(
            data4casecross_ind %>%
              filter(urbanicity == geo_strata[g],
                     season == "warm",
                     Case == 1,
                     if_any(all_of(
                       paste0("tot_hr_gte_", custout_thresh[c], "_", exp_lag[exp] )
                     ), ~ . > 0)) %>%
              distinct(new_id)
          )

        }

        if (season[s] == "cool"){

          # establish dynamic formula for clogit
          dyn_formula <- as.formula(paste0("Case ~ tot_hr_gte_", custout_thresh[c], "_", exp_lag[exp] ,
                                           "+ ns(tmin, df = ", cool_aic_tmin[g], ")",
                                           "+ ns(prcp, df = ", cool_aic_prcp[g], ")",
                                           "+ strata(new_id)")
          )

          mod <- clogit(dyn_formula,
                        method = "efron",
                        data = data4casecross_ind %>% filter(urbanicity == geo_strata[g],
                                                             season == "cool"))

          mod_tmp_var <- "tmin"
          n_case_days_total <- nrow(
            data4casecross_ind %>%
              filter(urbanicity == geo_strata[g],
                     season == "cool",
                     Case == 1) %>%
              distinct(new_id)
          )
          n_case_days_contrast <- nrow(
            data4casecross_ind %>%
              group_by(new_id) %>%
              mutate(n_outages_in_strata = sum(get(
                paste0("tot_hr_gte_", custout_thresh[c], "_", exp_lag[exp] )
              ), na.rm = TRUE)) %>%
              filter(row_number() == 1) %>%
              filter(urbanicity == geo_strata[g],
                     season == "cool",
                     n_outages_in_strata > 0)
          )
          n_case_days_exposed <- nrow(
            data4casecross_ind %>%
              filter(urbanicity == geo_strata[g],
                     season == "cool",
                     Case == 1,
                     if_any(all_of(
                       paste0("tot_hr_gte_", custout_thresh[c], "_", exp_lag[exp] )
                     ), ~ . > 0)) %>%
              distinct(new_id)
          )
        }

        mod_geo_strata <- geo_strata[g]
        mod_custout_thresh <- custout_thresh[c]
        mod_exp_lag <- exp_lag[exp]
        mod_season <- season[s]
        est <- round(exp(coef(mod))[1], 4)
        lci <- round(exp(confint(mod))[1,1], 4)
        uci <- round(exp(confint(mod))[1,2], 4)
        output <- cbind(mod_geo_strata, mod_custout_thresh, mod_exp_lag, mod_tmp_var, mod_season, est, lci, uci,
                        n_case_days_total, n_case_days_contrast, n_case_days_exposed)

        season_results <- rbind(season_results, output)
        
      }
    }
  }
}

View(season_results)
write_csv(season_results, paste0(path_results, "em_results_season.csv"))










