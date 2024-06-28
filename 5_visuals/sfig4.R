####***********************
#### Code Description ####
# Author: Vivian  
# Date: 1/18/24
# Goal: generate visualization for sfigure 4
# Figure 4 = effect estimates by effect modifiers, filter to po Cutpoint 10%, lag 1 
# forest plot taken from https://www.khstats.com/blog/forest-plots/
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
library(grid)

# set data paths
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"
path_results <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/results/"

# Cutpoint 10%, Lag 1-3 (supplement) ----------------------------------------------------

# read in results, filtering to Cutpoint 10% & lag 2
em_results_social_vars <- read_csv(paste0(path_results, "em_results_social_vars.csv")) %>% 
  filter(r_hr_custout_thresh == "10",
         r_exp_lag == "lag1_lag3",
         r_em_var != "age_lte75",
         !(r_em_var == "sex" & r_em_level == 0)) %>% 
  rename(em_var = r_em_var,
         em_level = r_em_level) %>% 
  select(-r_aic_tmax, -r_aic_prcp)

head(as.data.frame(em_results_social_vars), n = 25)

em_results_season <- read_csv(paste0(path_results, "em_results_season.csv")) %>% 
  mutate(em_var = "season") %>% 
  filter(mod_custout_thresh == "10",
         mod_exp_lag == "lag1") %>% 
  rename(r_geo_strata = mod_geo_strata,
         r_hr_custout_thresh = mod_custout_thresh,
         r_exp_lag = mod_exp_lag,
         em_level = mod_season) %>% 
  dplyr::select(-mod_tmp_var)

# join social and meterological vars
em_results <- rbind(em_results_social_vars, em_results_season)

# wrangle data
em_results_2 <- em_results %>% 
  select(r_geo_strata, r_exp_lag, n_case_days_exposed, est, lci, uci) %>% 
  group_by(r_geo_strata) %>%
  arrange(r_geo_strata) %>%
  slice(1) %>%
  bind_rows(em_results) %>%
  mutate("Rate Ratio (95% CI)" = paste0(sprintf("%.2f", round(est, 2)), " (", sprintf("%.2f", round(lci, 2)), "-", sprintf("%.2f", round(uci, 2)), ")")) %>% 
  group_by(r_geo_strata) %>% 
  mutate(Urbanicity = case_when(row_number(r_geo_strata) == 1 & r_geo_strata == "nyc" ~ "NYC",
                                row_number(r_geo_strata) == 1 & r_geo_strata == "urban" ~ "Non-NYC urban",
                                row_number(r_geo_strata) == 1 & r_geo_strata == "rural" ~ "Rural")) %>% 
  arrange(r_geo_strata) %>% 
  rename("N exposed" = n_case_days_exposed) %>% 
  mutate("Effect modifier" = case_when(em_var == "med_home_value_lte_25pctl" & em_level == 1 ~ "Median household value ≤ 25th percentile",
                                       em_var == "med_home_value_lte_25pctl" & em_level == 0 ~ "Median household value > 25th percentile",
                                       em_var == "pov_pct_gte10" & em_level == 1 ~ "Percent poverty ≥ 10%",
                                       em_var == "pov_pct_gte10" & em_level == 0 ~ "Percent poverty < 10%",
                                       em_var == "age_lte80" & em_level == 1 ~ "Aged 65-80",
                                       em_var == "age_lte80" & em_level == 0 ~ "Aged >80",
                                       em_var == "sex" & em_level == 1 ~ "Male",
                                       em_var == "sex" & em_level == 2 ~ "Female",
                                       em_var == "season" & em_level == "warm" ~ "Warm season",
                                       em_var == "season" & em_level == "cool" ~ "Cool season")) %>% 
  ungroup() %>% 
  select(Urbanicity, "Effect modifier", "N exposed", "Rate Ratio (95% CI)", est, lci, uci) %>% 
  mutate(across(!Urbanicity, ~ ifelse(!is.na(Urbanicity), NA, .))) %>% 
  mutate(across(!c("est", "lci", "uci"), ~ ifelse(is.na(.), "", as.character(.)))) %>% 
  mutate(" " = "                               ",
         size = 0.2)

em_results_2 <- as.data.frame(em_results_2) %>% 
  slice(c(1:11, 23:33, 12:22))

ptheme <- forest_theme(base_size = 10,
                       ci_pch = 18,
                       ci_fill = "black")

p <- forest(em_results_2 %>% select(Urbanicity, "Effect modifier", "N exposed", "Rate Ratio (95% CI)", " "),
            est = em_results_2$est,
            lower = em_results_2$lci,
            upper = em_results_2$uci,
            ci_column = 5,
            ref_line = 1,
            xlim = c((0.6), (1.5)),
            xlog = TRUE,
            ticks_at = c(0.6, 0.8, 1, 1.2, 1.5),
            sizes = em_results_2$size/10,
            theme = ptheme)
p

ggsave(plot = p,
       "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/figures/fig4_thresh10_lag1_lag3.png",
       dpi=300,
       height=10, width=15, units="in")


