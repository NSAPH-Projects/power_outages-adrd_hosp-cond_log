####***********************
#### Code Description ####
# Author: Vivian  
# Date: 1/18/24
# Goal: generate visualization for figure 3
# Figure 3 = effect estimates by urbanicity, set at Cutpoint 10%, all lags (lag 0, lag 1, lag 1-2, lag 1-3) 
# forest plot taken from https://www.khstats.com/blog/forest-plots/
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
library(grid)
library(forestploter)

# set data paths
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"
path_results <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/results/"

# read in results, filtering to Cutpoint 10%
main_results <- read_csv(paste0(path_results, "main_results.csv")) %>% 
  filter(r_hr_custout_thresh == "10") %>% 
  mutate(model = paste0(r_geo_strata, "_", r_lag, "_", r_hr_custout_thresh))


head(main_results, n = 20)


# Wrangle data ------------------------------------------------------------
main_results_2 <- main_results %>% 
  select(r_geo_strata, r_lag, n_case_days_exposed, est, lci, uci) %>% 
  group_by(r_geo_strata) %>%
  arrange(r_geo_strata) %>%
  slice(1) %>%
  bind_rows(main_results) %>%
  mutate("Rate Ratio (95% CI)" = paste0(sprintf("%.2f", round(est, 2)), " (", sprintf("%.2f", round(lci, 2)), "-", sprintf("%.2f", round(uci, 2)), ")")) %>% 
  group_by(r_geo_strata) %>% 
  mutate(Urbanicity = case_when(row_number(r_geo_strata) == 1 & r_geo_strata == "nyc" ~ "NYC",
                                row_number(r_geo_strata) == 1 & r_geo_strata == "urban" ~ "Non-NYC urban",
                                row_number(r_geo_strata) == 1 & r_geo_strata == "rural" ~ "Rural"),
         r_lag = case_when(r_lag == "lag0" ~ "Lag 0",
                           r_lag == "lag1" ~ "Lag 1",
                           r_lag == "lag1_lag2" ~ "Lags 1-2",
                           r_lag == "lag1_lag3" ~ "Lags 1-3")) %>% 
  arrange(r_geo_strata) %>% 
  rename("Exposure period" = r_lag,
         "N exposed" = n_case_days_exposed) %>% 
  ungroup() %>% 
  select(Urbanicity, "Exposure period", "N exposed", "Rate Ratio (95% CI)", est, lci, uci) %>% 
  mutate(across(!Urbanicity, ~ ifelse(!is.na(Urbanicity), NA, .))) %>% 
  mutate(across(!c("est", "lci", "uci"), ~ ifelse(is.na(.), "", as.character(.)))) %>% 
  mutate(" " = "                               ",
         size = 0.2) 

main_results_2 <- as.data.frame(main_results_2) %>% 
  slice(c(1:5, 11:15, 6:10))

ptheme <- forest_theme(base_size = 18,
                       ci_pch = 18,
                       ci_fill = "black")


# Supplementary (all estimates) -------------------------------------------
p <- forest(main_results_2 %>% 
              select(Urbanicity, "Exposure period", "N exposed", "Rate Ratio (95% CI)", " "),
            est = (main_results_2$est),
            lower = (main_results_2$lci),
            upper = (main_results_2$uci),
            ci_column = 5,
            ref_line = (1),
            xlim = c((0.6), (1.5)),
            xlog = TRUE,
            ticks_at = c(0.6, 0.8, 1, 1.2, 1.5),
            sizes = main_results_2$size/10,
            theme = ptheme)
p

ggsave(plot = p,
       "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/figures/fig3_supplement.png",
       dpi=300,
       height=10, width=15, units="in")

# Limit to main estimates -------------------------------------------
main_results_2 <- main_results %>% 
  select(r_geo_strata, r_lag, n_case_days_exposed, est, lci, uci) %>% 
  group_by(r_geo_strata) %>%
  arrange(r_geo_strata) %>%
  slice(2) %>%
  bind_rows(main_results) %>%
  mutate("Rate Ratio (95% CI)" = paste0(sprintf("%.2f", round(est, 2)), " (", sprintf("%.2f", round(lci, 2)), "-", sprintf("%.2f", round(uci, 2)), ")")) %>% 
  group_by(r_geo_strata) %>% 
  mutate(Urbanicity = case_when(row_number(r_geo_strata) == 1 & r_geo_strata == "nyc" ~ "NYC",
                                row_number(r_geo_strata) == 1 & r_geo_strata == "urban" ~ "Non-NYC urban",
                                row_number(r_geo_strata) == 1 & r_geo_strata == "rural" ~ "Rural"),
         r_lag = case_when(r_lag == "lag1" ~ "Lag 1",
                           r_lag == "lag1_lag2" ~ "Lags 1-2",
                           r_lag == "lag1_lag3" ~ "Lags 1-3")) %>% 
  arrange(r_geo_strata) %>% 
  filter(r_lag != "lag0") %>% 
  rename("Exposure period" = r_lag,
         "N exposed" = n_case_days_exposed) %>% 
  ungroup() %>% 
  select(Urbanicity, "Exposure period", "N exposed", "Rate Ratio (95% CI)", est, lci, uci) %>% 
  mutate(across(!Urbanicity, ~ ifelse(!is.na(Urbanicity), NA, .))) %>% 
  mutate(across(!c("est", "lci", "uci"), ~ ifelse(is.na(.), "", as.character(.)))) %>% 
  mutate(" " = "                               ",
         size = 0.2) %>% 
  slice(c(1:6, 13:20, 7:12))

main_results_2 <- as.data.frame(main_results_2) %>% 
  slice(c(1:4, 9:12, 5:8))

p <- forest(main_results_2 %>% 
              select(Urbanicity, "Exposure period", "N exposed", "Rate Ratio (95% CI)", " "),
            est = (main_results_2$est),
            lower = (main_results_2$lci),
            upper = (main_results_2$uci),
            ci_column = 5,
            ref_line = (1),
            xlim = c((0.6), (1.5)),
            xlog = TRUE,
            ticks_at = c(0.6, 0.8, 1, 1.2, 1.5),
            sizes = main_results_2$size/10,
            theme = ptheme)
p

ggsave(plot = p,
       "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/figures/fig3_main.png",
       dpi=300,
       height=10, width=15, units="in")



