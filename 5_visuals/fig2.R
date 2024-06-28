####***********************
#### Code Description ####
# Author: Vivian  
# Date: 1/17/24
# Goal: generate visualization for figure 2
# Figure 2 = effect estimates by urbanicity, set at lag 1 aka 24 hours prior to hospitalization day, all Cutpoints (.1%, 1%, 10%, 20%, 50%)
# forest plot taken from https://www.khstats.com/blog/forest-plots/
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
library(grid)
library(forestploter)

# set data paths
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"
path_results <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/results/"

# read in results, filtering to lag 1
main_results <- read_csv(paste0(path_results, "main_results.csv")) %>% 
  filter(r_lag == "lag1") %>% 
  mutate(model = paste0(r_geo_strata, "_", r_lag, "_", r_hr_custout_thresh))

head(main_results, n = 20)


# Data wrangling ----------------------------------------------------------
main_results <- read_csv(paste0(path_results, "main_results.csv")) %>% 
  filter(r_lag == "lag1") %>% 
  mutate(model = paste0(r_geo_strata, "_", r_lag, "_", r_hr_custout_thresh))

main_results_2 <- main_results %>% 
  select(r_geo_strata, r_hr_custout_thresh, n_case_days_exposed, est, lci, uci) %>% 
  group_by(r_geo_strata) %>%
  arrange(r_geo_strata) %>%
  slice(1) %>%
  bind_rows(main_results) %>%
  mutate("Rate Ratio (95% CI)" = paste0(sprintf("%.2f", round(est, 2)), " (", sprintf("%.2f", round(lci, 2)), "-", sprintf("%.2f", round(uci, 2)), ")")) %>% 
  group_by(r_geo_strata) %>% 
  mutate(Urbanicity = case_when(row_number(r_geo_strata) == 1 & r_geo_strata == "nyc" ~ "NYC",
                                row_number(r_geo_strata) == 1 & r_geo_strata == "urban" ~ "Non-NYC urban",
                                row_number(r_geo_strata) == 1 & r_geo_strata == "rural" ~ "Rural"),
         r_hr_custout_thresh = paste0(r_hr_custout_thresh, "%")) %>% 
  arrange(r_geo_strata) %>% 
  rename(Cutpoint = r_hr_custout_thresh,
         "N exposed" = n_case_days_exposed) %>% 
  ungroup() %>% 
  select(Urbanicity, Cutpoint, "N exposed", "Rate Ratio (95% CI)", est, lci, uci) %>% 
  mutate(across(!Urbanicity, ~ ifelse(!is.na(Urbanicity), NA, .))) %>% 
  mutate(across(!c("est", "lci", "uci"), ~ ifelse(is.na(.), "", as.character(.)))) %>% 
  mutate(" " = "                               ",
         size = 0.2,
         `Rate Ratio (95% CI)` = str_replace_all(`Rate Ratio (95% CI)`, "NA \\(NA-NA\\)", "Not estimated")) %>% 
  slice(c(1:6, 13:20, 7:12))

main_results_2 <- as.data.frame(main_results_2)

colors <- c(rep("#f6eff7", 6), rep("#d0d1e6", 6), rep("#a6bddb", 6))
ptheme <- forest_theme(base_size = 10,
                       ci_pch = 18,
                       ci_fill = "black",
                       core=list(bg_params=list(fill = c(colors))))

ptheme <- forest_theme(base_size = 18,
                       ci_pch = 18,
                       ci_fill = "black")

# Supplementary table (include ALL customer Cutpoints) ----------------------------------------------------
p <- forest(main_results_2 %>% 
              select(Urbanicity, Cutpoint, "N exposed", "Rate Ratio (95% CI)", " "),
            est = log(main_results_2$est),
            lower = log(main_results_2$lci),
            upper = log(main_results_2$uci),
            ci_column = 5,
            ref_line = log(1),
            xlim = c(log(0.6), log(1.6)),
            # ticks_at = c(log(0.6), 0.8, 1, 1.20, 1.4),
            sizes = main_results_2$size/10,
            theme = ptheme)

p <- forest(main_results_2 %>% 
              select(Urbanicity, Cutpoint, "N exposed", "Rate Ratio (95% CI)", " "),
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
       "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/figures/fig2_supplement.png",
       dpi=300,
       height=10, width=15, units="in")

# Limit to main analyses --------------------------------------------------
main_results_2 <- main_results %>% 
  select(r_geo_strata, r_hr_custout_thresh, n_case_days_exposed, est, lci, uci) %>% 
  group_by(r_geo_strata) %>%
  arrange(r_geo_strata) %>%
  slice(2) %>%
  bind_rows(main_results) %>%
  mutate("Rate Ratio (95% CI)" = paste0(sprintf("%.2f", round(est, 2)), " (", sprintf("%.2f", round(lci, 2)), "-", sprintf("%.2f", round(uci, 2)), ")")) %>% 
  group_by(r_geo_strata) %>% 
  mutate(Urbanicity = case_when(row_number(r_geo_strata) == 1 & r_geo_strata == "nyc" ~ "NYC",
                                row_number(r_geo_strata) == 1 & r_geo_strata == "urban" ~ "Non-NYC urban",
                                row_number(r_geo_strata) == 1 & r_geo_strata == "rural" ~ "Rural"),
         r_hr_custout_thresh = paste0(r_hr_custout_thresh, "%")) %>% 
  arrange(r_geo_strata) %>% 
  rename(Cutpoint = r_hr_custout_thresh,
         "N exposed" = n_case_days_exposed) %>% 
  ungroup() %>% 
  filter(!Cutpoint %in% c("0.1%", "50%")) %>% 
  select(Urbanicity, Cutpoint, "N exposed", "Rate Ratio (95% CI)", est, lci, uci) %>% 
  mutate(across(!Urbanicity, ~ ifelse(!is.na(Urbanicity), NA, .))) %>% 
  mutate(across(!c("est", "lci", "uci"), ~ ifelse(is.na(.), "", as.character(.)))) %>% 
  mutate(" " = "                               ",
         size = 0.2,
         `Rate Ratio (95% CI)` = str_replace_all(`Rate Ratio (95% CI)`, "NA \\(NA-NA\\)", "Not estimated")) %>% 
  slice(c(1:4, 9:12, 5:8))



p <- forest(main_results_2 %>% 
              select(Urbanicity, Cutpoint, "N exposed", "Rate Ratio (95% CI)", " "),
            est = (main_results_2$est),
            lower = (main_results_2$lci),
            upper = (main_results_2$uci),
            ci_column = 5,
            xlim = c((0.6), (1.5)),
            xlog = TRUE,
            ticks_at = c(0.6, 0.8, 1, 1.2, 1.5),
            sizes = main_results_2$size/10,
            theme = ptheme)
p

ggsave(plot = p,
       "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/figures/fig2_main.png",
       dpi=300,
       height=10, width=15, units="in")
