####***********************
#### Code Description ####
# Author: Vivian 
# Date: 10/11/23
# Goal: Generate power outage metrics
# power outage exposure will be cumulative power outages based on the following combinations:
#         - varying customer thresholds, .1%, 1%, 10%, 20%, 50%
#         - varying lags, lag 0, lag 1, lags1-2, lags1-3
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"

# read in data
zcta_urbanicity <- read_csv(paste0(path_data, "data_process/zcta_urbanicity.csv"))
length(unique(zcta_urbanicity$zcta))
length(unique(zcta_urbanicity$zcta)) == nrow(zcta_urbanicity)

zcta_outages <- read_fst(paste0(path_data, "/power_outages/zcta_outages.fst")) %>% 
  filter(datetime <= as.Date("2018-12-31")) %>% 
  mutate(zcta = as.character(zcta)) %>% 
  left_join(., zcta_urbanicity) %>% 
  select(-starts_with(c("i_", "po_id")))
length(unique(zcta_outages$zcta))

invest <- zcta_outages %>% 
  mutate(pct_custout = customers_out/customers * 100,
         pct_custout = ifelse(pct_custout > 100, 100, pct_custout))
invest_wo_0 <- invest %>% 
  filter(pct_custout > 0)
summary(invest$pct_custout)
hist(invest$pct_custout)

summary(invest_wo_0$pct_custout)
hist(invest_wo_0$pct_custout)

nrow(invest %>% filter(pct_custout > .1))/nrow(invest)
nrow(invest %>% filter(pct_custout > 1))/nrow(invest)
nrow(invest %>% filter(pct_custout > 10))/nrow(invest)
nrow(invest %>% filter(pct_custout > 20))/nrow(invest)
nrow(invest %>% filter(pct_custout > 25))/nrow(invest)
nrow(invest %>% filter(pct_custout > 50))/nrow(invest)

quantile(invest_wo_0$pct_custout, 0.1)
quantile(invest_wo_0$pct_custout, 0.2)
quantile(invest_wo_0$pct_custout, 0.5)
quantile(invest_wo_0$pct_custout, 0.8)
quantile(invest_wo_0$pct_custout, 0.9)

# Create metrics of different %s  -----------------------------------------
zcta_outages_w_po_metrics <- zcta_outages %>%
  mutate(day = as.Date(ymd_hms(datetime))) %>% 
  group_by(zcta, day) %>% 
  mutate(pct_custout = customers_out/customers * 100,
         pct_custout_gte_.1_lag0 = sum(ifelse(pct_custout >= 0.1, 1, 0)),
         pct_custout_gte_1_lag0 = sum(ifelse(pct_custout >= 1, 1, 0)),
         pct_custout_gte_10_lag0 = sum(ifelse(pct_custout >= 10, 1, 0)),
         pct_custout_gte_20_lag0 = sum(ifelse(pct_custout >= 20, 1, 0)),
         pct_custout_gte_50_lag0 = sum(ifelse(pct_custout >= 50, 1, 0)),
         pct_custout_gte_60_lag0 = sum(ifelse(pct_custout >= 60, 1, 0))) %>% 
  filter(row_number() == 1) %>% 
  select(day, zcta, urbanicity, starts_with("pct_"), -pct_custout) 


# Expand to full time series ----------------------------------------------
full_study_days <- zcta_outages %>%
  select(zcta, datetime) %>% 
  ungroup() %>%
  complete(zcta, datetime = seq.Date(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")) %>% 
  mutate(day = as.Date(datetime)) %>% 
  select(-datetime) %>% 
  distinct(zcta, day)

glimpse(full_study_days)
glimpse(zcta_outages_w_po_metrics)

zcta_outages_w_po_metrics <- zcta_outages_w_po_metrics %>% 
  full_join(., full_study_days) %>% 
  mutate_at(vars(starts_with("pct_")), ~ifelse(is.na(.), 0, .)) %>% 
  arrange(zcta, day) %>% 
  group_by(zcta) %>% 
  mutate(pct_custout_gte_.1_lag1 = lag(pct_custout_gte_.1_lag0, default = 0),
         pct_custout_gte_1_lag1 = lag(pct_custout_gte_1_lag0, default = 0),
         pct_custout_gte_10_lag1 = lag(pct_custout_gte_10_lag0, default = 0),
         pct_custout_gte_20_lag1 = lag(pct_custout_gte_20_lag0, default = 0),
         pct_custout_gte_50_lag1 = lag(pct_custout_gte_50_lag0, default = 0),
         pct_custout_gte_60_lag1 = lag(pct_custout_gte_60_lag0, default = 0),
         pct_custout_gte_.1_lag2 = lag(pct_custout_gte_.1_lag0, default = 0, n = 2),
         pct_custout_gte_1_lag2 = lag(pct_custout_gte_1_lag0, default = 0, n = 2),
         pct_custout_gte_10_lag2 = lag(pct_custout_gte_10_lag0, default = 0, n = 2),
         pct_custout_gte_20_lag2 = lag(pct_custout_gte_20_lag0, default = 0, n = 2),
         pct_custout_gte_50_lag2 = lag(pct_custout_gte_50_lag0, default = 0, n = 2),
         pct_custout_gte_60_lag2 = lag(pct_custout_gte_60_lag0, default = 0, n = 2),
         pct_custout_gte_.1_lag3 = lag(pct_custout_gte_.1_lag0, default = 0, n = 3),
         pct_custout_gte_1_lag3 = lag(pct_custout_gte_1_lag0, default = 0, n = 3),
         pct_custout_gte_10_lag3 = lag(pct_custout_gte_10_lag0, default = 0, n = 3),
         pct_custout_gte_20_lag3 = lag(pct_custout_gte_20_lag0, default = 0, n = 3),
         pct_custout_gte_50_lag3 = lag(pct_custout_gte_50_lag0, default = 0, n = 3),
         pct_custout_gte_60_lag3 = lag(pct_custout_gte_60_lag0, default = 0, n = 3),
         
         # total po hours in 48 hours prior to admission day
         tot_hr_gte_.1_lag1_lag2 = pct_custout_gte_.1_lag1 + pct_custout_gte_.1_lag2,
         tot_hr_gte_1_lag1_lag2 = pct_custout_gte_1_lag1 + pct_custout_gte_1_lag2,
         tot_hr_gte_10_lag1_lag2 = pct_custout_gte_10_lag1 + pct_custout_gte_10_lag2,
         tot_hr_gte_20_lag1_lag2 = pct_custout_gte_20_lag1 + pct_custout_gte_20_lag2,
         tot_hr_gte_50_lag1_lag2 = pct_custout_gte_50_lag1 + pct_custout_gte_50_lag2,
         tot_hr_gte_60_lag1_lag2 = pct_custout_gte_60_lag1 + pct_custout_gte_60_lag2,
         
         # total po hours in 72 hours prior to admission day
         tot_hr_gte_.1_lag1_lag3 = pct_custout_gte_.1_lag1 + pct_custout_gte_.1_lag2 + pct_custout_gte_.1_lag3,
         tot_hr_gte_1_lag1_lag3 = pct_custout_gte_1_lag1 + pct_custout_gte_1_lag2 + pct_custout_gte_1_lag3,
         tot_hr_gte_10_lag1_lag3 = pct_custout_gte_10_lag1 + pct_custout_gte_10_lag2 + + pct_custout_gte_10_lag3,
         tot_hr_gte_20_lag1_lag3 = pct_custout_gte_20_lag1 + pct_custout_gte_20_lag2 + pct_custout_gte_20_lag3,
         tot_hr_gte_50_lag1_lag3 = pct_custout_gte_50_lag1 + pct_custout_gte_50_lag2 + pct_custout_gte_50_lag3,
         tot_hr_gte_60_lag1_lag3 = pct_custout_gte_60_lag1 + pct_custout_gte_60_lag2 + pct_custout_gte_60_lag3) %>%
         
        # total po hours in 24 hours prior to admission day
  rename(tot_hr_gte_.1_lag1 = pct_custout_gte_.1_lag1,
         tot_hr_gte_1_lag1 = pct_custout_gte_1_lag1,
         tot_hr_gte_10_lag1 = pct_custout_gte_10_lag1,
         tot_hr_gte_20_lag1 = pct_custout_gte_20_lag1,
         tot_hr_gte_50_lag1 = pct_custout_gte_50_lag1,
         tot_hr_gte_60_lag1 = pct_custout_gte_60_lag1,
         
         # total po hours on same day of admission day
         tot_hr_gte_.1_lag0 = pct_custout_gte_.1_lag0,
         tot_hr_gte_1_lag0 = pct_custout_gte_1_lag0,
         tot_hr_gte_10_lag0 = pct_custout_gte_10_lag0,
         tot_hr_gte_20_lag0 = pct_custout_gte_20_lag0,
         tot_hr_gte_50_lag0 = pct_custout_gte_50_lag0,
         tot_hr_gte_60_lag0 = pct_custout_gte_60_lag0) %>% 
  select(-starts_with("pct_")) %>% 
  ungroup()

write_csv(zcta_outages_w_po_metrics, paste0(path_data, "/data_process/power_outage_metrics/po4casecross.csv"))


# Check -------------------------------------------------------------------

po4casecross <- read_csv(paste0(path_data, "/data_process/power_outage_metrics/po4casecross.csv"))

geo_strata_varname <- c("rural", "urban", "nyc")
for (g in 1:length(geo_strata_varname)){
  
  data <- po4casecross %>% 
    filter(urbanicity == paste0(geo_strata_varname[g])) %>% 
    mutate(month = month(day)) %>% 
    group_by(month) %>% 
    mutate(n_cumul_po = sum(cumul_daily_po),
           n_cumul_cust_hours = sum(cumul_cust_hours_out)) %>% 
    filter(row_number() == 1)
  
  plot1 <- ggplot(data, aes(x = as.factor(month), y = n_cumul_po)) +
    geom_bar(stat = "identity")
  
  plot2 <- ggplot(data, aes(x = as.factor(month), y = n_cumul_cust_hours)) +
    geom_bar(stat = "identity")
  
  print(plot1 + plot2 + plot_annotation(title = paste0(geo_strata_varname[g])))
  
}

for (g in 1:length(geo_strata_varname)){
  
  data <- po4casecross %>% 
    filter(urbanicity == paste0(geo_strata_varname[g])) %>% 
    mutate(year = year(day),
           month = month(day)) %>% 
    group_by(year, month) %>% 
    mutate(n_cumul_po = sum(cumul_daily_po),
           n_cumul_cust_hours = sum(cumul_cust_hours_out)) %>% 
    filter(row_number() == 1)
  
  plot1 <- ggplot(data, aes(x = as.factor(month), y = n_cumul_po)) +
    geom_bar(stat = "identity") +
    facet_wrap(~year)
  
  plot2 <- ggplot(data, aes(x = as.factor(month), y = n_cumul_cust_hours)) +
    geom_bar(stat = "identity") +
    facet_wrap(~year)
  
  print(plot1 + plot2 + plot_annotation(title = paste0(geo_strata_varname[g])))
  
}


