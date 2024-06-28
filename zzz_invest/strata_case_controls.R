####***********************
#### Code Description ####
# Author: Vivian 
# Date: 10/21/23
# Goal: Evaluate how many strata are in our final dataset/distribution across time and space
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"

data4casecross_ind <- read_csv(paste0(path_data, "data_process/data4casecross/data4casecross_ind.csv"))

# how many strata have exposure in strata (could be in case or control)
eval_strata_w_exp <- data4casecross_ind %>% 
  group_by(new_id) %>% 
  mutate(str_w_po = ifelse(sum(cumul_daily_po > 0), 1, 0),
         str_w_custout = ifelse(sum(cumul_cust_hours_out > 0), 1, 0)) %>% 
  filter(row_number() == 1)

eval_strata_w_exp_po <- eval_strata_w_exp %>% 
  filter(str_w_po == 1) # 66,696 strata with po > 0

eval_strata_w_exp_custout <- eval_strata_w_exp %>% 
  filter(str_w_custout == 1) #324,298 strata with custout > 0

nrow(eval_strata_w_exp_po)/length(unique(data4casecross_ind$new_id))
nrow(eval_strata_w_exp_custout)/length(unique(data4casecross_ind$new_id))

# temporal distribution
eval_strata_temporal_po <- eval_strata_w_exp_po %>% 
  mutate(month = month(DayDateTime))

ggplot(eval_strata_temporal_po, aes(x = as.factor(month))) +
  geom_bar(binwidth = 1, fill = "blue", color = "black") +
  facet_wrap(~urbanicity) +
  ggtitle("# strata where po exposure > 0 in case-control stratum")

eval_strata_temporal_custout <- eval_strata_w_exp_custout %>% 
  mutate(month = month(DayDateTime))

ggplot(eval_strata_temporal_custout, aes(x = as.factor(month))) +
  geom_bar(fill = "blue", color = "black") +
  facet_wrap(~urbanicity) +
  ggtitle("# strata where custout exposure > 0 in case-control stratum")


# how many strata have exposure
invest_strata_exp <- data4casecross_ind %>% 
  mutate(n_cases = n(),
         n_cases_w_po = sum(cumul_daily_po > 0),
         n_cases_w_custout = sum(cumul_cust_hours_out > 0))

(invest_strata_exp$n_cases_w_po)[1] #77,749 rows have po > 0 (total po > 0 that could occur during case or control day in study period)
(invest_strata_exp$n_cases_w_custout)[1] #781,193 rows have po > 0 (total custout > 0 that could occur during case or control day in study period)

# how many strata have cases with exposure
invest_case_w_exp <- data4casecross_ind %>% 
  filter(dayName == "Caseday_0") %>% 
  mutate(n_cases = n(),
         n_cases_w_po = sum(cumul_daily_po > 0),
         n_cases_w_custout = sum(cumul_cust_hours_out > 0))

(invest_case_w_exp$n_cases_w_po/invest_case_w_exp$n_cases)[1] #17,403 hospitalizations had a po > 0 
(invest_case_w_exp$n_cases_w_custout/invest_case_w_exp$n_cases)[1] #178,139 hospitalizations had a custout > 0

hist(invest_case_w_exp$cumul_daily_po)
hist(invest_case_w_exp$cumul_cust_hours_out)

invest_case_w_exp_wo_0 <- invest_case_w_exp %>% 
  filter(cumul_daily_po > 0)
hist(invest_case_w_exp_wo_0$cumul_daily_po)
invest_case_w_exp_wo_0 <- invest_case_w_exp %>% 
  filter(cumul_cust_hours_out > 0)
hist(invest_case_w_exp_wo_0$cumul_cust_hours_out)

# how many strata have cases and controls with exposure
invest_case_and_control_w_po <- data4casecross_ind %>%
  group_by(new_id) %>%
  filter(!(any(dayName == "Caseday_0" & cumul_daily_po == 0))) %>%
  ungroup()
length(unique(invest_case_and_control_w_po$new_id))
invest_case_and_control_w_po2 <- invest_case_and_control_w_po %>% 
  group_by(new_id) %>%
  filter(dayName != "Caseday_0" & cumul_daily_po > 1) %>%
  summarise(count = n()) %>%
  ungroup()
table(invest_case_and_control_w_po2$count)

invest_case_and_control_w_custout <- data4casecross_ind %>%
  group_by(new_id) %>%
  filter(!(any(dayName == "Caseday_0" & cumul_cust_hours_out == 0))) %>%
  ungroup()
length(unique(invest_case_and_control_w_custout$new_id))
invest_case_and_control_w_custout2 <- invest_case_and_control_w_custout %>% 
  group_by(new_id) %>%
  filter(dayName != "Caseday_0" & cumul_cust_hours_out > 1) %>%
  summarise(count = n()) %>%
  ungroup()
table(invest_case_and_control_w_custout2$count)


# Keep only strata that contribute ----------------------------------------
# remove strata where cases 
data4casecross_ind2 <- data4casecross_ind 



# PO and custout distribution for those cases -----------------------------
strata_w_po <- eval_strata_w_exp %>% 
  filter(str_w_po > 0)

hist(strata_w_po$cumul_daily_po)
summary(strata_w_po$cumul_daily_po)


strata_w_custout <- eval_strata_w_exp %>% 
  filter(str_w_custout > 0)

hist(strata_w_custout$cumul_cust_hours_out)
summary(strata_w_custout$cumul_cust_hours_out)





