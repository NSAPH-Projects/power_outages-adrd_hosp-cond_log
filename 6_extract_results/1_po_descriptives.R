####***********************
#### Code Description ####
# Author: Vivian  
# Date: 1/30/24
# Goal: generate visualization for figure 1
# Figure 1 = spatial distribution of power outages at the 10% threshold in the 24 hours prior to hospitalization day
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
library(tigris)

# set data paths
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"
path_results <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/results/"

# # read in power outage data for study participants (exposure during cases and controls)
# po_data <- read_csv(paste0(path_data, "data_process/data4casecross/data4casecross_ind.csv")) 

# read in power outage data for entire study period
po_data <- read_csv(paste0(path_data, "/data_process/power_outage_metrics/po4casecross.csv")) %>%
  rename(DayDateTime = day) %>%
  mutate(zcta = as.character(zcta))

# wrangle data 
po_exposure <- po_data %>% 
  select(DayDateTime, zcta, urbanicity, tot_hr_gte_10_lag0) %>% 
  group_by(DayDateTime, zcta) %>% 
  filter(row_number() == 1) %>% 
  group_by(zcta) %>%
  mutate(urbanicity = ifelse(all(!is.na(urbanicity)), urbanicity, urbanicity[!is.na(urbanicity)][1])) %>% 
  ungroup()

# get distribution by urbanicity
po_dist <- po_exposure %>% 
  group_by(zcta) %>% 
  mutate(total_po = sum(tot_hr_gte_10_lag0)) %>% 
  filter(row_number() == 1) %>% 
  group_by(urbanicity) %>% 
  summarise(t10_lg1_tot = sum(total_po, na.rm = TRUE),
            t10_lg1_med = median(total_po, na.rm = TRUE),
            t10_lg1_mean = mean(total_po, na.rm = TRUE),
            t10_lg1_q25 = quantile(total_po, probs = 0.25, na.rm = TRUE),
            t10_lg1_q75 = quantile(total_po, probs = 0.75, na.rm = TRUE))

# how many power outages total across urbanicity?
sum(po_dist$t10_lg1_tot)

# how many affected zctas? 
nrow(po_exposure %>% 
  filter(tot_hr_gte_10_lag0 > 0) %>% 
  distinct(zcta)) 

nrow(po_exposure %>% 
       distinct(zcta))

nrow(po_exposure %>% 
       filter(tot_hr_gte_10_lag0 > 0) %>% 
       distinct(zcta)) / nrow(po_exposure %>% 
                                distinct(zcta))
  

# seasonal
po_seasonal <- po_exposure %>% 
  group_by(urbanicity, month = month(DayDateTime)) %>% 
  summarise(t10_lg1_tot = sum(tot_hr_gte_10_lag0, na.rm = TRUE)) %>% 
  mutate(t10_lg1_pct = t10_lg1_tot/sum(t10_lg1_tot)*100) %>% 
  rename(Urbanicity = urbanicity) %>% 
  mutate(Urbanicity = case_when(Urbanicity == "nyc" ~ "NYC",
                                Urbanicity == "urban" ~ "Urban",
                                Urbanicity == "rural" ~ "Rural"))

po_seasonal$Urbanicity <- factor(po_seasonal$Urbanicity, levels = c("NYC", "Urban", "Rural"))
ggplot(po_seasonal, aes(x = as.factor(month), y = t10_lg1_tot, fill = Urbanicity)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Urbanicity, scales = "free_y", nrow = 3) +
  labs(x = "Month", y = "Count of power outages") +
  theme(legend.position = "none")




# many ZCTAs during our study across urbanicity levels did not experience daily power outages (XXX). 

visual <- po_exposure %>% 
  mutate(day = day(DayDateTime),
         month = month(DayDateTime),
         year = year(DayDateTime)) %>% 
  group_by(urbanicity, month, day) %>% 
  mutate(total_po = sum(tot_hr_gte_10_lag0)) %>% 
  filter(row_number() == 1) %>%
  mutate(month = factor(month, levels = 1:12),
         day = factor(day, levels = rev(1:31)))

ggplot(visual %>% filter(urbanicity == "nyc"), aes((month), (day), fill= total_po)) + 
  geom_tile() +
  facet_wrap(~urbanicity) +
  scale_fill_gradient(low="white", high="blue")  
  




