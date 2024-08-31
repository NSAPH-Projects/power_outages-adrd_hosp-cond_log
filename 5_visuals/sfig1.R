####***********************
#### Code Description ####
# Author: Vivian  
# Date: 1/30/24
# Goal: generate visualization for figure 1
# Figure 1 = spatial distribution of power outages at the 10% Cutpoint in the 24 hours prior to hospitalization day
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
sum(po_dist$t10_lg1_tot) #98378

# how many affected zctas? 1539
nrow(po_exposure %>% 
       filter(tot_hr_gte_10_lag0 > 0) %>% 
       distinct(zcta)) 

nrow(po_exposure %>% #1761
       distinct(zcta))

nrow(po_exposure %>% 
       filter(tot_hr_gte_10_lag0 > 0) %>% 
       distinct(zcta)) / nrow(po_exposure %>% 
                                distinct(zcta))


# seasonal
po_seasonal <- po_exposure %>%
  group_by(urbanicity, month = month(DayDateTime)) %>%
  summarise(t10_lg1_tot = sum(tot_hr_gte_10_lag0, na.rm = TRUE)) %>%
  mutate(t10_lg1_pct = t10_lg1_tot / sum(t10_lg1_tot) * 100) %>%
  rename(Urbanicity = urbanicity) %>%
  mutate(
    Urbanicity = case_when(
      Urbanicity == "nyc" ~ "NYC",
      Urbanicity == "urban" ~ "Urban",
      Urbanicity == "rural" ~ "Rural"
    ),
    month = factor(
      case_when(
        month == 1 ~ "January",
        month == 2 ~ "February",
        month == 3 ~ "March",
        month == 4 ~ "April",
        month == 5 ~ "May",
        month == 6 ~ "June",
        month == 7 ~ "July",
        month == 8 ~ "August",
        month == 9 ~ "September",
        month == 10 ~ "October",
        month == 11 ~ "November",
        month == 12 ~ "December"
      ),
      levels = c(
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December"
      )
    )
  )

po_seasonal$Urbanicity <- factor(po_seasonal$Urbanicity, levels = c("NYC", "Urban", "Rural"))
s <- ggplot(po_seasonal, aes(x = as.factor(month), y = t10_lg1_tot, fill = Urbanicity)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Urbanicity, scales = "free_y", nrow = 3) +
  labs(x = "Month", y = "Count of power outages") +
  theme(legend.position = "none",
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave(plot = s,
       "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/figures/sfig1_thresh10_lag1_seasonality.png",
       dpi=300,
       height=10, width=15, units="in")
