####***********************
#### Code Description ####
# Author: Vivian  
# Date: 1/17/24
# Goal: generate visualization for figure 1
# Figure 1 = spatial distribution of power outages at the 10% threshold (we will use lag0 variable because that is just # po on that day)
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
library(tigris)
library(MetBrewer)

# set data paths
path_data <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/"
path_results <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/results/"

# read in nys file
zcta_urbanicity <- read_csv(paste0(path_data, "data_process/zcta_urbanicity.csv"))
zcta_shapefile <- st_read(paste0(path_data, "/shapefile_zcta2010/US_zcta_2010.shp")) %>% 
  rename(zcta = ZCTA5CE10) %>% 
  left_join(., zcta_urbanicity %>% 
              mutate(in_nys = 1)) %>% 
  filter(in_nys == 1)

# # read in power outage data for study participants (exposure during cases and controls)
# po_data <- read_csv(paste0(path_data, "data_process/data4casecross/data4casecross_ind.csv")) 

# read in power outage data for entire study period
po_data <- read_csv(paste0(path_data, "/data_process/power_outage_metrics/po4casecross.csv")) %>%
  rename(DayDateTime = day) %>%
  mutate(zcta = as.character(zcta))

# organize data for display
po_exposure <- po_data %>% 
  select(DayDateTime, zcta, tot_hr_gte_10_lag0) %>% 
  group_by(DayDateTime, zcta) %>% 
  filter(row_number() == 1)

po_exposure <- po_exposure %>% 
  group_by(zcta) %>% 
  summarize(mean_po_gte10 = mean(tot_hr_gte_10_lag0, na.rm = TRUE),
            med_po_gte10 = median(tot_hr_gte_10_lag0, na.rm = TRUE),
            total_po_gte10 = sum(tot_hr_gte_10_lag0, na.rm = TRUE))

head(as.data.frame(po_exposure), n = 100)

# link shapefile with data
mapping_dta <- zcta_shapefile %>% 
  left_join(., po_exposure) 

# explore distribution 
mapping_dta_wo_0 <- mapping_dta %>% filter(total_po_gte10 > 0)
boundary_values <- quantile(mapping_dta_wo_0$total_po_gte10, probs = seq(0, 1, 0.25), na.rm = TRUE)
boundary_values

# set boundaries to be 0, 1-5, 6-10, 11-15, 16-20, 21+
mapping_dta <- mapping_dta %>%
  mutate(binned_po = case_when(total_po_gte10 == 0 ~ "0",
                               total_po_gte10 > 0 & total_po_gte10 <= 16 ~ "Q1: 1-16",
                               total_po_gte10 > 16 & total_po_gte10 <= 47 ~ "Q2: 17-47",
                               total_po_gte10 > 47 & total_po_gte10 <= 93 ~ "Q3: 48-93",
                               total_po_gte10 >= 93 ~ "Q4: 93-361",
                               is.na(total_po_gte10) ~ "Missing"))
mapping_dta$binned_po <- factor(mapping_dta$binned_po, levels = c("0", "Q1: 1-16", "Q2: 17-47", "Q3: 48-93", "Q4: 93-361", "Missing"))

# mapping_dta_wo_0 <- mapping_dta %>% filter(total_po_gte10 > 0)
# boundary_values <- quantile(mapping_dta_wo_0$total_po_gte10, probs = seq(0, 1, 0.25), na.rm = TRUE)
# boundary_values
# 
# # set boundaries to be 0, 1-5, 6-10, 11-15, 16-20, 21+
# mapping_dta <- mapping_dta %>% 
#   mutate(binned_po = case_when(total_po_gte10 == 0 ~ "0",
#                                total_po_gte10 > 0 & total_po_gte10 <= 2 ~ "Q1: 1-2",
#                                total_po_gte10 > 3 & total_po_gte10 <= 5 ~ "Q2: 3-5",
#                                total_po_gte10 > 6 & total_po_gte10 <= 15 ~ "Q3: 6-15",
#                                total_po_gte10 >= 16 ~ "Q4: 16-185"))
# mapping_dta$binned_po <- factor(mapping_dta$binned_po, levels = c("0", "Q1: 1-2", "Q2: 3-5", "Q3: 6-15", "Q4: 16-185"))

# NYC map
subset_nyc <- mapping_dta %>% filter(urbanicity == "nyc", total_po_gte10 < 16)
levels_binned_po <- levels(subset_nyc$binned_po)
custom_colors <- c(met.brewer("Isfahan2")[1], met.brewer("Isfahan2")[2])
color_mapping <- setNames(custom_colors, c("0", "Q1: 1-16"))

nyc_map <- ggplot(mapping_dta %>% filter(urbanicity == "nyc")) + 
  geom_sf(aes(fill = binned_po),
          size = .05,
          color = "black") +
  scale_fill_manual(values = color_mapping, na.value = "white") +
  theme_map() +
  coord_sf(crs = st_crs("EPSG:4326")) +
  theme(legend.position = "none")

nyc_grob <- ggplotGrob(nyc_map)

# NYS map
nys_map <- ggplot(mapping_dta) + 
  geom_sf(aes(fill = binned_po),
          size = .05,
          color = "black") +
  scale_fill_met_d("Isfahan2", direction = 1, na.value = "grey") +
  labs(fill = paste0("Cumulative hours of \npower outages")) +
  # theme(legend.text = element_text(size = 7)) +
  theme_map() +
  coord_sf(crs = st_crs("EPSG:4326")) +
  theme(legend.text = element_text(size = 7),
        legend.title = element_text(size = 10),
        legend.key.size = unit(.35, "cm"))

# join maps
# get coords of nys_map
layer_scales(nys_map)$x$range$range
layer_scales(nys_map)$y$range$range

nys_map_inset <- nys_map +
  annotation_custom(grob = nyc_grob, xmin = -79, xmax = -75, ymax = 41.85, ymin = 40.0) +
  geom_rect(aes(xmin = -78.5, xmax = -75.5, ymax = 41.85, ymin = 40), 
            color = "black", fill = NA, size = 0.5) +
  geom_segment(aes(x = -75.5, y = 41.85, xend = -74.15, yend = 40.8), color = "black", size = 0.5) +
  geom_segment(aes(x = -75.5, y = 40, xend = -74.15, yend = 40.35), color = "black", size = 0.5) 

nys_map_inset

ggsave("/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/figures/fig1.png",
       dpi=300,
       height=4, width=7, units="in")






