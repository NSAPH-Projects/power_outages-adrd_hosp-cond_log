####***********************
#### Code Description ####
# Author: Vivian  
# Date: 4/13/23
# Goal: Set up directories and libraries for project
####**********************

# set up proxy for RStudio
Sys.setenv(http_proxy="http://rcproxy.rc.fas.harvard.edu:3128")
Sys.setenv(https_proxy="http://rcproxy.rc.fas.harvard.edu:3128")

# data directory (equivalent of a symlink in terminal)
adrd_dir <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/data/adrd_hospitalization/"

# check that this is the correct directory
# adrd_hospitalization is the outcome data 
list.files(path = adrd_dir)


# data cleaning libraries
options(scipen = 999)
library(dplyr)
library(lubridate)
library(tidyverse)
library(here)
library(fst)
library(parallel) #for creating controls
library(vroom)
library(arrow)
library(readxl)


# mapping libraries
library(ggplot2)
library(maps)
library(mapdata)
# library(plot_usmap)
# library(janitor)

# analysis regression
library(dlnm)
library(splines)
library(survival)

# save results and outputs
# install.packages("table1")
library(devtools)
library(openxlsx)
library(table1)

# visuals
library(cowplot)
library(patchwork)
library(wesanderson)
library(sf)
library(stats)
library(MetBrewer)
library(grid)
library(forestploter)
