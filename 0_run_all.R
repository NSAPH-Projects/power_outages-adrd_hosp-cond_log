source(here::here("0_set_up", "1_libraries.R"))

##============================================================================
## 1. Data Tidying

# rerun this - 6/3/24 is when we remove hemorrhagic stroke
source(here::here("1_gather_study_pop", "1_identify_cvd_outcome.R"))

##============================================================================
## 2. Wrangle variables
source(here::here("2_wrangle_variables", "1_create_control_dates.R"))

##============================================================================
## 3. 
source(here::here("3_combine_data4casecross", "1_data4casecross.R"))

##============================================================================##
## 4. Analyses
source(here::here("4_analyses", "2_analysis_main.R"))
source(here::here("4_analyses", "3_analysis_em.R"))

##============================================================================##
## 5. Visuals
source(here::here("5_visuals", "fig2.R"))
source(here::here("5_visuals", "fig3.R"))
source(here::here("5_visuals", "fig4.R"))
source(here::here("5_visuals", "sfig1.R"))