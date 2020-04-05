##############################################################
## Code for running and saving all COVID simulations
##############################################################
library(tidyverse)
library(rtZIKVrisk)
source("code/sim-covid-outbreaks.R")
source("code/covid-plotting-fxns.R")


## Setup all of the parameters that need to be run
r_not <- c(1.1, 1.5, 3)
detection_probability <- c(0.05, 0.1, 0.2, 0.3, 0.4)
importation_rate <- c(0)

## Run and save simulations across all parameter combinations
run_df <- expand_grid(r_not, detection_probability, importation_rate)
run_df %>% 
  pmap(.f = save_covid_runs, num_reps = 10000) %>% 
  unlist()

## Get all simulation county data pre-processed
run_df %>% 
  pmap(.f = get_save_path, num_reps = 10000) %>% 
  map(save_cty_data)


## Create maps for figures
load(get_save_path(r_not = 1.5, 
                   detection_probability = 0.1, 
                   importation_rate = 0, 
                   num_reps = 10000, 
                   summary = TRUE))

## Plot the various figures
if(!dir.exists("figures/")){
  dir.create("figures/")
}
plot_county_risk(cty_data) %>% 
  save_plot(plot = ., filename = "figures/us_baseline_risk_map.png", base_height = 4, base_aspect_ratio = 1.7)
plot_county_risk(cty_data, state = "Texas") %>% 
  save_plot(plot = ., filename = "figures/tx_baseline_risk_map.png", base_height = 4, base_aspect_ratio = 1.3)

## Save data for sending off
if(!dir.exists("processed_data/")){
  dir.create("processed_data/")
}
write_csv(cty_data, paste0("processed_data/", Sys.Date(), "county-risk-estimates.csv"))


## Plot case summary statistics
get_all_summary_data("processed_data/") %>% 
  plot_county_summary_sensitivity() %>% 
  save_plot(plot = ., filename = "figures/us_sensitivity_plot.png", base_height = 4, base_aspect_ratio = 1.5)
