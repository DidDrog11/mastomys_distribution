# load packages

source(here::here("R", "library.R"))

# load project wide values

source(here("R", "project_wide_values.R"))

# read data

source(here("R", "load_data.R"))

# clean data

source(here("R", "clean_data.R"))

# combine data and prepare analysis

source(here("R", "prep_analysis.R"))

# run the models

source(here("R", "run_models.R"))

# produce a higher resolution raster for final predictions

source(here("R", "high_res_raster.R"))

# produce a lower resolution raster to test predictions on

source(here("R", "low_res_raster.R"))

# produce predictive raster

source(here("R", "produce_predictions.R"))

# explore predictions

source(here("R", "explore_predictions.R"))