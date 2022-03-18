
# First model
all_vars <- names(combined_cov)[!(names(combined_cov) %in% c("m_nat", "ID", "pop_density"))]

combined_cov <- as.data.frame(combined_cov)

# Run model ---------------------------------------------------------------

if(!file.exists(here("tmp", "sdm.rds"))) {
  
  # Automated variable selection
  sdm <- bart.step(x.data = combined_cov[ ,all_vars],
                   y.data = combined_cov[, "m_nat"],
                   full = TRUE,
                   quiet = TRUE)
  write_rds(sdm, here("tmp", "sdm.rds"))
  
} else {
  
  sdm <- read_rds(here("tmp", "sdm.rds"))
  
}


# Assess variable importance ----------------------------------------------

varimp(sdm, plots = TRUE)

# Second model

selected_vars <- names(combined_cov)[(names(combined_cov) %in% c("mean_temperature", "mean_diurnal_range", "temperature_seasonality",
                                                                 "max_temp_warmest", "min_temp_coldest", "annual_precipitation",
                                                                 "precipitation_wettest_month", "precipitation_driest_month",
                                                                 "precipitation_warmest", "precipitation_coldest",
                                                                 "elevation", "agriculture", "forest", "grassland", "shrubland",
                                                                 "sparse_vegetation", "wetland", "urban", "bare", "water",
                                                                 "land_use_change_count", "WA_ndvi_monthly_1", "WA_ndvi_monthly_3",
                                                                 "WA_ndvi_monthly_6", "WA_ndvi_monthly_9", "WA_ndvi_monthly_12",
                                                                 "WA_ndvi_cv", "log_pop_density"))]

# Run model ---------------------------------------------------------------

if(!file.exists(here("tmp", "sdm_1.rds"))) {
  
  # Automated variable selection
  sdm_1 <- bart.step(x.data = combined_cov[ ,selected_vars],
                     y.data = combined_cov[, "m_nat"],
                     full = TRUE,
                     quiet = TRUE)
  write_rds(sdm_1, here("tmp", "sdm_1.rds"))
  
} else {
  
  sdm_1 <- read_rds(here("tmp", "sdm_1.rds"))
  
}


# Assess variable importance ----------------------------------------------

varimp(sdm_1, plots = TRUE)