
# First model
all_vars <- names(combined_cov)[!(names(combined_cov) %in% c('m_nat'))]

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
