# set

terra_raster <- covariate_raster

# Trapping presence
# Thinning to a single point per raster cell

vect_presence <- vect(m_nat_presence)

tmp_m_nat_presence <- terra::rasterize(vect_presence, terra_raster[[1]], fun = "min", field = "m_nat")

pts_m_nat_presence <- as.points(tmp_m_nat_presence)

rm(list = c("tmp_m_nat_presence", "vect_presence"))

# We are left with 278 cells of presence data from trapping studies
nrow(pts_m_nat_presence)

# Trapping absence
vect_absence <- vect(m_nat_absence)

tmp_m_nat_absence <- rasterize(vect_absence, terra_raster[[1]], fun = "min", field = "m_nat")

pts_m_nat_absence <- as.points(tmp_m_nat_absence)

rm(list = c("tmp_m_nat_absence", "vect_absence"))

# We are left with 290 cells of absence data
nrow(pts_m_nat_absence)

# GBIF presence
vect_gbif <- vect(gbif_presence)

tmp_gbif <- rasterize(vect_gbif, terra_raster[[1]], fun = "min", field = "m_nat")

pts_gbif_presence <- as.points(tmp_gbif)

rm(list = c("tmp_gbif", "vect_gbif"))

# GBIF contains 360 cells of presence
nrow(pts_gbif_presence)

# Combined_presence
vect_combined <- vect(combined_presence)

tmp_combined <- rasterize(vect_combined, terra_raster[[1]], fun = "min", field = "m_nat")

pts_combined <- as.points(tmp_combined)

rm(list = c("tmp_combined", "vect_combined"))

# Combining these datasets gives us 634 cells of presence
nrow(pts_combined)


# Combined data -----------------------------------------------------------

# Extract our covariates at each point
pres_cov <- terra::extract(terra_raster, pts_combined)

# We will create a dataset of an equal number of pseudoabsences, we will use the absence data from trapping and supplement with a random sample
# Some of our absence points may reside in the same cell as presence so we will identify and remove these
tmp_st_absence <- st_as_sf(pts_m_nat_absence)
tmp_st_presence <- st_as_sf(pts_combined)

# This leaves us with 248 distinct absences
tmp_st_absence <- tmp_st_absence %>%
  filter(!geometry %in% tmp_st_presence$geometry)

pts_m_nat_absence <- vect(tmp_st_absence)

rm(list = c("tmp_st_absence", "tmp_st_presence"))

n_additional_points <- nrow(pts_combined) - nrow(pts_m_nat_absence)
  
pts_absence <- rbind(pts_m_nat_absence,
                     vect(randomPoints(raster(terra_raster[[1]]), n_additional_points), crs = project_crs))

# Extract our covariates at each point
abs_cov <- terra::extract(terra_raster, pts_absence)

# We then add the response variable to each of these covariate dataframes
pres_cov <- tibble(pres_cov) %>%
  mutate(m_nat = 1)

abs_cov <- tibble(abs_cov) %>%
  mutate(m_nat = 0)

# We now bring them together
combined_cov <- bind_rows(pres_cov, abs_cov) %>%
  drop_na() %>%
  mutate(land_cover = recode_factor(land_cover, !!!land_use_categories),
         log_pop_density = log10(pop_density)) %>%
  dummy_cols(select_columns = "land_cover", remove_selected_columns = TRUE) %>%
  select(-ID, -pop_density)
# And remove any with missing data, dummy coding landuse and transforming pop_density to log