lr_raster <- rast(here("data", "covariate_raster.tif"))

# Produce a temporary raster

tmp_raster <- lr_raster[[1]]

# The high resolution raster is at 30 arc seconds ~ 1km at the equator
# change this to 0.05 

res(tmp_raster) <- 0.05
new_cellsize <- cellSize(tmp_raster, unit = "km")

tmp_raster <- aggregate(subset(lr_raster,  c(1:21, 23:35)), fact = 6, fun = "median")
tmp_raster$log_pop_density <- log10(tmp_raster$pop_density)

land_cover_dummy_rast <- lr_raster[["land_cover"]] %>%
  classify(simplified_landuse, include.lowest = FALSE) %>%
  segregate()

land_cover_dummy_check <- lr_raster[["land_cover"]] %>%
  segregate()

names(land_cover_dummy_rast) <- all_vars[str_detect(all_vars, "land_cover")][2:10]
