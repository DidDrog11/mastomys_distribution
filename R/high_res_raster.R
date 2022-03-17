# Produce the prediction raster, requiring dummy coding of land use, conversion to log10_pop_density, removal of ID variable

if(!file.exists(here("data", "covariate_raster.tif"))) {

land_cover_dummy_rast <- covariate_raster[["land_cover"]] %>%
  classify(simplified_landuse, include.lowest = TRUE) %>%
  segregate()

names(land_cover_dummy_rast) <- all_vars[str_detect(all_vars, "land_cover")]

prediction_raster <- subset(covariate_raster, c(1:21, 23:35))
prediction_raster$log_pop_density <- log10(prediction_raster$pop_density)

prediction_raster <- stack(prediction_raster, stack(land_cover_dummy_rast))

writeRaster(prediction_raster, here("data", "covariate_raster.tif"))

} else {
  
  prediction_raster <- rast(here("data", "covariate_raster.tif"))
  
}
