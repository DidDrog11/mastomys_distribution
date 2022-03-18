# Produce the prediction raster, requiring dummy coding of land use, conversion to log10_pop_density, removal of ID variable

if(!file.exists(here("data", "covariate_raster.tif"))) {

writeRaster(covariate_raster, here("data", "covariate_raster.tif"), overwrite = T)

} else {
  
  prediction_raster <- rast(here("data", "covariate_raster.tif"))
  
}
