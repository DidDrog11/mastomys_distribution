if(!file.exists(here("data", "lr_covariate_raster.tif"))) {
  
  lr_raster <- rast(here("data", "covariate_raster_full.tif"))
  
  # Produce a temporary raster
  
  tmp_raster <- lr_raster[[1]]
  
  # The high resolution raster is at 30 arc seconds ~ 1km at the equator
  # change this to 0.05 
  
  res(tmp_raster) <- 0.05
  new_cellsize <- cellSize(tmp_raster, unit = "km")
  
  tmp_raster <- aggregate(subset(lr_raster,  c(1:20, 31:45)), fact = 6, fun = "median")
  tmp_landuse <- aggregate(subset(lr_raster, c(22:30)), fact =  6, fun = "modal")
  
  lr_raster <- c(tmp_raster, tmp_landuse)
  
  rm(tmp_raster, tmp_landuse, new_cellsize)
  
  writeRaster(lr_raster, here("data", "lr_covariate_raster.tif"), overwrite = TRUE) 
  
  lr_prediction_raster <- lr_raster
  
} else {
  
  lr_prediction_raster <- rast(here("data", "lr_covariate_raster.tif"))
  crs(lr_prediction_raster) <- project_crs
  
}
