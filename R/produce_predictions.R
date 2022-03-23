lr_prediction_raster <- rast(here("data", "lr_covariate_raster.tif"))

lr_prediction_stack <- stack(lr_prediction_raster)

# Produce low resolution predictions --------------------------------------

if(!file.exists(here("tmp", "m_nat_raster_lr_0.grd"))) {
  
  m_nat_layer_lr_0 <- predict(object = sdm,
                              x.layers = lr_prediction_stack,
                              quantiles = c(0.025, 0.975),
                              splitby = 20)
  writeRaster(m_nat_layer_lr_0, here("tmp", "m_nat_raster_lr_0.grd")) 
  
} else {
  
  m_nat_layer_lr_0 <- rast(here("tmp", "m_nat_raster_lr_0.grd"))
  
}

if(!file.exists(here("tmp", "m_nat_raster_lr_1.grd"))) {
  
  m_nat_layer_lr_1 <- predict(object = sdm_1,
                              x.layers = lr_prediction_stack,
                              quantiles = c(0.025, 0.975),
                              splitby = 20)
  writeRaster(m_nat_layer_lr_1, here("tmp", "m_nat_raster_lr_1.grd")) 
  
} else {
  
  m_nat_layer_lr_1 <- rast(here("tmp", "m_nat_raster_lr_1.grd"))
  
}

# Produce high resolution predictions -------------------------------------

# if(!file.exists(here("tmp", "m_nat_raster_hr.grd"))) {
#   
#   m_nat_layer <- predict(object = sdm,
#                          x.layers = prediction_raster,
#                          quantiles = c(0.025, 0.975),
#                          splitby = 20)
#   writeRaster(m_nat_layer, here("tmp", "m_nat_raster_hr.grd")) 
#   
# } else {
#   
#   m_nat_layer <- rast(here("tmp", "m_nat_raster_hr.grd"))
#   
# }
