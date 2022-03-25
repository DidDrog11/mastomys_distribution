explore_predictions <- function(prediction_raster = m_nat_layer_lr_0, name = "sdm_0") {
  
  if(class(prediction_raster) != "SpatRaster") prediction_raster <- rast(prediction_raster)
  
  names(prediction_raster) <- c("Central estimate", "2.5% CrI", "97.5% CrI")
  
  prediction_raster$`Uncertainty` <- prediction_raster$`97.5% CrI` - prediction_raster$`2.5% CrI`
  
  uncertainty_map <- gplot(prediction_raster) +
    geom_tile(aes(fill = value), na.rm = TRUE) +
    facet_wrap(~ variable, ncol = 2) +
    coord_equal() +
    geom_sf(data = WA_shape, lwd = 0.5, colour = "white", fill = NA, inherit.aes = FALSE) +
    scale_fill_viridis_c(na.value = NA) +
    labs(fill = "Probability of detection",
         x = element_blank(),
         y = element_blank()) +
    theme_minimal()
  
  save_plot(plot = uncertainty_map, here("output", paste0(name, "_all_vars.png")), base_height = 12, base_width = 8)
  
  pts_combined_sf <- st_as_sf(pts_combined) %>% st_set_crs(value = "EPSG:4326")
  
  if(name != "sdm_2") {
    
    pts_combined_sf <- st_as_sf(pts_combined) %>% st_set_crs(value = "EPSG:4326")
    
    new_lab <- c("Presence overlay")
    names(new_lab) <- c("Central estimate")
  
    overlay_presence <- gplot(prediction_raster$`Central estimate`) +
      geom_tile(aes(fill = value), na.rm = TRUE) +
      facet_wrap(~ variable, labeller = labeller(variable = new_lab)) +
      coord_equal() +
      geom_sf(data = WA_shape, lwd = 0.5, colour = "white", fill = NA, inherit.aes = FALSE) +
      geom_sf(data = pts_combined_sf, inherit.aes = FALSE, size = 0.8, shape = 21, fill = "#8b0000", colour = "white", alpha = 0.5) +
      scale_fill_viridis_c(na.value = NA) +
      labs(fill = "Probability of detection",
           x = element_blank(),
           y = element_blank()) +
      theme_minimal()
  
  } else {
    
    pts_all_sf <- st_as_sf(all_pres) %>% st_set_crs(value = "EPSG:4326")
    
    new_lab <- c("Presence overlay")
    names(new_lab) <- c("Central estimate")
    
    overlay_presence <- gplot(prediction_raster$`Central estimate`) +
      geom_tile(aes(fill = value), na.rm = TRUE) +
      facet_wrap(~ variable, labeller = labeller(variable = new_lab)) +
      coord_equal() +
      geom_sf(data = WA_shape, lwd = 0.5, colour = "white", fill = NA, inherit.aes = FALSE) +
      geom_sf(data = pts_all_sf, inherit.aes = FALSE, size = 0.8, shape = 21, fill = "#8b0000", colour = "white", alpha = 0.5) +
      scale_fill_viridis_c(na.value = NA) +
      labs(fill = "Probability of detection",
           x = element_blank(),
           y = element_blank()) +
      theme_minimal()
    
  }
  
  save_plot(plot = overlay_presence, here("output", paste0(name, "_overlay_presence.png")), base_height = 12, base_width = 8)
  
  pts_combined_sf <- pts_combined_sf %>%
    bind_cols(probability = terra::extract(prediction_raster$`Central estimate`, pts_combined)$`Central estimate`) %>%
    mutate(presence = "Presence")
  
  pts_absence_sf <- st_as_sf(pts_absence) %>% st_set_crs(value = "EPSG:4326") %>%
    bind_cols(probability = terra::extract(prediction_raster$`Central estimate`, pts_absence)$`Central estimate`) %>%
    mutate(presence = "Absence")
  
  plot_probability <- bind_rows(pts_combined_sf, pts_absence_sf)
  
  probability_prediction <- plot_probability %>% 
    tibble() %>% 
    ggplot() + 
    geom_histogram(aes(x = probability)) + 
    facet_wrap(~ presence, ncol = 1) +
    theme_minimal()
  
  return(output = list(prediction = uncertainty_map,
              overlay_presence = overlay_presence,
              probability_prediction = probability_prediction))

}
