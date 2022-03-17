

# Produce low resolution predictions --------------------------------------




# Produce high resolution predictions -------------------------------------

if(!file.exists(here("tmp", "m_nat_raster_hr.grd"))) {
  
  m_nat_layer <- predict(object = sdm,
                         x.layers = prediction_raster,
                         quantiles = c(0.025, 0.975),
                         splitby = 20)
  writeRaster(m_nat_layer, here("tmp", "m_nat_raster_hr.grd")) 
  
} else {
  
  m_nat_layer <- rast(here("tmp", "m_nat_raster_hr.grd"))
  
}


names(m_nat_layer) <- c("Central estimate", "2.5% CrI", "97.5% CrI")

m_nat_uncertainty <- gplot(m_nat_layer) +
  geom_tile(aes(fill = value), na.rm = TRUE) +
  facet_wrap(~ variable, ncol = 1) +
  coord_equal() +
  geom_sf(data = WA_shape, lwd = 0.5, colour = "white", fill = NA, inherit.aes = FALSE) +
  scale_fill_viridis_c(na.value = NA) +
  labs(fill = "Probability of detection",
       x = element_blank(),
       y = element_blank()) +
  theme_minimal()

save_plot(plot = m_nat_uncertainty, here("output", "sdm_all_vars.png"), base_height = 12, base_width = 8)

pts_combined_sf <- st_as_sf(pts_combined) %>% st_set_crs(value = "EPSG:4326")

m_nat_overlay_presence <- gplot(m_nat_layer[[1]]) +
  geom_tile(aes(fill = value), na.rm = TRUE) +
  coord_equal() +
  geom_sf(data = WA_shape, lwd = 0.5, colour = "white", fill = NA, inherit.aes = FALSE) +
  geom_sf(data = pts_combined_sf, inherit.aes = FALSE) +
  scale_fill_viridis_c(na.value = NA) +
  labs(fill = "Probability of detection",
       x = element_blank(),
       y = element_blank()) +
  theme_minimal()

pts_combined_sf <- pts_combined_sf %>%
  bind_cols(probability = terra::extract(m_nat_layer[[1]], pts_combined)$`Central estimate`) %>%
  mutate(presence = "Presence")

pts_absence_sf <- st_as_sf(pts_absence) %>% st_set_crs(value = "EPSG:4326") %>%
  bind_cols(probability = terra::extract(m_nat_layer[[1]], pts_absence)$`Central estimate`) %>%
  mutate(presence = "Absence")

plot_probability <- bind_rows(pts_combined_sf, pts_absence_sf)

plot_probability %>% 
  tibble() %>% 
  ggplot() + 
  geom_histogram(aes(x = probability)) + 
  facet_wrap(~ presence, ncol = 1) +
  theme_minimal()