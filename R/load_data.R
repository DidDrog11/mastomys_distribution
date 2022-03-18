# data is downloaded from a compiled dataset on rodent trapping studies in West Africa
if(!file.exists(here("data", "downloaded_data.xlsx"))) drive_download("https://docs.google.com/spreadsheets/d/1rQYjHhk6uk1PoKZZVsgFlmuqWGicU2tTisk9ddfAwTM/edit#gid=0", path = here("data", "downloaded_data.xlsx"), overwrite = T)

# data on the studies included
studies <- read_xlsx(here("data", "downloaded_data.xlsx"), sheet = "study", col_types = "text")

# rodent data
rodent_data <- read_xlsx(here("data", "downloaded_data.xlsx"), sheet = "trapping", col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess",
                                                                                                 "guess", "guess", "text", "text", "guess", "guess"))

# data is also obtained from GBIF to enrich the trapping data
gbif_data <- read_tsv(here("data", "mastomys_natalensis_gbif.csv")) %>%
  select(gbifID, species, countryCode, decimalLatitude, decimalLongitude)

# shapefiles of the study area

for(i in 1:length(WA_countries)) {
  
  getData("GADM", country = WA_countries[i], level = 0, path = here("data", "spatial"))
  
}

WA_shape <- lapply(list.files(here("data", "spatial"), full.names = TRUE), function(x) {read_rds(x) %>%
    st_as_sf()}) %>%
  bind_rows()

# read or create all covariate raster
if(!file.exists(here("data", "covariate_raster_full.tif"))) {
  # covariates including climate data from bioclim, elevation, landuse data, human population density
  
  # bioclim at 30s resolution has been obtained from her https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip
  # this has subsequently been cropped to the extent of the study area prior to saving
  
  # bio_files <- list.files(here("data", "bioclim"))
  # 
  # for(i in 1:19) {
  #   
  #   bio_raster <- rast(here("data", "bioclim", bio_files[i])) %>%
  #     crop(., extent(WA_shape))
  #   
  #   writeRaster(bio_raster, here("data", "bioclim", substring(bio_files[i], 11)), overwrite = TRUE)
  #   
  # }
  
  if(!file.exists(here("data", "bioclim", "bioclim_stack.tif"))) {
    bio_1 <- rast(here("data", "bioclim", "bio_1.tif"))
    names(bio_1) <- "mean_temperature"
    bio_2 <- rast(here("data", "bioclim", "bio_2.tif"))
    names(bio_2) <- "mean_dirunal_range"
    bio_3 <- rast(here("data", "bioclim", "bio_3.tif"))
    names(bio_3) <- "isothermality"
    bio_4 <- rast(here("data", "bioclim", "bio_4.tif"))
    names(bio_4) <- "temperature_seasonality"
    bio_5 <- rast(here("data", "bioclim", "bio_5.tif"))
    names(bio_5) <- "max_temp_warmest"
    bio_6 <- rast(here("data", "bioclim", "bio_6.tif"))
    names(bio_6) <- "min_temp_coldest"
    bio_7 <- rast(here("data", "bioclim", "bio_7.tif"))
    names(bio_7) <- "temp_range"
    bio_8 <- rast(here("data", "bioclim", "bio_8.tif"))
    names(bio_8) <- "mean_temperature_wettest"
    bio_9 <- rast(here("data", "bioclim", "bio_9.tif"))
    names(bio_9) <- "mean_temperature_driest"
    bio_10 <- rast(here("data", "bioclim", "bio_10.tif"))
    names(bio_10) <- "mean_temperature_warmest"
    bio_11 <- rast(here("data", "bioclim", "bio_11.tif"))
    names(bio_11) <- "mean_temperature_coldest"
    bio_12 <- rast(here("data", "bioclim", "bio_12.tif"))
    names(bio_12) <- "annual_precipitation"
    bio_13 <- rast(here("data", "bioclim", "bio_13.tif"))
    names(bio_13) <- "precipitation_wettest_month"
    bio_14 <- rast(here("data", "bioclim", "bio_14.tif"))
    names(bio_14) <- "precipitation_driest_month"
    bio_15 <- rast(here("data", "bioclim", "bio_15.tif"))
    names(bio_15) <- "precipitation_seasonality"
    bio_16 <- rast(here("data", "bioclim", "bio_16.tif"))
    names(bio_16) <- "precipitation_wettest_quarter"
    bio_17 <- rast(here("data", "bioclim", "bio_17.tif"))
    names(bio_17) <- "precipitation_driest_quarter"
    bio_18 <- rast(here("data", "bioclim", "bio_18.tif"))
    names(bio_18) <- "precipitation_warmest"
    bio_19 <- rast(here("data", "bioclim", "bio_19.tif"))
    names(bio_19) <- "precipitation_coldest"
    
    bioclim_stack <- stack(x = c(bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12,
                                 bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
    writeRaster(bioclim_stack, here("data", "bioclim", "bioclim_stack.tif"))
    
  } else {
    
    bioclim_stack <- rast(here("data", "bioclim", "bioclim_stack.tif"))
    names(bioclim_stack) <- c("mean_temperature", "mean_dirunal_range", "isothermality", "temperature_seasonality", "max_temp_warmest", "min_temp_coldest", "temp_range", "mean_temperature_wettest", "mean_temperature_driest", "mean_temperature_warmest", "mean_temperature_coldest", "annual_precipitation", "precipitation_wettest_month", "precipitation_driest_month", "precipitation_seasonality", "precipitation_wettest_quarter", "precipitation_driest_quarter", "precipitation_warmest", "precipitation_coldest")
    crs(bioclim_stack) <- project_crs
    
  }
  
  # elevation https://srtm.csi.cgiar.org/srtmdata/
  
  if(!file.exists(here("data", "elevation", "WA_elevation.tif"))) {
    
    east_elevation <- rast(here("data", "elevation", "cut_n00e000.tif"))
    west_elevation <- rast(here("data", "elevation", "cut_n00w030.tif"))
    
    combined_elevation <- merge(west_elevation, east_elevation)
    
    crs(combined_elevation) <- project_crs
    
    crop(combined_elevation, extent(WA_shape)) %>%
      writeRaster(., here("data", "elevation", "WA_elevation.tif"))
    
  } else {
    
    elevation <- rast(here("data", "elevation", "WA_elevation.tif"))
    names(elevation) <- "elevation"
    crs(elevation) <- project_crs
    
  }
  
  # gridded population of the world https://sedac.ciesin.columbia.edu/data/collection/gpw-v4
  
  if(!file.exists(here("data", "gridded_pop", "WA_population.tif"))) {
    
    global_pop <- rast(here("data", "gridded_pop", "gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif"))
    
    crs(global_pop) <- project_crs
    
    crop(global_pop, extent(WA_shape)) %>%
      writeRaster(., here("data", "gridded_pop", "WA_population.tif"), overwrite = TRUE)
    
  }  else {
    
    WA_population <- rast(here("data", "gridded_pop", "WA_population.tif"))
    names(WA_population) <- "pop_density"
    crs(WA_population) <- project_crs
    
  }
  
  # world settlement footprint https://figshare.com/articles/dataset/World_Settlement_Footprint_WSF_2015/10048412
  
  # if(!file.exists(here("data", "world_settlement", "WA_settlement.tif"))) {
  #   
  #   # world_settlement_e10 <- rast(here("data", "world_settlement", "WSF2015_v1_EPSG4326", "WSF2015_v1_EPSG4326_e000_n10_e010_n00.tif"))
  #   # world_settlement_e10_n10 <- rast(here("data", "world_settlement", "WSF2015_v1_EPSG4326", "WSF2015_v1_EPSG4326_e000_n20_e010_n10.tif"))
  #   # 
  #   # combined_e10 <- merge(world_settlement_e10, world_settlement_e10_n10)
  #   # writeRaster(combined_e10, here("data", "world_settlement", "temp", "e10.tif"))
  #   # removeTmpFiles(h = 0)
  #   # 
  #   # world_settlement_e20 <- rast(here("data", "world_settlement", "WSF2015_v1_EPSG4326", "WSF2015_v1_EPSG4326_e010_n10_e020_n00.tif"))
  #   # world_settlement_e20_n10 <- rast(here("data", "world_settlement", "WSF2015_v1_EPSG4326", "WSF2015_v1_EPSG4326_e010_n20_e020_n10.tif"))
  #   # 
  #   # combined_e20 <- merge(world_settlement_e20, world_settlement_e20_n10)
  #   # writeRaster(combined_e20, here("data", "world_settlement", "temp", "e20.tif"))
  #   # removeTmpFiles(h = 0)
  #   # 
  #   # world_settlement_w10 <- rast(here("data", "world_settlement", "WSF2015_v1_EPSG4326", "WSF2015_v1_EPSG4326_w010_n10_e000_n00.tif"))
  #   # world_settlement_w10_n10 <- rast(here("data", "world_settlement", "WSF2015_v1_EPSG4326", "WSF2015_v1_EPSG4326_w010_n20_e000_n10.tif"))
  #   # 
  #   # combined_w10 <- merge(world_settlement_w10, world_settlement_w10_n10)
  #   # writeRaster(combined_w10, here("data", "world_settlement", "temp", "w10.tif"))
  #   # removeTmpFiles(h = 0)
  #   # 
  #   # world_settlement_w20 <- rast(here("data", "world_settlement", "WSF2015_v1_EPSG4326", "WSF2015_v1_EPSG4326_w020_n10_w010_n00.tif"))
  #   # world_settlement_w20_n10 <- rast(here("data", "world_settlement", "WSF2015_v1_EPSG4326", "WSF2015_v1_EPSG4326_w020_n20_w010_n10.tif"))
  #   # 
  #   # combined_w20 <- merge(world_settlement_w20, world_settlement_w20_n10)
  #   # writeRaster(combined_w20, here("data", "world_settlement", "temp", "w20.tif"))
  #   # removeTmpFiles(h = 0)
  #   
  #   combined_e10 <- rast(here("data", "world_settlement", "temp", "e10.tif"))
  #   combined_e20 <- rast(here("data", "world_settlement", "temp", "e20.tif"))
  #   combined_w10 <- rast(here("data", "world_settlement", "temp", "w10.tif"))
  #   combined_w20 <- rast(here("data", "world_settlement", "temp", "w20.tif"))
  #   
  #   world_settlement <- merge(combined_e10, combined_e20, combined_w10, combined_w20)
  #   writeRaster(world_settlement, here("data", "world_settlement", "temp", "combined_settlement.tif"))
  #   removeTmpFiles(h = 0)
  #   
  #   world_settlement <- rast(here("data", "world_settlement", "temp", "combined_settlement.tif"))
  #   crs(world_settlement) <- project_crs
  #   
  #   crop(world_settlement, extent(WA_shape)) %>%
  #     writeRaster(., here("data", "world_settlement", "WA_settlement.tif"), overwrite = TRUE)
  #   
  # }  else {
  #   
  #   WA_settlement <- rast(here("data", "world_settlement", "WA_settlement.tif"))
  #   names(WA_settlement) <- "settlement"
  #   crs(WA_settlement) <- project_crs
  #   
  # }
  
  # land cover https://cds.climate.copernicus.eu/cdsapp#!/home
  
  if(!file.exists(here("data", "land_cover", "WA_land_cover_binary.tif"))) {
    
    land_cover <- rast(here("data", "land_cover", "C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"))
    land_cover <- c(land_cover[[1]], land_cover[[5]])
    names(land_cover) <- c("land_cover", "land_use_change_count")
    crs(land_cover) <- project_crs
    WA_land_cover <- crop(land_cover, extent(WA_shape))
    
    source(here("R", "project_wide_values.R"))
    
    binary_land_cover <- classify(WA_land_cover$land_cover, classification_raster) %>%
      segregate()
    
    names(binary_land_cover) <- c("agriculture", "forest", "grassland", "shrubland", "sparse_vegetation", "wetland", "urban", "bare", "water")
    
    WA_land_cover_bin <- c(binary_land_cover, WA_land_cover$land_use_change_count)
    
    writeRaster(WA_land_cover_bin, here("data", "land_cover", "WA_land_cover_binary.tif"), overwrite = TRUE)
    
  } else {
    
    WA_land_cover <- rast(here("data", "land_cover", "WA_land_cover_binary.tif"))
    crs(WA_land_cover) <- project_crs
    
  }
  
  # NDVI https://lpdaac.usgs.gov/products/myd13c2v061/
  
  if(!file.exists(here("data", "ndvi", "WA_ndvi_monthly.tif"))) {
    
    list_ndvi <- list.files(here("data", "ndvi"))[2:13]
    
    ndvi_raster <- list()
    
    for(i in 1:length(ndvi_monthly)) {
      
      ndvi_raster[[i]] <- rast(here("data", "ndvi", list_ndvi[i]))[[1]]
      crs(ndvi_raster[[i]]) <- project_crs
      ndvi_raster[[i]] <- crop(ndvi_raster[[i]], WA_shape)
      
    }
    
    ndvi_monthly <- stack(x = c(ndvi_raster[[1]], ndvi_raster[[2]], ndvi_raster[[3]], ndvi_raster[[4]], ndvi_raster[[5]], ndvi_raster[[6]],
                                ndvi_raster[[7]], ndvi_raster[[8]], ndvi_raster[[9]], ndvi_raster[[10]], ndvi_raster[[11]], ndvi_raster[[12]]))
    
    names(ndvi_monthly) <- c("NDVI_2021-02", "NDVI_2021-03", "NDVI_2021-04", "NDVI_2021-05", "NDVI_2021-06", "NDVI_2021-07",
                             "NDVI_2021-08", "NDVI_2021-095", "NDVI_2021-10", "NDVI_2021-11", "NDVI_2021-12", "NDVI_2022-01")
    
    writeRaster(ndvi_monthly, here("data", "ndvi", "WA_ndvi_monthly.tif"))
    
  } else {
    
    WA_ndvi <- rast(here("data", "ndvi", "WA_ndvi_monthly.tif"))
    
    WA_ndvi$c_v_ndvi <- rast(cv(as(WA_ndvi, "Raster")))
    
    crs(WA_ndvi) <- project_crs
    
  }
  
  raster_extents <- tibble(rast_name = c(rep("bioclim", 4),
                                         rep("elevation", 4),
                                         rep("WA_population", 4),
                                         rep("WA_land_cover", 4),
                                         rep("WA_ndvi", 4)),
                           param = rep(c("xmin", "xmax", "ymin", "ymax"), 5),
                           extent = c(ext(bioclim_stack)@ptr[["vector"]],
                                      ext(elevation)@ptr[["vector"]],
                                      ext(WA_population)@ptr[["vector"]],
                                      ext(WA_land_cover)@ptr[["vector"]],
                                      ext(WA_ndvi)@ptr[["vector"]])) %>%
    pivot_wider(names_from = "param", values_from = "extent")
  
  bioclim_stack <- resample(bioclim_stack, bioclim_stack)
  elevation <- resample(elevation, bioclim_stack)
  WA_population <- resample(WA_population, bioclim_stack)
  WA_land_cover <- resample(WA_land_cover, bioclim_stack, method = "near")
  WA_ndvi <- resample(WA_ndvi, bioclim_stack)
  
  all_vars <- c(bioclim_stack, elevation, WA_population, WA_land_cover, rast(NDVI_stack))
  
  all_vars$log_pop_density <- log10(all_vars$pop_density)
  
  write_rds(names(all_vars), here("data", "covariate_raster_names.rds"))
  writeRaster(all_vars, here("data", "covariate_raster_full.tif"), overwrite = TRUE)
  
  removeTmpFiles(h = 0)
  
} else {
  
  covariate_raster <- rast(here("data", "covariate_raster_full.tif"))
  crs(covariate_raster) <- project_crs
  
}
