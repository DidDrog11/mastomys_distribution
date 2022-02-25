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
  
}

# elevation

# gridded population of the world

# land cover
