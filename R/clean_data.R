# Cleaning the rodent data and limiting to confirmed Mastomys natalensis

# Retain only mastomys natalensis from downloaded data

m_nat <- rodent_data %>%
  filter(str_detect(genus, "mastomys") & str_detect(species, "natalensis")) %>%
  mutate(species = str_to_sentence(paste(genus, species))) %>%
  select(species, country, latitude_DMS_N, longitude_DMS_W, latitude_D_N, longitude_D_E, UTM_coordinates, number) %>%
  separate(col = longitude_DMS_W, into = c("long_degrees", "long_minutes", "long_seconds"), "_", remove = F) %>%
  separate(col = latitude_DMS_N, into = c("lat_degrees", "lat_minutes", "lat_seconds"), "_") %>%
  mutate(across(all_of(c("long_degrees", "long_minutes", "long_seconds","lat_degrees", "lat_minutes", "lat_seconds")), as.double),
         long_hemi = ifelse(long_degrees<0, "E", #assign -ve numbers to E
                            ifelse(substring(longitude_DMS_W, 1, 1) == "-", "E", "W")), #as 0 cannot be -ve we can check the sign on the text entry
         lat_hemi = ifelse(lat_degrees<0, "S", "N"),
         gps_dms = ifelse(is.na(lat_degrees|long_degrees), F, T),
         long_dms = ifelse(gps_dms == T,
                           paste(long_hemi, long_degrees, " ",
                                 ifelse(is.na(long_minutes), 0, long_minutes), '.',
                                 ifelse(is.na(long_seconds), 0, long_seconds),
                                 sep = ""), NA),
         lat_dms = ifelse(gps_dms == T,
                          paste(lat_hemi, lat_degrees, " ",
                                ifelse(is.na(lat_minutes), 0, lat_minutes), '.',
                                ifelse(is.na(lat_seconds), 0, lat_seconds),
                                sep = ""), NA),
         long_dms = gsub("-", "", long_dms),
         lat_dms = gsub("-", "", lat_dms),
         iso3c = countrycode(as.character(country), "country.name", "iso3c")) %>%
  select(-longitude_DMS_W)

# Converting coordinate types into consistent decimal degrees
dms <- m_nat %>%
  drop_na(long_dms, lat_dms)

dms <- dms %>%
  mutate(lon_dd = parzer::parse_lon(long_dms),
         lat_dd = parzer::parse_lat(lat_dms)) %>%
  st_as_sf(coords = c("lon_dd", "lat_dd"), crs = "+proj=longlat +datum=WGS84")

dd <- m_nat %>%
  drop_na(longitude_D_E,latitude_D_N) %>%
  mutate(lon_dd = longitude_D_E,
         lat_dd = latitude_D_N) %>%
  st_as_sf(coords = c("lon_dd", "lat_dd"), crs = "+proj=longlat +datum=WGS84")

m_nat_gps <- bind_rows(dms, dd) %>%
  select(species, iso3c, number, geometry) %>%
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2])

st_crs(m_nat_gps) = 4326

rm(dms, dd)

# Convert to presence and absence data

m_nat_presence <- m_nat_gps %>%
  filter(number >= 1) %>%
  mutate(m_nat = 1) %>%
  select(-number)

m_nat_absence <- m_nat_gps %>%
  filter(number == 0) %>%
  mutate(m_nat = 0) %>%
  select(-number)

# Convert GBIF data to presence only and remove non-WA countries

gbif_presence <- gbif_data %>%
  select(species, country = countryCode, x = decimalLongitude, y = decimalLatitude) %>%
  mutate(iso3c = countrycode(country, "iso2c", "iso3c")) %>%
  filter(iso3c %in% WA_countries) %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE) %>%
  mutate(m_nat = 1) %>%
  select(species, iso3c, geometry, x, y, m_nat)

# Combine m_nat_presence and gbif_presence

combined_presence <- bind_rows(m_nat_presence, gbif_presence)
