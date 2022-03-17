WA_countries <- c("BEN", "BFA", "CIV", "GMB", "GHA", "GIN", "GNB", "LBR",
                  "MLI", "MRT", "NER", "NGA", "SEN", "SLE", "TGO")


project_crs <- "GEOGCRS[\"WGS 84\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4326]]"

land_use_categories <- c("no_data", rep("agriculture", 6), rep("forest", 5), "grassland", rep("shrubland", 3), "grassland", rep("sparse_vegetation", 3),
                         rep("forest", 2), "wetland", "urban", rep("bare_areas", 2), "water")

classification_raster <- matrix(c(0, 0, 0, # no data
                                  0, 39, 1, # agriculture
                                  10, 11, 2, # agriculture
                                  11, 12, 3, # agriculture
                                  12, 20, 4, # agriculture
                                  20, 30, 5, # agriculture
                                  30, 40, 6, # agriculture
                                  40, 50, 7, # forest
                                  50, 60, 8, # forest
                                  60, 61, 9, # forest
                                  61, 62, 10, # forest
                                  62, 100, 11, # forest
                                  100, 110, 12, # grassland
                                  110, 120, 13, # shrubland
                                  120, 122, 14, # shrubland
                                  122, 130, 15, # shrubland
                                  130, 150, 16, # grassland
                                  150, 152, 17, # sparse vegetation
                                  152, 153, 18, # sparse vegetation
                                  153, 160, 19, # sparse vegetation
                                  160, 169, 20, # forest
                                  170, 179, 21, # forest
                                  180, 189, 22, # wetland
                                  190, 199, 23, # urban
                                  199, 201, 24, # bare
                                  201, 202, 25, # bare
                                  202, 255, 26), # water
                                ncol = 3, byrow = TRUE)

names(land_use_categories) <- c(classification_raster[,3])

simplified_landuse <- matrix(c(0, 0, # no data
                               1, 1,
                               2, 1,
                               3, 1,
                               4, 1,
                               5, 1,
                               6, 1, # 1-6 become agriculture (1)
                               7, 2,
                               8, 2,
                               9, 2,
                               10, 2,
                               11, 2, # 7-11 become forest (2)
                               12, 3, # grassland (3)
                               13, 4,
                               14, 4,
                               15, 4, # 13-15 become shrubland (4)
                               16, 3, # grassland (3)
                               17, 5,
                               18, 5,
                               19, 5, # 16-19 become sparse_vegetation (5)
                               20, 2,
                               21, 2, # forest (2)
                               22, 6, # wetland (6)
                               23, 7, # urban (7)
                               24, 8, 
                               25, 8, # 24-25 bare areas (8)
                               26, 9), # water (9)
                               ncol = 2, byrow = TRUE)
