WA_countries <- c("BEN", "BFA", "CIV", "GMB", "GHA", "GIN", "GNB", "LBR",
                  "MLI", "MRT", "NER", "NGA", "SEN", "SLE", "TGO")


project_crs <- "+proj=utm +zone=48 +datum=WGS84"

land_use_categories <- c("no_data", "cropland", "cropland", "cropland", "cropland", "mosaic_cropland", "mosaic_natural_vegetation", "tree_cover",
                         "tree_cover", "tree_cover", "tree_cover", "mosaic_tree_shrub", "mosaic_herbaceous", "shrubland", "shrubland",
                         "grassland", "sparse_vegetation", "sparse_shrub", "sparse_herbaceous", "tree_cover", "tree_cover", "shrub_cover", "urban_areas", 
                         "bare_areas", "bare_areas", "bare_areas", "water_bodies")
# names(land_use_categories) <- c(0, 10, 11, 12, 20, 30, 40, 50, 60, 61, 62, 100, 110, 120, 122, 130,
#                                 150, 152, 153, 160, 170, 180, 190, 200, 201, 202, 210)
classification_raster <- matrix(c(0, 0, 0,
                                  0, 10, 1,
                                  10, 11, 2,
                                  11, 12, 3,
                                  12, 20, 4,
                                  20, 30, 5,
                                  30, 40, 6,
                                  40, 50, 7,
                                  50, 60, 8,
                                  60, 61, 9,
                                  61, 62, 10,
                                  62, 100, 11,
                                  100, 110, 12,
                                  110, 120, 13,
                                  120, 122, 14,
                                  122, 130, 15,
                                  130, 150, 16,
                                  150, 152, 17,
                                  152, 153, 18,
                                  153, 160, 19,
                                  160, 170, 20, 
                                  170, 180, 21,
                                  180, 190, 22,
                                  190, 200, 23,
                                  200, 201, 24,
                                  201, 202, 25,
                                  202, 255, 26),
                                ncol = 3, byrow = TRUE)
