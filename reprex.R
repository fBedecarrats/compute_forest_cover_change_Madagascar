# Install latest version of mapme.biodiversity
remotes::install_github("mapme-initiative/mapme.biodiversity",
                        upgrade = "always")
# Installing needed packages
required_libs <- c("tidyverse", "readxl", "writexl", "sf", "geodata",
                   "terra", "tmap")
missing_libs <- !(required_libs %in% installed.packages())
if(any(missing_libs)) install.packages(required_libs[missing_libs])
purrr::map(required_libs, library, character.only = TRUE)
library(mapme.biodiversity)
# Download Madagascar national boundaries
dir.create("gadm")
contour_mada <- gadm(country = "Madagascar", resolution = 1, level = 0,
                     path = "gadm") %>%
  st_as_sf()

# THIS DOESN'T WORK
mada_poly  <- contour_mada %>% 
  st_cast("POLYGON")

mada_poly <- init_portfolio(mada_poly ,
                            years = 2000:2020,
                            outdir  = "out_Mada",
                            cores = 18,
                            add_resources = TRUE)

mada_poly  <- get_resources(x = mada_poly , 
                            resources = c("gfw_treecover", "gfw_lossyear"))

mada_poly  <- calc_indicators(x = mada_poly,
                              indicators = "treecover_area", 
                              min_cover = 10, min_size = 1)

# PROBLEM: The Madagascar mainland gets NA values for treecover

# Note: attr(mada_poly, "resources")$gfw_lossyear indicates the gpkg associated
# in this case: "out_Mada/gfw_lossyear/tileindex_gfw_lossyear.gpkg"
# So we read it to 'cut' the polygons from its borders

footprint_treecover_tiles <- st_read(attr(mada_poly, "resources")$gfw_treecover) 
footprint_lossyear_tiles <- st_read(attr(mada_poly, "resources")$gfw_lossyear)

mada_poly2  <- mada_poly %>% 
  st_intersection(footprint_treecover_tiles) %>%
  # Have to re-cast back and forth because st_intersection() created 
  # multipolygons among polygons
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")
  

mada_poly2 <- init_portfolio(mada_poly2,
                            years = 2000:2020,
                            outdir  = "out_Mada",
                            cores = 18,
                            add_resources = TRUE)

mada_poly2  <- get_resources(x = mada_poly2, 
                            resources = c("gfw_treecover", "gfw_lossyear"))

mada_poly2  <- calc_indicators(x = mada_poly2,
                              indicators = "treecover_area", 
                              min_cover = 10, min_size = 1)


mada_global <- mada_poly2 %>%
  unnest(treecover_area) %>%
  # filter(!is.na(years)) %>%
  pivot_wider(names_from = "years", values_from = "treecover", 
              names_prefix = "treecover_") %>%
  st_drop_geometry() %>%
  select(-assetid) %>%
  group_by(across(!starts_with("treecover"))) %>%
  summarise(across(starts_with("treecover"), sum, na.rm = TRUE))

library(writexl)
write_xlsx(Madagascar = mada_global,
           path = "couvert_forestier_mada.xlsx")
