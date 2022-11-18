# Install latest version of mapme.biodiversity
remotes::install_github("mapme-initiative/mapme.biodiversity",
                        upgrade = "always")
# Installing needed packages
required_libs <- c("tidyverse", "readxl", "writexl", "sf", "geodata",
                   "terra", "gdalUtils", "rgdal")
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

# THIS WORKS
# Hint for the solution comes from: 
# https://stackoverflow.com/questions/15876591/merging-multiple-rasters-in-r

# Rasters are tool large to merge with terra, so I use rgdal
install.packages("rgdal")
# rgdalUtil install doesn't work for CRAN, using {remotes}
remotes::install_github("gearslaboratory/gdalUtils",
                        upgrade = "always")
library(rgdal)
library(gdalUtils)



# Merging the GFC rasters into 1 mosaic
my_dirs <- c("all_mada", "all_mada/gfw_treecover", "all_mada/gfw_lossyear")
map(my_dirs, dir.create)

tc_tifs <- list.files("out_Mada/gfw_treecover", pattern = ".tif", 
                      full.names = TRUE)


gdal_setInstallation()
mosaic_rasters(gdalfile=c(tc_tifs[[1]], tc_tifs[[2]], tc_tifs[[3]]),
               dst_dataset="all_mada/gfw_treecover/all.tif",
               output_Raster = TRUE,
               of="GTiff", verbose=TRUE)

ly_tifs <- list.files("out_Mada/gfw_lossyear", pattern = ".tif", 
                      full.names = TRUE)
gdal_setInstallation()
mosaic_rasters(gdalfile=c(ly_tifs[[1]], ly_tifs[[2]], ly_tifs[[3]]),
               dst_dataset="all_mada/gfw_lossyear/all.tif",
               output_Raster = TRUE,
               of="GTiff", verbose=TRUE)

# Recreating the spatial index with the mosaïcs, method described in: 
# https://github.com/mapme-initiative/mapme.biodiversity/issues/92

manual_index <- function(outdir, resource) {
  rundir <- paste(outdir, resource, sep = "/")
  tindex_file <- file.path(rundir, paste0("tileindex_", resource, ".gpkg"))
  
  downloaded_files <- list.files(rundir, full.names = TRUE)
  footprints <- lapply(downloaded_files, function(file) {
    tmp <- rast(file)
    footprint <- st_as_sf(st_as_sfc(st_bbox(tmp)))
    st_geometry(footprint) <- "geom"
    footprint$location <- sources(tmp)
    footprint
  })
  footprints <- do.call(rbind, footprints)
  write_sf(footprints, dsn = tindex_file)
}

manual_index(outdir = "all_mada", resource = "gfw_treecover")
manual_index(outdir = "all_mada", resource = "gfw_lossyear")

# Now we can compute again

mada_poly2  <- contour_mada %>% 
  st_cast("POLYGON")

mada_poly2 <- init_portfolio(mada_poly2,
                            years = 2000:2020,
                            outdir  = "all_mada",
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
              names_prefix = "treecover_")
