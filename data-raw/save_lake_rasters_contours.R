# Save Lake Rasters and Contours

# 1. RASTERS. Load rasters as exported from ArcGIS and adds to the CSLSlevels
#             package. These rasters are clipped to approximately the highest
#             observed lake level (in 2019) plus a spatial buffer of 20m.
# 2. CONTOURS. Using rasters, draw 0.1m contour lines spanning from just above
#              the highest observed level to the minimum elevation of the lake.

# 0. SETUP ---------------------------------------------------------------------
# Load libraries
library(usethis)
library(raster)
library(CSLSdata)
library(rgdal)
library(sp)

# 1. RASTERS -------------------------------------------------------------------
psnt_raster <- raster("data-raw/psnt_raster.tif")
long_raster <- raster("data-raw/long_raster.tif")
pfl_raster  <- raster("data-raw/pfl_raster.tif")

lake_raster <- list(Pleasant = psnt_raster,
                    Long = long_raster,
                    Plainfield = pfl_raster)

usethis::use_data(lake_raster, overwrite = TRUE, compress = "xz")

# 2. CONTOURS ------------------------------------------------------------------
convert_to_contours <- function(lake, contour_interval_m = 0.1, buffer_m = 0.2){
  this_raster <- CSLSlevels::lake_raster[[lake]]
  min_elev    <- round(minValue(this_raster), 2)  # bottom of lake bed
  high_levels <- CSLSdata::lake_levels[[lake]]$level_m  # 2018-2019 lake obs.
  max_elev    <- round(max(high_levels, na.rm = TRUE), 1) + buffer_m
  lake_levels <- seq(max_elev, min_elev, -contour_interval_m)
  contours    <- rasterToContour(this_raster, levels = lake_levels)
  return(contours)
}

# Draw contours from raster
psnt_contours <- convert_to_contours("Pleasant")
long_contours <- convert_to_contours("Long")
pfl_contours  <- convert_to_contours("Plainfield")

# Save contours
writeOGR(psnt_contours,
         dsn = "data-raw/contours",
         layer = "psnt_contours",
         driver = "ESRI Shapefile")
writeOGR(long_contours,
         dsn = "data-raw/contours",
         layer = "long_contours",
         driver = "ESRI Shapefile")
writeOGR(pfl_contours,
         dsn = "data-raw/contours",
         layer = "pfl_contours",
         driver = "ESRI Shapefile")

# 3. PLANT SHORLINES -----------------------------------------------------------
# LONG LAKE
lake        <- "Long"
lake_raster <- CSLSlevels::lake_raster[[lake]]
lake_levels <- CSLSdata::lake_levels[[lake]]

# Long Lake 8/7/2018
plant_date      <- as_datetime(mdy("08-07-2018"))
lake_level      <- lake_levels %>% filter(.data$date == plant_date)
lake_level      <- lake_level$level_m
long_2018_08_07 <- rasterToContour(lake_raster, levels = lake_level)
writeOGR(long_2018_08_07,
         dsn = "data-raw/shorelines",
         layer = "long_2018_08_07",
         driver = "ESRI Shapefile")

# Long Lake 8/20/2019
plant_date      <- as_datetime(mdy("08-20-2019"))
lake_level      <- lake_levels %>% filter(.data$date == plant_date)
lake_level      <- lake_level$level_m
long_2019_08_20 <- rasterToContour(lake_raster, levels = lake_level)
writeOGR(long_2019_08_20,
         dsn = "data-raw/shorelines",
         layer = "long_2019_08_20",
         driver = "ESRI Shapefile")

# PLAINFIELD LAKE
lake        <- "Plainfield"
lake_raster <- CSLSlevels::lake_raster[[lake]]
lake_levels <- CSLSdata::lake_levels[[lake]]

# Plainfield Lake 8/7/2018
plant_date      <- as_datetime(mdy("08-07-2018"))
lake_level      <- lake_levels %>% filter(.data$date == plant_date)
lake_level      <- lake_level$level_m
pfl_2018_08_07 <- rasterToContour(lake_raster, levels = lake_level)
writeOGR(pfl_2018_08_07,
         dsn = "data-raw/shorelines",
         layer = "pfl_2018_08_07",
         driver = "ESRI Shapefile")

# Plainfield Lake 9/24/2018
plant_date      <- as_datetime(mdy("09-24-2018"))
lake_level      <- lake_levels %>% filter(.data$date == plant_date)
lake_level      <- lake_level$level_m
pfl_2018_09_24 <- rasterToContour(lake_raster, levels = lake_level)
writeOGR(pfl_2018_09_24,
         dsn = "data-raw/shorelines",
         layer = "pfl_2018_09_24",
         driver = "ESRI Shapefile")

# Plainfield Lake 8/20/2019
plant_date      <- as_datetime(mdy("08-20-2019"))
lake_level      <- lake_levels %>% filter(.data$date == plant_date)
lake_level      <- lake_level$level_m
pfl_2019_08_20 <- rasterToContour(lake_raster, levels = lake_level)
writeOGR(pfl_2019_08_20,
         dsn = "data-raw/shorelines",
         layer = "pfl_2019_08_20",
         driver = "ESRI Shapefile")
