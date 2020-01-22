# Save all .tif files to .Rda

psnt_raster <- raster("data-raw/psnt_raster.tif")
long_raster <- raster("data-raw/long_raster.tif")
pfl_raster  <- raster("data-raw/pfl_raster.tif")

lake_raster <- list(Pleasant = psnt_raster,
                    Long = long_raster,
                    Plainfield = pfl_raster)

usethis::use_data(lake_raster, overwrite = TRUE, compress = "xz")
