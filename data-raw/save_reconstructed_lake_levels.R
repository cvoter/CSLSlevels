# Load reconstructed lake levels

# Reads in csv with reconstructed lake levels, as provided by Bob Smail.
# Reconstructed lake levels are modeled as a function of the cumulative
# deviation of precipitation.

library(stringr)
library(dplyr)
library(lubridate)

include_devils <- TRUE

filename <- "study_lakes_bsts_levs_20200304.csv"
csls_levels <- read.csv(sprintf("data-raw/%s", filename))
csls_levels <- csls_levels %>%
               mutate(lake = str_replace(.data$lake_name, " Lake", ""),
                      date = as_datetime(.data$obs_mo),
                      level_obs = lake_lev_obs,
                      level_pred = bsts_lev_pred) %>%
               mutate_at("lake", as.factor) %>%
               select(.data$lake,
                      .data$date,
                      .data$level_obs,
                      .data$level_pred,
                      .data$usgs_obs,
                      .data$swims_obs,
                      .data$airphoto_obs)

csls_levels$lake <- factor(csls_levels$lake,
                           levels = c("Pleasant", "Long", "Plainfield"))
usethis::use_data(csls_levels, overwrite = TRUE, compress = "xz")

if (include_devils) {
  filename      <- "study_lakes_bsts_levs_20200218.csv"
  devils_levels <- read.csv(sprintf("data-raw/%s", filename))
  devils_levels <- devils_levels %>%
                   filter(.data$lake_name == "Devils Lake") %>%
                   mutate(lake = str_replace(.data$lake_name, " Lake", ""),
                          date = as_datetime(.data$obs_mo),
                          level_obs = lake_lev_obs,
                          level_pred = lake_lev_pred) %>%
                   mutate_at("lake", as.factor) %>%
                   select(.data$lake,
                          .data$date,
                          .data$level_obs,
                          .data$level_pred,
                          .data$usgs_obs,
                          .data$swims_obs,
                          .data$airphoto_obs)
  csls_levels  <- rbind(csls_levels, devils_levels)
  csls_levels$lake <- factor(csls_levels$lake,
                             levels = c("Pleasant", "Long", "Plainfield", "Devils"))
usethis::use_data(csls_levels, overwrite = TRUE, compress = "xz")
}
