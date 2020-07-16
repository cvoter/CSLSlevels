# Calculate imputed lake levels

library(tidyverse)
library(lubridate)
library(usethis)
library(CSLSlevels)

# PARAMETERS: Lakes of interest
lakes <- c("Pleasant", "Long", "Plainfield", "Devils")

# INPUTS: Load inputs for BSTS prediction
study_lakes <- read.csv("data-raw/study_lakes_pred_levs_20200302.csv")
study_obs   <- study_lakes %>%
               mutate(obs_mo = as.POSIXct(.data$obs_mo)) %>%
               select(.data$obs_mo, .data$lake_name, .data$swims_obs,
                      .data$usgs_obs, .data$airphoto_obs, .data$lake_lev_obs)

# CALCULATIONS 1/2: Loop through lake level prediction for lakes of interest ---
levels_bsts <- NULL
for (lake in lakes) {
  if (lake == "Devils") {
    devils_lake <- study_lakes %>%
                   filter(year(as.character(.data$obs_mo)) < 2002)
    this_lake   <- predict_lake_levels(devils_lake,
                                       start_date = "1936-01-01",
                                       lake_name = sprintf("%s Lake", lake))
  } else {
    this_lake   <- predict_lake_levels(study_lakes,
                                       lake_name = sprintf("%s Lake", lake))
  }
  this_lake$lake      <- lake
  levels_bsts[[lake]] <- this_lake
}
levels_bsts <- bind_rows(levels_bsts)

# Select useful columns (incl. observations)
levels_bsts <- levels_bsts %>%
               left_join(study_obs, by = c("obs_mo", "lake_name")) %>%
               select(lake = .data$lake,
                      date = .data$obs_mo,
                      sim = .data$sim,
                      level = .data$level,
                      mean = .data$mean,
                      sd = .data$sd,
                      p90 = .data$p90,
                      p10 = .data$p10,
                      SE = .data$SE,
                      RMSE = .data$RMSE,
                      rank = .data$rank,
                      level_obs = .data$lake_lev_obs,
                      usgs_obs = .data$usgs_obs,
                      swims_obs = .data$swims_obs,
                      airphoto_obs = .data$airphoto_obs)
levels_bsts$lake <- factor(levels_bsts$lake, levels = lakes)

# Filter to single model plus summary info for saving
hist_levels       <- levels_bsts %>%
                     group_by(.data$lake, .data$date) %>%
                     mutate(max = max(.data$level),
                            min = min(.data$level),
                            upper = ifelse(.data$max < .data$mean + 2*.data$sd,
                                           .data$max, .data$mean + 2*.data$sd),
                            lower = ifelse(.data$min > .data$mean - 2*.data$sd,
                                           .data$min, .data$mean - 2*.data$sd)) %>%
                     ungroup() %>%
                     filter(.data$rank == 1) %>%
                     select(.data$lake,
                            .data$date,
                            .data$level,
                            .data$mean,
                            .data$sd,
                            .data$p90,
                            .data$p10,
                            .data$min,
                            .data$max,
                            .data$upper,
                            .data$lower,
                            .data$level_obs,
                            .data$usgs_obs,
                            .data$swims_obs,
                            .data$airphoto_obs)

# CALCULATIONS 2/2: Calculate hydrologic metrics for each sim for each lake ----
hist_metrics_long  <- NULL
hist_metrics_short <- NULL
for (sim in unique(levels_bsts$sim)) {
  if (sim %% 20 == 0) {
    message(sprintf("Starting sim %s", sim))
  }
  # 1938-2019 metrics (or 1936-2001 for Devils Lake)
  this_rank    <- levels_bsts %>%
                  filter(.data$sim == !!sim) %>%
                  select(lake = .data$lake,
                         date = .data$date,
                         level_pred = .data$level)
  this_sim     <- calculate_metrics(this_rank)
  this_sim$sim <- sim
  hist_metrics_long[[sim]] <- this_sim

  # 1981-2018 metrics
  this_rank    <- this_rank %>%
                  filter(year(.data$date) >= 1981,
                         year(.data$date) <= 2018)
  this_sim     <- calculate_metrics(this_rank)
  this_sim$sim <- sim
  hist_metrics_short[[sim]] <- this_sim
}
hist_metrics_long  <- bind_rows(hist_metrics_long)
hist_metrics_short <- bind_rows(hist_metrics_short)

# SAVE: Write out
# usethis::use_data(levels_bsts, overwrite = TRUE, compress = "xz")
usethis::use_data(hist_levels, hist_metrics_long, hist_metrics_short,
                  overwrite = TRUE, compress = "xz")
