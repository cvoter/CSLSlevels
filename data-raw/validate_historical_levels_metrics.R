# Validate imputed lake levels using other seepage lakes

# To Do:
# * Add a step which merges the complete observation record for Anvil with the
#   record in "obs_df" (read in from Bob's csv file).
# * Add another loop which systematically adds lake level observations to
#   z_score prediction (start with at least 2 obs, not zero). Evaluate how the
#   lake standard deviation AND mean level change with more observations. Will
#   want several iterations here (as in gap analysis).
# * Run with an increase number of iterations on gaps (and additions?). Need
#   more iterations for small gaps than large gaps, but not sure if there's an
#   objective way to decide that (or if it's easier to just say, 100 iterations
#   for everything!). The BSTS prediction is the limiting factor. On my laptop,
#   it takes ~100 second for a full BSTS predition for Devils Lake (using
#   1936-2001 data). Goes faster for shorter time series.
# * Add in calculation of hydrologic metrics to the loops. This may take almost
#   as long as the BSTS prediction. Calculate RMSE in the prediction of these
#   metrics in additon to the RMSE of the lake level observations (will likely
#   need to calculate "TRUE" hydrologic metrics before loops so can refer back
#   to them same way refer back to full set of lake level observations.)
# * Save whatever data we'll need to access later (see
#   calculate_historical_Levels_metrics.R)

# Notes
# * Think about if we need more than just RMSE as a "goodness-of-fit" metrics.
#   It works better than R2 and NSE for small datasets (e.g., gaps of < 1 year).
#   But it's also true that RMSE only measures one aspect of error (accuracy).
#   Could also add in PBIAS.
# * Be careful of start_dates. I keep encountering errors of "Error: Problem
#   with `filter()` input `..1`." due to a line in the BSTS function
#   (predict_lake_levels) that filters the data frame based on the start date.
#   It's always that I'm using the wrong start date.
# * Please do check this code as you use it/expand it. I try to QC it as I go,
#   but it's easy to miss things.
# * I've been doing this in data-raw/ rather than in R/ because it takes so long I
#   only want to run it once, then save the resulting data frame (as in
#   calculate_historical_levels_metrics.R and
#   calculate_timeseries_length_sensitivity.R). Eventually it may make sense to
#   take some of the functions and/or plotting out of this script and move it to
#   functions within R/.

# Related code:
# * R/predict_lake_levels.R
# * data-raw/calculate_historical_levels_metrics.R
# * data-raw/calculate_timeseries_length_sensitivity.R
# * Bob's code: "\\dnr\programs\DG\DG_Projects\Water Use\WU_Central_Sands_Study\data_mgmt\hist_lake_levs"

library(tidyverse)
library(lubridate)
library(usethis)
library(CSLSlevels)
library(NISTunits)
library(zoo)
library(reshape2)

# FUNCTIONS --------------------------------------------------------------------
# Maybe a parallel "select_dates_add" function will be helpful for the
select_dates_drop <- function(this_lake, last_ind, gap) {
  # Dates to drop
  first_drop <- this_lake$date[round(runif(1, 1, last_ind))]
  last_drop  <- first_drop %m+% months(gap - 1)
  dates_drop <- seq(first_drop, last_drop, "1 month")
  # Ensure we're dropping a (close-to) full set of observations
  # For small gaps, "dropping" an obs that was already NA can cause issues
  # with R2, etc. Make sure this makes sense for longer Anvil record (post-2010
  # Anvil has >= 90% coverage in monthly observation)
  ten_percent <- ceiling(0.1*length(dates_drop))
  while(sum(is.na(this_lake$level_obs[this_lake$date %in% dates_drop])) >= ten_percent) {
    first_drop <- this_lake$date[round(runif(1, 1, last_ind))]
    last_drop  <- first_drop %m+% months(gap - 1)
    dates_drop <- seq(first_drop, last_drop, "1 month")
  }
  return(dates_drop)
}

predict_level_z_score <- function(df){
  # Z-score-predicted lake levels
  # Convert ppt cdm Z-score to lake levels
  # Questions for Bob:
  # * Why convert to mean annual at the start?
  # * What is the magic behind this approach for identifying population sd/mean
  #   from pairwise samples?
  annual_obs_z <- df %>%
                  filter(!is.na(level_obs)) %>%
                  group_by(year(.data$date)) %>%
                  summarize(level_obs = mean(level_obs),
                            ppt_cdm_z = mean(ppt_cdm_z),
                            .groups = "drop")
  level_combn <- annual_obs_z %>%
                 do(data.frame((t(combn(.$level_obs, 2)))))
  ppt_combn   <- annual_obs_z %>%
                 do(data.frame((t(combn(.$ppt_cdm_z, 2)))))
  sample      <- bind_cols(level_combn, ppt_combn) %>%
                 mutate(lake = lake)
  colnames(sample) <- c("x1", "x2", "z1", "z2", "lake")
  lake_stats  <- sample %>%
                 mutate(sample_sd = abs((.data$x2-.data$x1)/
                                          (.data$z2-.data$z1)),
                 sample_mean = (((.data$z2*.data$x1)-(.data$z1*.data$x2))/
                                  (.data$z2-.data$z1))) %>%
                 summarize(level_sd = median(.data$sample_sd),
                           level_mean = median(.data$sample_mean))
  z_score_df  <- df %>%
                 mutate(z_score = (ppt_cdm_z*lake_stats$level_sd)+
                          lake_stats$level_mean)
  return(z_score_df)
}

combine_results <- function(z_score_df, bsts_df, this_lake,
                            gap, iter, lake) {
  # Will probably want to adjust this function to spit out the full time series
  # (before summarizing the RMSE etc.) so can calculate hydrologic metrics on
  # them.
  z_score_df$level_obs <- NULL # gaps will be replaced by full observations
  results_df <- z_score_df %>%
                left_join(bsts_df, by = "date") %>%
                left_join(select(this_lake, c("date", "level_obs")),
                          by = "date") %>%
                mutate(cal_val = ifelse(.data$date %in% dates_drop,
                                            "val", "cal")) %>%
                select(date = .data$date,
                       obs = .data$level_obs,
                       cal_val = .data$cal_val,
                       bsts_sd = .data$sd,
                       bsts = .data$bsts,
                       z_score = .data$z_score) %>%
                melt(id.vars = c("date", "obs", "cal_val", "bsts_sd")) %>%
                group_by(.data$cal_val, .data$variable) %>%
                summarise(max_bsts_sd = max(.data$bsts_sd),
                          bsts_sd = median(.data$bsts_sd),
                          R2 = cor(.data$obs, .data$value, use = "complete.obs")^2,
                          NSE = 1 - sum((.data$value - .data$obs)^2, na.rm = TRUE)/
                            sum((.data$obs - mean(.data$obs, na.rm = TRUE))^2,
                                na.rm = TRUE),
                          RMSE = sqrt(mean((.data$obs - .data$value)^2,
                                           na.rm = TRUE)),
                          .groups = "drop") %>%
                mutate(gap = gap,
                       iter = as.character(iter),
                       lake = lake)
  return(results_df)
}

# PARAMETERS: Lakes of interest ------------------------------------------------
# Limit to fairly continuous period (<10% of monthly obs missing)
# Devils Lake limited to pre-pumping diversion (pre-2002)
# Start 5 years earlier for 60-month cdm ppt z-score
lake_info <- data.frame(lake_name = c("Devils Lake", "Lake Huron", "Anvil Lake",
                                      "Pleasant Lake", "Long Lake",
                                      "Plainfield Lake"),
                        start_year = c(1936, 2010, 2010, 1981, 1981, 1981),
                        end_year = c(2001, 2019, 2019, 2018, 2018, 2018))
obs_df    <- read.csv("data-raw/study_lakes_pred_levs_20200302.csv") %>%
             mutate(obs_mo = as.POSIXct(.data$obs_mo)) %>%
             left_join(lake_info, by = "lake_name") %>%
             filter(year(.data$obs_mo) >= .data$start_year-5,
                    year(.data$obs_mo) <= .data$end_year) %>%
             select(lake = .data$lake_name,
                    date = .data$obs_mo,
                    start_year = .data$start_year,
                    ppt_obs = .data$ppt_obs,
                    level_obs = .data$lake_lev_obs)

# Calculate 60-month moving average, then nix the first 60-months
# Calculate cumulative deviation from this 60-month moving mean
# Calculate Z-score
cdm_df <- obs_df %>%
          group_by(.data$lake) %>%
          mutate(ppt_mean_60 = rollmean(ppt_obs,
                                        k=60,
                                        align = "right",
                                        fill = TRUE))  %>%
          filter(year(.data$date) >= start_year) %>%
          mutate(ppt_cdm = cumsum(ppt_obs-ppt_mean_60),
                 ppt_cdm_z = scale(ppt_cdm)) %>%
          ungroup() %>%
          select(.data$lake, .data$date, .data$ppt_obs, .data$ppt_cdm_z,
                 .data$level_obs)

# Looping limits
lakes   <- "Lake Huron" #c("Devils Lake", "Lake Huron", "Anvil Lake")
gaps    <- c(1, 3, 6, 9, 12, 2*12, 3*12, 4*12, 5*12, 7*12, 10*12)
iters   <- seq(1,30)
summary <- NULL

# Loop through lakes, gaps, and iterations
for (lake in lakes) {
  this_lake  <- cdm_df %>%
                filter(.data$lake == !!lake) %>%
                mutate(lake = as.character(.data$lake))
  start_date <- min(this_lake$date)
  for (gap in gaps) {
    # Last possible date index to start with
    last_date <- max(this_lake$date) %m-% months(gap - 1)
    last_ind  <- which(this_lake$date == last_date)
    for (iter in iters) {
      dates_drop <- select_dates_drop(this_lake, last_ind, gap)
      gaps_df    <- this_lake %>%
                    mutate(level_obs = ifelse(as.POSIXct(.data$date) %in%
                                                dates_drop,
                                              NA,
                                              .data$level_obs))
      # Z-score-predicted lake levels
      z_score_df <- predict_level_z_score(gaps_df)

      # BSTS-predicted lake levels
      bsts_input <- z_score_df %>%
                    select(lake_name = .data$lake,
                           obs_mo = .data$date,
                           ppt_obs = .data$ppt_obs,
                           ppt_cdm_z = .data$ppt_cdm_z,
                           lake_lev_obs = .data$level_obs,
                           lake_lev_pred = .data$z_score)
      bsts_df    <- predict_lake_levels(bsts_input,
                                        lake_name = lake,
                                        start_date = start_date) %>%
                    filter(.data$rank == 1) %>%
                    select(date = .data$obs_mo,
                           bsts = .data$level,
                           sd = .data$sd)

      # Combine predictions & observations, calculate goodness-of-fit
      results_df  <- combine_results(z_score_df, bsts_df, this_lake,
                                     gap, iter, lake)
      summary     <- bind_rows(summary, results_df)
    }
  }
  for (study_lake in c("Pleasant Lake", "Long Lake", "Plainfield Lake")) {
    study_df     <- cdm_df %>%
                    filter(.data$lake == study_lake)
    start_date   <- min(study_df$date)
    end_date     <- max(study_df$date)
    this_lake    <- cdm_df %>%
                    filter(.data$lake == !!lake,
                           .data$date >= start_date,
                           .data$date <= end_date) %>%
                    mutate(lake = as.character(.data$lake))
    study_no_obs <- study_df %>%
                    filter(is.na(.data$level_obs))
    dates_drop   <- study_no_obs$date
    gaps_df      <- this_lake %>%
                    mutate(level_obs = ifelse(as.POSIXct(.data$date) %in%
                                                dates_drop, NA, .data$level_obs))
    # Z-score-predicted lake levels
    z_score_df <- predict_level_z_score(gaps_df)

    # BSTS-predicted lake levels
    bsts_input <- z_score_df %>%
                  select(lake_name = .data$lake,
                         obs_mo = .data$date,
                         ppt_obs = .data$ppt_obs,
                         ppt_cdm_z = .data$ppt_cdm_z,
                         lake_lev_obs = .data$level_obs,
                         lake_lev_pred = .data$z_score)
    bsts_df    <- predict_lake_levels(bsts_input,
                                      lake_name = lake,
                                      start_date = start_date) %>%
                  filter(.data$rank == 1) %>%
                  select(date = .data$obs_mo,
                         bsts = .data$level,
                         sd = .data$sd)

    # Combine predictions & observations, calculate goodness-of-fit
    results_df  <- combine_results(z_score_df, bsts_df, this_lake,
                                   gap, study_lake, lake)
    summary     <- bind_rows(summary, results_df)
  }
}
# Convert from meters to feet
summary <- summary %>%
           mutate_at(c("bsts_sd", "max_bsts_sd", "RMSE"), NISTmeterTOft)

# Plot gap validation performance
plot_df <- melt(summary, id.vars = c("lake", "iter", "gap", "cal_val", "variable"))
colnames(plot_df) <- c("lake", "iter", "gap", "cal_val", "method", "variable", "value")
# Hacky way to get PSNT, LONG, and PFL scenarios to plot on a continuous x-axis.
plot_df <- plot_df %>%
           mutate(gap = ifelse(.data$iter == "Pleasant Lake",
                               11*12, ifelse(.data$iter == "Long Lake",
                                             12*12, ifelse(.data$iter == "Plainfield Lake",
                                                           13*12, .data$gap))))
# Get median/min/max values for point-range plots
plot_df <- plot_df %>%
           filter(!is.infinite(.data$value)) %>%
           group_by(.data$lake, .data$gap, .data$cal_val, .data$method, .data$variable) %>%
           summarise(median = median(.data$value, na.rm = TRUE),
                     max = max(.data$value, na.rm = TRUE),
                     min = min(.data$value, na.rm = TRUE),
                     .groups = "drop")

# Plot
ggplot(filter(plot_df,
              .data$cal_val == "val" & .data$variable %in% c("RMSE"))) +
  geom_pointrange(aes(x = .data$gap/12,
                      y = .data$median,
                      ymin = .data$min,
                      ymax = .data$max,
                      group = .data$method,
                      color = .data$method,
                      shape = .data$method),
                  position = position_dodge(width = 0.25),
                  size = 1) +
  # facet_wrap(~variable, scales = "free") +
  scale_x_continuous(breaks = seq(0,13,1),
                     labels = c(as.character(seq(0,10,1)), "PSNT", "LONG", "PFL")) +
  scale_color_manual(name = "",
                     breaks = c("z_score", "bsts"),
                     values = c("blue", "red")) +
  labs(x = "Gap (years)",
       y = "RMSE (ft)",
       color = "",
       shape = "") +
  theme_bw() +
  theme(text = element_text(family = "Segoe UI Semilight",
                            size = 18),
        legend.position = "top")

# PLOT TIME SERIES -------------------------------------------------------------
# Uses "cdm_df" and "lake" from earlier in the code.
# Retains time series instead of summarizing them.
# Should be condensed/included in earlier loop.
all_timeseries <- NULL
for (study_lake in c("Pleasant Lake", "Long Lake", "Plainfield Lake")) {
  study_df     <- cdm_df %>%
                  filter(.data$lake == study_lake)
  start_date   <- min(study_df$date)
  end_date     <- max(study_df$date)
  this_lake    <- cdm_df %>%
                  filter(.data$lake == !!lake,
                         .data$date >= start_date,
                         .data$date <= end_date) %>%
                  mutate(lake = as.character(.data$lake))
  study_no_obs <- study_df %>%
                  filter(is.na(.data$level_obs))
  dates_drop   <- study_no_obs$date
  gaps_df      <- this_lake %>%
                  mutate(level_obs = ifelse(as.POSIXct(.data$date) %in%
                                              dates_drop, NA, .data$level_obs))
  # Z-score-predicted lake levels
  z_score_df <- predict_level_z_score(gaps_df)

  # BSTS-predicted lake levels
  bsts_input <- z_score_df %>%
                select(lake_name = .data$lake,
                       obs_mo = .data$date,
                       ppt_obs = .data$ppt_obs,
                       ppt_cdm_z = .data$ppt_cdm_z,
                       lake_lev_obs = .data$level_obs,
                       lake_lev_pred = .data$z_score)
  bsts_df    <- predict_lake_levels(bsts_input,
                                    lake_name = lake,
                                    start_date = start_date) %>%
                filter(.data$rank == 1) %>%
                select(date = .data$obs_mo,
                       bsts = .data$level,
                       sd = .data$sd)

  z_score_df$level_obs <- NULL # gaps will be replaced by full observations
  this_study_lake <- z_score_df %>%
                     left_join(bsts_df, by = "date") %>%
                     left_join(select(this_lake, c("date", "level_obs")),
                               by = "date") %>%
                     mutate(cal_val = ifelse(.data$date %in% dates_drop,
                            "val", "cal"),
                            study_lake = study_lake)
  all_timeseries <- bind_rows(all_timeseries, this_study_lake)

}

all_timeseries$study_lake <- factor(all_timeseries$study_lake,
                                    levels = c("Pleasant Lake",
                                               "Long Lake",
                                               "Plainfield Lake"))
# Plot observations, BSTS, and Z-score
ggplot(all_timeseries)   +
  geom_line(aes(x = .data$date,
                y = .data$bsts,
                color = "BSTS")) +
  geom_line(aes(x = .data$date,
                y = .data$z_score,
                color = "Z-score"),
            size = 1)  +
  geom_line(aes(x = .data$date,
                y = .data$level_obs,
                color = "Observed"),
            size = 1) +
  geom_point(data = filter(all_timeseries, .data$cal_val == "cal"),
             aes(x = .data$date,
                 y = .data$level_obs,
                 color = "Observed"),
             size = 3.5) +
  scale_color_manual(name = "",
                     breaks = c("Observed", "Z-score", "BSTS"),
                     values = c("black", "blue", "red")) +
  facet_wrap(~study_lake, ncol = 1) +
  labs(y = "Devils Lake Level (m)", x = "") +
  theme_bw() +
  theme(text = element_text(family = "Segoe UI Semilight",
                            size = 18))
