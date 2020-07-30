# Calculates hydrologic metrics for monte carlo and moving windows datasets

# Setup Environment
library(CSLSlevels)
library(lubridate)
library(dplyr)
library(usethis)
library(reshape2)
library(NISTunits)

# Parameters
csls_lakes   <- c("Pleasant", "Long", "Plainfield")
nreplicates  <- 100

# Data
hist_levels <- CSLSlevels::hist_levels
lake_bottom <- data.frame(lake = c(csls_lakes, "Devils"),
                          bottom = c(291.1426, 332.8622, 332.0755,
                                     NISTftTOmeter(964-47)))
hist_levels <- left_join(hist_levels, lake_bottom, by = "lake") %>%
               mutate(depth = .data$level - .data$bottom) %>%
               select(lake = .data$lake,
                      date = .data$date,
                      level = .data$depth)
hist_levels$lake <- factor(hist_levels$lake, levels = c(csls_lakes, "Devils"))

# Time Series Dates
csls_start   <- min(hist_levels$date[hist_levels$lake == "Pleasant"])
csls_end     <- max(hist_levels$date[hist_levels$lake == "Pleasant"])
csls_dates   <- seq(csls_start, csls_end, "1 month")
devils_start <- min(hist_levels$date[hist_levels$lake == "Devils"])
devils_end   <- max(hist_levels$date[hist_levels$lake == "Devils"])
devils_dates <- seq(devils_start, devils_end, "1 month")

max_years    <- year(csls_end) - year(csls_start) + 1

obs_metrics <- calculate_metrics(hist_levels)

# CALCULATIONS (1/2): grab continuous timeseries of increasing length
ts <- list()
for (nyears in 1:max_years) {
  # years in months
  message(sprintf("Sampling nyears: %d", nyears))
  nmonths     <- 12*nyears

  # Last possible date index for csls lakes
  last_csls   <- csls_end %m-% months(nmonths - 1)
  ind_csls    <- which(csls_dates == last_csls)

  # Last possible date index for devils lakes
  # Devils Lake has shorter record than CSLS lakes, need to skip some nyears
  # (i.e., set ind_devil to NULL)
  last_devils <- devils_end %m-% months(nmonths - 1)
  if (last_devils >= devils_start) {
    ind_devils <- which(devils_dates == last_devils)
  } else {
    ind_devils <- NULL
  }

  # Loop through all simulations for this length of time
  for (sim in 1:nreplicates){
    first_csls  <- csls_dates[round(runif(1, 1, ind_csls))]
    last_csls   <- first_csls %m+% months(nmonths - 1)
    csls_subset <- hist_levels %>%
                   filter(.data$lake %in% c("Pleasant", "Long", "Plainfield"),
                          .data$date >= first_csls,
                          .data$date <= last_csls) %>%
                   mutate(nsim = sim,
                          nyears = nyears) %>%
                   select(lake = .data$lake,
                          date = .data$date,
                          level = .data$level,
                          nsim = .data$nsim,
                          nyears = .data$nyears)
    if (!is.null(ind_devils)) {
      first_devils  <- devils_dates[round(runif(1, 1, ind_devils))]
      last_devils   <- first_devils %m+% months(nmonths - 1)
      devil_subset  <- hist_levels %>%
                       filter(.data$lake == "Devils",
                              .data$date >= first_devils,
                              .data$date <= last_devils) %>%
                       mutate(nsim = sim,
                              nyears = nyears) %>%
                       select(lake = .data$lake,
                              date = .data$date,
                              level = .data$level,
                              nsim = .data$nsim,
                              nyears = .data$nyears)
    } else {
      devil_subset <- NULL
    }
    # use nyears and sim to build a huge list from ts[[1]] to ts[[11,400]]
    ts[[(nyears-1)*nreplicates + sim]] <- rbind(csls_subset, devil_subset)
  }
}
time_series <- bind_rows(ts)

# CALCULATIONS (2/2): Calculate metrics ----------------------------------------
metrics <- NULL
for (nyear in 1:max_years) {
  message(sprintf("Calculating metrics for nyears: %d", nyear))
  this_year   <- time_series %>% filter(.data$nyears == nyear)
  year_metrics <- NULL
  for (nsim in 1:max(this_year$nsim)) {
    this_sim             <- this_year %>% filter(.data$nsim == !!nsim)
    sim_metrics          <- calculate_metrics(this_sim)
    sim_metrics$nsim     <- nsim
    year_metrics[[nsim]] <- sim_metrics
  }
  year_metrics         <- bind_rows(year_metrics)
  year_metrics$nyear   <- nyear
  metrics[[nyear]]     <- year_metrics
}
metrics <- bind_rows(metrics)
colnames(metrics)[which(colnames(metrics) == "value")] <- "sim"
colnames(obs_metrics)[which(colnames(obs_metrics) == "value")] <- "obs"
metrics <- merge(metrics,
                 obs_metrics,
                 by = c("lake", "metric", "variable"),
                 all.x = TRUE)

# SUMMARY ----------------------------------------------------------------------
# Summarize PBIAS, CV, and RMSE for each time series length
ts_summary <- metrics %>%
              group_by(.data$lake, .data$metric, .data$variable, .data$nyear) %>%
              summarise(PBIAS = 100*abs(mean((.data$sim - .data$obs)/.data$obs,
                                             na.rm = TRUE)),
                        CV = 100*abs(sd(.data$sim, na.rm = TRUE)/
                                       mean(.data$sim, na.rm = TRUE)),
                        RMSE = sqrt(mean((.data$obs - .data$sim)^2,
                                         na.rm = TRUE))) %>%
              ungroup() %>%
              as.data.frame()
ts_summary <- melt(ts_summary,
                   id.vars = c("lake", "metric", "variable", "nyear"))
colnames(ts_summary) <- c("lake", "metric", "variable", "nyear", "fit", "value")

# Extract all 38-year time series
level_metrics   <- c("median_level", "exceedance_level")
ts_metrics_short <- metrics %>%
                    filter((.data$lake %in% csls_lakes & .data$nyear == 38) |
                             (.data$lake == "Devils" & .data$nyear == 21))
ts_metrics_short <- left_join(ts_metrics_short, lake_bottom) %>%
                    mutate(sim = ifelse(.data$metric %in% level_metrics,
                                        .data$sim + .data$bottom, .data$sim),
                           obs = ifelse(.data$metric %in% level_metrics,
                                        .data$obs + .data$bottom, .data$obs))
ts_metrics_short$bottom <- NULL
ts_metrics_short$lake   <- factor(ts_metrics_short$lake,
                                  levels = c(csls_lakes, "Devils"))

usethis::use_data(ts_summary, ts_metrics_short,
                  overwrite = TRUE, compress = 'xz')
