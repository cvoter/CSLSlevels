# Calculates hydrologic metrics for monte carlo and moving windows datasets

# Setup Environment
library(CSLSlevels)
library(lubridate)
library(dplyr)
library(devtools)

calculate_summary <- function(df, obs_summary) {
  summary <- NULL
  for (nyear in 1:max(df$nyears)) {
      message(sprintf("nyears: %d", nyear))
    this_year   <- df %>% filter(.data$nyears == nyear)
    sim_summary <- NULL
    for (nsim in 1:max(this_year$nsim)) {
      this_sim          <- this_year %>% filter(.data$nsim == !!nsim)
      this_summary      <- calculate_metrics(this_sim, col_name = "level")
      this_summary$nsim <- nsim
      sim_summary[[(nyear - 1)*max(df$nsim) + nsim]]   <- this_summary
    }
    sim_summary         <- bind_rows(sim_summary)
    sim_summary$nyear   <- nyear
    summary[[nyear]]    <- sim_summary
  }
  summary <- bind_rows(summary)
  colnames(summary)[which(colnames(summary) == "value")] <- "sim"
  colnames(obs_summary)[which(colnames(obs_summary) == "value")] <- "obs"
  summary <- merge(summary,
                   obs_summary,
                   by = c("lake", "metric", "variable"),
                   all.x = TRUE)
  return(summary)
}

# Subset csls_lakes to correct timeframe
csls_lakes  <- c("Pleasant", "Long", "Plainfield")
csls_years  <- 1905:2018
devil_years <- 1936:2001
csls_levels <- CSLSlevels::csls_levels
csls_levels <- csls_levels %>%
               filter((.data$lake %in% csls_lakes & year(.data$date) %in% csls_years) |
                        (.data$lake == "Devils" & year(.data$date) %in% devil_years))
obs_summary <- calculate_metrics(csls_levels)

# Calculate metrics for monte_carlo and moving_windows
monte_carlo            <- CSLSlevels::monte_carlo
monte_carlo_metrics    <- calculate_summary(monte_carlo, obs_summary)
use_data(monte_carlo_metrics, overwrite = TRUE, compress = 'xz')
