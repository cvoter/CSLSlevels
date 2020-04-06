# Window Sampling of Years for Hydrologic Metrics

# Setup Environment
library(CSLSlevels)
library(lubridate)
library(dplyr)
library(devtools)

csls_levels <- CSLSlevels::csls_levels

csls_start  <- 1905
csls_end    <- 2018
devil_start <- 1936
devil_end   <- 2001
nreplicates <- 100
csls_max    <- csls_end - csls_start + 1
devil_max   <- devil_end - devil_start + 1

csls_years  <- csls_start:csls_end
csls_df     <- csls_levels %>%
               filter(lake %in% c("Pleasant", "Long", "Plainfield"),
                      year(date) %in% csls_years)
devil_years <- devil_start:devil_end
devil_df    <- csls_levels %>%
               filter(lake %in% c("Devils"),
                       year(date) %in% devil_years)

# Run Monte Carlo Simulation
moving_windows <- NULL
for (nyears in 1:csls_max) {
  message(sprintf("nyears: %d", nyears))
  for (nsim in 1:(csls_max - nyears + 1)) {
    these_years <- csls_years[(nsim:(nsim + nyears - 1))]
    csls_subset <- csls_levels %>%
                   filter(lake %in% c("Pleasant", "Long", "Plainfield"),
                          year(.data$date) %in% these_years) %>%
                   mutate(nsim = nsim,
                          nyears = nyears) %>%
                   select(lake = .data$lake,
                          date = .data$date,
                          level = .data$level_pred,
                          nsim = .data$nsim,
                          nyears = .data$nyears)
    moving_windows <- rbind(moving_windows, csls_subset)
  }
  if (nyears <= devil_max) {
    for (nsim in 1:(devil_max - nyears + 1)) {
      these_years  <- devil_years[(nsim:(nsim + nyears - 1))]
      devil_subset <- csls_levels %>%
                      filter(lake == "Devils",
                             year(date) %in% these_years) %>%
                      mutate(nsim = nsim,
                             nyears = nyears) %>%
                      select(lake = .data$lake,
                             date = .data$date,
                             level = .data$level_pred,
                             nsim = .data$nsim,
                             nyears = .data$nyears)
      moving_windows <- rbind(moving_windows, devil_subset)
    }
  }
}
moving_windows <- as.data.frame(moving_windows)
use_data(moving_windows, overwrite = TRUE, compress = 'xz')
