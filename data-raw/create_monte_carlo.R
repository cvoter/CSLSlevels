# Monte Carlo Sampling of Years for Lake Level Exceedance Curves

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

# Run Monte Carlo Simulation
monte_carlo <- NULL
for (sim in 1:nreplicates) {
  message(sprintf("Simulation # %d", sim))
  for (nyears in 1:csls_max) {
    csls_years  <- round(runif(nyears, csls_start, csls_end))
    csls_subset <- csls_levels %>%
                   filter(lake %in% c("Pleasant", "Long", "Plainfield"),
                          year(date) %in% csls_years) %>%
                   mutate(sim = sim,
                          nyears = nyears) %>%
                   select(lake = .data$lake,
                          date = .data$date,
                          level = .data$level_pred,
                          sim = .data$sim,
                          nyears = .data$nyears)
    if (nyears <= devil_max) {
      devil_years  <- round(runif(nyears, devil_start, devil_end))
      devil_subset <- csls_levels %>%
                      filter(lake == "Devils",
                             year(date) %in% devil_years) %>%
                      mutate(sim = sim,
                             nyears = nyears) %>%
                      select(lake = .data$lake,
                             date = .data$date,
                             level = .data$level_obs,
                             sim = .data$sim,
                             nyears = .data$nyears)
    } else {
      devil_subset <- NULL
    }
    monte_carlo <- rbind(monte_carlo, csls_subset, devil_subset)
  }
}
monte_carlo <- as.data.frame(monte_carlo)
use_data(monte_carlo, overwrite = TRUE, compress = 'xz')
