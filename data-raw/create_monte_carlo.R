# Monte Carlo Sampling of Years for Lake Level Exceedance Curves

# Setup Environment
library(CSLSlevels)
library(lubridate)
library(dplyr)
library(devtools)

csls_levels <- CSLSlevels::csls_levels
nreplicates <- 100

# Dates good for CSLS lakes
csls_start   <- as_datetime(mdy("01-01-1905"))
csls_end     <- as_datetime(mdy("12-01-2018"))
csls_dates   <- csls_levels %>%
                filter(.data$date >= csls_start,
                       .data$date <= csls_end) %>%
                select(.data$date) %>%
                unique()
rownames(csls_dates) <- NULL
csls_max    <- year(csls_end) - year(csls_start) + 1

# Dates good for Devil's Lake
devil_start  <- as_datetime(mdy("01-01-1936"))
devil_end    <- as_datetime(mdy("12-01-2001"))
devil_dates  <- csls_levels %>%
                filter(.data$date >= devil_start,
                       .data$date <= devil_end) %>%
                select(.data$date) %>%
                unique()
rownames(devil_dates) <- NULL

monte_carlo <- NULL
for (nyears in 1:csls_max) {
  # years in months
  message(sprintf("nyears: %d", nyears))
  nmonths     <- 12*nyears

  # Last possible date index for csls lakes
  last_csls   <- csls_end %m-% months(nmonths - 1)
  ind_csls    <- which(csls_dates$date == last_csls)

  # Last possible date index for devils lakes
  last_devil <- devil_end %m-% months(nmonths - 1)
  if (last_devil >= devil_start) {
    ind_devil <- which(devil_dates$date == last_devil)
  } else {
    ind_devil <- NULL
  }

  # Loop through all simulations for this length of time
  for (sim in 1:nreplicates){
    first_csls  <- csls_dates$date[round(runif(1, 1, ind_csls))]
    last_csls   <- first_csls %m+% months(nmonths - 1)
    csls_subset <- csls_levels %>%
                   filter(.data$lake %in% c("Pleasant", "Long", "Plainfield"),
                          .data$date >= first_csls,
                          .data$date <= last_csls) %>%
                   mutate(nsim = sim,
                          nyears = nyears) %>%
                   select(lake = .data$lake,
                          date = .data$date,
                          level = .data$level_pred,
                          nsim = .data$nsim,
                          nyears = .data$nyears)
    if (!is.null(ind_devil)) {
      first_devil  <- devil_dates$date[round(runif(1, 1, ind_devil))]
      last_devil   <- first_devil %m+% months(nmonths - 1)
      devil_subset <- csls_levels %>%
                      filter(.data$lake == "Devils",
                             .data$date >= first_devil,
                             .data$date <= last_devil) %>%
                      mutate(nsim = sim,
                             nyears = nyears) %>%
                      select(lake = .data$lake,
                             date = .data$date,
                             level = .data$level_pred,
                             nsim = .data$nsim,
                             nyears = .data$nyears)
    } else {
      devil_subset <- NULL
    }
    monte_carlo <- rbind(monte_carlo, csls_subset, devil_subset)
  }
}
monte_carlo <- as.data.frame(monte_carlo)
use_data(monte_carlo, overwrite = TRUE, compress = 'xz')
