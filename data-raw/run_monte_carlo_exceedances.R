# Monte Carlo Sampling of Years for Lake Level Exceedance Curves

# Setup Environment
library(CSLSlevels)
library(devtools)

csls_levels <- CSLSlevels::csls_levels

ts_start    <- 1905
nreplicates <- 50
probs       <- c(10, 50, 90)

# Run Monte Carlo Simulation
monte_carlo_2018 <- monte_carlo_exceedances(csls_levels,
                                            ts_start,
                                            ts_end = 2018,
                                            nreplicates,
                                            probs)
monte_carlo_2015 <- monte_carlo_exceedances(csls_levels,
                                            ts_start,
                                            ts_end = 2015,
                                            nreplicates,
                                            probs)
# Save Data
use_data(monte_carlo_2018, monte_carlo_2015, overwrite = TRUE, compress = 'xz')
