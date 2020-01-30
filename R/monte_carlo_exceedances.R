#' Monte carlo anlysis of effect of window size on exceedance probabilities
#'
#' This function loops thorugh a Monte Carlo analysis of increasing the size of
#' the window of analysis by randomly sampling different years.
#'
#' @param levels a data frame with the lake name (lake_name), month and year of
#'               prediction (obs_mo), and predicted lake level (lake_lev_pred)
#' @param ts_start year to start considering lake level data (inclusive),
#'                 defaults to 1905
#' @param ts_end year to stop considering lake level data (inclusive), defaults
#'               to 2018.
#' @param nreplicates number of monte carlo replicates to perform, defaults to
#'                    500.
#' @param probs probabilities to highlight with dashed vertical line, defaults
#'              to 10, 50, and 90 percent.
#'
#' @return monte_carlo, a data frame with the number of years used (year), note
#'   that these are random windows (type), monte carlo loop number (window),
#'   exceedance probability (prob), lake name (lake), and lake level (level).
#'
#' @export

monte_carlo_exceedances <- function(levels, ts_start = 1905, ts_end = 2018,
                                    nreplicates = 500, probs = c(10, 50, 90)) {

  nloops      <- ts_end - ts_start + 1
  monte_carlo <- NULL

  for (replicate in 1:nreplicates){
    for (nyears in 1:nloops) {
      ranked <- calculate_probs_of_levels(levels, ts_start, ts_end,
                                          nyears = nyears)
      this_sim        <- calculate_levels_at_probs(ranked, probs)
      this_sim$year   <- nyears
      this_sim$sim_no <- replicate
      monte_carlo     <- rbind(monte_carlo, this_sim)
    }
  }

  return(monte_carlo)
}
