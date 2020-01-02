# monte_carlo_functions.R
# - summarise_monte_carlo
# - monte_carlo_exceedances
# - rank_random_levels

# ------------------------------------------------------------------------------
#' Summarize Monte Carlo Analysis of Lake Levels
#'
#' This function summarizes the mean and sd of lake levels resulting from a
#' monte carlo analysis of lake level exceedance probabilities.
#'
#' @param monte_carlo a data frame with the lake name (lake), exceedance
#'                    probabilities (prob), associated lake levels (level), and
#'                    number of random years selected for analysis (year) in
#'                    monte carlo simulations of lake level exceedance curves.
#'
#' @return monte_carlo_summary, a summary of the input data frame with the mean
#'         and standard deviation of lake levels for each year.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise select
#' @importFrom rlang .data
#' @importFrom stats sd
#'
#' @export

summarise_monte_carlo <- function(monte_carlo) {
  monte_carlo_summary <- monte_carlo %>%
                         group_by(lake = .data$lake,
                                  prob = .data$prob,
                                  year = .data$year) %>%
                         summarise(std = sd(.data$level, na.rm = TRUE),
                                   mean = mean(.data$level, na.rm = TRUE)) %>%
                         select(.data$lake,
                                .data$prob,
                                .data$year,
                                .data$mean,
                                .data$std)
  return(monte_carlo_summary)
}

# ------------------------------------------------------------------------------
#' Monte Carlo Analysis of Lake Levels
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
      ranked <- rank_random_levels(levels, nyears, ts_start, ts_end)
      for (prob in probs){
        monte_carlo <- rbind(monte_carlo,
                             find_level(ranked, nyears, "random", replicate, prob))
      }
    }
  }

  return(monte_carlo)
}

# ------------------------------------------------------------------------------
#' Rank Rank Random Subset of Lake Levels
#'
#' Given a timeseries of lake levels and a specified number of years to sample,
#' this function ranks a random subset of lake levels by lake and calculates
#' lake level exceedance probability curve for each lake.
#'
#' @param levels a data frame with the lake name (lake_name), month and year of
#'               prediction (obs_mo), and predicted lake level (lake_lev_pred)
#' @param nyears number of years to sample
#' @param ts_start year to start considering lake level data (inclusive),
#'                 defaults to 1905
#' @param ts_end year to stop considering lake level data (inclusive), defaults
#'               to 2018
#' @param lake_names name of all lakes in order of desired factor levels.
#'
#' @return ranked, a data frame with the lake name (lake), observation month and
#'         year (obs_mo), lake level (level), and exceedance probability (prob).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by filter mutate dense_rank desc n_distinct select
#' @importFrom rlang .data
#' @importFrom stats runif
#' @import lubridate
#'
#' @export

rank_random_levels <- function(levels, nyears, ts_start = 1905, ts_end = 2018,
                               lake_names = c("Pleasant Lake",
                                              "Long Lake",
                                              "Plainfield Lake")){
  years <- round(runif(nyears, ts_start, ts_end))

  ranked <- levels %>%
    group_by(lake = .data$lake_name) %>%
    filter(year(.data$obs_mo) %in% years) %>%
    mutate(level = .data$lake_lev_pred,
           rank = dense_rank(desc(.data$lake_lev_pred)),
           prob = 100*(.data$rank/(n_distinct(.data$rank)+1))) %>%
    select(.data$lake, .data$obs_mo, .data$level, .data$prob)

  ranked$lake <- factor(ranked$lake, levels = lake_names)

  return(ranked)
}
