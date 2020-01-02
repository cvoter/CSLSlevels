# rank_lake_level_functions.R
# - rank_multiple_end_years
# - rank_lake_levels
# ------------------------------------------------------------------------------
#' Rank lake levels for multiple windows of analysis
#'
#' Given a timeseries of lake levels and a specified series of years, this
#' function ranks lake levels by lake for windows with the specified end years
#' and calculates lake level exceedance probability.
#'
#' @param levels a data frame with the lake name (lake_name), month and year of
#'               prediction (obs_mo), and predicted lake level (lake_lev_pred)
#' @param years vector of analysis end years to stop considering lake level data
#'              (inclusive)
#' @param types type of looping to do, defaults to "forward". Also "backward".
#' @param windows length of window of analysis. Defaults to zero to indicate use
#'               entire record.
#' @param start_year start year of lake level data
#' @param end_year end year of lake level data
#'
#' @return ranked, a data frame with the lake name (lake), observation month and
#'         year (obs_mo), lake level (level), exceedance probability (prob),
#'         and end year of analysis (end_year).
#'
#'
#' @export

rank_multiple_windows <- function(levels, years, types = "forward", windows = 0,
                                  start_year = 1905, end_year = 2018){
  # Replicate if all types or windows the same
  nyears <- length(years)
  if (length(types) == 1){ types <- rep(types[1], nyears) }
  if (length(windows) == 1){ windows <- rep(windows[1], nyears) }

  # Subset and rank lake levels
  ranked <- NULL
  for (i in 1:nyears){
    year   <- years[i]
    type   <- types[i]
    window <- windows[i]
    # Subset and rank this_year based on analysis type
    this_year   <- NULL
    if (type == "forward" & window == 0){
      window_start <- start_year
      window_end   <- year
      this_year    <- rank_lake_levels(levels, window_start, window_end)
    } else if (type == "backward" & window == 0) {
      window_start <- year
      window_end   <- end_year
      this_year    <- rank_lake_levels(levels, window_start, window_end)
    } else if ((type == "forward") & (year + window - 1 <= end_year)){
      window_start <- year
      window_end   <- year + window - 1
      this_year    <- rank_lake_levels(levels, window_start, window_end)
    } else if ((type == "backward") & (year - window + 1 >= start_year)){
      window_start <- year - window + 1
      window_end   <- year
      this_year    <- rank_lake_levels(levels, window_start, window_end)
    }

    # Add info on window of analysis
    if (length(this_year) > 0) {
      this_year$type   <- type
      this_year$year   <- year
      this_year$window <- window
    }

    # Combine with larger dataset
    ranked <- rbind(ranked, this_year)
  }
  return(ranked)
}

# ------------------------------------------------------------------------------
#' Rank lake levels
#'
#' Given a timeseries of lake levels and a specified time period this function
#' ranks lake levels by lake and calculates lake level exceedance probability
#' curve for each lake.
#'
#' @param levels a data frame with the lake name (lake_name), month and year of
#'               prediction (obs_mo), and predicted lake level (lake_lev_pred)
#' @param window_start year to start considering lake level data (inclusive),
#'                     defaults to 1905
#' @param window_end year to stop considering lake level data (inclusive),
#'                   defaults to 2018
#' @param lake_names name of all lakes in order of desired factor levels.
#'
#' @return ranked, a data frame with the lake name (lake), observation month and
#'         year (obs_mo), lake level (level), and exceedance probability (prob).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by filter mutate dense_rank desc n_distinct select
#' @importFrom rlang .data
#' @import lubridate
#'
#' @export

rank_lake_levels <- function(levels, window_start = 1905, window_end = 2018,
                             lake_names){
  ranked <- levels %>%
            group_by(lake = .data$lake_name) %>%
            filter(year(.data$obs_mo) <= window_end,
                   year(.data$obs_mo) >= window_start) %>%
            mutate(level = .data$lake_lev_pred,
                   rank = dense_rank(desc(.data$lake_lev_pred)),
                   prob = 100*(.data$rank/(n_distinct(.data$rank)+1))) %>%
            select(.data$lake, .data$obs_mo, .data$level, .data$prob)
  ranked$lake <- factor(ranked$lake, levels = lake_names)
  return(ranked)
}
