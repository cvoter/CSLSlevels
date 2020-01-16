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

rank_lake_levels_sensitivity <- function(levels,
                                         years,
                                         types = "forward",
                                         windows = 0,
                                         start_year = 1905,
                                         end_year = 2018){
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
