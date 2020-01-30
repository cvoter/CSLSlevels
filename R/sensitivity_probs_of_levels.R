#' Calculate sensitivity of exceedance probabilities to window of anlaysis
#'
#' Given a timeseries of lake levels and a specified series of years, this
#' function ranks lake levels by lake for multiple analysis windows and
#' calculates lake level exceedance probability.
#'
#' @param levels a data frame with the following columns:
#'  * **lake:** the name of the lake ("Pleasant", "Long", or "Plainfield")
#'  * **date:** the month an year of prediction in datetime (POSIXct) format
#'  * **level_pred:** predicted lake level, in meters above mean sea level
#' @param years vector of years to loop through (either start years or end years,
#'              depending on "types").
#' @param types type of looping to do, defaults to "forward". Also "backward".
#' @param windows length of window of analysis. Defaults to zero to indicate use
#'               entire record.
#' @param start_ts start year of lake level data
#' @param end_ts end year of lake level data
#'
#' @return ranked, a data frame with the following columns:
#' \item{lake}{the name of the lake ("Pleasant", "Long", or "Plainfield)}
#' \item{season}{the season of the prediction}
#' \item{date}{the month and year of prediction in datetime (POSIXct) format}
#' \item{level}{the predicted lake level (meters above mean sea level)}
#' \item{prob}{the probability of a lake level that equals or exceeds the
#'             predicted level (percent)}
#' \item{year}{year unique to this window of anlaysis (start year or end year)}
#' \item{sim_no}{id associated with this window of analysis}
#'
#'
#' @export

sensitivity_probs_of_levels <- function(levels,
                                        years,
                                        types = "forward",
                                        windows = 0,
                                        start_ts = 1905,
                                        end_ts = 2018){
  # Replicate if all types or windows the same
  nyears <- length(years)
  if (length(types) == 1){ types <- rep(types[1], nyears) }
  if (length(windows) == 1){ windows <- rep(windows[1], nyears) }

  # Subset and rank lake levels
  ranked <- NULL
  sim    <- 1
  for (i in 1:nyears){
    year      <- years[i]
    type      <- types[i]
    window    <- windows[i]
    window_start <- NULL
    window_end   <- NULL
    this_year    <- NULL
    # Subset and rank this_year based on analysis type
    if (type == "forward" & window == 0){
      window_start <- start_ts
      window_end   <- year
    } else if (type == "backward" & window == 0) {
      window_start <- year
      window_end   <- end_ts
    } else if ((type == "forward") & (year + window - 1 <= end_ts)){
      window_start <- year
      window_end   <- year + window - 1
    } else if ((type == "backward") & (year - window + 1 >= start_ts)){
      window_start <- year - window + 1
      window_end   <- year
    }

    if (!is.null(window_start)) {
      this_year <- calculate_probs_of_levels(levels, window_start, window_end)
    }
    # Add info on window of analysis
    if (length(this_year) > 0) {
      this_year$year     <- year
      this_year$sim_no   <- sim
      sim                <- sim + 1
    }

    # Combine with larger dataset
    ranked <- rbind(ranked, this_year)
  }
  return(ranked)
}
