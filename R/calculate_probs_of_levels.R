#' Calculate exceedance probabilities given lake level timeseries
#'
#' Given a timeseries of lake levels and a specified time period, this function
#' ranks lake levels by lake and season and calculates the lake level exceedance
#' probabilities.
#'
#' @param levels a data frame with the following columns:
#'  * **lake:** the name of the lake ("Pleasant", "Long", or "Plainfield")
#'  * **date:** the month an year of prediction in datetime (POSIXct) format
#'  * **level_pred:** predicted lake level, in meters above mean sea level
#'
#' @param start_yr year to start considering lake level data (inclusive),
#'                defaults to 1981
#' @param end_yr year to stop considering lake level data (inclusive),
#'               defaults to 2018
#' @param seasons a list with the season names and the month numbers associated
#'                with each season. For example, list(summer = c(6, 7, 8), fall
#'                = c(9, 10, 11), winter = c(12, 1, 2), spring = c(3, 4, 5)).
#'                Defaults to list(annual = seq(1, 12, 1)).
#' @param nyears number of years to randomly sample for observations from
#'               between start_yr and end_yr. Defaults to NULL to use all years
#'               between start_yr and end_yr (inclusive).
#'
#' @return ranked, a data frame with the following columns:
#' \item{lake}{the name of the lake ("Pleasant", "Long", or "Plainfield)}
#' \item{season}{the season of the prediction}
#' \item{date}{the month and year of prediction in datetime (POSIXct) format}
#' \item{level}{the predicted lake level (meters above mean sea level)}
#' \item{prob}{the probability of a lake level that equals or exceeds the
#'             predicted level (percent)}
#'
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats runif
#' @import dplyr
#' @import lubridate
#'
#' @export

calculate_probs_of_levels <- function(levels,
                                      start_yr = 1981,
                                      end_yr = 2018,
                                      seasons = list(annual = seq(1, 12, 1)),
                                      nyears = NULL) {

  # Classify seasons
  levels <- classify_season(levels, seasons)

  # Define years in timeseries to use
  if (is.null(nyears)) {
    years <- start_yr:end_yr
  } else {
    years <- round(runif(nyears, start_yr, end_yr))
  }

  # Rank by lake and season
  ranked <- levels %>%
            filter(year(.data$date) %in% years) %>%
            group_by(lake = .data$lake,
                     season = .data$season) %>%
            mutate(level = .data$level_pred,
                   rank = dense_rank(desc(.data$level_pred)),
                   prob = 100*(.data$rank/(n_distinct(.data$rank)+1))) %>%
            ungroup() %>%
            select(.data$lake,
                   .data$season,
                   .data$date,
                   .data$level,
                   .data$prob)

  return(ranked)
}
