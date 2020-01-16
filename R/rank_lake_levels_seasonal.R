#' Rank lake levels by each season
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
#' @param seasons a list with each season (summer, fall, winter, spring) and the
#'                month numbers associated with each season. Defaults to JJA,
#'                SON, DJF, and MAM.
#'
#' @return ranked, a data frame with the lake name (lake), observation month and
#'         year (obs_mo), lake level (level), and exceedance probability (prob).
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom rlang .data
#' @import lubridate
#'
#' @export

rank_lake_levels_seasonal <- function(levels,
                                      window_start = 1981,
                                      window_end = 2018,
                                      lake_names,
                                      seasons = list(summer = c(6, 7, 8),
                                                     fall = c(9, 10, 11),
                                                     winter = c(12, 1, 2),
                                                     spring = c(3, 4, 5))){
  # Classify by season
  levels$season <- NA
  for (i in 1:nrow(levels)) {
    this_month <- month(levels$obs_mo[i])
    if (this_month %in% seasons$summer) {
      levels$season[i] <- "summer"
    } else if (this_month %in% seasons$spring){
      levels$season[i] <- "spring"
    } else if (this_month %in% seasons$fall){
      levels$season[i] <- "fall"
    } else if (this_month %in% seasons$winter){
      levels$season[i] <- "winter"
    }
  }

  # Rank by lake and season
  ranked <- levels %>%
            filter(year(.data$obs_mo) <= window_end,
                   year(.data$obs_mo) >= window_start) %>%
            group_by(lake = .data$lake_name,
                     season = .data$season) %>%
            mutate(level = .data$lake_lev_pred,
                   rank = dense_rank(desc(.data$lake_lev_pred)),
                   prob = 100*(.data$rank/(n_distinct(.data$rank)+1))) %>%
            ungroup() %>%
            select(.data$lake, .data$season, .data$obs_mo,
                   .data$level, .data$prob)
  ranked$lake <- factor(ranked$lake, levels = lake_names)

  return(ranked)
}
