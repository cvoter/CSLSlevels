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
#' @import dplyr
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
            ungroup() %>%
            select(.data$lake, .data$obs_mo, .data$level, .data$prob)
  ranked$lake <- factor(ranked$lake, levels = lake_names)
  return(ranked)
}
