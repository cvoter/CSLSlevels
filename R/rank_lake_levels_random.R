#' Rank lake levels for a random subset of years
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

rank_lake_levels_random <- function(levels,
                                    nyears,
                                    ts_start = 1905,
                                    ts_end = 2018,
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
