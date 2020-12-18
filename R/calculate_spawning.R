#' Calculate good spawning years
#'
#' Given a data frame with a "lake", "level", "year", and "month" columns,
#' calculates whether a year is good (TRUE) or bad (FALSE) for pike spawning.
#'
#' @param df a data frame with a "lake" and a "level" column
#' @param growing_months months associated with growing season. Defaults to
#'                       c(5, 6, 7, 8, 9, 10).
#' @return good_spawning, a data frame with the lakes, years, and whether spring
#'   levels were sufficiently high, summer levels were sufficiently steady, and
#'   overall the year was good for pike spawning.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom reshape2 dcast
#' @importFrom NISTunits NISTinchTOmeter
#' @import dplyr
#'
#' @export

calculate_spawning <- function(df,
                               growing_months = c(4, 5, 6, 7, 8, 9, 10)) {
  max_fall      <- NISTinchTOmeter(-2)
  growing_mean  <- df %>%
                   filter(.data$month %in% growing_months) %>%
                   group_by(.data$lake, .data$year) %>%
                   summarise(mean_level = mean(.data$level),
                             .groups = "drop") %>%
                   mutate(year = .data$year + 1)
  high_spring   <- df %>%
                   filter(.data$month == 4) %>%
                   left_join(growing_mean, by = c("lake", "year")) %>%
                   mutate(high_spring = ifelse(.data$level > .data$mean_level,
                                                TRUE, FALSE)) %>%
                   select(.data$lake, .data$year, .data$high_spring)
  steady_summer <- df %>%
                   group_by(.data$lake, .data$year) %>%
                   summarise(spring = mean(.data$level[.data$month %in% c(3, 4, 5)]),
                             summer = mean(.data$level[.data$month %in% c(6, 7)]),
                             .groups = "drop") %>%
                   mutate(steady_summer = ifelse((.data$summer-.data$spring) >=
                                                   max_fall,
                                                 TRUE, FALSE)) %>%
                   select(.data$lake, .data$year, .data$steady_summer)
  good_spawning <- left_join(high_spring, steady_summer, by = c("lake", "year")) %>%
                   mutate(good_spawning = ifelse(.data$high_spring & .data$steady_summer,
                                                 TRUE, FALSE))

  return(good_spawning)
}
