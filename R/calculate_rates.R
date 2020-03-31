#' Calculate rates of change
#'
#' Given a data frame with a "lake", "date", and "level" columns as well as a
#' vector of desired lag (e.g., 1 month, 3 months, 12 months), calculates the
#' change in lake levels over each time period.
#'
#' @param df a data frame with a "lake" and a "level" column
#' @param months a vector with all lag times (in months) to consider, defaults
#'               to c(1, 3, 12).
#'
#' @return rates, a data frame with the following columns:
#' \item{lake}{name of lake}
#' \item{variable}{number of months lagged (e.g., 1, 3, 12)}
#' \item{value}{change in lake levels over given time period}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @import dplyr
#'
#' @export

calculate_rates <- function(df, months = c(1, 3, 12)) {
  rates <- NULL
  for (m in months) {
    month_name <- sprintf("%d", m)
    rate       <- df %>%
                  group_by(.data$lake) %>%
                  arrange(.data$date) %>%
                  mutate(value = lead(.data$level, m) - .data$level,
                         variable = m) %>%
                  ungroup() %>%
                  as.data.frame() %>%
                  select(.data$lake, .data$variable, .data$value)
    rates <- rbind(rates, rate)
  }

  return(rates)
}
