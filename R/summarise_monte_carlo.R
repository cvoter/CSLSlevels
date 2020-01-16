#' Summarize Monte Carlo Analysis of Lake Levels
#'
#' This function summarizes the mean and standard deviation of lake levels
#' resulting from a monte carlo analysis of lake level exceedance probabilities.
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
