#' Calculate durations
#'
#' Given a data frame with a "lake", "date", and "level" columns as well as a
#' vector of desired exceedance probabilities, calculates consecutive months
#' at/above (for probabilities <= 50%) or at/below (for probabilities > 50%) the
#' exceedance levels.
#'
#' @param df a data frame with a "lake" and a "level" column
#' @param probs a vector with all exceedance probabilities to calculate.
#'              Defaults to c(10, 25, 75, 90).
#' @return durations, a data frame with the following columns:
#' \item{lake}{name of lake}
#' \item{variable}{exceedance probability, e.g., "10", "25", "75", or "90"}
#' \item{value}{one count of number of months levels were consecutively above or
#'              below the given exceedance probability}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr arrange filter
#'
#' @export

calculate_durations <- function(df, probs = c(10, 25, 75, 90)) {
  # Compare levels to exceedance probability levels
  exceeds   <- calculate_exceedances(df, probs, melted = FALSE)
  df        <- merge(df, exceeds)
  df        <- arrange(df, .data$date)

  # Flag for duration and calculate lengths
  durations <- NULL
  for (lake in unique(df$lake)) {
    this_lake <- df %>% filter(.data$lake == !!lake)
    for (prob in probs) {
      prob_name <- sprintf("%d", prob)
      if (prob <= 50) {
        flags <- ifelse(this_lake$level >= this_lake[,prob_name], 1, 0)
      } else {
        flags <- ifelse(this_lake$level <= this_lake[,prob_name], 1, 0)
      }
      duration  <- data.frame(lake = lake,
                              variable = prob_name,
                              value = rle(flags)$lengths[which(rle(flags)$values == 1)])
      durations <- rbind(durations, duration)
    }
  }
  durations$lake <- factor(durations$lake, levels = levels(df$lake))

  return(durations)
}
