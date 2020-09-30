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
#' @param departures logical defaults to FALSE. If TRUE, calculates durations
#'                   1ft above median and 1ft below median
#' @return durations, a data frame with the following columns:
#' \item{lake}{name of lake}
#' \item{variable}{exceedance probability, e.g., "10", "25", "75", or "90"}
#' \item{value}{one count of number of months levels were consecutively above or
#'              below the given exceedance probability}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr arrange filter
#' @importFrom NISTunits NISTftTOmeter
#'
#' @export

calculate_durations <- function(df,
                                probs = c(10, 25, 50, 75, 90),
                                departures = FALSE) {
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

      # If 50%, note time above ("a") and below ("b")
      if (prob == 50) {
        duration1  <- data.frame(lake = lake,
                                 variable = sprintf("a%s", prob_name),
                                 value = rle(flags)$lengths[which(rle(flags)$values == 1)])
        duration2  <- data.frame(lake = lake,
                                 variable = sprintf("b%s", prob_name),
                                 value = rle(flags)$lengths[which(rle(flags)$values == 0)])
        duration   <- rbind(duration1, duration2)
      } else {
        duration  <- data.frame(lake = lake,
                                variable = prob_name,
                                value = rle(flags)$lengths[which(rle(flags)$values == 1)])
      }
      durations <- rbind(durations, duration)
    }

    # Calculate 1ft above/below median, if desired
    if (departures) {
      # 1 ft above median
      flags     <- ifelse(this_lake$level >=
                            this_lake[,"50"] + NISTftTOmeter(1),
                          1, 0)
      if (length(which(rle(flags)$values == 1)) == 0) {
        duration  <- data.frame(lake = lake,
                                variable = "a50_1",
                                value = 0)
      } else {
        duration  <- data.frame(lake = lake,
                                variable = "a50_1",
                                value = rle(flags)$lengths[which(rle(flags)$values == 1)])
      }
      durations <- rbind(durations, duration)

      # 1 ft below median
      flags     <- ifelse(this_lake$level <=
                            this_lake[,"50"]-NISTftTOmeter(1),
                          1, 0)
      if (length(which(rle(flags)$values == 1)) == 0) {
        duration  <- data.frame(lake = lake,
                                variable = "b50_1",
                                value = 0)
      } else {
        duration  <- data.frame(lake = lake,
                                variable = "b50_1",
                                value = rle(flags)$lengths[which(rle(flags)$values == 1)])
      }
      durations <- rbind(durations, duration)
    }
  }

  durations$lake <- factor(durations$lake, levels = levels(df$lake))

  return(durations)
}
