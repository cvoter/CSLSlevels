#' Calculate lake level at given exceedance probability
#'
#' Given a dataset with lake levels and exceedance probabilities, this function
#' interpolates the lake level associated with the provided exceedance
#' probability for each lake and season in the dataset.
#'
#' @param ranked a data frame with the following columns
#' * **lake:** the name of the lake ("Pleasant", "Long", or "Plainfield)
#' * **season:** the season of the prediction
#' * **date:** the month and year of prediction in datetime (POSIXct) format
#' * **level:** the predicted lake level (meters above mean sea level)
#' * **prob:** the probability of a lake level that equals or exceeds the
#'             predicted level (percent)
#'
#' @param probs the exceedance probabilities of interest (percent), defaults to
#'              10, 50, and 90.
#'
#' @return df, a data frame with the following columns:
#' \item{lake}{the name of the lake ("Pleasant", "Long", or "Plainfield)}
#' \item{season}{the season (e.g., annual, spring, summer, winter, or fall)}
#' \item{prob}{the exceedance probability}
#' \item{level}{the lake level associated with the given exceedance probability
#'              for the given lake and season}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom stats approx
#'
#' @export

calculate_levels_at_probs <- function(ranked, probs = c(10, 50, 90)) {
  lakes   <- unique(ranked$lake)
  seasons <- unique(ranked$season)
  df <- NULL
  for (lake in lakes) {
    for (season in seasons) {
      for (prob in probs) {
        this_lake  <- ranked %>%
                      filter(.data$lake == !!lake,
                             .data$season == !!season)
        level      <- approx(this_lake$prob, this_lake$level, prob)$y
        df         <- rbind(df, c(lake, season, prob, level))
      }
    }
  }
  df           <- as.data.frame(df)
  colnames(df) <- c("lake", "season", "prob", "level")
  rownames(df) <- NULL
  df$level     <- as.numeric(as.character(df$level))

  return(df)
}
