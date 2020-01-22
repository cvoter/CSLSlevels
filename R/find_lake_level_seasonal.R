#' Find lake level at given exceedance probabilities for each season
#'
#' Given a dataset with lake levels and exceedance probabilities, this function
#' interpolates the lake level associated with the provided exceedance
#' probability for each lake in the dataset.
#'
#' @param ranked a data frame with the lake name (lake), lake levels
#'               (level) and exceedance probabilities (prob) for each season
#'               (season).
#' @param probs the exceedance probabilities of interest (percent), defaults to
#'              10, 50, and 90.
#' @param seasons name of seasons, defaults to "winter", "fall", "spring", and
#'                "summer".
#'
#' @return df, a data frame with the following columns:
#' \item{lake}{name of the lake, e.g., "Long Lake"}
#' \item{season}{name of season}
#' \item{prob}{exceedance probability (percent)}
#' \item{level}{lake level associated with that exceedance probability for the
#'              given lake and season}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom stats approx
#' @importFrom tibble as_data_frame
#'
#' @export

find_lake_level_seasonal <- function(ranked,
                            probs = c(10, 50, 90),
                            seasons = c("winter", "fall", "spring", "summer")) {
  df <- NULL
  for (lake in unique(ranked$lake)) {
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
  df           <- as_data_frame(df)
  colnames(df) <- c("lake", "season", "prob", "level")
  rownames(df) <- NULL

  df           <- df %>%
                  mutate_at(c("season", "prob"), as.factor) %>%
                  mutate_at("level", as.numeric)
  return(df)
}
