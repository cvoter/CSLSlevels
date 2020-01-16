#' Find lake level at given exceedance probability
#'
#' Given a dataset with lake levels and exceedance probabilities, this function
#' interpolates the lake level associated with the provided exceedance
#' probability for each lake in the dataset.
#'
#' @param ranked a data frame with the lake name (lake), lake levels
#'               (level) and exceedance probabilities (prob).
#' @param year the end year of the analysis window used to calculate lake level
#'             exceedance probabilities, defaults to NA (optional).
#'
#' @param type type of looping done, defaults to "forward". Also "backward".
#' @param window length of window of analysis. Defaults to zero to indicate used
#'               entire record.
#' @param prob the exceedance probability of interest (percent), defaults to 50.
#'
#' @return df, a data frame which track the provided end year of the analysis
#'         window (end_year) and desired exceedance probability (prob) as well as
#'         the corresponding lake level (level) for each lake (lake).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom stats approx
#' @importFrom tibble as_data_frame
#'
#' @export

find_lake_level <- function(ranked, year = NA, type = "forward", window = 0,
                            prob = 50) {
  df <- NULL
  for (lake in unique(ranked$lake)) {
    this_lake  <- ranked %>% filter(.data$lake == !!lake)
    level      <- approx(this_lake$prob, this_lake$level, prob)$y
    df         <- rbind(df, c(year, type, window, prob, lake, level))
  }
  df           <- as_data_frame(df)
  colnames(df) <- c("year", "type", "window", "prob", "lake", "level")
  df$year      <- as.numeric(df$year)
  df$window    <- as.numeric(df$window)
  df$prob      <- as.factor(df$prob)
  df$level     <- as.numeric(df$level)
  return(df)
}
