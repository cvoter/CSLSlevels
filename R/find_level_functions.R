# find_level_functions.R
# - find_level

# ------------------------------------------------------------------------------
#' Find lake level at given exceedance probability for multiple windows of analyses
#'
#' Given a dataset with lake levels and exceedance probabilities, this function
#' interpolates the lake level associated with the provided exceedance
#' probability for each lake in the dataset.
#'
#' @param levels a data frame with the lake name (lake_name), month and year of
#'               prediction (obs_mo), and predicted lake level (lake_lev_pred)
#' @param years the end years of the analysis window used to calculate lake level
#'              exceedance probabilities.
#' @param probs the exceedance probabilities of interest (percent), defaults to
#'              c(10, 50, 90)
#' @param types type of looping to do, defaults to "forward". Also "backward".
#' @param windows length of window of analysis. Defaults to zero to indicate use
#'               entire record.
#'
#' @return df, a data frame which track the provided end year of the analysis
#'         window (end_year) and desired exceedance probability (prob) as well as
#'         the corresponding lake level (level) for each lake (lake).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @export

find_multiple_levels <- function(levels, years, probs = c(10, 50, 90),
                                 types = "forward", windows = 0) {

  # Replicate if all types or windows the same
  nyears <- length(years)
  if (length(types) == 1){ types <- rep(types[1], nyears) }
  if (length(windows) == 1){ windows <- rep(windows[1], nyears) }

  ranked <- rank_multiple_windows(levels, years, types, windows)

  df     <- NULL
  for (i in 1:nyears) {
    year   <- years[i]
    type   <- types[i]
    window <- windows[i]
    if (year %in% unique(ranked$year)) {
      this_year <- ranked %>%
                   filter(.data$year == !!year,
                          .data$type == !!type,
                          .data$window == !!window)
      for (prob in probs){
        df <- rbind(df, find_level(this_year, year, type, window, prob))
      }
    }
  }
  return(df)
}

# ------------------------------------------------------------------------------
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

find_level <- function(ranked, year = NA, type = "forward", window = 0,
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

