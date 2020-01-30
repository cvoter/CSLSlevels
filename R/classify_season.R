#' Classify season of observations
#'
#' Given a timeseries of observations and a list of which month falls in which
#' season, classify what season each observation falls into.
#'
#' @param df the input data frame, with a column for "date" that has the
#'           observation datetime in POSIXct.
#' @param seasons a list of the names of each season and the vectors of which
#'                months fall into each season (e.g., list(summer = c(6, 7, 8),
#'                fall = c(9, 10, 11))).
#'
#' @return df, the input data frame with a column added for the season of the
#'         observation.
#'
#' @import lubridate
#'
#' @export

classify_season <- function(df, seasons = list(summer = c(6, 7, 8),
                                               fall = c(9, 10, 11),
                                               winter = c(12, 1, 2),
                                               spring = c(3, 4, 5))) {
  # Length of loops
  nobs      <- nrow(df)
  nseasons  <- length(names(seasons))

  # Assign seasons
  df$season <- NA
  for (i in 1:nobs) {
    this_month <- month(df$date[i])
    for (j in 1:nseasons) {
      if (this_month %in% seasons[[j]]) {
        df$season[i] <- names(seasons)[j]
      }
    }
  }

  # Convert to factor
  df$season <- as.factor(df$season)

  return(df)
}
