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
#' @param start_ts start year of lake level data
#' @param end_ts end year of lake level data
#'
#' @return df, a data frame with the following columns:
#' \item{lake}{the name of the lake ("Pleasant", "Long", or "Plainfield)}
#' \item{season}{the season (e.g., annual, spring, summer, winter, or fall)}
#' \item{prob}{the exceedance probability}
#' \item{level}{the lake level associated with the given exceedance probability
#'              for the given lake, season, and window of analysis}
#' \item{year}{year unique to this window of anlaysis (start year or end year)}
#' \item{sim_no}{id associated with this window of analysis}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @export

sensitivity_levels_at_probs <- function(levels,
                                        years,
                                        probs = c(10, 50, 90),
                                        types = "forward",
                                        windows = 0,
                                        start_ts = 1905,
                                        end_ts = 2018) {
  ranked <- sensitivity_probs_of_levels(levels, years, types, windows,
                                        start_ts, end_ts)
  df <- NULL
  for (i in 1:max(ranked$sim_no)) {
    this_year           <- ranked %>% filter(.data$sim_no == i)
    these_levels        <- calculate_levels_at_probs(this_year, probs)
    these_levels$year   <- this_year$year[1]
    these_levels$sim_no <- i
    df <- rbind(df, these_levels)
  }
  return(df)
}
