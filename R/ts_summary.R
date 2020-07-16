#' Dataset: Summary of hydrologic metric fit with full timeseries for different timeseries lengths
#'
#' @docType data
#'
#' @usage data(ts_summary)
#'
#' @format A data frame with the following columns.
#' \describe{
#'   \item{lake}{name of lake, e.g., Pleasant, Long, Plainfield, Devils}
#'   \item{metric}{name of hydrologic metrics, e.g. median_level, cv_rise_rate}
#'   \item{variable}{name of variation on metrics, e.g. 10 for 10% exceedance
#'                   level}
#'   \item{nyear}{number of years randomly sampled in this simulation}
#'   \item{fit}{type of fit analyzed, PBIAS, CV, or RMSE}
#'   \item{value}{value of fit}
#' }
"ts_summary"
