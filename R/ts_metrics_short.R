#' Dataset: hydrologic metrics calculated with random 38-year samples of lake levels
#'
#' Hydrologic metrics calculated using randomly sampled lake levels.
#'
#' @docType data
#'
#' @usage data(ts_metrics_short)
#'
#' @format A data frame with the following columns.
#' \describe{
#'   \item{lake}{name of lake, e.g., Pleasant, Long, Plainfield, Devils}
#'   \item{metric}{name of hydrologic metrics, e.g. median_level, cv_rise_rate}
#'   \item{variable}{name of variation on metrics, e.g. 10 for 10% exceedance
#'                   level}
#'   \item{sim}{value of hydrologic metric calculated using randomly sampled levels}
#'   \item{nsim}{id number for this set of randomly sampled levels}
#'   \item{nyear}{number of years randomly sampled in this simulation}
#'   \item{obs}{value of hydrologic metric calculated using all valid levels in
#'              timeseries}
#' }
"ts_metrics_short"
