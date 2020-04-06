#' Dataset: hydrologic metrics calculated with moving windows of lake levels
#'
#' Hydrologic metrics calculated using moving windows of increasing size to
#' sample lake levels.
#'
#' @docType data
#'
#' @usage data(moving_windows_metrics)
#'
#' @format A data frame with the following columns.
#' \describe{
#'   \item{lake}{name of lake, e.g., Pleasant, Long, Plainfield, Devils}
#'   \item{metric}{name of hydrologic metrics, e.g. median_level, cv_rise_rate}
#'   \item{variable}{name of variation on metrics, e.g. 10 for 10% exceedance
#'                   level}
#'   \item{sim}{value of hydrologic metric calculated using this moving window}
#'   \item{nsim}{id number for this moving window}
#'   \item{nyear}{number of years used for this moving window}
#'   \item{obs}{value of hydrologic metric calculated using all valid levels in
#'              timeseries}
#' }
"moving_windows_metrics"
