#' Dataset: hydrologic metrics calculated from all Bayesian models for 1981-2018
#'
#' Hydrologic metrics calculated using all simulation results from monte carlo
#' Bayesian Structural Time Series process. Metrics summarize 1981-2018 (CSLS
#' lake) or 1981-2001 (Devils Lake).
#'
#' @docType data
#'
#' @usage data(hist_metrics_short)
#'
#' @format A data frame with the following columns.
#' \describe{
#'   \item{lake}{name of lake, e.g., Pleasant, Long, Plainfield, Devils}
#'   \item{metric}{name of hydrologic metrics, e.g. median_level, cv_rise_rate}
#'   \item{variable}{name of variation on metrics, e.g. 10 for 10% exceedance
#'                   level}
#'   \item{value}{value of hydrologic metric}
#'   \item{sim}{id of bayesian simulation used to calculate this metric}
#' }
"hist_metrics_short"
