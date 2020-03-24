#' Dataset: CSLS hydrologic metrics sensitivity analysis summary
#'
#' Data frame with calculated hydrologic metrics for each simulation (x100) for
#' an increasing number of years.
#'
#' @docType data
#'
#' @usage data(summary)
#'
#' @format A data frame with the following columns.
#' \describe{
#'   \item{lake}{name of lake, "Pleasant", "Long", "Plainfield", or "Devils"}
#'   \item{metric}{name of the hydrologic metric}
#'   \item{variable}{variation on the hydrologic metric (e.g., 10% exceedance
#'                   level)}
#'   \item{sim}{value of metric using this simulation}
#'   \item{nsim}{simulation number}
#'   \item{nyear}{number of years randomly sampled (100x)}
#'   \item{obs}{value of metric using entire timeseries}
#' }
"summary"
