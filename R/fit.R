#' Dataset: CSLS hydrologic metrics sensitivity analysis fit
#'
#' Data frame with calculated PBIAS, CV, and RMSE for each hydrologic metric for
#' a range of random years compared to the value of each metric calculated
#' usinge the entire timeseries.
#'
#' @docType data
#'
#' @usage data(fit)
#'
#' @format A data frame with the following columns.
#' \describe{
#'   \item{lake}{name of lake, "Pleasant", "Long", "Plainfield", or "Devils"}
#'   \item{metric}{name of the hydrologic metric}
#'   \item{variable}{variation on the hydrologic metric (e.g., 10% exceedance
#'                   level)}
#'   \item{nyear}{number of years randomly sampled (100x)}
#'   \item{fit}{goodness of fit parameter, PBIAS, CV, or RMSE}
#'   \item{value}{fit value}
#' }
"fit"
