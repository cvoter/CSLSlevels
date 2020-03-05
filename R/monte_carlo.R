#' Dataset: monte carlo sampling of lake levels 1905-2018
#'
#' Lake levels associated with an increasing number of randomly sampled years
#' for each lake.
#'
#' @docType data
#'
#' @usage data(monte_carlo)
#'
#' @format A data frame with the 10, 50, and 90 percent exceedance probability
#'   lake level under different random samples of years for CSLS lakes.
#' \describe{
#'   \item{lake}{lake name, "Long", "Plainfield", "Pleasant", or "Devils"}
#'   \item{date}{month and year of observation, POSIXct}
#'   \item{level}{lake level (mamsl) on that date}
#'   \item{sim}{simulation number}
#'   \item{nyears}{number of years sampled}
#' }
"monte_carlo"
