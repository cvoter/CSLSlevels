#' Dataset: moving window sampling of lake levels 1905-2018
#'
#' Lake levels associated with an increasing moving window of years
#' for each lake.
#'
#' @docType data
#'
#' @usage data(moving_windows)
#'
#' @format A data frame with the 10, 50, and 90 percent exceedance probability
#'   lake level under different random samples of years for CSLS lakes.
#' \describe{
#'   \item{lake}{lake name, "Long", "Plainfield", "Pleasant", or "Devils"}
#'   \item{date}{month and year of observation, POSIXct}
#'   \item{level}{lake level (mamsl) on that date}
#'   \item{nsim}{simulation number}
#'   \item{nyears}{number of years sampled}
#' }
"moving_windows"
