#' Monte Carlo 2018 results
#'
#' Lake levels at specific exceedance probabilities from different years of
#' analysis.
#'
#' @docType data
#'
#' @usage data(monte_carlo_2018)
#'
#' @format A data frame with the 10, 50, and 90 percent exceedance probability
#'   lake level under different random samples of years for CSLS lakes.
#' \describe{
#'   \item{year}{number of years randomly sampled}
#'   \item{type}{"random"}
#'   \item{window}{number of monte carlo run}
#'   \item{prob}{exceedance probability}
#'   \item{lake}{Lake name, "Long Lake", "Plainfield Lake", or "Pleasant Lake"}
#'   \item{level}{Lake level (mamsl)}
#' }
"monte_carlo_2018"
