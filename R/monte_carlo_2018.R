#' Dataset: monte carlo analysis of lake level exceedance probabilities 1905-2018
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
#'   \item{lake}{lake name, "Long Lake", "Plainfield Lake", or "Pleasant Lake"}
#'   \item{level}{lake level (mamsl) associated with exceedance probability for
#'                given lake and season}
#'   \item{prob}{exceedance probability associated with lake level}
#'   \item{season}{season associated with lake level}
#'   \item{sim_no}{id associated with simulation}
#'   \item{year}{number of years randomly sampled}
#' }
"monte_carlo_2018"
