#' Dataset: CSLS imputed lake levels
#'
#' Monthly historical lake levels from Bob Smail's Bayesian structural time
#' series monte carlo approach. The lake level time series represented by the
#' "level" column is the most representative simulation from the Bayesian model.
#' Mean, standard deviation, etc. capture the stats of all 4000 simulations.
#'
#' @docType data
#'
#' @usage data(hist_levels)
#'
#' @format A data frame with reconstructed historical lake levels for CSLS lakes.
#' \describe{
#'   \item{lake}{"Long", "Plainfield", "Pleasant", or "Devils}
#'   \item{date}{month and year (1st of the month) of lake level
#'                observation/prediction, POSIXct}
#'   \item{level}{Predicted lake level (mamsl)}
#'   \item{mean}{Mean lake level from all monte carlo simulations at this time
#'               step (mamsl)}
#'   \item{sd}{Standard deviation of lake levels from all monte carlo
#'             simulations at this time step (m)}
#'   \item{p90}{90th percentile lake level from all monte carlo simulations at
#'              this time step (m)}
#'   \item{p10}{10th percentile lake level from all monte carlo simulations at
#'              this time step (m)}
#'   \item{min}{Minimum lake level from all monte carlo simulations at this
#'              time step (m)}
#'   \item{max}{Maximum lake level from all monte carlo simulations at this
#'              time step (m)}
#'   \item{upper}{2 standard deviations above mean or maximum lake level from
#'                all monte carlo simulations at this time step, whichever is
#'                lower (mamsl)}
#'   \item{lower}{2 standard deviations below mean or minimum lake level from
#'                all monte carlo simulations at this time step, whichever is
#'                higher (mamsl)}
#'   \item{level_obs}{Mean observed lake level (mamsl or NA)}
#'   \item{usgs_obs}{USGS lake level for this month? Value or NA}
#'   \item{swims_obs}{SWIMS lake level for this month? Value or NA}
#'   \item{airphoto_obs}{Air photo lake level for this month? Value or NA}
#' }
"hist_levels"
