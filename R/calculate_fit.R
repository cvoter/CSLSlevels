#' Calculate goodness of fit metrics
#'
#' Given an observed value, a list of simulated values, and the desired metric,
#' calculates the goodness of fit.
#'
#' @param obs a single observed value
#' @param sim a vector with all simulated values
#' @param metric a string indicating which goodness-of-fit metric to use.
#'               Options include "PBIAS", "CV", or "RMSE".
#'
#' @return fit, a vector corresponding to "sim" with the goodness of fit metrics
#'              for each observation.
#'
#' @export

calculate_fit <- function(obs, sim, metric) {
  if (metric == "PBIAS") {
    fit <- 100*mean(abs((obs - sim)/obs), na.rm = TRUE)
  } else if (metric == "RMSE") {
    fit <- mean(sqrt((obs - sim)^2), na.rm = TRUE)
  } else if (metric == "CV") {
    fit <- sd(sim, na.rm = TRUE)/mean(sim, na.rm = TRUE)
  }

  return(fit)
}
