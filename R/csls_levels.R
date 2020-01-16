#' Dataset: CSLS reconstructed lake levels
#'
#' Monthly historical lake levels from Bob Smail's cumulative deviation in
#' precipitation approach.
#'
#' Results from Bob Smail's analysis are loaded in with the
#' "load_reconstructed_lake_level.R" script in the "data-raw" directory.
#'
#' @docType data
#'
#' @usage data(csls_levels)
#'
#' @format A data frame with reconstructed historical lake levels for CSLS lakes.
#' \describe{
#'   \item{lake_name}{"Long Lake", "Plainfield Lake", or "Pleasant Lake"}
#'   \item{obs_mo}{month and year (1st of the month) of lake level
#'                observation/prediction}
#'   \item{ppt_obs}{observed precipitation that month (mm)}
#'   \item{ppt_cdm_z}{z-score of cumulative deviatoin in precipitation for the month}
#'   \item{lake_lev_obs}{Mean observed lake level (mamsl or NA)}
#'   \item{lake_lev_pred}{Predicted lake level (mamsl)}
#'   \item{usgs_obs}{USGS lake level for this month? Value or NA}
#'   \item{swims_obs}{SWIMS lake level for this month? Value or NA}
#'   \item{airphoto_obs}{Air photo lake level for this month? Value or NA}
#' }
"csls_levels"
