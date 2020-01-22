#' Dataset: CSLS lake elevation rasters
#'
#' Rasters with elevation data around the CSLS lakes. Rasters clipped to highest
#' recorded lake level plus a 20 meter buffer.
#'
#' TIF files are loaded and save as rasters with the \code{save_lake_rasters.R}
#' script in the \code{data-raw} subdirectory.
#'
#' @docType data
#'
#' @usage data(lake_raster)
#'
#' @format A list with three rasters, one for each lake.
#' \describe{
#'   \item{Pleasant}{elevation raster for Pleasant Lake}
#'   \item{Long}{elevation raster for Long Lake}
#'   \item{Plainfield}{elevation raster for Plainfield Lake}
#' }
"lake_raster"
