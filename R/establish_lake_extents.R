#' Establish lake extents
#'
#' Defines minimum and maxiumum lake elevations based on deepest point of lake,
#' maximum observed level, and desired contour interval.
#'
#' @param lake name of lake to analyze, e.g., "Long". Must be present in both
#'             CSLSlevels::csls_levels and CSLSdata::lake_levels.
#' @param contour_interval interval of contours to use when creating
#'                         relationship, or precision of lake level (m).
#'                         Defaults to 0.01.
#' @param min_deepest minimum value of deepest vegetation classes (m). Defaults
#'                    to zero so does not account for vegetation classes deeper
#'                    than the max elevation of lake.
#'
#' @return elev, a list with the minimum and maxiumum elevation of the lake.
#'
#' @importFrom raster minValue
#'
#' @export
establish_lake_extents <- function(lake,
                                   contour_interval = 0.01,
                                   min_deepest = 0) {

  # Minimum lake level (i.e., bone dry)
  this_raster    <- CSLSlevels::lake_raster[[lake]]
  min_elev       <- round(minValue(this_raster), 2)

  # Maximum lake level (observed or interpolated)
  historic_levels  <- CSLSlevels::csls_levels
  recent_levels    <- CSLSdata::lake_levels[[lake]]$level_m
  max_estimate     <- max(historic_levels$level_pred[which(historic_levels$lake == lake)])
  max_data         <- max(recent_levels, na.rm = TRUE)
  max_elev         <- round(max(max_estimate, max_data), 2)
  max_elev         <- max_elev - min_deepest
  max_elev         <- min_elev +
                      contour_interval*ceiling((max_elev - min_elev)/contour_interval)

  return(list(min = min_elev, max = max_elev))
}
