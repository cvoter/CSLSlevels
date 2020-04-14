#' Classify vegetation transition depths
#'
#' Given the name of the lake of interest and shallow/deep limits for vegetation
#' classes, this defines transition depths between classes of vegetation. It is
#' not necessary to provide the shallowest depth of "upland" or the deepest
#' depth for "submergent" if these correspond with the maximum depth of the
#' lake. If desire depths in ft, be sure to set depth_to_meters to FALSE and
#' provice countour_interval in feet. Assumes deepest and shallowest depths are
#' always provided in ft.
#'
#' @param lake name of lake to analyze, e.g., "Long". Must be present in both
#'             CSLSlevels::csls_levels and CSLSdata::lake_levels.
#' @param deepest list with deepest depth each vegetation class can tolerate
#'                (ft). If depth_to_meters is TRUE, these will be converted to
#'                meters. If deepest not provided for "submergent", assumed to
#'                be the max depth of the lake.
#' @param shallowest list with shallowest depth each vegetation class can
#'                   tolerate (ft). If depth_to_meters is TRUE, these will be
#'                   converted to meters. If shallowest not provided for
#'                   "upland", assumed to be the negative max depth of the lake.
#' @param contour_interval interval of contours to use when creating
#'                         relationship, same units as desired transition depths
#'                         data frame. Defaults to 0.1.
#' @param depth_to_meters defaults to TRUE, indicating transition depths will be
#'                        in meters and deepest and shallowest should be
#'                        converted from feet to meters.
#' .
#'
#' @return transitions, a data frame with the following columns:
#' \item{depth}{depth of transition point (meters)}
#' \item{class}{name of vegetation classes present at/deeper than this depth}
#'
#' @importFrom NISTunits NISTftTOmeter NISTmeterTOft
#' @importFrom stringr str_c str_replace str_to_title
#' @importFrom magrittr %>%
#'
#' @export
classify_veg_transitions <- function(lake,
                                     deepest = data.frame(upland = -1,
                                                          inland_beach = 1.6,
                                                          emergent = 7,
                                                          floating = 10),
                                     shallowest = data.frame(floating = 5,
                                                             submergent = 1.6,
                                                             emergent = -1,
                                                             inland_beach = -1.6),
                                     contour_interval = 0.1,
                                     depth_to_meters = TRUE) {

  # Convert depths to meters, if needed
  if (depth_to_meters) {
    deepest = NISTftTOmeter(deepest)
    shallowest = NISTftTOmeter(shallowest)
  }

  # Fill in shallowest/deepest for upland/submergent, if not present
  elev <- establish_lake_extents(lake, contour_interval)
  if (!depth_to_meters) {
    elev$min <- NISTmeterTOft(elev$min)
    elev$max <- NISTmeterTOft(elev$max)
  }
  if (is.null(shallowest$upland)) {
    shallowest$upland <- elev$min - elev$max - contour_interval
  }
  if (is.null(deepest$submergent)) {
    deepest$submergent <- elev$max - elev$min + contour_interval
  }

  # Establish transitions for vegetation classes
  transitions <- c(as.numeric(deepest), as.numeric(shallowest)) %>%
                 sort() %>%
                 unique() %>%
                 as.data.frame()
  colnames(transitions) <- "depth"

  # Name vegetation classes present at/deeper than this transition
  veg_classes <- unique(c(colnames(deepest), colnames(shallowest)))
  for (i in 1:nrow(transitions)) {
    classes <- NULL
    for (veg in veg_classes) {
      if (transitions$depth[i] >= shallowest[[veg]] &
          transitions$depth[i] < deepest[[veg]]) {
        classes[veg] <- 1
      } else {
        classes[veg] <- 0
      }
    }
    classes <- classes[classes == 1]
    classes <- names(classes)
    transitions$class[i] <- str_c(classes, collapse = "/") %>%
                            str_replace("_", " ") %>%
                            str_to_title()
  }
  transitions <- transitions[which(transitions$class != ""),]
  return(transitions)
}
