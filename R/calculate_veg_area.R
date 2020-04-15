#' Calculate area of each vegetation class corresponding to lake elevations
#'
#' Given the name of the lake of interest and shallow/deep limits for vegetation
#' classes, this calculates habitable area in area units and as a percent. It is
#' not necessary to provide the shallowest depth of "upland" or the deepest
#' depth for "submergent" if these correspond with the maximum depth of the
#' lake. If desire depths in ft, be sure to set depth_to_meters to FALSE and
#' provice countour_interval in feet. Assumes deepest and shallowest depths are
#' always provided in ft.
#'
#' @param lake name of lake to analyze, e.g., "Long"
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
#'                         data frame. Defaults to 0.05.
#' @param depth_to_meters defaults to TRUE, indicating transition depths will be
#'                        in meters and deepest and shallowest should be
#'                        converted from feet to meters.
#'
#' @return a list with the following items:
#' \item{area}{elevations and corresponding areas habitable for each veg class}
#' \item{crs_units}{length units of raster, area units are square of these units}
#' \item{pcnt}{elevations and corresponding percent area habitable for each veg
#'             class}
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom raster rasterToContour crs
#' @importFrom sf st_as_sf st_polygonize st_area
#' @importFrom stringr str_extract str_trim str_replace
#'
#' @export
calculate_veg_area <- function(lake,
                               deepest = data.frame(upland = -1,
                                                    inland_beach = 1.6,
                                                    emergent = 7,
                                                    floating = 10),
                               shallowest = data.frame(floating = 5,
                                                       submergent = 1.6,
                                                       emergent = -1,
                                                       inland_beach = -1.6),
                               contour_interval = 0.05,
                               depth_to_meters = TRUE) {

  # Convert depths to meters, if needed
  if (depth_to_meters) {
    deepest = NISTftTOmeter(deepest)
    shallowest = NISTftTOmeter(shallowest)
  }

  # Contours of lake levels (elevations and areas)
  elev           <- establish_lake_extents(lake, contour_interval)
  this_raster    <- CSLSlevels::lake_raster[[lake]]
  lake_levels    <- seq(elev$min, elev$max, contour_interval)
  contours       <- rasterToContour(this_raster, levels = lake_levels)
  contours_sf    <- st_as_sf(contours)
  contours_poly  <- st_polygonize(contours_sf)
  contours_poly  <- contours_poly[order(contours_poly$level, decreasing = TRUE),]
  areas_poly     <- st_area(contours_poly)

  # Area of land: total area less than or equal to elevation, plus incremental
  # area before next lower contour
  elev_area_veg  <- data.frame(elev = as.numeric(as.character(contours_poly$level)),
                               area = as.numeric(areas_poly))
  elev_area_veg$inc_area <- elev_area_veg$area -
                            c(elev_area_veg$area[2:nrow(elev_area_veg)], 0)

  # Fill in shallowest/deepest for upland/submergent, if not present
  if (is.null(shallowest$upland)) {
    shallowest$upland <- elev$min - elev$max - contour_interval
  }
  if (is.null(deepest$submergent)) {
    deepest$submergent <- elev$max - elev$min + contour_interval
  }

  # Vegetation elevations: lower and upper extent of class
  upper <- data.frame(lake = elev_area_veg$elev)
  lower <- data.frame(lake = elev_area_veg$elev)

  for (veg in colnames(deepest)) {
    upper[,veg] <- upper$lake - shallowest[[veg]]
    lower[,veg] <- lower$lake - deepest[[veg]]
  }
  upper[upper > elev$max] <- elev$max
  upper[upper < elev$min] <- NA
  lower[lower > elev$max] <- elev$max
  lower[lower < elev$min] <- elev$min
  lower[is.na(upper)]     <- NA

  # Vegetation areas: total area and percent cover by class
  crs_units <- as.character(crs(this_raster))
  crs_units <- str_extract(crs_units, "\\+units=.* ") %>%
               str_trim() %>%
               str_replace("\\+units=", "")

  areas <- data.frame(elev = elev_area_veg$elev,
                      tot_area = max(elev_area_veg$area))
  for (veg in colnames(deepest)) {
    for (i in 1:nrow(areas)) {
      area_subset <- elev_area_veg %>%
                     filter(.data$elev >= lower[i,veg],
                            .data$elev <= upper[i,veg])
       areas[i,veg] <- sum(area_subset$inc_area, na.rm = TRUE)
    }
  }
  areas_pcnt <- cbind(areas$elev, 100*areas[,3:ncol(areas)]/areas[,"tot_area"])
  colnames(areas_pcnt) <- c("elev", colnames(areas[,3:ncol(areas)]))

  return(list(area = areas,
              crs_units = crs_units,
              pcnt = areas_pcnt))
}
