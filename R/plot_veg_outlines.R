#' Draw contours for vegetation classes for given lake/elevation
#'
#' Creates plot with areal extents of vegetation classes at given lake level,
#' with the lake level denoted with a black outline.
#'
#' @param lake name of lake to analyze, e.g., "Long". Must be present in both
#'             CSLSlevels::csls_levels and CSLSdata::lake_levels.
#' @param lake_level lake elevation (m) to use for classifying vegetation extents
#' @param deepest list with deepest depth each vegetation class can tolerate
#'                (ft).
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
#' @param color_values string with hex codes to use for vegetation classes, from
#'                     driest to wettest.
#' @param line_size line size for shoreline, defaults to 1.
#' @param legend_pos legend position. Defaults to "top" but can also be "none",
#'                   "right", etc.
#' @param text_size size of legend text, defaults to 12.
#'
#' @return plot_obj, a plot with the areal extents of vegetation classes at the
#'   given lake level, with the lake level denoted with a black outline.
#'
#' @importFrom rlang .data
#' @importFrom raster rasterToContour
#' @importFrom sf st_as_sf st_polygonize
#' @importFrom NISTunits NISTftTOmeter
#' @import ggplot2
#' @import ggspatial
#' @import extrafont
#'
#' @export

plot_veg_outlines <- function(lake,
                              lake_level,
                              deepest = data.frame(upland = -1,
                                                   inland_beach = 1.6,
                                                   emergent = 7,
                                                   floating = 10),
                              shallowest = data.frame(floating = 5,
                                                      submergent = 1.6,
                                                      emergent = -1,
                                                      inland_beach = -1.6),
                              contour_interval = 0.05,
                              depth_to_meters = TRUE,
                              color_values = c("#B15928", "#FDBF6F",
                                               "#B2DF8A", "#33A02C",
                                               "#A6CEE3", "#1F78B4",
                                               "#6A3D9A"),
                              line_size = 1,
                              legend_pos = "top",
                              text_size = 12) {

  # Identify depth transitions between veg classes
  transitions <- classify_veg_transitions(lake, deepest, shallowest,
                                          contour_interval, depth_to_meters)
  transitions <- transitions %>%
                 mutate(elev = lake_level - .data$depth) %>%
                 select(.data$elev, .data$depth, .data$class)
  if (depth_to_meters) {
    deepest = NISTftTOmeter(deepest)
    shallowest = NISTftTOmeter(shallowest)
  }

  # Convert depths to elevations based on given lake level
  elev <- establish_lake_extents(lake, contour_interval)
  transitions$elev[transitions$elev > elev$max] <- elev$max
  transitions$elev[transitions$elev < elev$min] <- NA
  color_labels <- rev(transitions$class[which(!is.na(transitions$elev))])

  # Get lake raster and draw vegetation class contours
  lake_raster   <- CSLSlevels::lake_raster[[lake]]
  contours      <- rasterToContour(lake_raster, levels = transitions$elev)
  contours_sf   <- st_as_sf(contours)
  contours_poly <- st_polygonize(contours_sf)
  contours_poly <- contours_poly[order(contours_poly$level, decreasing = TRUE),]
  outline       <- rasterToContour(lake_raster, levels = elev$max)
  shoreline     <- rasterToContour(lake_raster, levels = lake_level)

  # Create plot
  plot_obj    <- ggplot() +
                 layer_spatial(data = outline,
                               color = NA,
                               fill = NA)
  for (i in 1:length(contours_poly$level)) {
    plot_obj  <- plot_obj +
                 layer_spatial(data = contours_poly[i,],
                               aes(fill = .data$level),
                               color = NA)
  }
  plot_obj <- plot_obj +
              layer_spatial(data = shoreline,
                            color = "black",
                            fill = NA,
                            size = line_size) +
              scale_fill_manual(name = "",
                                values = rev(color_values[1:length(color_labels)]),
                                labels = color_labels) +
              guides(fill = guide_legend(reverse = TRUE)) +
              labs(title = "",
                   fill = "",
                   x = "", y = "") +
              theme_void() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    legend.position = legend_pos)
  return(plot_obj)
}
