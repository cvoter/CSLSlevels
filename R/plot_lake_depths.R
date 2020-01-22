#' Draw filled depth contours for a given lake
#'
#' Given a single lake level and the name of the lake of interest, this loads
#' the raster associated with the lake and draws filled depth contours.
#' Optionally scales the limits of the fill to the maximum observed depth, or
#' relative to the maximum depth at the given lake level.
#'
#' @param lake name of lake to analyze, e.g., "Long"
#' @param lake_level a single lake elevations (mamsl) to draw depth contors for.
#' @param relative_depth_fill logical defaults to TRUE to scale limits of
#'                            fill color to maximum observed depth.
#' @param legend_position position of legend, defaults to "top", set to "none"
#'                        to remove legend (note: still need to include
#'                        color_name, color_labels, and color_values even if
#'                        leave the legend off the returned plot).
#' @param title_name string to use for plot title
#' @param text_size font size for text in plot
#'
#' @return plot_obj, a plot with the contours of the lake.
#'
#' @importFrom rlang .data
#' @importFrom raster rasterToContour minValue
#' @importFrom sf st_as_sf st_polygonize
#' @import ggplot2
#' @import ggspatial
#' @import extrafont
#'
#' @export
plot_lake_depths <- function(lake,
                             lake_level,
                             relative_depth_fill = TRUE,
                             legend_position = "right",
                             title_name = NULL,
                             text_size = 10) {
  # Get raster and draw contours
  lake_raster    <- CSLSlevels::lake_raster[[lake]]
  raster_summary <- summary(lake_raster)
  min_elev       <- round(minValue(lake_raster), 2)
  lake_levels    <- CSLSdata::lake_levels[[lake]]$level_m
  max_elev       <- round(max(lake_levels, na.rm = TRUE), 2)
  max_depth      <- round(max_elev - min_elev, 2)
  lake_levels    <- seq(lake_level, min_elev, -0.1)
  depths         <- round(lake_levels - min_elev, 2)
  contours       <- rasterToContour(lake_raster, levels = lake_levels)
  max_contours   <- rasterToContour(lake_raster, levels = max_elev)
  contours_sf    <- st_as_sf(contours)
  contours_poly  <- st_polygonize(contours_sf)
  contours_poly  <- contours_poly[order(contours_poly$level, decreasing = TRUE),]

  contours_poly$level <- as.numeric(as.character(contours_poly$level))
  contours_poly$level <- round(contours_poly$level[1] - contours_poly$level + 0.1, 2)

  # Initialize plot with extents set to max observed lake level
  plot_obj    <- ggplot() +
                 layer_spatial(data = max_contours,
                               color = NA,
                               fill = NA)
  # Add filled depths, layer by layer
  for (i in 1:length(contours_poly$level)) {
    plot_obj  <- plot_obj +
                 layer_spatial(data = contours_poly[i,],
                               aes(fill = as.numeric(as.character(.data$level))),
                               color = NA)
  }
  # Determine limits of fill
  if (relative_depth_fill) {
    plot_obj  <- plot_obj + scale_fill_distiller(palette = "Blues",
                                                 direction = 1,
                                                 limits = c(0, max_depth))
  } else {
    plot_obj  <- plot_obj + scale_fill_distiller(palette = "Blues",
                                                 direction = 1)
  }
  # Add other aesthetics
  plot_obj    <- plot_obj +
                 layer_spatial(data = contours_poly[1,],
                               color = "black",
                               fill = NA) +
                 annotation_scale(location = "tl") +
                 labs(title = title_name,
                      fill = "Depth (m)",
                      x = "", y = "") +
                 theme_bw() +
                 theme(text = element_text(family = "Segoe UI Semilight",
                                           size = text_size),
                       panel.grid.major = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       legend.position = legend_position,
                       axis.text.x = element_text(angle = 45, hjust = 1))
  return(plot_obj)
}
