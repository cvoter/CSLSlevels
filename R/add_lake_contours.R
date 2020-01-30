#' Add contours for a given lake to existing plot object
#'
#' Given a) a single lake level or a vector of lake levels as well as b) the
#' name of the lake of interest, this loads the raster associated with the lake
#' and draws contours at the desired levels on an existing plot object. Note
#' that by default, each contour is color coded. To turn this off, set
#' color_values to the all the same color (e.g., c("black", "black", "black"))
#' and set legend_position to "none".
#'
#' @param plot_obj existing plot object to add outlines to.
#' @param lake name of lake to analyze, e.g., "Long"
#' @param lake_levels a single value or vector of lake elevations (mamsl) to
#'                    draw contors for.
#' @param color_legend_title legend title for colors.
#' @param color_labels a vector corresponding to lake_levels with the name to
#'                     use for each level in the legend. Defaults to
#'                     c(10, 50, 90) for the 10%, 50%, and 90% exceedance
#'                     probabilities.
#' @param color_values a vector corresponding to lake_levels with the color to
#'                     use for each level in the legend. Defaults to
#'                     c("#a80000", "#ffab00", "#004da8") for Tuscan Red
#'                     (#a80000), Electron Gold (#ffab00), and Ultra Blue
#'                     (#004da8). Note that the order of this vector does not
#'                     necessarily correspond with the order of lake_levels or
#'                     color_labels, it may take some trial and error to find
#'                     the right order for the visualization you have in mind.
#' @param line_size size (lineweight) of lake shoreline, defaults to 0.8
#'
#' @return plot_obj, a plot with the contours of the lake.
#'
#' @importFrom rlang .data
#' @importFrom raster rasterToContour
#' @import ggplot2
#' @import ggspatial
#' @import extrafont
#'
#' @export

add_lake_contours <- function(plot_obj,
                              lake,
                              lake_levels,
                              color_legend_title = "Exceedance Probability (%)",
                              color_labels = c(10, 50, 90),
                              color_values = c("#a80000", "#ffab00", "#004da8"),
                              line_size = 0.8) {
  # Get raster and draw contours
  lake_raster <- CSLSlevels::lake_raster[[lake]]
  contours    <- rasterToContour(lake_raster, levels = lake_levels)

  # Plot contours
  plot_obj    <- plot_obj +
                 layer_spatial(data = contours,
                               aes(color = .data$level),
                               fill = NA,
                               size = line_size) +
                 labs(color = color_legend_title) +
                 scale_color_manual(breaks = lake_levels,
                                    labels = color_labels,
                                    values = color_values)
  return(plot_obj)
}
