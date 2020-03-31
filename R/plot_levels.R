#' Plot inputed lake levels and observations
#'
#' Given a data frame with lake levels for a single lake or multiple lakes, plot
#' the estimated and observed lake levels. If data frame has multiple lakes,
#' will display as facet plots. Options to add yintercepts. Also option to add
#' second axis with lake depth, if only plotting one lake.
#'
#'
#' @param df a data frame with columns for "lake", "date", "level_obs", and
#'           "level_pred".
#' @param lakes names of lakes in df in order to display. Defaults to
#'              c("Pleasant", "Long", "Plainfield")
#' @param yintercept either a single value (if only one lake) or a data frame
#'                   with two columns: "yintercept" with the values, and "lake"
#'                   with the name of each lake in df. Default to NULL to not
#'                   add a line.
#' @param hline_color color of horizontal line, if present. Defaults to a red.
#' @param probs exceedance probability (e.g., 10 for 10%) to identify with
#'              a horizontal line
#' @param depth_axis logical defaults to TRUE to add a second axis with lake
#'                   depth if there's only one lake in the df
#' @param title string to use for title of plot, defaults to "".
#' @param legend_pos position of legend. Defaults to c(0.15, 0.95) but can also
#'                   be "top", "right", etc.
#' @param text_size size of text, defaults to 12
#' @param color_vals colors to use for line and points, defaults to c("grey70",
#'                   "black") for grey lines, black points
#' @param line_size line size, defaults to 1
#' @param point_size point size, defaults to 3
#' @param npretty_breaks defaults to NULL, set to 3 to limit Long, Plainfield,
#'   and Pleasant to integers (or play around, may be different with different
#'   periods of record)
#' @param force_pfl defaults to TRUE to force the y-limits of plainfield lake to the same as long lake.
#'
#' @return plot_obj, a plot with the imputed and observed lake levels.
#'
#' @importFrom raster minValue
#' @importFrom reshape2 melt
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom scales pretty_breaks
#' @import dplyr
#' @import ggplot2
#' @import extrafont
#'
#' @export
plot_levels <- function(df,
                        lakes = c("Pleasant", "Long", "Plainfield"),
                        yintercept = NULL,
                        probs = NULL,
                        depth_axis = TRUE,
                        title = "",
                        legend_pos = c(0.1, 0.95),
                        text_size = 12,
                        color_vals = c("grey70", "black"),
                        hline_color = "#c00000",
                        line_size = 1,
                        point_size = 3,
                        npretty_breaks = NULL,
                        force_pfl = TRUE) {

  # Basic plot w/lines for estimate, points for observations
  plot_obj <- ggplot(data = df) +
              geom_line(aes(x = .data$date,
                            y = .data$level_pred,
                            color = "Estimated"),
                        size = line_size) +
              geom_point(aes(x = .data$date,
                             y = .data$level_obs,
                             color = "Observation"),
                         size = point_size)

  # If more than one lake, use facets
  if (length(unique(df$lake)) > 1) {
    range     <- df %>%
                 group_by(.data$lake) %>%
                 summarize(lower = min(.data$level_pred),
                           upper = max(.data$level_pred)) %>%
                 mutate(midpoint = .data$lower + (.data$upper -.data$lower)/2) %>%
                 ungroup() %>%
                 mutate(range = .data$upper - .data$lower)
    max_range <- ceiling(10*max(range$range))/10
    new_range <- range[,1:3]
    new_range$lower <- range$midpoint - max_range/2
    new_range$upper <- range$midpoint + max_range/2
    range <- melt(new_range, id.vars = "lake")
    range$date <- median(df$date)

    if (force_pfl) {
      range$value[which(range$lake == "Plainfield" & range$variable == "upper")] <- range$value[which(range$lake == "Long" & range$variable == "upper")]
      range$value[which(range$lake == "Plainfield" & range$variable == "lower")] <- range$value[which(range$lake == "Long" & range$variable == "lower")]
    }

    plot_obj <- plot_obj +
                geom_blank(data = range,
                           aes(x = .data$date, y = .data$value)) +
                facet_wrap(~lake, scales = "free_y")
  }

  if (!is.null(probs)) {
    df2 <- df
    colnames(df2)[which(colnames(df2) == "level_pred")] <- "level"
    yintercept           <- calculate_exceedances(df2, probs)
    colnames(yintercept) <- c("lake", "variable", "xintercept")
    yintercept$lake      <- factor(yintercept$lake, levels = levels(df$lake))
  }

  if (!is.null(yintercept)) {
    plot_obj <- plot_obj +
                geom_hline(aes(yintercept = yintercept),
                           as.data.frame(yintercept),
                           color = hline_color,
                           linetype = "dashed",
                           size = line_size)
  }

  # If depth axis desired, add in
  if (length(unique(df$lake)) == 1 & depth_axis) {
    lake_raster <- CSLSlevels::lake_raster[[unique(df$lake)]]
    lake_bottom <- minValue(lake_raster)
    lake_top    <- max(df$level_pred)
    lake_range  <- c(lake_bottom, lake_top)
    plot_obj    <- plot_obj +
                   scale_y_continuous(sec.axis = sec_axis(~.-lake_range[1],
                                                       name = "Max Depth (m)"),
                                      limits = lake_range)
  }

  if (!is.null(npretty_breaks)) {
    plot_obj <- plot_obj + scale_y_continuous(breaks = pretty_breaks(npretty_breaks))
  }

  # Add in aesthetics
  plot_obj <- plot_obj +
              labs(x = "",
                   y = "Lake Elevation (m)",
                   color = "",
                   title = title) +
              scale_color_manual(values = color_vals,
                                 guide = guide_legend(override.aes = list(
                                   linetype = c("solid", "blank"),
                                   shape = c(NA, 16)))) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = legend_pos,
                    legend.background = element_blank())

  return(plot_obj)
}
