#' Plot bias, precision, and accuracy of metrics
#'
#' Creates a facet plot with lakes faceted horizontally and error estimates
#' faceted vertically.
#'
#' @param df_box data frame with the following columns for a shortened timeseries:
#' \itemize{
#'   \item{lake}{name of lake, e.g., Pleasant, Long, Plainfield, Devils}
#'   \item{metric}{name of hydrologic metrics, e.g. median_level, cv_rise_rate}
#'   \item{variable}{name of variation on metrics, e.g. 10 for 10% exceedance
#'                   level}
#'   \item{value}{value from this iteration}
#'   \item{nsim}{id of this iteration}
#' }
#' @param df_point data frame with the following columns for the entire timeseries:
#' \itemize{
#'   \item{lake}{name of lake, e.g., Pleasant, Long, Plainfield, Devils}
#'   \item{metric}{name of hydrologic metrics, e.g. median_level, cv_rise_rate}
#'   \item{variable}{name of variation on metrics, e.g. 10 for 10% exceedance
#'                   level}
#'   \item{value}{"true" value based on entire timeseries}
#' }
#' @param df_point_short optional data frame defaults to NULL, but if provided
#'                       includes the lake, metric, variable, and value for a
#'                       specific simulation.
#' @param metric_name name of metric to display on this plot
#' @param metric_title string to use for plot title, defaults to "" for no title
#' @param variable_title string to use for color legend, defaults to "" for no
#'                       title
#' @param variable_breaks vector with values to use for variable breaks in color
#'                        legend. Defaults to "sort" to convert unique options
#'                        in "variable" column to numeric values and sort in
#'                        ascending order.
#' @param variable_labels vector with strings to use for variable labels in
#'                        color legend. Defaults to "breaks" to indicate use
#'                        same values as breaks. Can also be "percent" to add a
#'                        percent symbol to values in variable_breaks.
#' @param value_title label for y-axis values
#' @param convert_units logical defulats to "". If "metersTOft", converts values
#'                      from meters to feet.
#' @param point_color color of points, defaults to "#c00000"
#' @param point_size size of points, defaults to 2,
#' @param text_size size of text on plots. Defaults to 12.
#' @param same_range logical defaults to TRUE to indicate all subplots should
#'                   have the same range in y. If FALSE
#' @param ytick_precision indicates precision of rounding, defaults to 0.5. Only
#'                        used if same_range is TRUE.
#' @param rotate_xticks defaults to FALSE. If TRUE, rotates x tick lables
#'                      90 degrees
#' @param pfl_is_long logical defaults to FALSE to indicate Plainfield and Long
#'                    can retain independent ranges. Only used if same_range is
#'                    TRUE.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom NISTunits NISTmeterTOft
#'
#' @export

plot_nyear <- function(df_box,
                             df_point,
                             df_point_short = NULL,
                             metric_name,
                             metric_title = "",
                             variable_title = "",
                             variable_breaks = "sort",
                             variable_labels,
                             value_title = "",
                             convert_units = "",
                             point_color = "#c00000",
                             point_size = 2,
                             text_size = 12,
                             same_range = TRUE,
                             ytick_precision = 0.5,
                             rotate_xticks = FALSE,
                             pfl_is_long = FALSE) {
  df_box   <- df_box %>%
              filter(.data$metric == metric_name)
  df_point <- df_point %>%
              filter(.data$metric == metric_name)
  if (variable_breaks == "sort") {
    variable_breaks <- sort(as.numeric(unique(df_box$variable)))
  }
  if (variable_labels == "breaks") {
    variable_labels <- variable_breaks
  } else if (variable_labels == "percent") {
    variable_labels <- sprintf("%d%%", variable_breaks)
  }
  df_box$variable    <- factor(df_box$variable,
                             levels = variable_breaks,
                             labels = variable_labels)
  df_point$variable  <- factor(df_point$variable,
                               levels = variable_breaks,
                               labels = variable_labels)
  if (!is.null(df_point_short)) {
    df_point_short          <- df_point_short %>%
                               filter(.data$metric == metric_name)
    df_point_short$variable <- factor(df_point_short$variable,
                                   levels = variable_breaks,
                                   labels = variable_labels)
    if (convert_units == "meterTOft") {
      df_point_short$value <- NISTunits::NISTmeterTOft(df_point_short$value)
    }
  }

  if (convert_units == "meterTOft") {
    df_box$value <- NISTmeterTOft(df_box$value)
    df_point$value <- NISTmeterTOft(df_point$value)
  }

  plot_obj <- ggplot() +
              geom_boxplot(data = df_box,
                           aes(x = .data$variable,
                               y = .data$value)) +
              geom_point(data = df_point,
                           aes(x = .data$variable,
                               y = .data$value),
                         color = point_color,
                         size = point_size,
                         shape = 17) +
              labs(x = variable_title,
                   y = value_title,
                   title = metric_title) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
              plot.title = element_text(hjust = 0.5))
  if (rotate_xticks){
    plot_obj <- plot_obj +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }

  if (same_range) {
    range <- specify_range(df_box, "lake", "value", ytick_precision, pfl_is_long)
    plot_obj <- plot_obj +
                facet_wrap(~lake,
                           scales = "free_y") +
                geom_blank(data = range,
                           aes(x = unique(df_box$variable)[1],
                               y = .data$value))
  } else {
    plot_obj <- plot_obj +
                facet_wrap(~lake)
  }

  if (!is.null(df_point_short)) {
    plot_obj <- plot_obj +
                geom_point(data = df_point,
                           aes(x = .data$variable,
                               y = .data$value,
                               shape = "Full Timeseries"),
                           color = point_color,
                           size = point_size) +
                geom_point(data = df_point_short,
                           aes(x = .data$variable,
                               y = .data$value,
                               shape = "1981 Start Year"),
                           color = point_color,
                           size = point_size) +
                scale_shape_manual(name = "",
                                   breaks = c("Full Timeseries",
                                              "1981 Start Year"),
                                   values = c(17, 15)) +
                theme(legend.position = "top")
  }
  return(plot_obj)
}
