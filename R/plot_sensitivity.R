#' Plot bias, precision, and accuracy of metrics
#'
#' Creates a facet plot with lakes faceted horizontally and error estimates
#' faceted vertically.
#'
#' @param df data frame with the following columns:
#' \itemize{
#'   \item{lake}{name of lake, e.g., Pleasant, Long, Plainfield, Devils}
#'   \item{metric}{name of hydrologic metrics, e.g. median_level, cv_rise_rate}
#'   \item{variable}{name of variation on metrics, e.g. 10 for 10% exceedance
#'                   level}
#'   \item{nyear}{number of years used to calculate metric}
#'   \item{fit}{type of fit measurement, PBIAS, CV, or RMSE}
#'   \item{value}{value of fit measurement}
#' }
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
#' @param variable_colors vector with hex codes of colors corresponding to
#'                        variable_breaks and variable_lables.
#' @param text_size size of text on plots. Defaults to 12.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @export

plot_sensitivity <- function(df,
                             metric_name,
                             metric_title = "",
                             variable_title = "",
                             variable_breaks = "sort",
                             variable_labels,
                             variable_colors = NULL,
                             text_size = 12) {
  plot_df           <- df %>%
                       filter(.data$metric == metric_name)
  if (variable_breaks == "sort") {
    variable_breaks <- sort(as.numeric(unique(plot_df$variable)))
  }
  if (variable_labels == "breaks") {
    variable_labels <- variable_breaks
  } else if (variable_labels == "percent") {
    variable_labels <- sprintf("%d%%", variable_breaks)
  }
  plot_df$variable  <- factor(plot_df$variable,
                              levels = variable_breaks,
                              labels = variable_labels)

  plot_obj <- ggplot(plot_df) +
              geom_line(aes(x = .data$nyear,
                            y = .data$value,
                            color = .data$variable),
                        size = 1) +
              facet_grid(fit ~ lake,
                         scales = "free_y",
                         switch = "y") +
              labs(x = "Number of years",
                   y = "",
                   title = metric_title) +
              scale_x_continuous(expand = c(0, 0)) +
              scale_y_continuous(expand = c(0, 0)) +
              scale_color_manual(name = variable_title,
                                 values = variable_colors) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    legend.position = "top")
  return(plot_obj)
}
