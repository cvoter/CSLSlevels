#' Plot bias, precision, and accuracy of metrics
#'
#' Creates a facet plot with lakes faceted horizontally and error estimates
#' faceted vertically.
#'
#' @param df_short data frame with the median/sd for hydrologic metrics w/a
#'                 short time series
#' @param df_long data frame with the median/sd for hydrologic metrics w/a
#'                 long time series
#' @param df_early data frame with the median/sd for hydrologic metrics w/a
#'                 short but early time series
#' @param metric_name name of metric to display on this plot
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
#' @param point_size size of points, defaults to 0.5,
#' @param text_size size of text on plots. Defaults to 12.
#' @param rotate_xticks defaults to FALSE. If TRUE, rotates x tick lables
#'                      90 degrees
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom NISTunits NISTmeterTOft
#'
#' @export

plot_short_v_long <- function(df_short,
                              df_long,
                              df_early,
                              metric_name,
                              variable_breaks = "sort",
                              variable_labels,
                              value_title = "",
                              convert_units = "",
                              point_size = 0.5,
                              text_size = 10,
                              rotate_xticks = FALSE) {

  # Filter to metric of interest
  df_short <- df_short %>%
              filter(.data$metric == metric_name) %>%
              mutate(ts = "1981-2018")
  df_long  <- df_long %>%
              filter(.data$metric == metric_name)%>%
              mutate(ts = "1938-2019")
  df_early  <- df_early %>%
               filter(.data$metric == metric_name)%>%
               mutate(ts = "1938-1975")
  df       <- rbind(df_short, df_long)
  df       <- rbind(df, df_early)

  # Arrange variable breaks/labels
  if (variable_breaks == "sort") {
    variable_breaks <- sort(as.numeric(unique(df$variable)))
  }
  if (variable_labels == "breaks") {
    variable_labels <- variable_breaks
  } else if (variable_labels == "percent") {
    variable_labels <- sprintf("%d%%", variable_breaks)
  }
  df$variable <- factor(df$variable,
                        levels = variable_breaks,
                        labels = variable_labels)

  # Convert units
  if (convert_units == "meterTOft") {
    df <- df %>% mutate_at(c("median", "sd"), NISTmeterTOft)
  }

  # Specify range of metric
  range <- specify_range(df,
                         group_col = "lake",
                         value_col = "median",
                         tick_precision = 0.5,
                         pfl_is_long = TRUE)
  range$variable <- variable_labels[1]

  # Plot
  plot_obj <- ggplot() +
              geom_pointrange(data = df,
                              aes(x = .data$variable,
                                  y = .data$median,
                                  ymin = .data$median - .data$sd,
                                  ymax = .data$median + .data$sd,
                                  color = ts,
                                  shape = ts),
                              position = position_dodge(0.5),
                              size = point_size) +
              geom_blank(data = range,
                         aes(x = .data$variable,
                             y = .data$median)) +
              labs(x = "",
                   y = value_title,
                   title = "") +
              facet_wrap(~lake, scales = "free_y") +
              scale_color_manual(name = "",
                                 breaks = c("1938-1975", "1938-2019", "1981-2018"),
                                 values = c("grey40", "black", "grey70")) +
              scale_shape_manual(name = "",
                                 breaks = c("1938-1975", "1938-2019", "1981-2018"),
                                 values = c(17, 16, 15)) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    legend.position = "top")
  if (rotate_xticks){
    plot_obj <- plot_obj +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }

  return(plot_obj)
}
