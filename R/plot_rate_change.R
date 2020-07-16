#' Plot rate of change
#'
#' Given a data frame with lake levels for a single lake or multiple lakes, plot
#' the typical rate of change over a given time interval. If data frame has
#' multiple lakes, will display as facet plots.
#'
#'
#' @param df a data frame with columns for "lake", "date", "level_obs", and
#'           "level_pred" that matches desired time series to analyze
#' @param lag_months number of months to analyze rise/fall over, defaults to 1
#' @param convert_to_ft defaults to true to converet rates from m/time period to
#'                      ft/time period
#' @param show_zero logical defaults to TRUE to plot vertical line at
#'                    median number of months.
#' @param show_median logical defaults to TRUE to display median rise/fall rates
#' @param show_labels logical defaults to TRUE to label median rise/fall rates (if displayed)
#' @param max_rate default to NULL, otherwise used to set max x limits
#' @param title string to use for title of plot, defaults to "".
#' @param text_size size of text, defaults to 12
#' @param vline_color color of vertical line, defaults to a red ("#c00000")
#' @param hist_color color of histogram bins, defaults to "grey80"
#' @param density_color color of density curve with alpha = 0.2, defaults to a
#'                      red ("#c00000")
#' @param line_size line size, defaults to 1
#' @param lakes vector of lakes to include in plot. Defaults to c("Pleasant",
#'              "Long", "Plainfield")
#'
#' @return plot_obj, a plot with the imputed and observed lake levels.
#'
#' @importFrom raster minValue
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom NISTunits NISTmeterTOft
#' @import ggplot2
#' @import extrafont
#'
#' @export
plot_rate_change <- function(df,
                             lag_months = c(1, 3, 12),
                             convert_to_ft = TRUE,
                             show_zero = TRUE,
                             show_median = TRUE,
                             show_labels = TRUE,
                             max_rate = NULL,
                             title = "",
                             text_size = 12,
                             vline_color = "#c00000",
                             hist_color = "grey80",
                             density_color = "#c00000",
                             line_size = 1,
                             lakes = c("Pleasant", "Long", "Plainfield")) {

  # Filter to desired lakes
  df <- df %>% filter(.data$lake %in% lakes)

  median_rates <- calculate_metrics(df, metrics = c("median_rise_rate",
                                                    "median_fall_rate"))

  if (convert_to_ft) {
    df$level <- NISTmeterTOft(df$level)
    median_rates$value <- round(NISTmeterTOft(median_rates$value), 1)
    xtitle   <- "Rate of Change (ft/time period)"
    bins     <- 0.25
  } else {
    median_rates$value <- round(median_rates$value, 1)
    xtitle   <- "Rate of Change (m/time period)"
    bins     <- 0.1
  }

  rates <- calculate_rates(df, lag_months)
  rates$variable <- as.character(rates$variable)
  rates$variable <- factor(rates$variable,
                           levels = unique(rates$variable),
                           labels = sprintf("%s mo.", unique(rates$variable)))
  median_rates$variable <- as.character(median_rates$variable)
  median_rates$variable <- factor(median_rates$variable,
                                  levels = unique(median_rates$variable),
                                  labels = sprintf("%s mo.", unique(median_rates$variable)))

  # Basic histogram w/lines for estimate, points for observations
  plot_obj <- ggplot(data = rates,
                     aes(x = .data$value)) +
              geom_histogram(aes(y = ..density..),
                             binwidth = bins,
                             colour = NA,
                             fill = hist_color,
                             size = line_size) +
              geom_density(alpha = 0.2,
                           fill = density_color,
                           size = line_size)

  # If more than one lake, use facets
  if (length(unique(df$lake)) > 1 & length(lag_months) == 1) {
    plot_obj <- plot_obj +
                facet_wrap(~lake)
  } else if (length(unique(df$lake)) > 1) {
    plot_obj <- plot_obj +
                facet_grid(variable~lake,
                           switch = "y")
  }

  # Zero rise/fall rate
  if (show_zero) {
    plot_obj <- plot_obj +
                geom_vline(xintercept = 0,
                           color = "black",
                           linetype = "solid",
                           size = 0.5)

  }

  # Median rates
  if (show_median) {
   plot_obj <- plot_obj +
               geom_vline(aes(xintercept = .data$value),
                          median_rates,
                          color = vline_color,
                          linetype = "dashed",
                          size = line_size)
    if (show_labels) {
      plot_obj <- plot_obj +
                  geom_text(data = filter(median_rates, .data$value > 0),
                            aes(x = .data$value,
                                y = 1,
                                label = .data$value),
                            hjust = 0,
                            nudge_x = 0.1,
                            family = "Segoe UI Semibold") +
                  geom_text(data = filter(median_rates, .data$value < 0),
                            aes(x = .data$value,
                                y = 1,
                                label = .data$value),
                            hjust = 1,
                            nudge_x = -0.1,
                            family = "Segoe UI Semibold")
    }
  }

  # Set x axis limits
  if (is.null(max_rate)) {
    max_rate <- max(abs(rates$value))
  }

  # Add in aesthetics
  plot_obj <- plot_obj +
              labs(x = xtitle,
                   y = "Density",
                   title = title) +
              scale_x_continuous(expand = c(0,0),
                                 limits = c(-max_rate, max_rate)) +
              scale_y_continuous(expand = c(0,0)) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.background = element_blank())

  return(plot_obj)
}
