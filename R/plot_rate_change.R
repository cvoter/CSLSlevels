#' Plot rate of change
#'
#' Given a data frame with lake levels for a single lake or multiple lakes, plot
#' the typical rate of change over a given time interval. If data frame has
#' multiple lakes, will display as facet plots.
#'
#'
#' @param df a data frame with columns for "lake", "date", "level_obs", and
#'           "level_pred" that matches desired time series to analyze
#' @param lakes name of lakes to analyze
#' @param lag_months number of months to analyze rise/fall over, defaults to 1
#' @param show_zero logical defaults to FALSE. If true, plots vertical line at
#'                    median number of months.
#' @param max_rate default to NULL, otherwise used to set max x limits
#' @param title string to use for title of plot, defaults to "".
#' @param text_size size of text, defaults to 12
#' @param vline_color color of vertical line, defaults to a red ("#c00000")
#' @param hist_color color of histogram bins, defaults to "grey80"
#' @param density_color color of density curve with alpha = 0.2, defaults to a
#'                      red ("#c00000")
#' @param line_size line size, defaults to 1
#'
#' @return plot_obj, a plot with the imputed and observed lake levels.
#'
#' @importFrom raster minValue
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @import ggplot2
#' @import extrafont
#'
#' @export
plot_rate_change <- function(df,
                          lakes = c("Pleasant", "Long", "Plainfield"),
                          lag_months = 1,
                          show_zero = TRUE,
                          max_rate = NULL,
                          title = "",
                          text_size = 12,
                          vline_color = "black",
                          hist_color = "grey80",
                          density_color = "#c00000",
                          line_size = 1) {

  colnames(df)[which(colnames(df) == "level_pred")] <- "level"
  rates <- calculate_rates(df, lag_months)

  # Basic histogram w/lines for estimate, points for observations
  plot_obj <- ggplot(data = rates,
                     aes(x = .data$value)) +
              geom_histogram(aes(y = ..density..),
                             binwidth = 0.1,
                             colour = NA,
                             fill = hist_color,
                             size = line_size) +
              geom_density(alpha = 0.2,
                           fill = density_color,
                           size = line_size)

  # If more than one lake, use facets
  if (length(lakes) > 1) {
    plot_obj <- plot_obj +
                facet_wrap(~lake)
  }

  # If x-intercept desired, add in
  if (show_zero) {
    plot_obj <- plot_obj +
                geom_vline(xintercept = 0,
                           color = vline_color,
                           linetype = "solid",
                           size = 0.5)

  }

  # Set x axis limits
  if (is.null(max_rate)) {
    max_rate <- max(abs(rates$value))
  }

  # Add in aesthetics
  plot_obj <- plot_obj +
              labs(x = "Rate of Change",
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
