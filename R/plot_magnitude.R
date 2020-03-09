#' Plot distribution of inputed lake levels
#'
#' Given a data frame with lake levels for a single lake or multiple lakes, plot
#' the distribution (density) of estimated lake levels. If data frame has
#' multiple lakes, will display as facet plots. Options to add xintercepts.
#'
#'
#' @param df a data frame with columns for "lake", "date", "level_obs", and
#'           "level_pred".
#' @param xintercept either a single value (if only one lake) or a data frame
#'                   with two columns: "xintercept" with the values, and "lake"
#'                   with the name of each lake in df. Default to NULL to not
#'                   add a line.
#' @param exceedance exceedance probability (e.g., 10 for 10%) to identify with
#'                   a horizontal line
#' @param title string to use for title of plot, defaults to "".
#' @param text_size size of text, defaults to 12
#' @param vline_color color of vertical line, defaults to a red ("#c00000")
#' @param hist_color color of histogram bins, defaults to "grey80"
#' @param density_color color of density curve with alpha = 0.2, defaults to a
#'                      red ("#c00000")
#' @param line_size line size, default to 1
#'
#' @return plot_obj, a plot with the distribution(s) of estimated lake elevation
#'
#' @import ggplot2
#' @import extrafont
#'
#' @export
plot_magnitude <- function(df,
                           xintercept = NULL,
                           exceedance = NULL,
                           title = "",
                           text_size = 12,
                           vline_color = "#c00000",
                           hist_color = "grey80",
                           density_color = "#c00000",
                           line_size = 1) {
  # Basic histogram w/lines for estimate, points for observations
  plot_obj <- ggplot(data = df, aes(x = .data$level_pred)) +
              geom_histogram(aes(y = ..density..),
                             binwidth = .1,
                             colour = NA,
                             fill = hist_color,
                             size = line_size) +
              geom_density(alpha = 0.2,
                           fill = density_color,
                           size = line_size)

  # If more than one lake, use facets
  if (length(unique(df$lake)) > 1) {
    plot_obj <- plot_obj +
                facet_wrap(~lake, scales = "free_x")
  }

  # If x-intercept desired, add in
  if (!is.null(xintercept)) {
    plot_obj <- plot_obj +
                geom_vline(aes(xintercept = xintercept),
                           as.data.frame(xintercept),
                           color = vline_color,
                           linetype = "dashed",
                           size = line_size)
  }

  # Add in aesthetics
  plot_obj <- plot_obj +
              labs(x = "Lake Elevation (m)",
                   y = "Density",
                   title = title) +
              scale_y_continuous(expand = c(0,0)) +
              scale_x_continuous(expand = c(0,0),
                                 breaks = seq(round(min(df$level_pred)),
                                              round(max(df$level_pred)),
                                              by = 1))+
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.background = element_blank())

  return(plot_obj)
}
