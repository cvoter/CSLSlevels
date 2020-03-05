#' Plot duration above/below exceedance levels
#'
#' Given a data frame with lake levels for a single lake or multiple lakes, plot
#' the duration of . If data frame has multiple lakes,
#' will display as facet plots. Options to add yintercepts. Also option to add
#' second axis with lake depth, if only plotting one lake.
#'
#'
#' @param df a data frame with columns for "lake", "date", "level_obs", and
#'           "level_pred" that matches desired time series to analyze
#' @param lakes name of lakes to analyze
#' @param exceedance exceedance probabilities to analyze, defaults to 0.9
#' @param show_median logical defaults to FALSE. If true, plots vertical line at
#'                    median number of months.
#' @param max_months default to NULL, otherwise used to set max
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
#' @import ggplot2
#' @import extrafont
#'
#' @export
plot_duration <- function(df,
                          lakes,
                          exceedance = 0.9,
                          show_median = FALSE,
                          max_months = NULL,
                          title = "",
                          text_size = 12,
                          vline_color = "#c00000",
                          hist_color = "grey80",
                          density_color = "#c00000",
                          line_size = 1) {

  start_date <- min(df$date)
  end_date   <- max(df$date)

  plot_df <- NULL
  for (lake in lakes) {
    df <- hi_lo_duration(lake,
                         exceeds = exceedance,
                         startDate = start_date,
                         endDate = end_date)
    df$dur_counts$lake <- lake
    plot_df <- rbind(plot_df, df$dur_counts)
  }
  plot_df$lake <- factor(plot_df$lake, levels = lakes)

  # Basic histogram w/lines for estimate, points for observations
  plot_obj <- ggplot(data = plot_df,
                     aes(x = .data$consecutive_months)) +
              geom_histogram(aes(y = ..density..),
                             binwidth = 2,
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
  if (show_median) {
    median   <- median(plot_df$consecutive_months, na.rm = TRUE)
    plot_obj <- plot_obj +
                geom_vline(aes(xintercept = median),
                           as.data.frame(median),
                           color = vline_color,
                           linetype = "dashed",
                           size = line_size)

  }

  if (is.null(max_months)) {
    plot_obj <- plot_obj +
                scale_x_continuous(expand = c(0,0))
  } else {
    plot_obj <- plot_obj +
                scale_x_continuous(expand = c(0,0),
                                   limits = c(0, max_months))
  }

  # Add in aesthetics
  plot_obj <- plot_obj +
              labs(x = "Number of months",
                   y = "Density",
                   title = title) +
              scale_y_continuous(expand = c(0,0))  +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.background = element_blank())

  return(plot_obj)
}
