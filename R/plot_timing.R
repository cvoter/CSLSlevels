#' Plot monthly timeseries
#'
#' Given a data frame with lake levels for a single lake or multiple lakes, plot
#' the estimated and observed lake levels. If data frame has multiple lakes,
#' will display as facet plots. Options to add yintercepts. Also option to add
#' second axis with lake depth, if only plotting one lake.
#'
#'
#' @param df a data frame with columns for "lake", "date", "level_obs", and
#'           "level_pred".
#' @param title string to use for title of plot, defaults to "".
#' @param text_size size of text, defaults to 12
#'
#' @return plot_obj, a plot with the imputed and observed lake levels.
#'
#' @import lubridate
#' @import ggplot2
#' @import extrafont
#'
#' @export
plot_timing <- function(df,
                        title = "",
                        text_size = 12) {

  df$month <- as.character(month(df$date,label=TRUE,abbr=TRUE))
  df$month <- factor(df$month, levels = unique(df$month))

  # Basic plot w/lines for estimate, points for observations
  plot_obj <- ggplot(data = df) +
              geom_boxplot(aes(x = .data$month,
                               y = .data$level_pred,
                               group = as.factor(.data$month)))

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
    plot_obj <- plot_obj +
                geom_blank(data = range,
                           aes(x = month(.data$date), y = .data$value)) +
                facet_wrap(~lake, scales = "free_y")
  }

  # Add in aesthetics
  plot_obj <- plot_obj +
              labs(x = "",
                   y = "Lake Elevation (m)",
                   color = "",
                   title = title) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    axis.text.x = element_text(angle = 45),
                    plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.background = element_blank())

  return(plot_obj)
}
