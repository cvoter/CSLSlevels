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
#' @param probs exceedance probabilities to analyze, defaults to 90
#' @param show_median logical defaults to FALSE. If true, plots vertical line at
#'                    median number of months.
#' @param show_labels logical defaults to TRUE to label median duration (if displayed)
#' @param max_months default to NULL, otherwise used to set max
#' @param title string to use for title of plot, defaults to "".
#' @param text_size size of text, defaults to 12
#' @param vline_color color of vertical line, defaults to a red ("#c00000")
#' @param hist_color color of histogram bins, defaults to "grey80"
#' @param density_color color of density curve with alpha = 0.2, defaults to a
#'                      red ("#c00000")
#' @param line_size line size, defaults to 1
#' @param bin_size width of bins, defaults to 2 months.
#' @param lakes vector of lakes to include in plot. Defaults to c("Pleasant",
#'              "Long", "Plainfield")
#'
#' @return plot_obj, a plot with the imputed and observed lake levels.
#'
#' @importFrom raster minValue
#' @importFrom rlang .data
#' @import ggplot2
#' @import extrafont
#'
#' @export
plot_duration <- function(df,
                          probs = 90,
                          show_median = TRUE,
                          show_labels = TRUE,
                          max_months = NULL,
                          title = "",
                          text_size = 12,
                          vline_color = "#c00000",
                          hist_color = "grey80",
                          density_color = "#c00000",
                          line_size = 1,
                          bin_size = 1,
                          lakes = c("Pleasant", "Long", "Plainfield")) {

  df <- df %>% filter(.data$lake %in% lakes)

  durations <- calculate_durations(df, probs)
  if (50 %in% probs) {
    durations <- durations %>%
                 filter(.data$variable != "b50") %>%
                 mutate(variable = as.character(.data$variable),
                        variable = ifelse(.data$variable == "a50",
                                          "50", .data$variable))
  }
  durations$variable <- factor(durations$variable,
                               levels = unique(durations$variable),
                               labels = sprintf("%s%%", unique(durations$variable)))
  if (!is.null(max_months)) {
    durations$value[durations$value > max_months] <- max_months
  }

  # Basic histogram w/lines for estimate, points for observations
  plot_obj <- ggplot(data = durations,
                     aes(x = .data$value)) +
              geom_histogram(binwidth = bin_size,
                             colour = NA,
                             fill = hist_color,
                             size = line_size)

  # If more than one lake, use facets
  if (length(unique(df$lake)) > 1 & length(probs) == 1) {
    plot_obj <- plot_obj +
                facet_wrap(~lake)
  } else if (length(unique(df$lake)) > 1) {
    plot_obj <- plot_obj +
                facet_grid(variable~lake,
                           switch = "y")
  }

  # If x-intercept desired, add in
  if (show_median) {
    median   <- durations %>%
                group_by(.data$lake, .data$variable) %>%
                summarise(median = median(.data$value, na.rm = TRUE)) %>%
                ungroup() %>%
                as.data.frame()
    plot_obj <- plot_obj +
                geom_vline(aes(xintercept = median),
                           median,
                           color = vline_color,
                           linetype = "dashed",
                           size = line_size)
    if (show_labels) {
      plot_obj <- plot_obj +
                  geom_text(data = median,
                            aes(x = .data$median,
                                y = 8,
                                label = sprintf("%s mo.", .data$median)),
                            hjust = 0,
                            nudge_x = 1,
                            family = "Segoe UI Semibold")
    }
  }

  if (is.null(max_months)) {
    plot_obj <- plot_obj +
                scale_x_continuous(expand = c(0,0))
  } else {
    plot_obj <- plot_obj +
                scale_x_continuous(expand = c(0,0),
                                   limits = c(0, max_months+bin_size),
                                   breaks = seq(0,max_months,2),
                                   labels = c(as.character(seq(0,max_months-2,2)),
                                              sprintf(">%d", max_months)))
  }

  # Add in aesthetics
  plot_obj <- plot_obj +
              labs(x = "Number of months",
                   y = "Number of times",
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
