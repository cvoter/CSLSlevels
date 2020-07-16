#' Plot distribution of inputed lake levels
#'
#' Given a data frame with lake levels for a single lake or multiple lakes, plot
#' the distribution (density) of estimated lake levels. If data frame has
#' multiple lakes, will display as facet plots. Options to add xintercepts.
#'
#'
#' @param df a data frame with columns for "lake", "date", "level_obs", and
#'           "level_pred".
#' @param lakes vector of lakes to include in plot. Defaults to c("Pleasant",
#'              "Long", "Plainfield")
#' @param convert_to_ft defaults to TRUE to convert lake levels from meters to ft
#' @param xintercept either a single value (if only one lake) or a data frame
#'                   with two columns: "xintercept" with the values, and "lake"
#'                   with the name of each lake in df. Default to NULL to not
#'                   add a line.
#' @param probs exceedance probabilities (e.g., 10 for 10%) to identify with
#'              a horizontal line
#' @param title string to use for title of plot, defaults to "".
#' @param text_size size of text, defaults to 12
#' @param vline_color color of vertical line, defaults to a red ("#c00000")
#' @param hist_color color of histogram bins, defaults to "grey80"
#' @param density_color color of density curve with alpha = 0.2, defaults to a
#'                      red ("#c00000")
#' @param line_size line size, default to 1
#' @param pfl_is_long defaults to TRUE to force the y-limits of plainfield lake
#'                    to the same as long lake.
#' @param labels logical defaults to TRUE to label exceedance probabilities (if displayed)
#'
#' @return plot_obj, a plot with the distribution(s) of estimated lake elevation
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom reshape2 melt
#' @importFrom ggrepel geom_text_repel
#' @importFrom NISTunits NISTmeterTOft
#'
#' @export
plot_frequency <- function(df,
                           convert_to_ft = TRUE,
                           xintercept = NULL,
                           probs = NULL,
                           title = "",
                           text_size = 12,
                           vline_color = "#c00000",
                           hist_color = "grey80",
                           density_color = "#c00000",
                           line_size = 1,
                           pfl_is_long = TRUE,
                           labels = TRUE,
                           lakes = c("Pleasant", "Long", "Plainfield")) {

  df <- df %>% filter(.data$lake %in% lakes)

  # Convert to ft, if desired
  if (convert_to_ft) {
    df$level <- NISTmeterTOft(df$level)
    xtitle   <- "Lake Elevation (ft)"
    bins     <- 0.25
  } else {
    xtitle   <- "Lake Elevation (m)"
    bins     <- 0.1
  }

  # Basic histogram w/lines for estimate, points for observations
  plot_obj <- ggplot(data = df, aes(x = .data$level)) +
              geom_histogram(aes(y = ..density..),
                             binwidth = bins,
                             colour = NA,
                             fill = hist_color,
                             size = line_size) +
              geom_density(alpha = 0.2,
                           fill = density_color,
                           size = line_size)

  # If more than one lake, use facets with same range
  if (length(unique(df$lake)) > 1) {
    range <- specify_range(df,
                           group_col = "lake",
                           value_col = "level",
                           tick_precision = 0.5,
                           pfl_is_long)
    plot_obj <- plot_obj +
                facet_wrap(~lake, scales = "free_x") +
                geom_blank(data = range,
                           aes(x = .data$level, y = 0.1))
  }

  # Plot x intercepts for desired probs
  if (!is.null(probs)) {
    xintercept           <- calculate_exceedances(df, probs)
    colnames(xintercept) <- c("lake", "variable", "xintercept")
    xintercept$lake      <- factor(xintercept$lake, levels = levels(df$lake))
  }

  if (!is.null(xintercept)) {
    plot_obj <- plot_obj +
                geom_vline(aes(xintercept = xintercept),
                           as.data.frame(xintercept),
                           color = vline_color,
                           linetype = "dashed",
                           size = line_size)
    if (!is.null(labels)) {
      plot_obj <- plot_obj +
                  geom_text_repel(data = xintercept,
                                  aes(x = .data$xintercept,
                                      y = 0.9,
                                      label = sprintf("%s%%", .data$variable)),
                                  direction = "y",
                                  box.padding = 0.4,
                                  family = "Segoe UI Semibold")
    }
  }

  # Add in aesthetics
  plot_obj <- plot_obj +
              labs(x = xtitle,
                   y = "Density",
                   title = title) +
              scale_y_continuous(expand = c(0,0),
                                 limits = c(0,1)) +
              scale_x_continuous(expand = c(0,0))+
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.background = element_blank())

  return(plot_obj)
}
