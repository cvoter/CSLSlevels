#' Plot lake levels under varying analysis windows
#'
#' This function plots the lake level at three predetermined exceedance
#' probabilities (e.g., 10, 50, 90 percent) for a range of analysis end years at
#' all lakes.
#'
#' @param df data frame to plot with the end year of analysis (year), lake level
#'           (level), exceedance probability (prob) and lake name (lake)
#' @param xintercept optional year to highlight with a dashed line
#' @param text_size size of text in plots
#' @param lakes the names of the lakes in the order you would like them
#'              displayed in plots, etc. Defaults to c("Pleasant", "Long",
#'              "Plainfield")
#'
#' @return plot_obj, the plot.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom rlang .data
#'
#' @export

plot_analysis_window <- function(df, xintercept = NA, text_size,
                                 lakes = c("Pleasant", "Long", "Plainfield")) {
  df$lake  <- factor(df$lake, levels = lakes)
  plot_obj <- ggplot(data = df) +
              geom_line(aes(x = .data$year,
                            y = .data$level,
                            color = .data$prob),
                        size = 1) +
              facet_wrap(~lake, scales = "free_y") +
              scale_color_manual(values = c("#4575B4", "#FDAE61", "#D73027")) +
              labs(x = "",
                   y = "Lake Level",
                   color = "Exceedance Probability") +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = "top")
  if (!is.na(xintercept)) {
    plot_obj <- plot_obj  +
                geom_vline(xintercept = xintercept,
                           linetype = "dashed")
  }
  return(plot_obj)

}
