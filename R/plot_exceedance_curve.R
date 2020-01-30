#' Plot lake levels exceedance curves
#'
#' This function plots the lake level at three predetermined exceedance
#' probabilities (e.g., 10, 50, 90 percent) for a range of analysis end years at all
#' lakes.
#'
#' @param df data frame to plot with the exceedance probability (prob), lake
#'           level (level), end year of analysis (end_year), and lake name
#'           (lake_name)
#' @param probs probabilities to highlight with dashed vertical line, defaults
#'              to 10, 50, and 90 percent.
#' @param text_size size of text in plots
#'
#' @return plot_obj, the plot.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom rlang .data
#'
#' @export

plot_exceedance_curve <- function(df, probs = c(10, 50, 90), text_size){

plot_obj <- ggplot(df,
                   aes(x = .data$prob,
                       y = .data$level,
                       color = as.factor(.data$sim_no))) +
            facet_wrap(~lake, scales = "free_y") +
            geom_line(size = 1) +
            geom_vline(xintercept = probs, linetype = "dashed")  +
            labs(x = "Exceedance Probability (%)",
                 y = "Lake Level",
                 color = "Window Type") +
            scale_color_brewer(palette = "RdYlBu") +
            theme_bw() +
            theme(text = element_text(family = "Segoe UI Semilight",
                                      size = text_size),
                  plot.title = element_text(hjust = 0.5),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position = "top")
  return(plot_obj)

}
