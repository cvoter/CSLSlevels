#' Plot distribution of inputed lake levels
#'
#' Given a data frame with lake levels for a single lake or multiple lakes, plot
#' the distribution (density) of estimated lake levels. If data frame has
#' multiple lakes, will display as facet plots. Options to add xintercepts.
#'
#'
#' @param df a data frame with columns for "lake", "date", and "level_pred".
#' @param convert_to_ft defaults to TRUE to convert lake levels from meters to ft
#' @param title string to use for title of plot, defaults to "".
#' @param text_size size of text, defaults to 12
#' @param pfl_is_long defaults to TRUE to force the y-limits of plainfield lake to the same as long lake.
#'
#' @return plot_obj, a plot with the distribution(s) of estimated lake elevation
#'
#' @import ggplot2
#' @import extrafont
#' @import lubridate
#' @importFrom reshape2 melt
#'
#' @export
plot_magnitude <- function(df,
                           convert_to_ft = TRUE,
                           title = "",
                           text_size = 12,
                           pfl_is_long = TRUE) {

  colnames(df)[which(colnames(df) == "level_pred")] <- "level"
  df$month <- month(df$date)
  df_all   <- df
  df_all$month <- 0
  df <- rbind(df, df_all)

  df$month <- as.character(df$month)
  df$month <- factor(df$month,
                     levels = as.character(0:12),
                     labels = c("Overall", "Jan", "Feb", "Mar", "Apr", "May",
                                "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

  # Convert to ft, if desired
  if (convert_to_ft) {
    df$level <- NISTmeterTOft(df$level)
    ytitle   <- "Lake Elevation (ft)"
  } else {
    ytitle   <- "Lake Elevation (m)"
  }

  # Basic histogram w/lines for estimate, points for observations
  plot_obj <- ggplot(df) +
              geom_boxplot(aes(x = .data$month, y = .data$level))

  # If more than one lake, use facets
  if (length(unique(df$lake)) > 1) {
    range     <- specify_range(df,
                               group_col = "lake",
                               value_col = "level",
                               tick_precision = 0.5,
                               pfl_is_long)

    plot_obj <- plot_obj +
                facet_wrap(~lake, ncol = 1, scales = "free_y") +
                geom_blank(data = range,
                           aes(x = "Overall", y = .data$level))
  }

  # Add in aesthetics
  plot_obj <- plot_obj +
              labs(x = "",
                   y = ytitle,
                   title = title) +
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
