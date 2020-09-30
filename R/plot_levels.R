#' Plot inputed lake levels and observations
#'
#' Given a data frame with lake levels for a single lake or multiple lakes, plot
#' the estimated and observed lake levels. If data frame has multiple lakes,
#' will display as facet plots. Options to add yintercepts. Also option to add
#' second axis with lake depth, if only plotting one lake.
#'
#'
#' @param df a data frame with columns for "lake", "date", "level_obs", and
#'           "level".
#' @param lakes vector of lakes to include in plot. Defaults to c("Pleasant",
#'              "Long", "Plainfield")
#' @param yintercept either a single value (if only one lake) or a data frame
#'                   with two columns: "yintercept" with the values, and "lake"
#'                   with the name of each lake in df. Default to NULL to not
#'                   add a line.
#' @param hline_color color of horizontal line, if present. Defaults to a red.
#' @param probs exceedance probability (e.g., 10 for 10%) to identify with
#'              a horizontal line
#' @param all_probs logical defaults to FALSE. If true, adds rectangle
#'                  representing 10-90 percentiles with solid line at 50th
#'                  percentile.
#' @param title string to use for title of plot, defaults to "".
#' @param legend_pos position of legend. Defaults to c(0.15, 0.95) but can also
#'                   be "top", "right", etc.
#' @param show_obs_type logical defaults to FALSE to show observations as
#'                      black points. If TRUE, uses different colors and shapes
#'                      to indicate different types of observations. If true,
#'                      suggest setting color_vals to c("grey70", "#1B9E77",
#'                      "#7570B3", "#D95F02").
#' @param text_size size of text, defaults to 12
#' @param color_vals colors to use for line and points, defaults to c("grey70",
#'                   "black") for grey lines, black points
#' @param line_size line size, defaults to 1
#' @param point_size point size, defaults to 3
#' @param npretty_breaks defaults to NULL, set to 3 to limit Long, Plainfield,
#'                       and Pleasant to integers (or play around, may be
#'                       different with different periods of record)
#' @param force_range defaults to TRUE to force y-limits of all facets to have
#'                    the same range.
#' @param force_pfl defaults to TRUE to force the y-limits of plainfield lake to
#'                  the same as long lake.
#' @param convert_units logical defaults to "". If "meterTOft", converts values
#'                      from meters to feet.
#' @param grid_off defaults to TRUE to turn off gridlines.
#' @param show_uncertainty defaults to FALSE. If TRUE, shows dark grey ribbon
#'   representing mean +/- one standard deviation and a light grey ribbon
#'   representing mean +/- two standard deviations.
#' @param show_exceeds defaults to NULL. If a value (e.g., 10, 90) creates a
#'                     ribbon that highlights when the lake level corresponding
#'                     to the given exceedance probability is exceeded.
#' @param ncol number of columns. Defaults to 1 (stacks plots vertically), but can be set to 3 to show all CSLS lakes horizontally
#'
#' @return plot_obj, a plot with the imputed and observed lake levels.
#'
#' @importFrom raster minValue
#' @importFrom reshape2 melt dcast
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom scales pretty_breaks
#' @import dplyr
#' @import ggplot2
#' @import extrafont
#'
#' @export
plot_levels <- function(df,
                        lakes = c("Pleasant", "Long", "Plainfield"),
                        yintercept = NULL,
                        probs = NULL,
                        all_probs = FALSE,
                        title = "",
                        legend_pos = c(0.1, 0.95),
                        show_obs_type = FALSE,
                        text_size = 12,
                        color_vals = c("grey70", "black"),
                        hline_color = "#c00000",
                        line_size = 1,
                        point_size = 3,
                        npretty_breaks = NULL,
                        force_range = TRUE,
                        force_pfl = TRUE,
                        convert_units = "",
                        grid_off = TRUE,
                        show_uncertainty = FALSE,
                        show_exceeds = NULL,
                        ncol = 1) {
  # Filter to desired lakes
  df <- df %>% filter(.data$lake %in% lakes)

  # Decide if elevation is reported in meters or feet
  if (convert_units == "meterTOft") {
    df <- df %>%
          mutate_at(c("level", "level_obs", "swims_obs", "usgs_obs",
                      "airphoto_obs", "mean", "sd", "p90", "p10","min",
                      "max", "upper", "lower"),
                    NISTmeterTOft)
    ytitle <- "Lake Elevation (ft)"
  } else {
    ytitle <- "Lake Elevation (m)"
  }

  # Determine major/minor date breaks
  date_min <- sprintf("%d-02-01", floor(year(min(df$date))/10)*10)
  date_max <- sprintf("%d-02-01", ceiling(year(max(df$date))/10)*10)
  if (max(year(df$date)) - min(year(df$date)) > 40) {
    breaks <- seq(as_datetime(date_min), as_datetime(date_max), "20 years")
    minor_breaks <- seq(as_datetime(date_min), as_datetime(date_max), "5 years")
  } else {
    breaks <- seq(as_datetime(date_min), as_datetime(date_max), "10 years")
    minor_breaks <- seq(as_datetime(date_min), as_datetime(date_max), "2 years")
  }
  date_limits  <- c(as_datetime(date_min), as_datetime(date_max) - months(1))

  # Add uncertainty ribbons if using
  if (show_uncertainty) {
    plot_obj <- ggplot(data = df) +
                geom_ribbon(aes(x = .data$date,
                                ymin = .data$lower,
                                ymax = .data$upper),
                            fill = "grey90") +
                geom_ribbon(aes(x = .data$date,
                                ymin = .data$mean - .data$sd,
                                ymax = .data$mean + .data$sd),
                            fill = "grey70")
  } else {
    plot_obj <- ggplot(data = df)
  }

  # Add exceedence ribbons, if using
  if (!is.null(show_exceeds)) {
    probs     <- show_exceeds
    exceeds   <- calculate_exceedances(df, probs)
    line_df   <- exceeds %>%
                 select(.data$lake, .data$value)
    if (probs < 50) {
      ribbon_df <- merge(df, line_df, by = "lake") %>%
                   group_by(.data$lake, .data$date) %>%
                   summarise(ymin = min(.data$level, .data$value),
                             ymax = .data$level) %>%
                   ungroup()
    } else {
      ribbon_df <- merge(df, line_df, by = "lake") %>%
                   group_by(.data$lake, .data$date) %>%
                   summarise(ymax = max(.data$level, .data$value),
                             ymin = .data$level) %>%
                   ungroup()
    }
    plot_obj  <- plot_obj +
                 geom_ribbon(data = ribbon_df,
                             aes(x = .data$date,
                                 ymin = .data$ymin,
                                 ymax = .data$ymax),
                             alpha = 0.25,
                             fill = hline_color,
                             color = NA)
  }

  # Basic plot w/lines for estimate, points for observations
  if (show_obs_type) {
    plot_obj <- plot_obj +
                geom_line(aes(x = .data$date,
                              y = .data$level,
                              color = "Estimated"),
                          size = line_size)  +
                geom_point(aes(x = .data$date,
                               y = .data$usgs_obs,
                               color = "USGS Obs."),
                           size = point_size,
                           shape = 16)  +
                geom_point(aes(x = .data$date,
                               y = .data$swims_obs,
                               color = "County Obs."),
                           shape = 17,
                           size = point_size)+
                geom_point(aes(x = .data$date,
                               y = .data$airphoto_obs,
                               color = "Air Photo Obs."),
                           shape = 15,
                           size = point_size)
    break_vals    <- c("Estimated", "USGS Obs.", "County Obs.", "Air Photo Obs.")
    linetype_vals <- c("solid", "blank", "blank", "blank")
    shape_vals    <- c(NA, 16, 17, 15)
  } else {
    plot_obj <- plot_obj +
                geom_line(aes(x = .data$date,
                              y = .data$level,
                              color = "Estimated"),
                          size = line_size) +
                geom_point(aes(x = .data$date,
                               y = .data$level_obs,
                               color = "Observation"),
                           size = point_size)
    break_vals    <- c("Estimated", "Observation")
    linetype_vals <- c("solid", "blank")
    shape_vals    <- c(NA, 16)
  }

  # If more than one lake, use facets
  if (length(unique(df$lake)) > 1) {
    if (force_range) {
      range <- specify_range(df,
                             group_col = "lake",
                             value_col = "level",
                             tick_precision = 0.5,
                             pfl_is_long = force_pfl)
      range$date <- median(df$date)

      plot_obj <- plot_obj +
                  geom_blank(data = range,
                             aes(x = .data$date, y = .data$level)) +
                  facet_wrap(~lake, scales = "free_y", ncol = ncol)
    } else {
      plot_obj <- plot_obj +
                  facet_wrap(~lake, scales = "free_y", ncol = ncol)
    }
  }

  # If want to highlight a particular exceedance probability, calculate that here
  if (!is.null(probs)) {
    df2 <- df
    colnames(df2)[which(colnames(df2) == "level")] <- "level"
    yintercept           <- calculate_exceedances(df2, probs)
    colnames(yintercept) <- c("lake", "variable", "yintercept")
    yintercept$lake      <- factor(yintercept$lake, levels = levels(df$lake))
  }

  # Add y-intercept, either specified in arguments or calculated from probs
  if (!is.null(yintercept)) {
    plot_obj <- plot_obj +
                geom_hline(aes(yintercept = yintercept),
                           as.data.frame(yintercept),
                           color = hline_color,
                           linetype = "solid",
                           size = line_size)
  }

  if (all_probs){
    df2     <- df
    # colnames(df2)[which(colnames(df2) == "level")] <- "level"
    exceeds <- calculate_exceedances(df2, c(10, 50, 90))
    rect_df <- dcast(filter(exceeds, variable != "50"),
                     lake~variable,
                     value.var = "value")
    line_df <- filter(exceeds, variable == "50") %>%
               select(.data$lake, .data$value)
    colnames(rect_df) <- c("lake", "ymax", "ymin")
    plot_obj <- plot_obj +
                geom_rect(data = rect_df,
                          mapping = aes(xmin = as_datetime(date_limits[1]),
                                        xmax = as_datetime(date_limits[2]),
                                        ymin = .data$ymin,
                                        ymax = .data$ymax),
                          alpha = 0.25,
                          fill = hline_color,
                          color = NA) +
                 geom_hline(data = line_df,
                            aes(yintercept = .data$value),
                            color = hline_color,
                            linetype = "solid",
                            size = line_size)
  }

  # Specify number of breaks in y axis
  if (!is.null(npretty_breaks)) {
    plot_obj <- plot_obj + scale_y_continuous(breaks = pretty_breaks(npretty_breaks))
  }

  # Add in aesthetics
  plot_obj <- plot_obj +
              labs(x = "",
                   y = ytitle,
                   color = "",
                   title = title) +
              scale_color_manual(breaks = break_vals,
                                 values = color_vals,
                                 guide = guide_legend(override.aes = list(
                                   linetype = linetype_vals,
                                   shape = shape_vals))) +
              scale_x_datetime(breaks = breaks,
                               minor_breaks = minor_breaks,
                               date_labels = "%Y",
                               expand = c(0,0),
                               limits = date_limits) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    legend.position = legend_pos,
                    legend.background = element_blank())  +
               scale_fill_manual(name = "",
                                 labels = "10%-90%")

  # Remove gridlines if not desired
  if (grid_off) {
    plot_obj <- plot_obj +
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
  }

  return(plot_obj)
}
