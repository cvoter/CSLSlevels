#' Fassett's Locoweed populations
#'
#' Plots and saves fassett's locoweed populations

#' @return nothing
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom NISTunits NISTmeterTOft
#' @importFrom reshape2 dcast
#' @import lubridate
#' @import ggplot2
#' @import dplyr

plot_plant_populations <- function() {

fl_pop <- data.frame(year = c(1980, 1981, 1986, 1987, 1988, 1989, 1990, 1994, 1995,
                              1996, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
                              2006, 2007, 2008, 2009, 2010, 2011,
                              2012, 2013, 2014, 2015, 2016, 2017, 2018,
                              2019),
                     pop = c(3000, NA, 500, NA, 900, 300000, NA, 19, 1,
                             NA, 5000, 5000, 10000, 5000, NA, 101000, 200000,
                             202680, 125808, 114563, 127700, 80000, 93186,
                             287870, NA, 136473, 188225, 120871, 7292, 479,
                             0))
fl_pop$date  <- as_datetime(sprintf("%s-06-01", fl_pop$year))

levels <- CSLSlevels::hist_levels %>%
          filter(.data$lake == "Plainfield",
                 year(.data$date) >= 1980,
                 year(.data$date) <= 2020) %>%
          mutate(level = NISTmeterTOft(.data$level)) %>%
          select(.data$lake, .data$date, .data$level)
levels_summer <- levels %>%
                 filter(month(.data$date) %in% c(6, 7, 8)) %>%
                 group_by(date = year(.data$date)) %>%
                 summarise(level = mean(.data$level)) %>%
                 ungroup() %>%
                 mutate(date = as_datetime(sprintf("%s-06-01",
                                                   .data$date)))

exceeds <- calculate_exceedances(levels, c(10, 25, 50, 75, 90)) %>%
           mutate(value = NISTmeterTOft(.data$value))
rect_df <- dcast(filter(exceeds, ! .data$variable %in% c(25, 50, 75)),
                 lake~variable,
                 value.var = "value")
colnames(rect_df) <- c("lake", "ymax", "ymin")

plot_obj <- ggplot() +
            geom_line(data = levels_summer,
                      aes(x = .data$date,
                          y = .data$level,
                          color = "Lake Elevation")) +
            geom_line(data = fl_pop,
                      aes(x = .data$date,
                          y = 0.00003*.data$pop + 1094,
                          color = "Plant Population")) +
            geom_point(data = fl_pop,
                       aes(x = .data$date,
                           y = 0.00003*.data$pop + 1094,
                           color = "Plant Population")) +
            geom_rect(data = rect_df,
                      mapping = aes(xmin = min(levels$date),
                                    xmax = max(levels$date),
                                    ymin = .data$ymin,
                                    ymax = .data$ymax),
                      alpha = 0.25,
                      fill = "grey70",
                      color = NA) +
            geom_hline(data = filter(exceeds, .data$variable %in% c(50)),
                       aes(yintercept = .data$value),
                       color = "black",
                       linetype = "solid") +
            geom_hline(data = filter(exceeds, .data$variable %in% c(25, 75)),
                       aes(yintercept = .data$value),
                       color = "black",
                       linetype = "dashed") +
            scale_color_manual(name = "",
                               breaks = c("Lake Elevation", "Plant Population"),
                               values = c("black", "springgreen4")) +
            scale_y_continuous(name = "Mean Summer Lake Elevation (ft)",
                               sec.axis = sec_axis(trans=~(. - 1094)/0.00003,
                                                   labels = scales::comma,
                                                   name = "Plant Population Size")) +
            scale_x_datetime(name = "",
                             expand = c(0,0),
                             date_breaks = "5 years",
                             date_minor_breaks = "1 years",
                             date_labels = format("%Y")) +
            theme_bw() +
            theme(text = element_text(family = "Segoe UI Semilight",
                                      size = 12),
                  legend.position = "top")

ggsave("fassets_locoweed_population.png", plot_obj, "png",
       width = 6.5, height = 4, units = "in")
}
