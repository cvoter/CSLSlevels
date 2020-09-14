#' Draw depth bathymetry contours for a given lake
#'
#' @param lake name of lake to analyze, e.g., "Long"
#' @param max_levels_ft maximum observed lake levels and max contour lake levels
#'                      at each lake.
#' @param fill breaks, limits, and labels for fill colors at each lake
#' @param lakes vector will all lake names
#' @param text_size size of text on plot
#'
#' @return plot_obj, a plot with the depth contours of the lake.
#'
#' @importFrom rlang .data
#' @importFrom raster rasterToContour
#' @importFrom dplyr filter group_by summarise left_join mutate select ungroup
#' @importFrom NISTunits NISTmeterTOft NISTftTOmeter
#' @import ggplot2
#' @import ggspatial
#' @import extrafont
#' @import sf
#' @importFrom ggsn scalebar
#'
#' @export

plot_bathymetry <- function(lake,
                            max_levels_ft = data.frame(lake = c("Pleasant",
                                                                "Long",
                                                                "Plainfield"),
                                                       max_ft = c(984,
                                                               1105,
                                                               1106),
                                                       max_level = c(983.5,
                                                                     1105.1,
                                                                     1104.7)),
                            fill = list(Pleasant = list(limits = c(-5, 25),
                                                        breaks = c(20, 15, 10,
                                                                   5, 0, -5),
                                                        labels = c("20", "15",
                                                                   "10", "5",
                                                                   "0", "+5")),
                                        Long = list(limits = c(-7.5, 7.5),
                                                    breaks = c(6, 3, 0,
                                                               -3, -6),
                                                    labels = c("6", "3", "0",
                                                               "+3", "+6")),
                                        Plainfield = list(limits = c(-8.5,
                                                                     8.5),
                                                          breaks = c(8, 4, 0,
                                                                     -4, -8),
                                                          labels = c("8", "4",
                                                                     "0", "+4",
                                                                     "+8"))),
                            lakes = c("Pleasant", "Long", "Plainfield"),
                            text_size = 10) {

  # Key Levels, all lakes ------------------------------------------------------
  max_levels_ft$max_ft <- apply(max_levels_ft[,2:3], 1, min)
  key_levels    <- CSLSlevels::hist_metrics_short %>%
                   filter(.data$lake %in% lakes,
                          .data$metric == "exceedance_level",
                          .data$variable == "50") %>%
                   group_by(.data$lake, .data$metric, .data$variable) %>%
                   summarise(median_m = median(.data$value)) %>%
                   ungroup() %>%
                   left_join(max_levels_ft, by = "lake") %>%
                   mutate(median_ft = NISTmeterTOft(.data$median_m),
                          max_m = NISTftTOmeter(.data$max_ft)) %>%
                   select(lake = .data$lake,
                          median_m = .data$median_m,
                          median_ft = .data$median_ft,
                          max_m = .data$max_m,
                          max_ft = .data$max_ft)
  key_levels$min_m  <- NA
  key_levels$min_ft <- NA
  for (i in 1:nrow(key_levels)) {
    l                    <- key_levels$lake[i]
    key_levels$min_m[i]  <- minValue(CSLSlevels::lake_raster[[l]])
    key_levels$min_ft[i] <- NISTmeterTOft(key_levels$min_m[i])
  }

  # Key levels, this lake ------------------------------------------------------
  median_m <- key_levels$median_m[which(key_levels$lake == lake)]
  min_m    <- key_levels$min_m[which(key_levels$lake == lake)]
  max_m    <- key_levels$max_m[which(key_levels$lake == lake)]

  upper_contours <- seq(median_m, max_m, by = NISTftTOmeter(0.5))
  lower_contours <- seq(median_m, min_m, by = -NISTftTOmeter(0.5))
  lake_levels    <- unique(c(rev(lower_contours), upper_contours))

  # Breaks, this lake ----------------------------------------------------------
  fill_limits <- fill[[lake]]$limits
  fill_breaks <- fill[[lake]]$breaks
  fill_labels <- fill[[lake]]$labels

  if (lake == "Pleasant") {
    scalebar_loc <- "topleft"
    interval     <- 5
  } else if (lake == "Plainfield") {
    scalebar_loc <- "bottomleft"
    interval     <- 2
  } else {
    scalebar_loc <- "bottomleft"
    interval     <- 1
  }

  # Raster ---------------------------------------------------------------------
  lake_raster   <- CSLSlevels::lake_raster[[lake]]
  lake_points   <- as.data.frame(rasterToPoints(lake_raster))
  colnames(lake_points) <- c("x", "y", "z")
  lake_points$z <- round(NISTmeterTOft(median_m - lake_points$z), 1)

  # Filled contours ------------------------------------------------------------
  contours          <- rasterToContour(lake_raster, levels = lake_levels)
  contours_sf       <- st_as_sf(contours)
  contours_sf$level <- as.numeric(as.character(contours_sf$level))
  contours_sf$level <- median_m - contours_sf$level
  contours_sf$level <- NISTmeterTOft(contours_sf$level)
  contours_poly     <- st_polygonize(contours_sf)
  contours_poly     <- contours_poly[order(contours_poly$level, decreasing = FALSE),]
  median            <- rasterToContour(lake_raster, levels = median_m)
  median_sf         <- st_as_sf(median)
  outline           <- rasterToContour(lake_raster, levels = max_m)
  outline_sf        <- st_as_sf(outline)

  # Line contours --------------------------------------------------------------
  lake_levels       <- lake_levels[(round(contours_sf$level,1) %% interval == 0) &
                                     round(contours_sf$level,1) > 0]
  contours          <- rasterToContour(lake_raster, levels = lake_levels)
  contours_sf       <- st_as_sf(contours)
  contours_sf$level <- as.numeric(as.character(contours_sf$level))
  contours_sf$level <- median_m - contours_sf$level
  contours_sf$level <- round(NISTmeterTOft(contours_sf$level), 1)
  # for (i in 1:length(contours_sf$level)) {
  #   contours_sf$level[i] <- ifelse(as.numeric(contours_sf$level[i]) >= 0,
  #                                  sprintf("%s ft", contours_sf$level[i]),
  #                                  sprintf("+%s ft",
  #                                          abs(as.numeric(contours_sf$level[i]))))
  # }
  contours_sf       <- st_cast(contours_sf, "LINESTRING")

  # Plot -----------------------------------------------------------------------
  plot_obj    <- ggplot() +
                 geom_sf(data = outline_sf,
                         color = NA,
                         fill = NA)
  for (i in 1:length(contours_poly$level)) {
    plot_obj  <- plot_obj +
                 geom_sf(data = contours_poly[i,],
                               aes(fill = .data$level),
                               color = NA,
                               size = 0.3)
  }

  if (lake == "Pleasant") {
    plot_obj <- plot_obj +
                geom_sf(data = contours_sf,
                        color = "#072859",
                        # color = "grey70",
                        fill = NA) +
                scale_fill_gradientn(colors = c("#543005", "white", "#08306B"),
                                     values = c(0, 0.166667, 1),
                                     limits = fill_limits,
                                     breaks = fill_breaks,
                                     labels = fill_labels)
  } else {
    plot_obj <- plot_obj +
                geom_sf(data = contours_sf,
                        color = "#072859",
                        fill = NA) +
                scale_fill_gradient2(low = "#543005",
                                     high = "#08306B",
                                     mid = "white",
                                     midpoint = 0,
                                     limits = fill_limits,
                                     breaks = fill_breaks,
                                     labels = fill_labels)
  }

  plot_obj <- plot_obj +
              geom_sf(data = median_sf,
                      color = "black",
                      fill = NA,
                      size = 0.8) +
              labs(title = "", x = "", y = "",
                   fill = "Lake Depth (ft)") +
              guides(fill = guide_colorbar(reverse = TRUE,
                                           title.position = "right"),
                     color = FALSE) +
              ggsn::scalebar(data = outline_sf,
                             location = scalebar_loc,
                             dist = 0.1,
                             dist_unit = "mi",
                             transform = FALSE,
                             height = 0.02,
                             st.size = 3.5) +
              theme_void() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = 10),
                    legend.title = element_text(angle = -90,
                                                vjust = 0.5,
                                                hjust = 0.5),
                    legend.position = "right",
                    legend.box = margin(0,0,0,0))
  return(plot_obj)
}
