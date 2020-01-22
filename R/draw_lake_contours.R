#' Draw contours for a given lake
#'
#' Given a dataset with lake levels and exceedance probabilities, this loads the
#' raster associated with the lake and draws contours at the desired levels.
#'
#' @param ranked_levels a data frame with the following columns:
#' \itemize{
#'   \item{lake}{name of the lake, e.g., "Long Lake"}
#'   \item{season}{name of season}
#'   \item{prob}{exceedance probability (percent)}
#'   \item{level}{lake level associated with that exceedance probability for the
#'                given lake and season}
#' }
#' @param lake name of lake to analyze, e.g., "Long"
#' @param season name of season ot analyze, e.g., "summer"
#'
#' @return plot_obj, a plot of the lake outline for each exceedance probability
#'   in the dataset for the given lake and season.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom raster rasterToContour
#' @importFrom stringr str_to_sentence
#' @import ggplot2
#' @import ggspatial
#' @import extrafont
#'
#' @export

draw_lake_contours <- function(ranked_levels, lake, season) {
  to_draw <- ranked_levels %>%
             filter(.data$lake == sprintf("%s Lake", !!lake),
                    .data$season == !!season)
  levels  <- to_draw$level
  probs   <- to_draw$prob

  lake_raster <- CSLSlevels::lake_raster[[lake]]
  contours    <- rasterToContour(lake_raster, levels = levels)



  plot_obj <- ggplot() +
              layer_spatial(data = contours,
                            aes(color = .data$level),
                            fill = NA,
                            size = 0.8) +
              theme_bw() +
              labs(title = str_to_sentence(season),
                   color = "Exceedance\nProbability (%)") +
              scale_color_manual(breaks = levels,
                                 labels = probs,
                                 # Electron Gold: ffab00
                                 # Tuscan Red: a80000
                                 # Ultra Blue: 004da8
                                 values = c("#a80000", "#ffab00", "#004da8")) +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = 10),
                    panel.grid.major = element_blank(),
                    plot.title = element_text(hjust=0.5))

  return(plot_obj)
}
