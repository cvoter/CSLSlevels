#' Calculate hydrologic metrics evaluated in the CSLS
#'
#' This function calculates hydrologic metrics describing the magnitude,
#' frequency, duration, rate of change, and timing of lake levels. Specific
#' metrics calculated include:
#'   * **median_level:** median monthly and overall lake levels
#'   * **cv_level:** coefficient of variation for monthly and overall lake levels
#'   * **exceedance_level:** lake levels corresponding to 10%, 25%, 50%, 75%,
#'                           and 90% exceedance probabilities
#'   * **exceedance_range:** range between 25% and 75% exceedance probabilities
#'                           and 10% and 90% exceedance probabilities for all
#'                           levels and annual mean levels.
#'   * **median_dur:** median duration at/above the 10% and 25% exceedance
#'                     probabilities or at/below the 75% and 90% exceedance
#'                     probabilities.
#'   * **cv_dur:** coefficient of vatiation for durations at/above the 10% and
#'                 25% exceedance probabilities and at/below the 75% and 90%
#'                 exceedance probabilities.
#'   * **median_rise_rate:** median lake level rise over 1 month, 3 months
#'                           (seasonal), and 12 months (annual).
#'   * **cv_rise_rate:** coefficient of variation in lake level rise over 1
#'                       month, 3 months (seasonal), and 12 months (annual).
#'   * **median_fall_rate:** median lake level rise over 1 month, 3 months
#'                           (seasonal), and 12 months (annual).
#'   * **cv_fall_rate:** coefficient of variation in lake level rise over 1
#'                       month, 3 months (seasonal), and 12 months (annual).
#'
#' @param df data frame to use, defaults "CSLSlevels::csls_levels". Must include
#'           columns for "date" (must be POSIXct), "lake" (must be factor), and
#'           one with the lake levels (named in "col_name" argument).
#' @param col_name name of column with lake level values to use. Defaults to
#'                 "level_pred".
#' @param metrics a list of which metrics to use. Defaults to all of them
#'                c("median_level", "cv_level", "exceedance_level",
#'                "exceedance_range", "median_dur", "cv_dur",
#'                "median_rise_rate", "cv_rise_rate", "median_fall_rate",
#'                "cv_fall_rate").
#'
#' @return summary, a data frame with the following columns:
#' \item{lake}{name of lake, character}
#' \item{metric}{name of metric, corresponds with values in inputted "metrics"
#'               argument, character}
#' \item{variable}{identifier for different types of metric. For example, the
#'                 metric "median_rise_rate" will have variables of "1", "3",
#'                 and "12" associated with it indicating the median rate over
#'                 1 month, 3 months, and 12 months, character}
#' \item{value}{calculated value of the metric, numeric}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom lubridate month year
#' @importFrom stats quantile
#' @import dplyr
#'
#' @export

calculate_metrics <- function(df = CSLSlevels::csls_levels,
                              col_name = "level_pred",
                              metrics = c("median_level",
                                          "cv_level",
                                          "exceedance_level",
                                          "exceedance_range",
                                          "median_dur",
                                          "cv_dur",
                                          "median_rise_rate",
                                          "cv_rise_rate",
                                          "median_fall_rate",
                                          "cv_fall_rate")) {

  # 0. Setup data frames =======================================================
  # Rename column with level info
  colnames(df)[which(colnames(df) == col_name)] <- "level"

  # Include month information
  df$month <- month(df$date)
  df$year  <- year(df$date)

  # Initialize summary data frame
  summary <- NULL

  # 1. MAGNITUDE ===============================================================
  # 1a. Monthly and annual median levels ---------------------------------------
  if ("median_level" %in% metrics) {
    # monthly median level
    vals <- df %>%
            group_by(.data$lake, .data$month) %>%
            summarise(median_level = median(.data$level, na.rm = TRUE)) %>%
            mutate(metric = "median_level") %>%
            ungroup() %>%
            as.data.frame() %>%
            select(lake = .data$lake,
                   metric = .data$metric,
                   variable = .data$month,
                   value = .data$median_level)
    summary <- rbind(summary, vals)

    # overall median level (month 0)
    vals <- df %>%
            group_by(.data$lake) %>%
            summarise(median_level = median(.data$level, na.rm = TRUE)) %>%
            mutate(metric = "median_level",
                   month = 0) %>%
            ungroup() %>%
            as.data.frame() %>%
            select(lake = .data$lake,
                   metric = .data$metric,
                   variable = .data$month,
                   value = .data$median_level)
    summary <- rbind(summary, vals)
  }

  # 1b. Monthly and annual CV levels -------------------------------------------
  if ("cv_level" %in% metrics) {
    # cv of monthly levels
    vals <- df %>%
            group_by(.data$lake, .data$month) %>%
            summarise(cv_level = 100*sd(.data$level, na.rm = TRUE)/
                                 mean(.data$level, na.rm = TRUE)) %>%
            mutate(metric = "cv_level") %>%
            ungroup() %>%
            as.data.frame() %>%
            select(lake = .data$lake,
                   metric = .data$metric,
                   variable = .data$month,
                   value = .data$cv_level)
    summary <- rbind(summary, vals)

    # cv of all levels (month 0)
    vals <- df %>%
            group_by(.data$lake) %>%
            summarise(cv_level = 100*sd(.data$level, na.rm = TRUE)/
                                 mean(.data$level, na.rm = TRUE)) %>%
            mutate(metric = "cv_level",
                   month = 0) %>%
            ungroup() %>%
            as.data.frame() %>%
            select(lake = .data$lake,
                   metric = .data$metric,
                   variable = .data$month,
                   value = .data$cv_level)
    summary <- rbind(summary, vals)
  }

  # 2. FREQUENCY ===============================================================
  # 2a. Exceedance probability levels ------------------------------------------
  if ("exceedance_level" %in% metrics) {
    exceeds <- calculate_exceedances(df, probs = c(10, 25, 50, 75, 90))
    vals    <- exceeds %>%
               mutate(metric = "exceedance_level") %>%
               select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 2b. Exceedance probability ranges ------------------------------------------
  if ("exceedance_range" %in% metrics) {
    # by all monthly values
    exceeds <- calculate_exceedances(df,
                                     probs = c(10, 25, 75, 90),
                                     melted = FALSE)
    ranges  <- exceeds %>%
               group_by(.data$lake) %>%
               mutate(m_10_90 = .data$`10` - .data$`90`,
                      m_25_75 = .data$`25` - .data$`75`) %>%
               ungroup() %>%
               as.data.frame() %>%
               select(.data$lake, .data$m_10_90, .data$m_25_75)
    vals    <- ranges %>%
               melt(id.vars = "lake") %>%
               mutate(metric = "exceedance_range") %>%
               select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)

    # by annual mean values
    annual  <- df %>%
               group_by(.data$lake, .data$year) %>%
               summarise(level = mean(.data$level, na.rm = TRUE)) %>%
               ungroup() %>%
               as.data.frame()
    exceeds <- calculate_exceedances(annual,
                                     probs = c(10, 25, 75, 90),
                                     melted = FALSE)
    ranges  <- exceeds %>%
               group_by(.data$lake) %>%
               mutate(a_10_90 = .data$`10` - .data$`90`,
                      a_25_75 = .data$`25` - .data$`75`) %>%
               ungroup() %>%
               as.data.frame() %>%
               select(.data$lake, .data$a_10_90, .data$a_25_75)
    vals    <- ranges %>%
               melt(id.vars = "lake") %>%
               mutate(metric = "exceedance_range") %>%
               select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 3. DURATION ================================================================
  durations <- calculate_durations(df, probs = c(10, 25, 75, 90))

  # 3a. Mean duration above/below levels ---------------------------------------
  if ("median_dur" %in% metrics) {
    vals <- durations %>%
            group_by(.data$lake, .data$variable) %>%
            summarise(value = median(.data$value, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            mutate(metric = "median_dur") %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 3b. CV duration above/below levels -----------------------------------------
  if ("cv_dur" %in% metrics) {
    vals <- durations %>%
            group_by(.data$lake, .data$variable) %>%
            summarise(value = 100*sd(.data$value, na.rm = TRUE)/
                              mean(.data$value, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            mutate(metric = "cv_dur") %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 4. RATE OF CHANGE ==========================================================
  rates   <- calculate_rates(df, months = c(1, 3, 12))
  rising  <- rates %>% filter(.data$value > 0)
  falling <- rates %>% filter(.data$value < 0)

  # 4a. Median rise rates ------------------------------------------------------
  if ("median_rise_rate" %in% metrics) {
    vals <- rising %>%
            group_by(.data$lake, .data$variable) %>%
            summarise(value = median(.data$value, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            mutate(metric = "median_rise_rate") %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 4b. CV rise rates ------------------------------------------------------
  if ("cv_rise_rate" %in% metrics) {
    vals <- rising %>%
            group_by(.data$lake, .data$variable) %>%
            summarise(value = 100*sd(.data$value, na.rm = TRUE)/
                              mean(.data$value, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            mutate(metric = "cv_rise_rate") %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 4c. Median fall rates ------------------------------------------------------
  if ("median_fall_rate" %in% metrics) {
    vals <- falling %>%
            group_by(.data$lake, .data$variable) %>%
            summarise(value = median(.data$value, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            mutate(metric = "median_fall_rate") %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 4d. CV fall rates ------------------------------------------------------
  if ("cv_fall_rate" %in% metrics) {
    vals <- falling %>%
            group_by(.data$lake, .data$variable) %>%
            summarise(value = 100*sd(.data$value, na.rm = TRUE)/
                        mean(.data$value, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            mutate(metric = "cv_fall_rate") %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  return(summary)
}
