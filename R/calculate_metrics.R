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
#' @param df data frame to use, defaults "CSLSlevels::hist_levels". Must include
#'           columns for "date" (must be POSIXct), "lake" (must be factor), and
#'           one with the lake levels (named in "col_name" argument).
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
#' @importFrom stats quantile median sd
#' @import dplyr
#'
#' @export

calculate_metrics <- function(df = CSLSlevels::hist_levels,
                              metrics = c("median_level",
                                          "cv_level",
                                          "exceedance_level",
                                          "exceedance_range",
                                          "depart_median",
                                          "median_dur",
                                          "median_dur_seas",
                                          "cv_dur",
                                          "cv_dur_seas",
                                          "num_dur",
                                          "num_dur_seas",
                                          "num_2yr",
                                          "num_2yr_seas",
                                          "num_dur_decade",
                                          "num_dur_decade_seas",
                                          "num_2yr_decade",
                                          "num_2yr_decade_seas",
                                          "median_rise_rate",
                                          "cv_rise_rate",
                                          "median_fall_rate",
                                          "cv_fall_rate",
                                          "fast_rise",
                                          "fast_fall",
                                          "good_spawning")) {

  # 0. Setup data frames =======================================================
  # Include month information
  df$month <- month(df$date)
  df$year  <- year(df$date)

  # Also get seasonal (3-mo avg) levels
  df_seasons <- df %>%
                group_by(.data$lake) %>%
                arrange(.data$date) %>%
                mutate(level = as.numeric(stats::filter(.data$level,
                                                         rep(1/3, 3),
                                                         sides = 2))) %>%
                ungroup() %>%
                filter(.data$month %in% c(1,4,7,10))

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

  # 1c. Exceedance probability levels ------------------------------------------
  if ("exceedance_level" %in% metrics) {
    exceeds <- calculate_exceedances(df, probs = c(10, 25, 50, 75, 90))
    vals    <- exceeds %>%
               mutate(metric = "exceedance_level") %>%
               select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 1d. Exceedance probability ranges ------------------------------------------
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

  # 2. FREQUENCY ===============================================================
  durations         <- calculate_durations(df,
                                           probs = c(10, 25, 50, 75, 90),
                                           departures = TRUE)
  durations_seasons <- calculate_durations(df_seasons,
                                           probs = c(10, 25, 50, 75, 90),
                                           departures = TRUE) %>%
                       mutate(value = .data$value*3)

  # 2a. Departure from median --------------------------------------------------
  if ("depart_median" %in% metrics) {
    probs   <- calculate_exceedances(df, departures = c(NISTftTOmeter(1),
                                                        NISTftTOmeter(-1)))
    vals    <- probs %>%
               mutate(metric = "depart_median") %>%
               select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 2b. Frequency of high/low levels -------------------------------------------
  if ("num_dur" %in% metrics) {
    vals <- durations %>%
            count(.data$lake, .data$variable) %>%
            as.data.frame() %>%
            mutate(metric = "num_dur",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  if ("num_2yr" %in% metrics) {
    vals <- durations %>%
            filter(.data$value >= 24) %>%
            count(.data$lake, .data$variable) %>%
            as.data.frame() %>%
            mutate(metric = "num_2yr",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  if ("num_dur_seas" %in% metrics) {
    vals <- durations_seasons %>%
            count(.data$lake, .data$variable) %>%
            as.data.frame() %>%
            mutate(metric = "num_dur_seas",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  if ("num_2yr_seas" %in% metrics) {
    vals <- durations_seasons %>%
            filter(.data$value >= 24) %>%
            count(.data$lake, .data$variable) %>%
            as.data.frame() %>%
            mutate(metric = "num_2yr_seas",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 3d. Frequency of high/low levels per decade --------------------------------
  if ("num_dur_decade" %in% metrics) {
    nyrs <- df %>%
            count(.data$lake) %>%
            mutate(n = .data$n/12)
    vals <- durations %>%
            count(.data$lake, .data$variable) %>%
            as.data.frame() %>%
            mutate(metric = "num_dur_decade",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    vals <- vals %>%
            left_join(nyrs, by = "lake") %>%
            mutate(value = .data$value/(.data$n/10)) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  if ("num_dur_decade_seas" %in% metrics) {
    nyrs <- df %>%
            count(.data$lake) %>%
            mutate(n = .data$n/12)
    vals <- durations_seasons %>%
            count(.data$lake, .data$variable) %>%
            as.data.frame() %>%
            mutate(metric = "num_dur_decade_seas",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    vals <- vals %>%
            left_join(nyrs, by = "lake") %>%
            mutate(value = .data$value/(.data$n/10)) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  if ("num_2yr_decade" %in% metrics) {
    nyrs <- df %>%
            count(.data$lake) %>%
            mutate(n = .data$n/12)
    vals <- durations %>%
            filter(.data$value >= 24) %>%
            count(.data$lake, .data$variable) %>%
            as.data.frame() %>%
            mutate(metric = "num_2yr_decade",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    vals <- vals %>%
            left_join(nyrs, by = "lake") %>%
            mutate(value = .data$value/(.data$n/10)) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  if ("num_2yr_decade_seas" %in% metrics) {
    nyrs <- df %>%
            count(.data$lake) %>%
            mutate(n = .data$n/12)
    vals <- durations_seasons  %>%
            filter(.data$value >= 24) %>%
            count(.data$lake, .data$variable) %>%
            as.data.frame() %>%
            mutate(metric = "num_2yr_decade_seas",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    vals <- vals %>%
            left_join(nyrs, by = "lake") %>%
            mutate(value = .data$value/(.data$n/10)) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 3. DURATION ================================================================
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

  if ("median_dur_seas" %in% metrics) {
    vals <- durations_seasons %>%
            group_by(.data$lake, .data$variable) %>%
            summarise(value = median(.data$value, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            mutate(metric = "median_dur_seas") %>%
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

  if ("cv_dur_seas" %in% metrics) {
    vals <- durations_seasons %>%
            group_by(.data$lake, .data$variable) %>%
            summarise(value = 100*sd(.data$value, na.rm = TRUE)/
                        mean(.data$value, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            mutate(metric = "cv_dur_seas") %>%
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

  if ("fast_rise" %in% metrics) {
    nyrs <- df %>%
            count(.data$lake) %>%
            mutate(n = .data$n/12)

     vals <- rising %>%
            filter(.data$value >= NISTunits::NISTftTOmeter(3),
                   .data$variable == "12") %>%
            group_by(.data$lake, .drop = FALSE) %>%
            count() %>%
            ungroup() %>%
            mutate(metric = "fast_rise",
                   variable = "rate_3ft",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)

    vals <- vals %>%
            left_join(nyrs, by = "lake") %>%
            mutate(value = .data$value/(.data$n/10),
                   metric = "fast_rise_decade") %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)

    vals <- rising %>%
            filter(.data$value >= NISTunits::NISTftTOmeter(1.5),
                   .data$variable == "12") %>%
            group_by(.data$lake, .drop = FALSE) %>%
            count() %>%
            ungroup() %>%
            mutate(metric = "fast_rise",
                   variable = "rate_1_5ft",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)

    vals <- vals %>%
            left_join(nyrs, by = "lake") %>%
            mutate(value = .data$value/(.data$n/10),
                   metric = "fast_rise_decade") %>%
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

  if ("fast_fall" %in% metrics) {
    nyrs <- df %>%
            count(.data$lake) %>%
            mutate(n = .data$n/12)
    vals <- falling %>%
            filter(.data$value <= NISTunits::NISTftTOmeter(-3),
             .data$variable == "12") %>%
            group_by(.data$lake, .drop = FALSE) %>%
            count() %>%
            ungroup() %>%
            mutate(metric = "fast_fall",
                   variable = "rate_3ft",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)

    vals <- vals %>%
            left_join(nyrs, by = "lake") %>%
            mutate(value = .data$value/(.data$n/10),
                   metric = "fast_fall_decade") %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)

    vals <- falling %>%
            filter(.data$value <= NISTunits::NISTftTOmeter(-1.5),
                   .data$variable == "12") %>%
            group_by(.data$lake, .drop = FALSE) %>%
            count() %>%
            ungroup() %>%
            mutate(metric = "fast_fall",
                   variable = "rate_1_5ft",
                   value = .data$n) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
    vals <- vals %>%
            left_join(nyrs, by = "lake") %>%
            mutate(value = .data$value/(.data$n/10),
                   metric = "fast_fall_decade") %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 4d. CV fall rates ------------------------------------------------------
  if ("cv_fall_rate" %in% metrics) {
    vals <- falling %>%
            group_by(.data$lake, .data$variable) %>%
            summarise(value = 100*sd(.data$value, na.rm = TRUE)/
                        mean(.data$value, na.rm = TRUE),
                      .groups = "drop") %>%
            as.data.frame() %>%
            mutate(metric = "cv_fall_rate") %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
    summary <- rbind(summary, vals)
  }

  # 5. TIMING ==================================================================
  if ("good_spawning" %in% metrics) {
    nyrs <- df %>%
            count(.data$lake) %>%
            mutate(n = .data$n/12)
    if (min(nyrs$n) >= 2) {
      spawning <- calculate_spawning(df)
      vals     <- spawning %>%
                  group_by(.data$lake) %>%
                  summarise(high_spring = sum(.data$high_spring, na.rm = TRUE),
                            steady_summer = sum(.data$steady_summer, na.rm = TRUE),
                            good_spawning = sum(.data$good_spawning, na.rm = TRUE),
                            .groups ="drop") %>%
                  left_join(nyrs, by = "lake") %>%
                  mutate(high_spring = 100*.data$high_spring/(.data$n-1),
                         steady_summer = 100*.data$steady_summer/.data$n,
                         good_spawning = 100*.data$good_spawning/(.data$n-1)) %>%
                  select(.data$lake, .data$high_spring, .data$steady_summer,
                         .data$good_spawning) %>%
                  melt(id.vars = "lake")
      vals     <- vals %>%
                  mutate(metric = "good_spawning") %>%
                  select(.data$lake, .data$metric, .data$variable, .data$value)
    } else {
      lakes    <- unique(df$lake)
      variable <- c("high_spring", "steady_summer", "good_spawning")
      vals     <- expand.grid(lakes, variable)
      colnames(vals) <- c("lake", "variable")
      vals     <- vals %>%
                  mutate(metric = "good_spawning",
                         value = 0) %>%
                  select(.data$lake, .data$metric, .data$variable, .data$value)
    }
    summary <- rbind(summary, vals)
  }

  return(summary)
}
