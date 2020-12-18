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
#' @param dts vector of dt values (months) to calculate metrics for, defaults to
#'            1 (monthly), 3 (seasonal), and 12 (annual).
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
#' \item{series}{type of time series used (monthly, seasonal, annual)}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom lubridate month year
#' @importFrom stats quantile median sd
#' @import dplyr
#'
#' @export

calculate_metrics <- function(df_month = CSLSlevels::hist_levels,
                              dts = c(1, 3, 12),
                              metrics = c("median_level",
                                          "cv_level",
                                          "exceedance_level",
                                          "exceedance_range",
                                          "depart_median",
                                          "median_dur",
                                          "cv_dur",
                                          "num_dur",
                                          "num_2yr",
                                          "num_dur_decade",
                                          "num_2yr_decade",
                                          "median_rise_rate",
                                          "cv_rise_rate",
                                          "median_fall_rate",
                                          "cv_fall_rate",
                                          "fast_rise",
                                          "fast_rise_decade",
                                          "fast_fall",
                                          "fast_fall_decade",
                                          "good_spawning")) {

  # 0. Setup data frames =======================================================
  # Get month and year information
  df_month$month <- month(df_month$date)
  df_month$year  <- year(df_month$date)
  nyrs           <- df_month %>%
                    count(.data$lake) %>%
                    mutate(n = .data$n/12)

  # Also get seasonal (3-mo avg) levels
  df_season <- df_month %>%
               group_by(.data$lake) %>%
               arrange(.data$date) %>%
               mutate(level = as.numeric(stats::filter(.data$level,
                                                       rep(1/3, 3),
                                                       sides = 2))) %>%
               ungroup() %>%
               filter(.data$month %in% c(1,4,7,10))

  # Also get annual levels
  df_annual  <- df_month %>%
                group_by(lake = .data$lake,
                         date = .data$year) %>%
                summarise(level = mean(.data$level, na.rm = TRUE),
                          .groups = "drop")

  # Initialize summary list
  summary <- list()
  output  <- list()
  i <- 1
  j <- 1

  for (dt in dts) {
    if (dt == 1) {
      df     <- df_month
      series <- "month"
    } else if (dt == 3) {
      df     <- df_season
      series <- "season"
    } else if (dt == 12) {
      df     <- df_annual
      series <- "annual"
    }

    # 1. MAGNITUDE =============================================================
    # 1a. Monthly and overall median levels and cv of levels -------------------
    if ("median_level" %in% metrics | "cv_level" %in% metrics) {
      if (dt %in% c(1, 3)) {
        # monthly median level and cv of levels
        vals <- df %>%
                group_by(.data$lake, .data$month) %>%
                summarise(median_level = median(.data$level, na.rm = TRUE),
                          cv_level = 100*sd(.data$level, na.rm = TRUE)/
                                     mean(.data$level, na.rm = TRUE),
                          .groups = "drop") %>%
                mutate(month = as.character(.data$month)) %>%
                melt(id.vars = c("lake", "month")) %>%
                select(lake = .data$lake,
                       metric = .data$variable,
                       variable = .data$month,
                       value = .data$value) %>%
                filter(.data$metric %in% metrics)
        summary[[i]] <- vals; i <- i + 1
      }

      # overall median level and cv of levels (month 0)
      vals <- df %>%
              group_by(.data$lake) %>%
              summarise(median_level = median(.data$level, na.rm = TRUE),
                        cv_level = 100*sd(.data$level, na.rm = TRUE)/
                                   mean(.data$level, na.rm = TRUE),
                        .groups = "drop") %>%
              mutate(month = "0") %>%
              melt(id.vars = c("lake", "month")) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$month,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    # 1b. Exceedance probability levels ----------------------------------------
    if ("exceedance_level" %in% metrics) {
      exceeds <- calculate_exceedances(df, probs = c(10, 25, 50, 75, 90))
      vals    <- exceeds %>%
                 mutate(metric = "exceedance_level",
                        variable = as.character(.data$variable)) %>%
                 select(.data$lake, .data$metric, .data$variable, .data$value)
      summary[[i]] <- vals; i <- i + 1
    }

    # 1c. Exceedance probability ranges ----------------------------------------
    if ("exceedance_range" %in% metrics) {
      # by all monthly values
      exceeds <- calculate_exceedances(df,
                                       probs = c(10, 25, 75, 90),
                                       melted = FALSE)
      ranges  <- exceeds %>%
                 group_by(.data$lake) %>%
                 mutate(range_10_90 = .data$`10` - .data$`90`,
                        range_25_75 = .data$`25` - .data$`75`) %>%
                 ungroup() %>%
                 select(.data$lake, .data$range_10_90, .data$range_25_75)
      vals    <- ranges %>%
                 melt(id.vars = "lake") %>%
                 mutate(metric = "exceedance_range",
                        variable = as.character(.data$variable)) %>%
                 select(.data$lake, .data$metric, .data$variable, .data$value)
      summary[[i]] <- vals; i <- i + 1
    }

    # 2. FREQUENCY =============================================================
    durations <- calculate_durations(df,
                                     probs = c(10, 25, 50, 75, 90),
                                     departures = FALSE) %>%
                 mutate(value = .data$value*dt,
                        variable = as.character(.data$variable))

    # 2a. Departure from median ------------------------------------------------
    if ("depart_median" %in% metrics) {
      probs <- calculate_exceedances(df, departures = c(NISTftTOmeter(1),
                                                          NISTftTOmeter(-1)))
      vals  <- probs %>%
               mutate(metric = "depart_median",
                      variable = as.character(.data$variable)) %>%
               select(.data$lake, .data$metric, .data$variable, .data$value)
      summary[[i]] <- vals; i <- i + 1
    }

    # 2b. Frequency of high/low levels -----------------------------------------
    if ("num_dur" %in% metrics | "num_dur_decade" %in% metrics) {
      vals <- durations %>%
              count(.data$lake, .data$variable) %>%
              select(lake = .data$lake,
                     probs = .data$variable,
                     num_dur = .data$n) %>%
              left_join(nyrs, by = "lake") %>%
              mutate(num_dur_decade = .data$num_dur/(.data$n/10)) %>%
              select(.data$lake, .data$num_dur, .data$num_dur_decade,
                     .data$probs) %>%
              melt(id.vars = c("lake", "probs")) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$probs,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    if ("num_2yr" %in% metrics | "num_2yr_decade" %in% metrics) {
      vals <- durations %>%
              filter(.data$value >= 24) %>%
              count(.data$lake, .data$variable) %>%
              select(lake = .data$lake,
                     probs = .data$variable,
                     num_2yr = .data$n) %>%
              left_join(nyrs, by = "lake") %>%
              mutate(num_2yr_decade = .data$num_2yr/(.data$n/10)) %>%
              select(.data$lake, .data$num_2yr, .data$num_2yr_decade,
                     .data$probs) %>%
              melt(id.vars = c("lake", "probs")) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$probs,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    # 3. DURATION ==============================================================
    # 3a. Median duration and CV of duration above/below levels ----------------
    if ("median_dur" %in% metrics | "cv_dur" %in% metrics) {
      vals <- durations %>%
              group_by(lake = .data$lake,
                       probs = .data$variable) %>%
              summarise(median_dur = median(.data$value, na.rm = TRUE),
                        cv_dur = 100*sd(.data$value, na.rm = TRUE)/
                                 mean(.data$value, na.rm = TRUE),
                        .groups = "drop") %>%
              melt(id.vars = c("lake", "probs")) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$probs,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    # 4. RATE OF CHANGE ========================================================
    periods <- c(1, 3, 12)/dt
    periods <- periods[periods >= 1]
    rates   <- calculate_rates(df, months = periods) %>%
               mutate(time = as.numeric(as.character(.data$variable))) %>%
               mutate(variable = as.character(.data$time*dt)) %>%
               select(.data$lake, .data$variable, .data$value)
    rising  <- rates %>% filter(.data$value > 0)
    falling <- rates %>% filter(.data$value < 0)

    # 4a. Median and CV of rise rates ------------------------------------------
    if ("median_rise_rate" %in% metrics | "cv_rise_rate" %in% metrics) {
      vals <- rising %>%
              group_by(lake = .data$lake,
                       time = .data$variable) %>%
              summarise(median_rise_rate = median(.data$value, na.rm = TRUE),
                        cv_rise_rate = 100*sd(.data$value, na.rm = TRUE)/
                                       mean(.data$value, na.rm = TRUE),
                        .groups = "drop") %>%
              melt(id.vars = c("lake", "time")) %>%
              mutate(time = as.character(.data$time)) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$time,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    # 4b. Median and CV of fall rates ------------------------------------------
    if ("median_fall_rate" %in% metrics | "cv_fall_rate" %in% metrics) {
      vals <- falling %>%
              group_by(lake = .data$lake,
                       time = .data$variable) %>%
              summarise(median_fall_rate = median(.data$value, na.rm = TRUE),
                        cv_fall_rate = 100*sd(.data$value, na.rm = TRUE)/
                                       mean(.data$value, na.rm = TRUE),
                        .groups = "drop") %>%
              melt(id.vars = c("lake", "time")) %>%
              mutate(time = as.character(.data$time)) %>%
              select(lake = .data$lake,
                     metric = .data$variable,
                     variable = .data$time,
                     value = .data$value) %>%
              filter(.data$metric %in% metrics)
      summary[[i]] <- vals; i <- i + 1
    }

    # 4c. Fast rise rates ------------------------------------------------------
    fast <- NISTunits::NISTftTOmeter(1.5)
    really_fast <- NISTunits::NISTftTOmeter(3)
    if ("fast_rise" %in% metrics | "fast_rise_decade" %in% metrics) {
      vals <- rising %>%
              filter(.data$variable == "12") %>%
              mutate(rate_3ft = ifelse(.data$value >= really_fast, TRUE, FALSE),
                     rate_1_5ft = ifelse(.data$value >= fast, TRUE, FALSE)) %>%
              count(.data$lake, .data$rate_1_5ft, .data$rate_3ft) %>%
              group_by(.data$lake) %>%
              summarise(rate_3ft = sum(.data$n[.data$rate_3ft == TRUE]),
                        rate_1_5ft = sum(.data$n[.data$rate_1_5ft == TRUE]),
                        .groups = "drop") %>%
              melt(id.vars = "lake") %>%
              mutate(metric = "fast_rise",
                     variable = as.character(.data$variable)) %>%
              select(.data$lake, .data$metric, .data$variable, .data$value)
      summary[[i]] <- vals; i <- i + 1

      if ("fast_rise_decade" %in% metrics) {
        vals <- vals %>%
                left_join(nyrs, by = "lake") %>%
                mutate(value = .data$value/(.data$n/10),
                       metric = "fast_rise_decade") %>%
                select(.data$lake, .data$metric, .data$variable, .data$value)
        summary[[i]] <- vals; i <- i + 1
      }
    }

    # 4d. Fast fall rates ------------------------------------------------------
    fast <- NISTunits::NISTftTOmeter(-1.5)
    really_fast <- NISTunits::NISTftTOmeter(-3)
    if ("fast_fall" %in% metrics | "fast_fall_decade" %in% metrics) {
      vals <- falling %>%
              filter(.data$variable == "12") %>%
              mutate(rate_3ft = ifelse(.data$value <= really_fast, TRUE, FALSE),
                     rate_1_5ft = ifelse(.data$value <= fast, TRUE, FALSE)) %>%
              count(.data$lake, .data$rate_1_5ft, .data$rate_3ft) %>%
              group_by(.data$lake) %>%
              summarise(rate_3ft = sum(.data$n[.data$rate_3ft == TRUE]),
                        rate_1_5ft = sum(.data$n[.data$rate_1_5ft == TRUE]),
                        .groups = "drop") %>%
              melt(id.vars = "lake") %>%
              mutate(metric = "fast_fall",
                     variable = as.character(.data$variable)) %>%
              select(.data$lake, .data$metric, .data$variable, .data$value)
      summary[[i]] <- vals; i <- i + 1

      if ("fast_fall_decade" %in% metrics) {
        vals <- vals %>%
                left_join(nyrs, by = "lake") %>%
                mutate(value = .data$value/(.data$n/10),
                       metric = "fast_fall_decade") %>%
                select(.data$lake, .data$metric, .data$variable, .data$value)
        summary[[i]] <- vals; i <- i + 1
      }
    }

    # 5. TIMING ================================================================
    if ("good_spawning" %in% metrics & dt %in% c(1, 3)) {
      if (min(nyrs$n) >= 2) {
        spawning <- calculate_spawning(df)
        vals     <- spawning %>%
                    group_by(.data$lake) %>%
                    summarise(high_spring = sum(.data$high_spring,
                                                na.rm = TRUE),
                              steady_summer = sum(.data$steady_summer,
                                                  na.rm = TRUE),
                              good_spawning = sum(.data$good_spawning,
                                                  na.rm = TRUE),
                              .groups ="drop") %>%
                    left_join(nyrs, by = "lake") %>%
                    mutate(high_spring = 100*.data$high_spring/
                                         (.data$n-1),
                           steady_summer = 100*.data$steady_summer/
                                           .data$n,
                           good_spawning = 100*.data$good_spawning/
                                           (.data$n-1)) %>%
                    select(.data$lake, .data$high_spring, .data$steady_summer,
                           .data$good_spawning) %>%
                    melt(id.vars = "lake") %>%
                    mutate(metric = "good_spawning",
                           variable = as.character(.data$variable)) %>%
                    select(.data$lake, .data$metric, .data$variable,
                           .data$value)
      } else {
        lakes    <- unique(df$lake)
        variable <- c("high_spring", "steady_summer", "good_spawning")
        vals     <- expand.grid(lakes, variable)
        colnames(vals) <- c("lake", "variable")
        vals     <- vals %>%
                    mutate(metric = "good_spawning",
                           value = 0,
                           variable = as.character(.data$variable)) %>%
                    select(.data$lake, .data$metric, .data$variable,
                           .data$value)
      }
      summary[[i]] <- vals; i <- i + 1
    }
    # Combine metrics for this type of time series
    summary        <- bind_rows(summary)
    summary$series <- series
    output[[j]]    <- summary; j <- j + 1
    summary        <- list()
    i              <- 1
  }
  output <- bind_rows(output)
  return(output)
}
