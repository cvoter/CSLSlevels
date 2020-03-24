#' Calculate hydrologic metrics evaluated in the CSLS
#'
#' @param df data frame to use, defaults "CSLSlevels::csls_levels".
#' @param start_date Earliest date to use in calculations in datetime format.
#'                   Defaults to NULL to use entire range of dates in df.
#' @param end_date Latest date to use in calculations in datetime format.
#'                 Defaults to NULL to use entire range of dates in df.
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
                              start_date = NULL,
                              end_date = NULL,
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

  # Rename column with level info
  colnames(df)[which(colnames(df) == col_name)] <- "level"

  # Limit date range
  if (is.null(start_date)) {
    start_date <- min(df$date)
  }
  if (is.null(end_date)) {
    end_date <- max(df$date)
  }
  df <- df %>%
        filter(.data$date > start_date,
               .data$date < end_date)

  # Include month information
  df$month <- month(df$date)
  df$year  <- year(df$date)

  # Loop through metrics
  summary <- NULL

  # 1. Median monthly and annual levels ------------------------------------------
  if ("median_level" %in% metrics) {
    # monthly median level
    vals <- df %>%
            group_by(.data$lake, .data$month) %>%
            summarise(median_level = median(.data$level)) %>%
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
            summarise(median_level = median(.data$level)) %>%
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

  # 2. CV monthly and annual levels --------------------------------------------
  if ("cv_level" %in% metrics) {
    # cv of monthly levels
    vals <- df %>%
            group_by(.data$lake, .data$month) %>%
            summarise(cv_level = 100*sd(.data$level)/mean(.data$level)) %>%
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
            summarise(cv_level = 100*sd(.data$level)/mean(.data$level)) %>%
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

  # 3. Exceedances -------------------------------------------------------------
  exceedances <- calculate_exceedances(df)

  # 3a. Exceedance probability levels ------------------------------------------
  if ("exceedance_level" %in% metrics) {
    vals <- melt_and_add_metric(exceedances, "exceedance_level", prefix = "p")
    summary <- rbind(summary, vals)
  }

  # 3b. Exceedance probability ranges ------------------------------------------
  if ("exceedance_range" %in% metrics) {
    # by all monthly values
    vals <- exceedances %>%
            group_by(.data$lake) %>%
            mutate(m_10_90 = .data$p10 - .data$p90,
                   m_25_75 = .data$p25 - .data$p75) %>%
            ungroup() %>%
            as.data.frame() %>%
            select(.data$lake, .data$m_10_90, .data$m_25_75)
    vals <- melt_and_add_metric(vals, "exceedance_range")
    summary <- rbind(summary, vals)

    # by annual mean values
    vals <- df %>%
            group_by(.data$lake, .data$year) %>%
            summarise(level = mean(.data$level)) %>%
            ungroup() %>%
            as.data.frame()
    vals <- calculate_exceedances(vals)
    vals <- vals %>%
            group_by(.data$lake) %>%
            mutate(a_10_90 = .data$p10 - .data$p90,
                   a_25_75 = .data$p25 - .data$p75) %>%
            ungroup() %>%
            as.data.frame() %>%
            select(.data$lake, .data$a_10_90, .data$a_25_75)
    vals <- melt_and_add_metric(vals, "exceedance_range")
    summary <- rbind(summary, vals)
  }

  # 4. Durations ---------------------------------------------------------------
  vals      <- merge(df, exceedances)
  durations <- vals %>%
               arrange(.data$date) %>%
               mutate(above_10 = ifelse(.data$level > .data$p10, 1, 0),
                      above_25 = ifelse(.data$level > .data$p25, 1, 0),
                      below_75 = ifelse(.data$level < .data$p75, 1, 0),
                      below_90 = ifelse(.data$level < .data$p90, 1, 0)) %>%
               select(.data$lake, .data$date, .data$above_10, .data$above_25,
                      .data$below_75, .data$below_90)
  # 4a. Mean duration above/below levels ---------------------------------------
  if ("median_dur" %in% metrics) {
    vals <- durations %>%
            group_by(.data$lake) %>%
            mutate(median_10 = median(rle(.data$above_10)$lengths),
                   median_25 = median(rle(.data$above_25)$lengths),
                   median_75 = median(rle(.data$below_75)$lengths),
                   median_90 = median(rle(.data$below_90)$lengths)) %>%
            ungroup() %>%
            as.data.frame() %>%
            select(.data$lake, .data$median_10, .data$median_25,
                   .data$median_75, .data$median_90) %>%
            unique()
    vals <- melt_and_add_metric(vals, "median_dur", "median_")
    summary <- rbind(summary, vals)
  }

  # 4b. CV duration above/below levels -----------------------------------------
  if ("cv_dur" %in% metrics) {
    vals <- durations %>%
            group_by(.data$lake) %>%
            mutate(cv_10 = 100*sd(rle(.data$above_10)$lengths)/
                           mean(rle(.data$above_10)$lengths),
                   cv_25 = 100*sd(rle(.data$above_25)$lengths)/
                           mean(rle(.data$above_25)$lengths),
                   cv_75 = 100*sd(rle(.data$below_75)$lengths)/
                           mean(rle(.data$below_75)$lengths),
                   cv_90 = 100*sd(rle(.data$below_90)$lengths)/
                           mean(rle(.data$below_90)$lengths)) %>%
            ungroup() %>%
            as.data.frame() %>%
            select(.data$lake, .data$cv_10, .data$cv_25,
                   .data$cv_75, .data$cv_90) %>%
            unique()
    vals <- melt_and_add_metric(vals, "cv_dur", "cv_")
    summary <- rbind(summary, vals)
  }

  # 5. Rate of Change ----------------------------------------------------------
  rates <- df %>%
           group_by(.data$lake) %>%
           arrange(.data$date) %>%
           mutate(rate01 = lead(.data$level, 1) - .data$level,
                  rate03 = lead(.data$level, 3) - .data$level,
                  rate12 = lead(.data$level, 12) - .data$level) %>%
           ungroup() %>%
           as.data.frame() %>%
           select(.data$lake, .data$date, .data$rate01, .data$rate03,
                  .data$rate12)
  rising <- rates %>%
            mutate(rate01 = ifelse(.data$rate01 > 0, .data$rate01, NA),
                   rate03 = ifelse(.data$rate03 > 0, .data$rate03, NA),
                   rate12 = ifelse(.data$rate12 > 0, .data$rate12, NA))
  falling <- rates %>%
             mutate(rate01 = ifelse(.data$rate01 < 0, .data$rate01, NA),
                    rate03 = ifelse(.data$rate03 < 0, .data$rate03, NA),
                    rate12 = ifelse(.data$rate12 < 0, .data$rate12, NA))

  # 5a. Median rise rates ------------------------------------------------------
  if ("median_rise_rate" %in% metrics) {
    vals <- rising %>%
            group_by(.data$lake) %>%
            summarise(median1 = median(.data$rate01, na.rm = TRUE),
                      median3 = median(.data$rate03, na.rm = TRUE),
                      median12 = median(.data$rate12, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            select(.data$lake, .data$median1, .data$median3, .data$median12)
    vals <- melt_and_add_metric(vals, "median_rise_rate", "median")
    summary <- rbind(summary, vals)
  }

  # 5b. CV rise rates ------------------------------------------------------
  if ("cv_rise_rate" %in% metrics) {
    vals <- rising %>%
            group_by(.data$lake) %>%
            summarise(cv1 = 100*sd(.data$rate01, na.rm = TRUE)/
                            mean(.data$rate01, na.rm = TRUE),
                      cv3 = 100*sd(.data$rate03, na.rm = TRUE)/
                            mean(.data$rate03, na.rm = TRUE),
                      cv12 = 100*sd(.data$rate12, na.rm = TRUE)/
                             mean(.data$rate12, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            select(.data$lake, .data$cv1, .data$cv3, .data$cv12)
    vals <- melt_and_add_metric(vals, "cv_rise_rate", "cv")
    summary <- rbind(summary, vals)
  }

  # 5c. Median fall rates ------------------------------------------------------
  if ("median_fall_rate" %in% metrics) {
    vals <- falling %>%
            group_by(.data$lake) %>%
            summarise(median1 = median(.data$rate01, na.rm = TRUE),
                      median3 = median(.data$rate03, na.rm = TRUE),
                      median12 = median(.data$rate12, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            select(.data$lake, .data$median1, .data$median3, .data$median12)
    vals <- melt_and_add_metric(vals, "median_fall_rate", "median")
    summary <- rbind(summary, vals)
  }

  # 5d. CV fall rates ------------------------------------------------------
  if ("cv_fall_rate" %in% metrics) {
    vals <- falling %>%
            group_by(.data$lake) %>%
            summarise(cv1 = 100*sd(.data$rate01, na.rm = TRUE)/
                            mean(.data$rate01, na.rm = TRUE),
                      cv3 = 100*sd(.data$rate03, na.rm = TRUE)/
                            mean(.data$rate03, na.rm = TRUE),
                      cv12 = 100*sd(.data$rate12, na.rm = TRUE)/
                             mean(.data$rate12, na.rm = TRUE)) %>%
            ungroup() %>%
            as.data.frame() %>%
            select(.data$lake, .data$cv1, .data$cv3, .data$cv12)
    vals <- melt_and_add_metric(vals, "cv_fall_rate", "cv")
    summary <- rbind(summary, vals)
  }

  return(summary)

}

################################################################################
#' Calculate exceedance levels
#'
#' Given a data frame with a "lake" and "level" column, calculates the 10%, 25%,
#' 50%, 75%, and 90% exceedance levels
#'
#' @param df a data frame with a "lake" and a "level" column
#'
#' @return vals, a data frame with the names of all lakes and corresponding 10%,
#'   25%, 50%, 75%, and 90% exceedance levels
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @import dplyr

calculate_exceedances <- function(df) {
  exceeds <- df %>%
             group_by(.data$lake) %>%
             summarise(p10 = quantile(.data$level,
                                      probs = (1 - 0.10),
                                      type = 6,
                                      na.rm = TRUE),
                       p25 = quantile(.data$level,
                                      probs = (1 - 0.25),
                                      type = 6,
                                      na.rm = TRUE),
                       p50 = quantile(.data$level,
                                      probs = (1 - 0.50),
                                      type = 6,
                                      na.rm = TRUE),
                       p75 = quantile(.data$level,
                                      probs = (1 - 0.75),
                                      type = 6,
                                      na.rm = TRUE),
                       p90 = quantile(.data$level,
                                      probs = (1 - 0.90),
                                      type = 6,
                                      na.rm = TRUE)) %>%
             ungroup() %>%
             as.data.frame()
  return(exceeds)
}

################################################################################
#' Calculate exceedance levels
#'
#' Given a data frame with a "lake" and "level" column, calculates the 10%, 25%,
#' 50%, 75%, and 90% exceedance levels
#'
#' @param vals a data frame with a "lake" and a "level" column
#' @param metric_name name of metric to append in a "metric" column. Defaults
#'                    to NULL to not include.
#' @param prefix prefix of melted column names to remove from entries in melted
#'               "variable" column. Defaults to null to keep names as-is.
#'
#' @return vals, a data frame with the columns:
#' \item{lake}{name of the lake}
#' \item{metric}{name of metric passed to function}
#' \item{variable}{name of variations on the metric, columnames in df}
#' \item{value}{value of each metric for each lake}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr mutate select
#' @importFrom reshape2 melt
#' @importFrom stringr str_replace

melt_and_add_metric <- function(vals, metric_name = NULL, prefix = NULL) {
  vals <- melt(vals, id.vars = "lake")
  vals <- vals %>%
          select(.data$lake, .data$variable, .data$value)
  if (!is.null(prefix)) {
    vals <- vals %>%
            mutate(variable = str_replace(.data$variable, prefix, ""))
  }
  if (!is.null(metric_name)) {
    vals <- vals %>%
            mutate(metric = metric_name) %>%
            select(.data$lake, .data$metric, .data$variable, .data$value)
  }
  return(vals)
}
