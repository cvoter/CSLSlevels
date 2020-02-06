#' Calculate a lot of metrics found in appendix to Kennard (2009)
#'
#' Uses the CSLSlevels data frame and calculates metrics related to length,
#' frequency and coeffiecient of variation of extreme lake levels.
#'
#' @param LakeLevels Data frame to use, "csls_levels" by default.
#' @param startDate Earliest date to use in calculations in YYY-MM-DD format. Null results in
#' the entire range of dates being used.
#' @param endDate Latest data to use in calculations in YYYY-MM-DD format.
#' @param LakeLevelColumn Column of data frame to use. Default refers to column in
#' "csls_levels". Will likely not need to be changed.
#' @param metrics A list of named, logical values corresponding to the metrics to be used. By default all metrics will be calculated.
#' The object "which_Kennard_metrics" can be used here to easily exclude some metrics.
#'
#' @return A list of metric results.
#'
#' @examples Kennard_metrics()
#' Kennard_metrics(metrics = c("meanMonth", "hiLevs"))
#' Kennard_metrics(metrics = which_Kennard_metrics[1:4])
#'
#' @import magrittr
#'
#' @export

Kennard_metrics <- function(LakeLevels = csls_levels,
                            startDate = NULL,
                            endDate = NULL,
                            LakeLevelColumn = "level_pred",
                            metrics = which_Kennard_metrics) {

  ll <- LakeLevels

  if (is.null(startDate)) {
    startDate <- min(ll$date)
  }

  if (is.null(endDate)) {
    endDate <- max(ll$date)
  }

  ll <- ll[ll$date >= as.Date(startDate)
           & ll$date <= as.Date(endDate), ]

  ll$month <- substr(ll$date, start = 6, 7) %>% as.numeric()
  ll$year <- substr(ll$date, start = 1, 4) %>% as.numeric()
  ll$usell <- ll[[LakeLevelColumn]]


  # Mean monthly levels
  if ("meanMonth" %in% metrics) {
  meanMonth <- tapply(X = ll$usell, INDEX = list(ll$lake, ll$month), FUN = mean, simplify = TRUE) %>%
    t() %>%
    `rownames<-`(unique(months(ll$date, abbreviate = TRUE)))
  } else {meanMonth <- NA}


  # Coefficient of Variation of monthly levels (expressed as percent)
  if ("cvMonth" %in% metrics) {
  cvMonth <- tapply(X = ll$usell, INDEX = list(ll$lake, ll$month), FUN = function(x) sd(x)*100/mean(x), simplify = TRUE) %>%
    t() %>%
    `rownames<-`(unique(months(ll$date, abbreviate = TRUE)))
  } else {cvMonth <- NA}


  # high levels, just some basic quantiles with type = 6 to get the Weibull distribution
  if ("hiLevs" %in% metrics) {
  hiLevs <- tapply(X = ll$usell, INDEX = ll$lake, FUN = quantile,
               probs = c(0.99, 0.9, 0.75), type = 6) %>%
    simplify2array() %>%
    `rownames<-`(c("1%", "10%", "25%"))
  } else {hiLevs <- NA}


  # Levels for 2, 5, 10, 20, year ARI
  if ("ARIs" %in% metrics) {
  ARIs <- tapply(X = ll$usell, INDEX = ll$lake, FUN = quantile,
                 probs = 1 - 1/c(2,5,10,15,20), type = 6) %>%
    simplify2array() %>%
    `rownames<-`(paste0(c(2,5,10,15,20), "_yr"))
  } else {ARIs <- NA}

  # this one uses the annual max, like for flood frequency analysis
#  ARIs_ <- tapply(X = ll$usell, INDEX = list(ll$lake, ll$year), FUN = max, simplify = TRUE) %>%
#    t() %>%
#    apply(MARGIN = 2, FUN = quantile, probs = 1 - 1/c(2,5,10,15,20), type = 6) %>%
#    `rownames<-`(c("2yr", "5yr", "10yr", "15yr", "20yr"))


  # Mean number of annual occurences of low or high conditions, plus coefficient of variation of those occurences
  if ("hiloAnn" %in% metrics) {
  hiloAnn <- data.frame(
    Plainfield = hi_lo_ann("Plainfield",
                           startDate = startDate,
                           endDate = endDate)[["result"]],
    Long = hi_lo_ann("Long",
                     startDate = startDate,
                     endDate = endDate)[["result"]],
    Pleasant = hi_lo_ann("Pleasant",
                         startDate = startDate,
                         endDate = endDate)[["result"]]
  )
  } else {hiloAnn <- NA}


  # Mean duration of low or high conditions in months
  if ("hiloDur" %in% metrics) {
    hiloDur <- data.frame(
      Plainfield = tapply(hi_lo_duration("Plainfield")$dur_counts$consecutive_months,
                          INDEX = hi_lo_duration("Plainfield")$dur_counts$name,
                          FUN = mean),
      Long = tapply(hi_lo_duration("Long")$dur_counts$consecutive_months,
                    INDEX = hi_lo_duration("Long")$dur_counts$name,
                    FUN = mean),
      Pleasant = tapply(hi_lo_duration("Pleasant")$dur_counts$consecutive_months,
                        INDEX = hi_lo_duration("Pleasant")$dur_counts$name,
                        FUN = mean)
    ) %>%
      `rownames<-`(paste0("Dur_", rownames(.)))
  } else {hiloDur <- NA}


  # Mean duration of low or high conditions in month*meters
  if ("hiloDurMM" %in% metrics) {
  hiloDurMM <- data.frame(
    Plainfield = tapply(hi_lo_duration("Plainfield")$mmRes$month.meters,
                        INDEX = hi_lo_duration("Plainfield")$mmRes$name,
                        FUN = mean),
    Long = tapply(hi_lo_duration("Long")$mmRes$month.meters,
                  INDEX = hi_lo_duration("Long")$mmRes$name,
                  FUN = mean),
    Pleasant = tapply(hi_lo_duration("Pleasant")$mmRes$month.meters,
                      INDEX = hi_lo_duration("Pleasant")$mmRes$name,
                      FUN = mean)
  ) %>%
    `rownames<-`(paste0("DurMM_", rownames(.)))
  } else {hiloDurMM <- NA}


  # coefficent of variation for duration of low or high conditions
  if ("hiloCV" %in% metrics) {
  hiloCV <- data.frame(
    Plainfield = tapply(hi_lo_duration("Plainfield")$mmRes$month.meters,
                        INDEX = hi_lo_duration("Plainfield")$mmRes$name,
                        FUN = function(x) sd(x)*100/mean(x)),
    Long = tapply(hi_lo_duration("Long")$mmRes$month.meters,
                  INDEX = hi_lo_duration("Long")$mmRes$name,
                  FUN = function(x) sd(x)*100/mean(x)),
    Pleasant = tapply(hi_lo_duration("Pleasant")$mmRes$month.meters,
                      INDEX = hi_lo_duration("Pleasant")$mmRes$name,
                      FUN = function(x) sd(x)*100/mean(x))
  ) %>%
    `rownames<-`(paste0("CV_", rownames(.)))
  } else {hiloCV <- NA}


  # mean rise or fall rate
  if ("meanRate" %in% metrics) {
  meanRate <- tapply(X = ll$usell, INDEX = ll$lake,
                      FUN = function(x) {
                        delta <- diff(x, 1)
                        rise_fall <- (ifelse(delta > 0, "rise", "fall"))
                        rise_fall[delta == 0] <- NA
                        tapply(delta, rise_fall, mean, simplify = TRUE)
                      }, simplify = TRUE) %>%
    simplify2array() %>%
    `rownames<-`(c("mean fall rate", "mean rise rate"))
  } else {meanRate <- NA}


  # coefficient of rise or fall rates
  if ("cvRate" %in% metrics) {
  cvRate <- tapply(X = ll$usell, INDEX = ll$lake,
                      FUN = function(x) {
                        delta <- diff(x, 1)
                        rise_fall <- (ifelse(delta > 0, "rise", "fall"))
                        rise_fall[delta == 0] <- NA
                        tapply(delta, rise_fall, function (x) 100*sd(x)/mean(x), simplify = TRUE)
                      }, simplify = TRUE) %>%
    simplify2array() %>%
    `rownames<-`(c("cv fall rate", "cv rise rate"))
  } else {cvRate <- NA}


  return(list(
    "Mean Monthly Levels" = meanMonth,
    "CV of Monthly Levels" = cvMonth,
    "High Levels" = hiLevs,
    "Annual Recurrance Intervals" = ARIs,
    "Annual Occurence of High or Low Levels" = hiloAnn,
    "Mean Dur. of High or Low Levels (months)" = hiloDur,
    "Mean Dur. of High or Low Levels (month*meters)" = hiloDurMM,
    "CV of length of High or Low Levels" = hiloCV,
    "Mean Rate of Rise or Fall" = meanRate,
    "CV of Rate of Rise or Fall" = cvRate
  ))

}
