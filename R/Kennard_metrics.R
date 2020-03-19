#' Calculate a lot of metrics found in appendix to Kennard (2009)
#'
#' Uses the CSLSlevels data frame and calculates metrics related to length,
#' frequency and coeffiecient of variation of extreme lake levels.
#'
#' @param LakeLevels Data frame to use, "csls_levels" by default.
#' @param startDate Earliest date to use in calculations in YYY-MM-DD format.
#'                  Null results in the entire range of dates being used.
#' @param endDate Latest data to use in calculations in YYYY-MM-DD format.
#' @param LakeLevelColumn Column of data frame to use. Default refers to column
#'                        in "csls_levels". Will likely not need to be changed.
#' @param metrics A list of named, logical values corresponding to the metrics
#'                to be used. By default all metrics will be calculated. The
#'                object "which_Kennard_metrics" can be used here to easily
#'                exclude some metrics.
#'
#' @return A list of metric results.
#'
#' @examples
#'   Kennard_metrics()
#'   Kennard_metrics(metrics = c("meanMonth", "hiLevs"))
#'   Kennard_metrics(metrics = which_Kennard_metrics[1:4])
#'
#' @import magrittr
#'
#' @export

Kennard_metrics <- function(LakeLevels = CSLSlevels::csls_levels,
                            startDate = NULL,
                            endDate = NULL,
                            LakeLevelColumn = "level_pred",
                            metrics = CSLSlevels::which_Kennard_metrics) {
  ll <- LakeLevels
  lakes <- unique(ll$lake)

  if (is.null(startDate)) {
    startDate <- min(ll$date)
  } else {
    startDate %<>% as.Date()
  }

  if (is.null(endDate)) {
    endDate <- max(ll$date)
  } else {
    endDate %<>% as.Date()
  }

  ll <- ll[ll$date >= startDate
           & ll$date <= endDate, ]

  ll$month <- substr(ll$date, start = 6, 7) %>% as.numeric()
  ll$year <- substr(ll$date, start = 1, 4) %>% as.numeric()
  ll$usell <- ll[[LakeLevelColumn]]


  # Mean monthly levels
  if ("meanMonth" %in% metrics) {
  meanMonth <- tapply(X = ll$usell,
                      INDEX = list(ll$lake, ll$month),
                      FUN = mean,
                      simplify = TRUE) %>%
               t() %>%
               `rownames<-`(unique(months(ll$date, abbreviate = TRUE)))
  } else {meanMonth <- NA}


  # Coefficient of Variation of monthly levels (expressed as percent)
  if ("cvMonth" %in% metrics) {
  cvMonth <- tapply(X = ll$usell,
                    INDEX = list(ll$lake, ll$month),
                    FUN = function(x) sd(x)*100/mean(x),
                    simplify = TRUE) %>%
              t() %>%
              `rownames<-`(unique(months(ll$date, abbreviate = TRUE)))
  } else {cvMonth <- NA}


  # high levels, just some basic quantiles with type = 6 to get the Weibull distribution
  if ("hiLevs" %in% metrics) {
  hiLevs <- tapply(X = ll$usell,
                   INDEX = ll$lake,
                   FUN = quantile,
                   probs = c(0.99, 0.9, 0.75),
                   type = 6) %>%
            simplify2array() %>%
            `rownames<-`(c("1%", "10%", "25%"))
  } else {hiLevs <- NA}


  # Levels for 2, 5, 10, 20, year ARI
  if ("ARIs" %in% metrics) {
  ARIs <- tapply(X = ll$usell,
                 INDEX = ll$lake,
                 FUN = quantile,
                 probs = 1 - 1/c(2,5,10,15,20),
                 type = 6) %>%
          simplify2array() %>%
          `rownames<-`(paste0(c(2,5,10,15,20), "_yr"))
  } else {ARIs <- NA}

  # this one uses the annual max, like for flood frequency analysis
#  ARIs_ <- tapply(X = ll$usell, INDEX = list(ll$lake, ll$year), FUN = max, simplify = TRUE) %>%
#    t() %>%
#    apply(MARGIN = 2, FUN = quantile, probs = 1 - 1/c(2,5,10,15,20), type = 6) %>%
#    `rownames<-`(c("2yr", "5yr", "10yr", "15yr", "20yr"))


  # Mean number of annual occurences of low or high conditions, plus coefficient
  # of variation of those occurences
  if ("hiloAnn" %in% metrics) {
    hiloAnn <- NULL
    for (lake in lakes) {
      if (is.null(hiloAnn)) {
        hiloAnn <- hi_lo_ann(lake,
                             startDate = startDate,
                             endDate = endDate)[["result"]]
      } else {
        hiloAnn <- cbind(hiloAnn, hi_lo_ann(lake,
                                            startDate = startDate,
                                            endDate = endDate)[["result"]])
      }
    }
  } else {hiloAnn <- NA}


  if ("hiloDur" %in% metrics) {
    hiloDur <- NULL
    for (lake in lakes) {
      hld       <- hi_lo_duration(lake,
                                  startDate = startDate,
                                  endDate = endDate)
      this_lake <- tapply(hld$dur_counts$consecutive_months,
                          INDEX = hld$dur_counts$name,
                          FUN = mean)
      if (is.null(hiloDur)) {
        hiloDur <- as.data.frame(this_lake)
      } else {
        hiloDur <- cbind(hiloDur, this_lake)
      }
    }
    colnames(hiloDur) <- lakes
    rownames(hiloDur) <- (paste0("Dur_", rownames(hiloDur)))
  } else {hiloDur <- NA}


  # Mean duration of low or high conditions in month*meters
  if ("hiloDurMM" %in% metrics) {
    hiloDurMM <- NULL
    for (lake in lakes) {
      hld       <- hi_lo_duration(lake,
                                  startDate = startDate,
                                  endDate = endDate)
      this_lake <- tapply(hld$mmRes$month.meters,
                          INDEX = hld$mmRes$name,
                          FUN = mean)
      if (is.null(hiloDurMM)) {
        hiloDurMM <- as.data.frame(this_lake)
      } else {
        hiloDurMM <- cbind(hiloDurMM, this_lake)
      }
    }
    colnames(hiloDurMM) <- lakes
    rownames(hiloDurMM) <- (paste0("DurMM_", rownames(hiloDurMM)))
  } else {hiloDurMM <- NA}


  # coefficent of variation for duration of low or high conditions
  if ("hiloCV" %in% metrics) {
    hiloCV <- NULL
    for (lake in lakes) {
      hld       <- hi_lo_duration(lake,
                                  startDate = startDate,
                                  endDate = endDate)
      this_lake <- tapply(hld$dur_counts$consecutive_months,
                          INDEX = hld$dur_counts$name,
                          FUN = function(x) sd(x)*100/mean(x))
      if (is.null(hiloCV)) {
        hiloCV <- as.data.frame(this_lake)
      } else {
        hiloCV <- cbind(hiloCV, this_lake)
      }
    }
    colnames(hiloCV) <- lakes
    rownames(hiloCV) <- (paste0("CV_", rownames(hiloCV)))
  } else {hiloCV <- NA}


  # mean rise or fall rate
  if ("meanRate" %in% metrics) {
    meanRate <- as.data.frame(NULL)
    for (m in c(1,2,3,6,12)) {
      meanRate_ <- tapply(X = ll$usell, INDEX = ll$lake,
                          FUN = function(x) {
                            delta <- diff(x, m)
                            rise_fall <- (ifelse(delta > 0, "rise", "fall"))
                            rise_fall[delta == 0] <- NA
                            tapply(delta, rise_fall, mean, simplify = TRUE)},
                          simplify = TRUE) %>%
                   simplify2array() %>%
                   `rownames<-`(paste0(c("mean fall rate_", "mean rise rate_"), m, "m"))
      # rownames(meanRate_) <- paste0(c("mean fall rate_", "mean rise rate_"), m, "m")
      meanRate <- rbind(meanRate, meanRate_)
    }
  } else {meanRate <- NA}


  # coefficient of rise or fall rates
  if ("cvRate" %in% metrics) {
    cvRate <- as.data.frame(NULL)
    for (m in c(1,2,3,6,12)) {
      cvRate_ <- tapply(X = ll$usell, INDEX = ll$lake,
                       FUN = function(x) {
                          delta <- diff(x, m)
                          rise_fall <- (ifelse(delta > 0, "rise", "fall"))
                          rise_fall[delta == 0] <- NA
                          tapply(delta, rise_fall, function (x) 100*sd(x)/mean(x), simplify = TRUE)
                        }, simplify = TRUE) %>%
        simplify2array() %>%
        `rownames<-`(paste0(c("cv fall rate", "cv rise rate"), m, "m"))
      cvRate <- rbind(cvRate, cvRate_)
    }
  } else {cvRate <- NA}

  if ("mn_ann_Range" %in% metrics) {
    ann <- tapply(X = ll$usell,
                  INDEX = list(ll$lake, ll$year),
                  FUN = mean) %>%
           simplify2array() %>%
           t() %>%
           apply(X = ., MARGIN = 2,
                 FUN = quantile,
                 probs = c(0.1, 0.25, 0.75, 0.9),
                 type = 6)

    mnth <- tapply(X = ll$usell, INDEX = ll$lake, simplify = TRUE,
                   FUN = quantile,
                   probs = c(0.1, 0.25, 0.75, 0.90), type = 6) %>%
            simplify2array()


    mn_ann_Range <- NULL
    for (lk in lakes) {
      this_lake <- as.data.frame(c(ann["90%", lk] - ann["10%", lk],
                                   mnth["90%", lk] - mnth["10%", lk],
                                   ann["75%", lk] - ann["25%", lk],
                                   mnth["75%", lk] - mnth["25%", lk]))
      rownames(this_lake) <- c("ann9010", "mnth9010", "ann7525", "mnth7525")
      colnames(this_lake) <- lk
      if (is.null(mn_ann_Range)) {
        mn_ann_Range <- this_lake
      } else {
        mn_ann_Range <- cbind(mn_ann_Range, this_lake)
      }
    }
  } else {mn_ann_Range <- NA}

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
    "CV of Rate of Rise or Fall" = cvRate,
    "Monthly and Annual Ranges" = mn_ann_Range
  ))

}
