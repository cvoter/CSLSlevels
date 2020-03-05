#' Calculate duration of extreme lake levels
#'
#' Calculate how long a lake remains above or below, given either exceedance
#' probabilities or elevations in meters above sea level.
#'
#' @param lakeName Name of the lake of interest without "lake", e.g.
#'                 "Plainfield", "Long", or "Pleasant"
#' @param exceeds The exceedance probabilities of interest, as a vector of
#'                values between 0 and 1
#' @param elevs The elevations of interest. If this argument is supplied,
#'              "exceeds" is ignored.
#' @param pred_colname The column of CSLSlevels::csls_levels to look for
#'                     predicted lake levels (in case that changes)
#' @param date_colname Same as above, but for date
#' @param lake_colname Same as above, but for lake
#' @param startDate Enter a different start date for the analysis, or leave
#'                  blank to use the entire sequence of predicted lake levels
#'                  (1905- 2019)
#' @param endDate Enter a different end date for the analysis, or leave blank to
#'                use the entire sequence of predicted lake levels (1905- 2019)
#' @return a list of the following:
#' \item{ll_hi_lo}{The lake level predictions and dates used in the calculation,
#'                 with extra columns added for each exceedance or elevation of
#'                 interest. One column with a 1 for every monthly prediciton
#'                 more extreme than the exceedance or elevation of interest,
#'                 and one with number of meters above or below the exceedance
#'                 or elevation of interest}
#' \item{dur_counts}{A data frame with a row for every sequence of months above
#'                   or below the exceedance or elevation of interest}
#' \item{mmRes}{The same as above except with cumulative meters above or below
#'              the exceedance or elevation of interest for consecutive months}
#' \item{levels}{The median predicted level and the elevations of interest used
#'               to perform the calculations.}
#'
#' @import magrittr
#' @importFrom stats quantile
#'
#' @export

hi_lo_duration <- function(lakeName,
                           exceeds = c(0.1, 0.25, 0.75, 0.9),
                           elevs = NULL,
                           # plot = FALSE,
                           pred_colname = "level_pred",
                           date_colname = "date",
                           lake_colname = "lake",
                           startDate = "1905-01-01",
                           endDate = "2019-12-01") {

  # shorter name for typing
  ll <- CSLSlevels::csls_levels

  ll$date %<>% as.Date()
  # filter by date, only use a few columns
  ll <- ll[ll[[date_colname]] >= as.Date(startDate)
           & ll[[date_colname]] <= as.Date(endDate)
           & ll[[lake_colname]] == lakeName, c(lake_colname,
                                         date_colname,
                                         pred_colname)]

  # month.meters result - starter dataframe
  mmRes <- data.frame(month.meters = numeric(0),
                      ex = character(0))
  # consecutive months result - starter data frame
  r <- data.frame(consecutive_months = numeric(0),
                  values = numeric(0),
                  ex = character(0),
                  name = character(0))
  median <- median(ll[[pred_colname]], na.rm = TRUE)

  # so that users can enter exceedances in function argument instead of probabilities
  probs <- 1 - exceeds

  if (is.null(elevs)) {
    expText <- "higher (for p >= 0.5) or lower (for p < 0.5) than the exceedance probability"
    # get elevations corresponding to the exceedances of interest
    # gotta include type = 6 to get the Weibull distribution up in here
    c.elevs <- quantile(x = ll[[pred_colname]], probs = probs, type = 6, names = FALSE)
    names(c.elevs) <- paste0((1-probs)*100, "%")
    ab_cutoff <- 0.5
  } else {
    # if user entered elevations instead, those are the elevations of interest
    c.elevs <- elevs
    names(c.elevs) <- c.elevs
    expText <- "higher (for elev. >= median) or lower (for elev. < median) than the elevation"
    ab_cutoff <- median
  }

    # for each elevation of interest, create a new column with 1s and 0s for if the predicted lake level is more
    # extreme than the elevation of interest
    for (i in 1:length(c.elevs)) {
      if (c.elevs[i] > median) {
        ll[[names(c.elevs)[i]]] <- ifelse(ll[[pred_colname]] >= c.elevs[i], 1, 0)
      } else {
        ll[[names(c.elevs)[i]]] <- ifelse(ll[[pred_colname]] < c.elevs[i], 1, 0)
      }

    # and create a new column with "mm" in front
    # if the predicted lake level is more extreme than the elevation of interest, calculate the difference, otherwise, 0
      ll[[paste0("mm", names(c.elevs)[i])]] <- ifelse(ll[[names(c.elevs)[i]]] == 1, ll[[pred_colname]] - c.elevs[i], 0)
      # capture the indices of the predictions that were more extreme than the elevation of interest so they can be summed
      # to get meter*months
      inds <- which(ll[[paste0("mm", names(c.elevs)[i])]] != 0)

      # not sure if this is working as intended - need to skip to next elevation... but what to execute before??
      if (length(inds) == 0) {
        next
      } else {
      # data frame created to help identify consecutive months where lake level is more extreme than level of interest
      mm <- data.frame(b = c(99, diff(inds, 1)),
                       inds = inds,
                       a = c(diff(inds, 1), 99))
      }

      # for each row, check if it's the beginning or end of a series of consecutive months (is the difference between
      # indices where the predicted level is more extreme than the level of interest not 1?)
      for (g in 1:length(inds)) {
        if (mm$b[g] != 1) {
          # if column b is not 1, then this is the start of series of months for month*meters to be summed
          st <- mm$inds[g]
        }

        if (mm$a[g] != 1) {
          # if column a is not one, this is the end of series of months for month*meters to be summed,
          # sum the month meters from the start to the end indices and add it to the final data frame
          end <- mm$inds[g]
          mmRes_ <- data.frame(month.meters = sum(ll[[paste0("mm", names(c.elevs)[i])]][st:end]),
                               ex = probs[i],
                               name = names(c.elevs)[i])
          mmRes <- rbind(mmRes, mmRes_)
        }
      }


      # use rle (run length encoding) to find how many 1s in a row (months where predicted level was more extreme than
      # level of interest)

      r_ <- data.frame(consecutive_months = rle(ll[[names(c.elevs)[i]]])$lengths,
                       values = rle(ll[[names(c.elevs)[i]]])$values,
                       ex = ifelse(is.null(elevs), probs[i], c.elevs[i]),
                       name = names(c.elevs[i]))
      # only interested in run lengths of 1
      r_ <- r_[r_$values == 1, ]

      r <- rbind(r, r_)
    }

# commented out a bunch of plotting options
#  if (mode == "month.meters") {
#    plot.object <- data.frame(x = mmRes$ex,
#                              y = mmRes$month.meters,
#                              name = mmRes$name, stringsAsFactors = FALSE)
#  } else {
#    plot.object <- data.frame(x = r$ex,
#                              y = r$consecutive_months,
#                              name = r$name, stringsAsFactors = FALSE)
#  }

 # if (isFALSE(plot)) {
    levels <- list(median, unlist(c.elevs))
    names(levels) <- c("median", "thresholds")

    return(list(ll_hi_lo = ll,
                dur_counts = r[ , c("consecutive_months", "ex", "name")],
                mmRes = mmRes,
                levels = levels,
                startDate = startDate,
                endDate = endDate)
    )

# } else if (plot == "xy") {
#    if (is.null(elevs)) {
#      xaxp = c(0.0, 1.0, 10)
#    } else {
#      xaxp = c(floor(min(c.elevs)),
#               ceiling(max(c.elevs)),
#               2*(ceiling(max(c.elevs)) - floor(min(c.elevs))))
#    }

#    plot(x = plot.object$x, y = plot.object$y,
#         main = lakeName,
#         xlab = strwrap(paste("Each dot is a span of months during which the lake was",expText,"on the x-axis"), width = 80),
#         ylab = "",
#         xaxp = xaxp)
#  } else if (plot == "boxplot") {
#    plot(x = as.factor(plot.object$x), y = plot.object$y,
#         xlab = strwrap(paste("Each dot is a span of months during which the lake was",expText,"on the x-axis"), width = 80))
#  } else if (plot == "histogram") {
#    for (e in names(c.elevs)) {
#      hist(x = plot.object$y[plot.object$name == e], breaks = length(plot.object$y[plot.object$name == e])/3)
#    }
#  }
}
