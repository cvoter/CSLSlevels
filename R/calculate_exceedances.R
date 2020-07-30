#' Calculate exceedance levels
#'
#' Given a data frame with a "lake" and "level" column, calculates levels
#' associated with desired exceedance probabilities.
#'
#' @param df a data frame with a "lake" and a "level" column
#' @param probs a vector with all exceedance probabilities to calculate.
#'              Defaults to c(10, 25, 50, 75, 90)
#' @param melted logical defaults to true to indicate should melt data frame to
#'               just 3 columns (lake, variable, value). Otherwise, keeps as
#'               data frame with one column per exceedance probability.
#' @departures optional vector with departures from median to evaluate
#'             probability of occurrence. Defaults to NULL to trigger exceedance
#'             probability calculations instead.
#' @return exceeds, a data frame with the names of all lakes and corresponding
#'         exceedance levels
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom reshape2 melt
#' @import dplyr
#'
#' @export

calculate_exceedances <- function(df,
                                  probs = c(10, 25, 50, 75, 90),
                                  departures = NULL,
                                  melted = TRUE) {
  exceeds <- data.frame(lake = levels(df$lake))
  if (is.null(departures)) {
    for (prob in probs) {
      prob_name <- sprintf("%d", prob)
      exceed    <- df %>%
                   group_by(.data$lake) %>%
                   summarise(p = quantile(.data$level,
                                          probs = (1 - prob/100),
                                          type = 6,
                                          na.rm = TRUE)) %>%
                   ungroup() %>%
                   as.data.frame()
      colnames(exceed) <- c("lake", prob_name)
      exceeds <- merge(exceeds, exceed)
    }
  } else {
    medians     <- df %>%
                   group_by(.data$lake) %>%
                   summarise(median = quantile(.data$level,
                                               probs = (0.5),
                                               type = 6,
                                               na.rm = TRUE)) %>%
                   ungroup() %>%
                   as.data.frame()
    for (depart in departures) {
      depart_name <- sprintf("%.1fm", depart)
      exceed      <- df %>%
                     left_join(medians, by = "lake") %>%
                     group_by(.data$lake) %>%
                     summarise(new_level = mean(.data$median) + depart,
                               p = ecdf(.data$level)(.data$new_level)) %>%
                     ungroup() %>%
                     as.data.frame() %>%
                     select(.data$lake, .data$p)
      exceed$p    <- abs(exceed$p - 0.5)*100
      colnames(exceed) <- c("lake", depart_name)
      exceeds  <- merge(exceeds, exceed)
    }
  }

  if (melted) {
    exceeds <- melt(exceeds, id.vars = "lake")
  }
  exceeds$lake <- factor(exceeds$lake, levels = levels(df$lake))

  return(exceeds)
}
