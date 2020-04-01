#' Create data frame with equal ranges for all groups
#'
#' Given a data frame with values corresponding to different groups, defines a
#' data frame "range" with min and max values for each group such that the real
#' data can be centered around the midpoint but all subplots cover the same
#' range (1 unit on plot A = 1 unit on plot B).
#'
#' @param df a data frame with columns for the group and corresponding values
#' @param group_col name of the group column
#' @param value_col name of the values column
#' @param tick_precision round range to nearest X, inclusive (e.g., 0.1, 0.5, etc.)
#' @param pfl_is_long logical defaults to FALSE to maintain separate ranges for
#'                     Long and Plainfield lakes. If TRUE, forces plainfield to
#'                     use range for long.
#'
#' @return range, a data frame with a column for "group" and another column for
#'         "value" (real names: group_col and value_col) with min and max values
#'         for each group.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom reshape2 melt
#' @import dplyr
#'
#' @export

specify_range <- function(df,
                          group_col,
                          value_col,
                          tick_precision = NA,
                          pfl_is_long = FALSE) {
  # Rename value column
  colnames(df)[which(colnames(df) == group_col)] <- "group"
  colnames(df)[which(colnames(df) == value_col)] <- "value"

  # Calculate initial range
  range     <- df %>%
               group_by(.data$group) %>%
               summarize(lower = min(.data$value),
                         upper = max(.data$value)) %>%
               mutate(midpoint = .data$lower + (.data$upper -.data$lower)/2) %>%
               ungroup() %>%
               mutate(range = .data$upper - .data$lower)

  # identify max range (round if desired)
  if (is.na(tick_precision)) {
    max_range <- max(range$range)
  } else {
    max_range <- ceiling((1/tick_precision)*max(range$range))/(1/tick_precision)
  }

  # Force all to max range
  new_range <- range %>% select(.data$group)
  new_range$lower <- range$midpoint - max_range/2
  new_range$upper <- range$midpoint + max_range/2
  range           <- melt(new_range, id.vars = "group")
  range           <- range %>% select(.data$group, .data$value)

  # Force pfl range to be same as long, if desired
  if (pfl_is_long) {
    new_pfl       <- range[which(range$group == "Long"),]
    new_pfl$group <- "Plainfield"
    range         <- range[-which(range$group == "Plainfield"),]
    range         <- rbind(range, new_pfl)
  }

  # Rename value column
  colnames(range) <- c(group_col, value_col)

  return(range)
}
