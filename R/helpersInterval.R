
#' Helper function. Adds a new column, which divides the dataset into intervals of a given length.
#' The base R function \code{cut} is used with \code{include.lowest = TRUE}.
#' Values larger than the biggest cut point will be assigned a new interval.
#' @template param_data
#' @template param_interval_steps
#' @template param_interval_time_in_sec
#' @template param_unit
#' @family helper functions
#' @return character. This character divides the dataset into intervals.
#' @export
divideDataIntoIntervals = function(data, steps, time_in_sec, unit = "s") {
  if (!xor(missing(steps), missing(time_in_sec)))
    stop("Pass either steps or time_in_sec, but not both!")
  if (missing(steps)) checkmate::assert_numeric(time_in_sec)
  if (missing(time_in_sec)) checkmate::assert_integerish(steps)
  checkmate::assertDataFrame(data)
  checkmate::assertNames(names(data), must.include = c("timestamp"))
  checkmate::assertSubset(unit, c("s", "ms"))

  if (missing(steps)) {
    timestamp = data$timestamp
    if (anyNA(timestamp)) stop("your dataset contains NA in the timestamp variable")

    start_time = min(timestamp)
    end_time = max(timestamp)
    x = ifelse(unit == "s", 1, 1000)

    breaks = seq(start_time, end_time, by = time_in_sec * x)
    iv = cut(timestamp, breaks = c(breaks, max(breaks) + time_in_sec * x), include.lowest = TRUE)
    # if (anyNA(iv)) stop("after cutting the timestamp variable, NA's occured.") #this should not happen! just for caution.
    iv = paste0("interval", as.numeric(iv))
    return(iv)
  } else {
    breaks = seq(1, nrow(data), by = steps)
    iv = cut(1:nrow(data), breaks = c(breaks, max(breaks) + steps), include.lowest = TRUE)
    # if (anyNA(iv)) stop("after cutting the datasets, NA's occured.") #this should not happen! just for caution.
    iv = paste0("interval", as.numeric(iv))
    return(iv)
  }
}
