#' Filter function. Filters the data by weekdays. The dataset can e.g. be filtered by all days between monday and friday.
#' The dataset needs an ordered factor variable named 'weekday'.
#'
#' @template param_data_uid
#' @param from_time character. Time where filtering should start (on the day from_day). Must be a string with the format "hh:mm:ss". Defaults to "00:00:00".
#' @param until_time character. Time where filtering should end (on the day until_day). Must be a string with the format "hh:mm:ss". Defaults to "23:59:59".
#' @family filter functions
#' @return The dataset filtered according to the given time constraints.
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @export
filterDaytime = function(data, from_time = "07:00:00", until_time = "18:00:00") {
  time_in_sec = NULL
  # check inputs
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(from_time)
  checkmate::assertCharacter(until_time)
  checkTimeFormat(from_time)
  checkTimeFormat(until_time)
  
  if (!"time" %in% colnames(data)) stop("Your data set needs a column named 'time'. See function addTime().")
  
  # convert timestamp character string input and time variable of dataset into numeric (in seconds) starting from time 00:00:00
  ft = timeToSec(from_time)
  ut = timeToSec(until_time)
  
  res = data
  res$time_in_sec = timeToSec(res$time)

  if (ft <= ut) {
    res = res %>% filter(time_in_sec >= ft & time_in_sec <= ut)
  } else {
    res = res %>% filter(time_in_sec >= ft | time_in_sec <= ut)
  }
  res
}
