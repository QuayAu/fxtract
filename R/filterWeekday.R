#' Filter function. Filters the data by weekdays. The dataset can e.g. be filtered by all days between monday and friday.
#' The dataset needs an ordered factor variable named 'weekday'.
#'
#' @template param_data
#' @param from_day character. Day of the week where filtering should start. Please check the machines locale for correct usage. Defaults to "Mon".
#' @param from_time character. Time where filtering should start (on the day from_day). Must be a string with the format "hh:mm:ss". Defaults to "00:00:00".
#' @param until_day character. Day of the week where filtering should end. Please check the machines locale for correct usage. Defaults to "Sun".
#' @param until_time character. Time where filtering should end (on the day until_day). Must be a string with the format "hh:mm:ss". Defaults to "23:59:59".
#' @family filter functions
#' @return filtered dataframe by weekday and time.
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @export
filterWeekday = function(data, from_day = "Mon", from_time = "00:00:00", until_day = "Sun", until_time = "23:59:59") {
  weekday = time_in_sec = NULL
  # check inputs
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(from_day)
  checkmate::assertCharacter(from_time)
  checkmate::assertCharacter(until_day)
  checkmate::assertCharacter(until_time)
  checkTimeFormat(from_time)
  checkTimeFormat(until_time)

  # check data
  lw = levels(data$weekday)
  if (!"weekday" %in% colnames(data)) stop("Your data set needs a column named 'weekday'. See function addWeekday().")
  if (!"time" %in% colnames(data)) stop("Your data set needs a column named 'time'. See function addTime().")
  if (!is.ordered(data$weekday)) stop("The variable 'weekday' must be an ordered factor, e.g. Levels: Mon < Tue < Wed < Thu < Fri < Sat < Sun")
  if (length(lw) != 7) warning("The variable 'weekday' does not have 7 levels. Please check your data!")

  # convert from_day and until_day into ordered factors
  from_day = factor(from_day, levels = lw, ordered = TRUE)
  until_day = factor(until_day, levels = lw, ordered = TRUE)

  # filter dataset according to from_day and until_day
  if (from_day <= until_day) {
    df_res = data %>% dplyr::filter(weekday >= from_day, weekday <= until_day)
  } else {
    df_res = data %>% dplyr::filter(weekday >= from_day | weekday <= until_day)
  }

  # convert timestamp character string input and time variable of dataset into numeric (in seconds) starting from time 00:00:00
  ft = timeToSec(from_time)
  ut = timeToSec(until_time)
  df_res$time_in_sec = timeToSec(df_res$time)

  # filter dataset according to from_time and until_time
  df_res = df_res %>% dplyr::filter(time_in_sec >= ft | weekday != from_day, time_in_sec <= ut | weekday != until_day) %>% dplyr::select(-time_in_sec)

  if (nrow(df_res) == 0) stop("there are no dataset entries within the chosen time interval")

  return(df_res)
}


