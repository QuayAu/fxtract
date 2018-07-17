checkCols = function(columnnames, loggingdata) {
  ld = colnames(loggingdata)
  for (i in 1:length(columnnames)) if (!any(ld %in% columnnames[i]))
    stop(paste0("Your data set needs a column named '", columnnames[i], "'"))
}

#' Adds column 'weekday' to a dataframe containing a timestamp variable.
#'
#' @template param_data
#' @template param_tz
#' @template param_unit
#' @param week_start day on which week starts following ISO conventions - 1 means Monday, 7 means Sunday (default). See \code{\link[lubridate]{wday}}.
#' @param locale locale to use for day names. Defaults to 'English_United States.1252'. See \code{\link[lubridate]{wday}}.
#' @family helper functions
#' @return dataframe with added ordered factor variable 'weekday'.
#' @importFrom lubridate wday as_datetime
#' @export
addWeekday = function(data, tz = "UTC", unit = "s", week_start = 1, locale = "English_United States.1252") {
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(tz)
  checkmate::assertCharacter(unit)
  checkmate::assertCharacter(locale)
  checkmate::assertNumeric(week_start)
  checkCols("timestamp", data)
  if (!unit %in% c("s", "ms")) stop("unit must be 's' or 'ms")
  if (is.character(data$timestamp)) {
    data$timestamp = as.numeric(data$timestamp)
    message("timestamp was converted from character to numeric")
  }
  if (locale != "English_United States.1252") message("locale was changed. Use at own risk.")


  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  data$weekday = lubridate::wday(dt, label = TRUE, week_start = week_start, locale = locale)
  return(data)
}

#' Adds column 'time' to a dataframe containing a timestamp variable.
#'
#' @template param_data
#' @template param_tz
#' @template param_unit
#' @family helper functions
#' @return dataframe with added variable 'time'.
#' @importFrom lubridate as_datetime
#' @export
addTime = function(data, tz = "UTC", unit = "s") {
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(tz)
  checkmate::assertCharacter(unit)
  checkCols("timestamp", data)
  if (!unit %in% c("s", "ms")) stop("unit must be 's' or 'ms")
  if (is.character(data$timestamp)) {
    data$timestamp = as.numeric(data$timestamp)
    message("timestamp was converted from character to numeric")
  }
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  data$time = strftime(dt, "%H:%M:%S", tz = tz)
  return(data)
}

#' Adds column 'date' to a dataframe containing a timestamp variable.
#'
#' @template param_data
#' @template param_tz
#' @template param_unit
#' @family helper functions
#' @return dataframe with added variable 'date'.
#' @importFrom lubridate as_datetime as_date
#' @export
addDate = function(data, tz = "UTC", unit = "s") {
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(tz)
  checkmate::assertCharacter(unit)
  checkCols("timestamp", data)
  if (!unit %in% c("s", "ms")) stop("unit must be 's' or 'ms")
  if (is.character(data$timestamp)) {
    data$timestamp = as.numeric(data$timestamp)
    message("timestamp was converted from character to numeric")
  }
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  data$date = lubridate::as_date(dt)
  return(data)
}

#' Adds column 'date_time' to a dataframe containing a timestamp variable.
#'
#' @template param_data
#' @template param_tz
#' @template param_unit
#' @family helper functions
#' @return dataframe with added variable 'date_time'.
#' @importFrom lubridate as_datetime
#' @export
addDateTime = function(data, tz = "UTC", unit = "s") {
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(tz)
  checkmate::assertCharacter(unit)
  checkCols("timestamp", data)
  if (!unit %in% c("s", "ms")) stop("unit must be 's' or 'ms")
  if (is.character(data$timestamp)) {
    data$timestamp = as.numeric(data$timestamp)
    message("timestamp was converted from character to numeric")
  }
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  data$date_time = dt
  return(data)
}


#' Helper function. Takes the mininum of timestamp as first day and labels oncoming days with the time difference to this day.
#'
#' @template param_data
#' @family helper functions
#' @return dataframe with added ordered factor variable 'studyDay'. The first day for will be 'day1'.
#' @importFrom dplyr slice pull
#' @importFrom magrittr "%>%"
#' @export
calcStudyDay = function(data) {
  checkmate::assertDataFrame(data)
  if (!"date" %in% colnames(data)) stop("data needs a column named 'date'. Consider adding 'date' by using addDate().")
  timestamp = date = NULL

  alldays = data$date
  daysId = character(length(alldays))
  uniquedays = unique(alldays)
  day1 = data %>% slice(which.min(timestamp)) %>% pull(date)

  daysId[which(alldays == day1)] = "day1"
  j = which(uniquedays == day1)
  diffToDay1 = difftime(uniquedays[-j], day1, units = "days")
  for (i in 1:length(diffToDay1)) {
    if (diffToDay1[i] > 0) daysId[which(alldays == uniquedays[-j][i])] = paste0("day", diffToDay1[i] + 1)
  }
  daysId
}
