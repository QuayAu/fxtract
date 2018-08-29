#' Helper function. Checks if character has correct time format 'hh:mm:ss'.
#' @param x character vector.
checkTimeFormat = function(x) {
  for (i in 1:length(x)) {
    if (as.numeric(substr(x[i], 1, 2)) >= 24) stop("hours cannot exceed 23.")
    if (!grepl(pattern = "[012][[:digit:]]:[012345][[:digit:]]:[012345][[:digit:]]", x = x[i])) stop("character must be a time format like '15:21:30'")
  }
}

#' Helper function. Converts "hh:mm:ss" into number of seconds.
#' @param x character vector.
#' @return numeric vector. Number of seconds.
#' @export
timeToSec = function(x) {
  checkTimeFormat(x)
  y = strsplit(x, ":")
  y = lapply(y, as.numeric)
  f = function(z) 3600*z[1] + 60*z[2] + z[3]
  res = unlist(lapply(y, f))
  return(res)
}

#' Adds column 'weekday' to a dataframe containing a timestamp variable.
#'
#' @template param_data_ts
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
  checkmate::assertNames(names(data), must.include = "timestamp")
  checkmate::assertSubset(unit, c("s", "ms"))
  if (is.character(data$timestamp)) {
    data$timestamp = as.numeric(data$timestamp)
    message("timestamp was converted from character to numeric")
  }
  if (locale != "English_United States.1252") message("locale was changed. Use at own risk.")


  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  if ("weekday" %in% names(data)) warning("Your dataset already has a column named 'weekday'. It will be overwritten!")
  data$weekday = lubridate::wday(dt, label = TRUE, week_start = week_start, locale = locale)
  return(data)
}

#' Adds column 'time' to a dataframe containing a timestamp variable.
#'
#' @template param_data_ts
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
  checkmate::assertNames(names(data), must.include = "timestamp")
  checkmate::assertSubset(unit, c("s", "ms"))
  if (is.character(data$timestamp)) {
    data$timestamp = as.numeric(data$timestamp)
    message("timestamp was converted from character to numeric")
  }
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  if ("time" %in% names(data)) warning("Your dataset already has a column named 'time'. It will be overwritten!")
  data$time = strftime(dt, "%H:%M:%S", tz = tz)
  return(data)
}

#' Adds column 'date' to a dataframe containing a timestamp variable.
#'
#' @template param_data_ts
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
  checkmate::assertNames(names(data), must.include = "timestamp")
  checkmate::assertSubset(unit, c("s", "ms"))
  if (is.character(data$timestamp)) {
    data$timestamp = as.numeric(data$timestamp)
    message("timestamp was converted from character to numeric")
  }
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  if ("date" %in% names(data)) warning("Your dataset already has a column named 'date'. It will be overwritten!")
  data$date = lubridate::as_date(dt)
  return(data)
}

#' Adds column 'date_time' to a dataframe containing a timestamp variable.
#'
#' @template param_data_ts
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
  checkmate::assertNames(names(data), must.include = "timestamp")
  checkmate::assertSubset(unit, c("s", "ms"))
  if (is.character(data$timestamp)) {
    data$timestamp = as.numeric(data$timestamp)
    message("timestamp was converted from character to numeric")
  }
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  if ("date_time" %in% names(data)) warning("Your dataset already has a column named 'date_time'. It will be overwritten!")
  data$date_time = dt
  return(data)
}
