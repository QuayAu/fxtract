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


#' Helper function. Takes the mininum of timestamp as first day and labels oncoming days with the time difference to this day.
#'
#' @template param_data_ts
#' @family helper functions
#' @return character. Character variable where 'day1' indicates the first day and the following days are saved as 'dayx', where x is the difference to 'day1'.
#' @importFrom dplyr slice pull
#' @importFrom magrittr "%>%"
calcStudyDay = function(data) {
  checkmate::assertDataFrame(data)
  if (!"date" %in% colnames(data)) stop("data needs a column named 'date'. Consider adding 'date' by using addDate().")
  timestamp = date = NULL

  alldays = data$date
  daysId = character(length(alldays))
  uniquedays = unique(alldays)
  day1 = data %>% slice(which.min(timestamp)) %>% pull(date)

  daysId[which(alldays == day1)] = "day1"

  if (length(uniquedays) > 1) {
    j = which(uniquedays == day1)
    diffToDay1 = difftime(uniquedays[-j], day1, units = "days")
    for (i in 1:length(diffToDay1)) {
      if (diffToDay1[i] > 0) daysId[which(alldays == uniquedays[-j][i])] = paste0("day", diffToDay1[i] + 1)
    }
  }
  daysId
}

#' Helper function. For each group variable it takes the mininum of timestamp as first day and labels oncoming days with the time difference to this day.
#'
#' @template param_data
#' @template param_group_col
#' @template param_colname
#' @param ordered logical. If \code{TRUE}, the added character variable will be ordered, like 'day1' < 'day2' < 'day3' < ... .
#' @family helper functions
#' @return dataframe with added factor variable 'studyDay' for each group variable. The first day for each variable will be 'day1' .
#' @importFrom dplyr slice pull
#' @importFrom magrittr "%>%"
#' @export
addStudyDayByGroup = function(data, group_col, colname = "studyDay", ordered = TRUE) {
  checkmate::assertNames(names(data), must.include = group_col)
  if (colname %in% names(data)) stop("colname is already in dataset. Please choose a different colname!")

  data = addColumnByGroup(data = data, group_col = group_col, fun = calcStudyDay, colname = colname)
  if (ordered) {
    max_number_days = max(as.numeric(gsub("\\D", "", data[, colname])))
    day_levels = paste0("day", 1:max_number_days)
    data[, colname] = factor(data[, colname], levels = day_levels, ordered = TRUE)
  }
  return(data)
}

#' Helper function. It takes the mininum of timestamp as first day and labels oncoming days with the time difference to this day.
#'
#' @template param_data_ts
#' @template param_colname
#' @param ordered logical. If \code{TRUE}, the added factor variable will be ordered, like 'day1' < 'day2' < 'day3' < ... .
#' @family helper functions
#' @return dataframe with added factor variable 'studyDay'. The first day will be 'day1' .
#' @importFrom dplyr slice pull
#' @importFrom magrittr "%>%"
#' @export
addStudyDay = function(data, colname = "studyDay", ordered = TRUE) {
  message("If you have data with different users, be aware, that this function ignores that! See addStudyDayByGroup().")
  if (colname %in% names(data)) stop("colname is already in dataset. Please choose a different colname!")

  data = addColumn(data = data, fun = calcStudyDay, colname = colname)
  if (ordered) {
    max_number_days = max(as.numeric(gsub("\\D", "", data[, colname])))
    day_levels = paste0("day", 1:max_number_days)
    data[, colname] = factor(data[, colname], levels = day_levels, ordered = TRUE)
  }
  return(data)
}

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
