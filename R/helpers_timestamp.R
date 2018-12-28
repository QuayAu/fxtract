#' Helper function. Checks if character has correct time format 'hh:mm:ss'.
#' @param x character vector.
check_time_format = function(x) {
  for (i in 1:length(x)) {
    if (as.numeric(substr(x[i], 1, 2)) >= 24) stop("hours cannot exceed 23.")
    if (!grepl(pattern = "[012][[:digit:]]:[012345][[:digit:]]:[012345][[:digit:]]", x = x[i])) stop("character must be a time format like '15:21:30'")
  }
}

#' Helper function. Converts "hh:mm:ss" into number of seconds.
#' @param x character vector.
#' @return numeric vector. Number of seconds.
#' @export
time_to_sec = function(x) {
  check_time_format(x)
  y = strsplit(x, ":")
  y = lapply(y, as.numeric)
  f = function(z) 3600*z[1] + 60*z[2] + z[3]
  res = unlist(lapply(y, f))
  return(res)
}

#' Adds column 'weekday' to a dataframe containing a timestamp variable.
#'
#' @template param_data_ts
#' @template param_utc_col
#' @template param_tz
#' @template param_unit
#' @param week_start day on which week starts following ISO conventions - 1 means Monday, 7 means Sunday (default). See \code{\link[lubridate]{wday}}.
#' @param locale locale to use for day names. Defaults to 'English_United States.1252'. See \code{\link[lubridate]{wday}}.
#' @family helper functions
#' @return dataframe with added ordered factor variable 'weekday'.
#' @importFrom lubridate wday as_datetime
#' @export
add_weekday = function(data, utc_col = character(1), tz = "UTC", unit = "s", week_start = 1, locale = "English_United States.1252") {
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(tz)
  checkmate::assertCharacter(unit)
  checkmate::assertCharacter(locale)
  checkmate::assertNumeric(week_start)
  checkmate::assertSubset(unit, c("s", "ms"))
  checkmate::assertCharacter(utc_col, len = 1)
  if (nchar(utc_col) == 0) stop("please specify timestamp column")
  checkmate::assertSubset(utc_col, names(data))

  if (is.character(data[[utc_col]])) {
    data[[utc_col]] = as.numeric(data[[utc_col]])
    message("timestamp was converted from character to numeric")
  }
  if (locale != "English_United States.1252") message("locale was changed. Use at own risk.")


  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data[[utc_col]] / conv, tz = tz) #lubridate needs timestamp to be in seconds

  if ("weekday" %in% names(data)) warning("Your dataset already has a column named 'weekday'. It will be overwritten!")
  data$weekday = lubridate::wday(dt, label = TRUE, week_start = week_start, locale = locale)
  return(data)
}

#' Adds column 'time' to a dataframe containing a timestamp variable.
#'
#' @template param_data_ts
#' @template param_utc_col
#' @template param_tz
#' @template param_unit
#' @family helper functions
#' @return dataframe with added variable 'time'.
#' @importFrom lubridate as_datetime
#' @export
add_time = function(data, utc_col = character(1), tz = "UTC", unit = "s") {
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(tz)
  checkmate::assertCharacter(unit)
  checkmate::assertSubset(unit, c("s", "ms"))
  checkmate::assertCharacter(utc_col, len = 1)
  if (nchar(utc_col) == 0) stop("please specify timestamp column")
  checkmate::assertSubset(utc_col, names(data))

  if (is.character(data[[utc_col]])) {
    data[[utc_col]] = as.numeric(data[[utc_col]])
    message("timestamp was converted from character to numeric")
  }
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data[[utc_col]] / conv, tz = tz) #lubridate needs timestamp to be in seconds

  if ("time" %in% names(data)) warning("Your dataset already has a column named 'time'. It will be overwritten!")
  data$time = strftime(dt, "%H:%M:%S", tz = tz)
  return(data)
}

#' Adds column 'date' to a dataframe containing a timestamp variable.
#'
#' @template param_data_ts
#' @template param_utc_col
#' @template param_tz
#' @template param_unit
#' @family helper functions
#' @return dataframe with added variable 'date'.
#' @importFrom lubridate as_datetime as_date
#' @export
add_date = function(data, utc_col = character(1), tz = "UTC", unit = "s") {
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(tz)
  checkmate::assertCharacter(unit)
  checkmate::assertSubset(unit, c("s", "ms"))
  checkmate::assertCharacter(utc_col, len = 1)
  if (nchar(utc_col) == 0) stop("please specify timestamp column")
  checkmate::assertSubset(utc_col, names(data))

  if (is.character(data[[utc_col]])) {
    data[[utc_col]] = as.numeric(data[[utc_col]])
    message("timestamp was converted from character to numeric")
  }
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data[[utc_col]] / conv, tz = tz) #lubridate needs timestamp to be in seconds

  if ("date" %in% names(data)) warning("Your dataset already has a column named 'date'. It will be overwritten!")
  data$date = lubridate::as_date(dt)
  return(data)
}

#' Adds column 'date_time' to a dataframe containing a timestamp variable.
#'
#' @template param_data_ts
#' @template param_utc_col
#' @template param_tz
#' @template param_unit
#' @family helper functions
#' @return dataframe with added variable 'date_time'.
#' @importFrom lubridate as_datetime
#' @export
add_date_time = function(data, utc_col = character(1), tz = "UTC", unit = "s") {
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(tz)
  checkmate::assertCharacter(unit)
  checkmate::assertSubset(unit, c("s", "ms"))
  checkmate::assertCharacter(utc_col, len = 1)
  if (nchar(utc_col) == 0) stop("please specify timestamp column")
  checkmate::assertSubset(utc_col, names(data))

  if (is.character(data[[utc_col]])) {
    data[[utc_col]] = as.numeric(data[[utc_col]])
    message("timestamp was converted from character to numeric")
  }
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data[[utc_col]] / conv, tz = tz) #lubridate needs timestamp to be in seconds

  if ("date_time" %in% names(data)) warning("Your dataset already has a column named 'date_time'. It will be overwritten!")
  data$date_time = dt
  return(data)
}

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
filter_weekday = function(data, from_day = "Mon", from_time = "00:00:00", until_day = "Sun", until_time = "23:59:59") {
  weekday = time_in_sec = NULL
  # check inputs
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(from_day)
  checkmate::assertCharacter(from_time)
  checkmate::assertCharacter(until_day)
  checkmate::assertCharacter(until_time)
  check_time_format(from_time)
  check_time_format(until_time)

  # check data
  lw = levels(data$weekday)
  if (!"weekday" %in% colnames(data)) stop("Your data set needs a column named 'weekday'. See function add_weekday().")
  if (!"time" %in% colnames(data)) stop("Your data set needs a column named 'time'. See function add_time().")
  if (!is.ordered(data$weekday)) stop("The variable 'weekday' must be an ordered factor, e.g. Levels: Mon < Tue < Wed < Thu < Fri < Sat < Sun")
  # if (length(lw) != 7) warning("The variable 'weekday' does not have 7 levels. Please check your data!") #do we need this check?

  # convert from_day and until_day into ordered factors
  from_day = factor(from_day, levels = lw, ordered = TRUE)
  until_day = factor(until_day, levels = lw, ordered = TRUE)

  # filter dataset according to from_day and until_day
  if (from_day <= until_day) {
    df_res = data %>% dplyr::filter(weekday >= from_day, weekday <= until_day)
  } else {
    df_res = data %>% dplyr::filter(weekday >= from_day | weekday <= until_day)
  }
  if (nrow(df_res) == 0) stop("there are no dataset entries within the chosen time interval")

  # convert timestamp character string input and time variable of dataset into numeric (in seconds) starting from time 00:00:00
  ft = time_to_sec(from_time)
  ut = time_to_sec(until_time)
  df_res$time_in_sec = time_to_sec(df_res$time)

  # filter dataset according to from_time and until_time
  df_res = df_res %>% dplyr::filter(time_in_sec >= ft | weekday != from_day, time_in_sec <= ut | weekday != until_day) %>% dplyr::select(-time_in_sec)
  return(df_res)
}

#' Filter function. Filters the data by weekdays. The dataset can e.g. be filtered by all days between monday and friday.
#' The dataset needs an ordered factor variable named 'weekday'.
#'
#' @template param_data
#' @param from_time character. Time where filtering should start (on the day from_day). Must be a string with the format "hh:mm:ss". Defaults to "00:00:00".
#' @param until_time character. Time where filtering should end (on the day until_day). Must be a string with the format "hh:mm:ss". Defaults to "23:59:59".
#' @family filter functions
#' @return The dataset filtered according to the given time constraints.
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @export
filter_daytime = function(data, from_time = "07:00:00", until_time = "18:00:00") {
  time_in_sec = NULL
  # check inputs
  checkmate::assertDataFrame(data)
  checkmate::assertCharacter(from_time)
  checkmate::assertCharacter(until_time)
  check_time_format(from_time)
  check_time_format(until_time)

  if (!"time" %in% colnames(data)) stop("Your data set needs a column named 'time'. See function add_time().")

  # convert timestamp character string input and time variable of dataset into numeric (in seconds) starting from time 00:00:00
  ft = time_to_sec(from_time)
  ut = time_to_sec(until_time)

  res = data
  res$time_in_sec = time_to_sec(res$time)

  if (ft <= ut) {
    res = res %>% filter(time_in_sec >= ft & time_in_sec <= ut) %>% select(-time_in_sec)
  } else {
    res = res %>% filter(time_in_sec >= ft | time_in_sec <= ut) %>% select(-time_in_sec)
  }
  res
}
