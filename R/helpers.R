checkInputs = function(ids, loggingdata) {
  checkmate::assertCharacter(ids)
  checkmate::assertDataFrame(loggingdata)
}

checkType = function(type) {
 checkmate::assertCharacter(type)
}

checkSource = function(source, loggingdata) {
  #if (any(!source %in% loggingdata$source)) stop(paste0("Your data source does not contain ", source, " events"))
}

checkId = function(ids, loggingdata) {
  if (any(!ids %in% loggingdata$user_id)) stop("id not available")
}

checkCols = function(columnnames, loggingdata) {
  ld = colnames(loggingdata)
  for (i in 1:length(columnnames)) if (!any(ld %in% columnnames[i]))
    stop(paste0("Your data set needs a column named '", columnnames[i], "'"))
}

# timeToSec = function(x) {
#   x = as.numeric(x)
#   3600*x[1] + 60*x[2] + x[3]
# }
#
#
# timeToNumericHours = function(x) {
#   unlist(lapply(strsplit(x, split = ":"), timeToSec)) / 60 / 60
# }
#
#
# my.rle = function(x) {
#   y = c(FALSE, x[-length(x)])
#   y[is.na(y)] = "NA"
#   z = x != y
#   z[is.na(z)] = TRUE
#   z #keep NA
# }
#
# occur = function(patrn, x) {
#   patrn.rev <- rev(patrn)
#   w <- embed(x,length(patrn))
#   which(apply(w, 1, function(r) all(r == patrn.rev)))
# }

# findFirstEvent = function(loggingdata, ids = unique(loggingdata$userId), from.time = "05:00:00") {
#
#   if (!grepl(pattern = "[012][[:digit:]]:[012345][[:digit:]]:[012345][[:digit:]]", x = from.time))
#     stop("from.time must be a time format like '15:21:30'")
#   threshhold = timeToSec(strsplit(from.time, ":")[[1]])
#
#   res = loggingdata %>%
#     filter(userId %in% ids) %>%
#     labelScreenEvents() %>%
#     filter(source == "SCREEN") %>%
#     mutate(time.in.sec = sapply(strsplit(time, ":"), timeToSec)) %>%
#     filter(time.in.sec >= threshhold, general.usageType %in% c("USING_START", "CHECKING_START")) %>%
#     group_by(userId, studyDay) %>%
#     slice(1) %>%
#     select(time, userId, time.in.sec, studyDay)
#
#   return(res)
# }
#
#
# shiftStudyDay = function(x, increment = 1){
#   x_nlevel = nlevels(x)
#   x_lables = levels(x)
#
#   # apply function .fun to the numeric of the ordered vector
#   res = as.numeric(x) + increment
#
#   # cap to 1 and x_nlevel if the increment was larger than the original range of the factor levels
#   res = ((res - 1) %% x_nlevel) + 1
#
#   ordered(res, levels = 1:x_nlevel, labels = x_lables)
# }
#
# findLastEvent = function(loggingdata, ids = unique(loggingdata$userId), until.time = "04:00:00") {
#   if (!grepl(pattern = "[012][[:digit:]]:[012345][[:digit:]]:[012345][[:digit:]]", x = until.time))
#     stop("until.time must be a time format like '15:21:30'")
#   max.time = sapply(strsplit("24:00:00", ":"), timeToSec)
#   threshhold = timeToSec(strsplit(until.time, ":")[[1]])
#
#   res = loggingdata %>%
#     filter(userId %in% ids) %>%
#     labelScreenEvents() %>%
#     filter(source == "SCREEN") %>%
#     mutate(time.in.sec = sapply(strsplit(time, ":"), timeToSec)) %>%
#     mutate(night.of = ifelse(time.in.sec < threshhold, as.character(shiftStudyDay(studyDay, increment = -1)),  as.character(studyDay)))%>%
#     filter(general.usageType %in% c("USING_END", "CHECKING_END")) %>%
#     group_by(userId, night.of) %>%
#     slice(n()) %>%
#     mutate(time.in.sec = ifelse(time.in.sec < threshhold, max.time + time.in.sec, time.in.sec)) %>%
#     select(time, userId, time.in.sec, night.of)
#
#   return(res)
# }
#
#
# fetchTime = function(date_time) {
#   strftime(date_time, "%H:%M:%S", tz = "UTC")
# }
#
# # timeToNextEvent returns column 'time to next event' in seconds
# # (timestamp of input loggingdata must be in milliseconds)
# timeToNextEvent = function(loggingdata, ids = unique(loggingdata$userId),
#   background_apps = character()) {
#
#   df = ignoreBackgroundApps(loggingdata, ids, background_apps)
#   df0 = df[0, ]
#
#   for (id in ids) {
#     df1 = dplyr::filter(df, userId == id) %>% dplyr::arrange(timestamp)
#     df1$timeToNextEvent = NA
#     if (nrow(df1) > 1) {
#       n = nrow(df1)
#       df1$timeToNextEvent[n] = NA
#       df1$timeToNextEvent[1:(n-1)] = (df1$timestamp[2:n] - df1$timestamp[1:(n - 1)])/1000
#     }
#     df0 = dplyr::bind_rows(df0, df1)
#   }
#   df0 = df0 %>% dplyr::arrange(userId, timestamp)
#   return(df0)
# }
#
# # helper function for AR coefficients:
# # if timeInterval is a divisor of 24:  label time intervals of day by timeInterval
# labelTimeInterval = function(time, timeInterval) {
#   hour = as.numeric(substr(time, start = 1, stop = 2))
#   n = hour %/% timeInterval
#   return(n + 1)
# }
#
# # helper function for AR coefficients:
# # if timeInterval is a multiple of 24:  label time intervals of study days timeInterval
# labelTimeInterval2 = function(studyDayNum, timeInterval) {
#   n = (studyDayNum - 1) %/% (timeInterval/24)
#   return(n + 1)
# }
#
#

#' Adds column 'weekday' to a dataframe containing a timestamp variable.
#'
#' @param data dataframe containing column named 'timestamp'.
#' @param tz timezone. Defaults to using UTC.
#' @param unit unit of timezone. Can be 's' or 'ms'. Defaults to s (seconds).
#' @param week_start day on which week starts following ISO conventions - 1 means Monday, 7 means Sunday (default).
#' @param locale locale to use for day names. Default to 'English_United States.1252'.
#' @family helper functions
#' @return dataframe with added ordered factor variable 'weekday'.
#' @import lubridate
#' @export
addWeekday = function(data, tz = "UTC", unit = "s", week_start = 1, locale = "English_United States.1252") {
  checkCols("timestamp", data)
  if (!unit %in% c("s", "ms")) stop("unit must be 's' or 'ms")
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  data$weekday = lubridate::wday(dt, label = TRUE, week_start = week_start, locale = locale)
  return(data)
}

#' Adds column 'time' to a dataframe containing a timestamp variable.
#'
#' @param data dataframe containing column named 'timestamp'.
#' @param tz timezone. Defaults to using UTC.
#' @param unit unit of timezone. Can be 's' or 'ms'. Defaults to s (seconds).
#' @family helper functions
#' @return dataframe with added variable 'time'.
#' @import lubridate
#' @export
addTime = function(data, tz = "UTC", unit = "s") {
  checkCols("timestamp", data)
  if (!unit %in% c("s", "ms")) stop("unit must be 's' or 'ms")
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  data$time = strftime(dt, "%H:%M:%S", tz = tz)
  return(data)
}

#' Adds column 'date' to a dataframe containing a timestamp variable.
#'
#' @param data dataframe containing column named 'timestamp'.
#' @param tz timezone. Defaults to using UTC.
#' @param unit unit of timezone. Can be 's' or 'ms'. Defaults to s (seconds).
#' @family helper functions
#' @return dataframe with added variable 'date'.
#' @import lubridate
#' @export
addDate = function(data, tz = "UTC", unit = "s") {
  checkCols("timestamp", data)
  if (!unit %in% c("s", "ms")) stop("unit must be 's' or 'ms")
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  data$date = lubridate::as_date(dt)
  return(data)
}

#' Adds column 'date_time' to a dataframe containing a timestamp variable.
#'
#' @param data dataframe containing column named 'timestamp'.
#' @param tz timezone. Defaults to using UTC.
#' @param unit unit of timezone. Can be 's' or 'ms'. Defaults to s (seconds).
#' @family helper functions
#' @return dataframe with added variable 'date_time'.
#' @import lubridate
#' @export
addDateTime = function(data, tz = "UTC", unit = "s") {
  checkCols("timestamp", data)
  if (!unit %in% c("s", "ms")) stop("unit must be 's' or 'ms")
  conv = ifelse(unit == "s", 1, 1000)
  dt = lubridate::as_datetime(data$timestamp / conv, tz = tz) #lubridate needs timestamp to be in seconds

  data$date_time = dt
  return(data)
}


#' Adds column 'studyDay' to a dataframe containing a timestamp variable.
#'
#' @param data dataframe containing column named 'timestamp'.
#' @family helper functions
#' @return dataframe with added ordered factor variable 'studyDay'. The first day for each userId will be 'day1'.
#' @import lubridate
#' @export
addStudyDay = function(data) {

}



#
# #add factor variable day1 < day2 < day3 < day4 .....
# numberDays = 10000
# dayx = paste0("day", 1:numberDays)
#
# library(dplyr)
# phonedata = phonedata = phonedata %>% group_by(userId) %>% arrange(userId, timestamp)
#
# addStudyDay = function(id, loggingdata = phonedata, study.begin = loggingdata[loggingdata$source == "DEVICEINFO", c("userId", "date")]) {
#   x = dplyr::filter(phonedata, userId == id)
#   if (!all.equal(x, x[order(x$timestamp), ])) stop()
#   uniquedays = unique(x$date)
#   day1 = study.begin[study.begin$userId == id, "date"][1]
#   alldays = x$date #x$date = as_date(x$timestamp)
#
#   daysId = character(length(alldays))
#
#   daysId[which(alldays == day1)] = "day1"
#
#   if (length(uniquedays) > 1) {
#     diffToDay1 = difftime(uniquedays[-1], day1, units = "days")
#     for (i in 1:length(diffToDay1)) {
#       if (diffToDay1[i] > 0) daysId[which(alldays == uniquedays[-1][i])] = paste0("day", diffToDay1[i] + 1)
#     }
#   }
#   daysId
# }
