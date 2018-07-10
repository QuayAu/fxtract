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
