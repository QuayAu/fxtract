context("Helpers")

test_that("not_exported", {
  td = data.frame(x = 1:3, y = 4:6)
  expect_error(checkCols("z", td), regexp = "Your data set needs a column named 'z'")
})


test_that("test_wrong_inputs", {
  td = data.frame(timestamp = c(1, 2, 3))
  #data
  expect_error(addWeekday(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(addTime(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(addDate(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(addDateTime(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(calcStudyDay(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(addColumnByUserId(data = "test", fun = "mean", colname = "cn"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")

  #tz
  expect_error(addWeekday(data = td, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")
  expect_error(addTime(data = td, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")
  expect_error(addDate(data = td, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")
  expect_error(addDateTime(data = td, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")

  #unit
  expect_error(addWeekday(data = td, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")
  expect_error(addTime(data = td, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")
  expect_error(addDate(data = td, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")
  expect_error(addDateTime(data = td, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")

  expect_error(addWeekday(data = td, unit = "S"), regexp = "unit must be 's' or 'ms")
  expect_error(addTime(data = td, unit = "S"), regexp = "unit must be 's' or 'ms")
  expect_error(addDate(data = td, unit = "S"), regexp = "unit must be 's' or 'ms")
  expect_error(addDateTime(data = td, unit = "S"), regexp = "unit must be 's' or 'ms")

  #addWeekday
  expect_error(addWeekday(data = td, week_start = "Mon"), regexp = "Assertion on 'week_start' failed: Must be of type 'numeric', not 'character'.")
  # expect_error(addWeekday(data = td, locale = "123456"))
})


test_that("add_time_variables", {
  td = data.frame(timestamp = c(1531393277, 915152461,-631144492))
  td2 = td3 = td
  td2$timestamp = td2$timestamp * 1000
  td3$timestamp = as.character(td3$timestamp)
  #1531393277 is 07/12/2018 @ 11:01:17am (UTC) Thu, 12 Jul 2018
  #915152461 is 01/01/1999 @ 1:01:01am (UTC) Fri, 01 Jan
  #-631144492 01/01/1950 @ 2:05:08am (UTC) Sun, 01 Jan

  ##messages
  expect_message(addWeekday(td3), regexp = "timestamp was converted from character to numeric")
  expect_message(addTime(td3), regexp = "timestamp was converted from character to numeric")
  expect_message(addDate(td3), regexp = "timestamp was converted from character to numeric")
  expect_message(addDateTime(td3), regexp = "timestamp was converted from character to numeric")

  #check addWeekday
  x = addWeekday(td)
  expect_equal(as.character(x$weekday), c("Thu", "Fri", "Sun"))

  x2 = addWeekday(td2, unit = "ms")
  expect_equal(as.character(x2$weekday), c("Thu", "Fri", "Sun"))

  ##change locale
  expect_message(addWeekday(data = td, locale = "Deu"), regexp = "locale was changed. Use at own risk.")
  expect_message(addWeekday(data = td, locale = "Esp"), regexp = "locale was changed. Use at own risk.")

  #check addTime
  x = addTime(td)
  expect_equal(as.character(x$time), c("11:01:17", "01:01:01", "02:05:08"))

  x2 = addTime(td2, unit = "ms")
  expect_equal(as.character(x2$time), c("11:01:17", "01:01:01", "02:05:08"))


  #check addDateTime
  x = addDateTime(td)
  expect_equal(as.character(x$date_time),
    c("2018-07-12 11:01:17", "1999-01-01 01:01:01", "1950-01-01 02:05:08"))

  x2 = addDateTime(td2, unit = "ms")
  expect_equal(as.character(x2$date_time),
    c("2018-07-12 11:01:17", "1999-01-01 01:01:01", "1950-01-01 02:05:08"))

  #check addDate
  x = addDate(td)
  expect_equal(as.character(x$date),
    c("2018-07-12", "1999-01-01", "1950-01-01"))

  x2 = addDate(td2, unit = "ms")
  expect_equal(as.character(x2$date),
    c("2018-07-12", "1999-01-01", "1950-01-01"))
})


test_that("calcStudyDay", {
  #check dataframe without 'date' variable
  td = data.frame(timestamp = c(1531393277, 915152461, -631144492))
  expect_error(calcStudyDay(td), regexp = "data needs a column named 'date'. Consider adding 'date' by using addDate().")

  #check equality
  td = addDate(td)
  expect_equal(as.character(calcStudyDay(td)), c("day25030", "day17898", "day1"))

  d1 = 123456789
  td = data.frame(timestamp = c(d1, d1 + 1, d1 + 2, d1 + 10000, d1 + 10001, d1 + 100000, d1 + 100001, d1 + 100001, d1 + 300001))
  #11/29/1973, 11/29/1973, 11/29/1973,  11/30/1973,  11/30/1973, 12/01/1973, 12/01/1973, 12/01/1973, 12/03/1973
  #day1, day1, day1, day2, day2, day3, day3, day3, day5
  td = addDate(td)
  expect_equal(as.character(calcStudyDay(td)), c("day1", "day1", "day1", "day2", "day2", "day3", "day3", "day3", "day5"))
})


test_that("addColumnByUserId", {
  td = data.frame(timestamp = c(1, 2, 3, 4, 5, 6), userId = c(rep("1", 3), rep("2", 3)))

  #wrong function
  myFun = function(data) mean(data$timestamp)
  expect_error(addColumnByUserId(data = td, fun = myFun, colname = "meanTimestamp"), regexp = "fun must return a vector of length: nrow data")

  #right function
  myFun = function(data) rep(mean(data$timestamp), nrow(data))
  expect_equal(addColumnByUserId(data = td, fun = myFun, colname = "meanTimestamp")$meanTimestamp, c(2, 2, 2, 5, 5, 5))
  expect_equal(addColumnByUserId(data = td, fun = myFun, colname = "meanTimestamp")$meanTimestamp, c(2, 2, 2, 5, 5, 5))

  #test calcStudyDay
  d1 = 123456789
  td = data.frame(timestamp = c(d1, d1 + 1, d1 + 2, d1 + 10000, d1 + 10001, d1 + 100000, d1 + 100001, d1 + 100001, d1 + 300001))
  td$userId = userId = c(rep("1", 4), rep("2", 5))
  td = addDate(td)
  #expect: c("day1", "day1", "day1", "day2", "day1", "day2", "day2", "day2", "day4")
  res =  c("day1", "day1", "day1", "day2", "day1", "day2", "day2", "day2", "day4")
  expect_equal(addColumnByUserId(data = td, fun = calcStudyDay, colname = "studyDay")$studyDay, res)
})

test_that("slidingWindow", {
  td = data.frame(timestamp = c(1:10, 15:25), x = 1:21)
  fun = function(data) sum(data$x)

  #test steps
  x = slidingWindow(td, fun, steps = 3)$new_feature
  expect_equal(x[1:3], c(NA_integer_, NA_integer_, NA_integer_))
  expect_equal(x[4:21], c(6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57))

  #test time in seconds
  td = addDateTime(td)
  x = slidingWindow(td, fun, time_in_sec = 3)$new_feature

  expect_equal(x, c(NA_integer_, 1, 3, 5, 7, 9, 11, 13, 15, 17, 0, 11, 23, 25, 27, 29, 31, 33, 35, 37, 39))
})

