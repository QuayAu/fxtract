context("Helpers")

test_that("test_wrong_inputs", {
  toydata = data.frame(timestamp = c(1, 2, 3))
  #data
  expect_error(addWeekday(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(addTime(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(addDate(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(addDateTime(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(calcStudyDay(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")

  #tz
  expect_error(addWeekday(data = toydata, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")
  expect_error(addTime(data = toydata, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")
  expect_error(addDate(data = toydata, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")
  expect_error(addDateTime(data = toydata, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")

  #unit
  expect_error(addWeekday(data = toydata, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")
  expect_error(addTime(data = toydata, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")
  expect_error(addDate(data = toydata, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")
  expect_error(addDateTime(data = toydata, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")

  expect_error(addWeekday(data = toydata, unit = "S"), regexp = "unit must be 's' or 'ms")
  expect_error(addTime(data = toydata, unit = "S"), regexp = "unit must be 's' or 'ms")
  expect_error(addDate(data = toydata, unit = "S"), regexp = "unit must be 's' or 'ms")
  expect_error(addDateTime(data = toydata, unit = "S"), regexp = "unit must be 's' or 'ms")

  #addWeekday
  expect_error(addWeekday(data = toydata, week_start = "Mon"), regexp = "Assertion on 'week_start' failed: Must be of type 'numeric', not 'character'.")
  expect_error(addWeekday(data = toydata, locale = "123456"))


})


test_that("add_time_variables", {
  toydata = data.frame(timestamp = c(1531393277, 915152461,-631144492))
  toydata2 = toydata
  toydata2$timestamp = toydata2$timestamp * 1000
  #1531393277 is 07/12/2018 @ 11:01:17am (UTC) Thu, 12 Jul 2018
  #915152461 is 01/01/1999 @ 1:01:01am (UTC) Fri, 01 Jan
  #-631144492 01/01/1950 @ 2:05:08am (UTC) Sun, 01 Jan

  #check addWeekday
  x = addWeekday(toydata)
  expect_equal(as.character(x$weekday), c("Thu", "Fri", "Sun"))

  x2 = addWeekday(toydata2, unit = "ms")
  expect_equal(as.character(x2$weekday), c("Thu", "Fri", "Sun"))


  #check addTime
  x = addTime(toydata)
  expect_equal(as.character(x$time), c("11:01:17", "01:01:01", "02:05:08"))

  x2 = addTime(toydata2, unit = "ms")
  expect_equal(as.character(x2$time), c("11:01:17", "01:01:01", "02:05:08"))


  #check addDateTime
  x = addDateTime(toydata)
  expect_equal(as.character(x$date_time),
    c("2018-07-12 11:01:17", "1999-01-01 01:01:01", "1950-01-01 02:05:08"))

  x2 = addDateTime(toydata2, unit = "ms")
  expect_equal(as.character(x2$date_time),
    c("2018-07-12 11:01:17", "1999-01-01 01:01:01", "1950-01-01 02:05:08"))

  #check addDate
  x = addDate(toydata)
  expect_equal(as.character(x$date),
    c("2018-07-12", "1999-01-01", "1950-01-01"))

  x2 = addDate(toydata2, unit = "ms")
  expect_equal(as.character(x2$date),
    c("2018-07-12", "1999-01-01", "1950-01-01"))
})
