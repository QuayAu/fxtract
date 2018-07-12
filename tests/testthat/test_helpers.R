context("Helpers")

test_that("add_time_variables", {
  toydata = data.frame(timestamp = c(1531393277, 915152461,-631144492))
  toydata2 = toydata
  toydata2$timestamp = toydata2$timestamp * 1000
  #1531393277 is 07/12/2018 @ 11:01:17am (UTC) Thu, 12 Jul 2018
  #915152461 is 01/01/1999 @ 1:01:01am (UTC) Fri, 01 Jan
  #-631144492 01/01/1950 @ 2:05:08am (UTC) Sun, 01 Jan

  #check addWeekday
  x = addWeekday(toydata)
  expect_factor(x$weekday)
  expect_equal(as.character(x$weekday), c("Thu", "Fri", "Sun"))

  x2 = addWeekday(toydata2, unit = "ms")
  expect_factor(x2$weekday)
  expect_equal(as.character(x2$weekday), c("Thu", "Fri", "Sun"))


  #check addTime
  x = addTime(toydata)
  expect_character(x$time)
  expect_equal(as.character(x$time), c("11:01:17", "01:01:01", "02:05:08"))

  x2 = addTime(toydata2, unit = "ms")
  expect_character(x2$time)
  expect_equal(as.character(x2$time), c("11:01:17", "01:01:01", "02:05:08"))


  #check addDateTime
  x = addDateTime(toydata)
  expect_equal(as.character(x$date_time),
    c("2018-07-12 11:01:17", "1999-01-01 01:01:01", "1950-01-01 02:05:08"))

  x2 = addDateTime(toydata2, unit = "ms")
  expect_equal(as.character(x2$date_time),
    c("2018-07-12 11:01:17", "1999-01-01 01:01:01", "1950-01-01 02:05:08"))

})
