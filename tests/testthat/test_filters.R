context("filters")

test_that("filterWeekday", {
  i = 3
  days = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  td = data.frame(weekday = c(rep(days, i), days[1:3]), time = c("00:18:49", "01:12:45",
    "02:18:23", "03:29:56", "04:37:39", "05:01:31", "06:54:09", "07:43:16", "08:30:57", "09:56:20",
    "10:27:13", "11:53:02", "12:36:21", "13:48:01", "14:02:41", "15:29:14", "16:44:57", "17:36:26",
    "18:11:30", "19:01:27", "20:34:04", "21:37:53", "22:06:12", "23:33:30"), stringsAsFactors = FALSE)
  td$weekday = factor(td$weekday, levels = days, ordered = TRUE)
  
  # test correct filtering according to the given days
  expect_equal(days %in% unique(filterWeekday(data = td, from_day = "Mon", from_time = "00:00:00", until_day = "Wed",
    until_time = "23:59:59")$weekday), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(days %in% unique(filterWeekday(data = td, from_day = "Sat", from_time = "00:00:00", until_day = "Tue",
    until_time = "23:59:59")$weekday), c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(days %in% unique(filterWeekday(data = td, from_day = "Tue", from_time = "11:15:23", until_day = "Thu",
    until_time = "18:36:17")$weekday), c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(days %in% unique(filterWeekday(data = td, from_day = "Sun", from_time = "11:15:23", until_day = "Mon",
    until_time = "18:36:17")$weekday), c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  
  # test correct filtering according to the given times
  time1 = "11:00:00"
  time2 = "19:00:00"
  df1 = filterWeekday(data = td, from_day = "Mon", from_time = time1, until_day = "Wed",
    until_time = time2)
  
  time1_in_sec = timeToSec(time1)
  time2_in_sec = timeToSec(time2)
  df1$time_in_sec = timeToSec(df1$time)
  expect_true(min(df1$time_in_sec[df1$weekday == "Mon"]) >= time1_in_sec)
  expect_true(max(df1$time_in_sec[df1$weekday == "Wed"]) <= time2_in_sec)
  
  # test wrong format for from_time and until_time
  expect_error(filterWeekday(data = td, from_day = "Mon", from_time = "00:0a:00", until_day = "Sun",
    until_time = "23:59:59"))
  expect_error(filterWeekday(data = td, from_day = "Mon", from_time = "00:00:00", until_day = "Sun",
    until_time = "23:59:5z"))
  expect_error(filterWeekday(data = td, from_day = "Mon", from_time = "30:00:00", until_day = "Sun",
    until_time = "23:59:59"))
  expect_error(filterWeekday(data = td, from_day = "Mon", from_time = "00:00:00", until_day = "Sun",
    until_time = "23:70:59"))
  
  
})

test_that("filterDaytime", {
  i = 3
  td = data.frame(time = c("00:18:49", "01:12:45",
    "02:18:23", "03:29:56", "04:37:39", "05:01:31", "06:54:09", "07:43:16", "08:30:57", "09:56:20",
    "10:27:13", "11:53:02", "12:36:21", "13:48:01", "14:02:41", "15:29:14", "16:44:57", "17:36:26",
    "18:11:30", "19:01:27", "20:34:04", "21:37:53", "22:06:12", "23:33:30"), stringsAsFactors = FALSE)

  # test correct filtering according to the given times
  time1 = "11:00:00"
  time2 = "19:00:00"
  df1 = filterDaytime(data = td, from_time = time1, until_time = time2)

  time1_in_sec = timeToSec(time1)
  time2_in_sec = timeToSec(time2)
  df1$time_in_sec = timeToSec(df1$time)
  expect_true(min(df1$time_in_sec) >= time1_in_sec)
  expect_true(max(df1$time_in_sec) <= time2_in_sec)


  time1 = "19:00:00"
  time2 = "10:00:00"
  df1 = filterDaytime(data = td, from_time = time1, until_time = time2)

  time1_in_sec = timeToSec(time1)
  time2_in_sec = timeToSec(time2)
  df1$time_in_sec = timeToSec(df1$time)
  expect_true(all(df1$time_in_sec >= time1_in_sec | df1$time_in_sec <= time2_in_sec))

  # test wrong format for from_time and until_time
  expect_error(filterDaytime(data = td, from_time = "00:0a:00", until_time = "23:59:59"))
  expect_error(filterDaytime(data = td, from_time = "00:00:00", until_time = "23:59:5z"))
  expect_error(filterDaytime(data = td, from_time = "30:00:00", until_time = "23:59:59"))
  expect_error(filterDaytime(data = td, from_time = "00:00:00", until_time = "23:70:59"))
})
