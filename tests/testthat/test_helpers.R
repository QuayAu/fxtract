context("Helpers")

test_that("test_non_exported", {
  x = "23:14:21"
  expect_silent(check_time_format(x))

  x = c(x, x)
  expect_silent(check_time_format(x))

  x = "25:14:22"
  expect_error(check_time_format(x), regexp = "hours cannot exceed 23.")

  x = c("01:02:02", x)
  expect_error(check_time_format(x), regexp = "hours cannot exceed 23.")

  x = "14:96:53"
  expect_error(check_time_format(x), regexp = "character must be a time format like '15:21:30'")

  x = "13:42:60"
  expect_error(check_time_format(x), regexp = "character must be a time format like '15:21:30'")
})

test_that("test_wrong_inputs", {
  td = data.frame(timestamp = c(1, 2, 3))
  #data
  expect_error(add_weekday(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(add_time(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(add_date(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")
  expect_error(add_date_time(data = "test"), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")

  #tz
  expect_error(add_weekday(data = td, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")
  expect_error(add_time(data = td, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")
  expect_error(add_date(data = td, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")
  expect_error(add_date_time(data = td, tz = 1), regexp = "Assertion on 'tz' failed: Must be of type 'character', not 'double'.")

  #unit
  expect_error(add_weekday(data = td, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")
  expect_error(add_time(data = td, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")
  expect_error(add_date(data = td, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")
  expect_error(add_date_time(data = td, unit = 1), regexp = "Assertion on 'unit' failed: Must be of type 'character', not 'double'.")

  expect_error(add_weekday(data = td, unit = "S"))
  expect_error(add_time(data = td, unit = "S"))
  expect_error(add_date(data = td, unit = "S"))
  expect_error(add_date_time(data = td, unit = "S"))

  #utc_col
  expect_error(add_weekday(data = td, utc_col = "ts"))
  expect_error(add_time(data = td, utc_col = "ts"))
  expect_error(add_date(data = td, utc_col = "ts"))
  expect_error(add_date_time(data = td, utc_col = "ts"))

  expect_error(add_weekday(data = td), regexp = "please specify timestamp column")
  expect_error(add_time(data = td), regexp = "please specify timestamp column")
  expect_error(add_date(data = td), regexp = "please specify timestamp column")
  expect_error(add_date_time(data = td), regexp = "please specify timestamp column")

  #add_weekday
  expect_error(add_weekday(data = td, week_start = "Mon"), regexp = "Assertion on 'week_start' failed: Must be of type 'numeric', not 'character'.")
  td$weekday = c("Mon", "Tue", "Wed")
  expect_warning(add_weekday(td, utc_col = "timestamp"), regexp = "Your dataset already has a column named 'weekday'. It will be overwritten!")

  #add_time
  td$time = c("11:01:17", "01:01:01", "02:05:08")
  expect_warning(add_time(td, utc_col = "timestamp"), regexp = "Your dataset already has a column named 'time'. It will be overwritten!")

  #add_date
  td$date = c("2018-07-12", "1999-01-01", "1950-01-01")
  expect_warning(add_date(td, utc_col = "timestamp"), regexp = "Your dataset already has a column named 'date'. It will be overwritten!")

  #add_date
  td$date_time = c("2018-07-12 11:01:17", "1999-01-01 01:01:01", "1950-01-01 02:05:08")
  expect_warning(add_date_time(td, utc_col = "timestamp"), regexp = "Your dataset already has a column named 'date_time'. It will be overwritten!")
})


test_that("add_time_variables", {
  td = data.frame(timestamp = c(1531393277, 915152461,-631144492))
  #1531393277 is 07/12/2018 @ 11:01:17am (UTC) Thu, 12 Jul 2018
  #915152461 is 01/01/1999 @ 1:01:01am (UTC) Fri, 01 Jan
  #-631144492 01/01/1950 @ 2:05:08am (UTC) Sun, 01 Jan
  td4 = td2 = td3 = td
  #timestamp in ms
  td2$timestamp = td2$timestamp * 1000
  #timestamp as character
  td3$timestamp = as.character(td3$timestamp)
  #different timestamp name
  td4$utc = td4$timestamp
  td4$timestamp = NULL

  #character timestamp
  expect_message(add_weekday(td3, utc_col = "timestamp"), regexp = "timestamp was converted from character to numeric")
  expect_message(add_time(td3, utc_col = "timestamp"), regexp = "timestamp was converted from character to numeric")
  expect_message(add_date(td3, utc_col = "timestamp"), regexp = "timestamp was converted from character to numeric")
  expect_message(add_date_time(td3, utc_col = "timestamp"), regexp = "timestamp was converted from character to numeric")

  #check add_weekday
  expected = c("Thu", "Fri", "Sun")
  x = add_weekday(td, utc_col = "timestamp")
  expect_equal(as.character(x$weekday), expected)

  x = add_weekday(td2, utc_col = "timestamp", unit = "ms")
  expect_equal(as.character(x$weekday), expected)

  x = add_weekday(td3, utc_col = "timestamp")
  expect_equal(as.character(x$weekday), expected)

  x = add_weekday(td4, utc_col = "utc")
  expect_equal(as.character(x$weekday), expected)

  ##change locale
  expect_message(add_weekday(data = td, utc_col = "timestamp", locale = "Deu"), regexp = "locale was changed. Use at own risk.")
  expect_message(add_weekday(data = td, utc_col = "timestamp", locale = "Esp"), regexp = "locale was changed. Use at own risk.")

  #check add_time
  expected = c("11:01:17", "01:01:01", "02:05:08")

  x = add_time(td, utc_col = "timestamp")
  expect_equal(as.character(x$time), expected)

  x = add_time(td2, utc_col = "timestamp", unit = "ms")
  expect_equal(as.character(x$time), expected)

  x = add_time(td3, utc_col = "timestamp")
  expect_equal(as.character(x$time), expected)

  x = add_time(td4, utc_col = "utc")
  expect_equal(as.character(x$time), expected)

  #check add_date_time
  expected = c("2018-07-12 11:01:17", "1999-01-01 01:01:01", "1950-01-01 02:05:08")

  x = add_date_time(td, utc_col = "timestamp")
  expect_equal(as.character(x$date_time), expected)

  x = add_date_time(td2, utc_col = "timestamp", unit = "ms")
  expect_equal(as.character(x$date_time), expected)

  x = add_date_time(td3, utc_col = "timestamp")
  expect_equal(as.character(x$date_time), expected)

  x = add_date_time(td4, utc_col = "utc")
  expect_equal(as.character(x$date_time), expected)

  #check add_date
  expected = c("2018-07-12", "1999-01-01", "1950-01-01")

  x = add_date(td, utc_col = "timestamp")
  expect_equal(as.character(x$date), expected)

  x = add_date(td2, utc_col = "timestamp", unit = "ms")
  expect_equal(as.character(x$date), expected)

  x = add_date(td3, utc_col = "timestamp")
  expect_equal(as.character(x$date), expected)

  x = add_date(td4, utc_col = "utc")
  expect_equal(as.character(x$date), expected)
})

test_that("sliding_window", {
  td = data.frame(timestamp = c(1:10, 15:25), x = 1:21)

  fun = function(data) data.frame(sum_x_last3 = sum(data$x), max_x_last3 = max(data$x))

  #test steps
  x = sliding_window(td, fun = fun, steps = 3)
  expect_equal(dim(x), c(21, 4))
  expect_equal(x$sum_x_last3[1:3], c(NA_integer_, NA_integer_, NA_integer_))
  expect_equal(x$sum_x_last3[4:21], c(6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57))
  expect_equal(x$max_x_last3[4:21], 3:20)

  ##test eval_at_rows
  x = sliding_window(td, fun = fun, steps = 3, eval_at_rows = c(5, 10, 15, 20))
  expect_equal(x$sum_x_last3[4:21], c(NA, 9, NA, NA, NA, NA, 24, NA, NA, NA, NA, 39, NA, NA, NA, NA, 54, NA))

  ###test steps > interval at eval_at_rows
  x = sliding_window(td, fun = fun, steps = 6, eval_at_rows = c(5, 10, 15, 20))
  expect_equal(x$sum_x_last3[4:21], c(NA, NA, NA, NA, NA, NA, 39, NA, NA, NA, NA, 69, NA, NA, NA, NA, 19 + 18 + 17 + 16 + 15 + 14, NA))

  ##test wrong inputs
  expect_error(sliding_window(td, fun = fun, time_in_sec = 5), regexp = "please specify timestamp column")
  expect_error(sliding_window(td, fun = fun, time_in_sec = 5, steps = 2), regexp = "Pass either steps or time_in_sec, but not both!")
  expect_error(sliding_window(td, fun = fun, time_in_sec = 5, utc_col = "wrong timestamp"))
  expect_error(sliding_window(td, fun = fun, time_in_sec = 5, utc_col = "timestamp", unit = "wrong unit"))

  #test time in seconds
  x = sliding_window(td, fun = fun, time_in_sec = 3, utc_col = "timestamp")

  expect_equal(x$sum_x_last3, c(NA_integer_, 1, 3, 5, 7, 9, 11, 13, 15, 17, 0, 11, 23, 25, 27, 29, 31, 33, 35, 37, 39))
  expect_equal(x$max_x_last3, c(NA_integer_, 1:9, -Inf, 11:20))

  #test time in milliseconds
  td = data.frame(timestamp = c(1:10, 15:25), x = 1:21)
  td$timestamp[2] = 1.5
  td$timestamp[4] = 3.5
  td$timestamp = td$timestamp * 1000
  x = sliding_window(td, fun = fun, time_in_sec = 2.4, utc_col = "timestamp", unit = "ms")
  expect_equal(x$sum_x_last3, c(NA_integer_, 1, 3, 5, 7, 5, 11, 13, 15, 17, 0, 11, 23, 25, 27, 29, 31, 33, 35, 37, 39))

  ##test eval_at_rows
  td = data.frame(timestamp = c(1:10, 15:25), x = 1:21)
  x = sliding_window(td, fun = fun, time_in_sec = 3, utc_col = "timestamp", eval_at_rows = c(5, 10, 15, 20))

  expect_equal(x$sum_x_last3, c(rep(NA_integer_, 4), 7, rep(NA_integer_, 4), 17, rep(NA_integer_, 4), 27, rep(NA_integer_, 4), 37, NA))
})

test_that("filter_weekday", {
  td = data.frame(timestamp = 1:10)

  #test checks
  expect_error(filter_weekday(td), regexp = "Your data set needs a column named 'weekday'. See function add_weekday().")
  td = add_weekday(td, utc_col = "timestamp")
  expect_error(filter_weekday(td), regexp = "Your data set needs a column named 'time'. See function add_time().")
  td = add_time(td, utc_col = "timestamp")
  expect_error(filter_weekday(td, from_day = "Fri", until_day = "Sun"), regexp = "there are no dataset entries within the chosen time interval")
  td$weekday = as.character(td$weekday)
  expect_error(filter_weekday(td), regexp = "The variable 'weekday' must be an ordered factor, e.g. Levels: Mon < Tue < Wed < Thu < Fri < Sat < Sun")

  #test functionality
  i = 3
  days = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  td = data.frame(weekday = c(rep(days, i), days[1:3]), time = c("00:18:49", "01:12:45",
    "02:18:23", "03:29:56", "04:37:39", "05:01:31", "06:54:09", "07:43:16", "08:30:57", "09:56:20",
    "10:27:13", "11:53:02", "12:36:21", "13:48:01", "14:02:41", "15:29:14", "16:44:57", "17:36:26",
    "18:11:30", "19:01:27", "20:34:04", "21:37:53", "22:06:12", "23:33:30"), stringsAsFactors = FALSE)
  td$weekday = factor(td$weekday, levels = days, ordered = TRUE)

  # test correct filtering according to the given days
  expect_equal(days %in% unique(filter_weekday(data = td, from_day = "Mon", from_time = "00:00:00", until_day = "Wed",
    until_time = "23:59:59")$weekday), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(days %in% unique(filter_weekday(data = td, from_day = "Sat", from_time = "00:00:00", until_day = "Tue",
    until_time = "23:59:59")$weekday), c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(days %in% unique(filter_weekday(data = td, from_day = "Tue", from_time = "11:15:23", until_day = "Thu",
    until_time = "18:36:17")$weekday), c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(days %in% unique(filter_weekday(data = td, from_day = "Sun", from_time = "11:15:23", until_day = "Mon",
    until_time = "18:36:17")$weekday), c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))

  # test correct filtering according to the given times
  time1 = "11:00:00"
  time2 = "19:00:00"
  df1 = filter_weekday(data = td, from_day = "Mon", from_time = time1, until_day = "Wed",
    until_time = time2)

  time1_in_sec = time_to_sec(time1)
  time2_in_sec = time_to_sec(time2)
  df1$time_in_sec = time_to_sec(df1$time)
  expect_true(min(df1$time_in_sec[df1$weekday == "Mon"]) >= time1_in_sec)
  expect_true(max(df1$time_in_sec[df1$weekday == "Wed"]) <= time2_in_sec)

  # test wrong format for from_time and until_time
  expect_error(filter_weekday(data = td, from_day = "Mon", from_time = "00:0a:00", until_day = "Sun",
    until_time = "23:59:59"))
  expect_error(filter_weekday(data = td, from_day = "Mon", from_time = "00:00:00", until_day = "Sun",
    until_time = "23:59:5z"))
  expect_error(filter_weekday(data = td, from_day = "Mon", from_time = "30:00:00", until_day = "Sun",
    until_time = "23:59:59"))
  expect_error(filter_weekday(data = td, from_day = "Mon", from_time = "00:00:00", until_day = "Sun",
    until_time = "23:70:59"))
})

test_that("filter_daytime", {
  i = 3
  td = data.frame(time = c("00:18:49", "01:12:45",
    "02:18:23", "03:29:56", "04:37:39", "05:01:31", "06:54:09", "07:43:16", "08:30:57", "09:56:20",
    "10:27:13", "11:53:02", "12:36:21", "13:48:01", "14:02:41", "15:29:14", "16:44:57", "17:36:26",
    "18:11:30", "19:01:27", "20:34:04", "21:37:53", "22:06:12", "23:33:30"), stringsAsFactors = FALSE)

  # test correct filtering according to the given times
  time1 = "11:00:00"
  time2 = "19:00:00"
  df1 = filter_daytime(data = td, from_time = time1, until_time = time2)

  time1_in_sec = time_to_sec(time1)
  time2_in_sec = time_to_sec(time2)
  df1$time_in_sec = time_to_sec(df1$time)
  expect_true(min(df1$time_in_sec) >= time1_in_sec)
  expect_true(max(df1$time_in_sec) <= time2_in_sec)


  time1 = "19:00:00"
  time2 = "10:00:00"
  df1 = filter_daytime(data = td, from_time = time1, until_time = time2)

  time1_in_sec = time_to_sec(time1)
  time2_in_sec = time_to_sec(time2)
  df1$time_in_sec = time_to_sec(df1$time)
  expect_true(all(df1$time_in_sec >= time1_in_sec | df1$time_in_sec <= time2_in_sec))

  # test wrong format for from_time and until_time
  expect_error(filter_daytime(data = td, from_time = "00:0a:00", until_time = "23:59:59"))
  expect_error(filter_daytime(data = td, from_time = "00:00:00", until_time = "23:59:5z"))
  expect_error(filter_daytime(data = td, from_time = "30:00:00", until_time = "23:59:59"))
  expect_error(filter_daytime(data = td, from_time = "00:00:00", until_time = "23:70:59"))
})
