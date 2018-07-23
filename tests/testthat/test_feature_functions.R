context("feature_functions")

test_that("calcFeaturePerStudyDayAndUserId", {
  d1 = 123456789
  td = data.frame(timestamp = c(d1, d1 + 1, d1 + 2, d1 + 10000, d1 + 10001, d1 + 100000, d1 + 100001, d1 + 100001, d1 + 300001))
  td$userId = userId = c(rep("1", 4), rep("2", 5))
  td$x = 1:nrow(td)
  td = addDate(td)
  td = addStudyDayPerUserId(td)

  #check defaults
  fun = function(x) sum(x$x)
  summary_fun = function(x) mean(x, na.rm = TRUE)
  res = calcFeaturePerStudyDayAndUserId(data = td, colname = "mean_x_of_sum_per_day",
    summary_fun = summary_fun, fun = fun, export_results_per_day = TRUE)
  expect_equal(dim(res$df.res), c(2, 5))
  expect_equal(res$res$mean_x_of_sum_per_day[1], (1 + 2 + 3 + 4) / 2)
  expect_equal(res$res$mean_x_of_sum_per_day[2], (5 + 6 + 7 + 8 + 9) / 3)

  #check max_study_day
  res = calcFeaturePerStudyDayAndUserId(data = td, colname = "mean_x_of_sum_per_day",
    summary_fun = summary_fun, fun = fun, max_study_day = 2, export_results_per_day = TRUE)
  expect_equal(dim(res$df.res), c(2, 3))
  expect_equal(res$res$mean_x_of_sum_per_day[1], (1 + 2 + 3 + 4) / 2)
  expect_equal(res$res$mean_x_of_sum_per_day[2], (5 + 6 + 7 + 8) / 2)

  #check impute
  res = calcFeaturePerStudyDayAndUserId(data = td, colname = "mean_x_of_sum_per_day",
    summary_fun = summary_fun, fun = fun, max_study_day = 5, impute_empty_day = 0, export_results_per_day = TRUE)
  expect_equal(dim(res$df.res), c(2, 6))
  expect_equal(res$res$mean_x_of_sum_per_day[1], (1 + 2 + 3 + 4) / 5)
  expect_equal(res$res$mean_x_of_sum_per_day[2], (5 + 6 + 7 + 8 + 9) / 5)



})
