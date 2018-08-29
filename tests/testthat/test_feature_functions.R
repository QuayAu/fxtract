context("feature_functions")

# test_that("calcFeaturePerStudyDayAndByGroup", {
#   d1 = 123456789
#   td = data.frame(timestamp = c(d1, d1 + 1, d1 + 2, d1 + 10000, d1 + 10001, d1 + 100000, d1 + 100001, d1 + 100001, d1 + 300001))
#   td$userId = userId = c(rep("1", 4), rep("2", 5))
#   td$x = 1:nrow(td)
#   td = addDate(td)
#   td = addStudyDayByGroup(td, group_col = "userId")
#
#   #check defaults
#   fun = function(x) sum(x$x)
#   summary_fun = function(x) mean(x, na.rm = TRUE)
#   res = calcFeaturePerStudyDayAndByGroup(data = td, group_col = "userId", colname = "mean_x_of_sum_per_day",
#     summary_fun = summary_fun, fun = fun, export_results_per_day = TRUE)
#   expect_equal(dim(res$df.res), c(2, 5))
#   expect_equal(res$res$mean_x_of_sum_per_day[1], (1 + 2 + 3 + 4) / 2)
#   expect_equal(res$res$mean_x_of_sum_per_day[2], (5 + 6 + 7 + 8 + 9) / 3)
#
#   #check colname already in dataset
#   expect_error(calcFeaturePerStudyDayAndByGroup(data = td, group_col = "userId", colname = "date",
#     summary_fun = summary_fun, fun = fun),
#     regexp = "colname is already in dataset. Please choose a different colname!")
#
#   #check max_study_day
#   res = calcFeaturePerStudyDayAndByGroup(data = td, group_col = "userId", colname = "mean_x_of_sum_per_day",
#     summary_fun = summary_fun, fun = fun, max_study_day = 2, export_results_per_day = TRUE)
#   expect_equal(dim(res$df.res), c(2, 3))
#   expect_equal(res$res$mean_x_of_sum_per_day[1], (1 + 2 + 3 + 4) / 2)
#   expect_equal(res$res$mean_x_of_sum_per_day[2], (5 + 6 + 7 + 8) / 2)
#
#   #check impute
#   res = calcFeaturePerStudyDayAndByGroup(data = td, group_col = "userId", colname = "mean_x_of_sum_per_day",
#     summary_fun = summary_fun, fun = fun, max_study_day = 5, impute_empty_day = 0, export_results_per_day = TRUE)
#   expect_equal(dim(res$df.res), c(2, 6))
#   expect_equal(res$res$mean_x_of_sum_per_day[1], (1 + 2 + 3 + 4) / 5)
#   expect_equal(res$res$mean_x_of_sum_per_day[2], (5 + 6 + 7 + 8 + 9) / 5)
#
#   #check different group name
#   td$group = td$userId
#   res = calcFeaturePerStudyDayAndByGroup(data = td, group_col = "group", colname = "mean_x_of_sum_per_day",
#     summary_fun = summary_fun, fun = fun, max_study_day = 5, impute_empty_day = 0, export_results_per_day = TRUE)
#   expect_equal(dim(res$df.res), c(2, 6))
#   expect_equal(res$res$mean_x_of_sum_per_day[1], (1 + 2 + 3 + 4) / 5)
#   expect_equal(res$res$mean_x_of_sum_per_day[2], (5 + 6 + 7 + 8 + 9) / 5)
# })

test_that("calcFeatureByGroup", {
  d1 = 123456789
  td = data.frame(timestamp = c(d1, d1 + 1, d1 + 2, d1 + 10000, d1 + 10001, d1 + 100000, d1 + 100001, d1 + 100001, d1 + 300001))
  td$userId = userId = c(rep("1", 4), rep("2", 5))
  td$x = 1:nrow(td)
  td = addDate(td)

  #test checks
  expect_error(calcFeatureByGroup(data = td, group_col = "y"))
  fun = function(data) data$x
  expect_error(calcFeatureByGroup(data = td, group_col = "userId", fun = fun, colname = "test"),
    regexp = "fun must return a vector of length 1")
  expect_error(calcFeatureByGroup(data = td, group_col = "userId", fun = fun, colname = "userId", check_fun = FALSE),
    regexp = "colname is already in dataset. Please choose a different colname!")

  #check functionality
  fun = function(data) mean(data$x)
  y = calcFeatureByGroup(data = td, group_col = "userId", fun = fun, colname = "test")
  expect_equal(y$test, c(mean(1:4), mean(5:9)))
})

