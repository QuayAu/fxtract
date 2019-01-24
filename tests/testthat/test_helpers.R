context("Helpers")

test_that("dplyr_wrapper", {
  df = data.frame(id = c(rep(1, 10), rep(2, 10)))
  df$task = rep(c(rep("task1", 5), rep("task2", 5)), 2)
  df$hour = rep(c(rep("hour1", 3), rep("hour2", 2), rep("hour1", 2), rep("hour2", 3)), 2)
  df$x = 1:20

  #test checks
  fun = function(data) data.frame(x = data$x, x2 = data$x * 2)
  expect_error(dplyr_wrapper(data = df, group_by = "id", fun = fun))

  #return 1 value
  fun = function(data) c(mean_x = mean(data$x))
  y = dplyr_wrapper(data = df, group_by = "id", fun = fun)
  expect_equal(y$mean_x, c(mean(1:10), mean(11:20)))
  expect_equal(colnames(y)[2], "mean_x")

  #return more values
  more_features = function(data) c(mean_x = mean(data$x), sd_x = sd(data$x), min_x = min(data$x))
  y = dplyr_wrapper(data = df, group_by = "id", fun = more_features)
  expect_equal(y$mean_x, c(mean(1:10), mean(11:20)))
  expect_equal(y$sd_x, c(sd(1:10), sd(11:20)))
  expect_equal(y$min_x, c(min(1:10), min(11:20)))
  expect_equal(colnames(y)[2:4], c("mean_x", "sd_x", "min_x"))

  #group by 2 columns
  y = dplyr_wrapper(data = df, group_by = c("id", "task"), fun = more_features)
  expect_equal(dim(y), c(2, 7))

  #group by 3 columns
  y = dplyr_wrapper(data = df, group_by = c("id", "task", "hour"), fun = more_features)
  expect_equal(dim(y), c(2, 3 * 4 + 1))

  #group by 4 columns
  df$y = c(rep(1, 2), rep(2, 2))
  y = dplyr_wrapper(data = df, group_by = c("id", "task", "hour", "y"), fun = more_features)
  expect_equal(dim(y), c(2, 3 * 4 * 2 + 1))

  #return list
  df = data.frame(id = c(rep(1, 10), rep(2, 10)))
  df$task = rep(c(rep("task1", 5), rep("task2", 5)), 2)
  df$hour = rep(c(rep("hour1", 3), rep("hour2", 2), rep("hour1", 2), rep("hour2", 3)), 2)
  df$x = 1:20

  #test checks
  fun = function(data) list(x = c(mean(data$x), sd(data$x)), x2 = "test")
  expect_error(dplyr_wrapper(data = df, group_by = "id", fun = fun))
  fun = function(data) list(mean_x = mean(data$x), x2 = "test")
  y = dplyr_wrapper(df, group_by = "id", fun)
  expect_equal(y$mean_x, c(mean(1:10), mean(11:20)))
  expect_equal(y$x2, c("test", "test"))
})
