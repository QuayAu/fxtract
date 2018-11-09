context("feature_functions")

test_that("calcFeatureByGroup", {
  df = data.frame(id = c(rep(1, 10), rep(2, 10)))
  df$task = rep(c(rep("task1", 5), rep("task2", 5)), 2)
  df$hour = rep(c(rep("hour1", 3), rep("hour2", 2), rep("hour1", 2), rep("hour2", 3)), 2)
  df$x = 1:20
  
  #test checks
  fun = function(data) data$x
  expect_error(calcFeatureByGroup(data = df, group_col = "id", fun = fun),
    regexp = "fun must return a vector of length 1 or a dataframe with 1 row!")

  #return 1 value
  mean_x = function(data) mean(data$x)
  y = calcFeatureByGroup(data = df, group_col = "id", fun = mean_x)
  expect_equal(y$mean_x, c(mean(1:10), mean(11:20)))
  
  #return 1 row of data
  more_features = function(data) data.frame(mean_x = mean(data$x), sd_x = sd(data$x), min_x = min(data$x))
  y = calcFeatureByGroup(data = df, group_col = "id", fun = more_features)
  expect_equal(y$mean_x, c(mean(1:10), mean(11:20)))
  expect_equal(y$sd_x, c(sd(1:10), sd(11:20)))
  expect_equal(y$min_x, c(min(1:10), min(11:20)))

  #group by 2 columns
  y = calcFeatureByGroup(data = df, group_col = c("id", "task"), fun = more_features)
  expect_equal(dim(y), c(2, 7))
  
  #group by 3 columns
  y = calcFeatureByGroup(data = df, group_col = c("id", "task", "hour"), fun = more_features)
  expect_equal(dim(y), c(2, 3 * 4 + 1))
  
  #group by 4 columns
  df$y = c(rep(1, 2), rep(2, 2))
  y = calcFeatureByGroup(data = df, group_col = c("id", "task", "hour", "y"), fun = more_features)
  expect_equal(dim(y), c(2, 3 * 4 * 2 + 1))
})

