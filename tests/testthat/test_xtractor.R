context("Xtractor")

test_that("initialize", {
  unlink("fxtract_files", recursive = TRUE)
  x = Xtractor$new(name = "xtractor")
  expect_true(dir.exists("fxtract_files"))
  expect_true(dir.exists("fxtract_files/xtractor"))
  expect_true(dir.exists("fxtract_files/xtractor/reg"))
  expect_true(dir.exists("fxtract_files/xtractor/rds_files"))
  expect_true(dir.exists("fxtract_files/xtractor/rds_files/data"))
  expect_true(dir.exists("fxtract_files/xtractor/rds_files/features"))

  #test second xtractor
  y = Xtractor$new(name = "xtractor2")
  expect_true(dir.exists("fxtract_files"))
  expect_true(dir.exists("fxtract_files/xtractor2"))
  expect_true(dir.exists("fxtract_files/xtractor2/reg"))
  expect_true(dir.exists("fxtract_files/xtractor2/rds_files"))
  expect_true(dir.exists("fxtract_files/xtractor2/rds_files/data"))
  expect_true(dir.exists("fxtract_files/xtractor2/rds_files/features"))

  #test same xtractor name
  expect_error(Xtractor$new(name = "xtractor"))

  #test wrong input
  expect_error(Xtractor$new(1), regexp = "Assertion on 'name' failed: Must be of type 'character', not 'double'.")

  #check R6 slots
  expect_equal(x$name, "xtractor")
  expect_equal(x$dir, "fxtract_files/xtractor")
  expect_true(is.null(x$group_by))

  #test loading xtractor
  z = Xtractor$new("xtractor", load = TRUE)
  expect_equal(z, x)

  #test print
  y = capture.output(x$print())
  expect_equal(y[6], "Backend: batchtools")
  expect_equal(y[7], "Percentage calculated: 0%")
  x$backend = "dplyr"
  y = capture.output(x$print())
  expect_equal(y[6], "Backend: dplyr")
  expect_equal(length(y), 6)

  unlink("fxtract_files", recursive = TRUE)
})

test_that("add_data", {
  unlink("fxtract_files", recursive = TRUE)
  x = Xtractor$new(name = "xtractor")

  #test wrong inputs
  expect_error(x$add_data(iris))
  expect_error(x$add_data(iris, group_by = "test"))

  #test right data and get_data
  x$add_data(iris, group_by = "Species")
  expect_true(file.exists("fxtract_files/xtractor/rds_files/data/setosa.RDS"))
  expect_true(file.exists("fxtract_files/xtractor/rds_files/data/versicolor.RDS"))
  expect_true(file.exists("fxtract_files/xtractor/rds_files/data/virginica.RDS"))
  expect_equal(iris, x$get_data(x$datasets))
  expect_equal(x$group_by, "Species")
  expect_equal(x$datasets, c("setosa", "versicolor", "virginica"))

  #test second dataframe different group by error
  expect_error(x$add_data(iris, group_by = "Petal.Length"), regexp = "The group_by variable was set to Species.")

  #test active bindings
  expect_equal(x$datasets, c("setosa", "versicolor", "virginica"))

  #test print with many datasets
  for (i in 1:10) {
    iris2 = iris
    iris2$Species = paste0(iris2$Species, i)
    x$add_data(iris2, group_by = "Species")
  }
  y = capture.output(x$print())
  expect_true(" ..." %in% strsplit(y[4], ",")[[1]])
  expect_false(" virginica7" %in% strsplit(y[4], ",")[[1]])

  #test adding numeric grouping variable
  iris3 = iris
  iris3$Species = as.numeric(iris3$Species)
  x$add_data(iris3, group_by = "Species")

  unlink("fxtract_files", recursive = TRUE)
})

test_that("preprocess_data", {
  unlink("fxtract_files", recursive = TRUE)
  x = Xtractor$new(name = "xtractor")
  x$add_data(iris, group_by = "Species")

  fun2 = function(data) {
    c(sum = data$new_col[1])
  }
  x$add_feature(fun2)
  x$calc_features()
  expect_equal(nrow(x$error_messages), 3)

  #preprocess data
  fun = function(data) {
    data$new_col = max(data$Sepal.Length) + max(data$Petal.Length)
    data
  }
  x$preprocess_data(fun = fun)

  #test right data and get_data
  expect_true(file.exists("fxtract_files/xtractor/rds_files/data/setosa.RDS"))
  expect_true(file.exists("fxtract_files/xtractor/rds_files/data/versicolor.RDS"))
  expect_true(file.exists("fxtract_files/xtractor/rds_files/data/virginica.RDS"))
  iris2 = x$get_data(datasets = x$datasets)
  iris3 = iris %>% dplyr::group_by(Species) %>% dplyr::mutate(new_col = max(Sepal.Length) + max(Petal.Length)) %>% data.frame()
  expect_equal(iris3, iris2)

  iris2 = x$get_data()
  expect_equal(iris3, iris2)

  #test updated batchtools problems
  x$calc_features()
  expect_equal(x$results$sum, c(7.7, 12.1, 14.8))

  unlink("fxtract_files", recursive = TRUE)
})

test_that("remove_data", {
  unlink("fxtract_files", recursive = TRUE)
  x = Xtractor$new(name = "xtractor")
  x$add_data(iris, group_by = "Species")
  x$remove_data("setosa")
  expect_equal(list.files(paste0(x$dir, "/rds_files/data/")), c("versicolor.RDS", "virginica.RDS"))
  expect_equal(x$datasets, c("versicolor", "virginica"))
  x$remove_data(c("versicolor", "virginica"))
  expect_equal(list.files(paste0(x$dir, "/rds_files/data/")), character(0))
  expect_equal(x$datasets, character(0))
  unlink("fxtract_files", recursive = TRUE)
})

test_that("add_feature", {
  unlink("fxtract_files", recursive = TRUE)
  x = Xtractor$new(name = "xtractor")
  x$add_data(iris, group_by = "Species")

  sepal_length_fun = function(data) {
    c(mean_sepal_length = mean(data$Sepal.Length),
      max_sepal_length = max(data$Sepal.Length),
      sd_sepal_length = sd(data$Sepal.Length)
    )
  }
  sepal_width_fun = function(data) {
    c(mean_sepal_width = mean(data$Sepal.Width),
      max_sepal_width = max(data$Sepal.Width),
      sd_sepal_width = sd(data$Sepal.Width)
    )
  }

  x$add_feature(sepal_length_fun)
  x$add_feature(sepal_width_fun)
  expect_true(file.exists("fxtract_files/xtractor/rds_files/features/sepal_length_fun.RDS"))
  expect_true(file.exists("fxtract_files/xtractor/rds_files/features/sepal_width_fun.RDS"))

  expect_equal(x$features, c("sepal_length_fun", "sepal_width_fun"))

  #test remove features as function
  x$remove_feature(sepal_length_fun)
  expect_equal(x$features, c("sepal_width_fun"))

  #test remove features as character
  x$add_feature(sepal_length_fun)
  x$remove_feature("sepal_length_fun")
  expect_equal(x$features, c("sepal_width_fun"))

  #test remove wrong feature
  expect_error(x$remove_feature("sepal_width_fun2"), regexp = "Assertion on 'fun' failed: Must be a subset of")

  #test get_feature
  x$add_feature(sepal_length_fun)

  y = x$get_feature("sepal_width_fun")
  expect_equal(y, sepal_width_fun)
  y = x$get_feature("sepal_length_fun")
  expect_equal(y, sepal_length_fun)

  expect_error(x$get_feature("abc"), regexp = "Assertion on 'fun' failed: Must be a subset of")

  #test print with many functions
  for (i in 1:20) {
    eval(parse(text = paste0("fun", i, " = sepal_length_fun")))
    eval(parse(text = paste0("x$add_feature(fun", i, ")")))
  }
  y = capture.output(x$print())
  expect_true(" ..." %in% strsplit(y[5], ",")[[1]])
  expect_false(" fun19" %in% strsplit(y[4], ",")[[1]])

  unlink("fxtract_files", recursive = TRUE)
})

test_that("calculate features", {
  unlink("fxtract_files", recursive = TRUE)
  x = Xtractor$new(name = "xtractor")
  x$add_data(iris, group_by = "Species")

  sepal_length_fun = function(data) {
    c(mean_sepal_length = mean(data$Sepal.Length),
      max_sepal_length = max(data$Sepal.Length),
      sd_sepal_length = sd(data$Sepal.Length)
    )
  }
  sepal_width_fun = function(data) {
    c(mean_sepal_width = mean(data$Sepal.Width),
      max_sepal_width = max(data$Sepal.Width),
      sd_sepal_width = sd(data$Sepal.Width)
    )
  }

  x$add_feature(sepal_length_fun)
  x$add_feature(sepal_width_fun)

  #test meaningful error message $results
  expect_error(x$results, regexp = "No features have been calculated yet.")
  expect_error(x$status)

  #test submitting jobs by batchtools
  batchtools::submitJobs(1:2, reg = x$reg)
  expect_equal(x$status$perc_done, 1/3)
  expect_equal(x$perc_done, 1/3)
  y = capture.output(x)
  expect_equal(y[7], "Percentage calculated: 33%")

  #test submitting jobs by R6 method
  x$calc_features()
  expect_equal(x$status$perc_done, 1)
  res = x$results
  expect_true(!anyNA(res))
  cn = c(names(sepal_length_fun(iris)), names(sepal_width_fun(iris)))
  expect_equal(colnames(res[, -which(colnames(res) == "Species")]), cn)

  #test load data and continue calculate
  x$remove_feature(sepal_length_fun)
  x$add_feature(sepal_length_fun)
  y = Xtractor$new("xtractor", load = TRUE)
  y$calc_features()
  expect_equal(y$status$perc_done, 1)
  res = y$results
  expect_true(!anyNA(res))
  cn = c(names(sepal_length_fun(iris)), names(sepal_width_fun(iris)))
  expect_equal(colnames(res[, -which(colnames(res) == "Species")]), cn)

  unlink("fxtract_files", recursive = TRUE)
})

test_that("error handling", {
  unlink("fxtract_files", recursive = TRUE)
  x = Xtractor$new(name = "xtractor")
  expect_error(x$calc_features(), regexp = "Please add datasets with method")
  x$add_data(iris, group_by = "Species")
  expect_error(x$calc_features(), regexp = "Please add feature functions with method")

  fun1 = function(data) {
    if ("versicolor" %in% data$Species) stop("fun1 not compatible on versicolor")
    c(mean_sepal_length = mean(data$Sepal.Length),
      sd_sepal_length = sd(data$Sepal.Length))
  }

  fun2 = function(data) {
    if ("virginica" %in% data$Species) stop("fun2 not compatible on virginica")
    c(mean_petal_length = mean(data$Petal.Length),
      sd_petal_length = sd(data$Petal.Length))
  }

  x$add_feature(fun1)
  x$add_feature(fun2)

  x$calc_features()
  expect_equal(x$error_messages$error[1], "Error in fun(data) : fun1 not compatible on versicolor")
  expect_equal(x$error_messages$error[2], "Error in fun(data) : fun2 not compatible on virginica")
  expect_equal(nrow(x$error_messages), 2)
  expect_equal(length(x$log_files), 2)

  unlink("fxtract_files", recursive = TRUE)
})

test_that("change backend", {
  unlink("fxtract_files", recursive = TRUE)
  x = Xtractor$new(name = "xtractor")
  expect_error(x$calc_features(), regexp = "Please add datasets with method")
  x$add_data(iris, group_by = "Species")
  expect_error(x$calc_features(), regexp = "Please add feature functions with method")

  fun1 = function(data) {
    c(mean_sepal_length = mean(data$Sepal.Length),
      sd_sepal_length = sd(data$Sepal.Length))
  }

  fun2 = function(data) {
    c(mean_petal_length = mean(data$Petal.Length),
      sd_petal_length = sd(data$Petal.Length))
  }

  x$add_feature(fun1)
  x$add_feature(fun2)

  expect_error(x$backend <- "x", regexp = "Assertion on 'backend' failed.")
  expect_error(x$backend <- 5, regexp = "Assertion on 'backend' failed: Must be of type")

  x$backend = "dplyr"
  expect_error(x$error_messages, regexp = "This slot is only available if backend is set to 'batchtools'.")
  expect_error(x$status, regexp = "This slot is only available if backend is set to 'batchtools'.")
  expect_error(x$log_files, regexp = "This slot is only available if backend is set to 'batchtools'.")
  expect_error(x$perc_done, regexp = "This slot is only available if backend is set to 'batchtools'.")
  res_dplyr = x$results


  x$backend = "batchtools"
  x$calc_features()
  expect_message(x$results, "Calculating results from batchtools registry")
  expect_message(x$results, "No new results found. Returning last generated results:")
  res_batchtools = x$results
  expect_equal(res_dplyr, res_batchtools)

  unlink("fxtract_files", recursive = TRUE)
})

test_that("wrong function returns", {
  unlink("fxtract_files", recursive = TRUE)
  x = Xtractor$new(name = "xtractor")
  expect_error(x$calc_features(), regexp = "Please add datasets with method")
  x$add_data(iris, group_by = "Species")
  expect_error(x$calc_features(), regexp = "Please add feature functions with method")

  fun1 = function(data) {
    data.frame(mean_sepal_length = mean(data$Sepal.Length),
      sd_sepal_length = sd(data$Sepal.Length))
  }

  fun2 = function(data) {
    data.frame(mean_petal_length = c(mean(data$Petal.Length), 2),
      sd_petal_length = sd(data$Petal.Length))
  }

  x$add_feature(fun1, check_fun = FALSE)
  x$add_feature(fun2, check_fun = FALSE)

  # backend batchtools
  x$calc_features()
  expect_false(nrow(x$results) == 3)
  x$remove_feature(fun1)
  x$remove_feature("fun2")

  x$add_feature(fun1, check_fun = TRUE)
  x$add_feature(fun2, check_fun = TRUE)
  x$calc_features()
  expect_equal(nrow(x$error_messages), 6)

  # backend dplyr
  x$backend = "dplyr"
  x$remove_feature(fun1)
  x$remove_feature("fun2")
  x$add_feature(fun1, check_fun = FALSE)
  x$add_feature(fun2, check_fun = FALSE)
  x$calc_features()
  expect_false(nrow(x$results) == 3)
  x$remove_feature(fun1)
  x$remove_feature("fun2")
  x$add_feature(fun1, check_fun = TRUE)
  x$add_feature(fun2, check_fun = TRUE)
  expect_error(x$calc_features(), "task 1 failed -")

  unlink("fxtract_files", recursive = TRUE)
})

test_that("right function returns", {
  unlink("fxtract_files", recursive = TRUE)
  x = Xtractor$new(name = "xtractor")
  expect_error(x$calc_features(), regexp = "Please add datasets with method")
  x$add_data(iris, group_by = "Species")
  expect_error(x$calc_features(), regexp = "Please add feature functions with method")

  fun1 = function(data) {
    c(mean_sepal_length = mean(data$Sepal.Length),
      sd_sepal_length = sd(data$Sepal.Length))
  }

  fun2 = function(data) {
    list(mean_petal_length = c(mean(data$Petal.Length), 1),
      sd_petal_length = sd(data$Petal.Length))
  }

  x$add_feature(fun1, check_fun = TRUE)
  x$add_feature(fun2, check_fun = TRUE)

  # test wrong function
  x$calc_features()
  expect_true(nrow(x$error_messages) == 3)

  x$backend = "dplyr"
  expect_error(x$calc_features())

  #test right function
  x$backend = "batchtools"
  x$remove_feature(fun2)
  fun2 = function(data) {
    list(mean_petal_length = mean(data$Petal.Length),
      sd_petal_length = sd(data$Petal.Length))
  }
  x$add_feature(fun2)
  x$calc_features()
  expect_true(nrow(x$error_messages) == 0)
  res_batchtools = x$results

  x$backend = "dplyr"
  x$calc_features()
  expect_equal(x$results, res_batchtools)

  unlink("fxtract_files", recursive = TRUE)
})
