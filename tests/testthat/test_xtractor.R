context("Xtractor")

test_that("initialize", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
  expect_true(dir.exists(file.path(dir, "fxtract_files")))
  expect_true(dir.exists(file.path(dir, "fxtract_files", "xtractor")))
  expect_true(dir.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files")))
  expect_true(dir.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files", "data")))
  expect_true(dir.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files", "features")))
  expect_true(dir.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results")))

  #test second xtractor
  y = Xtractor$new(name = "xtractor2", file.dir = dir)
  expect_true(dir.exists(file.path(dir, "fxtract_files")))
  expect_true(dir.exists(file.path(dir, "fxtract_files", "xtractor2")))
  expect_true(dir.exists(file.path(dir, "fxtract_files", "xtractor2", "rds_files")))
  expect_true(dir.exists(file.path(dir, "fxtract_files", "xtractor2", "rds_files", "data")))
  expect_true(dir.exists(file.path(dir, "fxtract_files", "xtractor2", "rds_files", "features")))
  expect_true(dir.exists(file.path(dir, "fxtract_files", "xtractor2", "rds_files", "results")))

  #test same xtractor name
  expect_error(Xtractor$new(name = "xtractor", file.dir = dir))

  #test wrong input
  expect_error(Xtractor$new(1), regexp = "Assertion on 'name' failed: Must be of type 'character', not 'double'.")

  #test loading xtractor
  z = Xtractor$new(name = "xtractor", file.dir = dir, load = TRUE)
  expect_equal(z, x)

  #test print
  y = capture.output(x$print())
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("add_data", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)

  #test wrong inputs
  expect_error(x$add_data(iris))
  expect_error(x$add_data(iris, group_by = "test"))

  #test right data and get_data (data.frame)
  x$add_data(iris, group_by = "Species")
  expect_true(file.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files", "data", "setosa.RDS")))
  expect_true(file.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files", "data", "versicolor.RDS")))
  expect_true(file.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files", "data", "virginica.RDS")))
  expect_equal(iris, x$get_data(x$ids))
  expect_equal(x$ids, c("setosa", "versicolor", "virginica"))

  #test data.table
  x2 = Xtractor$new(name = "xtractorDT", file.dir = dir)
  irisDt = data.table::copy(iris)
  irisDt = data.table::setDT(irisDt)
  x2$add_data(irisDt, group_by = "Species")
  expect_true(file.exists(file.path(dir, "fxtract_files", "xtractorDT", "rds_files", "data", "setosa.RDS")))
  expect_true(file.exists(file.path(dir, "fxtract_files", "xtractorDT", "rds_files", "data", "versicolor.RDS")))
  expect_true(file.exists(file.path(dir, "fxtract_files", "xtractorDT", "rds_files", "data", "virginica.RDS")))

  #test second dataframe different group by error
  expect_error(x$add_data(iris, group_by = "Petal.Length"), regexp = "The group_by variable was set to Species.")

  #test active bindings
  expect_equal(x$ids, c("setosa", "versicolor", "virginica"))

  #test print with many datasets
  for (i in 1:10) {
    iris2 = iris
    iris2$Species = paste0(iris2$Species, i)
    x$add_data(iris2, group_by = "Species")
  }
  y = capture.output(x$print())
  expect_equal(y[4], "Number IDs: 33. See $ids for all ids.")

  #test adding numeric grouping variable
  iris3 = iris
  iris3$Species = as.numeric(iris3$Species)
  x$add_data(iris3, group_by = "Species")

  #check get data
  expect_message(x$get_data(), "Different vector types detected")

  #add same ID - error
  expect_error(x$add_data(iris, group_by = "Species"), regexp = "Adding data multiple times is not allowed!")

  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("preprocess_data", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
  x$add_data(iris, group_by = "Species")

  fun2 = function(data) {
    c(sum = data$new_col[1])
  }
  x$add_feature(fun2)
  x$calc_features()
  expect_equal(as.character(x$error_messages$error_message[1]), "fun(data) returns NULL. Please check your feature function.")

  #preprocess data
  fun = function(data) {
    data$new_col = max(data$Sepal.Length) + max(data$Petal.Length)
    data
  }
  x$preprocess_data(fun = fun)

  #test right data and get_data
  expect_true(file.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files", "data", "setosa.RDS")))
  expect_true(file.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files", "data", "versicolor.RDS")))
  expect_true(file.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files", "data", "virginica.RDS")))
  iris2 = x$get_data(ids = x$ids)
  iris3 = iris %>% dplyr::group_by(Species) %>% dplyr::mutate(new_col = max(Sepal.Length) + max(Petal.Length)) %>% data.frame()
  expect_equal(iris3, iris2)

  iris2 = x$get_data()
  expect_equal(iris3, iris2)

  #test calc features
  x$calc_features()
  expect_equal(x$results$sum, c(7.7, 12.1, 14.8))
  expect_equal(nrow(x$error_messages), 0)
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("remove_data", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
  x$add_data(iris, group_by = "Species")
  fun = function(data) c(m_sp = mean(data$Sepal.Length))
  fun2 = function(data) {
    if ("setosa" %in% data$Species) stop("error")
    c(sd_sp = sd(data$Sepal.Length))
  }

  x$add_feature(fun)
  x$add_feature(fun2)
  x$calc_features()
  expect_equal(nrow(x$results), 3)
  expect_equal(nrow(x$error_messages), 1)
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "done", "fun")), c("setosa.RDS", "versicolor.RDS", "virginica.RDS"))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "done", "fun2")), c("versicolor.RDS", "virginica.RDS"))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "failed", "fun1")), character(0))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "failed", "fun2")), c("setosa.RDS"))

  x$remove_data("setosa")
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "data")), c("versicolor.RDS", "virginica.RDS"))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "done", "fun")), c("versicolor.RDS", "virginica.RDS"))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "done", "fun2")), c("versicolor.RDS", "virginica.RDS"))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "failed", "fun")), character(0))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "failed", "fun2")), character(0))

  expect_equal(x$ids, c("versicolor", "virginica"))
  expect_equal(nrow(x$results), 2)
  expect_equal(nrow(x$error_messages), 0)
  expect_equal(as.character(x$results$Species), c("versicolor", "virginica"))

  x$remove_data(c("versicolor", "virginica"))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "data")), character(0))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "done", "fun")), character(0))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "done", "fun2")), character(0))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "failed", "fun")), character(0))
  expect_equal(list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "failed", "fun2")), character(0))
  expect_equal(x$ids, character(0))
  expect_equal(nrow(x$results), 0)
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("add_feature", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
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

  expect_error(x$add_feature(sepal_width_fun), regexp = "Feature function 'sepal_width_fun' was already added.")
  expect_true(file.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files", "features", "sepal_length_fun.RDS")))
  expect_true(file.exists(file.path(dir, "fxtract_files", "xtractor", "rds_files", "features", "sepal_width_fun.RDS")))
  expect_true("sepal_length_fun" %in% list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "done")))
  expect_true("sepal_length_fun" %in% list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "failed")))
  expect_true("sepal_width_fun" %in% list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "done")))
  expect_true("sepal_width_fun" %in% list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "failed")))

  expect_equal(x$features, c("sepal_length_fun", "sepal_width_fun"))

  #test remove features as function
  x$remove_feature(sepal_length_fun)
  expect_equal(x$features, c("sepal_width_fun"))
  expect_false("sepal_length_fun" %in% list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "done")))
  expect_false("sepal_length_fun" %in% list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "failed")))

  #test remove features as character
  x$add_feature(sepal_length_fun)
  x$remove_feature("sepal_length_fun")
  expect_equal(x$features, c("sepal_width_fun"))
  expect_false("sepal_length_fun" %in% list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "done")))
  expect_false("sepal_length_fun" %in% list.files(file.path(dir, "fxtract_files", "xtractor", "rds_files", "results", "failed")))

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
  expect_equal(y[5], "Number feature functions: 22. See $features for all feature functions.")

  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("calculate features", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
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
  expect_equal(ncol(x$results), 1)
  expect_true(all(x$status[, -1] == "not_done"))

  #test submitting one feature function
  expect_error(x$calc_features(features = sepal_length_fun), regexp = "Assertion on 'features' failed")
  x$calc_features("sepal_length_fun")
  expect_equal(capture.output(x)[6], "Extraction done: 50%")

  #test submitting remaining jobs
  x$calc_features()
  expect_equal(capture.output(x)[6], "Extraction done: 100%")
  res = data.frame(x$results)
  expect_true(!anyNA(res))
  cn = c(names(sepal_length_fun(iris)), names(sepal_width_fun(iris)))
  expect_equal(colnames(res[, -which(colnames(res) == "Species")]), cn)

  #test load data and continue calculate
  x$remove_feature(sepal_length_fun)
  x$add_feature(sepal_length_fun)
  y = Xtractor$new("xtractor", load = TRUE, file.dir = dir)
  expect_equal(capture.output(y)[6], "Extraction done: 50%")
  y$calc_features()
  expect_equal(capture.output(y)[6], "Extraction done: 100%")

  res = y$results
  expect_true(!anyNA(res))
  cn = c(names(sepal_length_fun(iris)), names(sepal_width_fun(iris)))

  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("error handling", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
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
  expect_equal(as.character(x$error_messages$error[1]), "fun1 not compatible on versicolor")
  expect_equal(as.character(x$error_messages$error[2]), "fun2 not compatible on virginica")
  expect_equal(nrow(x$error_messages), 2)
  expect_equal(sum(is.na(x$results)), 4)
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("wrong function returns", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
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

  x$calc_features()
  expect_false(nrow(x$results) == 3)
  x$remove_feature(fun1)
  x$remove_feature("fun2")

  x$add_feature(fun1, check_fun = TRUE)
  x$add_feature(fun2, check_fun = TRUE)
  x$calc_features()
  expect_equal(nrow(x$error_messages), 6)

  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("right function returns", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
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

  #test right function
  x$remove_feature(fun2)
  fun2 = function(data) {
    list(mean_petal_length = mean(data$Petal.Length),
      sd_petal_length = sd(data$Petal.Length))
  }
  x$add_feature(fun2)
  x$calc_features()
  expect_true(nrow(x$error_messages) == 0)

  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("add new dataset after features were already calculated", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
  iris1 = iris %>% dplyr::filter(Species %in% c("virginica", "setosa"))
  iris2 = iris %>% dplyr::filter(Species %in% c("versicolor"))
  x$add_data(iris1, group_by = "Species")

  fun1 = function(data) {
    c(mean_sepal_length = mean(data$Sepal.Length),
      sd_sepal_length = sd(data$Sepal.Length))
  }

  fun2 = function(data) {
    list(mean_petal_length = mean(data$Petal.Length),
      sd_petal_length = sd(data$Petal.Length))
  }

  x$add_feature(fun1, check_fun = TRUE)
  x$add_feature(fun2, check_fun = TRUE)
  x$calc_features()
  expect_equal(strsplit(capture.output(x)[6], split = " ")[[1]][3], "100%")
  x$add_data(iris2, group_by = "Species")
  expect_equal(strsplit(capture.output(x)[6], split = " ")[[1]][3], "66.6666666666667%")
  expect_true(all(is.na(x$results[x$results$Species == "versicolor", -1])))
  x$calc_features("fun1")
  expect_equal(strsplit(capture.output(x)[6], split = " ")[[1]][3], "83.3333333333333%")
  expect_equal(nrow(x$results), 3)
  expect_true(anyNA(x$results))
  expect_message(x$calc_features(), "Feature function 'fun1' was already applied on every ID and will be skipped.")
  expect_equal(strsplit(capture.output(x)[6], split = " ")[[1]][3], "100%")
  expect_true(!anyNA(x$results))
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("test retry failed features", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
  df = data.frame(ID = 1:30)
  x$add_data(df, group_by = "ID")

  fun1 = function(data) {
    y = rnorm(1)
    if (y >= 0) stop("retryable error")
    c(rnorm1 = y)
  }

  x$add_feature(fun1, check_fun = TRUE)
  x$calc_features()
  expect_true(nrow(x$error_messages) > 0)
  res = x$results

  #test skipping failed features
  for (i in 1:10) {
    x$calc_features(retry_failed = FALSE)
    expect_equal(x$results, res)
  }

  while (nrow(x$error_messages) > 0) {
    x$calc_features(retry_failed = TRUE)
  }
  expect_true(nrow(x$error_messages) == 0)
  expect_equal(strsplit(capture.output(x)[6], split = " ")[[1]][3], "100%")
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("check fun", {
  #check one function, but not the other
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
  df = data.frame(ID = 1:3)
  x$add_data(df, group_by = "ID")

  fun1 = function(data) {
    data.frame(x = 1:2)
  }
  fun2 = function(data) {
    data.frame(x = 2:3)
  }
  x$add_feature(fun1, check_fun = TRUE)
  x$add_feature(fun2, check_fun = FALSE)

  x$calc_features()
  expect_equal(nrow(x$error_messages), 3)

  #load xtractor
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
  df = data.frame(ID = 1:3)
  x$add_data(df, group_by = "ID")

  fun1 = function(data) {
    data.frame(x = 1:2)
  }
  fun2 = function(data) {
    data.frame(x = 2:3)
  }
  x$add_feature(fun1, check_fun = TRUE)
  x$add_feature(fun2, check_fun = FALSE)

  y = Xtractor$new("xtractor", file.dir = dir, load = TRUE)
  y$calc_features()
  expect_equal(nrow(y$error_messages), 3)
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("function returns different number of features", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
  df = data.frame(ID = 1:3)
  x$add_data(df, group_by = "ID")

  fun1 = function(data) {
    if (data$ID == 1) {
      return(c(x = 1, y = 2))
    } else {
      return(c(x = 1))
    }
  }

  x$add_feature(fun1)
  x$calc_features()
  expect_equal(data.frame(x$results[, c("x", "y")]), data.frame(x = c(1, 1, 1), y = c(2, NA, NA)))
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

test_that("extract single features from single IDs", {
  dir = tempdir()
  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
  x = Xtractor$new(name = "xtractor", file.dir = dir)
  df = data.frame(ID = 1:3)
  x$add_data(df, group_by = "ID")

  fun1 = function(data) {
    c(x = 1)
  }

  fun2 = function(data) {
    c(y = 2)
  }

  x$add_feature(fun1)
  x$add_feature(fun2)

  x$calc_features(features = "fun1", ids = "1")
  expect_equal(strsplit(capture.output(x)[6], split = " ")[[1]][3], "16.6666666666667%")
  expect_equal(data.frame(x$results)$x, c(1, NA, NA))

  x$calc_features(features = "fun1", ids = "2")
  expect_equal(strsplit(capture.output(x)[6], split = " ")[[1]][3], "33.3333333333333%")
  expect_equal(data.frame(x$results)$x, c(1, 1, NA))

  x$calc_features(features = "fun1", ids = "3")
  expect_equal(strsplit(capture.output(x)[6], split = " ")[[1]][3], "50%")
  expect_equal(data.frame(x$results)$x, c(1, 1, 1))

  x$calc_features(features = "fun2", ids = "1")
  expect_equal(strsplit(capture.output(x)[6], split = " ")[[1]][3], "66.6666666666667%")
  expect_equal(data.frame(x$results)$y, c(2, NA, NA))

  #submitting remaining:
  x$calc_features()
  expect_equal(strsplit(capture.output(x)[6], split = " ")[[1]][3], "100%")
  expect_true(!any(is.na(x$results)))

  if (dir.exists(file.path(dir, "fxtract_files"))) unlink(file.path(dir, "fxtract_files"), recursive = TRUE)
})

