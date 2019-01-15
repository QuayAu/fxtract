context("project")

test_that("initialize", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  expect_true(dir.exists("projects"))
  expect_true(dir.exists("projects/my_project"))
  expect_true(dir.exists("projects/my_project/reg"))
  expect_true(dir.exists("projects/my_project/rds_files"))
  expect_true(dir.exists("projects/my_project/rds_files/data"))
  expect_true(dir.exists("projects/my_project/rds_files/features"))

  #test second project
  y = Project$new(project_name = "my_project2")
  expect_true(dir.exists("projects"))
  expect_true(dir.exists("projects/my_project2"))
  expect_true(dir.exists("projects/my_project2/reg"))
  expect_true(dir.exists("projects/my_project2/rds_files"))
  expect_true(dir.exists("projects/my_project2/rds_files/data"))
  expect_true(dir.exists("projects/my_project2/rds_files/features"))

  #test same project name
  expect_error(Project$new(project_name = "my_project"))

  #test wrong input
  expect_error(Project$new(1), regexp = "Assertion on 'project_name' failed: Must be of type 'character', not 'double'.")

  #check R6 slots
  expect_equal(x$project_name, "my_project")
  expect_equal(x$dir, "projects/my_project")
  expect_true(is.null(x$group_by))

  #test loading project
  z = Project$new("my_project", load = TRUE)
  expect_equal(z, x)

  #test print
  y = capture.output(x$print())
  expect_equal(y[6], "Percentage calculated: 0%")

  unlink("projects", recursive = TRUE)

})

test_that("add_data", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")

  #test wrong inputs
  expect_error(x$add_data(iris))
  expect_error(x$add_data(iris, group_by = "test"))

  #test right data and get_data
  x$add_data(iris, group_by = "Species")
  expect_true(file.exists("projects/my_project/rds_files/data/setosa.RDS"))
  expect_true(file.exists("projects/my_project/rds_files/data/versicolor.RDS"))
  expect_true(file.exists("projects/my_project/rds_files/data/virginica.RDS"))
  expect_equal(iris, x$get_data(x$datasets))
  expect_equal(x$group_by, "Species")
  expect_equal(x$datasets, c("setosa", "versicolor", "virginica"))

  #test second dataframe different group by error
  expect_error(x$add_data(iris, group_by = "Petal.Length"), regexp = "The group_by variable was set to Species. Only one group_by variable is allowed per project!")

  #test active bindings
  expect_equal(x$datasets, c("setosa", "versicolor", "virginica"))

  unlink("projects", recursive = TRUE)
})

test_that("preprocess_data", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  x$add_data(iris, group_by = "Species")

  fun2 = function(data) {
    c(sum = data$new_col[1])
  }
  x$add_feature(fun2)
  x$calc_features()
  expect_equal(nrow(batchtools::findErrors(reg = x$reg)), 3)

  #preprocess data
  fun = function(data) {
    data$new_col = max(data$Sepal.Length) + max(data$Petal.Length)
    data
  }
  x$preprocess_data(fun = fun)

  #test right data and get_data
  expect_true(file.exists("projects/my_project/rds_files/data/setosa.RDS"))
  expect_true(file.exists("projects/my_project/rds_files/data/versicolor.RDS"))
  expect_true(file.exists("projects/my_project/rds_files/data/virginica.RDS"))
  iris2 = x$get_data(datasets = x$datasets)
  iris2 = x$get_data()

  iris3 = iris %>% dplyr::group_by(Species) %>% dplyr::mutate(new_col = max(Sepal.Length) + max(Petal.Length)) %>% data.frame()
  expect_equal(iris3, iris2)

  #test updated batchtools problems
  x$calc_features()
  expect_equal(x$collect_results()$sum, c(7.7, 12.1, 14.8))

  unlink("projects", recursive = TRUE)
})

test_that("remove_data", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  x$add_data(iris, group_by = "Species")
  x$remove_data("setosa")
  expect_equal(list.files(paste0(x$dir, "/rds_files/data/")), c("versicolor.RDS", "virginica.RDS"))
  expect_equal(x$datasets, c("versicolor", "virginica"))
  x$remove_data(c("versicolor", "virginica"))
  expect_equal(list.files(paste0(x$dir, "/rds_files/data/")), character(0))
  expect_equal(x$datasets, character(0))
})

test_that("add_feature", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
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
  expect_true(file.exists("projects/my_project/rds_files/features/sepal_length_fun.RDS"))
  expect_true(file.exists("projects/my_project/rds_files/features/sepal_width_fun.RDS"))

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
})

test_that("calculate features", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
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

  #test meaningful error message $collect_results()
  expect_error(x$collect_results(), regexp = "No features have been calculated yet.")
  expect_error(x$get_project_status())

  #test submitting jobs by batchtools
  batchtools::submitJobs(1:2, reg = x$reg)
  expect_equal(x$get_project_status()$perc_done, 1/3)
  expect_equal(x$perc_done, 1/3)
  y = capture.output(x)
  expect_equal(y[6], "Percentage calculated: 33%")

  #test submitting jobs by R6 method
  x$calc_features()
  expect_equal(x$get_project_status()$perc_done, 1)
  res = x$collect_results()
  expect_true(!anyNA(res))
  cn = c(names(sepal_length_fun(iris)), names(sepal_width_fun(iris)))
  expect_equal(colnames(res[, -which(colnames(res) == "Species")]), cn)

  #test load data and continue calculate
  x$remove_feature(sepal_length_fun)
  x$add_feature(sepal_length_fun)
  y = Project$new("my_project", load = TRUE)
  y$calc_features()
  expect_equal(y$get_project_status()$perc_done, 1)
  res = y$collect_results()
  expect_true(!anyNA(res))
  cn = c(names(sepal_length_fun(iris)), names(sepal_width_fun(iris)))
  expect_equal(colnames(res[, -which(colnames(res) == "Species")]), cn)
})
