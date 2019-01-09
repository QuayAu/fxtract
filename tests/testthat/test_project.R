context("project")

test_that("initialize", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  expect_true(dir.exists("projects"))
  expect_true(dir.exists("projects/my_project"))
  expect_true(dir.exists("projects/my_project/reg"))
  expect_true(dir.exists("projects/my_project/rds_files"))

  #test second project
  y = Project$new(project_name = "my_project2")
  expect_true(dir.exists("projects"))
  expect_true(dir.exists("projects/my_project2"))
  expect_true(dir.exists("projects/my_project2/reg"))
  expect_true(dir.exists("projects/my_project2/rds_files"))

  #test same project name
  expect_error(Project$new(project_name = "my_project"), regexp = "The project name already exists. Please choose another name or delete the existing project and try again!")

  #test wrong input
  expect_error(Project$new(1), regexp = "Assertion on 'project_name' failed: Must be of type 'character', not 'double'.")

  #check R6 slots
  expect_equal(x$project_name, "my_project")

  #test loading project
  z = Project$new("my_project", load = TRUE)
  expect_equal(z, x)
  unlink("projects", recursive = TRUE)
})

test_that("add_data", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")

  #test wrong inputs
  expect_error(x$add_data(iris))
  expect_error(x$add_data(iris, group_by = "test"))

  #test right data
  x$add_data(iris, group_by = "Species")
  expect_true(file.exists("projects/my_project/rds_files/setosa.RDS"))
  expect_true(file.exists("projects/my_project/rds_files/versicolor.RDS"))
  expect_true(file.exists("projects/my_project/rds_files/virginica.RDS"))
  d1 = readRDS("projects/my_project/rds_files/setosa.RDS")
  d2 = readRDS("projects/my_project/rds_files/versicolor.RDS")
  d3 = readRDS("projects/my_project/rds_files/virginica.RDS")
  expect_equal(iris, rbind(d1, d2, d3))
  expect_equal(x$group_by, "Species")
  expect_equal(x$reg$problems, c("setosa", "versicolor", "virginica"))

  #test second dataframe different group by error
  expect_error(x$add_data(iris, group_by = "Petal.Length"), regexp = "The group_by variable was set to Species. Only one group_by variable is allowed per project!")

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

  #test right data
  expect_true(file.exists("projects/my_project/rds_files/setosa.RDS"))
  expect_true(file.exists("projects/my_project/rds_files/versicolor.RDS"))
  expect_true(file.exists("projects/my_project/rds_files/virginica.RDS"))
  d1 = readRDS("projects/my_project/rds_files/setosa.RDS")
  d2 = readRDS("projects/my_project/rds_files/versicolor.RDS")
  d3 = readRDS("projects/my_project/rds_files/virginica.RDS")
  iris2 = rbind(d1, d2, d3)
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
  expect_equal(list.files(paste0(x$dir, "/rds_files/")), c("versicolor.RDS", "virginica.RDS"))
  expect_equal(x$reg$problems, c("versicolor", "virginica"))
  x$remove_data(c("versicolor", "virginica"))
  expect_equal(list.files(paste0(x$dir, "/rds_files/")), character(0))
  expect_equal(x$reg$problems, character(0))
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
  expect_equal(x$reg$algorithms, c("sepal_length_fun", "sepal_width_fun"))

  #test remove features as function
  x$remove_feature(sepal_length_fun)
  expect_equal(x$reg$algorithms, c("sepal_width_fun"))

  #test remove features as character
  x$add_feature(sepal_length_fun)
  x$remove_feature("sepal_length_fun")
  expect_equal(x$reg$algorithms, c("sepal_width_fun"))

  #test remove wrong feature
  expect_error(x$remove_feature(sepal_width_fun2), regexp = "Assertion on 'fun' failed: Must be a subset of")
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
  expect_message(x$get_project_status())

  #test submitting jobs by batchtools
  batchtools::submitJobs(1:2, reg = x$reg)
  expect_equal(x$get_project_status()$perc_done, 1/3)

  #test submitting jobs by R6 method
  x$calc_features()
  expect_equal(x$get_project_status()$perc_done, 1)
  res = x$collect_results()
  expect_true(!anyNA(res))
  cn = c(names(sepal_length_fun(iris)), names(sepal_width_fun(iris)))
  expect_equal(colnames(res[, -which(colnames(res) == "Species")]), cn)
})
