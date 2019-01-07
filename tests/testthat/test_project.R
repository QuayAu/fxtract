context("project")

test_that("initialize", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  expect_true(dir.exists("projects"))
  expect_true(dir.exists("projects/my_project"))
  expect_true(dir.exists("projects/my_project/reg"))
  expect_true(dir.exists("projects/my_project/raw_rds_files"))
  expect_true(dir.exists("projects/my_project/batchtools_algorithms"))
  expect_true(dir.exists("projects/my_project/batchtools_problems"))

  #test second project
  y = Project$new(project_name = "my_project2")
  expect_true(dir.exists("projects"))
  expect_true(dir.exists("projects/my_project2"))
  expect_true(dir.exists("projects/my_project2/reg"))
  expect_true(dir.exists("projects/my_project2/raw_rds_files"))
  expect_true(dir.exists("projects/my_project2/batchtools_algorithms"))
  expect_true(dir.exists("projects/my_project2/batchtools_problems"))

  #test same project name
  expect_error(Project$new(project_name = "my_project"), regexp = "The project name already exists. Please choose another name or delete the existing project and try again!")

  #test wrong input
  expect_error(Project$new(1), regexp = "Assertion on 'project_name' failed: Must be of type 'character', not 'double'.")

  #check R6 slots
  expect_equal(x$dir, "projects/my_project")
  expect_equal(x$project_name, "my_project")
  unlink("projects", recursive = TRUE)
})

test_that("use_dataframe", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")

  #test wrong inputs
  expect_error(x$use_dataframe(iris))
  expect_error(x$use_dataframe(iris, group_by = "test"))

  #test right data
  x$use_dataframe(iris, group_by = "Species")
  expect_true(file.exists("projects/my_project/raw_rds_files/setosa.RDS"))
  expect_true(file.exists("projects/my_project/raw_rds_files/versicolor.RDS"))
  expect_true(file.exists("projects/my_project/raw_rds_files/virginica.RDS"))
  d1 = readRDS("projects/my_project/raw_rds_files/setosa.RDS")
  d2 = readRDS("projects/my_project/raw_rds_files/versicolor.RDS")
  d3 = readRDS("projects/my_project/raw_rds_files/virginica.RDS")
  expect_equal(iris, rbind(d1, d2, d3))
  expect_equal(x$group_by, "Species")

  #test second dataframe different group by error
  expect_error(x$use_dataframe(iris, group_by = "Petal.Length"), regexp = "The group_by variable was set to Species. Only one group_by variable is allowed per project!")

  unlink("projects", recursive = TRUE)
})

test_that("add_batchtools_problems", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  x$use_dataframe(iris, group_by = "Species")
  x$add_batchtools_problems()
  expect_equal(x$reg$problems, c("setosa", "versicolor", "virginica"))

  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  x$use_dataframe(iris, group_by = "Species")
  x$add_batchtools_problems(n.chunks = 1)
  expect_equal(x$reg$problems, c("chunk_1"))

  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  x$use_dataframe(iris, group_by = "Species")
  x$add_batchtools_problems(n.chunks = 2)
  expect_setequal(x$reg$problems, c("chunk_1", "chunk_2"))

  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  x$use_dataframe(iris, group_by = "Species")
  x$add_batchtools_problems(n.chunks = 3)
  expect_setequal(x$reg$problems, c("chunk_1", "chunk_2", "chunk_3"))

  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  x$use_dataframe(iris, group_by = "Species")
  expect_error(x$add_batchtools_problems(n.chunks = 4), regexp = "n.chunks > number of different grouping variables!")
})

test_that("remove_batchtools_problem", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  x$use_dataframe(iris, group_by = "Species")
  x$add_batchtools_problems()
  x$remove_batchtools_problem("projects/my_project/batchtools_problems/setosa")
  expect_equal(x$reg$problems, c("versicolor", "virginica"))
  expect_equal(list.files("projects/my_project/batchtools_problems/"), c("versicolor", "virginica"))
  x$remove_batchtools_problem("versicolor")
  expect_equal(x$reg$problems, c("virginica"))
  expect_equal(list.files("projects/my_project/batchtools_problems/"), c("virginica"))
})

test_that("add_batchtools_algorithm", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  x$use_dataframe(iris, group_by = "Species")
  x$add_batchtools_problems()

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

  x$add_batchtools_algorithm(sepal_length_fun)
  x$add_batchtools_algorithm(sepal_width_fun)
  expect_equal(x$reg$algorithms, c("sepal_length_fun", "sepal_width_fun"))

  #test remove features
  x$remove_batchtools_algorithm("projects/my_project/batchtools_algorithms/sepal_length_fun")
  expect_equal(x$reg$algorithms, c("sepal_width_fun"))

  x$add_batchtools_algorithm(sepal_length_fun)
  x$remove_batchtools_algorithm("sepal_length_fun")
  expect_equal(x$reg$algorithms, c("sepal_width_fun"))
})

test_that("calculate features", {
  unlink("projects", recursive = TRUE)
  x = Project$new(project_name = "my_project")
  x$use_dataframe(iris, group_by = "Species")
  x$add_batchtools_problems()

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

  x$add_batchtools_algorithm(sepal_length_fun)
  x$add_batchtools_algorithm(sepal_width_fun)

  #test meaningful error message $collect_results()
  expect_error(x$collect_results(), regexp = "No features have been calculated yet. Start calculating with method submit_jobs().")

  #test submitting jobs by batchtools
  batchtools::submitJobs(1:2, reg = x$reg)
  expect_equal(x$get_project_status()$perc_done, 1/3)

  #test submitting jobs by R6 method
  x$submit_jobs(3:4)
  expect_equal(x$get_project_status()$perc_done, 2/3)

  #calculate rest
  x$submit_jobs()
  expect_equal(x$get_project_status()$perc_done, 1)
  res = x$collect_results()
  expect_true(!anyNA(res))
  cn = c(names(sepal_length_fun(iris)), names(sepal_width_fun(iris)))
  expect_equal(colnames(res[, -which(colnames(res) == "Species")]), cn)
})


