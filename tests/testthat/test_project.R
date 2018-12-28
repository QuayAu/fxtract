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
