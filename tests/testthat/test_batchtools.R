context("batchtools_functions")

test_that("makeProject", {
  unlink("projects", recursive = TRUE)
  #test no project folder
  project = makeProject("my_project", group_by = "userId")
  expect_true(dir.exists("projects"))
  expect_true(dir.exists("projects/my_project"))
  expect_true(dir.exists("projects/my_project/reg"))
  expect_true(dir.exists("projects/my_project/feature_functions"))
  expect_true(dir.exists("projects/my_project/raw_rds_files"))

  #test second project
  project2 = makeProject("my_project2", group_by = "userId")
  expect_true(dir.exists("projects"))
  expect_true(dir.exists("projects/my_project2"))
  expect_true(dir.exists("projects/my_project2/reg"))
  expect_true(dir.exists("projects/my_project2/feature_functions"))
  expect_true(dir.exists("projects/my_project2/raw_rds_files"))

  #test same project name
  expect_error(makeProject("my_project", group_by = "userId"), regexp = "The project name already exists. Please choose another name or delete the existing project and try again!")

  #test loadProject
  project3 = loadProject("projects/my_project")
  expect_equal(project$dir, project3$dir)
  expect_equal(project$group_by, project3$group_by)
})

test_that("save rds files", {
  skip("this one is skipped")
  #sql data base
  unlink("projects", recursive = TRUE)
  project = makeProject("my_project", group_by = "Species")
  my_database = dplyr::src_sqlite("SQL_database_iris.sql", create = TRUE)
  dplyr::copy_to(my_database, df = iris, temporary = FALSE, overwrite = TRUE)
  sqlToRds(project = project, file.dir = "SQL_database_iris.sql", tbl_name = "iris")
  expect_true(file.exists("projects/my_project/raw_rds_files/setosa.RDS"))
  expect_true(file.exists("projects/my_project/raw_rds_files/versicolor.RDS"))
  expect_true(file.exists("projects/my_project/raw_rds_files/virginica.RDS"))

  ## test wrong group_by argument
  unlink("projects", recursive = TRUE)
  project = makeProject("my_project", group_by = "Species2")
  my_database = dplyr::src_sqlite("SQL_database_iris.sql", create = TRUE)
  dplyr::copy_to(my_database, df = iris, temporary = FALSE, overwrite = TRUE)
  expect_error(sqlToRds(project = project, file.dir = "SQL_database_iris.sql", tbl_name = "iris"))

  # RPostgreSQL::dbDisconnect(my_database$con)
  # file.remove("SQL_database_iris.sql")

  #dataframe
  unlink("projects", recursive = TRUE)
  project = makeProject("my_project", group_by = "Species")
  dataframeToRds(project, iris)
  expect_true(file.exists("projects/my_project/raw_rds_files/setosa.RDS"))
  expect_true(file.exists("projects/my_project/raw_rds_files/versicolor.RDS"))
  expect_true(file.exists("projects/my_project/raw_rds_files/virginica.RDS"))

  ## test wrong group_by argument
  unlink("projects", recursive = TRUE)
  project = makeProject("my_project2", group_by = "Species2")
  expect_error(dataframeToRds(project, iris))
})

test_that("add batchtools problems", {

})
