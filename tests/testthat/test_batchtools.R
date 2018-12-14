context("batchtools_functions")

test_that("makeProject", {
  unlink("projects", recursive = TRUE)
  #test no project folder
  project = makeProject("my_project")
  expect_true(dir.exists("projects"))
  expect_true(dir.exists("projects/my_project"))
  expect_true(dir.exists("projects/my_project/reg"))
  expect_true(dir.exists("projects/my_project/feature_functions"))
  expect_true(dir.exists("projects/my_project/raw_rds_files"))

  #test second project
  project2 = makeProject("my_project2")
  expect_true(dir.exists("projects"))
  expect_true(dir.exists("projects/my_project2"))
  expect_true(dir.exists("projects/my_project2/reg"))
  expect_true(dir.exists("projects/my_project2/feature_functions"))
  expect_true(dir.exists("projects/my_project2/raw_rds_files"))

  #test same project name
  expect_error(makeProject("my_project"), regexp = "The project name already exists. Please choose another name or delete the existing project and try again!")

  #test loadProject
  project3 = loadProject("projects/my_project")
  expect_equal(project$dir, project3$dir)
})

test_that("save rds files", {
  skip("this one is skipped")
  #sql data base
  unlink("projects", recursive = TRUE)
  project = makeProject("my_project")
  my_database = dplyr::src_sqlite("SQL_database_iris.sql", create = TRUE)
  dplyr::copy_to(my_database, df = iris, temporary = FALSE, overwrite = TRUE)
  project = useSqlDatabase(project = project, file.dir = "SQL_database_iris.sql", tbl_name = "iris", group_by = "Species")
  expect_true(file.exists("projects/my_project/raw_rds_files/setosa.RDS"))
  expect_true(file.exists("projects/my_project/raw_rds_files/versicolor.RDS"))
  expect_true(file.exists("projects/my_project/raw_rds_files/virginica.RDS"))
  project = loadProject(project$dir)
  expect_equal(project$group_by, "Species")
  ## test wrong group_by argument
  unlink("projects", recursive = TRUE)
  project = makeProject("my_project")
  my_database = dplyr::src_sqlite("SQL_database_iris.sql", create = TRUE)
  dplyr::copy_to(my_database, df = iris, temporary = FALSE, overwrite = TRUE)
  expect_error(useSqlDatabase(project = project, file.dir = "SQL_database_iris.sql", tbl_name = "iris", group_by = "Species2"))

  # RPostgreSQL::dbDisconnect(my_database$con)
  # file.remove("SQL_database_iris.sql")

  #dataframe
  unlink("projects", recursive = TRUE)
  project = makeProject("my_project")
  useDataframe(project, iris, group_by = "Species")
  expect_true(file.exists("projects/my_project/raw_rds_files/setosa.RDS"))
  expect_true(file.exists("projects/my_project/raw_rds_files/versicolor.RDS"))
  expect_true(file.exists("projects/my_project/raw_rds_files/virginica.RDS"))
  project = loadProject(project$dir)
  expect_equal(project$group_by, "Species")

  ## test wrong group_by argument
  unlink("projects", recursive = TRUE)
  project = makeProject("my_project2")
  expect_error(useDataframe(project, iris, group_by = "Species2"))
})

test_that("add batchtools problems", {

})
