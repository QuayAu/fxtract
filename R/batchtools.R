#' Iniates batchtools functionality
#'
#' This function iniates batchtools functionality of this package.
#' Check the tutorial on the github page!
#' Every desired function will be calculated for each member
#' of a grouping variable (e.g. each participant of a study).
#' The data will be saved as an RDS-file for each member of a grouping variable.
#' This enables the possibility to calculate features even if all raw data cannot be loaded into RAM.
#' Feature extraction can be paused and restarted at a later time, even on a different machine.
#' Parallelization can be done via \cite{batchtools}.
#' @param newProjectName character. The new project name. A subfolder under the (created) folder `projects` will be created.
#' @param ... other arguments passed to \code{batchtools::makeExperimentRegistry}
#' @return fxtract_project object.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \dontrun{
#' project = makeProject("my_project")
#' }
makeProject = function(newProjectName, ...){
  checkmate::assertCharacter(newProjectName)
  newDirPath = paste0("projects/", newProjectName)
  if (dir.exists("projects")) print("The project folder 'projects' already exists. The new project will be saved in this folder")
  if (!dir.exists("projects")) dir.create("projects")
  if (dir.exists(newDirPath)) stop("The project name already exists. Please choose another name or delete the existing project and try again!")
  dir.create(newDirPath)
  dir.create(paste0(newDirPath, "/feature_functions"))
  dir.create(paste0(newDirPath, "/raw_rds_files"))
  project = list(dir = newDirPath)
  reg = batchtools::makeExperimentRegistry(paste0(project$dir, "/reg"), ...)
  project[["reg"]] = reg
  class(project) = c("fxtract_project", class(project))
  project
}

#' Loads fxtract project.
#'
#' In order to restart feature extraction, the fxtract project needs to be loaded with this function.
#' @param file.dir character. The project's directory.
#' @param group_by character. The grouping variable needs to be defined again.
#' @return fxtract_project object
#' @export
#' @examples
#' \dontrun{
#' project = loadProject("projects/my_project", group_by = "user")
#' }
loadProject = function(file.dir, group_by) {
  checkmate::assertTRUE(file.exists(paste0(file.dir, "/feature_functions")))
  checkmate::assertTRUE(file.exists(paste0(file.dir, "/raw_rds_files")))
  project = list(dir = file.dir)
  class(project) = c("fxtract_project", class(project))
  reg = batchtools::loadRegistry(paste0(project$dir, "/reg"), writeable = TRUE)
  project[["reg"]] = reg
  project[["group_by"]] = group_by
  project
}

#' Writes single RDS files from an SQL database.
#'
#' Given an SQL database with a grouping variable, this function loads the SQL database into RAM for each grouping variable
#' and saves these as dataframes in RDS files. Parallelization is available via \link{foreach}.
#'
#' @param project fxtract_project object created by \code{\link{makeProject}}
#' @param file.dir character. The file directory of the SQL database.
#' @param tbl_name character. The table name required by \code{\link[dplyr]{tbl}}.
#' @param group_by character. The grouping variable.
#' @return fxtract_project
#' @importFrom foreach "%dopar%"
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \dontrun{
#' project = sqlToRds(project, "projects/myProject/SQL_database.sql",
#'   tbl_name = "table", group_by = "user")
#' }
sqlToRds = function(project, file.dir, tbl_name, group_by){
  i = NULL
  checkmate::assertClass(project, "fxtract_project")
  checkmate::assertCharacter(tbl_name)
  db = dplyr::src_sqlite(file.dir, create = FALSE)
  logs = dplyr::tbl(db, from = tbl_name)
  gb = logs %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()

  foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
    db = dplyr::src_sqlite(file.dir, create = FALSE)
    logs = dplyr::tbl(db, from = tbl_name)
    logs_i = logs %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
    saveRDS(logs_i, file = paste0(project$dir, "/raw_rds_files/", i, ".RDS"))
  }
  project[["group_by"]] = group_by
  project
}



#' Adds batchtools problems.
#'
#' Every grouping variable's dataframe is added as a batchtools problem.
#'
#' @param project fxtract_project file created by \code{\link{makeProject}}
#' @export
#' @examples
#' \dontrun{
#' addBatchtoolsProblems(project = project)
#' }
addBatchtoolsProblems = function(project) {
  rds_files = list.files(path = paste0(project$dir, "/raw_rds_files"))
  for (id in rds_files) {
    data_id = readRDS(paste0(project$dir, "/raw_rds_files/", id))
    name = gsub(".RDS", "", id)
    batchtools::addProblem(name = name, data = data_id, reg = project$reg)
  }
}

#' Adds batchtools algorithms and experiments.
#'
#' User defined functions must be saved as separate .R files in the projects subfolder 'feature_functions'.
#' See the tutorial for more information. These functions need a dataframe as input and a named vector as output.
#'
#' @param project fxtract_project object created by \code{\link{makeProject}}
#' @export
#' @examples
#' \dontrun{
#' addBatchtoolsAlgorithms(project = project)
#' }
addBatchtoolsAlgorithms = function(project) {
  feature_functions = list.files(path = paste0(project$dir, "/feature_functions"))
  for (feat_fun in feature_functions) {
    fun = source(paste0(project$dir, "/feature_functions/", feat_fun))
    name = gsub(".R", "", feat_fun)
    eval(parse(text = paste0(name, " = fun$value")))
    eval(parse(text = paste0("batchtools::batchExport(export = list(", name, " =", name, "))")))
    eval(parse(text = paste0("batchtools::addAlgorithm(name, fun = function(job, data, instance) ", name, "(data))")))
  }
  batchtools::addExperiments()
}


#' Summarizes the projects status.
#'
#' A detailed summary of which feature functions were already calculated.
#' @param project fxtract_project object created by \code{\link{makeProject}}
#' @return list.
#' @export
#' @examples
#' \dontrun{
#' getProjectStatus(project = project)
#' }
#' @importFrom stats as.formula setNames
getProjectStatus = function(project) {
  problem = vars = funs = NULL
  reg = project$reg
  batchtools::assertRegistry(reg, "ExperimentRegistry")
  res = batchtools::reduceResultsDataTable(reg = reg)
  jt = batchtools::getJobTable(reg = reg)
  jt = data.frame(jt)
  dcast_formula = as.formula("problem ~ algorithm")
  res = data.table::dcast(data.table::setDT(jt), dcast_formula, value.var = "done")
  doneFun = function(x) ifelse(!is.na(x), 1, 0)
  res = res %>% dplyr::mutate_at(dplyr::vars(-problem), dplyr::funs(doneFun))
  res2 = list(detailed = res)
  res2[["problem_wise"]] = data.frame(problem = res$problem,
    finished = res %>% dplyr::select(-problem) %>% rowMeans())
  res2[["feature_wise"]] = res %>% dplyr::select(-problem) %>% colMeans()
  res2
}

getAllFeatureFunctions = function(project) {
  feature_functions = list.files(path = paste0(project$dir, "/feature_functions"))
  gsub(".R", replacement = "", feature_functions)
}

getAllProblems = function(project) {
  rds_files = list.files(path = paste0(project$dir, "/raw_rds_files"))
  gsub(".RDS", "", rds_files)
}

#' Collects results.
#'
#' Summarizes all calculated features into one dataframe.
#'
#' @param project fxtract_project object created by \code{\link{makeProject}}
#' @return dataframe.
#' @export
#' @examples
#' \dontrun{
#' collectResults(project = project)
#' }
#' @importFrom magrittr "%>%"
collectResults = function(project) {
  all_features = job.id = problem = algorithm = NULL
  reg = project$reg
  batchtools::assertRegistry(reg, "ExperimentRegistry")
  res = batchtools::reduceResultsDataTable(reg = reg)
  jt = batchtools::getJobTable(reg = reg)
  lookup = jt %>% select(job.id, problem, algorithm)

  features = getAllFeatureFunctions(project)

  results = getAllProblems(project) %>% data.frame(stringsAsFactors = FALSE) %>% setNames("problem")
  for (feature in features) {
    ids = lookup %>% filter(algorithm %in% feature)
    res_feat = res[job.id %in% ids$job.id]
    listOfDataframes = res_feat$result %>% setNames(res_feat$job.id)
    df = data.frame(do.call("rbind", listOfDataframes))
    df$job.id = res_feat$job.id
    df = df %>% dplyr::left_join(lookup, by = "job.id") %>% dplyr::select(-job.id, -algorithm)
    results = results %>% dplyr::left_join(df, by = "problem")
  }
  colnames(results)[which(colnames(results) == "problem")] = project$group_by
  results
}
