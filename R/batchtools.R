#' Iniates batchtools functionality
#'
#' This function iniates batchtools functionality of this package.
#' Check the tutorial on the github page!
#' Every desired function will be calculated for each grouping variable (e.g. each participant of a study).
#' The data will be saved as an RDS-file for each grouping variable.
#' This enables the possibility to calculate features even if all raw data cannot be loaded into RAM.
#' Feature extraction can be paused and restarted at a later time, even on a different machine.
#' Parallelization can be done via \cite{batchtools}.
#' @param newProjectName character. The new project name. A subfolder under the (created) folder `projects` will be created.
#' @param group_by character. The grouping variable.
#' @param ... other arguments passed to \code{batchtools::makeExperimentRegistry}
#' @return fxtract_project object.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \dontrun{
#' project = makeProject("my_project", group_by = "userId")
#' }
makeProject = function(newProjectName, group_by, ...){
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
  project[["group_by"]] = group_by
  class(project) = c("fxtract_project", class(project))
  saveRDS(project, file = paste0(newDirPath, "/project.RDS"))
  project
}

#' Loads fxtract project.
#'
#' In order to restart feature extraction, the fxtract project needs to be loaded with this function.
#' @param file.dir character. The project's directory.
#' @return fxtract_project object
#' @export
#' @examples
#' \dontrun{
#' project = loadProject("projects/my_project")
#' }
loadProject = function(file.dir) {
  checkmate::assertTRUE(file.exists(paste0(file.dir, "/feature_functions")))
  checkmate::assertTRUE(file.exists(paste0(file.dir, "/raw_rds_files")))
  project = readRDS(paste0(file.dir, "/project.RDS"))
  reg = batchtools::loadRegistry(paste0(project$dir, "/reg"), writeable = TRUE)
  project[["reg"]] = reg
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
#' @return fxtract_project
#' @importFrom foreach "%dopar%"
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \dontrun{
#' project = sqlToRds(project, "projects/myProject/SQL_database.sql",
#'   tbl_name = "table")
#' }
sqlToRds = function(project, file.dir, tbl_name){
  i = NULL
  checkmate::assertClass(project, "fxtract_project")
  checkmate::assertCharacter(tbl_name)
  db = dplyr::src_sqlite(file.dir, create = FALSE)
  logs = dplyr::tbl(db, from = tbl_name)
  group_by = project$group_by
  checkmate::assert_subset(group_by, colnames(logs))

  gb = logs %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()

  foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
    db = dplyr::src_sqlite(file.dir, create = FALSE)
    logs = dplyr::tbl(db, from = tbl_name)
    logs_i = logs %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
    saveRDS(logs_i, file = paste0(project$dir, "/raw_rds_files/", i, ".RDS"))
  }
  return(invisible(project))
}

#' Writes single RDS files from an R dataframe.
#'
#' Given an R dataframe with a grouping variable (defined by the project object), this function saves single RDS files for every grouping variable.
#' Parallelization is available via \link{foreach}.
#'
#' @param project fxtract_project object created by \code{\link{makeProject}}
#' @param dataframe R dataframe.
#' @return fxtract_project
#' @importFrom foreach "%dopar%"
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \dontrun{
#' dataframeToRds(project, studenlife.small)
#' }
dataframeToRds = function(project, dataframe){
  i = NULL
  checkmate::assertClass(project, "fxtract_project")
  checkmate::assertDataFrame(dataframe)
  group_by = project$group_by
  checkmate::assert_subset(group_by, colnames(dataframe))
  gb = dataframe %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()

  foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
    dataframe_i = dataframe %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
    saveRDS(dataframe_i, file = paste0(project$dir, "/raw_rds_files/", i, ".RDS"))
  }
  return(invisible(project))
}

#' Adds batchtools problems.
#'
#' Every grouping variable's dataframe is added as a batchtools problem. These problems can contain chunked dataframes.
#' Parallelization is available for reading the RDS-files via \link{foreach}.
#'
#' @param project fxtract_project file created by \code{\link{makeProject}}
#' @param n.chunks integer. Number of chunks the dataframes will be chunked with \code{dplyr::bind_rows()}. Defaults to number of unique grouping variables.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \dontrun{
#' addBatchtoolsProblems(project = project, n.chunks = 2)
#' }
addBatchtoolsProblems = function(project, n.chunks) {
  chunk = files = f = NULL
  checkmate::assertIntegerish(n.chunks)
  rds_files = list.files(path = paste0(project$dir, "/raw_rds_files"))
  rds_files = data.frame(files = rds_files)
  if (missing(n.chunks)) {
    for (id in rds_files$files) {
      data_id = readRDS(paste0(project$dir, "/raw_rds_files/", id))
      name = gsub(".RDS", "", id)
      batchtools::addProblem(name = name, data = data_id, reg = project$reg)
    }  
  } else {
    rds_files$chunk = batchtools::chunk(1:nrow(rds_files), n.chunks = n.chunks)
  
    chunks = unique(rds_files$chunk)
    for (z in chunks) {
      files = rds_files %>% dplyr::filter(chunk == z) %>% dplyr::pull(files) %>% as.character()
  
      x = foreach::foreach(f = files) %dopar% {
        readRDS(paste0(project$dir, "/raw_rds_files/", f))
      }
      data_chunk = dplyr::bind_rows(x)
      batchtools::addProblem(name = paste0("chunk_", z), data = data_chunk, reg = project$reg)
    }
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
  batchtools::batchExport(export = list(calcFeature = fxtract::calcFeature))
  for (feat_fun in feature_functions) {
    fun = source(paste0(project$dir, "/feature_functions/", feat_fun))
    name = gsub(".R", "", feat_fun)
    eval(parse(text = paste0(name, " = fun$value")))
    eval(parse(text = paste0("batchtools::batchExport(export = list(", name, " =", name, "))")))
    eval(parse(text = paste0(
          "batchtools::addAlgorithm(name, fun = function(job, data, instance) fxtract::calcFeature(data, group_col = project$group_by, fun = ",
          name,
          "))"
        )
      )
    )
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
  jt = batchtools::getJobTable(reg = reg)
  jt = data.frame(jt)

  jt = jt %>% dplyr::left_join(data.frame(job.id = batchtools::findDone(), really_done = "DONE"), by = "job.id")

  dcast_formula = as.formula("problem ~ algorithm")

  res = data.table::dcast(data.table::setDT(jt), dcast_formula, value.var = "really_done")
  doneFun = function(x) ifelse(!is.na(x), 1, 0)
  res = res %>% dplyr::mutate_at(dplyr::vars(-problem), dplyr::funs(doneFun))

  res2 = list(detailed = res)
  res2[["problem_wise"]] = data.frame(problem = res$problem,
    finished = res %>% dplyr::select(-problem) %>% rowMeans())
  res2[["feature_wise"]] = res %>% dplyr::select(-problem) %>% colMeans()
  res2
}

getAllFeatureFunctions = function(project) {
  unique(batchtools::getJobTable(reg = project$reg)$algorithm)
}

getAllProblems = function(project) {
  unique(batchtools::getJobTable(reg = project$reg)$problem)
}

#' Collects results.
#'
#' Summarizes all calculated features into one dataframe.
#'
#' @param project fxtract_project object created by \code{\link{makeProject}}
#' @return dataframe.
#' @importFrom foreach "%dopar%"
#' @export
#' @examples
#' \dontrun{
#' collectResults(project = project)
#' }
#' @importFrom magrittr "%>%"
collectResults = function(project) {
  feature = NULL
  job.id = problem = algorithm = NULL
  reg = project$reg
  batchtools::assertRegistry(reg, "ExperimentRegistry")
  res = batchtools::reduceResultsDataTable(reg = reg)
  jt = batchtools::getJobTable(reg = reg)
  lookup = jt %>% select(job.id, problem, algorithm)

  features = getProjectStatus(project)$feature_wise
  features = names(features[features != 0])


  results = foreach::foreach(feature = features) %dopar% {
    ids = lookup %>% filter(algorithm %in% feature)
    res_feat = res[job.id %in% ids$job.id]
    listOfDataframes = res_feat$result %>% setNames(res_feat$job.id)
    dplyr::bind_rows(listOfDataframes)
  }

  final_result = results[[1]]
  if (length(results) >= 2) {
    for (i in 2:length(results)) {
     final_result = final_result %>% dplyr::full_join(results[[i]])
    }
  }
  final_result
}
