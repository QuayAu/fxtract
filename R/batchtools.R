#make project
# #' @export
makeProject = function(newProjectName){
  checkmate::assertCharacter(newProjectName)
  newDirPath = paste0("Projects/", newProjectName)
  if (dir.exists("Projects")) print("The project folder 'Projects' already exists. The new project will be saved in this folder")
  if (!dir.exists("Projects")) dir.create("Projects")
  if (dir.exists(newDirPath)) stop("The project name already exists. Please choose another name or delete the existing project and try again!")
  dir.create(newDirPath)
  dir.create(paste0(newDirPath, "/feature_functions"))
  dir.create(paste0(newDirPath, "/raw_rds_files"))
  project = list(dir = newDirPath)
  class(project) = c("fxtract_project", class(project))
  project
}

#load project
# #' @export
loadProject = function(file.dir) {
  checkmate::assertTRUE(file.exists(paste0(file.dir, "/feature_functions")))
  checkmate::assertTRUE(file.exists(paste0(file.dir, "/raw_rds_files")))
  project = list(dir = file.dir)
  class(project) = c("fxtract_project", class(project))
  project
}

#' @importFrom dplyr src_sqlite
# #' @export
readSQLData = function(file.dir, tbl_name) {
  checkmate::assertCharacter(tbl_name)
  db = dplyr::src_sqlite(file.dir, create = FALSE)
  logs = dplyr::tbl(db, from = tbl_name)
  class(logs) = c("fxtract_db", class(logs))
  logs
}

#' @importFrom foreach "%dopar%"
#' @importFrom magrittr "%>%"
# #' @export
sqlToRds = function(project, file.dir, tbl_name, filter_by){
  i = NULL
  checkmate::assertClass(project, "fxtract_project")
  checkmate::assertCharacter(tbl_name)
  db = dplyr::src_sqlite(file.dir, create = FALSE)
  logs = dplyr::tbl(db, from = tbl_name)
  gb = logs %>% dplyr::distinct_(.dots = filter_by) %>% data.frame() %>% unlist()

  foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
    db = dplyr::src_sqlite(file.dir, create = FALSE)
    logs = dplyr::tbl(db, from = tbl_name)
    logs_i = logs %>% dplyr::filter(!!as.name(filter_by) == i) %>% data.frame()
    saveRDS(logs_i, file = paste0(project$dir, "/raw_rds_files/", i, ".RDS"))
  }
}

#' @importFrom batchtools makeExperimentRegistry
# #' @export
makeBatchtoolsExperiment = function(project, ...) {
  reg = batchtools::makeExperimentRegistry(paste0(project$dir, "/reg"), ...)
  project[["reg"]] = reg
  project
}

#' @importFrom batchtools addProblem
# #' @export
addBatchtoolsProblems = function(project) {
  rds_files = list.files(path = paste0(project$dir, "/raw_rds_files"))
  for (id in rds_files) {
    data_id = readRDS(paste0(project$dir, "/raw_rds_files/", id))
    name = gsub(".RDS", "", id)
    batchtools::addProblem(name = name, data = data_id, reg = project$reg)
  }
}

#' @importFrom batchtools batchExport addAlgorithm
# #' @export
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

#' @importFrom batchtools loadRegistry
# #' @export
loadBatchtoolsExperiment = function(project, writeable = TRUE) {
  reg = batchtools::loadRegistry(paste0(project$dir, "/reg"), writeable = writeable)
  project[["reg"]] = reg
  project
}

#project status
#' @importFrom magrittr "%>%"
# #' @export
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
  res = res %>% dplyr::mutate_at(vars(-problem), funs(doneFun))
  res2 = list(detailed = res)
  res2[["problem_wise"]] = data.frame(problem = res$problem,
    finished = res %>% dplyr::select(-problem) %>% rowMeans())
  res2[["feature_wise"]] = res %>% dplyr::select(-problem) %>% colMeans()
  res2
}

#collect results
# #' @export
getAllFeatureFunctions = function(project) {
  feature_functions = list.files(path = paste0(project$dir, "/feature_functions"))
  gsub(".R", replacement = "", feature_functions)
}

#' @importFrom magrittr "%>%"
# #' @export
findFeatureFunctionJobId = function(feature_function, project) {
  job.id = algorithm = NULL
  reg = project$reg
  batchtools::assertRegistry(reg, "ExperimentRegistry")
  jt = batchtools::getJobTable(reg = reg)
  jt = data.frame(jt)
  jt %>% dplyr::filter(algorithm == feature_function) %>% dplyr::select(job.id)
}
