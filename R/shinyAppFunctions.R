makeProject = function(newProjectName){
  checkmate::assertCharacter(newProjectName)
  newDirPath = paste0("Projects/", newProjectName)
  if (dir.exists("Projects")) print("The project folder 'Projects' already exists. The new project will be saved in this folder")
  if (!dir.exists("Projects")) dir.create("Projects")
  if (dir.exists(newDirPath)) stop("The project name already exists. Please choose another name or delete the existing project and try again!")
  dir.create(newDirPath)
  dir.create(paste0(newDirPath, "/features_csv"))
  dir.create(paste0(newDirPath, "/feature_functions"))
  dir.create(paste0(newDirPath, "/raw_rds_files"))
  proj = list(dir = newDirPath)
  class(proj) = c("fxtract_proj", class(proj))
  proj
}



#' @importFrom dplyr src_sqlite
readSQLData = function(file.dir, tbl_name) {
  checkmate::assertCharacter(tbl_name)
  db = dplyr::src_sqlite(file.dir, create = FALSE)
  logs = dplyr::tbl(db, from = tbl_name)
  class(logs) = c("fxtract_db", class(logs))
  logs
}

#' @importFrom foreach %dopar%
sqlToRds = function(proj, file.dir, tbl_name, group_by){
  i = NULL
  checkmate::assertClass(proj, "fxtract_proj")
  checkmate::assertCharacter(tbl_name)
  db = dplyr::src_sqlite(file.dir, create = FALSE)
  logs = dplyr::tbl(db, from = tbl_name)
  gb = logs %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()

  x = foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
    db = dplyr::src_sqlite(file.dir, create = FALSE)
    logs = dplyr::tbl(db, from = tbl_name)
    logs_i = logs %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
    saveRDS(logs_i, file = paste0(proj$dir, "/raw_rds_files/", i, ".RDS"))
  }
}

#' @importFrom batchtools makeExperimentRegistry addExperiments addAlgorithm
makeBatchtoolsExperiment = function(proj) {
  reg = batchtools::makeExperimentRegistry(paste0(proj$dir, "/reg"))
  rds_files = list.files(path = paste0(proj$dir, "/raw_rds_files"))
  for (id in rds_files) {
    data_id = readRDS(paste0(proj$dir, "/raw_rds_files/", id))
    name = gsub(".RDS", "", id)
    batchtools::addProblem(name = name, data = data_id)
  }
  feature_functions = list.files(path = paste0(proj$dir, "/feature_functions"))

  for (feat_fun in feature_functions) {
    fun = source(paste0(proj$dir, "/feature_functions/", feat_fun))
    fun = fun$value
    name = gsub(".RDS", "", feat_fun)
    batchtools::addAlgorithm(name, fun = function(job, data, instance) fun(data))
  }
  batchtools::addExperiments()
}


#load project

#collect results

#project status

#delete feature
#add feature
