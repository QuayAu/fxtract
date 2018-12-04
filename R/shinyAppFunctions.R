makeProject = function(newProjectName){
  checkmate::assertCharacter(newProjectName)
  newDirPath = paste0("Projects/", newProjectName)
  if (dir.exists("Projects")) print("The project folder 'Projects' already exists. The new project will be saved in this folder")
  if (!dir.exists("Projects")) dir.create("Projects")
  if (dir.exists(newDirPath)) stop("The project name already exists. Please choose another name or delete the existing project and try again!")
  dir.create(newDirPath)
  dir.create(paste0(newDirPath, "/features"))
  dir.create(paste0(newDirPath, "/raw_rda_files"))
  proj = list(dir = newDirPath)
  class(proj) = c("fxtract_proj", class(proj))
  proj
}



#' @importFrom dplyr src_sqlite
readSQLData = function(file.dir, tbl_name) {
  checkmate::assertClass(proj, "fxtract_proj")
  checkmate::assertCharacter(tbl_name)
  db = dplyr::src_sqlite(file.dir, create = FALSE)
  logs = dplyr::tbl(db, from = tbl_name)
  class(logs) = c("fxtract_db", class(logs))
  logs
}

sqlToRda = function(project, sql_db, participants){

}


unlink("Projects/personality_prediction", recursive = TRUE)
proj = makeProject("Personality_Prediction")
sql_empra = readSQLData(file.dir = "../Personality_Prediction/Data/SQL_database_EMPRA", tbl_name = "df1")


sqlToRda = function(proj, file.dir, tbl_name, group_by){
  checkmate::assertClass(proj, "fxtract_proj")
  checkmate::assertCharacter(tbl_name)
  db = dplyr::src_sqlite(file.dir, create = FALSE)
  logs = dplyr::tbl(db, from = tbl_name)
  gb = logs %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()

  x = foreach::foreach(i = gb, .packages = c("dplyr")) foreach::%dopar% {
    db = dplyr::src_sqlite(file.dir, create = FALSE)
    logs = dplyr::tbl(db, from = tbl_name)
    logs_i = logs %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
    saveRDS(logs_i, file = paste0(proj$dir, "/raw_rda_files/", i, ".RDS"))
  }
}


library(foreach)
library(doSNOW)
cl = makeCluster(32)
registerDoSNOW(cl)
sqlToRda(proj = proj, file.dir = "../Personality_Prediction/Data/SQL_database_EMPRA", tbl_name = "df1", group_by = "userId")
snow::stopCluster(cl); registerDoSEQ()


source("../Personality_Prediction_OSF/Scripts/code_blocks/helper_functions.R")





