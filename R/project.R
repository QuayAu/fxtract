#' @title Project Class
#' @format [R6Class] object
#' @name Project
#' @import R6
NULL

#' @export
Project = R6Class("Project",
  public = list(
    project_name = NULL,
    dir = NULL,
    data = NULL,
    features = NULL,
    project_status = NULL,
    reg = NULL,
    submit_jobs = NULL,
    result = NULL,
    initialize = function(project_name, ...) {
      self$project_name = assert_character(project_name)
      newDirPath = paste0("projects/", project_name)
      if (dir.exists("projects")) print("The project folder 'projects' already exists. The new project will be saved in this folder")
      if (!dir.exists("projects")) dir.create("projects")
      if (dir.exists(newDirPath)) stop("The project name already exists. Please choose another name or delete the existing project and try again!")
      dir.create(newDirPath)
      dir.create(paste0(newDirPath, "/feature_functions"))
      dir.create(paste0(newDirPath, "/raw_rds_files"))
      self$dir = newDirPath
      self$reg = batchtools::makeExperimentRegistry(paste0(newDirPath, "/reg"), ...)
      self$data = list()
    },
    use_dataframe = function(dataframe, group_by) {
      i = NULL
      checkmate::assertDataFrame(dataframe)
      # self$group_by = group_by
      checkmate::assert_subset(group_by, colnames(dataframe))
      gb = dataframe %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()

      foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
        dataframe_i = dataframe %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
        saveRDS(dataframe_i, file = paste0(project$dir, "/raw_rds_files/", i, ".RDS"))
      }
      self$data = c(self$data,
            setNames(list(list(ids = as.character(gb), group_by = group_by)), deparse(substitute(dataframe)))
      )
      return(invisible(self))
    }
  )
)
