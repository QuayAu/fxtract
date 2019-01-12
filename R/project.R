#' R6 Object for Feature Extraction.
#'
#' @description
#' \code{Project} calculates features from longitudinal data for each grouping variable individually with batchtools.
#'
#' @format \code{\link{R6Class}} object.
#' @name Project
#'
#' @section Usage:
#' \preformatted{
#' my_project = Project$new("my_project")
#' }
#'
#' @section Arguments:
#'
#' For Project$new():
#' \describe{
#' \item{project_name: }{('character(1)'): A user defined name of the project. All necessary data will be saved on the path: ./projects/project_name/}
#' \item{load: }{(`logical(1)`): If TRUE, an existing project will be loaded.}
#' }
#' @section Details:
#' All datasets and feature functions are saved in this R6 object. \code{Project} heavily relies on the R-package batchtools.
#' Data will be saved as single RDS files (for each grouping variable) and feature functions are calculated on each single dataset.
#' A big advantage of this method is that it scales nicely for larger datasets. Data is only loaded into RAM, when needed.
#'
#' @section Fields:
#' \describe{
#' \item{project_name: }{(`character(1)`): The projects name.}
#' \item{dir: }{(`character(1)`): The projects directory.}
#' \item{group_by: }{(`character(1)`): The column on which to group by.}
#' \item{reg: }{(`registry`): batchtools registry.}
#' }
#'
#' @section Methods:
#' \describe{
#' \item{add_data(dataframe, group_by)}{[dataframe: (`data.frame`)] A dataframe which shall be added to the R6 object. \cr
#'  [group_by: (`character(1)`)] The grouping variable of the dataframe. \cr \cr
#'  This method writes single RDS files (this can be parallelized with foreach) and adds them as batchtools problems for each grouping variable of a dataframe.
#'  After that, batchtools experiments will be added too.}
#' \item{preprocess_data(fun)}{[fun: (`function`)] A function, which has a dataframe as input and a dataframe as output. \cr \cr
#'  This method loads the RDS files and applies this function on them. The old RDS files are overwritten and the batchtools
#'  problems and experiments are updated.}
#' \item{remove_data(data)}{[data: (`character(1)`)] The grouping variable's name. \cr \cr
#'  This method deletes the RDS file and batchtools problem of one single grouping variable.}
#' \item{add_feature(fun)}{[fun: (`function`)] A function, which has a dataframe as input and a named vector as output. \cr \cr
#'  This method adds a function as batchtools algorithm. After that, batchtools experiments are added too.}
#' \item{remove_feature(fun)}{[fun: (`function | character(1)`)] A function (or the name of the function as character) which shall be removed. \cr \cr
#'  This method removes the batchtools algorithms and experiments corresponding to the given function.}
#' \item{calc_features()}{This method calculates all features on all datasets. Internally, it submits all batchtools jobs, which are not done yet.}
#' \item{get_project_status()}{This method gives an overview over which features are done on which datasets.}
#' \item{collect_results()}{This method returns a dataframe with the calculated features.}
#' \item{plot()}{[internal] method to print the R6 object.}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#'
#' @examples
#' unlink("projects/my_project", recursive = TRUE)
#' my_project = Project$new("my_project")
#' my_project$add_data(iris, group_by = "Species")
#' fun = function(data) {
#'   c(mean_sepal_length = mean(data$Sepal.Length))
#' }
#' my_project$add_feature(fun)
#' my_project$calc_features()
#' my_project$collect_results()
#' unlink("projects/my_project", recursive = TRUE)
#' @import R6
#' @import dplyr
#' @import batchtools
#' @importFrom foreach "%dopar%" "%do%"
NULL

#' @export
Project = R6Class("Project",
  public = list(
    project_name = NULL,
    group_by = NULL,
    reg = NULL,
    dir = NULL,
    initialize = function(project_name, load = FALSE) {
      self$project_name = checkmate::assert_character(project_name)
      newDirPath = paste0("projects/", project_name)
      self$dir = newDirPath
      if (!load) {
        if (!dir.exists("projects")) dir.create("projects")
        if (dir.exists(newDirPath)) stop("The project name already exists. Please choose another name, delete the existing project, or set load = TRUE, if you want to load the old project.")
        dir.create(newDirPath)
        dir.create(paste0(newDirPath, "/rds_files"))
        saveRDS(NULL, file = paste0(self$dir, "/group_by.RDS"))
        self$reg = batchtools::makeExperimentRegistry(paste0(newDirPath, "/reg"))
      } else {
        checkmate::assert_subset(project_name, list.files("projects/"))
        self$reg = batchtools::loadRegistry(paste0(newDirPath, "/reg"), writeable = TRUE)
        self$group_by = readRDS(paste0(newDirPath, "/group_by.RDS"))
      }
    },
    print = function() {
      problems = self$reg$problems[1:20]
      problems = rep(problems, 1000)
      problems = problems[!is.na(problems)]
      if (length(problems) <= 20) {
        problems = paste0(problems, collapse = ", ")
      } else {
        problems = paste0(problems[1:20], collapse = ", ")
        problems = paste0(problems, ", ...")
      }
      algos = self$reg$algorithms[1:20]
      algos = algos[!is.na(algos)]
      if (length(algos) <= 20) {
        algos = paste0(algos, collapse = ", ")
      } else {
        algos = paste0(algos[1:20], collapse = ", ")
        algos = paste0(algos, ", ...")
      }
      cat("R6 Object \n")
      cat(paste0("Project name: ", self$project_name, "\n"))
      cat(paste0("Grouping variables: ", problems, "\n"))
      cat(paste0("Feature functions: ", algos, "\n"))
      invisible(self)
    },
    add_data = function(dataframe, group_by) {
      id = NULL
      checkmate::assert_data_frame(dataframe)
      checkmate::assert_subset(group_by, colnames(dataframe))
      checkmate::assert_character(group_by, len = 1)
      if (is.null(self$group_by)) {
        self$group_by = group_by
        saveRDS(group_by, file = paste0(self$dir, "/group_by.RDS"))
      }
      if (group_by != self$group_by) stop(paste0("The group_by variable was set to ", self$group_by,
        ". Only one group_by variable is allowed per project!"))
      gb = dataframe %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()

      #save rds files
      foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
        dataframe_i = dataframe %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
        saveRDS(dataframe_i, file = paste0(self$dir, "/rds_files/", i, ".RDS"))
        message(paste0("Saving raw RDS file " , i, ".RDS ", "on disk."))
      }

      #add batchtools problems
      for (id in gb) { #cannot be parallelized, because of batchtools
        data_id = dataframe %>% dplyr::filter(!!as.name(group_by) == id) %>% data.frame()
        batchtools::addProblem(name = id, data = data_id, reg = self$reg)

        #add experiments
        prob.designs = replicate(1L, data.table::data.table(), simplify = FALSE)
        names(prob.designs) = id
        private$add_experiments(prob.designs = prob.designs)
      }
      return(invisible(self))
    },
    preprocess_data = function(fun) {
      datasets = list.files(paste0(self$dir, "/rds_files/"))
      #update RDS files
      foreach::foreach(i = datasets, .packages = c("dplyr")) %dopar% {
        dataframe_i = readRDS(paste0(self$dir, "/rds_files/", i))
        data_preproc = fun(dataframe_i)
        saveRDS(data_preproc, file = paste0(self$dir, "/rds_files/", i))
        message(paste0("Updating raw RDS file " , i))
      }

      #update batchtools problems
      gb = gsub(".RDS", "", datasets)
      for (id in gb) { #cannot be parallelized, because of batchtools
        batchtools::removeProblems(id, reg = self$reg)
        data_id = readRDS(paste0(self$dir, "/rds_files/", id, ".RDS"))
        batchtools::addProblem(name = id, data = data_id, reg = self$reg)
        #add experiments
        prob.designs = replicate(1L, data.table::data.table(), simplify = FALSE)
        names(prob.designs) = id
        private$add_experiments(prob.designs = prob.designs)
      }
      return(invisible(self))
    },
    remove_data = function(data) {
      checkmate::assert_character(data)
      checkmate::assert_subset(data, self$reg$problems)
      message("Deleting RDS file ", data, ".RDS")
      unlink(paste0(paste0(self$dir, "/rds_files/", data, ".RDS")))
      batchtools::removeProblems(data, reg = self$reg)
      return(invisible(self))
    },
    add_feature = function(fun) {
      checkmate::assert_function(fun)
      batchtools::batchExport(export = setNames(list(fun), deparse(substitute(fun))), reg = self$reg)
      batchtools::addAlgorithm(
        name = deparse(substitute(fun)),
        fun = function(job, data, instance) fxtract::calc_feature(data, group_by = self$group_by, fun = fun),
        reg = self$reg
      )

      #add experiments
      algo.designs = replicate(1L, data.table::data.table(), simplify = FALSE)
      names(algo.designs) = deparse(substitute(fun))
      private$add_experiments(algo.designs = algo.designs)

      return(invisible(self))
    },
    remove_feature = function(fun) {
      fun = as.character(substitute(fun))
      jt = batchtools::getJobTable(reg = self$reg)
      checkmate::assert_subset(fun, unique(jt$algorithm))
      batchtools::removeAlgorithms(fun, reg = self$reg)
      return(invisible(self))
    },
    calc_features = function() {
      running = batchtools::findRunning(reg = self$reg)$job.id
      not_done = batchtools::findNotDone(reg = self$reg)$job.id
      batchtools::submitJobs(ids = setdiff(not_done, running), reg = self$reg)
      return(invisible(self))
    },
    get_project_status = function() {
      problem = vars = funs = NULL
      reg = self$reg
      if (nrow(batchtools::findDone(reg = reg)) == 0) stop("No features have been calculated yet or all functions resulted in errors. Start calculating with method $calc_features().")
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
      res2[["perc_done"]] = mean(unlist(res2$detailed %>% dplyr::select(-problem)))
      res2
    },
    collect_results = function() {
      feature = job.id = problem = algorithm = NULL
      reg = self$reg
      res = batchtools::reduceResultsDataTable(reg = reg)
      if (nrow(res) == 0) stop("No features have been calculated yet. Start calculating with method $calc_features().")
      done_id = res$job.id
      res = setNames(res$result, done_id)

      jt = batchtools::getJobTable(reg = reg)
      lookup = jt %>% select(job.id, problem, algorithm)
      features = self$get_project_status()$feature_wise
      features = names(features[features != 0])
      results = foreach::foreach(feature = features) %do% {
        ids = lookup %>% dplyr::filter(algorithm %in% feature)
        res_feat = res[names(res) %in% ids$job.id]
        dplyr::bind_rows(res_feat)
      }
      final_result = results[[1]]
      if (length(results) >= 2) {
        for (i in 2:length(results)) {
         final_result = final_result %>% dplyr::full_join(results[[i]], by = self$group_by)
        }
      }
      final_result
    }
  ),
  private = list(
    add_experiments = function(prob.designs = NULL, algo.designs = NULL) {
      batchtools::addExperiments(reg = self$reg, prob.designs = prob.designs, algo.designs = algo.designs)
      return(invisible(self))
    }
  )
)
