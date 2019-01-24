#' R6 Object for Feature Extraction.
#'
#' @description
#' \code{Xtractor} calculates features from raw data for each ID of a grouping variable individually with batchtools.
#'
#' @format \code{\link{R6Class}} object.
#' @name Xtractor
#'
#' @section Usage:
#' \preformatted{
#' xtractor = Xtractor$new("xtractor")
#' }
#'
#' @section Arguments:
#'
#' For Xtractor$new():
#' \describe{
#' \item{\code{name}: }{('character(1)'): A user defined name of the Xtractor. All necessary data will be saved on the path: ./fxtract_files/name/}
#' \item{\code{load}: }{(`logical(1)`): If TRUE, an existing Xtractor will be loaded.}
#' }
#' @section Details:
#' All datasets and feature functions are saved in this R6 object. \code{Xtractor} heavily relies on the R-package batchtools.
#' Data will be saved as single RDS files (for each ID) and feature functions are calculated on each single dataset.
#' A big advantage of this method is that it scales nicely for larger datasets. Data is only read into RAM, when needed.
#'
#' @section Fields:
#' \describe{
#' \item{\code{name}: }{(`character(1)`): The Xtractors name.}
#' \item{\code{dir}: }{(`character(1)`): The directory where files are saved.}
#' \item{\code{group_by}: }{(`character(1)`): The column on which to group by.}
#' \item{\code{reg}: }{(`registry`): batchtools registry.}
#' \item{\code{perc_done}: }{(`numeric(1)`): Active binding. Percentage of finished calculations.}
#' \item{\code{backend}: }{(`character(1)`): Active binding. The calculation backend. Can be 'dplyr' or 'batchtools'.}
#' \item{\code{error_messages}: }{(`data.frame()`): Active binding. A dataframe with information about error messages.}
#' \item{\code{log_files}: }{(`list()`): Active binding. A list with the log files which were created due to errors.}
#' \item{\code{datasets}: }{(`character()`): Active binding. A character vector with the IDs of the grouping variable.}
#' \item{\code{features}: }{(`character()`): Active binding. A character vector with the feature functions which were added.}
#' \item{\code{status}: }{(`data.frame()`): Active binding. A dataframe with an overview over which features are done on which datasets.}
#' \item{\code{results}: }{(`data.frame()`): Active binding. A dataframe with all calculated features of all IDs.}
#' }
#'
#' @section Methods:
#' \describe{
#' \item{\code{add_data(dataframe, group_by)}}{[dataframe: (`data.frame`)] A dataframe which shall be added to the R6 object. \cr
#'  [group_by: (`character(1)`)] The grouping variable's name of the dataframe. \cr
#'  This method writes single RDS files (this can be parallelized with foreach) and adds them as batchtools problems for each ID of the grouping variable.
#'  After that, batchtools experiments will be added too.} \cr
#' \item{\code{preprocess_data(fun)}}{[fun: (`function`)] A function, which has a dataframe as input and a dataframe as output. \cr
#'  This method loads the RDS files and applies this function on them. The old RDS files are overwritten and the batchtools
#'  problems and experiments are updated.}
#' \item{\code{remove_data(data)}}{[data: (`character(1)`)] An ID of the grouping variable. \cr
#'  This method deletes the RDS file and batchtools problem of one single ID of the grouping variable.}
#' \item{\code{get_data(data)}}{[data: (`character()`)] One ore many IDs of the grouping variable. \cr
#'  This method returns one dataframe with the chosen IDs.} \cr
#' \item{\code{add_feature(fun, check_fun)}}{[fun: (`function`)] A function, which has a dataframe as input and a named vector as output. \cr
#'  [check_fun: (`logical(1)`)] The function will be checked if it returns a vector or a  list. Disable, if calculation takes too long. \cr
#'  This method adds a function as batchtools algorithm. After that, batchtools experiments are added too.} \cr
#' \item{\code{remove_feature(fun)}}{[fun: (`function | character(1)`)] A function (or the name of the function as character) which shall be removed. \cr
#'  This method removes the batchtools algorithms and experiments corresponding to the given function.} \cr
#' \item{\code{get_feature(fun)}}{[fun: (`character(1)`)] The name of a function as character. \cr
#'  This method reads the RDS file of the function. Useful for debugging after loading an Xtractor.} \cr
#' \item{\code{calc_features()}}{This method calculates all features on all datasets. Internally, it submits all batchtools jobs, which are not done yet.} \cr
#' \item{\code{plot()}}{[internal] method to print the R6 object.} \cr
#' \item{\code{clone()}}{[internal] method to clone the R6 object.} \cr
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#'
#' @examples
#' \dontrun{
#'   xtractor = Xtractor$new("xtractor")
#'   xtractor$add_data(iris, group_by = "Species")
#'   xtractor$datasets
#'   fun = function(data) {
#'     c(mean_sepal_length = mean(data$Sepal.Length))
#'   }
#'   xtractor$add_feature(fun)
#'   xtractor$features
#'   xtractor$calc_features()
#'   xtractor$results
#'   xtractor$perc_done
#'   unlink("fxtract_files/xtractor", recursive = TRUE)
#' }
#' @import R6
#' @import dplyr
#' @import batchtools
#' @importFrom foreach "%dopar%" "%do%"
NULL

#' @export
Xtractor = R6Class("Xtractor",
  public = list(
    name = NULL,
    group_by = NULL,
    reg = NULL,
    dir = NULL,
    initialize = function(name, load = FALSE) {
      self$name = checkmate::assert_character(name)
      newDirPath = paste0("fxtract_files/", name)
      self$dir = newDirPath
      if (!load) {
        if (!dir.exists("fxtract_files")) dir.create("fxtract_files")
        if (dir.exists(newDirPath)) stop("The Xtractor name already exists. Please choose another name, delete the existing Xtractor, or set load = TRUE, if you want to load the old Xtractor.")
        dir.create(newDirPath)
        dir.create(paste0(newDirPath, "/rds_files"))
        dir.create(paste0(newDirPath, "/rds_files/data"))
        dir.create(paste0(newDirPath, "/rds_files/features"))
        saveRDS(NULL, file = paste0(self$dir, "/rds_files/group_by.RDS"))
        self$reg = batchtools::makeExperimentRegistry(paste0(newDirPath, "/reg"))
      } else {
        checkmate::assert_subset(name, list.files("fxtract_files/"))
        self$reg = batchtools::loadRegistry(paste0(newDirPath, "/reg"), writeable = TRUE)
        self$group_by = readRDS(paste0(newDirPath, "/rds_files/group_by.RDS"))
      }
    },
    print = function() {
      problems = self$reg$problems
      problems = problems[!is.na(problems)]
      if (length(problems) <= 20) {
        problems = paste0(problems, collapse = ", ")
      } else {
        problems = paste0(problems[1:20], collapse = ", ")
        problems = paste0(problems, ", ...")
      }
      algos = self$reg$algorithms
      algos = algos[!is.na(algos)]
      if (length(algos) <= 20) {
        algos = paste0(algos, collapse = ", ")
      } else {
        algos = paste0(algos[1:20], collapse = ", ")
        algos = paste0(algos, ", ...")
      }
      cat("R6 Object \n")
      cat(paste0("Name: ", self$name, "\n"))
      cat(paste0("Grouping variable: ", self$group_by, "\n"))
      cat(paste0("IDs: ", problems, "\n"))
      cat(paste0("Feature functions: ", algos, "\n"))
      cat(paste0("Backend: ", private$.backend, "\n"))
      if (private$.backend == "batchtools") cat(paste0("Percentage calculated: ", round(self$perc_done, digits = 2) * 100, "%\n"))
      invisible(self)
    },
    add_data = function(dataframe, group_by) {
      private$.results = NULL
      id = NULL
      checkmate::assert_data_frame(dataframe)
      checkmate::assert_subset(group_by, colnames(dataframe))
      checkmate::assert_character(group_by, len = 1)
      if (is.null(self$group_by)) {
        self$group_by = group_by
        saveRDS(group_by, file = paste0(self$dir, "/group_by.RDS"))
      }
      if (group_by != self$group_by) stop(paste0("The group_by variable was set to ", self$group_by,
        ". Only one group_by variable is allowed per Xtractor!"))
      gb = dataframe %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()

      #save rds files
      foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
        dataframe_i = dataframe %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
        saveRDS(dataframe_i, file = paste0(self$dir, "/rds_files/data/", i, ".RDS"))
        message(paste0("Saving raw RDS file ", i, ".RDS ", "on disk."))
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
      datasets = list.files(paste0(self$dir, "/rds_files/data"))
      #update RDS files
      foreach::foreach(i = datasets, .packages = c("dplyr")) %dopar% {
        dataframe_i = readRDS(paste0(self$dir, "/rds_files/data/", i))
        data_preproc = fun(dataframe_i)
        saveRDS(data_preproc, file = paste0(self$dir, "/rds_files/data/", i))
        message(paste0("Updating raw RDS file " , i))
      }

      #update batchtools problems
      gb = gsub(".RDS", "", datasets)
      for (id in gb) { #cannot be parallelized, because of batchtools
        batchtools::removeProblems(id, reg = self$reg)
        data_id = readRDS(paste0(self$dir, "/rds_files/data/", id, ".RDS"))
        batchtools::addProblem(name = id, data = data_id, reg = self$reg)
        #add experiments
        prob.designs = replicate(1L, data.table::data.table(), simplify = FALSE)
        names(prob.designs) = id
        private$add_experiments(prob.designs = prob.designs)
      }
      return(invisible(self))
    },
    remove_data = function(datasets) {
      private$.results = NULL
      checkmate::assert_character(datasets, min.len = 1L)
      checkmate::assert_subset(datasets, self$reg$problems)
      for (data in datasets) {
        message("Deleting RDS file ", data, ".RDS")
        unlink(paste0(paste0(self$dir, "/rds_files/data/", data, ".RDS")))
        batchtools::removeProblems(data, reg = self$reg)
      }
      return(invisible(self))
    },
    get_data = function(datasets) {
      if (missing(datasets)) datasets = self$datasets
      checkmate::assert_character(datasets, min.len = 1L)
      checkmate::assert_subset(datasets, self$datasets)
      data = foreach::foreach(data = datasets) %dopar% {
        readRDS(paste0(self$dir, "/rds_files/data/", data, ".RDS"))
      }
      dplyr::bind_rows(data)
    },
    add_feature = function(fun, check_fun = TRUE) {
      private$.results = NULL
      checkmate::assert_logical(check_fun)
      private$.check_fun = check_fun
      checkmate::assert_function(fun)
      message(paste0("Saving raw RDS file ", deparse(substitute(fun)), ".RDS ", "on disk."))
      saveRDS(fun, file = paste0(self$dir, "/rds_files/features/", deparse(substitute(fun)), ".RDS"))
      batchtools::batchExport(export = setNames(list(fun), deparse(substitute(fun))), reg = self$reg)
      batchtools::addAlgorithm(
        name = deparse(substitute(fun)),
        fun = function(job, data, instance) fxtract::dplyr_wrapper(data, group_by = self$group_by, fun = fun, check_fun = private$.check_fun),
        reg = self$reg
      )

      #add experiments
      algo.designs = replicate(1L, data.table::data.table(), simplify = FALSE)
      names(algo.designs) = deparse(substitute(fun))
      private$add_experiments(algo.designs = algo.designs)

      return(invisible(self))
    },
    remove_feature = function(fun) {
      private$.results = NULL
      if (is.function(fun)) fun = as.character(substitute(fun))
      checkmate::assert_character(fun, min.len = 1L)
      jt = batchtools::getJobTable(reg = self$reg)
      checkmate::assert_subset(fun, unique(jt$algorithm))
      for (f in fun) {
        batchtools::removeAlgorithms(f, reg = self$reg)
        unlink(paste0(self$dir, "/rds_files/features/", f, ".RDS"))
      }
      return(invisible(self))
    },
    get_feature = function(fun) {
      checkmate::assert_character(fun, len = 1L)
      checkmate::assert_subset(fun, self$features)
      readRDS(paste0(self$dir, "/rds_files/features/", fun, ".RDS"))
    },
    calc_features = function() {
      private$.results = NULL
      if (length(self$datasets) == 0) stop("Please add datasets with method $add_data().")
      if (length(self$features) == 0) stop("Please add feature functions with method $add_feature().")

      if (private$.backend == "batchtools") {
        running = batchtools::findRunning(reg = self$reg)$job.id
        not_done = batchtools::findNotDone(reg = self$reg)$job.id
        batchtools::submitJobs(ids = base::setdiff(not_done, running), reg = self$reg)
      }

      if (private$.backend == "dplyr") {
        data_all = self$get_data()
        features = self$features
        feature_list = foreach::foreach(feature = features) %dopar% {
          fun = self$get_feature(feature)
          dplyr_wrapper(data_all, group_by = self$group_by, fun = fun, check_fun = private$.check_fun)
        }
        for (i in 1:length(feature_list)) {
          if (i == 1) {
            res = feature_list[[1]]
          } else {
            res = dplyr::full_join(res, feature_list[[i]])
          }
        }
        private$.results = res
      }
      return(invisible(self))
    }
  ),
  private = list(
    add_experiments = function(prob.designs = NULL, algo.designs = NULL) {
      batchtools::addExperiments(reg = self$reg, prob.designs = prob.designs, algo.designs = algo.designs)
      return(invisible(self))
    },
    .backend = "batchtools",
    .results = NULL,
    .check_fun = NULL
  ),
  active = list(
    backend = function(backend) {
      if (missing(backend)) {
        private$.backend
      } else {
        checkmate::assert_character(backend)
        checkmate::assert_subset(backend, c("dplyr", "batchtools"))
        private$.backend = backend
        backend
      }
    },
    perc_done = function() {
      if (private$.backend != "batchtools") stop("This slot is only available if backend is set to 'batchtools'.")
      k = nrow(batchtools::getJobTable(reg = self$reg))
      n = nrow(batchtools::findDone(reg = self$reg)) / nrow(batchtools::getJobTable(reg = self$reg))
      ifelse(k == 0, 0, n)
    },
    error_messages = function() {
      if (private$.backend != "batchtools") stop("This slot is only available if backend is set to 'batchtools'.")
      err = batchtools::getErrorMessages(reg = self$reg)
      jt = batchtools::getJobTable(ids = err, reg = self$reg)
      jt = jt[, c("job.id", "error", "problem", "algorithm")]
      colnames(jt) = c("batchtools_job_id", "error", "data", "feature")
      jt
    },
    log_files = function() {
      if (private$.backend != "batchtools") stop("This slot is only available if backend is set to 'batchtools'.")
      err_table = self$error_messages
      if (nrow(err_table) >= 1) {
        log_files = lapply(1:nrow(err_table), function(x) batchtools::getLog(self$error_messages$batchtools_job_id[x]))
        log_files = setNames(log_files, paste0("batchtools_job_id ", err_table$batchtools_job_id))
        return(log_files)
      }
    },
    datasets = function() {
      self$reg$problems
    },
    features = function() {
      self$reg$algorithms
    },
    status = function() {
      if (private$.backend != "batchtools") stop("This slot is only available if backend is set to 'batchtools'.")
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
    results = function() {
      if (self$backend == "batchtools") {
        if (!is.null(private$.results)) {
          message("No new results found. Returning last generated results:")
          return(private$.results)
        }
        message("Calculating results from batchtools registry.")
        feature = job.id = problem = algorithm = NULL
        reg = self$reg
        res = batchtools::reduceResultsDataTable(reg = reg)
        if (nrow(res) == 0) stop("No features have been calculated yet. Start calculating with method $calc_features().")
        done_id = res$job.id
        res = setNames(res$result, done_id)

        jt = batchtools::getJobTable(reg = reg)
        lookup = jt %>% select(job.id, problem, algorithm)
        features = self$status$feature_wise
        features = names(features[features != 0])
        results = foreach::foreach(feature = features) %dopar% {
          ids = lookup %>% dplyr::filter(algorithm %in% feature)
          res_feat = res[names(res) %in% ids$job.id]
          data.table::rbindlist(res_feat)
        }
        final_result = results[[1]]
        if (length(results) >= 2) {
          for (i in 2:length(results)) {
            final_result = final_result %>% dplyr::full_join(results[[i]], by = self$group_by)
          }
        }
        private$.results = final_result
        return(final_result)
      }

      if (self$backend == "dplyr") {
        if (is.null(private$.results)) {
          message("Calculation has not yet been started. Starting calculation with $calc_features() now...")
          self$calc_features()
        }
        message("Retrieving results from dplyr backend:")
        return(private$.results)
      }
    }
  )
)
