#' @title Project Class
#' @format [R6Class] object
#' @name Project
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
    initialize = function(project_name, ...) {
      self$project_name = checkmate::assert_character(project_name)
      newDirPath = paste0("projects/", project_name)
      if (!dir.exists("projects")) dir.create("projects")
      if (dir.exists(newDirPath)) stop("The project name already exists. Please choose another name or delete the existing project and try again!")
      dir.create(newDirPath)
      private$dir = newDirPath
      dir.create(paste0(newDirPath, "/raw_rds_files"))
      self$reg = batchtools::makeExperimentRegistry(paste0(newDirPath, "/reg"), ...)
    },
    add_data = function(dataframe, group_by) {
      id = NULL
      checkmate::assert_data_frame(dataframe)
      checkmate::assert_subset(group_by, colnames(dataframe))
      checkmate::assert_character(group_by, len = 1)
      if (is.null(self$group_by)) self$group_by = group_by
      if (group_by != self$group_by) stop(paste0("The group_by variable was set to ", self$group_by,
        ". Only one group_by variable is allowed per project!"))
      gb = dataframe %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()

      #save rds files
      foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
        dataframe_i = dataframe %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
        saveRDS(dataframe_i, file = paste0(private$dir, "/raw_rds_files/", i, ".RDS"))
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
    remove_problems = function(problems) {
      checkmate::assert_character(problems)
      checkmate::assert_subset(problems, self$reg$problems)
      batchtools::removeProblems(problems, reg = self$reg)
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
      batchtools::submitJobs(reg = self$reg)
      return(invisible(self))
    },
    get_project_status = function() {
      problem = vars = funs = NULL
      reg = self$reg
      if (nrow(batchtools::findDone(reg = reg)) == 0) {
        message("No features have been calculated yet. Start calculating with method $submit_jobs(). Added data and features:")
        return(list(data = reg$problems, features = reg$algorithms))
      }
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
      if (nrow(res) == 0) stop("No features have been calculated yet. Start calculating with method $submit_jobs().")
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
    },
    dir = NULL
  )
)
