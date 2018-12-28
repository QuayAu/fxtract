#' @title Project Class
#' @format [R6Class] object
#' @name Project
#' @import R6
#' @import dplyr
#' @import batchtools
#' @importFrom foreach "%dopar%"
NULL

#' @export
Project = R6Class("Project",
  public = list(
    project_name = NULL,
    dir = NULL,
    group_by = NULL,
    reg = NULL,
    initialize = function(project_name, ...) {
      self$project_name = checkmate::assert_character(project_name)
      newDirPath = paste0("projects/", project_name)
      if (!dir.exists("projects")) dir.create("projects")
      if (dir.exists(newDirPath)) stop("The project name already exists. Please choose another name or delete the existing project and try again!")
      dir.create(newDirPath)
      dir.create(paste0(newDirPath, "/raw_rds_files"))
      dir.create(paste0(newDirPath, "/batchtools_problems"))
      dir.create(paste0(newDirPath, "/batchtools_algorithms"))
      self$dir = newDirPath
      self$reg = batchtools::makeExperimentRegistry(paste0(newDirPath, "/reg"), ...)
    },
    use_dataframe = function(dataframe, group_by) {
      i = NULL
      checkmate::assert_data_frame(dataframe)
      checkmate::assert_subset(group_by, colnames(dataframe))
      if (is.null(self$group_by)) self$group_by = group_by
      if (group_by != self$group_by) stop(paste0("The group_by variable was set to ", self$group_by, 
        ". Only one group_by variable is allowed per project!"))
      gb = dataframe %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()
      foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
        dataframe_i = dataframe %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
        saveRDS(dataframe_i, file = paste0(self$dir, "/raw_rds_files/", i, ".RDS"))
        message(paste0("Saving raw RDS file " , i, ".RDS ", "on disk."))
      }
      return(invisible(self))
    },
    add_batchtools_problems = function(n.chunks) {
      chunk = files = f = NULL
      rds_files = list.files(path = paste0(self$dir, "/raw_rds_files"))
      rds_files = data.frame(files = rds_files)
      if (missing(n.chunks)) {
        for (id in rds_files$files) {
          data_id = readRDS(paste0(self$dir, "/raw_rds_files/", id))
          name = gsub(".RDS", "", id)
          write.table(NULL, file = paste0(self$dir, "/batchtools_problems/", name))
          batchtools::addProblem(name = name, data = data_id, reg = self$reg)
        }  
      } else {
        checkmate::assertIntegerish(n.chunks)
        rds_files$chunk = batchtools::chunk(1:nrow(rds_files), n.chunks = n.chunks)
        chunks = unique(rds_files$chunk)
        if (n.chunks > length(chunks)) stop("n.chunks > number of different grouping variables!")
        for (z in chunks) {
          files = rds_files %>% dplyr::filter(chunk == z) %>% dplyr::pull(files) %>% as.character()
          x = foreach::foreach(f = files) %dopar% {
            readRDS(paste0(self$dir, "/raw_rds_files/", f))
          }
          data_chunk = dplyr::bind_rows(x)
          name = paste0("chunk_", z)
          write.table(NULL, file = paste0(self$dir, "/batchtools_problems/", name))
          batchtools::addProblem(name = name, data = data_chunk, reg = self$reg)
        }
      }
      return(invisible(self))
    },
    remove_batchtools_problem = function(problem) {
      problem = sub('.*\\/', '', problem) #regex: removes everything before "/", makes auto completion possible by listing files under batchtools_problems
      batchtools::removeProblems(problem, reg = self$reg)
      unlink(paste0(self$dir, "/batchtools_problems/", problem))
      return(invisible(self))
    },
    add_feature = function(fun) {
      write.table(NULL, file = paste0(self$dir, "/batchtools_algorithms/", deparse(substitute(fun))))
      batchtools::batchExport(export = setNames(list(fun), deparse(substitute(fun))))
      batchtools::addAlgorithm(
        name = deparse(substitute(fun)),
        fun = function(job, data, instance) fxtract::calc_feature(data, group_col = self$group_by, fun = fun)
      )
      algo.designs = replicate(1L, data.table::data.table(), simplify = FALSE)
      names(algo.designs) = deparse(substitute(fun))
      batchtools::addExperiments(algo.designs = algo.designs)
      return(invisible(self))
    },
    remove_batchtools_algorithm = function(algorithm) {
      algorithm = sub('.*\\/', '', algorithm) #regex: removes everything before "/", makes auto completion possible by listing files under batchtools_algorithms
      batchtools::removeAlgorithms(algorithm, reg = self$reg)
      unlink(paste0(self$dir, "/batchtools_algorithms/", algorithm))
      return(invisible(self))
    },
    get_project_status = function() {
      problem = vars = funs = NULL
      reg = self$reg
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
    },
    collect_results = function() {
      feature = job.id = problem = algorithm = NULL
      reg = self$reg
      res = batchtools::reduceResultsDataTable(reg = reg)
      jt = batchtools::getJobTable(reg = reg)
      lookup = jt %>% select(job.id, problem, algorithm)
      features = self$get_project_status()$feature_wise
      features = names(features[features != 0])
      results = foreach::foreach(feature = features) %dopar% {
        ids = lookup %>% filter(algorithm %in% feature)
        res_feat = res[job.id %in% ids$job.id]
        list_of_dataframes = res_feat$result %>% setNames(res_feat$job.id)
        dplyr::bind_rows(list_of_dataframes)
      }
      final_result = results[[1]]
      if (length(results) >= 2) {
        for (i in 2:length(results)) {
         final_result = final_result %>% dplyr::full_join(results[[i]])
        }
      }
      final_result
    }
  )
)
