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
    data = NULL,
    features = NULL,
    project_status = NULL,
    group_by = NULL,
    reg = NULL,
    result = NULL,
    initialize = function(project_name, ...) {
      self$project_name = checkmate::assert_character(project_name)
      newDirPath = paste0("projects/", project_name)
      if (dir.exists("projects")) print("The project folder 'projects' already exists. The new project will be saved in this folder")
      if (!dir.exists("projects")) dir.create("projects")
      if (dir.exists(newDirPath)) stop("The project name already exists. Please choose another name or delete the existing project and try again!")
      dir.create(newDirPath)
      dir.create(paste0(newDirPath, "/raw_rds_files"))
      dir.create(paste0(newDirPath, "/batchtools_problems"))
      dir.create(paste0(newDirPath, "/batchtools_algorithms"))
      self$dir = newDirPath
      self$reg = batchtools::makeExperimentRegistry(paste0(newDirPath, "/reg"), ...)
      self$data = list()
    },
    use_dataframe = function(dataframe, group_by) {
      i = NULL
      checkmate::assert_data_frame(dataframe)
      checkmate::assert_subset(group_by, colnames(dataframe))
      gb = dataframe %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()
      foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
        dataframe_i = dataframe %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
        saveRDS(dataframe_i, file = paste0(self$dir, "/raw_rds_files/", i, ".RDS"))
      }
      self$group_by = group_by
      self$data = c(self$data, as.character(gb))
      return(invisible(self))
    },
    use_sql_database = function(file.dir, tbl_name, group_by) {
      i = NULL
      checkmate::assert_character(tbl_name)
      checkmate::assert_character(group_by)
      db = dplyr::src_sqlite(file.dir, create = FALSE)
      logs = dplyr::tbl(db, from = tbl_name)
      checkmate::assert_subset(group_by, colnames(logs))
      gb = logs %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()
      foreach::foreach(i = gb, .packages = c("dplyr")) %dopar% {
        db = dplyr::src_sqlite(file.dir, create = FALSE)
        logs = dplyr::tbl(db, from = tbl_name)
        logs_i = logs %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
        saveRDS(logs_i, file = paste0(self$dir, "/raw_rds_files/", i, ".RDS"))
      }
      self$group_by = group_by
      self$data = c(self$data, as.character(gb))
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
    submit_jobs = function(ids = NULL, resources = list(), sleep = NULL) {
      batchtools::submitJobs(ids = ids, resources = resources, sleep = sleep, reg = self$reg)
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
      features = self$get_project_status(self)$feature_wise
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
  )
)
