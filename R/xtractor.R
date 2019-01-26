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
    initialize = function(name, file.dir = ".", load = FALSE) {
      private$name = checkmate::assert_character(name)
      newDirPath = paste0(file.dir, "/fxtract_files/", name)
      private$dir = newDirPath
      if (!load) {
        if (!dir.exists("fxtract_files")) dir.create("fxtract_files")
        if (dir.exists(newDirPath)) stop("The Xtractor name already exists. Please choose another name, delete the existing Xtractor, or set load = TRUE, if you want to load the old Xtractor.")
        dir.create(newDirPath)
        dir.create(paste0(newDirPath, "/rds_files"))
        dir.create(paste0(newDirPath, "/rds_files/data"))
        dir.create(paste0(newDirPath, "/rds_files/features"))
        dir.create(paste0(newDirPath, "/rds_files/results"))
        dir.create(paste0(newDirPath, "/rds_files/results/done"))
        dir.create(paste0(newDirPath, "/rds_files/results/status"))
        dir.create(paste0(newDirPath, "/rds_files/results/failed"))
        saveRDS(NULL, file = paste0(private$dir, "/rds_files/group_by.RDS"))
      } else {
        checkmate::assert_subset(name, list.files("fxtract_files/"))
        private$group_by = readRDS(paste0(newDirPath, "/rds_files/group_by.RDS"))
      }
    },
    print = function() {
      ids = self$ids
      feats = self$features

      cat("R6 Object: Xtractor_future \n")
      cat(paste0("Name: ", private$name, "\n"))
      cat(paste0("Grouping variable: ", private$group_by, "\n"))
      if (length(ids) <= 10) {
        cat(paste0("IDs: ", paste0(ids, collapse = ", "), "\n"))
      } else {
        cat(paste0("Number IDs: ", length(ids), ". See $ids for all ids. \n"))
      }
      if (length(feats) <= 10) {
        cat(paste0("Feature functions: ", paste0(feats, collapse = ", "), "\n"))
      } else {
        cat(paste0("Number feature functions: ", length(feats), ". See $features for all feature functions. \n"))
      }
      if (ncol(self$status[, -1, drop = FALSE]) >= 1) cat(paste0("Calculation process done: ", mean(as.matrix(self$status[, -1])) * 100, "% \n"))
      cat(paste0("Errors during calculation: ", nrow(self$error_messages), " \n"))
      invisible(self)
    },
    add_data = function(data, group_by) {
      checkmate::assert_data_frame(data)
      checkmate::assert_subset(group_by, colnames(data))
      checkmate::assert_character(group_by, len = 1)
      if (is.null(private$group_by)) {
        private$group_by = group_by
        saveRDS(group_by, file = paste0(private$dir, "/rds_files/group_by.RDS"))
      }
      if (group_by != private$group_by) stop(paste0("The group_by variable was set to ", private$group_by,
        ". Only one group_by variable is allowed per Xtractor!"))
      gb = data %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()

      #save rds files, we want this no matter the backend (because of preprocessing data per ID)
      message("Saving raw RDS files. This might take a while. You can speed up this process by calling future::plan(multiprocess) before adding data!")
      future.apply::future_lapply(gb, function(i) {
        data_i = data %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
        saveRDS(data_i, file = paste0(private$dir, "/rds_files/data/", i, ".RDS"))
      })
      return(invisible(self))
    },
    preprocess_data = function(fun) {
      message("Saving raw RDS files. This might take a while. You can speed up this process by calling future::plan(multiprocess) before preprocessing data!")
      future.apply::future_lapply(self$ids, function(i) {
        data_i = readRDS(paste0(private$dir, "/rds_files/data/", i, ".RDS"))
        data_preproc = fun(data_i)
        saveRDS(data_preproc, file = paste0(private$dir, "/rds_files/data/", i, ".RDS"))
      }, future.seed = TRUE)
      return(invisible(self))
    },
    remove_data = function(ids) {
      checkmate::assert_character(ids, min.len = 1L)
      checkmate::assert_subset(ids, self$ids)
      for (data in ids) {
        message("Deleting RDS file ", data, ".RDS")
        unlink(paste0(paste0(private$dir, "/rds_files/data/", data, ".RDS")))
      }
      return(invisible(self))
    },
    get_data = function(ids) {
      if (missing(ids)) ids = self$ids
      checkmate::assert_character(ids, min.len = 1L)
      checkmate::assert_subset(ids, self$ids)
      data = future.apply::future_lapply(ids, function(i) {
        readRDS(paste0(private$dir, "/rds_files/data/", i, ".RDS"))
      })
      dplyr::bind_rows(data)
    },
    add_feature = function(fun, check_fun = TRUE) {
      checkmate::assert_logical(check_fun)
      private$.check_fun = check_fun
      checkmate::assert_function(fun)
      message(paste0("Saving raw RDS file ", deparse(substitute(fun)), ".RDS ", "on disk."))
      saveRDS(fun, file = paste0(private$dir, "/rds_files/features/", deparse(substitute(fun)), ".RDS"))
      return(invisible(self))
    },
    remove_feature = function(fun) {
      if (is.function(fun)) fun = as.character(substitute(fun))
      checkmate::assert_character(fun, min.len = 1L)
      checkmate::assert_subset(fun, self$features)
      for (f in fun) {
        unlink(paste0(private$dir, "/rds_files/features/", f, ".RDS"))
        unlink(paste0(private$dir, "/rds_files/results/done/", f, ".RDS"))
        unlink(paste0(private$dir, "/rds_files/results/failed/", f, ".RDS"))
        unlink(paste0(private$dir, "/rds_files/results/status/", f, ".RDS"))
      }
      return(invisible(self))
    },
    get_feature = function(fun) {
      checkmate::assert_character(fun, len = 1L)
      checkmate::assert_subset(fun, self$features)
      readRDS(paste0(private$dir, "/rds_files/features/", fun, ".RDS"))
    },
    calc_features = function(features) {
      if (missing(features)) features = self$features
      checkmate::assert_subset(features, self$features)
      if (length(self$ids) == 0) stop("Please add datasets with method $add_data().")
      if (length(self$features) == 0) stop("Please add feature functions with method $add_feature().")

      #skip features, which have already been calculated
      features_new = features
      for (feature in features) {
        if (all(self$status[[feature]] == 1)) features_new = setdiff(features_new, feature)
      }

      #calculating features using futures
      for (feature in features_new) {
        feat_fun = self$get_feature(feature)
        ids_calc = self$ids
        feat_already_calc = paste0(private$dir, "/rds_files/results/status/", feature, ".RDS")
        if (file.exists(feat_already_calc)) {
          ids_calc = setdiff(ids_calc, readRDS(feat_already_calc)$ids)
        }
        res_feat = future.apply::future_lapply(ids_calc, function(x) {
          data = self$get_data(x)
          group_by = private$group_by
          future(fxtract::dplyr_wrapper(data, group_by, feat_fun))
        }, future.seed = TRUE)
        res_value = setNames(lapply(res_feat, function(x) tryCatch(value(x), error = function(e) e$message)), ids_calc)
        is_error = sapply(res_value, is.character)
        if (any(is_error)) for (error in which(is_error)) message(paste0("Feature ", feature, " failed on ID ", names(is_error)[error], ". See $error_messages for more details."))

        #save done
        res_data = data.table::rbindlist(res_value[!is_error])
        done_exist = paste0(private$dir, "/rds_files/results/done/", feature, ".RDS")
        if (file.exists(done_exist)) res_data = data.table::rbindlist(list(res_data, readRDS(done_exist)))
        saveRDS(res_data, done_exist)

        #save status
        status_data = data.frame(ids = as.character(ids_calc), feature = 1, stringsAsFactors = FALSE)
        colnames(status_data)[2] = feature
        done_status = paste0(private$dir, "/rds_files/results/status/", feature, ".RDS")
        if (file.exists(done_status)) status_data = data.table::rbindlist(list(status_data, readRDS(done_status)))
        saveRDS(status_data, done_status)

        #save error messages
        res_error = res_value[is_error]
        if (length(res_error) >= 1) for (i in 1:length(res_error)) res_error[[i]] = data.frame(feature_function = feature, id = names(res_error[i]), error_message = res_error[[i]])
        res_error = data.table::rbindlist(res_error)
        done_error = paste0(private$dir, "/rds_files/results/failed/", feature, ".RDS")
        if (file.exists(done_error)) res_error = data.table::rbindlist(list(res_error, readRDS(done_error)))
        saveRDS(res_error, done_error)
      }
      return(invisible(self))
    },
    retry_failed_features = function(features) {
      message("This result in non reproducible results. Make sure your feature function is not stochastical. Remove and add feature function again (and add seed) for stochastical features.")
      if (missing(features)) features = self$features
      checkmate::assert_subset(features, self$features)
      if (nrow(self$error_messages) == 0) stop("No failed features found!")
      error_feats = as.character(unique(self$error_messages$feature_function))
      features_new = intersect(features, error_feats)

      #calculating features using futures
      for (feature in features_new) {
        message(paste0("Retrying feature function: ", feature))
        feat_fun = self$get_feature(feature)
        ids_calc = self$error_messages %>% dplyr::filter(feature_function == feature) %>% dplyr::pull(id) %>% as.character()
        res_feat = future.apply::future_lapply(ids_calc, function(x) {
          data = self$get_data(x)
          group_by = private$group_by
          future(fxtract::dplyr_wrapper(data, group_by, feat_fun))
        }, future.seed = TRUE)
        res_value = setNames(lapply(res_feat, function(x) tryCatch(value(x), error = function(e) e$message)), ids_calc)
        is_error = sapply(res_value, is.character)
        if (any(is_error)) for (error in which(is_error)) message(paste0("Feature ", feature, " failed on ID ", names(is_error)[error], ". See $error_messages for more details."))

        #save done
        res_data = data.table::rbindlist(res_value[!is_error])
        done_exist = paste0(private$dir, "/rds_files/results/done/", feature, ".RDS")
        if (file.exists(done_exist)) res_data = data.table::rbindlist(list(res_data, readRDS(done_exist)))
        saveRDS(res_data, done_exist)

        #save status
        #only resolved IDs were used. Status not changed.

        #save error messages
        res_error = res_value[is_error]
        if (length(res_error) >= 1) for (i in 1:length(res_error)) res_error[[i]] = data.frame(feature_function = feature, id = names(res_error[i]), error_message = res_error[[i]])
        res_error = data.table::rbindlist(res_error)
        done_error = paste0(private$dir, "/rds_files/results/failed/", feature, ".RDS")
        if (file.exists(done_error)) {
          old_error = readRDS(done_error)
          old_error = old_error %>% dplyr::filter(!id %in% ids_calc)
          res_error = data.table::rbindlist(list(res_error, old_error))
        }
        saveRDS(res_error, done_error)
      }
      return(invisible(self))
    }
  ),
  private = list(
    name = NULL,
    group_by = NULL,
    dir = NULL,
    .results = NULL,
    .check_fun = NULL
  ),
  active = list(
    error_messages = function() {
      dir_failed = paste0(private$dir, "/rds_files/results/failed/")
      failed = list.files(dir_failed)
      errors = future.apply::future_lapply(failed, function(x) readRDS(paste0(dir_failed, "/", x)))
      data.table::rbindlist(errors)
    },
    ids = function() {
      gsub(".RDS", "", list.files(paste0(private$dir, "/rds_files/data")))
    },
    features = function() {
      gsub(".RDS", "", list.files(paste0(private$dir, "/rds_files/features")))
    },
    results = function() {
      dir_done = paste0(private$dir, "/rds_files/results/done/")
      done = list.files(dir_done)
      results = future.apply::future_lapply(done, function(x) readRDS(paste0(dir_done, "/", x)))
      final_result = results[[1]]
      if (length(results) >= 2) {
        for (i in 2:length(results)) {
          final_result = final_result %>% dplyr::full_join(results[[i]], by = private$group_by)
        }
      }
      final_result
    },
    status = function() {
      todo_data = gsub(".RDS", "", list.files(paste0(private$dir, "/rds_files/data/")))
      todo_feats = gsub(".RDS", "", list.files(paste0(private$dir, "/rds_files/features/")))
      done_feats = list.files(paste0(private$dir, "/rds_files/results/status/"))
      done_feats_list = future.apply::future_lapply(done_feats, function(x) readRDS(paste0(private$dir, "/rds_files/results/status/", x)))
      status = data.frame(ids = todo_data, stringsAsFactors = FALSE)
      if (length(done_feats_list) >= 1) for (i in 1:length(done_feats_list)) status = status %>% dplyr::left_join(done_feats_list[[i]], by = "ids")
      status[is.na(status)] = 0
      for (feat in todo_feats) if (!feat %in% colnames(status)) eval(parse(text = paste0("status$", feat, " = 0")))
      status
    }
  )
)
