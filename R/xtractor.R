#' R6 Object for Feature Extraction.
#'
#' @description
#' \code{Xtractor} calculates features from raw data for each ID of a grouping variable individually. This process can be parallelized with the package future.
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
#' \item{\code{name}: }{(`character(1)`): A user defined name of the Xtractor. All necessary data will be saved on the path: ./fxtract_files/name/}
#' \item{\code{load}: }{(`logical(1)`): If TRUE, an existing Xtractor will be loaded.}
#' \item{\code{file.dir}: }{(`character(1)`): Path where all files of the Xtractor are saved. Default is the current working directory.}
#' }
#' @section Details:
#' All datasets and feature functions are saved in this R6 object.
#' Datasets will be saved as single RDS files (for each ID) and feature functions are calculated on each single dataset.
#' A big advantage of this method is that it scales nicely for larger datasets. Data is only read into RAM, when needed.
#'
#' @section Fields:
#' \describe{
#' \item{\code{error_messages}: }{(`data.frame()`): Active binding. A dataframe with information about error messages.}
#' \item{\code{ids}: }{(`character()`): Active binding. A character vector with the IDs of the grouping variable.}
#' \item{\code{features}: }{(`character()`): Active binding. A character vector with the feature functions which were added.}
#' \item{\code{status}: }{(`data.frame()`): Active binding. A dataframe with an overview over which features are calculated on which datasets.}
#' \item{\code{results}: }{(`data.frame()`): Active binding. A dataframe with all calculated features of all IDs.}
#' }
#'
#' @section Methods:
#' \describe{
#' \item{\code{add_data(data, group_by)}}{[data: (`data.frame` | `data.table`)] A dataframe or data.table which shall be added to the R6 object. \cr
#'  [group_by: (`character(1)`)] The grouping variable's name of the dataframe. \cr \cr
#'  This method writes single RDS files (this can be parallelized with future)}
#' \item{\code{preprocess_data(fun)}}{[fun: (`function`)] A function, which has a dataframe as input and a dataframe as output. \cr \cr
#'  This method loads the RDS files and applies this function on them. The old RDS files are overwritten.}
#' \item{\code{remove_data(ids)}}{[ids: (`character()`)] One or many IDs of the grouping variable. \cr \cr
#'  This method deletes the RDS files of the given IDs.}
#' \item{\code{get_data(ids)}}{[ids: (`character()`)] One or many IDs of the grouping variable. \cr \cr
#'  This method returns one dataframe with the chosen IDs.}
#' \item{\code{add_feature(fun, check_fun)}}{[fun: (`function`)] A function, which has a dataframe as input and a named vector or list as output. \cr
#'  [check_fun: (`logical(1)`)] The function will be checked if it returns a vector or a  list. Defaults to \code{TRUE}. Disable, if calculation takes too long. \cr \cr
#'  This method adds the feature function to the R6 object. It writes an RDS file of the function which can be retrieved later.}
#' \item{\code{remove_feature(fun)}}{[fun: (`function | character(1)`)] A function (or the name of the function as character) which shall be removed. \cr \cr
#'  This method removes the function from the object and deletes all corresponding files and results.}
#' \item{\code{get_feature(fun)}}{[fun: (`character(1)`)] The name of a function as character. \cr \cr
#'  This method reads the RDS file of the function. Useful for debugging after loading an Xtractor.}
#' \item{\code{calc_features(features, ids)}}{[features: (`character()`)] A character vector of the names of the features which shall be calculated. Defaults to all features. \cr
#' [ids: (`character()`)] One or many IDs of the grouping variable. Defaults to all IDs. \cr \cr
#' This method calculates all features on the chosen IDs.}
#' \item{\code{retry_failed_features(features)}}{[features: (`character()`)] A character vector of the names of the features which shall be calculated. Defaults to all features. \cr \cr
#' This method retries calculation of failed features. Useful if calculation failed because of memory problems.}
#' \item{\code{plot()}}{[internal] method to print the R6 object.}
#' \item{\code{clone()}}{[internal] method to clone the R6 object.}
#' \item{\code{initialize()}}{[internal] method to initialize the R6 object.}
#' }
#'
#' @examples
#' # one feature function
#' dir = tempdir()
#' xtractor = Xtractor$new("xtractor", file.dir = dir)
#' xtractor$add_data(iris, group_by = "Species")
#' xtractor$ids
#' fun = function(data) {
#'   c(mean_sepal_length = mean(data$Sepal.Length))
#' }
#' xtractor$add_feature(fun)
#' xtractor$features
#' xtractor$calc_features()
#' xtractor$results
#' xtractor$status
#' xtractor
#'
#' # failing function on only one ID
#' fun2 = function(data) {
#'   if ("setosa" %in% data$Species) stop("my error")
#'   c(sd_sepal_length = sd(data$Sepal.Length))
#' }
#' xtractor$add_feature(fun2)
#' xtractor$calc_features()
#' xtractor$results
#' xtractor$error_messages
#' xtractor
#'
#' # remove feature function
#' xtractor$remove_feature("fun2")
#' xtractor$results
#' xtractor
#'
#' # remove ID
#' xtractor$remove_data("setosa")
#' xtractor$results
#' xtractor$ids
#' xtractor
#'
#' # get datasets and functions
#' fun3 = xtractor$get_feature("fun")
#' df = xtractor$get_data()
#' dplyr_wrapper(data = df, group_by = "Species", fun = fun3)
#'
#' @import R6
#' @import dplyr
#' @import future.apply
NULL

#' @export
Xtractor = R6Class("Xtractor",
  public = list(
    initialize = function(name, file.dir = ".", load = FALSE) {
      private$name = checkmate::assert_character(name)
      newDirPath = paste0(file.dir, "/fxtract_files/", name)
      private$dir = newDirPath
      if (!load) {
        if (!dir.exists(paste0(file.dir, "/fxtract_files"))) dir.create(paste0(file.dir, "/fxtract_files"))
        if (dir.exists(newDirPath)) stop("The Xtractor name already exists. Please choose another name, delete the existing Xtractor, or set load = TRUE, if you want to load the old Xtractor.")
        dir.create(newDirPath)
        dir.create(paste0(newDirPath, "/rds_files"))
        dir.create(paste0(newDirPath, "/rds_files/data"))
        dir.create(paste0(newDirPath, "/rds_files/features"))
        dir.create(paste0(newDirPath, "/rds_files/results"))
        dir.create(paste0(newDirPath, "/rds_files/results/done"))
        dir.create(paste0(newDirPath, "/rds_files/results/failed"))
        saveRDS(NULL, file = paste0(private$dir, "/rds_files/group_by.RDS"))
      } else {
        checkmate::assert_subset(name, list.files(paste0(file.dir, "/fxtract_files/")))
        private$group_by = readRDS(paste0(newDirPath, "/rds_files/group_by.RDS"))
      }
    },
    print = function() {
      ids = self$ids
      feats = self$features

      cat("R6 Object: Xtractor\n")
      cat(paste0("Name: ", private$name, "\n"))
      cat(paste0("Grouping variable: ", private$group_by, "\n"))
      if (length(ids) <= 10) {
        cat(paste0("IDs: ", paste0(ids, collapse = ", "), "\n"))
      } else {
        cat(paste0("Number IDs: ", length(ids), ". See $ids for all ids.\n"))
      }
      if (length(feats) <= 10) {
        cat(paste0("Feature functions: ", paste0(feats, collapse = ", "), "\n"))
      } else {
        cat(paste0("Number feature functions: ", length(feats), ". See $features for all feature functions.\n"))
      }
      if (ncol(self$status[, -1, drop = FALSE]) >= 1) cat(paste0("Extraction done: ", mean(as.matrix(self$status[, -1]) == "done") * 100, "%\n"))
      cat(paste0("Errors during calculation: ", nrow(self$error_messages), " \n"))
      invisible(self)
    },
    add_data = function(data, group_by) {
      checkmate::assert_subset(group_by, colnames(data))
      checkmate::assert(
        checkmate::checkClass(data, "data.frame"),
        checkmate::checkClass(data, "data.table")
      )
      checkmate::assert_subset(group_by, colnames(data))
      checkmate::assert_character(group_by, len = 1)
      if (is.null(private$group_by)) {
        private$group_by = group_by
        saveRDS(group_by, file = paste0(private$dir, "/rds_files/group_by.RDS"))
      }
      if (group_by != private$group_by) stop(paste0("The group_by variable was set to ", private$group_by,
        ". Only one group_by variable is allowed per Xtractor!"))
      gb = data %>% dplyr::distinct_(.dots = group_by) %>% data.frame() %>% unlist()

      if (any(gb %in% self$ids)) stop(paste0("Adding data multiple times is not allowed! Following ID(s) are already added to the R6 object: ", paste0(gb[which(gb %in% self$ids)], collapse = ", ")))
      #save rds files, we want this no matter the backend (because of preprocessing data per ID)
      message("Saving raw RDS files.")
      lapply(gb, function(i) {
        data_i = data %>% dplyr::filter(!!as.name(group_by) == i) %>% data.frame()
        saveRDS(data_i, file = paste0(private$dir, "/rds_files/data/", i, ".RDS"))
      })
      return(invisible(self))
    },
    preprocess_data = function(fun) {
      message("Updating raw RDS files. Parallelize by calling future::plan(multiprocess) before.")
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
      for (id in ids) {
        message("Deleting RDS file ", id, ".RDS")
        unlink(paste0(paste0(private$dir, "/rds_files/data/", id, ".RDS")))
      }

      #delete done
      done_dir = paste0(private$dir, "/rds_files/results/done/")
      done_features = list.files(done_dir)
      for (feature in done_features) {
        for (id in ids) {
          if (file.exists(paste0(private$dir, "/rds_files/results/done/", feature, "/", id, ".RDS")))
            message(paste0("Deleting results from id: ", id))
          unlink(paste0(private$dir, "/rds_files/results/done/", feature, "/", id, ".RDS"))
        }
      }
      #delete error
      failed_dir = paste0(private$dir, "/rds_files/results/failed/")
      failed_features = list.files(failed_dir)
      for (feature in failed_features) {
        for (id in ids) {
          if (file.exists(paste0(private$dir, "/rds_files/results/failed/", feature, "/", id, ".RDS")))
            message(paste0("Deleting error messages from id: ", id))
          unlink(paste0(private$dir, "/rds_files/results/failed/", feature, "/", id, ".RDS"))
        }
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
      if (length(unique(lapply(data, function(df) typeof(df[, private$group_by])))) > 1) {
        message("Different vector types detected. Converting group column to character.")
        for (i in 1:length(data)) {
          data[[i]][, private$group_by] = as.character(data[[i]][, private$group_by])
        }
      }
      dplyr::bind_rows(data)
    },
    add_feature = function(fun, check_fun = TRUE) {
      checkmate::assert_logical(check_fun)
      checkmate::assert_function(fun)
      if (deparse(substitute(fun)) %in% self$features) stop(paste0("Feature function '", deparse(substitute(fun)), "' was already added."))
      message(paste0("Saving raw RDS file ", deparse(substitute(fun)), ".RDS ", "on disk."))
      saveRDS(list(fun = fun, check_fun = check_fun), file = paste0(private$dir, "/rds_files/features/", deparse(substitute(fun)), ".RDS"))
      dir.create(paste0(private$dir, "/rds_files/results/done/", deparse(substitute(fun))))
      dir.create(paste0(private$dir, "/rds_files/results/failed/", deparse(substitute(fun))))
      return(invisible(self))
    },
    remove_feature = function(fun) {
      if (is.function(fun)) fun = as.character(substitute(fun))
      checkmate::assert_character(fun, min.len = 1L)
      checkmate::assert_subset(fun, self$features)
      for (f in fun) {
        unlink(paste0(private$dir, "/rds_files/features/", f, ".RDS"))
        unlink(paste0(private$dir, "/rds_files/results/done/", f), recursive = TRUE)
        unlink(paste0(private$dir, "/rds_files/results/failed/", f), recursive = TRUE)
      }
      return(invisible(self))
    },
    get_feature = function(fun) {
      checkmate::assert_character(fun, len = 1L)
      checkmate::assert_subset(fun, self$features)
      readRDS(paste0(private$dir, "/rds_files/features/", fun, ".RDS"))$fun
    },
    calc_features = function(features, ids) {
      message("Parallelize by calling future::plan(multiprocess) before.")
      if (missing(features)) features = self$features
      checkmate::assert_subset(features, self$features)

      if (!missing(ids)) checkmate::assert_character(ids)
      if (!missing(ids)) checkmate::assert_subset(ids, self$ids)

      if (length(self$ids) == 0) stop("Please add datasets with method $add_data().")
      if (length(self$features) == 0) stop("Please add feature functions with method $add_feature().")

      #skip features, which have already been calculated
      #FIXME: rewrite this
      features_new = features

      for (feature in features) {
        if (all(self$status[[feature]] == 1)) {
          features_new = setdiff(features_new, feature)
          message(paste0("Feature function '", feature, "' was already applied on every ID and will be skipped."))
        }
      }

      #calculating features using futures
      for (feature in features_new) {
        message(paste0("Calculating feature function: ", feature))
        feat_fun = self$get_feature(feature)
        if (missing(ids)) {
          ids_calc = self$ids
        } else {
          ids_calc = ids
        }

        res_value = future.apply::future_lapply(ids_calc, function(x) {
          data = self$get_data(x)
          group_by = private$group_by
          res_id = tryCatch(fxtract::dplyr_wrapper(data, group_by, feat_fun, check_fun = private$get_check_fun(feature)), error = function(e) e$message)

          #if error, save as error, else save result
          if (is.character(res_id)) {
            saveRDS(res_id, file = paste0(private$dir, "/rds_files/results/failed/", feature, "/", x, ".RDS"))
          } else {
            unlink(paste0(private$dir, "/rds_files/results/failed/", feature, "/", x, ".RDS"))
            saveRDS(res_id, file = paste0(private$dir, "/rds_files/results/done/", feature, "/", x, ".RDS"))
          }
        }, future.seed = TRUE)
      }
      return(invisible(self))
    }
  ),
  private = list(
    name = NULL,
    group_by = NULL,
    dir = NULL,
    get_check_fun = function(fun) {
      checkmate::assert_character(fun, len = 1L)
      checkmate::assert_subset(fun, self$features)
      readRDS(paste0(private$dir, "/rds_files/features/", fun, ".RDS"))$check_fun
    }
  ),
  active = list(
    error_messages = function() {
      error_df = setNames(data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = FALSE), c("feature_function", "id", "error_message"))
      for (feat in self$features) {
        error_feats = list.files(paste0(private$dir, "/rds_files/results/failed/", feat))
        if (length(error_feats) == 0) next
        for (file in error_feats) {
          error_message = readRDS(paste0(private$dir, "/rds_files/results/failed/", feat, "/", file))
          error_df = dplyr::bind_rows(error_df, data.frame(feature_function = feat,
            id = gsub(".RDS", "", file), error_message = error_message, stringsAsFactors = FALSE))
        }
      }
      data.table::data.table(error_df)
    },
    ids = function() {
      gsub(".RDS", "", list.files(paste0(private$dir, "/rds_files/data")))
    },
    features = function() {
      gsub(".RDS", "", list.files(paste0(private$dir, "/rds_files/features")))
    },
    results = function() {
      todo_data = gsub(".RDS", "", list.files(paste0(private$dir, "/rds_files/data/")))
      final_result = setNames(data.frame(todo_data, stringsAsFactors = FALSE), private$group_by)
      if (nrow(final_result) == 0) return(final_result)

      for (feat in self$features) {
        results_feat = future.apply::future_lapply(list.files(paste0(private$dir, "/rds_files/results/done/", "/", feat), full.names = TRUE), readRDS)
        results_feat = data.table::rbindlist(results_feat) %>% data.frame()
        results_feat[, private$group_by] = as.character(results_feat[, private$group_by])
        final_result = dplyr::full_join(final_result, results_feat, by = private$group_by)
      }
      data.table::data.table(final_result)
    },
    status = function() {
      todo_data = gsub(".RDS", "", list.files(paste0(private$dir, "/rds_files/data/")))
      todo_feats = gsub(".RDS", "", list.files(paste0(private$dir, "/rds_files/features/")))
      status = setNames(data.frame(todo_data, stringsAsFactors = FALSE), private$group_by)
      if (nrow(status) == 0) return(status)
      for (feat in todo_feats) {
        #make done df
        done_feat = gsub(".RDS", "", list.files(paste0(private$dir, "/rds_files/results/done/", feat)))
        if (length(done_feat) >= 1) {
          done_df = setNames(data.frame(done_feat, "done", stringsAsFactors = FALSE), c(private$group_by, feat))
        } else {
          done_df = setNames(data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors = FALSE), c(private$group_by, feat))
        }
        #make error df
        error_feat = gsub(".RDS", "", list.files(paste0(private$dir, "/rds_files/results/failed/", feat)))
        if (length(error_feat) >= 1) {
          error_df = setNames(data.frame(error_feat, "failed", stringsAsFactors = FALSE), c(private$group_by, feat))
        } else {
          error_df = setNames(data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors = FALSE), c(private$group_by, feat))
        }
        #make not done df
        not_done_feat = setdiff(todo_data, c(done_feat, error_feat))
        if (length(not_done_feat) >= 1) {
          not_done_df = setNames(data.frame(not_done_feat, "not_done", stringsAsFactors = FALSE), c(private$group_by, feat))
        } else {
          not_done_df = setNames(data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors = FALSE), c(private$group_by, feat))
        }
        status_feat = dplyr::bind_rows(done_df, error_df, not_done_df)
        status = dplyr::left_join(status, status_feat, by = private$group_by)
      }
      status
    }
  )
)
