#' Calculates a given function on the dataset for each studyDay and grouped by \code{group_col}.
#' Needs a function, which has a dataframe as input and a singular value as output.
#' A summary function, which summarizes the results for each day, will be evaluated by each member of \code{group_col}.
#'
#' @template param_data
#' @template param_group_col
#' @template param_fun_n
#' @template param_colname
#' @template param_summary_fun
#' @param max_study_day integer. Function will be evaluated until the max study day.
#' @param impute_empty_day can be character, factor, or numerical.
#' Results on days, which did not have any data, will be imputed with this value.
#' @param export_results_per_day logical. If TRUE, an additional dataframe, where \code{fun} is evaluated at each studyDay, is returned.
#' @family feature functions
#' @return dataframe with columns \code{group_col} and \code{colname}. One value for each variable of \code{group_col}.
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' fun = function(data) {
#'   nrow(dplyr::filter(data, RUNNING_TASKS_baseActivity_mPackage == "com.android.chrome"))
#' } #number uses google chrome
#' summary_fun = function(x) mean(x, na.rm = TRUE)
#' data = addStudyDay(studentlife.small)
#' calcFeaturePerStudyDayAndByGroup(data = data, group_col = "userId", fun = fun,
#'   colname = "mean_uses_chrome_per_day", summary_fun = summary_fun)
calcFeaturePerStudyDayAndByGroup = function(data, group_col, fun, colname, summary_fun, max_study_day, impute_empty_day = NA,
  export_results_per_day = FALSE) {
  studyDay = min_day =  max_day = NULL
  checkmate::assertDataFrame(data)
  checkmate::assertNames(names(data), must.include = group_col)
  if (!"studyDay" %in% colnames(data)) stop("Your data set needs a column named 'studyDay'.
      See function addStudyDayPerUserId() or addStudyDay().")
  if (length(do.call(fun, list(data))) != 1) stop("fun must return a vector of length 1")

  if (missing(max_study_day)) {
    message("max_study_day will be automatically calculated from the given data.")
    day_end = data %>% dplyr::summarize(max_day = max(studyDay)) %>% dplyr::pull(max_day) %>% as.numeric()
  } else {
    day_end = max_study_day
  }

  day_start = data %>% dplyr::summarize(min_day = min(studyDay)) %>% dplyr::pull(min_day) %>% as.numeric()
  day_levels = paste0("day", day_start:day_end)
  days = factor(day_levels, levels = day_levels, ordered = TRUE)

  df.res = data.frame(unique(data[group_col]))
  for (day in days) {
    df.day = data %>% dplyr::filter(studyDay == day)
    fun2 = function(x) calcFeatureByGroup(x, group_col = group_col, fun = fun, colname = "")
    feature.day = do.call("fun2", list(df.day))
    colnames(feature.day)[2] = as.character(day)
    df.res = suppressWarnings(dplyr::left_join(df.res, feature.day, by = group_col))
  }

  #impute NA
  df.res[is.na(df.res)] = impute_empty_day

  #calculate summary_fun
  df.res.noId = df.res[, -which(colnames(df.res) == group_col)]
  res = data.frame(unique(data[group_col]))
  res$n = apply(df.res.noId, 1, function(x) do.call(summary_fun, list(x)))
  colnames(res)[2] = colname

  if (export_results_per_day) {
    return(list(res = res, df.res = df.res))
  } else {
    return(res)
  }
}
