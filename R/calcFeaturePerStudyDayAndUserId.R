#' Calculates a given function on the dataset for each studyDay and grouped by the column 'userId'.
#' Needs a function, which has a dataframe as input and a singular value as output.
#' A summary function, which summarizes the results for each day, will be evaluated for each 'userId'.
#'
#' @template param_data_uid
#' @template param_fun_n
#' @template param_colname
#' @template param_summary_fun
#' @param max_study_day integer. Function will be evaluated until the max study day.
#' @param impute_empty_day can be character, factor, or numerical.
#' Results on days, which did not have any data, will be imputed with this value.
#' @param export_results_per_day logical. If TRUE, an additional dataframe, where \code{fun} is evaluated at each studyDay, is returned.
#' @family feature functions
#' @return dataframe with columns userId and \code{colname}. One value for each userId.
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' fun = function(data) {
#'   nrow(dplyr::filter(data, RUNNING_TASKS_baseActivity_mPackage == "com.android.chrome"))
#' } #number uses google chrome
#' summary_fun = function(x) mean(x, na.rm = TRUE)
#' data = addStudyDay(studentlife.small)
#' calcFeaturePerStudyDayAndUserId(data = data, fun = fun, colname = "mean_uses_chrome_per_day",
#'  summary_fun = summary_fun)
calcFeaturePerStudyDayAndUserId = function(data, fun, colname, summary_fun, max_study_day, impute_empty_day = NA,
  export_results_per_day = FALSE) {
  userId = studyDay = min_day =  max_day = NULL
  checkmate::assertDataFrame(data)
  checkCols("userId", data)
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

  df.res = data.frame(userId = unique(data$userId))
  for (day in days) {
    df.day = data %>% dplyr::filter(studyDay == day)
    fun2 = function(x) calcFeaturePerUserId(x, fun, colname = "")
    feature.day = do.call("fun2", list(df.day))
    colnames(feature.day)[2] = as.character(day)
    df.res = suppressWarnings(dplyr::left_join(df.res, feature.day, by = "userId"))
  }

  #impute NA
  df.res[is.na(df.res)] = impute_empty_day

  #calculate summary_fun
  df.res.noId = df.res[, -which(colnames(df.res) == "userId")]
  res = data.frame(userId = unique(data$userId))
  res$n = apply(df.res.noId, 1, function(x) do.call(summary_fun, list(x)))
  colnames(res)[2] = colname

  if (export_results_per_day) {
    return(list(res = res, df.res = df.res))
  } else {
    return(res)
  }
}
