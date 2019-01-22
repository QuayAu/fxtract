#' Helper function. Calculates a given function for each ID of a grouping variable.
#'
#' @template param_data
#' @param group_by (`character()`). Name of column, which contains identifiers on which the dataset should be grouped by. E.g. different user IDs.
#' @param fun (`function`). Must be a function, which has a dataframe as input and a (named) vector of desired length as output.
#' @template param_check_fun
#' @param summarize (`function`). If more than one value is returned for each ID of a grouping variable, this function summarizes these values to one value or more values. E.g. mean or/and sd.
#' @param colname (`character()`). If one variable is returned for each ID of a grouping variable, you can specify a custom column name for this new column.
#' @return (`dataframe`)
#' @family helper functions
#' @importFrom dplyr group_by do one_of
#' @importFrom magrittr "%>%"
#' @importFrom data.table dcast setDT
#' @importFrom stats as.formula setNames
#' @export
#' @examples
#' fun = function(data) {
#'   c(uses_chrome = nrow(
#'     dplyr::filter(data, RUNNING_TASKS_baseActivity_mPackage == "com.android.chrome"))
#'   )
#' }
#' calc_feature(data = studentlife_small, group_by = "userId", fun = fun)
calc_feature = function(data, group_by, fun, check_fun = TRUE, summarize, colname) {
  . = NULL
  checkmate::assertDataFrame(data)
  checkmate::assertNames(names(data), must.include = group_by)
  checkmate::assertLogical(check_fun)
  if (check_fun) checkmate::assertAtomicVector(fun(data))

  if (!missing(summarize)) checkmate::assertFunction(summarize)
  if (!missing(colname)) checkmate::assertCharacter(colname)

  if (length(group_by) == 1) res = data %>% dplyr::group_by_(.dots = group_by) %>% dplyr::do(data.frame(t(fun(.))))
  if (length(group_by) >= 2) {
    res = data %>% dplyr::group_by_(.dots = group_by) %>% dplyr::do(data.frame(t(fun(.))))
    dcast_formula = as.formula(paste(group_by[1], "~", paste(group_by[-1], collapse = "+")))
    res = data.table::dcast(data.table::setDT(res), dcast_formula, value.var = setdiff(colnames(res), group_by))
  }

  if (!missing(colname) & ncol(res) == 2) colnames(res)[2] = ifelse(missing(colname), deparse(substitute(fun)), colname)
  res = data.frame(res)

  if (!missing(summarize)) {
    res0 = res %>% select(-one_of(group_by[1]))
    for (i in 1:nrow(res0)) {
      if (i == 1) res1 = do.call(summarize, list(unlist(res0[i, ])))
      if (i >= 2) res1 = rbind(res1, do.call(summarize, list(unlist(res0[i, ]))))
    }
    res1 = data.frame(res1)
    rownames(res1) = NULL
    if (ncol(res1) == 1) colnames(res1) = deparse(substitute(summarize))
    if (!missing(colname) & ncol(res1) == 1) colnames(res1) = colname
    res = data.frame(res %>% select(one_of(group_by[1])), res1)
  }
  return(res)
}
