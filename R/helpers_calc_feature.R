#' Helper function. Calculates a given function for each ID of a grouping variable.
#'
#' @param data ( `dataframe`). A dataframe with a grouping variable.
#' @param group_by (`character()`). Name of column, which contains identifiers on which the dataset should be grouped by. E.g. different user IDs.
#' @param fun (`function`). Must be a function, which has a dataframe as input and a (named) vector of desired length as output.
#' @param check_fun (`logical(1)`). If \code{TRUE}, fun(data) will be evaluated and checked if the outcome is of correct form. Set to \code{FALSE}
#' if evaluation on the whole dataset takes too long.
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
  if (check_fun) {
    check_data = fun(data)
    if (!is.atomic(check_data)) {
      if (!inherits(check_data, "list")) stop("Your function must return a named vector or named list with atomic entries with 1 value each.")
    }
    if (is.atomic(check_data)) {
      if (is.null(names(check_data))) stop("Your function returns an unnamed vector. Please give each entry a name.")
      checkmate::assert_atomic_vector(check_data)
    }
    if (inherits(check_data, "list")) {
      for (i in 1:length(check_data)) {
        if (is.null(names(check_data[i]))) stop(paste0("List entry ", i, "is unnamed. Please name each list entry."))
        checkmate::assert_atomic(check_data[[i]], len = 1L)
      }
    }
  }

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

  list_entries = lapply(res, class)
  for (i in 1:length(list_entries)) {
    if (list_entries[i] == "list") res[i] = unlist(res[i])
  }

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
