#' Calculates a given function grouped by \code{group_col}.
#'
#' @template param_data
#' @template param_group_col
#' @template param_fun_1
#' @template param_check_fun
#' @param summarize function. If more than one value is returned for each group, this function summarizes these values to one value or more values. E.g. mean or/and sd.
#' @param colname character. If one variable is returned for each group, you can specify a custom column name for this new column.
#' @family feature functions
#' @return dataframe
#' @importFrom dplyr group_by do one_of
#' @importFrom magrittr "%>%"
#' @importFrom data.table dcast setDT
#' @importFrom stats as.formula setNames
#' @export
#' @examples
#' number_uses_chrome = function(data) {
#'   nrow(dplyr::filter(data, RUNNING_TASKS_baseActivity_mPackage == "com.android.chrome"))
#' }
#' calcFeature(data = studentlife_small, group_col = "userId", fun = number_uses_chrome)
calcFeature = function(data, group_col, fun, check_fun = TRUE, summarize, colname) {
  . = NULL
  checkmate::assertDataFrame(data)
  checkmate::assertNames(names(data), must.include = group_col)
  checkmate::assertLogical(check_fun)
  if (check_fun) {
    if (dim(do.call(fun, list(data)) %>% data.frame())[1] != 1) stop("fun must return a vector of length 1 or a dataframe with 1 row!")
  }

  if (!missing(summarize)) checkmate::assertFunction(summarize)
  if (!missing(colname)) checkmate::assertCharacter(colname)

  if (length(group_col) == 1) res = data %>% dplyr::group_by_(.dots = group_col) %>% dplyr::do(data.frame(do.call(fun, list(.))))
  if (length(group_col) >= 2) {
    res = data %>% dplyr::group_by_(.dots = group_col) %>% dplyr::do(data.frame(do.call(fun, list(.))))
    dcast_formula = as.formula(paste(group_col[1], "~", paste(group_col[-1], collapse = "+")))
    res = data.table::dcast(data.table::setDT(res), dcast_formula, value.var = setdiff(colnames(res), group_col))
  }

  if (ncol(res) == 2) colnames(res)[2] = ifelse(missing(colname), deparse(substitute(fun)), colname)
  res = data.frame(res)

  if (!missing(summarize)) {
    res0 = res %>% select(-one_of(group_col[1]))
    for (i in 1:nrow(res0)) {
      if (i == 1) res1 = do.call(summarize, list(unlist(res0[i, ])))
      if (i >= 2) res1 = rbind(res1, do.call(summarize, list(unlist(res0[i, ]))))
    }
    rownames(res1) = NULL
    if (ncol(res1) == 1) colnames(res1) = deparse(substitute(fun))
    if (!missing(colname) & ncol(res1) == 1) res1 = res1 %>% setNames(colname)
    res = data.frame(res %>% select(one_of(group_col[1])), res1)
  }
  return(res)
}
