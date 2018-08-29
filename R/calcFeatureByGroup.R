#' Calculates a given function grouped by \code{group_col}. For each group variable one single value is returned.
#'
#' @template param_data
#' @template param_group_col
#' @template param_fun_1
#' @template param_colname
#' @template param_check_fun
#' @family feature functions
#' @return dataframe with columns \code{group_col} and \code{colname}.
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' fun = function(data) {
#'   nrow(dplyr::filter(data, RUNNING_TASKS_baseActivity_mPackage == "com.android.chrome"))
#' }
#' calcFeatureByGroup(data = studentlife.small, group_col = "userId", fun = fun,
#'   colname = "number_uses_chrome")
calcFeatureByGroup = function(data, group_col, fun, colname, check_fun = TRUE) {
  . = NULL
  checkmate::assertDataFrame(data)
  checkmate::assertNames(names(data), must.include = group_col)
  checkmate::assertLogical(check_fun)
  if (colname %in% names(data)) stop("colname is already in dataset. Please choose a different colname!")

  if (check_fun) {
    if (length(do.call(fun, list(data))) != 1) stop("fun must return a vector of length 1")
  }

  res = data %>% dplyr::group_by_(.dots = group_col) %>% dplyr::do(do.call(fun, list(.)) %>% data.frame())
  colnames(res)[2] = colname
  return(res)
}
