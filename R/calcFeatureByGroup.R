#' Calculates a given function grouped by \code{group_col}.
#'
#' @template param_data
#' @template param_group_col
#' @template param_fun_1
#' @template param_check_fun
#' @family feature functions
#' @return dataframe
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @importFrom data.table dcast setDT
#' @importFrom stats as.formula
#' @export
#' @examples
#' number_uses_chrome = function(data) {
#'   nrow(dplyr::filter(data, RUNNING_TASKS_baseActivity_mPackage == "com.android.chrome"))
#' }
#' calcFeatureByGroup(data = studentlife.small, group_col = "userId", fun = number_uses_chrome)
calcFeatureByGroup = function(data, group_col, fun, check_fun = TRUE) {
  . = NULL
  checkmate::assertDataFrame(data)
  checkmate::assertNames(names(data), must.include = group_col)
  checkmate::assertLogical(check_fun)

  if (check_fun) {
    if (dim(do.call(fun, list(data)) %>% data.frame())[1] != 1) stop("fun must return a vector of length 1 or a dataframe with 1 row!")
  }
  
  if (length(group_col) == 1) res = data %>% dplyr::group_by_(.dots = group_col) %>% dplyr::do(data.frame(do.call(fun, list(.))))
  if (length(group_col) >= 2) {
    res = data %>% dplyr::group_by_(.dots = group_col) %>% dplyr::do(data.frame(do.call(fun, list(.))))
    dcast_formula = as.formula(paste(group_col[1], "~", paste(group_col[-1], collapse = "+")))
    data.table::dcast(setDT(res), dcast_formula, value.var = setdiff(colnames(res), group_col)) 
  }
  if (ncol(res) == 2) colnames(res)[2] = deparse(substitute(fun))
  return(res)
}
