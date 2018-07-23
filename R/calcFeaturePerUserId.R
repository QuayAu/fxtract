#' Calculates a given function for each userId
#'
#' @template param_data_uid
#' @template param_fun_1
#' @param colname character. Desired column name.
#' @family feature functions
#' @return dataframe with columns userId and \code{colname}.
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' fun = function(data) {
#'   nrow(dplyr::filter(data, RUNNING_TASKS_baseActivity_mPackage == "com.android.chrome"))
#' }
#' calcFeaturePerUserId(data = studentlife.small, fun = fun, colname = "number_uses_chrome")
calcFeaturePerUserId = function(data, fun, colname) {
  userId = . = NULL
  checkmate::assertDataFrame(data)
  if (length(do.call(fun, list(data))) != 1) stop("fun must return a vector of length 1")

  res = data %>% dplyr::group_by(userId) %>% dplyr::do(do.call(fun, list(.)) %>% data.frame())
  colnames(res)[2] = colname
  return(res)
}
