#' Calculates a given function for each userId
#'
#' @param data dataframe containing column named 'userId', which should be the unique identifier for each ID.
#' @param fun function. Must be a function, which has a dataframe as input and a vector of length 1 as output.
#' @param colname character. Desired column name.
#' @family feature functions
#' @return dataframe with columns userId and \code{colname}.
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' myFun = function(data) nrow(data %>% filter(RUNNING_TASKS_baseActivity_mPackage == "com.android.chrome")) # 
#' calcFeaturePerUserId(data = studentlife.small, fun = myFun, colname = "number_uses_chrome")
calcFeaturePerUserId = function(data, fun, colname) {
  checkmate::assertDataFrame(data)
  if (length(do.call(fun, list(data))) != 1) stop("fun must return a vector of length 1")
  userId = . = NULL
  res = data %>% group_by(userId) %>% do(do.call(fun, list(.)) %>% data.frame())
  colnames(res)[2] = colname
  return(res)
}
