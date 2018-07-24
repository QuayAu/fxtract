#' Helper function. Needs a function, which has a dataframe as input and a vector of length nrow(dataframe) as output.
#' This function will be evaluated grouped by the column 'userId'.
#'
#' @template param_data_uid
#' @template param_fun_n
#' @template param_colname
#' @family helper functions
#' @return dataframe with added column, where a \code{fun} is evaluated grouped by 'userid'.
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
addColumnByUserId = function(data, fun, colname) {
  checkmate::assertDataFrame(data)
  checkmate::checkNames(names(data), must.include = "userId")
  if (length(do.call(fun, list(data))) != nrow(data)) stop("fun must return a vector of length: nrow data")
  userId = . = NULL
  newCol = data %>% group_by(userId) %>% do(do.call(fun, list(.)) %>% data.frame()) %>% pull(.)
  eval(parse(text = paste0("data$", colname, " = newCol")))
  return(data)
}


