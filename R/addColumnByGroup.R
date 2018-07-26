#' Helper function. Needs a function, which has a dataframe as input and a vector of length nrow(dataframe) as output.
#' This function will be evaluated grouped by \code{group_col}.
#'
#' @template param_data
#' @template param_group_col
#' @template param_fun_n
#' @template param_colname
#' @template param_check_fun
#' @family helper functions
#' @return dataframe with added column, where a function \code{fun} is evaluated grouped by \code{group_col}.
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
addColumnByGroup = function(data, group_col, fun, colname, check_fun = TRUE) {
  checkmate::assertDataFrame(data)
  checkmate::assertNames(names(data), must.include = group_col)
  checkmate::assertLogical(check_fun)
  if (colname %in% names(data)) stop("colname is already in dataset. Please choose a different colname!")

  if (check_fun) {
    if (length(do.call(fun, list(data))) != nrow(data)) stop("fun must return a vector of length: nrow data")
  }
  
  . = NULL
  newCol = data %>% dplyr::group_by_(group_col) %>% dplyr::do(do.call(fun, list(.)) %>% data.frame()) %>% dplyr::pull(.)
  eval(parse(text = paste0("data$", colname, " = newCol")))
  return(data)
}


