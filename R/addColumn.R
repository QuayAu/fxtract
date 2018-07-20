#' Helper function. Needs a function, which has a dataframe as input and a vector of length nrow(dataframe) as output.
#'
#' @template param_data_ts
#' @template param_fun_n
#' @template param_colname
#' @family helper functions
#' @return dataframe with added column.
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
addColumn = function(data, fun, colname) {
  checkmate::assertDataFrame(data)
  if (length(do.call(fun, list(data))) != nrow(data)) stop("fun must return a vector of length: nrow data")
  . = NULL
  newCol = data %>% do(do.call(fun, list(.)) %>% data.frame()) %>% pull(.)
  eval(parse(text = paste0("data$", colname, " = newCol")))
  return(data)
}
