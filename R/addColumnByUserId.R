#' Helper function. Takes the mininum of timestamp as first day and labels oncoming days with the time difference to this day.
#'
#' @template param_data_uid
#' @template param_fun_n
#' @template param_colname
#' @family helper functions
#' @return dataframe with added column
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
addColumnByUserId = function(data, fun, colname) {
  checkmate::assertDataFrame(data)
  if (length(do.call(fun, list(data))) != nrow(data)) stop("fun must return a vector of length: nrow data")
  userId = . = NULL
  newCol = data %>% group_by(userId) %>% do(do.call(fun, list(.)) %>% data.frame()) %>% pull(.)
  eval(parse(text = paste0("data$", colname, " = newCol")))
  return(data)
}


