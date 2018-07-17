#' Helper function. Takes the mininum of timestamp as first day and labels oncoming days with the time difference to this day.
#'
#' @param data dataframe containing column named 'userId', which should be the unique identifier for each ID.
#' @param fun character. Must be the name of a function, which has a dataframe as input and a vector (of length nrow(dataframe)) as output.
#' @param colname character. Column name of the new column.
#' @family helper functions
#' @return dataframe with added column
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
addColumnByUserId = function(data, fun, colname) {
  userId = . = NULL
  checkmate::assertVector(do.call(fun, list(data)))
  newCol = data %>% group_by(userId) %>% do(do.call(fun, list(.)) %>% data.frame()) %>% pull(.)
  eval(parse(text = paste0("data$", colname, " = newCol")))
  return(data)
}


