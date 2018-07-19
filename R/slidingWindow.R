#' FIXME: add documentation
#'
#' @param data dataframe containing column named 'userId', which should be the unique identifier for each ID.
#' @template param_fun_1
#' @family helper functions
#' @return dataframe with added column
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
# slidingWindow = function(data, fun, steps, time_in_seconds) {
#   checkmate::assertDataFrame(data)
#   if (length(do.call(fun, list(data))) != 1) stop("fun must return a vector of length 1")
#   
#   
#   # userId = . = NULL
#   # newCol = data %>% group_by(userId) %>% do(do.call(fun, list(.)) %>% data.frame()) %>% pull(.)
#   # eval(parse(text = paste0("data$", colname, " = newCol")))
#   # return(data)
# }
