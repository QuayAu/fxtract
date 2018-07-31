#' Calculates a given function on the dataset for each studyDay and grouped by the column 'userId'.
#' Needs a function, which has a dataframe as input and a singular value as output.
#' A summary function, which summarizes the results for each day, will be evaluated for each 'userId'.
#'
#' @template param_data
#' @template param_group_col
#' @template param_fun_n
#' @template param_colname
#' @template param_summary_fun
#' @template param_interval_steps
#' @template param_interval_time_in_sec
#' @template param_unit
#' @param export_results_per_interval logical. If TRUE, an additional dataframe, where \code{fun} is evaluated at each interval, is returned.
#' @template param_check_fun
#' @family feature functions
#' @return dataframe with columns userId and \code{colname}. One value for each userId.
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' data = studentlife.small %>% filter(!is.na(altitude))
#' fun = function(data) mean(data$altitude, na.rm = TRUE) 
#' summary_fun = function(vector) ar(vector[!is.na(vector)], order.max = 1)$ar 
#' 
# calcFeatureperIntervalAndByGroup = function(data, group_col, fun, colname, summary_fun, steps, time_in_sec, unit,
#   export_results_per_interval = FALSE, check_fun = TRUE) {
#   # studyDay = min_day =  max_day = NULL 
#   if (missing(steps)) checkmate::assert_numeric(time_in_sec)
#   if (missing(time_in_sec)) checkmate::assert_integerish(steps)
#   
#   
#   if (check_fun) {
#     if (length(do.call(fun, list(data))) != 1) stop("fun must return a vector of length 1")
#   }
#   data = addColumnByGroup(data, group_col = group_col, fun = function(data) divideDataIntoIntervals(data, steps = steps, 
#     time_in_sec = time_in_sec, unit = unit), colname = "interval_helper", check_fun = check_fun)
#   
#   res.df = data %>% group_by_(group_col, "interval_helper") %>% dplyr::do(do.call(fun, list(.)) %>% data.frame())
#   
#   #TODO: interval factor levels 
#   #return right stuff.....
# }
