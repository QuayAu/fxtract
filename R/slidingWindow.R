#' FIXME: add documentation
#'
#' @template param_data_uid
#' @template param_fun_1
#' @param steps integer. Number of steps which the sliding window should go back.
#' @param time_in_sec integer. Number of seconds which the sliding window should go back.
#' @template param_colname
#' @family helper functions
#' @return dataframe with added column
#' @importFrom dplyr do
#' @importFrom magrittr "%>%"
#' @export
slidingWindow = function(data, fun, steps, time_in_sec, colname) {
  . = NULL
  checkmate::assertDataFrame(data)
  if (!missing(steps)) checkmate::assertNumber(steps)
  if (!missing(time_in_sec)) checkmate::assertNumber(time_in_sec)
  if (length(do.call(fun, list(data))) != 1) stop("fun must return a vector of length 1")

  if (!xor(missing(steps), missing(time_in_sec)))
    stop("Pass either steps or time_in_sec, but not both!")

  eval(parse(text = paste0("data$", colname, " = NA"))) #add new column

  if (missing(steps)) {
    if (!"date_time" %in% colnames(data)) stop("Your data set needs a column named 'date_time'.
      See function addDateTime().")
    for (i in 2:nrow(data)) {
      time_row = data$date_time[i]
      diff_times = difftime(data$date_time, time_row, units = "s")
      res_i = data %>% dplyr::filter(diff_times <= time_in_sec) %>%
        dplyr::do(do.call(fun, list(.)) %>% data.frame()) %>% pull(.)
      eval(parse(text = paste0("data$", colname, "[i] = res_i")))
      rm(res_i)
    }
  } else {
    for (i in 1:nrow(data)) {
      if (i - steps <= 0) next
      res_i = data[(i - steps):i, ] %>% dplyr::do(do.call(fun, list(.)) %>% data.frame()) %>% pull(.)
      eval(parse(text = paste0("data$", colname, "[i] = res_i")))
      rm(res_i)
    }
  }
  
  return(data)
}
