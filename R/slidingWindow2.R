#' Adds a variable to a dataset by a sliding window approach.
#'
#' @template param_data
#' @param fun function. A function that has data as sole input and a dataframe with 1 row as output.
#' @param steps integer. Number of steps which the sliding window should go back.
#'   Use this for data with fixed sample rate.
#' @param time_in_sec integer. Number of seconds which the sliding window should go back.
#'   Use this for data with variable sample rate.
#' @template param_check_fun
#' @family helper functions
#' @return dataframe with added column
#' @importFrom dplyr do bind_rows
#' @importFrom magrittr "%>%"
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @examples
#' fun = function(x) data.frame(mean.accuracy.last30 = mean(x$accuracy, na.rm = TRUE), 
#'   max.accuracy.last30 = max(x$accuracy, na.rm = TRUE))
#' data = addDateTime(studentlife.small[1:30, ]) #needs date_time variable
#' slidingWindow2(data, fun = fun, time_in_sec = 60 * 60) #mean accuracy of last hour
slidingWindow2 = function(data, fun, steps, time_in_sec, check_fun = TRUE) {
  . = NULL
  checkmate::assertDataFrame(data)
  checkmate::assertLogical(check_fun)
  
  if (!missing(steps)) checkmate::assertNumber(steps)
  if (!missing(time_in_sec)) checkmate::assertNumber(time_in_sec)
  
  if (check_fun) {
    fd = do.call(fun, list(data))
    if (nrow(fd) != 1) stop("fun must have a dataframe with 1 row as output!")
    if (length(intersect(names(fd), names(data))) > 0) stop("calculated column has a column name already present in data!")
  }
  
  if (!xor(missing(steps), missing(time_in_sec)))
    stop("Pass either steps or time_in_sec, but not both!")

  if (missing(steps)) {
    if (!"date_time" %in% colnames(data)) stop("Your data set needs a column named 'date_time'.
      See function addDateTime().")
    for (i in 2:nrow(data)) {
      time_row = data$date_time[i]
      diff_times = difftime(data$date_time, time_row, units = "s")
      res_i = data %>% dplyr::filter(diff_times < 0 & abs(diff_times) < time_in_sec) %>%
        dplyr::do(do.call(fun, list(.)) %>% data.frame())
      if (i == 2) {
        res = res_i
      } else {
        res = rbind(res, res_i)
      }
      rm(res_i)
    }
    res = dplyr::bind_rows(data.frame(NA), res)[, names(res), drop = FALSE]
  } else {
    pb = txtProgressBar(min = 0, max = nrow(data), style = 3)
    for (i in 1:nrow(data)) {
      if (i - steps <= 0) next
      if (i - steps == 1) res = data[(i - steps):(i - 1), ] %>% dplyr::do(do.call(fun, list(.)) %>% data.frame())
      res_i = data[(i - steps):(i - 1), ] %>% dplyr::do(do.call(fun, list(.)) %>% data.frame())
      res = rbind(res, res_i)
      rm(res_i)
      setTxtProgressBar(pb, i)
    }
    res = dplyr::bind_rows(data.frame(rep(NA, steps - 1)), res)[, names(res), drop = FALSE]
  }
  
  if (nrow(res) != nrow(data)) stop("something went wrong")
  data = data.frame(data, res)
  return(data)
}
