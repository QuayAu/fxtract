#' Adds a variable (or a set of variables) to a dataset by a sliding window approach.
#'
#' @template param_data
#' @param fun function. A function that has `data`` as input and a dataframe with 1 row as output.
#' @param steps integer. Number of steps which the sliding window should go back.
#'   Use this for data with fixed sample rate.
#' @param time_in_sec integer. Number of seconds which the sliding window should go back.
#'   Use this for data with variable sample rate.
#' @template param_check_fun
#' @param eval_at_rows integer. Inte
#' @family helper functions
#' @return dataframe with added column(s).
#' @importFrom dplyr do bind_rows
#' @importFrom magrittr "%>%"
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @examples
#' fun = function(x) data.frame(mean.accuracy.last30 = mean(x$accuracy, na.rm = TRUE),
#'   max.accuracy.last30 = max(x$accuracy, na.rm = TRUE))
#' data = addDateTime(studentlife.small[1:30, ]) #needs date_time variable
#' slidingWindow(data, fun = fun, time_in_sec = 60 * 60) #mean accuracy of last hour
slidingWindow = function(data, fun, steps, time_in_sec, check_fun = TRUE, eval_at_rows = numeric(0)) {
  . = NULL
  checkmate::assertDataFrame(data)
  checkmate::assertLogical(check_fun)
  checkmate::assertInteger(eval_at_rows)
  checkmate::assertSubset(eval_at_rows, 1:nrow(data))

  if (!missing(steps)) checkmate::assertNumber(steps)
  if (!missing(time_in_sec)) checkmate::assertNumber(time_in_sec)

  if (check_fun) {
    fd = do.call(fun, list(data))
    if (nrow(fd) != 1) stop("fun must have a dataframe with 1 row as output!")
    if (length(intersect(names(fd), names(data))) > 0) stop("calculated column has a column name already present in data!")
  }

  if (!xor(missing(steps), missing(time_in_sec)))
    stop("Pass either steps or time_in_sec, but not both!")

  if (length(eval_at_rows) == 0) eval_at_rows = 1:nrow(data)

  if (missing(steps)) {
    if (!"date_time" %in% colnames(data)) stop("Your data set needs a column named 'date_time'.
      See function addDateTime().")
    for (i in setdiff(eval_at_rows, 1)) {
      time_row = data$date_time[i]
      diff_times = difftime(data$date_time, time_row, units = "s")
      res_i = data %>% dplyr::filter(diff_times < 0 & abs(diff_times) < time_in_sec) %>%
        dplyr::do(do.call(fun, list(.)) %>% data.frame())
      res_i = data.frame(rn = i, res_i)
      if (i == setdiff(eval_at_rows, 1)[1]) {
        res = res_i
      } else {
        res = rbind(res, res_i)
      }
      rm(res_i)
    }
  } else {
    pb = txtProgressBar(min = 0, max = length(eval_at_rows), style = 3)
    for (i in eval_at_rows) {
      if (i - steps <= 0) next
      if (i - steps == 1 | i == eval_at_rows[1]) {
        res = data[(i - steps):(i - 1), ] %>% dplyr::do(do.call(fun, list(.)) %>% data.frame())
        res = data.frame(rn = i, res)
      } else {
        res_i = data[(i - steps):(i - 1), ] %>% dplyr::do(do.call(fun, list(.)) %>% data.frame())
        res_i = data.frame(rn = i, res_i)
        res = rbind(res, res_i)
        rm(res_i)
      }
      setTxtProgressBar(pb, i)
    }
  }
  res2 = data.frame(rn = 1:nrow(data))
  res2 = res2 %>% dplyr::left_join(res, by = "rn")
  res2$rn = NULL

  if (nrow(res2) != nrow(data)) stop("something went wrong")
  data = data.frame(data, res2)
  return(data)
}
