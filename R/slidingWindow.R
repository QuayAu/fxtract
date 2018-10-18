#' Adds a variable (or a set of variables) to a dataset by a sliding window approach.
#'
#' @template param_data
#' @param fun function. A function that has `data`` as input and a dataframe with 1 row as output.
#' @template param_utc_col
#' @param steps integer. Number of steps which the sliding window should go back.
#'   Use this for data with fixed sample rate.
#' @param time_in_sec integer. Number of seconds which the sliding window should go back.
#'   Use this for data with variable sample rate.
#' @template param_check_fun
#' @param eval_at_rows integer. Integer vector at which rows the function should be executed. Defaults to `1:nrow(data)`.
#' @template param_unit
#' @family helper functions
#' @return dataframe with added column(s).
#' @importFrom dplyr do bind_rows
#' @importFrom magrittr "%>%"
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @examples
#' fun = function(x) data.frame(mean.accuracy.last30 = mean(x$accuracy, na.rm = TRUE),
#'   max.accuracy.last30 = max(x$accuracy, na.rm = TRUE))
#' data = studentlife.small[1:30, ] #needs date_time variable
#' slidingWindow(data, fun = fun, time_in_sec = 60 * 60,
#'   utc_col = "timestamp") #mean accuracy of last hour
slidingWindow = function(data, utc_col = character(1), fun, steps, time_in_sec, check_fun = TRUE, eval_at_rows = numeric(0), unit = "s") {
  . = NULL
  checkmate::assertDataFrame(data)
  checkmate::assertLogical(check_fun)
  checkmate::assertIntegerish(eval_at_rows)
  checkmate::assertSubset(eval_at_rows, 1:nrow(data))
  checkmate::assertCharacter(utc_col, len = 1)

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
    if (nchar(utc_col) == 0) stop("please specify timestamp column")
    checkmate::assertSubset(utc_col, names(data))
    checkmate::assertNumeric(data[[utc_col]])
    checkmate::assertSubset(unit, c("s", "ms"))
    conv = ifelse(unit == "s", 1, 1000)

    pb = txtProgressBar(min = 0, max = length(eval_at_rows), style = 3)
    for (i in setdiff(eval_at_rows, 1)) {
      time_row = data[[utc_col]][i]
      diff_times = (data[[utc_col]] - time_row) / conv
      res_i = data %>% dplyr::filter(diff_times < 0 & abs(diff_times) < time_in_sec) %>%
        dplyr::do(do.call(fun, list(.)) %>% data.frame())
      res_i = data.frame(rn = i, res_i)
      if (i == setdiff(eval_at_rows, 1)[1]) {
        res = res_i
      } else {
        res = rbind(res, res_i)
      }
      rm(res_i)
      setTxtProgressBar(pb, i)
    }
  } else {
    pb = txtProgressBar(min = 0, max = length(eval_at_rows), style = 3)
    for (i in eval_at_rows) {
      if (i - steps <= 0) next
      if (!exists("res", inherits = FALSE)) {
        res = data[(i - steps):(i - 1), ] %>% dplyr::do(do.call(fun, list(.)) %>% data.frame())
        res = data.frame(rn = i, res)
      } else {
        res_i = data[(i - steps):(i - 1), ] %>% dplyr::do(do.call(fun, list(.)) %>% data.frame())
        res_i = data.frame(rn = i, res_i)
        res = dplyr::bind_rows(res, res_i)
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
