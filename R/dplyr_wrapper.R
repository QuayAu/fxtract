#' Wrapper for dplyr's summarize
#'
#' This function wraps dplyr's \code{summarize()} function in a convenient way. The user only needs to define functions on the dataset with a named vector or list (with atomic entries of length 1) as return.
#'
#' @param data (`dataframe`). A dataframe with a grouping variable.
#' @param group_by (`character()`). Name of column, which contains identifiers on which the dataset should be grouped by. E.g. different user IDs.
#' @param fun (`function`). Must be a function, which has a dataframe as input and a (named) vector of desired length as output.
#' @param check_fun (`logical(1)`). If \code{TRUE}, fun(data) will be evaluated and checked if the outcome is of correct form. Set to \code{FALSE}
#' if evaluation on the whole dataset takes too long.
#' @return (`dataframe`)
#' @importFrom dplyr group_by do
#' @importFrom magrittr "%>%"
#' @importFrom data.table dcast setDT
#' @importFrom stats as.formula setNames
#' @export
#' @examples
#' # Number of used chrome apps
#' fun1 = function(data) {
#'   c(uses_chrome = nrow(
#'     dplyr::filter(data, RUNNING_TASKS_baseActivity_mPackage == "com.android.chrome"))
#'   )
#' }
#' dplyr_wrapper(data = studentlife_small, group_by = "userId", fun = fun1)
#'
#' # mean, max, sd of a column
#' fun2 = function(data) {
#'   c(mean_sepal_length = mean(data$Sepal.Length),
#'     max_sepal_length = max(data$Sepal.Length),
#'     sd_sepal_length = sd(data$Sepal.Length)
#'   )
#' }
#' dplyr_wrapper(data = iris, group_by = "Species", fun = fun2)
#'
#' # return list
#' fun3 = function(data) {
#'   list(mean_sepal_length = mean(data$Sepal.Length),
#'     max_sepal_length = max(data$Sepal.Length),
#'     sd_sepal_length = sd(data$Sepal.Length)
#'   )
#' }
#' dplyr_wrapper(data = iris, group_by = "Species", fun = fun3)
#'
#' # group by two columns
#' df = data.frame(id = c(rep(1, 10), rep(2, 10)))
#' df$task = rep(c(rep("task1", 5), rep("task2", 5)), 2)
#' df$hour = rep(c(rep("hour1", 3), rep("hour2", 2), rep("hour1", 2), rep("hour2", 3)), 2)
#' df$x = 1:20
#' fun4 = function(data) c(mean_x = mean(data$x))
#' dplyr_wrapper(data = df, group_by = c("id", "task"), fun = fun4)
#'
dplyr_wrapper = function(data, group_by, fun, check_fun = TRUE) {
  . = NULL
  checkmate::assertDataFrame(data)
  checkmate::assertNames(names(data), must.include = group_by)
  checkmate::assertLogical(check_fun)
  if (check_fun) {
    check_data = fun(data)
    if (is.null(check_data)) stop("fun(data) returns NULL. Please check your feature function.")
    if (!is.atomic(check_data)) {
      if (!inherits(check_data, "list")) stop("Your function must return a named vector or named list with atomic entries with 1 value each.")
    }

    #check vector return
    if (is.atomic(check_data)) {
      if (is.null(names(check_data))) stop("Your function returns an unnamed vector. Please give each entry a name.")
      checkmate::assert_atomic_vector(check_data)
    }

    #check list return
    if (inherits(check_data, "list")) {
      for (i in seq_along(check_data)) {
        if (is.null(names(check_data[i]))) {
          stop(paste0("List entry ", i, " is unnamed. Please name each list entry."))
        } else {
          if (names(check_data[i]) == "") stop(paste0("List entry ", i, " is unnamed. Please name each list entry."))
        }
        checkmate::assert_atomic(check_data[[i]], len = 1L)
      }
    }
  }
  if (length(group_by) == 1) res = data %>% dplyr::group_by(.dots = group_by) %>% dplyr::do(data.frame(t(fun(.))))
  if (length(group_by) >= 2) {
    res = data %>% dplyr::group_by_(.dots = group_by) %>% dplyr::do(data.frame(t(fun(.))))
    dcast_formula = as.formula(paste(group_by[1], "~", paste(group_by[-1], collapse = "+")))
    res = data.table::dcast(data.table::setDT(res), dcast_formula, value.var = setdiff(colnames(res), group_by))
  }

  res = data.frame(res)

  list_entries = lapply(res, class)
  for (i in seq_along(list_entries)) {
    if (list_entries[i] == "list") res[i] = unlist(res[i])
  }

  return(res)
}
