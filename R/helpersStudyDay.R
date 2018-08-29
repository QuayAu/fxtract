#' Helper function. Takes the mininum of timestamp as first day and labels oncoming days with the time difference to this day.
#'
#' @template param_data_ts
#' @family helper functions
#' @return character. Character variable where 'day1' indicates the first day and the following days are saved as 'dayx', where x is the difference to 'day1'.
#' @importFrom dplyr slice pull
#' @importFrom magrittr "%>%"
calcStudyDay = function(data) {
  checkmate::assertDataFrame(data)
  if (!"date" %in% colnames(data)) stop("data needs a column named 'date'. Consider adding 'date' by using addDate().")
  timestamp = date = NULL

  alldays = data$date
  daysId = character(length(alldays))
  uniquedays = unique(alldays)
  day1 = data %>% slice(which.min(timestamp)) %>% pull(date)

  daysId[which(alldays == day1)] = "day1"

  if (length(uniquedays) > 1) {
    j = which(uniquedays == day1)
    diffToDay1 = difftime(uniquedays[-j], day1, units = "days")
    for (i in 1:length(diffToDay1)) {
      if (diffToDay1[i] > 0) daysId[which(alldays == uniquedays[-j][i])] = paste0("day", diffToDay1[i] + 1)
    }
  }
  daysId
}

#' Helper function. For each group variable it takes the mininum of timestamp as first day and labels oncoming days with the time difference to this day.
#'
#' @template param_data
#' @template param_group_col
#' @template param_colname
#' @param ordered logical. If \code{TRUE}, the added character variable will be ordered, like 'day1' < 'day2' < 'day3' < ... .
#' @family helper functions
#' @return dataframe with added factor variable 'studyDay' for each group variable. The first day for each variable will be 'day1' .
#' @importFrom dplyr slice pull
#' @importFrom magrittr "%>%"
#' @export
addStudyDayByGroup = function(data, group_col, colname = "studyDay", ordered = TRUE) {
  checkmate::assertNames(names(data), must.include = group_col)
  if (colname %in% names(data)) stop("colname is already in dataset. Please choose a different colname!")

  data = addColumnByGroup(data = data, group_col = group_col, fun = calcStudyDay, colname = colname)
  if (ordered) {
    max_number_days = max(as.numeric(gsub("\\D", "", data[, colname])))
    day_levels = paste0("day", 1:max_number_days)
    data[, colname] = factor(data[, colname], levels = day_levels, ordered = TRUE)
  }
  return(data)
}

#' Helper function. It takes the mininum of timestamp as first day and labels oncoming days with the time difference to this day.
#'
#' @template param_data_ts
#' @template param_colname
#' @param ordered logical. If \code{TRUE}, the added factor variable will be ordered, like 'day1' < 'day2' < 'day3' < ... .
#' @family helper functions
#' @return dataframe with added factor variable 'studyDay'. The first day will be 'day1' .
#' @importFrom dplyr slice pull
#' @importFrom magrittr "%>%"
#' @export
addStudyDay = function(data, colname = "studyDay", ordered = TRUE) {
  message("If you have data with different users, be aware, that this function ignores that! See addStudyDayByGroup().")
  if (colname %in% names(data)) stop("colname is already in dataset. Please choose a different colname!")

  data = addColumn(data = data, fun = calcStudyDay, colname = colname)
  if (ordered) {
    max_number_days = max(as.numeric(gsub("\\D", "", data[, colname])))
    day_levels = paste0("day", 1:max_number_days)
    data[, colname] = factor(data[, colname], levels = day_levels, ordered = TRUE)
  }
  return(data)
}
