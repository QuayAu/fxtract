#' Helper function. Takes the mininum of timestamp as first day and labels oncoming days with the time difference to this day.
#'
#' @param data dataframe containing column named 'userId', which should be the unique identifier for each ID.
#' @param fun function, which has a dataframe as input and a vector (of length nrow(dataframe)) as output.
#' @family helper functions
#' @return dataframe with added column
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export
addColumnByUserId = function(data, fun) {

}


