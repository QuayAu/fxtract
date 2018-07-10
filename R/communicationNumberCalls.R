#' Calculates the total number of calls.
#'
#' @template param_ids
#' @template param_loggingdata
#' @template param_call_type
#' @template param_colname
#' @template param_progressbar
#' @family communication functions
#' @return Returns a dataframe with the total number of calls grouped by the participant IDs.
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @export
#' @examples
#' communicationNumberCalls(phonedata, call_type = "INCOMING", colname = "number_inc_calls")
communicationNumberCalls = function(loggingdata, ids = unique(loggingdata$userId), call_type = "all", colname = "NumberCalls",
  progress_bar = FALSE) {
  #checks
  checkInputs(ids, loggingdata)
  checkmate::assertCharacter(call_type)
  checkmate::assertLogical(progress_bar)
  cols = c("source", "userId", "phone.event")
  checkCols(cols, loggingdata)

  # check if call_type variable makes sense
  if (!all(call_type %in% c("all", "OUTGOING", "INCOMING", "RINGING", "MISSED", "ONHOLD", "REJECTED")))
    stop("call_type can only be 'all', 'OUTGOING', 'INCOMING', 'RINGING', 'MISSED', 'ONHOLD', or 'REJECTED'")

  if (any(call_type == "all"))
    call_type = c("OUTGOING", "INCOMING", "RINGING", "MISSED", "ONHOLD", "REJECTED")

  df1 = dplyr::filter(loggingdata, source == "PHONE", userId %in% ids, phone.event %in% call_type)[, cols]

  res = data.frame(userId = ids)
  res$n = NA

  if (progress_bar) pb = txtProgressBar(min = 0, max = length(ids), style = 3) #progress bar

  for (id in ids) {
    res[which(res$userId == id), "n"] = df1 %>% dplyr::filter(userId == id) %>% nrow()
    if (progress_bar) setTxtProgressBar(pb, which(id == ids))
  }

  colnames(res)[2] = colname
  return(res)
}
