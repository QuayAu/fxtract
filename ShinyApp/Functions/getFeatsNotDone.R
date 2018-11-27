getFeatsNotDone <- function(){
  # This functions returns a df with the features with no calculated user and features for which not all users were calculated
  
  dfNot <- getFeatNotCalc()
  dfRemain <- getFreqRemUsers()
  df <- rbind(dfNot, dfRemain)
  df
  
}