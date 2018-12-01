getFeatsNotDone <- function(){
  # This functions returns a df containing the number of remaining users for each feature
  # Only the users selected in the data tab are counted among the remaining users.
  
  dfNot <- getFeatNotCalc()
  dfRemain <- getFreqRemUsers()
  df <- rbind(dfNot, dfRemain)
  
  return(df)
  
}