getFeatNotCalc <- function(){
  # This functions returns a df containing all not calculated features 
  # Structure: feature name | n(selected users)

  allFeats = getAllFeats(rv$inpSelFeature)$feature 
  calcFeats <- getFreqCalcFeats()$Calulated_Features
  notCalcFeats <- allFeats[!(allFeats %in% calcFeats)]
  
  if(length(notCalcFeats) == 0) return(NULL)

  notCalcFeatsUsers = data.frame(notCalcFeats, length(rv$selectedUsers))
  names(notCalcFeatsUsers) = c("Feature", "RemainingUsers")
  
  return(notCalcFeatsUsers)
  
}