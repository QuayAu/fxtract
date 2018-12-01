getFeatDone <- function(){
  # This functions returns a df containing all calculated features 
  # Structure: feature name | remaining Users = 0
  
  calcFeats = getFreqCalcFeats() 

  if(is_empty(calcFeats)) return(NULL)
  calcFeats = calcFeats %>% filter(Count == length(rv$selectedUsers))
  if(length(calcFeats[,1]) == 0) return(NULL)
  dfDone = data.frame("Feature" = calcFeats[,1], "RemainingUsers" = 0)
  
}