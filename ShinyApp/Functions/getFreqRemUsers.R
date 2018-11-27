getFreqRemUsers <- function(){
  # This function returns the frequency of users not yet calculated for features for which there is at least one calculated user
  
  freqCalcFeats = getFreqCalcFeats() # Redundant -> Change later

  if(is_empty(freqCalcFeats)) return(NULL)
  nSelUsers = length(rv$selectedUsers)
  freqCalcFeatsNotDone = freqCalcFeats %>% filter(Count != nSelUsers) # N of calculated users per feature not done (but with at least one user calculated)

  if(nrow(freqCalcFeatsNotDone) == 0) return(NULL)
  
  freqCalcFeatsNotDone$nRemUsers = nSelUsers - freqCalcFeatsNotDone$Count
  freqCalcFeatsNotDone = freqCalcFeatsNotDone %>% select(-Count)
  names(freqCalcFeatsNotDone) = c("Feature", "RemainingUsers")
  
  return(freqCalcFeatsNotDone)
  
}