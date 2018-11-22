getFreqRemUsers <- function(){
  # This function returns the frequency of users not yet calculated for features for which there is at least one calculated user
  
  freqCalcFeats <- getFreqCalcFeats() # Redundant -> Change later

  if(is_empty(freqCalcFeats)) return(NULL)
  nSelectedUsers = length(rv$selectedUsers)
  freqCalcFeatsNotDone <- freqCalcFeats %>% filter(Count != nSelectedUsers) # N of calculated users per feature not done (but with at least one user calculated)
  if(nrow(freqCalcFeatsNotDone) == 0) return(NULL)
  
  freqCalcFeatsNotDone$nRemUsers <- nSelectedUsers - freqCalcFeatsNotDone$Count
    
  freqCalcFeatsNotDone <- paste0(freqCalcFeatsNotDone$Calulated_Features, " (Remaining users: ", freqCalcFeatsNotDone$nRemUsers, ")")
  
}