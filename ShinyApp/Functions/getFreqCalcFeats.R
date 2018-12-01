getFreqCalcFeats <- function(){
  # returns a data frame with the calculated features (name) and the corresponding frequency
  # df: | Calulated_Features | Count |
  #     |--------------------------
  
  lsFilesCalc <- getCalcFeats()
  
  if(is_empty(lsFilesCalc) | is_empty(rv$inpSelFeature)) return(NULL) # If no calculated features or no users selected

  # Generate table userID | feature
  userIds = str_extract(lsFilesCalc, '^[^_]+')
  feats = substring(lsFilesCalc, regexpr("_", lsFilesCalc) + 1) # remove userId
  feats = str_extract(feats, '^[^.]+') # remove '.csv'
  tUserFeat = data.frame(userIds, feats)
  
  tUserFeat = tUserFeat %>% filter(userIds %in% rv$selectedUsers)
  featsCalcFreq <-  table(tUserFeat$feats) %>% as.data.frame()
  names(featsCalcFreq) <- c("Calulated_Features", "Count")
  featsCalcFreq = featsCalcFreq %>% filter(Count > 0)

  return(featsCalcFreq)
  
}