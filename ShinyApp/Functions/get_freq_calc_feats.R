get_freq_calc_feats <- function(){
  # returns
  # df: | Calulated_Features | Freq |
  #     |--------------------------
  
  ls_files_calc <- get_calc_feats()

  if(is_empty(ls_files_calc) | is_empty(input$selFeature)) return(NULL) # If no calculated features or no users selected -> return(NULL)

  # Generate table userID | feature
  userIds = str_extract(ls_files_calc, '^[^_]+')
  feats = substring(ls_files_calc, regexpr("_", ls_files_calc) + 1) # remove userId
  feats = str_extract(feats, '^[^.]+') # remove '.csv'
  tUserFeat = data.frame(userIds, feats)
  
  tUserFeat = tUserFeat %>% filter(userIds %in% rv$selected_users)
  features_calc_freq <-  table(tUserFeat$feats) %>% as.data.frame()
  names(features_calc_freq) <- c("Calulated_Features", "Count")
  features_calc_freq = features_calc_freq %>% filter(Count > 0)

  return(features_calc_freq)
  
}