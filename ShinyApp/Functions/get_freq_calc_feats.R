get_freq_calc_feats <- function(){
  # returns
  # df: | Calulated_Features | Freq |
  #     |--------------------------
  
  ls_files_calc <- get_calc_feats()
  if(is_empty(ls_files_calc)) return(NULL) # If no calculated features -> return(NULL)
  
  ls_files_calc <- substring(ls_files_calc, regexpr("_", ls_files_calc) + 1) # remove userId
  ls_features_calc <- str_extract(ls_files_calc, '^[^.]+') # remove '.csv'
  
  features_calc_freq <-  table(ls_features_calc) %>% as.data.frame()
  names(features_calc_freq) <- c("Calulated_Features", "Count")
  return(features_calc_freq)
  
}