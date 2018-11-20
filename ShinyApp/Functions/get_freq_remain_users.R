get_freq_remain_users <- function(){
  
  calc_feat <- get_freq_calc_feats()

  if(is_empty(calc_feat)) return(NULL)
  nSelectedUsers = length(rv$selected_users)
  calc_feat_missing_users <- calc_feat %>% filter(Count != nSelectedUsers)
  if(nrow(calc_feat_missing_users) == 0) return(NULL)
  
  calc_feat_missing_users$Num_remaining_users <- nSelectedUsers - calc_feat_missing_users$Count
    
  calc_feat_missing_users <- paste0(calc_feat_missing_users$Calulated_Features, " (Remaining users: ", nSelectedUsers - calc_feat_missing_users$Count, ")")
  
}