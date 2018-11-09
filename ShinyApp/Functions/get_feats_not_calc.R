get_feat_not_calc <- function(){
  
  all_feat <- get_all_features(type = rv$cur_feature_type)
  calc_feat <- get_freq_calc_feats()$Calulated_Features
  not_calc_feat <- all_feat[!(all_feat %in% calc_feat)]
  str_not_cal_feat <- paste(not_calc_feat, "(Remaining users: 'All')")
  
  
}