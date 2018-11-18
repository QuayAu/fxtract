get_feat_not_calc <- function(){
  
  feats = get_all_features(input$selFeature)
  
  calc_feat <- get_freq_calc_feats()$Calulated_Features
  print("calc_feat")
  print(calc_feat)

  not_calc_feat <- feats[!(feats %in% calc_feat)]

  if(length(not_calc_feat) == 0) return(NULL)
  str_not_cal_feat <- paste(not_calc_feat, "(Remaining users: 'All')")
  str_not_cal_feat
  
}