get_feats_not_done <- function(){
  
  df_not <- get_feat_not_calc()

  df_remaining <- get_freq_remain_users()
  print("df_remaining")
  print(df_remaining)
  
  df <- c(df_not, df_remaining) #%>% as.data.frame()
  
}