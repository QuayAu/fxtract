get_feats_not_done <- function(){
  
  df_not <- get_feat_not_calc()
  df_remaining <- get_freq_remain_users()
  df <- c(df_not, df_remaining)
  
}