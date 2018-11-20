getAllFeatsPerUser <- reactive({
# returns df with features per user
# structure:feature_category | user_id | feature_name | file_exists
  
  df_feats_cat <- get_all_features() # Remove .R
  
  # print("calc_feat")
  # print(calc_feat)
  
  # feats_cat <- vapply(strsplit(calc_feat,"/"), `[`, 1, FUN.VALUE=character(1))
  # feats <- vapply(strsplit(calc_feat,"/"), `[`, 2, FUN.VALUE=character(1))
  # df_feats_cat <- cbind(feats_cat, feats)
  
  df <- NULL
  
  
  for (id in ids){
    for (row in 1:nrow(df_feats_cat)){
      feat_cat = df_feats_cat[row, 1]
      feat = df_feats_cat[row, 2]
      
      file_name <- paste0(feat_cat, "/", id, "_", feat, ".csv")
      
      file_path <- paste0("Projects/", input$sProjectName, "/csv_exports/", feat_cat, "/", id, "_", feat, ".csv")
      file_exists <- file.exists(file_path)
      df_temp <- data.frame(file_name, file_exists)
      df <- rbind(df, df_temp)
      df
    }
  } 
  
  df$file_name <- str_extract(df$file_name, '^[^.]+') # Remove .R
  
  print("head(df)")
  print(head(df))
  
  df <- df %>% separate(file_name, sep = "/", into = c("feature_category", "user_feature_name"))
  df <- df %>% separate(user_feature_name, c("user_id", "feature_name"), "_", extra = "merge")
  


  df

  
})