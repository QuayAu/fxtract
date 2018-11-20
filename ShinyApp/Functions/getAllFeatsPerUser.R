getAllFeatsPerUser <- reactive({
# returns df with features per user
# structure:feature_category | user_id | feature_name | file_exists
  
  df_feats_cat <- get_all_features() # Remove .R
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
  
  df <- df %>% separate(file_name, sep = "/", into = c("feature_category", "user_feature_name"))
  df <- df %>% separate(user_feature_name, c("user_id", "feature_name"), "_", extra = "merge")

  df
  
})