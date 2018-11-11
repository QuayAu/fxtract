# UI =================================================================================
# Tabelle -------------------------------------------------------------------

output$dt <- DT::renderDataTable({
  
  calc_feat <- get_all_features() # Remove .R
  
  feats_cat <- vapply(strsplit(calc_feat,"/"), `[`, 1, FUN.VALUE=character(1))
  feats <- vapply(strsplit(calc_feat,"/"), `[`, 2, FUN.VALUE=character(1))
  df_feats_cat <- cbind(feats_cat, feats)
  
  df <- NULL

  for (id in ids){
    for (row in 1:nrow(df_feats_cat)){
      feat_cat = df_feats_cat[row, 1]
      feat = df_feats_cat[row, 2]

      file_name <- paste0(feat_cat, "/", id, "_", feat, ".csv")

      file_path <- paste0("Projects/", input$sProjectName, "/", feat_cat, "/", id, "_", feat, ".csv")
      file_exists <- file.exists(file_path)
      df_temp <- data.frame(file_name, file_exists)
      df <- rbind(df, df_temp)
      df
    }
  } 
  
  
  df$file_name <- str_extract(df$file_name, '^[^.]+') # Remove .R
  
  df <- df %>% separate(file_name, sep = "/", into = c("feature_category", "user_feature_name"))
  df <- df %>% separate(user_feature_name, c("user_id", "feature_name"), "_", extra = "merge")

  
  selFeatCats = input$cbFeatureCat
  df = df %>% filter(feature_category %in% selFeatCats)
  df <- df[-1] %>% spread(feature_name, file_exists)
  
  rownames(df) <- df[,1]
  df <- df[,-1]

  DT::datatable(df, rownames=TRUE,
    options = list( 
      scrollX = TRUE
    )
  ) %>% formatStyle(1:ncol(df),
    backgroundColor = styleEqual(c(1), c('#b5f6aa'))
  )
  
  
})

# All/None Checkbox -------------------------------------------------------------------
dt_proxy <- DT::dataTableProxy("dt")

observeEvent(input$dt_sel, {
  if (isTRUE(input$dt_sel)) {
    DT::selectRows(dt_proxy, input$dt_rows_all)
  } else {
    DT::selectRows(dt_proxy, NULL)
  }
})