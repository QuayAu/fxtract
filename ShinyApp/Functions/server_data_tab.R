# UI =================================================================================
# Tabelle -------------------------------------------------------------------

output$dt <- DT::renderDataTable({
  
  calc_feat <- get_all_features() # Remove .R
  
  feats_cat <- vapply(strsplit(calc_feat,"/"), `[`, 1, FUN.VALUE=character(1))
  feats <- vapply(strsplit(calc_feat,"/"), `[`, 2, FUN.VALUE=character(1))
  
  df <- NULL
  for (id in ids){
    file_name <- paste0(feats_cat, "/", id, "_", feats, ".csv")
    file_path <- paste0("Projects/", input$sProjectName, "/", feats_cat, "/", id, "_", feats, ".csv")
    file_exists <- file.exists(file_path)
    df_temp <- data.frame(file_name, file_exists)
    df <- rbind(df, df_temp)
    df
  } 
  
  df$file_name <- str_extract(df$file_name, '^[^.]+') # Remove .R
  
  df <- df %>% separate(file_name, sep = "/", into = c("feature_category", "user_feature_name"))
  df <- df %>% separate(user_feature_name, c("user_id", "feature_name"), "_", extra = "merge")
  
  df_lookup_feat_cat <<- unique(df[,c(1,3)])
  print(df_lookup_feat_cat)
  df <- df[-1] %>% spread(feature_name, file_exists)

  DT::datatable(df, rownames=FALSE,extensions = c('FixedColumns',"FixedHeader"),
    options = list(dom = 't', 
      scrollX = TRUE, 
      fixedColumns = list(leftColumns = 1, rightColumns = 0))
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