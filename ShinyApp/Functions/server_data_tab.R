# UI =================================================================================

dt_proxy <- DT::dataTableProxy("dt")

# Table -------------------------------------------------------------------
output$dt <- DT::renderDataTable({
  
  df <- getAllFeatsPerUser()

  selFeatCats = input$cbFeatureCat
  df = df %>% filter(feature_category %in% selFeatCats)

  df <- df[-1] %>% spread(feature_name, file_exists)
  
  if(nrow(df) == 0) return(NULL) #All feature categories filtered
  
  if (ncol(df) > 2) percentComplPerUser = rowSums(df[,2:ncol(df)]) / (ncol(df) - 1)
  else percentComplPerUser = df[,2] / ncol(df)
  
  df = add_column(df, Completed = percentComplPerUser, .after = 1)
  
  rownames(df) <- df[,1]
  df <- df[,-1] %>% as.data.frame()
  data_dt <<- df

  DT::datatable(df, rownames=TRUE, filter = "top",
    options = list( 
      scrollX = TRUE
    )
  ) %>% formatStyle(1:ncol(df),
    backgroundColor = styleEqual(c(1), c('#b5f6aa'))
  )
  
  
})

# All/None Checkbox -------------------------------------------------------------------


observeEvent(input$dt_sel, {
  if (isTRUE(input$dt_sel)) {
    DT::selectRows(dt_proxy, input$dt_rows_all)
  } else {
    DT::selectRows(dt_proxy, NULL)
  }
})

# Observe selected users -------------------------------------------------------------------
observeEvent(input$dt_rows_selected, {
  rv$selected_users <- rownames(data_dt[input$dt_rows_selected,])
})






