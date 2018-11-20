# UI =================================================================================

dt_proxy <- DT::dataTableProxy("dt")

# Table -------------------------------------------------------------------
output$dt <- DT::renderDataTable({
  print("render")

  
  df <- getAllFeatsPerUser()
  # print("head(df)")
  # print(head(df))

  selFeatCats = input$cbFeatureCat
  print("selFeatCats")
  print(selFeatCats)
  df = df %>% filter(feature_category %in% selFeatCats)
  print("head(df)2")
  print(head(df))

  df <- df[-1] %>% spread(feature_name, file_exists)
  
  rownames(df) <- df[,1]
  df <- df[,-1]
  data_dt <<- df

  DT::datatable(df, rownames=TRUE,
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

# observe({
#   if (input$id_tabs == "tab_data"){
#     
#     if (isTRUE(input$dt_sel)) {
#       DT::selectRows(dt_proxy, input$dt_rows_all)
#     } 
#     
#   }
# })




