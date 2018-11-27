# Render UI for Data tab ----------------------------------------------------------------------------------------

dtProxy <- DT::dataTableProxy("dt")

# Table -------------------------------------------------------------------
output$dt <- DT::renderDataTable({

  selUsers = isolate(rv$selectedUsers)
  df <- getAllFeatsPerUser()

  if(is_empty(df)) return(NULL)
  selFeatCats = input$cbFeatureCat # Selected Feature Categories in drop down
  df = df %>% filter(featCat %in% selFeatCats)

  df <- df[-1] %>% spread(featName, fileExists)
  
  if(nrow(df) == 0) return(NULL) #All feature categories filtered
  
  if (ncol(df) > 2) percentComplPerUser = rowSums(df[,2:ncol(df)]) / (ncol(df) - 1)
  else percentComplPerUser = df[,2] / ncol(df)
  
  df = add_column(df, Completed = percentComplPerUser, .after = 1)
  rownames(df) <- df[,1]
  df <- df[,-1] %>% as.data.frame()
  dataDT <<- df

  dt = DT::datatable(df, rownames=TRUE, filter = "top", selection = list(selected = selUsers)) %>% formatStyle(1:ncol(df),
    backgroundColor = styleEqual(c(1), c('#b5f6aa'))
  )
  
  
})

# All/None Checkbox -------------------------------------------------------------------
observeEvent(input$dtSelAll, {
  if (isTRUE(input$dtSelAll)) {
    DT::selectRows(dtProxy, input$dt_rows_all)
    rv$selectedUsers = input$dt_rows_all
  } else {
    DT::selectRows(dtProxy, NULL)
    rv$selectedUsers = NULL
  }
})

# Observe selected users -------------------------------------------------------------------
observeEvent(input$dt_rows_selected, {
  rv$selectedUsers <- rownames(dataDT[input$dt_rows_selected,])
})






