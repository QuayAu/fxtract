# Render UI for Feature tab ----------------------------------------------------------------------------------------

dtFTabProxy <- DT::dataTableProxy("dtFTab")

# Table -------------------------------------------------------------------
output$dtFTab <- DT::renderDataTable({
  
  if(is_empty(rv$inpSelFeature)) return(NULL) # Move back from features tab to data tab
  df <- getFeatsNotDone()%>% as.data.frame()
  
  if(nrow(df) == 0) return(NULL) #If no features to be shown
  rv$dataDtFTab = df
  
  DT::datatable(df, rownames=TRUE, filter = "top") %>% 
  formatStyle(1:ncol(df),
    backgroundColor = styleEqual(c(1), c('#b5f6aa'))
  )
  
})


# All/None Checkbox ---------------------------------------------------------------------------
observeEvent(input$dtSelAllFTab, {
  if (isTRUE(input$dtSelAllFTab)) {
    DT::selectRows(dtFTabProxy, input$dtFTab_rows_all)
  } else {
    DT::selectRows(dtFTabProxy, NULL)
  }
})

# Number of selected Users -------------------------------------------------------------------
output$nUsers <- renderText({paste("n =", length(rv$selectedUsers))})

# Observe selected features ------------------------------------------------------------------
observe({
  if(selFeats()) rv$selectedFeats = rownames(rv$dataDtFTab[input$dtFTab_rows_selected,])
  else rv$selectedFeats = NULL
})

selFeats <- reactive({!is.null(input$dtFTab_rows_selected)}) 

# Button calc selected -----------------------------------------------------------------------
output$btnCalcSel <- renderUI({
  if (length(rv$selectedFeats) == 0 | length(rv$selectedUsers) == 0) return(NULL)
  actionButton("btnCalcSel", label = "Calculate selected!",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4;")
})

# Observe Button calc all --------------------------------------------------------------------
observeEvent(input$btnCalcSel, {
  
  # Prepare log files
  if (dir.exists("logfiles") == FALSE){
    dir.create("logfiles")
  }
  
})


