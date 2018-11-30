# Render UI for Feature tab ----------------------------------------------------------------------------------------

dtFTabProxy = DT::dataTableProxy("dtFTab")

# Table -------------------------------------------------------------------
output$dtFTab <- DT::renderDataTable({
  
  if(is_empty(rv$inpSelFeature)) return(NULL) # Move back from features tab to data tab
  df = getFeatsNotDone()%>% as.data.frame()
  
  if(nrow(df) == 0) return(NULL) #If no features to be shown
  rv$dataDtFTab = df
  
  DT::datatable(df, rownames=TRUE, filter = "top") %>% 
  formatStyle(1:ncol(df),
    backgroundColor = styleEqual(c(1), c('#E3F6CE'))
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

selFeats = reactive({!is.null(input$dtFTab_rows_selected)}) 

# Button calc selected -----------------------------------------------------------------------
output$btnCalcSel <- renderUI({
  if (length(rv$selectedFeats) == 0 | length(rv$selectedUsers) == 0) return(NULL)
  actionButton("btnCalcSel", label = "Calculate selected!",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4;")
})

observeEvent(input$btnCalcSel, {
  
  projectPathName = paste0("Projects/",input$sProjectName)
  
  # Create csvExports-folder if not existing
  if (dir.exists(paste0(projectPathName, "/csvExports")) == FALSE){
    dir.create(paste0(projectPathName, "/csvExports"))
  }
  
  #Prepare log files
  if (dir.exists(paste0(projectPathName, "/logfiles")) == FALSE){
    dir.create(paste0(projectPathName, "/logfiles"))
  }
  
  #logFilePath = paste0(projectPathName, "/logfiles/",format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), "_logfile.txt")
  #logFile = file(logFilePath, open = "wt")
  #sink(logFile, type = "message")
  
  # Determine features to be calculated
  featCat = input$selFeatureFTab
  featNames = rv$dataDtFTab[rv$selectedFeats,1]
  featPaths = paste0(featCat, "/", featNames, ".R") %>% sort()
  
  #pb <- txtProgressBar(min = 0, max = length(rv$selectedUsers), style = 3)
  selUsers = rv$selectedUsers %>% as.vector()
  for (uId in selUsers){

    #Sys.sleep(0.1) # wegen progressbar

    data_id = logsAll %>% filter(userId == uId) %>% as.data.frame()
    msg <- paste0("\n User: ", uId)
    cat(msg)
    
    for (feature in featPaths){
      out <- tryCatch(
        {
          calcExpFeats(feature, data_id, uId, projectPathName)
        },
        error = function(cond){
          message(paste(uId, ":", feature, ":",cond))
          return(NA)
        },
        warning = function(cond){
          message(paste(uId, ":", feature, ":", cond))
          return(NA)
        }
      )
    }
    
  }

    #setTxtProgressBar(pb, i)
  #}
  
  # Close log files
  #sink(type = "message")
  #close(logFile)
  #readLines(logFilePath) # Remove later
  
})


