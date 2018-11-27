getCalcFeats <- reactivePoll(1000, session,
# This reactive poll returns the name of the csv-files that are already saved in the 'csv_exports' in the active project
                                        
  # Check wether calculated features of current feature category have changed                                    
  checkFunc = function(){
    
    if(is_empty(input$idTabs)) return(NULL)
    if (input$idTabs == "tabData") csvExportPath = paste0("Projects/", input$sProjectName, "/csv_exports/", input$selFeature)
    else csvExportPath = paste0("Projects/", input$sProjectName, "/csv_exports/", input$selFeatureFTab)

    if(dir.exists(csvExportPath)) list.files(csvExportPath)
    
  },
  
  # If changed -> reload calculated features                             
  valueFunc = function(){

    if (input$idTabs == "tabData") csvExportPath = paste0("Projects/", input$sProjectName, "/csv_exports/", input$selFeature)
    else allFeats =  csvExportPath = paste0("Projects/", input$sProjectName, "/csv_exports/", input$selFeatureFTab)
    allFiles = sort(list.files(csvExportPath)) %>% unlist()
    lsFiles = list.files(csvExportPath)
    lsFiles
    
  }

)