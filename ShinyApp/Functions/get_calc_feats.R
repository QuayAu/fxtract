get_calc_feats <- reactivePoll(1000, session,
# This reactive poll returns the name of the csv-files that are already saved in the 'csv_exports' in the active project
                                        
  # Check wether calculated features of current feature category have changed                                    
  checkFunc = function(){
    
    csv_export_path <- paste0("Projects/", input$sProjectName, "/csv_exports/", input$selFeature)
    if(dir.exists(csv_export_path)) list.files(csv_export_path)
    
  },
  
  # If changed -> reload calculated features                             
  valueFunc = function(){

    csv_export_path <- paste0("Projects/", input$sProjectName, "/csv_exports/", input$selFeature)
    all_Files = sort(list.files(csv_export_path)) %>% unlist()
    ls_files <- list.files(csv_export_path)
    ls_files
    
  }

)