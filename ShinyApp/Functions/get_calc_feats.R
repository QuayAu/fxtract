get_calc_feats <- reactivePoll(1000, session,
                                        
  # Check wether calculated features of current feature category have changed                                    
  checkFunc = function(){
    
    if(exists("rv")) cur_feature_type <- rv$cur_feature_type
    else cur_feature_type <- list.files("Features")[[1]]
    csv_export_path <- paste0("Projects/", input$sProjectName, "/csv_exports/", cur_feature_type)
    if(dir.exists(csv_export_path)) list.files(csv_export_path)
    
  },
  
  # If changed -> reload calculated features                             
  valueFunc = function(){

    if(exists("rv")) cur_feature_type <- rv$cur_feature_type
    else cur_feature_type <- list.files("Features")[[1]]
    csv_export_path <- paste0("Projects/", input$sProjectName, "/csv_exports/", cur_feature_type)
    all_Files = sort(list.files(csv_export_path)) %>% unlist()
    ls_files <- list.files(csv_export_path)
  }

)