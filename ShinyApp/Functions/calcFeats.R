calcFeats = function(){ # Documentation!!
  
  # This functions starts the calculation for all the selected features in the feature tab for all the selected user in the user tab
  # Each feature will be calculated and stored for each user separately in a csv. file
  # The csv files with the features will be saved folder with the corresponding feature category in the active project folder

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
          calcExpFeat(feature, data_id, uId, projectPathName)
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
  
}