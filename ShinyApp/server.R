shinyServer(function(input, output, session) {
  
  source("Functions/getAllFeats.R", local = TRUE, encoding = "utf-8")
  source("Functions/getCalcFeats.R", local = TRUE, encoding = "utf-8")
  source("Functions/getFreqCalcFeats.R", local = TRUE, encoding = "utf-8")
  source("Functions/getFeatNotCalc.R", local = TRUE, encoding = "utf-8")
  source("Functions/getFreqRemUsers.R", local = TRUE, encoding = "utf-8")
  source("Functions/getFeatsNotDone.R", local = TRUE, encoding = "utf-8")
  source("Functions/f_calc_save_feature.R", local = TRUE, encoding = "utf-8")
  source("Functions/serverSidepanel.R", local = TRUE, encoding = "utf-8")
  source("Functions/serverProjectTab.R", local = TRUE, encoding = "utf-8")
  source("Functions/serverFeatsTab.R", local = TRUE, encoding = "utf-8")
  source("Functions/serverDataTab.R", local = TRUE, encoding = "utf-8")
  source("Functions/getAllFeatsPerUser.R", local = TRUE, encoding = "utf-8")
  source("Functions/helperFunctions.R", local = TRUE, encoding = "utf-8")
  source(paste0(dirname(getwd()), "/R/addColumn.R"), local = TRUE, encoding = "utf-8")
  
  load_all() # Loading fxtract
  
  projectNames <<- list.files("Projects")
  db_path <- paste0(dirname(getwd()), "/vignettes/tutorial/studentlife/SQL_database.sql")
  db <<- src_sqlite(db_path, create = FALSE)
  logsAll <<- tbl(db, "studentlife_data")

  ids <<- logsAll %>% distinct(userId) %>% as.data.frame() %>% as.vector()
  
  rv = reactiveValues()
  rv$selectedUsers = NULL # Currently selected user ids in data table in data tab
  rv$selectedFeats = NULL
  
  

  
  # 
  # # Observe Button calculate all appusage features
  # observeEvent(input$btn_CalcAll_AppUsage, {
  #   output$btn_Calc_selected_AppUsage <- NULL
  #   output$giAllAppUsage <- NULL
  #   
  #   # output$giAllAppUsage <- renderUI({
  #   #   box(textOutput(
  #   #     
  #   #     cat("Hi")
  #   #   ))
  #   # })
  # 
  #   project_path_name = paste0("Projects/",rv$currentProject)
  #   
  #   # Prepare log files
  #   if (dir.exists(paste0(project_path_name, "/logfiles")) == FALSE){
  #     dir.create(paste0(project_path_name, "/logfiles"))
  #   }
  #   
  #   myfile <- paste0(project_path_name, "/logfiles/",format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), "_logfile.txt")
  #   zz <- file(myfile, open = "wt")
  #   sink(zz, type = "message")
  #   
  #   # Determine features to be calculated
  #   ls_features = list.files("Features/appusage", recursive = T)
  #   ls_features = paste0("appusage/", ls_features)
  #   ls_features <- ls_features %>% sort()
  #   
  # 
  #   pb <- txtProgressBar(min = 0, max = length(ids), style = 3)
  #   for (i in 1:length(ids)){
  #     
  #     Sys.sleep(0.1) # wegen progressbar
  #     setTxtProgressBar(pb, i)
  #     
  #     id = ids[i]
  #     data_id = logs_all %>% filter(userId == id)
  #     data_id = data.frame(data_id)
  #     
  #     msg <- paste0("\n User: ", id)
  #     cat(msg)
  #     
  #     for (feature in ls_features){
  #       out <- tryCatch(
  #         {
  #           f_calc_save_feature(feature, data_id, id, project_path_name)
  #         },
  #         error = function(cond){
  #           message(paste(id, ":", feature, ":",cond))
  #           return(NA)
  #         },
  #         warning = function(cond){
  #           message(paste(id, ":", feature, ":", cond))
  #           return(NA)
  #         }
  #       )
  #     }
  #     
  #   }
  #   
  #   # Close log files
  #   sink(type = "message")
  #   close(zz)
  #   readLines(myfile)
  #   
  # })
  

}) 
  
  
  
  