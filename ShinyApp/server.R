shinyServer(function(input, output, session) {
  
  source("Functions/get_all_feats.R", local = TRUE, encoding = "utf-8")
  source("Functions/get_calc_feats.R", local = TRUE, encoding = "utf-8")
  source("Functions/get_freq_calc_feats.R", local = TRUE, encoding = "utf-8")
  source("Functions/get_feats_not_calc.R", local = TRUE, encoding = "utf-8")
  source("Functions/get_freq_remain_users.R", local = TRUE, encoding = "utf-8")
  source("Functions/get_feats_not_done.R", local = TRUE, encoding = "utf-8")
  source("Functions/f_calc_save_feature.R", local = TRUE, encoding = "utf-8")
  source("Functions/server_sidepanel.R", local = TRUE, encoding = "utf-8")
  source("Functions/server_project_tab.R", local = TRUE, encoding = "utf-8")
  source("Functions/server_feats_category_tabs.R", local = TRUE, encoding = "utf-8")
  source("Functions/server_data_tab.R", local = TRUE, encoding = "utf-8")
  source("Functions/getAllFeatsPerUser.R", local = TRUE, encoding = "utf-8")
  
  
  db_path <- paste0(dirname(getwd()), "/vignettes/tutorial/studentlife/SQL_database.sql")
  db <<- src_sqlite(db_path, create = FALSE)
  ids <<- tbl(db, "studentlife_data") %>% distinct(userId) %>% as.data.frame() %>% as.vector()#%>% unlist()
  num_total_users <<- nrow(ids)
  
  
  rv <- reactiveValues()
  rv$cur_feature_type <- NULL # Currently active feature type (tab) e.g. 'communication', 'appusage'
  rv$selected_users <- NULL # Currently selected users in data table in data tab

  
  
  
  # # Appusage tab ----------------------------------------------------------------------------------------
  # 
  # # Observe Button find not done
  # observeEvent(input$btn_FindNotDone_AppUsage, {
  #   
  #   rv$choices = get_Features_not_Done()
  #   print("here")
  #   output$giAllAppUsage <- renderUI({
  #     box(
  #       checkboxInput('cAllNone', 'All/None'),
  #       checkboxGroupInput(
  #         inputId = "giAllAppUsage",
  #         choices = rv$choices,
  #         label = "All app usage features"
  #       )
  #     )
  # 
  #   })
  # 
  # })
  # 
  # # Observe Button list all
  # observeEvent(input$btn_listAll_appusage, {
  # 
  #   rv$choices = get_All_Features()
  # 
  #   output$giAllAppUsage <- renderUI({
  #     box(
  #       checkboxInput('cAllNone', 'All/None'),
  #       checkboxGroupInput(
  #         inputId = "giAllAppUsage",
  #         choices = rv$choices,
  #         label = "All app usage features"
  #       )
  #     )
  # 
  #   })
  # 
  # })
  # 
  # # Observe select all/none Checkbox
  # observe({
  #   
  #   selectAll = input$cAllNone
  #   if (is.null(selectAll)) selectAll = F
  #   
  #   if (input$inTabset != "tab_project"){
  #     updateCheckboxGroupInput(
  #       session, 'giAllAppUsage', choices = rv$choices,
  #       selected = if (selectAll) rv$choices
  #     )
  #   }
  #   
  # })
  # 
  # # Render Button calulate selected Features
  # output$btn_Calc_selected_AppUsage <- renderUI({
  #   
  #   selected_features <- input$giAllAppUsage
  #   if (length(selected_features) == 0) return(NULL)
  #   actionButton("btn_calc_selected_appusage", label = "Calculate selected!",
  #     style="color: #fff; background-color: #337ab7; border-color: #2e6da4;")
  #   
  # })
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
  
  
  
  