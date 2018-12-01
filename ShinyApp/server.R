shinyServer(function(input, output, session) {
  
  source("Functions/getAllFeats.R", local = TRUE, encoding = "utf-8")
  source("Functions/getCalcFeats.R", local = TRUE, encoding = "utf-8")
  source("Functions/getFreqCalcFeats.R", local = TRUE, encoding = "utf-8")
  source("Functions/getFeatNotCalc.R", local = TRUE, encoding = "utf-8")
  source("Functions/getFreqRemUsers.R", local = TRUE, encoding = "utf-8")
  source("Functions/getFeatsNotDone.R", local = TRUE, encoding = "utf-8")
  source("Functions/calcExpFeat.R", local = TRUE, encoding = "utf-8")
  source("Functions/serverSidepanel.R", local = TRUE, encoding = "utf-8")
  source("Functions/serverProjectTab.R", local = TRUE, encoding = "utf-8")
  source("Functions/serverFeatsTab.R", local = TRUE, encoding = "utf-8")
  source("Functions/serverDataTab.R", local = TRUE, encoding = "utf-8")
  source("Functions/getAllFeatsPerUser.R", local = TRUE, encoding = "utf-8")
  source("Functions/calcFeats.R", local = TRUE, encoding = "utf-8")

  load_all() # Loading fxtract
  library(shinyBS)
  
  projectNames <<- list.files("Projects")
  db_path <- paste0(dirname(getwd()), "/vignettes/tutorial/studentlife/SQL_database.sql")
  db <<- src_sqlite(db_path, create = FALSE)
  logsAll <<- tbl(db, "studentlife_data")

  ids <<- logsAll %>% distinct(userId) %>% as.data.frame() %>% as.vector()
  
  rv = reactiveValues()
  rv$selectedUsers = NULL # Currently selected user ids in data table in data tab
  rv$selectedFeats = NULL

}) 
  
  
  
  