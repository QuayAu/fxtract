getAllFeatsPerUser <- reactive({
# returns df with features per user
# structure: featCat | userId | featName | fileExists
  
  dfFeatsWithCat <- getAllFeats() # Remove .R
  df <- NULL
  
  for (id in ids){
    for (row in 1:nrow(dfFeatsWithCat)){
      featCat = dfFeatsWithCat[row, 1]
      feat = dfFeatsWithCat[row, 2]
      
      fileName <- paste0(featCat, "/", id, "_", feat, ".csv")
      
      filePath <- paste0("Projects/", input$sProjectName, "/csv_exports/", featCat, "/", id, "_", feat, ".csv")
      fileExists <- file.exists(filePath)
      dfTemp <- data.frame(fileName, fileExists)
      df <- rbind(df, dfTemp)
      df
    }
  } 
  
  df$fileName <- str_extract(df$fileName, '^[^.]+') # Remove .R
  
  df <- df %>% separate(fileName, sep = "/", into = c("featCat", "userFeatureName"))
  df <- df %>% separate(userFeatureName, c("userId", "featName"), "_", extra = "merge")

  df
  
})