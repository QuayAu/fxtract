getLastResults = function(){
  
  df = rv$lastCalFeatsPerUser
  results = list()
  featPath = convertToFeatPath(df)
  df = cbind(df, featPath)
  uIds = df[,1]
  
  for (k in 1:length(uIds)) {
    res = readResults(df[k,3] %>% as.character(), uIds[k])
    results[[k]] = res
  }
  results
  
  dfResults = rbindlist(results, fill = TRUE) %>% as.data.frame()
  
}

convertToFeatPath = function(df){
  # converts df of form | uID | category/feature | to Features/Category/u00_featureName
  featCatName = str_extract(df[,2], '^[^.]+') # Remove .R
  featCatName = strsplit(as.character(featCatName), split = "/")
  featCat = lapply(featCatName, `[[`, 1)
  featName = lapply(featCatName, `[[`, 2)
  projectPathName = paste0("Projects/", input$sProjectName) #"Psychologie")#input$sProjectName)
  featPath = paste0(projectPathName, "/", "csvExports/", featCat, "/", df[,1], "_", featName, ".csv")
  featPath
}

readResults = function(x, uId){
  
  featPath = x
  dfResults = read.csv(featPath)
  if(length(dfResults) > 1) names(dfResults) = paste0(getFeatName(featPath), ".", names(dfResults))
  dfResults = cbind(uId, dfResults)
  
}


getFeatCat = function(x){
  
  featCatName = str_extract(x, '^[^.]+') # Remove .R
  featCatName = strsplit(as.character(featCatName), split = "/")[1]
  featCat = featCatName[[1]][[1]]
  
}

getFeatName = function(x){
  
  featCatName = str_extract(x, '^[^.]+') # Remove .csv
  featCatName = strsplit(as.character(featCatName), split = "/")
  featName = featCatName[[1]][length(featCatName[[1]])]
  featCatName = strsplit(as.character(featName), split = "_")[[1]][2]
  featCatName
  
}


# testi = getLastResults(test_df)
# testi
# do.call(rbind.data.frame, fill = T, testi)
# library(data.table)
# rbindlist(testi, fill = TRUE)
