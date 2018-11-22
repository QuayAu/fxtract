getAllFeats <- function(type = "all"){
# returns a data frame containing all features with corresponding categories
# The underlying reactivePoll recognizes changes in the features folder and adds or removes the features from data
  
  df <- allFeats()
  
  fCategory <- vapply(strsplit(df,"/"), `[`, 1, FUN.VALUE=character(1))
  feature <- vapply(strsplit(df,"/"), `[`, 2, FUN.VALUE=character(1))
  dfFeatsWithCat <- data.frame(fCategory, feature)
  
  if(type != "all") dfFeatsWithCat = dfFeatsWithCat %>% filter(fCategory == input$selFeature)
  
  dfFeatsWithCat
  
}




allFeats <- reactivePoll(10, session,
  
  # Check wether calculated features of current feature category have changed                                    
  checkFunc = function(){
    featPath <- "Features/"
    if(dir.exists(featPath)) list.files(featPath)
  },
  
  # If changed -> reload calculated features                             
  valueFunc = function(){
    featPath <- "Features/"
    allFiles <- sort(list.files(featPath, recursive = T)) %>% unlist()
    allFeats <- str_extract(allFiles, '^[^.]+') # remove '.R
    allFeats
  }
  
)
