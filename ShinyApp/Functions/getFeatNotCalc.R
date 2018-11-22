getFeatNotCalc <- function(){
  # This functions returns a vector containing all not calculated features 
  # The format is: " feature name, "(Remaining users: 'All')" "
  
  allFeats = getAllFeats(input$selFeature)$feature
  calcFeats <- getFreqCalcFeats()$Calulated_Features
  notCalcFeats <- allFeats[!(allFeats %in% calcFeats)]

  if(length(notCalcFeats) == 0) return(NULL)
  strNotCalcFeats <- paste(notCalcFeats, "(Remaining users: 'All')")
  strNotCalcFeats
  
}