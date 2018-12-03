calcExpFeat = function(featPath, df, uId, projectPath){
  
  # This functions calculates the given feature for a specific user and saves the result as csv. file
  # in the ShinyApp/csvExports directory in the relevant folder (depending on feature category of the feature)
  
  # Arguments:
  # -- featPath: Path of the feature in the form: featureCategory/featureName.R
  # -- df: data of the user
  # -- uId: userId of the user (could also be determined via df but is given as argument in order to avoid double extraction)
  # -- projectPath: path of the active project in the form: Projects/projectName

  featFile <- paste0("Features/", featPath)
  source(featFile)
  featFile = strsplit(featFile, ".", fixed = T)[[1]][1] #anpassen beim echten use case
  featCat = strsplit(featFile, "/")[[1]][2]
  featName = strsplit(featFile, "/")[[1]][3]
  
  #QUAY: unit tests einbauen
  ## check: output dataframe mit richtigen dimensionen
  ## check: uId richtiger name
  ## check: feature richtiger name
  
  df = list(df)
  
  # Calc feature
  featCalc = do.call(featName, df) 
  
  # If calculation successfull -> write csv 
  expFileName = paste0(uId, "_", featName)
  
  # Create csvExports-folder if not existing
  expDir <- paste0(projectPath, "/csvExports/")
  if (dir.exists(expDir) == FALSE){
    dir.create(expDir)
  }
  
  # Create featureCategory folder if not existing
  expDir <- paste0(expDir, "/", featCat)
  if (dir.exists(paste0(expDir)) == FALSE){
    dir.create(paste0(expDir))
  }
  
  expPath = paste0(expDir, "/", expFileName, ".csv")
  write_csv(featCalc, path = expPath)
  
  msg <- paste0(" \n Feature: '", featName, "' was successfully calculated and saved \n")
  cat(msg)
  if(length(featCalc) > 1) names(featCalc) = paste0(featName, ".", names(featCalc))
  
}