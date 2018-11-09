f_calc_save_feature = function(feature_path, x, id, project_path_name){  #argumente richtige namen geben und dokumentieren #### add active_project_name
  
  file_path <- paste0("Features/", feature_path)
  print(file_path)
  source(file_path)
  
  #Calc Feature
  feature_calc = feature.fun(x) 
  
  #QUAY: unit tests einbauen
  ## check: output dataframe mit richtigen dimensionen
  ## check: id richtiger name
  ## check: feature richtiger name
  file_path = strsplit(file_path, ".", fixed = T)[[1]][1] #anpassen beim echten use case
  feature_cat = strsplit(file_path, "/")[[1]][2]
  feature_name = strsplit(file_path, "/")[[1]][3]
  
  
  # If successfull write csv 
  myfilename = paste0(id, "_", feature_name)
  
  saving_dir <- paste0(project_path_name, "/csv_exports/")
  if (dir.exists(saving_dir) == FALSE){
    dir.create(saving_dir)
  }
  saving_dir <- paste0(saving_dir, "/", feature_cat)
  if (dir.exists(paste0(saving_dir)) == FALSE){
    dir.create(paste0(saving_dir))
  }
  
  myfilename = paste0(saving_dir, "/", myfilename, ".csv")
  
  write_csv(feature_calc, path = myfilename)
  
  msg <- paste0(" \n Feature: '", feature_name, "' was successfully calculated and saved \n")
  cat(msg)
}