get_all_features <- function(type = "all"){
  
  if(type == "all") cur_feature_type <<- ""
  else cur_feature_type <<- rv$cur_feature_type
  
  df <- all_features()
  
}




all_features <- reactivePoll(10, session,
  
  # Check wether calculated features of current feature category have changed                                    
  checkFunc = function(){
    feat_path <- paste0("Features/", cur_feature_type)
    if(dir.exists(feat_path)) list.files(feat_path)
  },
  
  # If changed -> reload calculated features                             
  valueFunc = function(){
    feat_path <- paste0("Features/", cur_feature_type)
    all_files <- sort(list.files(feat_path, recursive = T)) %>% unlist()
    all_features <- str_extract(all_files, '^[^.]+') # remove '.R
  }
  
)
