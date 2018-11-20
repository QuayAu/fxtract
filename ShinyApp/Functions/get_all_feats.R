get_all_features <- function(type = "all"){
  
  df <- all_features()
  
  fCategory <- vapply(strsplit(df,"/"), `[`, 1, FUN.VALUE=character(1))
  feature <- vapply(strsplit(df,"/"), `[`, 2, FUN.VALUE=character(1))
  df_feats_cat <- data.frame(fCategory, feature)
  
  if(type != "all") df_feats_cat = df_feats_cat %>% filter(fCategory == input$selFeature)
  
  df_feats_cat
  
}




all_features <- reactivePoll(10, session,
  
  # Check wether calculated features of current feature category have changed                                    
  checkFunc = function(){
    feat_path <- "Features/"
    if(dir.exists(feat_path)) list.files(feat_path)
  },
  
  # If changed -> reload calculated features                             
  valueFunc = function(){
    feat_path <- "Features/"
    all_files <- sort(list.files(feat_path, recursive = T)) %>% unlist()
    all_features <- str_extract(all_files, '^[^.]+') # remove '.R
    all_features
  }
  
)
