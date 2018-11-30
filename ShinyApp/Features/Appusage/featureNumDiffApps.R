featureNumDiffApps = function(df){
  x = length(unique(df$RUNNING_TASKS_baseActivity_mPackage))
  data.frame("n" = x)
}