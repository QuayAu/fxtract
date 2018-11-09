featureMeanNumDiffAppsPerDay = function(data) {
  n = data %>% dplyr::group_by(date) %>% distinct(RUNNING_TASKS_baseActivity_mPackage) %>% summarize(n = n()) %>% pull(n)
  return(data.frame(mean = mean(n), sd = sd(n)))
}