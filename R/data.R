#' phonedata
#'
#' Contains exemplary raw phone logs.
#'
#' @name phonedata
#' @keywords data
#' @docType data
"phonedata"

#' studentlife.small
#'
#' Contains exemplary gps and app data from the studentlife dataset (users: u00, u01, u02)
#'
#' http://studentlife.cs.dartmouth.edu/dataset.html
#'
#' Wang, Rui, Fanglin Chen, Zhenyu Chen, Tianxing Li, Gabriella Harari, Stefanie Tignor, Xia Zhou, Dror Ben-Zeev, and Andrew T. Campbell. "StudentLife: Assessing Mental Health, Academic Performance and Behavioral Trends of College Students using Smartphones." In Proceedings of the ACM Conference on Ubiquitous Computing. 2014.
#' @name studentlife.small
#' @keywords data
#' @docType data
"studentlife.small"

# library(dplyr)
# gpsdummy = read.csv("data_gitignore/studentlife/dataset/sensing/gps/gps_u00.csv")
# cn = c(names(gpsdummy), "DELETEME")
# gps00 = read.csv("data_gitignore/studentlife/dataset/sensing/gps/gps_u00.csv", col.names = cn) %>%
#   select(-DELETEME) %>%
#   addColumn(function(x) rep("00", nrow(x)), colname = "userId") %>%
#   mutate(timestamp = time, time = NULL)
# gps01 = read.csv("data_gitignore/studentlife/dataset/sensing/gps/gps_u01.csv", col.names = cn) %>%
#   select(-DELETEME) %>%
#   addColumn(function(x) rep("01", nrow(x)), colname = "userId") %>%
#   mutate(timestamp = time, time = NULL)
# gps02 = read.csv("data_gitignore/studentlife/dataset/sensing/gps/gps_u02.csv", col.names = cn) %>%
#   select(-DELETEME) %>%
#   addColumn(function(x) rep("02", nrow(x)), colname = "userId") %>%
#   mutate(timestamp = time, time = NULL)
# app00 = read.csv("data_gitignore/studentlife/dataset/app_usage/running_app_u00.csv") %>% addColumn(function(x) rep("00", nrow(x)), colname = "userId")
# app01 = read.csv("data_gitignore/studentlife/dataset/app_usage/running_app_u01.csv") %>% addColumn(function(x) rep("01", nrow(x)), colname = "userId")
# app02 = read.csv("data_gitignore/studentlife/dataset/app_usage/running_app_u02.csv") %>% addColumn(function(x) rep("02", nrow(x)), colname = "userId")
#
#
# d00 = gps00 %>% bind_rows(app00) %>% arrange(timestamp)
# d01 = gps01 %>% bind_rows(app01) %>% arrange(timestamp)
# d02 = gps02 %>% bind_rows(app02) %>% arrange(timestamp)
# studentlife.small = d00 %>% bind_rows(d01) %>% bind_rows(d02)
# studentlife.small$ï..id = NULL
# library(devtools)
# use_data(studentlife.small, overwrite = TRUE)
