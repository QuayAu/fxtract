library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)
library(devtools)
library(dplyr)
library(tidyr)
library(pryr)
library(robustbase)
library(readr)
library(DT)

first_load <- T
projectNames <- list.files("Projects")

ids <- c("001", "002", "003") # Temporary -> later ids from db
num_total_users <- length(ids)

cur_feature_type <- NULL # Currently active feature type (tab) e.g. 'communication', 'appusage'
feature_categories <<- list.files("Features")
source("Functions/drop_down_button.R", local = TRUE, encoding = "utf-8")

# Create features ------------------------------------
#load_all("phonestudyLMU")
# options(scipen = 999)
# mem_used()

# some important functions we need
#source("Scripts/code_blocks/helper_functions.R")

# allapps <- read_csv("Data/apps.csv")
# consider using the top 100, 500 etc apps
# allapps <- allapps %>% select(apps.packageName) %>% distinct(apps.packageName) %>% pull(apps.packageName)

# Custom functions
# my.sd = function(x) sd(x[!is.nan(x)], na.rm = TRUE) # function to use the standard deviation of days where data was available
# my.mean = function(x) mean(x[!is.nan(x)], na.rm = TRUE)
# my.max = function(x) max(x[!is.nan(x)], na.rm = TRUE)
# my.min = function(x) min(x[!is.nan(x)], na.rm = TRUE)
# my.sum = function(x) sum(x[!is.nan(x)], na.rm = TRUE)
# my.huberM = function(x) robustbase::huberM(x)$mu
# my.Qn = function(x) {
#   if (all(is.na(x))) x = NA
#   x = x[!is.na(x)]
#   robustbase::Qn(x)
# }
# my.robust.sum = function(x) huberM(x)$mu * length(x)


#### DEFINE CONSTRAINTS ####
#get background apps to ignore:
# app.categories <- read_delim("Data/app_categories.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# background_apps = app.categories %>% filter(category.new == "Background") %>% select(packagename)
# background_apps = background_apps$packagename
# 
# # define app categories
# #appCatPlay <- app.categories %>% select(category.GooglePlayStore) %>% filter(!is.na(category.GooglePlayStore)) %>% unique() %>% pull()
# 
# #define launcher
# launcher_apps = app.categories %>% filter(category.new == "Launcher") %>% select(packagename)
# launcher_apps = launcher_apps$packagename
# 
# # Create Features Empra ----------------------------------------------
# 
# db = src_sqlite("Data/SQL_database_EMPRA", create = FALSE)
# src_tbls(db)
# logs_all = tbl(db, from = "df1")
# 
# 
# #### Start Calculating Features ---------------------------------------------
# 
# # Folder Andi in folder Scripts
# 
# ids = logs_all %>% select(userId) %>% distinct(userId)
# ids = data.frame(ids)
# ids = ids$userId[1:2] # TEMPORARY




