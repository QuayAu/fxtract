# Sidepanel ----------------------------------------------------------------------------------------
### Tabs
output$tabs <- renderUI({
  
  # Read all feature categories (sub folder names in 'features' folder) e.g. appusage, communication
  # Create for each folder a separate menuSubItem in sidebar under 'Project' tab
  
  sidebarMenu(id = "id_tabs",
    menuItem(text = "Project", tabName = "tab_project"),
    menuItem(text = "Data", tabName = "tab_data"),
    menuItem(text = "Features", tabName = "tabFeatures"),
    menuItem(text = "Collect results", tabName = "tab_collect_res")
  )
  
})

# On First load active project tab
observe({
  if(first_load){
    updateTabItems(session, "id_tabs", selected = "tab_project")
    first_load <<- F
  }
})


