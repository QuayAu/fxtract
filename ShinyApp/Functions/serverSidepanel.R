# Sidepanel ----------------------------------------------------------------------------------------
### Tabs
output$tabs <- renderUI({
  
  # Read all feature categories (sub folder names in 'features' folder) e.g. appusage, communication
  # Create for each folder a separate menuSubItem in sidebar under 'Project' tab
  
  sidebarMenu(id = "idTabs",
    menuItem(text = "Project", tabName = "tabProject"),
    menuItem(text = "Data", tabName = "tabData"),
    menuItem(text = "Features", tabName = "tabFeatures"),
    menuItem(text = "Collect results", tabName = "tabCollResults")
  )
  
})

# On First load active project tab
observe({
  if(first_load){
    updateTabItems(session, "idTabs", selected = "tabProject")
    first_load <<- F
  }
})


