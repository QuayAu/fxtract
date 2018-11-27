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

# On First load active project tab; Create project folder if not existing
observe({
  if(is_empty(input$idTabs)){
    updateTabItems(session, "idTabs", selected = "tabProject")
    if (!dir.exists("Projects")) dir.create("Projects")
  }
})


observe({
  if(is_empty(input$idTabs)) return(NULL) # First load -> prevent crash
  
  if(input$idTabs == "tabData") rv$inpSelFeature = input$selFeature # Differentiate between buttons with almost same functionality on different pages
  else if (input$idTabs == "tabFeatures") rv$inpSelFeature = input$selFeatureFTab
})