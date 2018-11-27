# Render UI for Project tab ----------------------------------------------------------------------------------------
# ### Select Project
output$projectName<-renderUI({
  
  selectInput("sProjectName",
    choices = projectNames,
    label = tags$h4("Select Project Name",style="text-align:center;color:#207d09;font-size:19;")
  )
  
})

### Create Project
observeEvent(input$btnCreateProject, {
  
  newProjectName <- input$txtNewProjectName
  newDirPath = paste0("Projects/", newProjectName)
  
  if (dir.exists(newDirPath)){
    
    output$txtDirExists <- renderUI({
      HTML("The project name already exists. Please choose another name or delete the existing project and try again!")
    })
    
  } else {
    
    output$txtDirExists <- renderText({""})
    
    dir.create(newDirPath)
    
    updateSelectInput(session, "sProjectName",
      choices = list.files("Projects"),
      selected = newProjectName
    )
    
  }
  
})

### Next Button
observeEvent(input$btnGoToAppUsage, {
  updateTabItems(session, "idTabs",
    selected = "tabData")
})