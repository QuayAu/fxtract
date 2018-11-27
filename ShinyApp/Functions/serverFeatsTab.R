
# UI =================================================================================
# Upper fix part -------------------------------------------------------------------
output$featsTabUi <- renderUI({
  
  tagList(

    fluidRow(style='padding:10px;'),

    ### Calculate ALL features (of specific feature category)
    fluidRow(

      # Text
      column(width = 4, h4("Calculate all", input$selFeature ,"features",
        style = "color:#207d09;font-size:19;")),

      # Button
      column(width = 2, actionButton(
        inputId = "btnCalcAll",
        label = "Calculate!",
        style="color: #fff; background-color: #207d09; border-color: #207d09;",
        width = 100
      ))

    ),

    fluidRow(style='padding:8px;'),

    ### Find Features not done (of specific feature category)
    fluidRow(

      column(width = 4, h4("Find", input$selFeature, "features not calculated",
        style = "color:#207d09;font-size:19;")),

      column(width = 2, actionButton(
        inputId = "btnFindNotDone",
        label = "Find!",
        style="color: #fff; background-color: #207d09; border-color: #207d09;",
        width = 100
      ))

    ),

    fluidRow(style='padding:8px;'),

    ### Show All features (of specific feature category)
    fluidRow(

      column(width = 4, h4("Select specific features",
        style = "color:#207d09;font-size:19;")),

      column(width = 2, actionButton(
        inputId = "btnListAll",
        label = "Select!",
        style="color: #fff; background-color: #207d09; border-color: #207d09;",
        width = 100
      ))

    ),

    fluidRow(style='padding:20px;'),

    fluidRow(

      # List box with features
      uiOutput(
        outputId = "lbFeats"
      ),

      # Button: Calculate selected features in list box
      uiOutput(
        outputId = "btnCalcSel"
      )

    )
    
  )

})

# Lower flexible part -------------------------------------------------------------------

# Button: Calculate selected features in list box
output$btnCalcSel <- renderUI({
  
  selected_features <- input$inpLbFeats
  
  if (length(selected_features) == 0) return(NULL)
  actionButton("btnCalcSel", label = "Calculate selected!",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4;")
  
})

# Observe Button list all -> if clicked show listbox with all features
observeEvent(input$btnListAll, {
  
  if(is_empty(rv$selectedUsers)) return(NULL)
  output$lbFeats <- renderUI({
    box(
      checkboxInput('cAllNone', 'All/None'),
      checkboxGroupInput(
        inputId = "inpLbFeats",
        choices = getAllFeats(type = input$selFeature)$feature,
        label = "All app usage features"
      )
    )

  })
  
  rv$show_all <- T

})

# Observe select all/none Checkbox
observeEvent(input$cAllNone, {

  if(rv$show_all  == T) choice <- getAllFeats(type = input$selFeature)$feature
  else choice <- getFeatsNotDone()
  selectAll <- input$cAllNone

  if (is.null(selectAll)) selectAll <- F

  updateCheckboxGroupInput(
    session = session,
    inputId = 'inpLbFeats',
    choices = choice,
    selected = if (selectAll) choice
  )

})


# Observe Button find not done
observeEvent(input$btnFindNotDone, {
  
  if(is_empty(rv$selectedUsers)) return(NULL)
  
  output$lbFeats <- renderUI({
    box(
      checkboxInput('cAllNone', 'All/None'),
      checkboxGroupInput(
        inputId = "inpLbFeats",
        choices = getFeatsNotDone(),
        label = "All app usage features"
      )
    )

  })
  
  rv$show_all  <- F
  
})

# Observe Button calc all
observeEvent(input$btnCalcAll, {
  
  # Prepare log files
  if (dir.exists("logfiles") == FALSE){
    dir.create("logfiles")
  }
  
})



dtFTabProxy <- DT::dataTableProxy("dtFTab")
# Table -------------------------------------------------------------------
output$dtFTab <- DT::renderDataTable({
  
  if(is_empty(rv$inpSelFeature)) return(NULL) # Move back from features tab to data tab
  df <- getFeatsNotDone()%>% as.data.frame()
  
  if(nrow(df) == 0) return(NULL) #If no features to be shown
  
  DT::datatable(df, rownames=TRUE, filter = "top"#,
    # options = list( 
    #   scrollX = TRUE
    # )
  ) %>% formatStyle(1:ncol(df),
    backgroundColor = styleEqual(c(1), c('#b5f6aa'))
  )
  
  
})


# All/None Checkbox -------------------------------------------------------------------
observeEvent(input$dtSelAllFTab, {
  if (isTRUE(input$dtSelAllFTab)) {
    DT::selectRows(dtFTabProxy, input$dtFTab_rows_all)
  } else {
    DT::selectRows(dtFTabProxy, NULL)
  }
})

# Number of selected Users
output$nUsers <- renderText({paste("n =", length(rv$selectedUsers))})






