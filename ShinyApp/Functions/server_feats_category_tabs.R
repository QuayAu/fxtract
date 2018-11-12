
# UI =================================================================================
# Upper fix part -------------------------------------------------------------------
output$feature_category_tabs <- renderUI({
  
  rv$cur_feature_type <- input$selFeature
  
  tagList(
    
    h1("Features"),
    
    fluidRow(style='padding:10px;'),
  
    ### Calculate ALL features (of specific feature category)
    fluidRow(
      
      # Text
      column(width = 3, h4("Calculate all", rv$cur_feature_type ,"features",
        style = "color:#207d09;font-size:19;")),
      
      # Button
      column(width = 2, actionButton(
        inputId = "btn_calc_all",
        label = "Calculate!",
        style="color: #fff; background-color: #207d09; border-color: #207d09;",
        width = 100
      ))
      
    ),
    
    fluidRow(style='padding:8px;'),
    
    ### Find Features not done (of specific feature category)
    fluidRow(
      
      column(width = 3, h4("Find", rv$cur_feature_type, "features not calculated",
        style = "color:#207d09;font-size:19;")),
      
      column(width = 2, actionButton(
        inputId = "btn_find_not_done",
        label = "Find!",
        style="color: #fff; background-color: #207d09; border-color: #207d09;",
        width = 100
      ))
      
    ),
    
    fluidRow(style='padding:8px;'),
     
    ### Show All features (of specific feature category)
    fluidRow(
      
      column(width = 3, h4("Select specific features",
        style = "color:#207d09;font-size:19;")),
      
      column(width = 2, actionButton(
        inputId = "btn_list_all",
        label = "Select!",
        style="color: #fff; background-color: #207d09; border-color: #207d09;",
        width = 100
      ))
      
    ),
    
    fluidRow(style='padding:20px;'),
    
    fluidRow(
      
      # List box with features
      uiOutput(
        outputId = "list_box_features"
      ),
      
      # Button: Calculate selected features in list box
      uiOutput(
        outputId = "btn_Calc_selected"
      )
      
    )
    
  )

})

# Lower flexible part -------------------------------------------------------------------

# Button: Calculate selected features in list box
output$btn_Calc_selected <- renderUI({
  
  selected_features <- input$inp_list_box_features
  
  if (length(selected_features) == 0) return(NULL)
  actionButton("btn_calc_selected", label = "Calculate selected!",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4;")
  
})

# Observe Button list all -> if clicked show listbox with all features
observeEvent(input$btn_list_all, {

  output$list_box_features <- renderUI({
    box(
      checkboxInput('cAllNone', 'All/None'),
      checkboxGroupInput(
        inputId = "inp_list_box_features",
        choices = get_all_features(type = rv$cur_feature_type),
        label = "All app usage features"
      )
    )

  })
  
  b_show_all <<- T

})

# Observe select all/none Checkbox
observeEvent(input$cAllNone, {
  
  if(b_show_all == T) choice <- get_all_features(type = rv$cur_feature_type)
  else choice <- get_feats_not_done()
  selectAll <- input$cAllNone
  print(selectAll)
  
  if (is.null(selectAll)) selectAll <- F

  updateCheckboxGroupInput(
    session = session, 
    inputId = 'inp_list_box_features', 
    choices = choice,
    selected = if (selectAll) choice
  )
  
})


# Observe Button find not done
observeEvent(input$btn_find_not_done, {
  
  output$list_box_features <- renderUI({
    box(
      checkboxInput('cAllNone', 'All/None'),
      checkboxGroupInput(
        inputId = "inp_list_box_features",
        choices = get_feats_not_done(),
        label = "All app usage features"
      )
    )

  })
  
  b_show_all <<- F
  
})

# Observe Button find not done
observeEvent(input$btn_calc_all, {
  
  # Prepare log files
  if (dir.exists("logfiles") == FALSE){
    dir.create("logfiles")
  }
  

  
})








