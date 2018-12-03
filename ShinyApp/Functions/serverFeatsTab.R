# Render UI for Feature tab ----------------------------------------------------------------------------------------

dtFTabProxy = DT::dataTableProxy("dtFTab")

# Table -------------------------------------------------------------------
output$dtFTab <- DT::renderDataTable({ # Also include already done feats
  
  if(is_empty(rv$inpSelFeature)) return(NULL) # Move back from features tab to data tab
  df = getFeatsNotDone()%>% as.data.frame()
  
  if(nrow(df) == 0) return(NULL) #If no features to be shown
  rv$dataDtFTab = df
  
  DT::datatable(df, rownames=TRUE, filter = "top")
  
})


# All/None Checkbox ---------------------------------------------------------------------------
observeEvent(input$dtSelAllFTab, {
  if (isTRUE(input$dtSelAllFTab)) {
    DT::selectRows(dtFTabProxy, input$dtFTab_rows_all)
  } else {
    DT::selectRows(dtFTabProxy, NULL)
  }
})

# Number of selected Users -------------------------------------------------------------------
output$nUsers <- renderText({paste("n =", length(rv$selectedUsers))})

# Observe selected features ------------------------------------------------------------------
observe({
  if(selFeats()) rv$selectedFeats = rownames(rv$dataDtFTab[input$dtFTab_rows_selected,])
  else rv$selectedFeats = NULL
})

selFeats = reactive({!is.null(input$dtFTab_rows_selected)}) 

# Button calc selected -----------------------------------------------------------------------
output$btnCalcSel <- renderUI({
  if (length(rv$selectedFeats) == 0 | length(rv$selectedUsers) == 0) return(NULL)
  actionButton("btnCalcSel", label = "Calculate selected!",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4;")
})

observeEvent(input$btnCalcSel, {
  
  remUsers = rv$dataDtFTab[input$dtFTab_rows_selected,2]

  if(all(remUsers == length(rv$selectedUsers))) calcFeats()
  else {
    showModal(modalDialog(
      tags$div(HTML("Some of the features have already been calculated for some of the users! <br>
          Do you want to calculate the features for all"), length(rv$selectedUsers), HTML("selected users or only the
        remaining ones?"), style = "font-size: 15px;"),
      fluidRow(style='padding:15px;'),
      tags$div(checkboxInput("cRemOnly", label = "Only remaining users", value = T), style = "font-size: 13px;"),
      title = "Calculate Features",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btnCalcSelModal", label = "Start Calculation!",
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"))
      )
    )
  }
})


# Button Calculate Selected on modalDialog
observeEvent(input$btnCalcSelModal, {
  removeModal()
  rv$RemainOnly = input$cRemOnly
  calcFeats()
})


