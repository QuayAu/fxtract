##################### Header ############################################################################
header <- dashboardHeader(title="Menu")

##################### Sidebar ############################################################################
sidebar <- dashboardSidebar(
  
  uiOutput(
    outputId = "tabs"
  )

)

##################### Body ######################################################################
body <-  dashboardBody(
  
  ## NOTE: Colours in feature categories tabs same as in map (for find all, etc.); Headlines in green

  fluidPage(
    
    # Include CSS files for styling
    includeCSS("styles.css"),
    
    tabItems(

      # Project tab ----------------------------------------------------------------------------------------
      tabItem(tabName = "tab_project",
        
        h1("Project"),
        
        # Select Project
        fluidRow(
          
          column(
            width = 3,
            uiOutput("projectName")
          ),
          
          column(
            width = 1,
            
            fluidRow(style='padding:22px;'),
            
              actionButton(width = 100,
                "btnGoToAppUsage", "OK",
                style="color: #fff; background-color: #207d09; border-color: #207d09;"
              )
            
          )
          
        ),

        ## Create Project
        fluidRow(
          
          ### Textfield
          column(
            
            width = 3,
            
            textInput(
              inputId = "txtNewProjectName",
              label = tags$h4("Create New Project",style="text-align:center;color:#207d09;font-size:19;"),
              placeholder = "New Project Name"
            ),
            
            htmlOutput(
              outputId = "txtDirExists"
            )
            
          ),
          ### Button
          column(width = 1,
            
            fluidRow(style='padding:22px;'),
            
            actionButton(
              "btnCreateProject", "Create", icon("plus-circle"),
              style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"
            )

          )
          
        )

      ),
      
      # Data tab ------------------------------------------------------------------------------------
      tabItem(tabName = "tab_data",
        box(div(
          fluidRow(
            column(width = 2, h4("Users")), 
            column(width = 8, h4("Features"))),

          fluidRow(
            column(width = 2, checkboxInput("dt_sel", "(Un)Select All")),
            column(width = 4,
              drop_down_button(
                label = "Select Feature Category", status = "default", width = 80,
                checkboxGroupInput(inputId = "cbFeatureCat", label = "Choose", choices = feature_categories, selected = feature_categories)
              )
            )
          ),

          DT::dataTableOutput("dt")),
          title= "Table",
          solidHeader = TRUE,
          status = "warning",
          width = 13,
          collapsible = T
        )
      ),
      
      # Feature category tabs ------------------------------------------------------------------------------------
      tabItem(tabName = paste0("tab_", feature_categories[[1]]),
        uiOutput(
          outputId = "feature_category_tabs"
        )
      )
      
      # tabItem(tabName = input$id_tabs,
      #   h1("Appusage")
      # )

      # Appusage tab ----------------------------------------------------------------------------------------
  #     tabItem(tabName = "tab_appusage",
  #       h1("Appusage"),
  # 
  #       fluidRow(style='padding:10px;'),
  # 
  #       ### Calculate BUTTON
  #       fluidRow(
  # 
  #           column(width = 2, h4("Calculate all appusage features",
  #                 style = "color:#207d09;font-size:19;")),
  # 
  #           column(width = 2, actionButton(
  #             inputId = "btn_CalcAll_AppUsage",
  #             label = "Calculate!",
  #             style="color: #fff; background-color: #207d09; border-color: #207d09;",
  #             width = 100
  #           ))
  # 
  #       ),
  # 
  #       fluidRow(style='padding:8px;'),
  # 
  #       ### Features not done
  #       fluidRow(
  # 
  #         column(width = 2, h4("Find features not done",
  #           style = "color:#207d09;font-size:19;")),
  # 
  #         column(width = 2, actionButton(
  #           inputId = "btn_FindNotDone_AppUsage",
  #           label = "Find!",
  #           style="color: #fff; background-color: #207d09; border-color: #207d09;",
  #           width = 100
  #         ))
  # 
  #       ),
  # 
  #       fluidRow(style='padding:8px;'),
  # 
  # 
  #       # All features
  #       fluidRow(
  # 
  #         column(width = 2, h4("Select specific features",
  #           style = "color:#207d09;font-size:19;")),
  # 
  #         column(width = 2, actionButton(
  #           inputId = "btn_listAll_appusage",
  #           label = "Select!",
  #           style="color: #fff; background-color: #207d09; border-color: #207d09;",
  #           width = 100
  #         ))
  # 
  #       ),
  # 
  # 
  #       fluidRow(style='padding:20px;'),
  # 
  #       fluidRow(
  # 
  #         uiOutput(
  #           outputId = "giAllAppUsage"
  #         ),
  # 
  #         uiOutput(
  #           outputId = "btn_Calc_selected_AppUsage"
  #         )
  # 
  #       )
  # 
  #     ),
  # 
  #     # Communication tab
  #     tabItem(tabName = "tab_communication",
  #       h2("Communication")
  #     ),
  # 
  #     # General tab
  #     tabItem(tabName = "tab_general",
  #       h2("General")
  #     ),
  # 
  #     # Music tab
  #     tabItem(tabName = "tab_music",
  #       h2("Music")
  #     ),
  # 
  #     # Collect results tab
  #     tabItem(tabName = "tab_collect_res",
  #       h2("Collect results")
  #     )
  # 
  #   )
  # 
  ),

  tags$head(tags$style("#txtDirExists{color: red;
                                 font-size: 12px;
                                 }"
            )
  )

))



ui <- dashboardPage(header, sidebar, body)
  