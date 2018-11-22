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
      tabItem(tabName = "tabProject",
        
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
      tabItem(tabName = "tabData",
        
        fluidRow(
          box(
            div(
              fluidRow(column(width = 3, h4("Select data source"))),
              fluidRow(column(width = 3, h4("Select path")))
            ),
            title = "Data Source",
            solidHeader = TRUE,
            status = "success",
            width = 13,
            collapsible = TRUE
          )
        ),
        
        
        fluidRow(box(div(
         # box(
            fluidRow(
            column(width = 2, h4("Users")), 
            column(width = 8, h4("Features"))),

          fluidRow(
            column(width = 2, checkboxInput("dtSelAll", "(Un)Select All")),
            column(width = 4,
              drop_down_button(
                label = "Select Feature Category", status = "default", width = 80,
                checkboxGroupInput(inputId = "cbFeatureCat", label = "Choose", choices = feature_categories, selected = feature_categories)
              )
            #)
          )),
          
          br(),

          DT::dataTableOutput("dt")),
          title= "Select Users",
          solidHeader = TRUE,
          status = "success",
          width = 13,
          collapsible = T
        )
      )),
      
      tabItem(tabName = "tabFeatures",
        selectInput(inputId = "selFeature", label = "Select feature category", choices = list.files("Features")),
        uiOutput(
            outputId = "featsTabUi"
        )
      ),
      
      tabItem(tabName = "tabCollResults",
        h1("Collection of Results")
      )

  ),

  tags$head(tags$style("#txtDirExists{color: red;
                                 font-size: 12px;
                                 }"
            )
  )

))



ui <- dashboardPage(header, sidebar, body)
  