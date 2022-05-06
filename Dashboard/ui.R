ui <- fluidPage(
  
# Header -----------------------------------------------------------------------
  
  navbarPage(
    "PEI IE Collaborative",
    theme = shinytheme("flatly"),
    id = "main",
    collapsible = TRUE, 
    header = tags$head(
      includeCSS("www/styles.css"),
      HTML("<html lang='en'>")
    ),
    
    useShinydashboard(),

# Home page --------------------------------------------------------------------

    tabPanel(
      title = "Home",
      
      fluidRow(
        column(width = 1),
        column(
          width = 10,
          
          ## About ------------------------------------------------------------
          h1("Welcome to PEI IE Collaborative Dashboard"),
          
          fluidRow(
            
            column(
              width = 8,
              includeMarkdown("www/aboutpei.md")
            ),
            
            column(
              width = 4,
              uiOutput("n_ies"),
              uiOutput("n_countries"),
              uiOutput("n_institutions") 
            )
          ),
          
          ## Thematic priorities ----------------------------------------------
          h4("Thematic priorities"),
          
          fluidRow(
            
            ### Boxes ---------------------------------------------------------
            column(
              width = 8,
              
              box(
                width = 12,
                title = "Effectiveness at scale",
                collapsible = TRUE,
                solidHeader = TRUE,
                collapsed = TRUE,
                includeMarkdown("www/scale.md")
              ),
              
              box(
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                collapsed = TRUE,
                title = '"Scalable" delivery modalities',
                includeMarkdown("www/scalable.md")
              ),
              
              box(
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                collapsed = TRUE,
                title = "Dynamics over time",
                includeMarkdown("www/dynamics.md")
              ),
              
              box(
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                collapsed = TRUE,
                title = "Bundling of interventions",
                includeMarkdown("www/bundling.md")
              ),
              
              box(
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                collapsed = TRUE,
                title = "Targeting/heterogeneity",
                includeMarkdown("www/targeting.md")
              ),
              
              box(
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                solidHeader = TRUE,
                title = "External validity across settings",
                includeMarkdown("www/validity.md")
              ),
              
              box(
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                solidHeader = TRUE,
                title = "Resilience and shock-responsiveness",
                includeMarkdown("www/resilience.md")
              )
            ),
          
            ### Graph ----------------------------------------------------------
            column(
              width = 4,
              status = "info",
              title = "Share of learning priorities", 
              
              plotOutput(
                "learning_priority_bar"
              )
            )
          ),
            
          ## Footer ------------------------------------------------------------
          fluidRow(
            includeMarkdown("www/footer.md")
          )
        )
      )
    ),

# Map page ---------------------------------------------------------------------

   tabPanel(

     title = "World map",

     column(width = 1), # Just to have some margin. Should fix this with the container later

     column(
       width = 10,
       
       ## Filters -------------------------------------------------------------
       fluidRow(
         
         box(
           width = 12,
           title = NULL,
           solidHeader = TRUE,
           status = "success",
           
           ### Identification strategy -------------------------------------------
           column(
             width = 3,
             pickerInput(
               inputId = "map_method",
               label = "Impact evaluation identification strategy:",
               choices = ie_method_lab,
               selected = ie_method_lab,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             )
           ),
           
           ### PEI learning priority ---------------------------------------------
           column(
             width = 3,
             pickerInput(
               inputId = "map_learning",
               label = "Impact evaluation learning priority:",
               choices = learning_priority,
               selected = c(1:12) %>% as.character,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             )
           ),
           
           ### PI Affiliation ----------------------------------------------------
           column(
             width = 3,
             pickerInput(
               inputId = "map_affiliation",
               label = "Principal Investigator's affiliation:",
               choices = pi_affiliation_lab,
               selected = pi_affiliation_lab,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             )
           ),
           
           ### Priority Group ---------------------------------------------------
           column(
             width = 3,
             pickerInput(
               inputId = "map_target",
               label = "Priority population groups targeted:",
               choices = priority_group_lab,
               selected = priority_group_lab,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             )
           )
           
         )
       ),
       
       ## Number of projects ---------------------------------------------------
       
       fluidRow(
         id = "info_map",
         textOutput("n_projects")
       ),
        
       ## Map ------------------------------------------------------------------

       fluidRow(
         box(
           width = 12,
           collapsible = FALSE,
           
           leafletOutput(
             "map",
             width = "100%",
             height = "600px"
           )
         )
       ),

       ## Footer ---------------------------------------------------------------

       fluidRow(
         includeMarkdown("www/footer.md")
       )
     ) # closes main box for page
   ), # Map Tab Panel closing

# Table page -------------------------------------------------------------------
   tabPanel(
      title = "Browse projects",
      
      column(width = 1), # Just to have some margin. Should fix this with the container later
      column(
        width = 10,
        
        fluidRow(
          column(
            width = 3,
            
            box(
              width = 12,
              
              uiOutput("table_vars"),
              uiOutput("table_region"),
              uiOutput("table_country"),
              
              ### Identification strategy -------------------------------------------
              pickerInput(
                inputId = "table_method",
                label = "Impact evaluation identification strategy:",
                choices = ie_method_lab,
                selected = ie_method_lab,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ),
              
              ### PEI learning priority ---------------------------------------------
              pickerInput(
                inputId = "table_learning",
                label = "Impact evaluation learning priority:",
                choices = learning_priority,
                selected = c(1:12) %>% as.character,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ),
              
              ### PI Affiliation ----------------------------------------------------
              pickerInput(
                inputId = "table_affiliation",
                label = "Principal Investigator's affiliation:",
                choices = pi_affiliation_lab,
                selected = pi_affiliation_lab,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ),
              
              ### Priority Group ---------------------------------------------------
              pickerInput(
                inputId = "table_target",
                label = "Priority population groups targeted:",
                choices = priority_group_lab,
                selected = priority_group_lab,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              )
            )
          ),
          
          column(
            width = 9,
            dataTableOutput("table")
          )
        ),
        
        fluidRow(
          includeMarkdown("www/footer.md")
        )
      )
    ) # Closes Table tab panel
  ) # Navbarpage closing
) # UI closing

