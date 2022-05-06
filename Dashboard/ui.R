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

# Home page --------------------------------------------------------------------

    tabPanel(
      title = "Home",
      
      fluidRow(
        column(width = 1),
        
        column(
          width = 10,
          h1("Welcome to PEI IE Collaborative Dashboard")
        )
      ),
      
      fluidRow(
        column(width = 1),
        
        column(
          width = 7,
          includeMarkdown("www/aboutpei.md")
        ),
        
        column(
          width = 3,
          uiOutput("n_ies"),
          uiOutput("n_countries") 
        )
      ),
      
      fluidRow(
        column(width = 1),
        
        column(
          width = 5,
          
          box(
            title = "Effectiveness at scale",
            includeMarkdown("www/scale.md")
          ),
          
          box(
            title = '"Scalable" delivery modalities',
            includeMarkdown("www/scalable.md")
          )
        ),
        
        column(
          width = 5,
          status = "info",
          title = "Share of learning priorities", 
          plotOutput(
            "learning_priority_bar", 
            height = "40vh"
          )
        )
      ),
      
      fluidRow(
        column(width = 1),
        column(
          width = 10,
          includeMarkdown("www/footer.md")
        )
      )
    ),

# Map page ---------------------------------------------------------------------

   tabPanel(
     
     title = "World map",

     column(width = 1), # Just to have some margin. Should fix this with the container later
     
     box(
       width = 10,
       title = NULL,
       collapsible = FALSE,
         
       ## Filters -------------------------------------------------------------
       fluidRow(
         
         ### Identification strategy -------------------------------------------
         column(
           width = 3,
           pickerInput(
             inputId = "method",
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
             inputId = "learning",
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
             inputId = "affiliation",
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
             inputId = "target",
             label = "Priority population groups targeted:",
             choices = priority_group_lab,
             selected = priority_group_lab,
             options = list(`actions-box` = TRUE),
             multiple = TRUE
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
             height = "700px"
           )
         )
       ),
       
       ## Footer ---------------------------------------------------------------
       
       fluidRow(
         column(width = 1),
         column(
           width = 10,
           includeMarkdown("www/footer.md")
         )
       )
     ) # closes main box for page
   ) # Map Tab Panel closing
  ) # Navbarpage closing
) # UI closing

