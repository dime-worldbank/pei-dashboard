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
    shinyjs::useShinyjs(),

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
              
              infoBox(
                width = 12,
                title = "",
                subtitle = "impact evaluations recorded",
                value = project_data %>% nrow(),
                icon = icon("briefcase", lib = "glyphicon"),
                color = "teal",
                fill = TRUE
              ),
              
              infoBox(
                width = 12,
                title = "",
                subtitle = "countries with active impact evaluations",
                value = projects_country %>% pull(iso) %>% n_distinct(),
                icon = icon("map-marker", lib = "glyphicon"),
                color = "teal",
                fill = TRUE
              ),
              
              infoBox(
                width = 12,
                title = "",
                subtitle = "institutions with active impact evaluations",
                value = pi_affiliation_lab %>% n_distinct(),
                icon = icon("building"),
                color = "teal",
                fill = TRUE
              )
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
       
       fluidRow(
         
         ## Filters -------------------------------------------------------------
         column(
           width = 3,
           
           box(
             width = 12,
             title = h3("Filter impact evaluations"),
             
             ### Identification strategy -------------------------------------------
             pickerInput(
               inputId = "map_method",
               label = "Identification strategy",
               choices = ie_method_lab,
               selected = ie_method_lab,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             ),
             
             ### PEI learning priority ---------------------------------------------
             pickerInput(
               inputId = "map_learning",
               label = "Learning priority",
               choices = learning_priority,
               selected = c(1:12) %>% as.character,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             ),
             
             ### PI Affiliation ----------------------------------------------------
             pickerInput(
               inputId = "map_affiliation",
               label = "Principal investigator's affiliation",
               choices = pi_affiliation_lab,
               selected = pi_affiliation_lab,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             ),
             
             ### Priority Group ---------------------------------------------------
             pickerInput(
               inputId = "map_target",
               label = "Priority groups targeted",
               choices = priority_group_lab,
               selected = priority_group_lab,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             ),
             
             fluidRow(
               valueBox(
                 width = 12,
                 subtitle = "impact evaluations selected",
                 value = textOutput("n_projects"),
                 color = "light-blue"
               )
             )
           )
         ),
         
         column(
           width = 9,
           
           h2("IE Collaborative Impact Evaluations"),

           plotlyOutput(
             "map",
             height = "700px"
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
              
              h3("Information to display"),
              
              uiOutput("table_vars"),
              
              h3("Filter interventions"),
              
              pickerInput(
                inputId = "table_region",
                label =  "Region", 
                choices = projects_country %>% pull(region) %>% unique,
                selected = projects_country %>% pull(region) %>% unique,
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
              ),
              
              pickerInput(
                inputId = "table_country",
                label =  "Country", 
                choices = projects_country %>% pull(country) %>% unique,
                selected = projects_country %>% pull(country) %>% unique,
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
              ),
              
              ### Priority Group ---------------------------------------------------
              pickerInput(
                inputId = "table_target",
                label = "Priority population groups targeted",
                choices = priority_group_lab,
                selected = priority_group_lab,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ),
              
              h3("Filter impact evaluations"),
              
              ### Identification strategy -------------------------------------------
              pickerInput(
                inputId = "table_method",
                label = "Impact evaluation identification strategy",
                choices = ie_method_lab,
                selected = ie_method_lab,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ),
              
              ### PEI learning priority ---------------------------------------------
              pickerInput(
                inputId = "table_learning",
                label = "Impact evaluation learning priority",
                choices = learning_priority,
                selected = c(1:12) %>% as.character,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              ),
              
              ### PI Affiliation ----------------------------------------------------
              pickerInput(
                inputId = "table_affiliation",
                label = "Principal Investigator's affiliation",
                choices = pi_affiliation_lab,
                selected = pi_affiliation_lab,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              )
            )
          ),
          
          column(
            width = 9,
            dataTableOutput("table")
          ),
          bsModal(
            id = "popup", 
            title = "Project Details", 
            trigger = "test",
            withSpinner(
              uiOutput(
                "iecard"
                )
              )
            )
        ),
        
        fluidRow(
          includeMarkdown("www/footer.md")
        )
      )
    ) # Closes Table tab panel
  ) # Navbarpage closing
) # UI closing

