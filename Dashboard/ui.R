ui <- dashboardPage(
  
  freshTheme = create_theme(bs4dash_layout(sidebar_width = "400px")),
  
# Header -----------------------------------------------------------------------
  
  dashboardHeader(
    
    title = dashboardBrand(
      title = "IE Collaborative Dashboard",
    ),
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("th"),
    fixed = FALSE
    
  ),

# Navigation menu ---------------------------------------------------------
  dashboardSidebar(
    
    status = "info",
    skin = "light",
    elevation = 5,
    
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("World map", tabName = "world_map", icon = icon("globe-americas")),
      menuItem("Browse projects", tabName = "data", icon = icon("table"))
    )
  ),

# Body ------------------------------------------------------------------------
  
  dashboardBody(
    tabItems(
      
      ## Landing page --------------------------------------------------------
      tabItem(

        tabName = "home",

        bs4Card(
          width = 12,
          status = "navy",
          solidHeader = TRUE,
          title = "IE Collaborative Dashboard",
          p("The World Bank ")
        )
      ),

      ## Map page ---------------------------------------------------------------------
      tabItem(
        tabName = "world_map",

        box(
          width = 11,
          solidHeader = TRUE,
          title = "Select information to display",
          status = "success",
          collapsible = TRUE,

          ### Filters -------------------------------------------------------------
          fluidRow(

            #### Identification strategy -------------------------------------------
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

            #### PEI learning priority ---------------------------------------------
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

            #### PI Affiliation ----------------------------------------------------
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

            #### Priority Group ---------------------------------------------------
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


          ### Map ------------------------------------------------------------------
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

          ### Number of projects ---------------------------------------------------
          fluidRow(
            id = "info_map",
            textOutput("n_projects")
          )
        ) # closes main box for page
      ) # Closes map tab item
    ) # Closes tab items
  ) # Closes dashboard body
) # UI closing

