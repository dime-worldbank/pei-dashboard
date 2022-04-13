# Loading the necessary packages

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(dplyr)
library(shinyjs)
library(sf)
library(ggplot2)
library(plotly)


# Define UI for the application----
ui <- navbarPage(id = "intabset",
           ## Header ----
           title = "IE Collaborative Dashboard", # Navigation bar
           theme = shinytheme("cerulean"), #Theme of the app (blue navbar)
           collapsible = TRUE, #tab panels collapse into menu in small screens
           header = tags$head(includeCSS("www/styles.css"), # CSS styles
                              HTML("<html lang='en'>")),
           ## Home page ----
           tabPanel(
             title = " Home", icon = icon("home"),
             fluidPage(
               # Sidebar layout with inputs
               sidebarLayout(
                 # Sidebar panel for inputs
                 sidebarPanel(
                   width = 3,
                   h3(paste("Filters")), #Title
                   
                   # First input: Identification strategy
                   selectInput(inputId = "strategy_select",
                               label = "Impact evaluation identification strategy:",
                               choices = list("All", "Randomized Controlled Trial (RCT)", 
                                              "Regression Discontinutiy Design", 
                                              "Matching", "Difference-in-Difference", 
                                              "Instrumental Variable", 
                                              "Other"),
                               selected = "All"),
                   
                   # Second input: PEI Learning Priority
                   selectInput(inputId = "learningpriority_select",
                               label = "Impact evaluation learning priority:",
                               choices = list("All", 
                                              "Effectiveness at scale: What is the cost-effectiveness of large-scale government-led programs?", 
                                              "Effectiveness at scale: What is the nature and extent of spillovers and general equilibrium effects on the local economy?", 
                                              "Scalable delivery modalities: How do alternative delivery modalities that enhance scalability affect program impact and cost-effectiveness?", 
                                              "Dynamics over time: How do impacts vary over time? Are impacts sustained in the short, medium and the long-term? How do impacts over time affect its cost-benefit analysis?", 
                                              "Bundling of Interventions: What is the appropriate/optimal bundle for a given context? What is the marginal contribution of constituent interventions to overall impact and cost?",
                                              "Bundling of Interventions: Does the timing, sequencing, and intensity of interventions matter?", 
                                              "Targeting/heterogeneity: What is the cost-effectiveness of economic inclusion programs across population groups?", 
                                              "Targeting/heterogeneity: What modifications in bundle design and delivery are necessary to increase cost-effectiveness for different sub-groups?", 
                                              "External validity across settings: How to adapt economic inclusion programs in urban contexts?", 
                                              "External validity across settings: How to adapt economic inclusion programs in FCV or displacement affected contexts?", 
                                              "Resilience and shock-responsiveness: Do economic inclusion programs improve households' resilience to (climate, conflict, or economic) shocks? How?",
                                              "Other"),
                               selected = "All"),
                   
                   # Third input: PI Affiliation (rendered from the server: text variable)
                   uiOutput("piaffiliation_select"),
                   
                   # Fourth input: Priority Group
                   selectInput(inputId = "prioritygroup_select",
                               label = "Priority population groups that the program targets:",
                               choices = list("All", "Women", 
                                              "Children", "Youth", 
                                              "Elderly", "People with disabilities", 
                                              "Refugees", "Internally displaced", 
                                              "Ethnic minorities", "Other"),
                               selected = "All")
                   
                 ), # Sidebarpanel bracket
                 
                 # Main panel for displaying outputs
                 mainPanel(
                   width = 9,
                   fluidRow(id="info_map",
                            textOutput("projectcount")
                   ), # Information bracket
                   #div(style = "margin-bottom: 30px;"), # Create some space between filters and map
                   fluidRow(
                     box(width = 12,
                         plotlyOutput("map", width = "100%", height = "500px")
                     )
                     
                   ) # Map bracket
                 )
                 
               ), # Sidebarlayout bracket
             ) #Fluid page bracket
            ), # Home Tab Panel Bracket
           ## Project Page ----
           tabPanel(
             title = "Browse Projects", icon = icon("table"), value = "projects",
             fluidPage(
               fluidRow(id="filter_table",
                        "Put some filters here"
                        ), # Filters bracket
               
               fluidRow(
                 box(id = "box_table", width = 12, status = "primary", 
                     title = "List of projects matching the criterias",
                     "The table will be here"
                    )
               ) # Map bracket
             ) #Fluid page bracket
            ), # Project Tab Panel Bracket
           ## Project Card ----
           tabPanel(
             title = "Project Card", icon = icon("file-invoice"), value = "card"
            ) # Project Card Bracket

)# Navbar bracket



# Define server logic----
server <- function(input, output) {
  
  # Load data -----
  # Load the raw survey data
  peimain_data <- read.csv("Output/data/peisurvey_main.csv", sep = ";")
  
  # Load the PI repeat group data
  pi_data <- read.csv("Output/data/peisurvey_pi.csv", sep = ";")
  
  # Load the Impact Evaluation question repeat group data
  evalquest_data <- read.csv("Output/data/peisurvey_evalq.csv", sep = ";")
  
  # Load the treatment arms  repeat group data
  treat_data <- read.csv("Output/data/peisurvey_treat.csv", sep = ";")
  
  # Load country data structure that will be used to join geom data and survey data
  countries_structure <- read.csv("Output/data/country_data_structure.csv", sep = ";")
  
  # Data cleaning and transformation ----
  # Pull country label vector assigning each country code to its corresponding label
  # Sort decreasing code to avoid error while labeling
  country_label <- setNames(countries_structure$country_name, c(1:217))
  country_label <- sort(country_label, decreasing = TRUE)
  #Define label for priority_group
  priority_group_label <- c("1" = "Women", "2" = "Children", "3" = "Youth", 
                            "4" = "Elderly", "5" = "People with disabilities", 
                            "6" = "Refugees", "7" = "Internally displaced", 
                            "8" = "Ethnic minorities", "9" = "Other")
  #Label for learning priority
  # Sort decreasing code to avoid error while labeling
  learning_priority_label <- c( 
    "1" = "Effectiveness at scale: What is the cost-effectiveness of large-scale government-led programs?", 
    "2" = "Effectiveness at scale: What is the nature and extent of spillovers and general equilibrium effects on the local economy?", 
    "3" = "Scalable delivery modalities: How do alternative delivery modalities that enhance scalability affect program impact and cost-effectiveness?", 
    "4" = "Dynamics over time: How do impacts vary over time? Are impacts sustained in the short, medium and the long-term? How do impacts over time affect its cost-benefit analysis?", 
    "5" = "Bundling of Interventions: What is the appropriate/optimal bundle for a given context? What is the marginal contribution of constituent interventions to overall impact and cost?",
    "6" = "Bundling of Interventions: Does the timing, sequencing, and intensity of interventions matter?", 
    "7" = "Targeting/heterogeneity: What is the cost-effectiveness of economic inclusion programs across population groups?", 
    "8" = "Targeting/heterogeneity: What modifications in bundle design and delivery are necessary to increase cost-effectiveness for different sub-groups?", 
    "9" = "External validity across settings: How to adapt economic inclusion programs in urban contexts?", 
    "10" = "External validity across settings: How to adapt economic inclusion programs in FCV or displacement affected contexts?", 
    "11" = "Resilience and shock-responsiveness: Do economic inclusion programs improve households' resilience to (climate, conflict, or economic) shocks? How?",
    "12" = "Other")
  learning_priority_label <- learning_priority_label[sort(as.numeric(names(learning_priority_label)), decreasing = TRUE)]
  #Define label for area
  area_label <- c("1" = "Rural", "2" = "Urban", "3" = "Peri-urban")
  #Define label for ie_method
  ie_method_label <- c("1" = "Randomized Controlled Trial (RCT)", 
                       "2" = "Regression Discontinutiy Design", 
                       "3" = "Matching", 
                       "4" = "Difference-in-Difference", 
                       "5" = "Instrumental Variable", 
                       "6" = "Other")
  
  peimain_data <- peimain_data %>%
    mutate(country_lbl = str_replace_all(peimain_data$country, country_label), # Label for country
           priority_group_lbl = str_replace_all(priority_group, priority_group_label), # Label for priority group
           area_lbl = str_replace_all(area, area_label), # Label for program area
           ie_method_lbl = str_replace_all(ie_method, ie_method_label), # Label IE identification strategy
           learning_priority_lbl = str_replace_all(learning_priority, learning_priority_label) # Label for Learning priority
    ) 
  
  
  #Home Page-----
  
  # PI Affiliation Select Input
  output$piaffiliation_select <- renderUI({
    piaffiliation <- pi_data %>%
      select(pi_affiliation) %>% 
      pull() %>% 
      unique() %>%
      sort()
    piaffiliation <- c("All", piaffiliation)
    
    selectInput(inputId = "piaffiliation_select",
                label = "Principal Investigator's affiliation:",
                choices = piaffiliation,
                selected = "All")
  })
  
  # Reactive data for mapping which will update based on the criteria ----
  
  reactive_pei_data <- reactive({
    req(input$strategy_select)
    req(input$learningpriority_select)
    req(input$piaffiliation_select)
    req(input$prioritygroup_select)
    
    # Retrive filter value and deleting parentheses to avoid error
    f1 <- gsub("\\s*\\([^\\)]+\\)", "", input$strategy_select)
    f2 <- input$learningpriority_select
    f3 <- input$piaffiliation_select
    f4 <- input$prioritygroup_select
    
    # Filtering PI affiliation from the repeat group and pulling the corresponding project
    if (f3 == "All") {
      set_of_pis = "All"
    } else {
      set_of_pis <- pi_data %>%
        filter(pi_affiliation == f3) %>%
        select(SET.OF.pis) %>% 
        pull() %>%
        unique()
    }
    
    
    # Filter data based on the input value
    peimain_data %>%
    filter(if (f1 == "All") {!is.na(ie_method_lbl)} else {grepl(f1, ie_method_lbl)},
           if (f2 == "All") {!is.na(learning_priority_lbl)} else {grepl(f2, learning_priority_lbl)},
           if (set_of_pis == "All") {!is.na(SET.OF.pis)} else {SET.OF.pis %in% set_of_pis},
           if (f4 == "All") {!is.na(priority_group_lbl)} else {grepl(f4, priority_group_lbl)}
           ) 
  })
 
  # Information Box: Number of project ------
  output$projectcount <- renderText({
    nb <- reactive_pei_data() %>%
      nrow()
    paste0("Number of projects: ", nb)
    
  })
  
  # Map Plot -----
  
  output$map <- renderPlotly({
    
    # Cleaning and transforming data for map
    map_data <- reactive_pei_data() %>% 
      select(country_1:country_217) %>%
      t() %>%
      as.data.frame() %>%
      rowwise() %>%
      mutate(nb_prog_country = sum(across(everything()), na.rm = T)) %>%
      bind_cols(pei_country_name = countries_structure$country_name, 
                country_region = countries_structure$country_region_wb,
                country_code = countries_structure$country_wb_code,
                OBJECTID = countries_structure$OBJECTID) %>%
      select(OBJECTID, country_code, country_region, pei_country_name, nb_prog_country) %>%
      filter(!(country_region == "")) %>%
      group_by(country_region) %>%
      summarise(nb_prog_region = sum(nb_prog_country))
    
    # Send a notification to the user
    showNotification("Ploting the map... Please wait.", type = "message", duration = 3)
    
    # Load Geo data file
    wb_country_geom <- read_rds("Output/data/wb_country_geom_fact.rds")
    
    # Map PEI project count by country
    # Static Map
    map_plot <- 
      wb_country_geom %>%
      left_join(map_data, by=c("REGION_WB"="country_region")) %>%
      ggplot() +
      geom_sf(
        aes(
          fill = REGION_WB,
          text = paste0("Region name: ", REGION_WB, "<br>", 
                        "Number of program: ", nb_prog_region)
        ),
        color = "black",
        size = 0.1
      ) +
      scale_fill_manual(
        name = NULL,
        values = c("East Asia & Pacific" = "#688ade",
                   "Europe & Central Asia" = "#d1a04b",
                   "Latin America & Caribbean" = "#b86867",
                   "Middle East & North Africa" = "#76abad",
                   "North America" = "#936d9e",
                   "South Asia" = "#6d9e7d",
                   "Sub-Saharan Africa" = "#dbcd60",
                   "Not available" = "#808080"),
        na.value = "#808080",
        drop = FALSE) +
      labs(title = paste0("<b>", "Distribution of number of programs per region", "</b>")) +
      theme_void()
    
    # Interactive map
    
    map_plot %>%
      ggplotly(tooltip = "text") %>%
      layout(
        legend = list(
          title = list(text = '<b>Region Name:</b>'),
          y = 0.5
        ),
        margin = list(t = 50, b = 110),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations =
          list(x = 0,
               y = -0.2,
               text = HTML(
                 paste(
                   str_wrap(
                     "<b>Disclaimer:</b> Country borders or names do not necessarily reflect the World Bank Group's official position.
                     This map is for illustrative purposes and does not imply the expression of any opinion on the part of the World Bank,
                     concerning the legal status of any country or territory or concerning the delimitation of frontiers or boundaries.",
                     175
                   ),
                   str_wrap(
                     paste(
                       "<b>Source:</b>",
                       "PEI's Survey of Ongoing Economic Inclusion Impact Evaluations"
                     ),
                     175
                   ),
                   sep = "<br>"
                 )
               ),
               showarrow = F,
               xref = 'paper',
               yref = 'paper',
               align = 'left',
               font = list(size = 10)
          )
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian", "zoom2d", "pan2d"),
        toImageButtonOptions = list(
          filename = "nb_program_region_map",
          width = 1050,
          height =  675
        )
      )
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
