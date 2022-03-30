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
library(RColorBrewer)


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
                               selected = "All"),
                   
                   # Action button to apply the filters
                   column(width = 4, offset = 4
                          , actionBttn("apply_filters", "Apply filters")
                   ), 
                   
                   div(style = "margin-bottom: 40px;") # Create some space between button and sidebar end
                   
                 ), # Sidebarpanel bracket
                 
                 # Main panel for displaying outputs
                 mainPanel(
                   width = 9,
                   fluidRow(id="info_map",
                            textOutput("projectcount") 
                   ), # Information bracket
                   #div(style = "margin-bottom: 30px;"), # Create some space between filters and map
                   fluidRow(
                     box(width = 12, height = "80vh",
                         "Map will be displayed here!"
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
 
  # Information Box: Number of project
  output$projectcount <- renderText({
    nb <- peimain_data %>%
      nrow()
    paste0("Number of projects: ", nb)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
