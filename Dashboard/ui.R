ui <- navbarPage(
  
  id = "intabset",
   
# Header -----------------------------------------------------------------------

   title = "IE Collaborative Dashboard", # Navigation bar
   theme = shinytheme("cerulean"), #Theme of the app (blue navbar)
   collapsible = TRUE, #tab panels collapse into menu in small screens
   header = tags$head(
     includeCSS("www/styles.css"),
     HTML("<html lang='en'>")
   ),


# Home page --------------------------------------------------------------------

  tabPanel(
    title = "Home", 
    icon = icon("home")
  ),

# Map page ---------------------------------------------------------------------

   tabPanel(
     
     title = "World map",
     icon = icon("map-location-dot"),
     
     fluidPage(
       
       # Sidebar layout with inputs
       sidebarLayout(
         
         # Sidebar panel for inputs
         sidebarPanel(
           
           width = 3,
           h3(paste("Filters")), #Title
           
           # First input: Identification strategy
           selectInput(
             inputId = "strategy_select",
             label = "Impact evaluation identification strategy:",
             choices = ie_method_lab,
             selected = ie_method_lab
            ),
           
           # Second input: PEI Learning Priority
           selectInput(
             inputId = "learningpriority_select",
             label = "Impact evaluation learning priority:",
             choices = learning_priority,
             selected = learning_priority
           ),
           
           # Third input: PI Affiliation (rendered from the server: text variable)
           selectInput(
             inputId = "piaffiliation_select",
             label = "Principal Investigator's affiliation:",
             choices = pi_affiliation_lab,
             selected = pi_affiliation_lab
           ),
           
           # Fourth input: Priority Group
           selectInput(
             inputId = "prioritygroup_select",
             label = "Priority population groups that the program targets:",
             choices = priority_group_lab,
             selected = priority_group_lab
            )
           
         ), # Sidebarpanel bracket
         
         # Main panel for displaying outputs
         mainPanel(
           
           width = 9,
           fluidRow(
             id = "info_map",
             textOutput("projectcount")
           ), 
           
           fluidRow(
             box(
               width = 12,
               plotlyOutput(
                 "map", 
                 width = "100%", 
                 height = "500px"
               )
             )
           )

         ) # Main panel bracket
         
       ) # Sidebarlayout bracket
     ) # Fluid page bracket
   ) # Home Tab Panel Bracket
)# Navbar bracket

