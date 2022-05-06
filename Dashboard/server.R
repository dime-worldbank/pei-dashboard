server <- function(input, output) {
  
  
  #Home Page-----
  # Reactive data for mapping which will update based on the criteria ----
  
  selected_projects <- 
    eventReactive(
      c(
        input$method,
        input$learning,
        input$affiliation,
        input$target 
      ),
      
      {
        
        project_data %>%
          filter(
            strs_detect_any(ie_method, input$method),
            strs_detect_any(learning_priority, input$learning),
            strs_detect_any(pi_affiliation, input$affiliation),
            strs_detect_any(priority_group, input$target)
          )
      }
    )
    
  # Information Box: Number of project ------

  output$n_projects <- 
    renderText({
      paste0(
        "Number of projects: ",
          selected_projects() %>%
            nrow()
      )
    })
  
  # Map Plot -----
  
  output$map <- 
    renderLeaflet({
      
      projects_location <-
        project_data %>%
          select(-country) %>%
          left_join(projects_country) %>%
          group_by(iso) %>%
          summarise(n_projects = n()) 
      
      projects_location <-
        centroids %>%
        inner_join(projects_location)
      
    # Interactive map
    
    leaflet() %>%
      addPolygons(
        data = map,
        color = ~pal(region),
        fillOpacity = 1
      ) %>%
      setMapWidgetStyle(list(background= "white")) %>%
      addAwesomeMarkers(
        data = projects_location,
        icon = icons,
        label = ~paste(country, "\n", n_projects, "projects")
      )
    })
}
