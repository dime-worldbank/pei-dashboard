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
    
    
    # map_plot %>%
    #   ggplotly(tooltip = "text") %>%
    #   layout(
    #     legend = list(
    #       title = list(text = '<b>Region Name:</b>'),
    #       y = 0.5
    #     ),
    #     margin = list(t = 50, b = 110),
    #     xaxis = list(visible = FALSE),
    #     yaxis = list(visible = FALSE),
    #     annotations =
    #       list(x = 0,
    #            y = -0.2,
    #            text = HTML(
    #              paste(
    #                str_wrap(
    #                  "<b>Disclaimer:</b> Country borders or names do not necessarily reflect the World Bank Group's official position.
    #                  This map is for illustrative purposes and does not imply the expression of any opinion on the part of the World Bank,
    #                  concerning the legal status of any country or territory or concerning the delimitation of frontiers or boundaries.",
    #                  175
    #                ),
    #                str_wrap(
    #                  paste(
    #                    "<b>Source:</b>",
    #                    "PEI's Survey of Ongoing Economic Inclusion Impact Evaluations"
    #                  ),
    #                  175
    #                ),
    #                sep = "<br>"
    #              )
    #            ),
    #            showarrow = F,
    #            xref = 'paper',
    #            yref = 'paper',
    #            align = 'left',
    #            font = list(size = 10)
    #       )
    #   ) %>%
    #   config(
    #     displaylogo = FALSE,
    #     modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian", "zoom2d", "pan2d"),
    #     toImageButtonOptions = list(
    #       filename = "nb_program_region_map",
    #       width = 1050,
    #       height =  675
    #     )
    #   )
    
  # })
  
  
}
