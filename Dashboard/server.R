server <- function(input, output) {
  
  
  # Home Page-------------------------------------------------------------------

  ## Graph: Share of learning priority -----------------------------------------
  output$learning_priority_bar <- 
    renderPlot({
      priorities %>%
        group_by(name) %>%
        summarise(count = n()) %>%
        ggplot(
          aes(
            x = count,
            y = name,
            label = count
          )
        ) +
        geom_col() + 
        geom_text(
          hjust = -.5
        ) +
        scale_fill_manual(
          values = categorical_colors
        ) +
        labs(
          x = "Number of impact evaluations",
          y = "Priority number"
        ) +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 14)
        )
        
    })

  # Map page -------------------------------------------------------------------

  ## Reactive data for mapping which will update based on the criteria ---------
  map_projects <-
    eventReactive(
      c(
        input$map_method,
        input$map_learning,
        input$map_affiliation,
        input$map_target
      ),

      {
        project_data %>%
          filter(
            strs_detect_any(ie_method, input$map_method),
            strs_detect_any(learning_priority, input$map_learning),
            strs_detect_any(pi_affiliation, input$map_affiliation),
            strs_detect_any(priority_group, input$map_target)
          )
      }
    )

  ## Information Box: Number of projects ----------------------------------------
  output$n_projects <-
    renderText({
      map_projects() %>%nrow()
    })

  ## Map Plot ------------------------------------------------------------------
  output$map <-
    renderPlotly({

      projects_location <-
        map_projects() %>%
          select(-country) %>%
          left_join(projects_country) %>%
          group_by(iso) %>%
          summarise(n_projects = n()) 

      projects_location <-
        centroids %>%
        inner_join(projects_location) %>%
        mutate(label = paste(country, "\n", n_projects, "project(s)"))
      
      source(
        file.path(
          "auxiliary",
          "maps.R"
        )
      )
      
      map <-
        world_map +
        geom_sf(
          data = projects_location,
          aes(text = label),
          size = 3,
          color = "red"
        )
 
      map %>%
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
        )

    })
  
  # Table page ----------------------------------------------------------------
  
  ## Filters ------------------------------------------------------------------
  
  ### Columns -----------------------------------------------------------------
  output$table_vars <- 
    renderUI({
      pickerInput(
        inputId = "table_vars", 
        label =  "Column", 
        choices = names(column_list), 
        multiple = TRUE,
        selected = c(
          "Title", 
          "Country", 
          #"Research Team", 
          "Geographic coverage", 
          "Area", 
          "Target method"
        ),
        options = list(`actions-box` = TRUE)
      ) %>%
        helper(
          type = "inline",
          title = "Column to display",
          content = c("Use this picker to select the columns to display.",
                      "<b>All the columns selected</b> will be displayed in the table."),
          size = "s"
        )
    })
  
  ### Region -------------------------------------------------------------------
  output$table_region <- 
    
    renderUI({
      
      regions <- 
        projects_country %>% 
        pull(region) %>% 
        unique
      
      pickerInput(
        inputId = "table_region",
        label =  "Region", 
        choices = regions,
        selected = regions,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
    ) 
  })
  
  ### Country -------------------------------------------------------------------
  output$table_country <- 
    
    renderUI({
      
      countries <- 
        projects_country %>% 
        pull(country) %>% 
        unique
      
      pickerInput(
        inputId = "table_region",
        label =  "Country", 
        choices = countries,
        selected = countries,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ) 
    })
  
  ## Actual table --------------------------------------------------------------
  output$table <- 
    renderDataTable(
      {
        project_data %>%
          filter(
            strs_detect_any(ie_method, input$table_method),
            strs_detect_any(learning_priority, input$table_learning),
            strs_detect_any(pi_affiliation, input$table_affiliation),
            strs_detect_any(priority_group, input$table_target)
            #strs_detect_any(country, input$table_country)
          ) %>%
          rename(column_list) %>%
          select(all_of(input$table_vars)) #%>%
          # mutate(
          #   Details = shinyInput(
          #     actionButton, 
          #     nrow(.), 
          #     "button_", 
          #     label = "View project details", 
          #     class = "btn-primary",
          #     onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')
          # )
      },
      filter = "top",
      selection = "multiple", 
      escape = FALSE,
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 20, 50, 100),
        autoWidth = TRUE),
      rownames = FALSE,
      server = FALSE
        
      
    )
  
    
}
