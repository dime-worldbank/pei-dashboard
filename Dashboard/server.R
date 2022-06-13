server <- function(input, output, session) {
  
  # Initialize helper
  observe_helpers(withMathJax = TRUE)
  
  # Home Page-------------------------------------------------------------------

  ## Graph: Share of learning priority -----------------------------------------
  output$learning_priority_bar <- 
    renderPlot({
      priorities %>%
        left_join(learning_priority_aggregate) %>%
        group_by(label) %>%
        summarise(count = n()) %>%
        filter(label != "Other") %>%
        ggplot(
          aes(
            x = count,
            y = label,
            label = paste("   ", count),
            fill = label,
            text = label
          )
        ) +
        geom_col() + 
        geom_text(
          size = 5
        ) +
        scale_fill_manual(
          values = priority_colors
        ) +
        labs(
          x = NULL,
          y = NULL,
          title = "Number of impact evaluations by thematic priority"
        ) +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_blank(),
          legend.position = "none",
          plot.title = element_text(
            hjust = -2,
            size = 16
          )
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
      
        data <- 
          project_data 

        if (!(is.null(input$map_method))) {
          data <-
            data %>%
            filter(strs_detect_any(ie_method, input$map_method))
        }

        if (!(is.null(input$map_learning))) {
          data <-
            data %>%
            filter(strs_detect_any(learning_priority, input$map_learning))

        }
        
        if (!(is.null(input$map_target))) {
          data <-
            data %>%
            filter(strs_detect_any(priority_group, input$map_target))
        }
        
        if (!(is.null(input$map_affiliation))) {
          data <-
            data %>%
            filter(strs_detect_any(pi_affiliation, input$map_affiliation))
        }

        return(data)
      }
    )

  ## Information Box: Number of projects ----------------------------------------
  output$n_projects <-
    renderText({
      map_projects() %>%
        nrow()
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
      
      if (nrow(projects_location) > 0) {
        world_map <-
          world_map +
          geom_sf(
            data = projects_location,
            aes(text = label),
            size = 3,
            color = "red"
          )
      }
 
      world_map %>%
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

  ## Actual table --------------------------------------------------------------
  
  observeEvent(
    input$table_region,
    
    {
      countries <- 
        projects_country %>% 
        filter(region %in% input$table_region) %>%
        pull(country) %>% 
        unique
      
      updatePickerInput(
        session = session,
        "table_country",
        selected = countries
      )
        
    }
  )
  
  output$table <- 
    renderDataTable(
      {
        table <-
          project_data 
        
        if (!(is.null(input$table_method))) {
          table <-
            table %>%
            filter(strs_detect_any(ie_method, input$table_method))
        }
        
        if (!(is.null(input$table_learning))) {
          table <-
            table %>%
            filter(strs_detect_any(learning_priority, input$table_learning))
        }
        
        if (!(is.null(input$table_affiliation))) {
          table <-
            table %>%
            filter(strs_detect_any(pi_affiliation, input$table_affiliation))
        }
        
        if (!(is.null(input$table_country))) {
          table <-
            table %>%
            filter(strs_detect_any(country, input$table_country))
        }
        
        if (!(is.null(input$table_target))) {
          table <-
            table %>%
            filter(strs_detect_any(priority_group, input$table_target))
        }
          
       table %>%
         rename(column_list) %>%
         select(all_of(input$table_vars)) %>%
           mutate(
             Details = shinyInput(
               actionButton,
               nrow(.),
               row.names(.),
               label = "View project details",
               class = "btn-primary",
               onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')
           )
      },
      filter = "top",
      selection = "none", 
      escape = FALSE,
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 20, 50, 100),
        autoWidth = TRUE,
        scrollX = TRUE,
        scroller = TRUE),
      rownames = FALSE,
      server = FALSE
        
      
    )
  
  # Project page ----------------------------------------------------------------
  
  observeEvent(
    input$select_button, 
    {
      ie_data <- 
        project_data %>%
        filter(
          row.names(.) == input$select_button
          )
      
      # Render the IE Card
      output$iecard <- 
        renderUI(
          {
            includeHTML(
              rmarkdown::render(
                file.path(
                  "iecard",
                  "iecard.Rmd"
                  ),
                params = list(
                  ie_data = ie_data
                  ),
                output_file = "iecard.html"
              )
            )
          }
          )
      
      toggleModal(session, "popup", toggle = "toggle")
      
    }
  )
  
 
  output$download <- 
    downloadHandler(
      filename = "ProjectDetails.html",
      content = function(file) {
        rmarkdown::render(
          file.path(
            "iecard",
            "iecard_download.Rmd"
          ),
          output_file = file,
          envir = new.env(parent = globalenv())
        )
        }
      )
  
    
}
