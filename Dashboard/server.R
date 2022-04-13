server <- function(input, output) {
  
  
  #Home Page-----
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
