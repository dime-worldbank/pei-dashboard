
# Base plot: regions -----------------------------------------------------------

  world_map <-
    ggplot() +
      geom_sf(
        data = map,
        aes(
          fill = region,
          colour = region
        )
      ) +
      scale_fill_manual(
        name = NULL,
        values = region_colors
      ) +
        scale_color_manual(
          name = NULL,
          values = region_colors
        ) +
      labs(title = paste0("<b>", "Distribution of number of programs per region", "</b>")) +
      theme_void() +
      theme(
        legend.position = "none"
      )
