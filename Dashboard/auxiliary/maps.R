
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
      theme_void() +
      theme(
        legend.position = "none"
      )
