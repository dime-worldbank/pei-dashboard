
# Base plot: regions -----------------------------------------------------------

  world_map <-
    ggplot() +
      geom_sf(
        data = map,
        aes(
          fill = region,
          colour = region
        ),
        alpha = .8
      ) +
      scale_fill_manual(
        name = NULL,
        values = priority_colors
      ) +
        scale_color_manual(
          name = NULL,
          values = priority_colors
        ) +
      theme_void() +
      theme(
        legend.position = "none"
      )
