theme_clean <- 
  function(font_size = 12,
           font_family = "",
           center_axis_labels = FALSE
  ){
    
    if (center_axis_labels) {
      haxis_just_x <- 0.5
      vaxis_just_y <- 0.5
      v_rotation_x <- 0
      v_rotation_y <- 0
    }
    else {
      haxis_just_x <- 1
      vaxis_just_y <- 1
      v_rotation_x <- 0
      v_rotation_y <- 0
    }
    
    ggplot2::theme(
      text = ggplot2::element_text(
        family = font_family,
        face = "plain",
        colour = "gray30",
        size = font_size,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.9,
        margin = ggplot2::margin(),
        debug = FALSE
      ),
      # axis.text.x = ggplot2::element_text(),
      # axis.text.y = ggplot2::element_text(),
      axis.title.x = ggplot2::element_text(hjust = haxis_just_x, angle = v_rotation_x, size = .8*font_size),
      axis.title.y = ggplot2::element_text(vjust = vaxis_just_y, angle = v_rotation_y, size = .8*font_size),
      title = ggplot2::element_text(colour='gray30', size = font_size*1.25),
      legend.key = ggplot2::element_rect(fill='transparent', colour = NA),
      legend.background = ggplot2::element_rect(fill='transparent', colour = NA),
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
  }