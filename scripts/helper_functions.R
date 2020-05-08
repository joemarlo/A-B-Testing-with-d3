library(tidyverse)

# set seed
set.seed(44)

# set scientific notation options 
options(scipen = 100, digits = 4)

# gg theme ----------------------------------------------------------------

theme_custom <- function() {
  theme_gray() +
    theme(
      panel.grid.minor.y = element_line(color = NA),
      panel.grid.major.y = element_line(color = "gray95"),
      panel.grid.minor.x = element_line(color = NA),
      panel.grid.major.x = element_line(color = "gray95"),
      panel.background = element_rect(fill = NA),
      plot.background = element_rect(
        fill = NA,
        color = "gray95",
        size = 10
      ),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      axis.title = element_text(color = "gray30"),
      axis.ticks = element_line(color = NA),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(
        color = "gray30",
        size = 11,
        face = "bold"
      ),
      plot.title = element_text(color = "gray30",
                                face = "bold"),
      plot.subtitle = element_text(size = 10,
                                   color = "gray30"),
      text = element_text(family = "Helvetica"),
      plot.caption = element_text(face = "italic",
                                  size = 6,
                                  color = 'grey50'),
      legend.title = element_blank(),
      legend.position = 'bottom',
      legend.key = element_rect(fill = NA)
    )
}

theme_set(theme_custom())

# wrap ggplot() with a new default color palette
ggplot <- function(...){ ggplot2::ggplot(...) + 
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Set1")
}

# set wrapper around saving plots so size and type is consistent
save_plot <- function(name, plot = ggplot2::last_plot(), type = c("png", "svg"), height = 4, width = 6.5){
  # function saves ggplots with standardized sizes
  # if more than one type is provided, then plot is saved once per each type
  
  invisible(
    map(type, function(x) {
      ggplot2::ggsave(
       filename = paste0('figures/', name, '.', x),
       plot = plot,
       device = x,
        height = height,
       width = width
    )
  })
  )
}
