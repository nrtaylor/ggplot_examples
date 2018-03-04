# ggplot with white background

library(ggplot2)

get_plain_theme <- function()
{
  custom_theme <- theme_void()
  custom_theme$panel.background <- element_rect("white")
  custom_theme
}

export_plain_plot <- function(plot_to_export, filename)
{
  pdf(filename)
  plot_to_export + get_plain_theme()
  dev.off()
  1
}