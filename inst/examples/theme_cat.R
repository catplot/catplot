library(catplot)
library(ggplot2)

p <- iris |>
  ggplot(aes(
    x = Sepal.Length,
    y = Sepal.Width,
    color = Petal.Width,
    size = Petal.Width
  )) +
  geom_point()
p
# Axis title
p + theme_cat(show_title = "y")

# Axis text
p + theme_cat(
  text_italic = "x",
  show_text = "x"
)

# Axis ticks
p + theme_cat(show_ticks = "y")

# Panel grid
p + theme_cat(show_panel_grid = "both")

# Frame
p + theme_cat(
  frame = "closed", aspect_ratio = 1,
  show_panel_grid = "both",
  show_title = "y"
)

# Panel size
p + theme_cat(panel_widths = 120, panel_heights = 120)


