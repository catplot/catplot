library(catplot)
library(ggplot2)

p <- iris |>
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species))
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
