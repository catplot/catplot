library(testthat)

# Test function for theme_cat
test_theme_cat <- function() {
  test_that("theme_cat default values", {
    theme <- theme_cat()

    # Check that the theme is a ggplot2 theme
    expect_s3_class(theme, "theme")

    # Check default values for axis titles
    expect_equal(theme$axis.title.x$size, 8)
    expect_equal(theme$axis.title.y$size, 8)

    # Check default line width
    expect_equal(theme$axis.line$linewidth, 0.5 * 0.5 / 1.07)

    # Check panel grid visibility
    expect_s3_class(theme$panel.grid.major.x, "element_blank")
    expect_s3_class(theme$panel.grid.major.y, "element_blank")
  })

  test_that("theme_cat with custom values", {
    theme <- theme_cat(base_font_size = 10, font_family = "Times", frame = "closed", show_panel_grid = "both")

    # Check custom values for axis titles
    expect_equal(theme$axis.title.x$size, 10)
    expect_equal(theme$axis.title.y$size, 10)

    # Check panel border for 'closed' frame
    expect_s3_class(theme$panel.border, "element_rect")
    expect_equal(theme$panel.border$colour, "black")

    # Check panel grid visibility
    expect_s3_class(theme$panel.grid.major.x, "element_line")
    expect_s3_class(theme$panel.grid.major.y, "element_line")
  })

  test_that("theme_cat legend position inside", {
    theme <- theme_cat(legend_position = "inside", legend_position_inside = TRUE)

    # Check legend position inside the plot
    expect_equal(theme$legend.position, "inside")
    expect_null(theme$legend.margin)
  })
}

# Run the tests
test_theme_cat()
