#' Custom theme for catplot
#'
#' @param base_font_size Numeric, font size (default: 8)
#' @param font_family Character, font family (default: NULL)
#' @param linewidth Numeric, line width (default: 0.5)
#' @param aspect_ratio Numeric, aspect ratio of the plot (default: NULL)
#' @param frame Character, frame type, "none","closed" or "open" (default:
#'   "closed")
#' @param show_panel_grid_marjor Character, major panel grid visibility
#'  (default: "none")
#' @param show_panel_grid_minor Character, minor panel grid visibility
#' (default: "none")
#' @param panel_widths Numeric vector, panel widths in pt (default: NULL)
#' @param panel_heights Numeric vector, panel heights in pt (default: NULL)
#' @param show_title Character, axis title visibility (default: "both")
#' @param show_text Character, axis text visibility (default: "both")
#' @param text_italic Character, axis text italicization (default: "none")
#' @param show_ticks Character, axis tick visibility (default: "both")
#' @param ticks_length Numeric, tick length (default: 4)
#' @param legend_position Character, position of legend (default: "right")
#' @param legend_direction Character, direction of legend (default: NULL)
#' @param show_legend_title Logical, show legend title (default: TRUE)
#' @param legend_position_inside Logical, position legend inside the plot
#'   (default: NULL)
#' @param x_text_angle Numeric, angle of x-axis text (default: 0)
#' @return A ggplot2 theme
#' @param ... Additional arguments
#'
#' @return A ggplot2 theme
#' @example inst/examples/theme_cat.R
#' @export
#' @concept theme
theme_cat <- function(
    base_font_size = 8,
    font_family = NULL,
    linewidth = 0.5,
    panel_widths = NULL,
    panel_heights = NULL,
    aspect_ratio = NULL,
    frame = "open",
    show_panel_grid_marjor = "none",
    show_panel_grid_minor = "none",
    show_title = "both",
    show_text = "both",
    text_italic = "none",
    show_ticks = "both",
    ticks_length = 4,
    show_legend_title = TRUE,
    legend_position = "right",
    legend_position_inside = NULL,
    legend_direction = NULL,
    x_text_angle = 0,
    ...) {
  linewidth <- linewidth * 0.5 / 1.07
  panel_widths <- if (!is.null(panel_widths)) {
    unit(panel_widths, "pt")
  }
  panel_heights <- if (!is.null(panel_heights)) {
    unit(panel_heights, "pt")
  }
  # panel_widths and panel_heights can not be set together with aspect_ratio
  if (!is.null(aspect_ratio) && (!is.null(panel_widths) || !is.null(panel_heights))) {
    abort("`aspect_ratio` cannot be set together with `panel_widths` or `panel_heights`.")
  }

  arg_match0(arg = frame, values = c("none", "closed", "open"))
  arg_match0(arg = show_panel_grid_marjor, values = c("none", "x", "y", "both"))
  arg_match0(arg = show_panel_grid_minor, values = c("none", "x", "y", "both"))
  arg_match0(arg = show_title, values = c("none", "x", "y", "both"))
  arg_match0(arg = show_text, values = c("none", "x", "y", "both"))
  arg_match0(arg = text_italic, values = c("none", "x", "y", "both"))
  arg_match0(arg = show_ticks, values = c("none", "x", "y", "both"))
  arg_match0(
    arg = legend_position,
    values = c("none", "right", "bottom", "left", "top", "inside")
  )

  if (is_null(x = legend_position_inside)) {
    legend_margin <- switch(legend_position,
                            "right" = margin(l = -8),
                            "top" = margin(b = -8),
                            "bottom" = margin(t = -8),
                            "left" = margin(r = -8)
    )
  } else {
    legend_position <- "inside"
    legend_margin <- NULL
  }

  if (show_legend_title) {
    legend_title <- element_text(
      family = font_family,
      size = base_font_size,
      face = "plain",
      colour = "black",
      margin = margin()
    )
  } else {
    legend_title <- element_blank()
  }


  legend_direction <- legend_direction %||% "default"
  legend_direction <- switch(legend_position,
                             "h" = "horizontal",
                             "horizontal" = "horizontal",
                             "v" = "vertical",
                             "vertical" = "vertical",
                             "default" = "NULL"
  )
  tick_element <-
    element_line(
      linewidth = linewidth,
      lineend = "square",
      colour = "black"
    )

  grid_element <- element_line(
    colour = "lightgrey",
    linewidth = linewidth,
    lineend = "square"
  )

  # Axis title
  if (show_title %in% c("x", "both")) {
    axis_title_x <- element_text(
      family = font_family,
      face = "plain",
      colour = "black",
      size = base_font_size
    )
  } else {
    axis_title_x <- element_blank()
  }

  if (show_title %in% c("y", "both")) {
    axis_title_y <- element_text(
      face = "plain",
      colour = "black",
      size = base_font_size
    )
  } else {
    axis_title_y <- element_blank()
  }
  # Axis.text
  if (show_text %in% c("x", "both")) {
    axis_text_x <- element_text(
      family = font_family,
      face = switch(text_italic,
                    "x" = "italic",
                    "y" = "plain",
                    "both" = "italic",
                    "none" = "plain"
      ),
      colour = "black",
      size = base_font_size,
      angle = x_text_angle,
      hjust = adjust_text_alignment(angle = x_text_angle)$hjust,
      vjust = adjust_text_alignment(angle = x_text_angle)$vjust
    )
  } else {
    axis_text_x <- element_blank()
  }
  if (show_text %in% c("y", "both")) {
    axis_text_y <- element_text(
      family = font_family,
      face = switch(text_italic,
                    "x" = "plain",
                    "y" = "italic",
                    "both" = "italic",
                    "none" = "plain"
      ),
      colour = "black",
      size = base_font_size
    )
  } else {
    axis_text_y <- element_blank()
  }
  # Axis ticks
  if (show_ticks %in% c("x", "both")) {
    axis_ticks_x <- tick_element
  } else {
    axis_ticks_x <- element_blank()
  }
  if (show_ticks %in% c("y", "both")) {
    axis_ticks_y <- tick_element
  } else {
    axis_ticks_y <- element_blank()
  }
  # Panel grid
  ## panel_grid_major
  if (show_panel_grid_marjor %in% c("x", "both")) {
    panel_grid_major_x <- grid_element
  } else {
    panel_grid_major_x <- element_blank()
  }
  if (show_panel_grid_marjor %in% c("y", "both")) {
    panel_grid_major_y <- grid_element
  } else {
    panel_grid_major_y <- element_blank()
  }
  ## panel_grid_minor
  if (show_panel_grid_minor %in% c("x", "both")) {
    panel_grid_minor_x <- grid_element
  } else {
    panel_grid_minor_x <- element_blank()
  }
  if (show_panel_grid_minor %in% c("y", "both")) {
    panel_grid_minor_y <- grid_element
  } else {
    panel_grid_minor_y <- element_blank()
  }
  # Panel border & axis line
  if (frame == "closed") {
    panel_border <- element_rect(
      fill = NA,
      colour = "black",
      linewidth = linewidth
    )
    axis_line <- element_blank()
  } else if (frame == "open") {
    panel_border <- element_blank()
    axis_line <- element_line(
      colour = "black",
      linewidth = linewidth,
      lineend = "square"
    )
  } else if (frame == "none") {
    panel_border <- element_blank()
    axis_line <- element_blank()
    axis_ticks_x <- element_blank()
    axis_ticks_y <- element_blank()
    axis_title_x <- element_blank()
    axis_title_y <- element_blank()
    axis_text_x <- element_blank()
    axis_text_y <- element_blank()
  }
  # Default theme
  default_theme <- theme(
    aspect.ratio = aspect_ratio,
    panel.widths = panel_widths,
    panel.heights = panel_heights,
    line = element_line(color = "black"),
    rect = element_rect(),
    text = element_text(
      family = font_family,
      size = base_font_size,
      color = "black"
    ),
    # Axis line
    axis.line = axis_line,
    # Axis title
    axis.title.x = axis_title_x,
    axis.title.y = axis_title_y,
    # Axis text
    axis.text.x = axis_text_x,
    axis.text.y = axis_text_y,
    # Axis ticks
    axis.ticks.x = axis_ticks_x,
    axis.ticks.y = axis_ticks_y,
    axis.ticks.length = unit(ticks_length, "pt"),
    # Panel grid
    panel.grid.major.x = panel_grid_major_x,
    panel.grid.major.y = panel_grid_major_y,
    panel.grid.minor.x = panel_grid_minor_x,
    panel.grid.minor.y = panel_grid_minor_y,
    # Panel border
    panel.border = panel_border,
    panel.background = element_blank(),
    # Plot
    plot.background = element_blank(),
    plot.title = element_text(
      family = font_family,
      size = base_font_size,
      face = "plain",
      colour = "black",
      hjust = 0.5
    ),
    # Legend position
    legend.key.height = unit(base_font_size, "pt"),
    legend.key.width = unit(base_font_size, "pt"),
    legend.position = legend_position,
    legend.position.inside = legend_position_inside,
    legend.margin = legend_margin,
    legend.direction = legend_direction,
    legend.title = legend_title,
    legend.text = element_text(
      family = font_family,
      size = base_font_size,
      face = "plain",
      colour = "black",
      margin = margin(l = 0)
    ),
    legend.background = element_blank(),
    # Strip
    strip.background = element_blank(),
    strip.text = element_text(
      size = base_font_size,
      face = "plain",
      colour = "black"
    )
  )
  theme <- default_theme
  return(theme)
}

#' Adjust text alignment
#' @keywords internal
#' @param angle Numeric, angle of text
adjust_text_alignment <- function(angle) {
  if (angle == 0) {
    hjust <- 0.5
    vjust <- 1
  } else if (angle > 0 && angle < 90) {
    hjust <- 1
    vjust <- 1
  } else if (angle >= 90 && angle < 135) {
    hjust <- 1
    vjust <- 0.5
  } else if (angle >= 135 && angle <= 180) {
    hjust <- 1
    vjust <- 0
  } else {
    stop("Angle must be between 0 and 180 degrees.")
  }
  return(list(hjust = hjust, vjust = vjust))
}
