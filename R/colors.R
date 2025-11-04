cat_colors <- list(
  category = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d"),
  group = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F"),
  status = c("#D62728", "#2CA02C"),
  heat = c("#132B43", "#56B1F7", "#FDAE61"),
  diverging = c("#B2182B", "#F7F7F7", "#2166AC"),
  primary = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51"),
  status_1 = c("#cccccc", "#8c1a63")
)

cat_palette <- function(palette = "category", reverse = FALSE, continuous = FALSE) {
  pal <- cat_colors[[palette]]
  if (is.null(pal)) stop(paste0("Palette '", palette, "' not found in cat_colors"))
  if (reverse) pal <- rev(pal)

  if (continuous) {
    scales::gradient_n_pal(pal)
  } else {
    function(n) {
      if (n > length(pal)) warning("Not enough colors in palette '", palette, "'")
      pal[1:n]
    }
  }
}


#' @export
scale_color_cat <- function(palette = "category", reverse = FALSE, continuous = FALSE, ...) {
  if (continuous) {
    ggplot2::scale_color_gradientn(colours = cat_palette(palette, reverse, TRUE)(256), ...)
  } else {
    ggplot2::discrete_scale("colour", paste0("cat_", palette),
                            palette = cat_palette(palette, reverse, FALSE), ...)
  }
}

#' @export
scale_fill_cat <- function(palette = "category", reverse = FALSE, continuous = FALSE, ...) {
  if (continuous) {
    ggplot2::scale_fill_gradientn(colours = cat_palette(palette, reverse, TRUE)(256), ...)
  } else {
    ggplot2::discrete_scale("fill", paste0("cat_", palette),
                            palette = cat_palette(palette, reverse, FALSE), ...)
  }
}

#' @export
cat_palettes <- function() names(cat_colors)

