#' Plot survival probability
#' @export
#' @concept others
#' @example inst/examples/cat_survival.R
cat_survival <- function(x,
                         add_censor_mark = TRUE,
                         add_pvalue = TRUE,
                         add_confidence_interval = FALSE,
                         palette = "status_1") {
  check_installed(pkg = "ggsurvfit", reason = "to plot survival curves")
  p <- ggsurvfit::ggsurvfit(
    x = x,
    linewidth = 0.5 / .pt,
    theme = theme_cat(aspect_ratio = 1)
  ) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = c(0, 0),
      labels = function(x)
        x * 100
    )

  if (add_censor_mark) {
    p <- p + ggsurvfit::add_censor_mark(show.legend = FALSE, size = 0.5)
  }
  if (add_pvalue) {
    p <- p + ggsurvfit::add_pvalue(
      location = "annotation",
      size = 8 / .pt,
      x = 0.5,
      y = 0.1,
      hjust = 0,
      prepend_p = FALSE,
      caption = "italic(P) == {p.value}",
      parse = TRUE
    )
  }

  if (add_confidence_interval) {
    p <- p + ggsurvfit::add_confidence_interval(type = "ribbon",
                                                show.legend = FALSE)
  }
  max_time <- max(p$data$time, na.rm = TRUE)

  if (max_time > 3000) {
    time_unit <- "days"
    divisor <- 1
    interval <- 500
  } else if (max_time > 100) {
    time_unit <- "months"
    divisor <- 1
    interval <- 10
  } else {
    time_unit <- "years"
    divisor <- 1
    interval <- if (max_time <= 5)
      1
    else if (max_time <= 10)
      2
    else if (max_time <= 30)
      5
    else
      10
  }

  max_break <- ceiling(max_time / interval) * interval
  x_breaks <- seq(0, max_break, by = interval)

  p <- p +
    scale_x_continuous(
      breaks = x_breaks,
      labels = x_breaks,
      limits = c(0, max_break),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(x = paste0("Time (", time_unit, ")"), y = "Survival probability")
  p +
    scale_color_cat(palette = palette) +
    scale_fill_cat(palette = palette)
}
