
<!-- README.md is generated from README.Rmd. Please edit that file -->

# catplot: Capable And Tidy Plot <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![pipeline
status](http://gitlab.catplot.com/catplot/catplot/badges/main/pipeline.svg)](http://gitlab.catplot.com/catplot/catplot/-/commits/main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## 🤪 Overview

`catplot` is a capable and tidy data visualization tool and maintained
by [Songqi Duan](https://songqi.org).

## 📦 Installation

You can install the development version of `catplot` like so:

``` r
install.packages("pak")
pak::pak("catplot/catplot")
```

## 🕹️ Usage

This is a basic usage of `catplot`:

``` r
library(ggplot2)
library(catplot)

data("iris")

p <- iris |>
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species)) +
  theme_cat(aspect_ratio = 1)
p
```

<img src="man/figures/README-example-1.png" width="60%" style="display: block; margin: auto;" />

## 🧩 Code of Conduct

Please note that the catplot project is released with a [Contributor
Code of Conduct](https://catplot.catplot.org/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
