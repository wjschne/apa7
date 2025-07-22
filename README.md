
<!-- README.md is generated from README.Rmd. Please edit that file -->

# apa7

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/apa7)](https://CRAN.R-project.org/package=apa7)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of apa7 is to facilitate writing documents in APA Style (7th
Edition).

## Installation

You can install the development version of apa7 from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("wjschne/apa7")
```

## Tables

The package provides functions to create APA-style tables, including
correlation matrices and regression tables. The tables can be formatted
using the `flextable` package.

``` r
library(apa7)
# Correlation matrix 
apa_cor(trees, star_significant = TRUE)
```

<img src="man/figures/README-showtables-1.png" width="100%" />

``` r
# Make regression model, format parameters, and display flextable
lm(Volume ~ Girth + Height, data = trees) |> 
  apa_parameters() |> 
  apa_flextable()
```

<img src="man/figures/README-showtables-2.png" width="100%" />

``` r

# Contingency table with chi-square test of independence
d <- mtcars[, c("am", "gear")]
colnames(d) <- c("Transmission", "Gears")
d$Transmission <- factor(d$Transmission, 
                         levels = c(0, 1), 
                         labels = c("Automatic", "Manual"))
apa_chisq(d)
```

<img src="man/figures/README-showtables-3.png" width="100%" />

## Formatting functions

The package provides functions to format p-values, numbers, and other
statistical results according to APA Style.

``` r
# Format p-values
apa_p(c(0.0007, 0.001, 0.0081, 0.024, 0.454))
#> [1] "<.001" ".001"  ".008"  ".02"   ".45"
```
