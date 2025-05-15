
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

``` r
library(apa7)
apa_chisq(mtcars[, c("am", "gear")])
#> Registered S3 method overwritten by 'ftExtra':
#>   method                  from     
#>   as_flextable.data.frame flextable
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
apa_cor(trees)
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />

## Install apaquarto extension

``` r
apa7::install_apaquarto()
```
