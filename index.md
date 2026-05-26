# apa7

The goal of apa7 is to facilitate writing documents in APA Style (7th
Edition).

## Installation

To install the published version from CRAN:

``` r

install.packages("apa7")
```

You can install the development version of apa7 with:

``` r

install.packages("apa7", repos = c('https://wjschne.r-universe.dev'))
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

![](reference/figures/README-showcor-1.png)

``` r

# Make regression model, format parameters, and display flextable
lm(Volume ~ Girth + Height, data = trees) |> 
  apa_parameters() |> 
  apa_flextable()
```

![](reference/figures/README-showlm-1.png)

``` r

# Contingency table with chi-square test of independence
d <- mtcars[, c("am", "gear")]
colnames(d) <- c("Transmission", "Gears")
d$Transmission <- factor(d$Transmission, 
                         levels = c(0, 1), 
                         labels = c("Automatic", "Manual"))
apa_chisq(d)
```

![](reference/figures/README-showcontingence-1.png)

## Formatting functions

The package provides functions to format p-values, numbers, and other
statistical results according to APA Style.

``` r

# Format p-values
apa_p(c(0.0007, 0.001, 0.0081, 0.024, 0.454))
#> [1] "<.001" ".001"  ".008"  ".02"   ".45"
```
