# Scientific notation for markdown

Scientific notation for markdown

## Usage

``` r
sci_md(x, digits = 2, lb = 0.0095, ub = 99.5, align = FALSE)
```

## Arguments

- x:

  numeric vector

- digits:

  number of decimal places (of the mantissa, for numbers in scientific
  notation)

- lb, ub:

  numbers at or below `lb` or at or above `ub` are written in scientific
  notation (e.g., 2.46 × 10^7^); numbers between them are written as
  usual

- align:

  if `TRUE`, the numbers are aligned on the decimal point with
  [`align_chr()`](https://wjschne.github.io/apa7/reference/align_chr.md)

## Value

A character vector

## Examples

``` r
sci_md(c(.0045,1.44, 24567857.45))
#> [1] "4.50×10^−3^" "1.44"        "2.46×10^7^" 
```
