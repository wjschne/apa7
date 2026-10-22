# Scientific notation for markdown

Scientific notation for markdown

## Usage

``` r
sci_md(x, digits = 2, lb = 0.0095, ub = 99.5, align = FALSE)
```

## Arguments

- x:

  Numeric vector

- digits:

  number of digits of precision

- lb:

  lower bound where scientific notation is turned on

- ub:

  upper bound where scientific notation is turned on

- align:

  decimal align

## Value

A character vector

## Examples

``` r
sci_md(c(.0045,1.44, 24567857.45))
#> [1] "4.50×10^−3^" "1.44"        "2.46×10^7^" 
```
