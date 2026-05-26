# Tests if a character vector contains numeric-like values

Tests if a character vector contains numeric-like values

## Usage

``` r
is_numeric_like(x, elementwise = FALSE)
```

## Arguments

- x:

  character vector

- elementwise:

  if `TRUE`, returns a logical vector for each element, otherwise
  returns a single logical value indicating if all elements are
  numeric-like (default: `FALSE`)

## Value

logical vector

## Examples

``` r
is_numeric_like(c("-9", " 2.0", "-1.0 "))
#> [1] TRUE
is_numeric_like(c("9-", -1, "10"))
#> [1] FALSE
is_numeric_like(c("9", -1.2, "10"))
#> [1] TRUE
```
